;*=====================================================================*/
;*    /priv/serrano2/bigloo/wasm/comptime/SawWasm/code.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Hubert Gruniaux                                   */
;*    Creation    :  Sat Sep 14 08:29:47 2024                          */
;*    Last change :  Wed Sep 25 14:28:40 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Wasm code generation                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_wasm_code
   (include "Tools/trace.sch"
	    "Tools/location.sch")
   (import type_type
	   ast_var
	   ast_node
	   ast_env
	   module_module
	   engine_param
	   type_tools
	   type_cache
	   type_typeof
	   tvector_tvector
	   object_class
	   object_slots
	   cnst_alloc
	   tools_shape
	   tools_location
	   backend_backend
	   backend_cvm
	   saw_defs
	   saw_woodcutter
	   saw_node2rtl
	   saw_expr
	   saw_regset
	   saw_register-allocation
	   saw_bbv
	   saw_bbv-config
	   saw_bbv-types
	   saw_bbv-debug
	   saw_wasm_relooper)
   (export (wasm-gen b::wasm v::global)
	   (wasm-type t::type #!optional (may-null #f))
	   (wasm-default-value t::type)
	   (wasm-vector-type t::type)
	   (wasm-sym t::bstring)
	   (wasm-cnst-nil)
	   (wasm-cnst-false)
	   (wasm-cnst-true)
	   (wasm-cnst-unspec)
	   (emit-wasm-atom-value type value)
	   (gen-reg reg)
	   (gen-basic-block b)
	   (gen-ins ins::rtl_ins)
	   (gen-switch fun type patterns labels args gen-go gen-block-label)
	   (cnst-table-sym)
	   *allocated-strings*
	   *extra-types*)
   (cond-expand ((not bigloo-class-generate) (include "SawWasm/code.sch")))
   (static (wide-class SawCIreg::rtl_reg index)))

;*---------------------------------------------------------------------*/
;*    compilation configuration ...                                    */
;*---------------------------------------------------------------------*/
(define *wasm-tailcall* #t)
(define *wasm-split-inits* #t)
(define *wasm-peephole* #t)

;*---------------------------------------------------------------------*/
;*    wasm-gen ...                                                     */
;*---------------------------------------------------------------------*/
(define (wasm-gen b::wasm v::global)
   (let ((l (global->blocks b v)))
      (gen-fun b v l)))

;*---------------------------------------------------------------------*/
;*    gen-fun ...                                                      */
;*---------------------------------------------------------------------*/
(define (gen-fun b::wasm v::global l)
   
   (define (split-body body)
      (let loop ((body body)
		 (locals '()))
	 (match-case body
	    (()
	     (values (reverse locals) body))
	    (((and ?head (local . ?-)) . ?rest)
	     (loop rest (cons head locals)))
	    (else
	     (values (reverse locals) body)))))
   
   (with-trace 'wasm "gen-fun"
      (with-access::global v (id name import value type)
	 (trace-item "fun=" id " " name)
	 (trace-item "type=" (shape type))
	 ;; debug 
	 (with-access::sfun value (loc args)
	    (let ((params (map local->reg args)))
	       
	       (build-tree b params l)
	       
	       (set! l (register-allocation b v params l))
	       (set! l (bbv b v params l))
	       (when *wasm-split-inits*
		  (set! l (split-blocks l)))
	       
	       (when *bbv-dump-cfg*
		  (with-access::sfun value (args)
		     (dump-cfg v (map local->reg args) l ".wasm.cfg")))
	       
	       (let* ((locals (get-locals params l))
		      (body (gen-body v l))
		      (inits (gen-local-inits locals v l body)))
		  `(comment ,(string-append (symbol->string (global-id v))
				" in " (symbol->string (global-module v)))
		      ,(with-loc loc
			  `(func ,(wasm-sym name)
			      
			      ,@(if (eq? import 'export)
				    `((export ,name))
				    '())
			      
			      ,@(gen-params params)
			      ,@(gen-result type)
			      ,@(gen-locals locals)
			      
			      ,@(if (null? inits)
				    body
				    (multiple-value-bind (locals rest)
				       (split-body body)
				       (append locals
					  (list '(comment "local initialization"))
					  inits
					  rest))))))))))))

;*---------------------------------------------------------------------*/
;*    gen-body ...                                                     */
;*---------------------------------------------------------------------*/
(define (gen-body v::global blocks)
   (or (relooper v blocks)
       (gen-dispatcher-body blocks)))

;*---------------------------------------------------------------------*/
;*    *extra-types*                                                    */
;*---------------------------------------------------------------------*/
;; Extra types that must be inserted in the module preamble.
;; This is mostly used for
;; temp function types for extra-light funcalls.
(define *extra-types* '())

;*---------------------------------------------------------------------*/
;*    get-locals ...                                                   */
;*---------------------------------------------------------------------*/
(define (get-locals params l)
   ;; Update all reg to ireg and return all regs not in params.
   (define n 0)
   (define regs '())
   
   (define (expr->ireg e)
      (cond
	 ((isa? e SawCIreg)
	  #unspecified)
	 ((rtl_reg? e)
	  (widen!::SawCIreg e
	     (index n))
	  (set! n (+fx n 1))
	  (set! regs (cons e regs)))
	 (else
	  (map expr->ireg (rtl_ins-args e)))))
   
   (define (visit! b::block)
      (for-each (lambda (ins)
		   (with-access::rtl_ins ins (dest fun args)
		      (if dest (expr->ireg dest))
		      (for-each expr->ireg args)))
	 (block-first b)))
   
   (for-each expr->ireg params)
   
   (set! regs '())
   (for-each visit! l)
   
   regs)

;*---------------------------------------------------------------------*/
;*    wasm-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (wasm-type t::type #!optional (may-null #f))
   (let ((id (type-id t))
	 (name (type-name t)))
      (case id
	 ((obj) '(ref eq))
	 ((nil) '(ref eq))
	 ((magic) '(ref eq))
	 ((unspecified) '(ref eq))
	 ((bbool) '(ref i31))
	 ((bchar) '(ref i31))
	 ((class-field) '(ref eq))
	 ((pair-nil) '(ref eq))
	 ((cobj) '(ref eq))
	 ((void*) 'i32) ;; A raw pointer into the linear memory
	 ((tvector) 'arrayref)
	 ((cnst) '(ref i31))
	 ((funptr) '(ref func))
	 ((bool) 'i32)
	 ((byte) 'i32)
	 ((ubyte) 'i32)
	 ((char) 'i32)
	 ((uchar) 'i32)
	 ((ucs2) 'i32)
	 ((int8) 'i32)
	 ((uint8) 'i32)
	 ((int16) 'i32)
	 ((uint16) 'i32)
	 ((int32) 'i32)
	 ((uint32) 'i32)
	 ((int64) 'i64)
	 ((uint64) 'i64)
	 ((int) 'i32)
	 ((uint) 'i32)
	 ((long) 'i64)
	 ((ulong) 'i64)
	 ((elong) 'i64)
	 ((uelong) 'i64)
	 ((llong) 'i64)
	 ((ullong) 'i64)
	 ((float) 'f32)
	 ((double) 'f64)
	 ((vector) '(ref $vector))
	 ((string bstring) '(ref $bstring))
	 (else 
	  (cond 
	     ((foreign-type? t)
	      (error "wasm-gen" "unimplemented foreign type in WASM" id))
	     ;; Classes
	     ((string-suffix? "_bglt" name)
	      (if (and #f may-null)
		  `(ref null ,(wasm-sym name))
		  `(ref ,(wasm-sym name))))
	     ((tvec? t)
	      (if (and #f may-null)
		  `(ref null ,(wasm-vector-type t))
		  `(ref ,(wasm-vector-type t))))
	     (else
	      (if (and #f may-null)
		  `(ref null ,(wasm-sym (symbol->string id)))
		  `(ref ,(wasm-sym (symbol->string id))))))))))

;*---------------------------------------------------------------------*/
;*    wasm-default-value ...                                           */
;*---------------------------------------------------------------------*/
(define (wasm-default-value ty)
   (case (type-id ty)
      ((bool) '(i32.const 0))
      ((bbool) (wasm-cnst-false))
      ((char) '(i32.const 0))
      ((bchar) `(global.get $bchar-default-value))
      ((uchar) '(i32.const 0))
      ((ucs2) '(i32.const 0))
      ((bucs2) `(global.get $bucs2-default-value))
      ((byte) '(i32.const 0))
      ((ubyte) '(i32.const 0))
      ((int8) '(i32.const 0))
      ((uint8) '(i32.const 0))
      ((int16) '(i32.const 0))
      ((uint16) '(i32.const 0))
      ((int32) '(i32.const 0))
      ((uint32) '(i32.const 0))
      ((int64) '(i64.const 0))
      ((uint64) '(i64.const 0))
      ((int) '(i32.const 0))
      ((uint) '(i32.const 0))
      ((long) '(i64.const 0))
      ((elong) '(i64.const 0))
      ((uelong) '(i64.const 0))
      ((ulong) '(i64.const 0))
      ((llong) '(i64.const 0))
      ((ullong) '(i64.const 0))
      ((float) '(f32.const 0))
      ((double) '(f64.const 0))
      ((bint) `(global.get $bint-default-value))
      ((bint8) `(global.get $bint8-default-value))
      ((bint16) `(global.get $bint16-default-value))
      ((bint32) `(global.get $bint32-default-value))
      ((bint64) `(global.get $bint64-default-value))
      ((buint8) `(global.get $buint8-default-value))
      ((buint16) `(global.get $buint16-default-value))
      ((buint32) `(global.get $buint32-default-value))
      ((buint64) `(global.get $buint64-default-value))
      ((belong) `(global.get $belong-default-value))
      ((pair) `(global.get $pair-default-value))
      ((epair) `(global.get $epair-default-value))
      ((nil) (wasm-cnst-nil))
      ((pair-nil) (wasm-cnst-nil))
      ((cell) `(global.get $cell-default-value))
      ((symbol) `(global.get $symbol-default-value))
      ((keyword) `(global.get $keyword-default-value))
      ((string bstring) `(global.get $bstring-default-value))
      ((ucs2string) `(global.get $ucs2string-default-value))
      ((regexp) `(global.get $regexp-default-value))
      ((vector) `(global.get $vector-default-value))
      ((u8vector) `(global.get $u8vector-default-value))
      ((s8vector) `(global.get $s8vector-default-value))
      ((u16vector) `(global.get $u16vector-default-value))
      ((s16vector) `(global.get $s16vector-default-value))
      ((u32vector) `(global.get $u32vector-default-value))
      ((s32vector) `(global.get $s32vector-default-value))
      ((u64vector) `(global.get $u64vector-default-value))
      ((s64vector) `(global.get $s64vector-default-value))
      ((f32vector) `(global.get $f32vector-default-value))
      ((f64vector) `(global.get $f64vector-default-value))
      ((struct) `(global.get $struct-default-value))
      ((class) `(global.get $class-default-value))
      ((procedure) `(global.get $procedure-default-value))
      ((procedure-l) `(global.get $procedure-l-default-value))
      ((procedure-el) `(global.get $procedure-el-default-value))
      ((mutex) `(global.get $mutex-default-value))
      ((condvar) `(global.get $condvar-default-value))
      ((date) `(global.get $date-default-value))
      ((real) `(global.get $real-default-value))
      ((bignum) `(global.get $bignum-default-value))
      ((port) `(global.get $port-default-value))
      ((output-port) `(global.get $output-port-default-value))
      ((file-output-port) `(global.get $file-output-port-default-value))
      ((input-port) `(global.get $input-port-default-value))
      ((file-input-port) `(global.get $file-input-port-default-value))
      ((binary-port) `(global.get $binary-port-default-value))
      ((socket) `(global.get $socket-default-value))
      ((datagram-socket) `(global.get $datagram-socket-default-value))
      ((weakptr) `(global.get $weakptr-default-value))
      ((mmap) `(global.get $mmap-default-value))
      ((process) `(global.get $process-default-value))
      ((custom) `(global.get $custom-default-value))
      ((foreign) `(global.get $foreign-default-value))
      ((exit) `(global.get $exit-default-value))
      ((dynamic-env) `(global.get $dynamic-env-default-value))
      ((object) `(global.get $object-default-value))
      ((funptr) `(global.get $funptr-default-value))
      ((unspecified) (wasm-cnst-unspec))
      ((obj) (wasm-cnst-unspec))
      (else
       (cond
	  ((isa? ty tclass)
	   (with-access::tclass ty (name holder)
	      `(ref.cast (ref ,(wasm-sym name))
		  (call $BGL_CLASS_INSTANCE_DEFAULT_VALUE
		     (global.get ,(wasm-sym (global-name holder)))))))
	  ((isa? ty tvec)
	   `(array.new_fixed ,(wasm-vector-type ty) 0))
	  ((foreign-type? ty)
	   (error "wasm" "Unknown foreign type for default value" (type-id ty)))
	  (else
	   (error "wasm" "No default init value for builtin type" (type-id ty)))))))

;*---------------------------------------------------------------------*/
;*    wasm-vector-type ...                                             */
;*---------------------------------------------------------------------*/
(define (wasm-vector-type vtype::type)
  (if (eq? (type-id vtype) 'vector)
    '$vector
    (get-ref-tvector-type (tvec-item-type vtype))))

(define *tvector-types* '())
;*---------------------------------------------------------------------*/
;*    get-ref-tvector-type ...                                         */
;*---------------------------------------------------------------------*/
(define (get-ref-tvector-type vtype::type)
   (if (eq? type-id vtype)
       '$vector
       (let ((type (assq vtype *tvector-types*)))
	  (if type
	      (cdr type)
	      (let ((sym (gensym "$tvecty"))
		    (wasm-vtype
		       (case (type-id vtype)
			  ;; i8 and i16 are allowed in WASM array definitions
			  ;; use them instead of i32 to avoid wasting space.
			  ((bool) 'i8)
			  ((byte) 'i8)
			  ((ubyte) 'i8)
			  ((char) 'i8)
			  ((uchar) 'i8)
			  ((int8) 'i8)
			  ((uint8) 'i8)
			  ((int16) 'i8)
			  ((uint32) 'i8)
			  (else (wasm-type vtype)))))
		 (set! *extra-types*
		    (cons `(type ,sym (array (mut ,wasm-vtype)))
		       *extra-types*))
		 (set! *tvector-types*
		    (cons (cons vtype sym)
		       *tvector-types*))
		 sym)))))

;*---------------------------------------------------------------------*/
;*    gen-params ...                                                   */
;*---------------------------------------------------------------------*/
(define (gen-params l)
   (map (lambda (arg)
	   `(param ,(wasm-sym (reg-name arg)) ,(wasm-type (rtl_reg-type arg))))
      l))

;*---------------------------------------------------------------------*/
;*    gen-result ...                                                   */
;*---------------------------------------------------------------------*/
(define (gen-result t)
   (if (eq? (type-id t) 'void)
       '()
       `((result ,(wasm-type t)))))

;*---------------------------------------------------------------------*/
;*    gen-locals ...                                                   */
;*---------------------------------------------------------------------*/
(define (gen-locals l)
   (map (lambda (local)
	   `(local ,(wasm-sym (reg-name local))
	       ,(wasm-type (rtl_reg-type local))))
      l))

;*---------------------------------------------------------------------*/
;*    gen-local-inits ...                                              */
;*    -------------------------------------------------------------    */
;*    As of 17sep2024, the wasm spec says:                             */
;*                                                                     */
;*        "Track initialisation status of locals during validation     */
;*         and only allow local.get after a local.set/tee in the same  */
;*         or a surrounding block."                                    */
;*                                                                     */
;*    (See https://github.com/WebAssembly/function-references/)        */
;*                                                                     */
;*    To work around this restriction, the generated code includes     */
;*    dummy initializations of variables that could be considered      */
;*    non-initialized by wasm-as.                                      */
;*---------------------------------------------------------------------*/
(define (gen-local-inits locals::pair-nil v::global l::pair-nil wasm)

   (define (type-require-init? t)
      (case (type-id t)
	 ((int long double bool) #f)
	 (else #t)))

   (define (first-block-assigs l)
      ;; super simple heuristic, white list the variables
      ;; assigned in the first basic block
      (with-trace 'wasm "first-block-assigs"
	 (cond
	    ((null? l)
	     '())
	    ((head-loop? (car l) (cdr l))
	     '())
	    (else
	     (with-access::block (car l) (first)
		(let loop ((first first)
			   (wl '()))
		   (if (null? first)
		       wl
		       (let ((i (car first)))
			  (with-access::rtl_ins i (fun dest)
			     (trace-item "i=" (typeof fun) " " (shape dest))
			     (cond
				((or (isa? fun rtl_go)
				     (isa? fun rtl_switch)
				     (isa? fun rtl_ifeq)
				     (isa? fun rtl_ifne))
				 wl)
				((not (isa? dest rtl_reg))
				 (loop (cdr first) wl))
				((or (isa? fun rtl_mov)
				     (isa? fun rtl_new)
				     (isa? fun rtl_loadg))
				 (trace-item (shape dest))
				 (loop (cdr first) (cons dest wl)))
				((isa? fun rtl_call)
				 ;; MS 20/09/2024: not sure if this is always
				 ;; legal to continue with the next
				 ;; instructions in case of a call...
				 (trace-item (shape dest))
				 (loop (cdr first) (cons dest wl)))
				(else
				 (loop (cdr first) wl))))))))))))

   (define (mark-temp! t::rtl_reg temps block)
      (let ((c (assq t temps)))
	 (when (pair? c)
	    (unless (memq block (cdr c))
	       (set-cdr! c (cons block (cdr c)))))))
   
   (define (mark-ins! i::rtl_ins temps block)
      (with-access::rtl_ins i (dest args)
	 (when (isa? dest rtl_reg)
	    (mark-temp! dest temps block))
	 (for-each (lambda (a)
		      (cond
			 ((isa? a rtl_reg)
			  (mark-temp! a temps block))
			 ((isa? a rtl_ins)
			  (mark-ins! a temps block))))
	    args)))
   
   (define (single-block-ref l)
      ;; white list the variables that are used in
      ;; a unique basic block
      (with-trace 'wasm "single-block-ref"
	 (let ((temps (map list locals)))
	    (let loop ((l l)
		       (stack '()))
	       (cond
		  ((null? l)
		   (if (getenv "DEBUG_INITS")
		       (begin
			  (print "DEBUG_INITS...")
			  '())
		       (filter-map (lambda (temp)
				      (when (and (pair? (cdr temp))
						 (null? (cddr temp)))
					 (trace-item (shape (car temp)))
					 (car temp)))
			  temps)))
		  ((memq (car l) stack)
		   (loop (cdr l) stack))
		  (else
		   (with-access::block (car l) (first)
		      (for-each (lambda (i)
				   (mark-ins! i temps (car l)))
			 first))
		   (loop (cdr l) (cons (car l) stack))))))))
   
   (define whitelist
      (if (dispatcher? wasm)
	  '()
	  (delete-duplicates!
	     (append (first-block-assigs l)
		(single-block-ref l)))))

   (with-trace 'wasm "gen-local-inits"
      (trace-item "locals=" (map shape locals))
      (trace-item "WL=" (map shape whitelist))
      (filter-map (lambda (r::rtl_reg)
		     (unless (memq r whitelist)
			(let* ((t (rtl_reg-type r))
			       (i (wasm-default-value t)))
			   (when (type-require-init? t)
			      (trace-item "init=" (reg-name r) "::"
				 (shape t))
			      `(local.set ,(wasm-sym (reg-name r))
				  ,(wasm-default-value t))))))
	 locals)))

;*---------------------------------------------------------------------*/
;*    reg-debugname ...                                                */
;*---------------------------------------------------------------------*/
(define (reg-debugname reg::rtl_reg)
   (with-access::rtl_reg reg (var debugname key)
      (cond
	 (debugname
	  debugname)
	 ((global? var)
	  (with-access::global var (id module alias)
	     (set! debugname
		(bigloo-module-mangle (symbol->string (or alias id))
		   (symbol->string module)))
	     debugname))
	 ((local? var)
	  (with-access::local var (id)
	     (set! debugname
		(bigloo-mangle
		   (string-append
		      (symbol->string id) "_" (symbol->string key))))
	     debugname))
	 (else
	  (set! debugname (symbol->string (gensym 'bbv)))
	  debugname))))

;*---------------------------------------------------------------------*/
;*    reg-name ...                                                     */
;*---------------------------------------------------------------------*/
(define (reg-name reg) ;()
   (with-access::rtl_reg reg (var debugname name key)
      (cond
	 (debugname
	  debugname)
	 ((global? var)
	  (with-access::global var (id module alias)
	     (set! debugname
		(bigloo-module-mangle (symbol->string (or alias id))
		   (symbol->string module)))
	     debugname))
	 ((local? var)
	  (with-access::local var (id)
	     (set! debugname
		(bigloo-mangle
		   (string-append
		      (symbol->string id) "_" (symbol->string key))))
	     debugname))
	 (else
	  (set! debugname (symbol->string name))
;* 	     (string-append (if (SawCIreg-var reg) "V" "R")            */
;* 		(integer->string (SawCIreg-index reg))))               */
	  debugname))))

;*---------------------------------------------------------------------*/
;*    needs-dispatcher? ...                                            */
;*---------------------------------------------------------------------*/
(define (needs-dispatcher? l)
   ;; if there is a single basic block or none,
   ;; then we don't have any control flow
   ;; and therefore we are sure that we don't need a dispatcher block.
   (not (or (null? l) (null? (cdr l)))))

;*---------------------------------------------------------------------*/
;*    dispatcher? ...                                                  */
;*---------------------------------------------------------------------*/
(define (dispatcher? body)
   (match-case body
      (((local $__label i32) . ?-) #t)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    gen-dispatcher-body ...                                          */
;*---------------------------------------------------------------------*/
(define (gen-dispatcher-body blocks)
   ;; If there is only a single basic block in the function, there is no
   ;; need to do hard work or generate a dispatcher.
   ;; Just dump the basic block code.
   (if (null? (cdr blocks))
       (gen-basic-block (car blocks))
       `((local $__label i32)
	 (local.set $__label (i32.const ,(block-label (car blocks))))
	 (loop $__dispatcher
	    ,@(let iter-block ((l (reverse blocks))
			       (label #f))
		 (if (null? l)
		     `((block ,(wasm-block-sym label)
			  ,(gen-dispatcher blocks)))
		     (let ((bb (car l)))
			(if label
			    `((block ,(wasm-block-sym label)
				 ,@(iter-block (cdr l) (block-label bb))
				 ,@(gen-basic-block bb)))
			    `(,@(iter-block (cdr l) (block-label bb))
				,@(gen-basic-block bb))))))))))

;*---------------------------------------------------------------------*/
;*    wasm-block-sym ...                                               */
;*---------------------------------------------------------------------*/
(define (wasm-block-sym label)
  (string->symbol (string-append "$bb_" (integer->string label))))

;*---------------------------------------------------------------------*/
;*    wasm-block-label ...                                             */
;*---------------------------------------------------------------------*/
(define (wasm-block-label b)
  (wasm-block-sym (block-label b)))

;*---------------------------------------------------------------------*/
;*    gen-dispatcher ...                                               */
;*---------------------------------------------------------------------*/
(define (gen-dispatcher blocks)
  `(br_table ,@(map wasm-block-label blocks) (local.get $__label)))

;*---------------------------------------------------------------------*/
;*    gen-basic-block ...                                              */
;*---------------------------------------------------------------------*/
(define (gen-basic-block b)
  (filter-map gen-ins (block-first b)))

(define-generic (do-push? fun::rtl_fun) #f)

; TODO: add remaining instructions
(define-method (do-push? fun::rtl_loadi) #t)
(define-method (do-push? fun::rtl_loadg) #t)
(define-method (do-push? fun::rtl_nop) #t)
(define-method (do-push? fun::rtl_funcall) #t)
(define-method (do-push? fun::rtl_valloc) #t)
(define-method (do-push? fun::rtl_vref) #t)
(define-method (do-push? fun::rtl_vlength) #t)
(define-method (do-push? fun::rtl_boxref) #t)
(define-method (do-push? fun::rtl_cast) #t)
(define-method (do-push? fun::rtl_new) #t)
(define-method (do-push? fun::rtl_getfield) #t)
(define-method (do-push? fun::rtl_makebox) #t)
(define-method (do-push? fun::rtl_mov) #t) ;; FIXME: not correct depends on arguments
(define-method (do-push? fun::rtl_call)
  (let ((retty (variable-type (rtl_call-var fun))))
    (not (eq? (type-id retty) 'void))))

; TODO: implement do-push? for other types

(define (gen-ins ins::rtl_ins)
  (with-access::rtl_ins ins (dest fun args)
    (if dest
      `(local.set ,(gen-reg/dest dest) ,(gen-expr fun args))
      ;; We need to add an explicit drop if the instruction push
      ;; some data to the stack. Indeed, at the end of each block
      ;; the stack must be empty (all pushed values must have been
      ;; popped).
      (if (do-push? fun) `(drop ,(gen-expr fun args)) (gen-expr fun args)))))

(define-generic (gen-expr fun::rtl_fun args) #unspecified)

(define (gen-args args)
  (map (lambda (arg) (gen-reg arg)) args))

;*---------------------------------------------------------------------*/
;*    gen-reg ...                                                      */
;*---------------------------------------------------------------------*/
(define (gen-reg reg)
   (if (isa? reg SawCIreg)
       `(local.get ,(gen-reg/dest reg))
       (gen-expr (rtl_ins-fun reg) (rtl_ins-args reg))))

;*---------------------------------------------------------------------*/
;*    gen-reg/dest ...                                                 */
;*---------------------------------------------------------------------*/
(define (gen-reg/dest reg)
   (string->symbol (string-append "$" (reg-name reg))))

;*---------------------------------------------------------------------*/
;*    with-loc ...                                                     */
;*---------------------------------------------------------------------*/
(define (with-loc loc expr)
   (let ((parsed-loc (parse-location loc)))
      (if parsed-loc
	  `(@ ,parsed-loc ,expr)
	  expr)))

;*---------------------------------------------------------------------*/
;*    with-fun-loc ...                                                 */
;*---------------------------------------------------------------------*/
(define (with-fun-loc fun expr)
   (with-access::rtl_fun fun (loc)
      (with-loc loc expr)))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-nil ...                                                */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-nil)
   '(global.get $BNIL))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-false ...                                              */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-false)
   '(global.get $BFALSE))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-true ...                                               */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-true)
   '(global.get $BTRUE))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-unspec ...                                             */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-unspec)
   '(global.get $BUNSPEC))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-optional ...                                           */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-optional)
   '(global.get $BOPTIONAL))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-key ...                                                */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-key)
   '(global.get $BKEY))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-rest ...                                               */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-rest)
   '(global.get $BREST))

;*---------------------------------------------------------------------*/
;*    wasm-cnst-eoa ...                                                */
;*---------------------------------------------------------------------*/
(define (wasm-cnst-eoa)
   '(global.get $BEOA))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_nop ...                                           */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_nop args)
   ;; Strangely, NOP is defined as returning the constant BUNSPEC...
   (with-fun-loc fun (wasm-cnst-unspec)))

(define-method (gen-expr fun::rtl_getfield args)
  (with-access::rtl_getfield fun (name objtype)
    (with-fun-loc fun 
      `(struct.get ,(wasm-sym (type-class-name objtype))
	  ,(wasm-sym name) ,@(gen-args args)))))

(define-method (gen-expr fun::rtl_setfield args)
  (with-access::rtl_setfield fun (name objtype)
    (with-fun-loc fun
      `(struct.set ,(wasm-sym (type-class-name objtype)) ,(wasm-sym name) ,@(gen-args args)))))

(define-method (gen-expr fun::rtl_instanceof args)
  ; TODO: NOT IMPLEMENTED
  (with-fun-loc fun `(INSTANCEOF ,@(gen-args args))))

(define-method (gen-expr fun::rtl_makebox args)
  (with-fun-loc fun 
    `(struct.new $cell ,@(gen-args args))))

(define-method (gen-expr fun::rtl_boxref args)
  ;; FIXME: remove the cast to cell
  (with-fun-loc fun
    `(struct.get $cell $val (ref.cast (ref $cell) ,(gen-reg (car args))))))

(define-method (gen-expr fun::rtl_boxset args)
  ;; FIXME: remove the cast to cell
  (with-fun-loc fun
    `(struct.set $cell $val (ref.cast (ref $cell) ,(gen-reg (car args))) ,@(gen-args (cdr args)))))

(define-method (gen-expr fun::rtl_fail args)
  ;; TODO
  (with-fun-loc fun 
    '(throw $fail)))

(define-method (gen-expr fun::rtl_return args)
   (with-fun-loc fun
      (let ((expr (gen-args args)))
	 (if *wasm-tailcall*
	     (match-case expr
		(((call . ?args))
		 `(return_call ,@args))
		((call-ref . ?args)
		 `(return_call_ref ,@args))
		(else
		 `(return ,@expr)))
	     `(return ,@expr)))))

(define-method (gen-expr fun::rtl_go args)
  ; We can not return a list of WASM instruction there (as it is not the 
  ; expected interface by the callers of this function). Therefore, we
  ; need to encapsulate the two instructions inside a WASM block.
  (with-fun-loc fun
    `(block ,@(gen-go (rtl_go-to fun)))))

(define-method (gen-expr fun::rtl_ifne args)
  (with-fun-loc fun
    `(if ,@(gen-args args) (then ,@(gen-go (rtl_ifne-then fun))))))

(define-method (gen-expr fun::rtl_ifeq args)
  (with-fun-loc fun
    `(if (i32.eqz ,@(gen-args args)) (then ,@(gen-go (rtl_ifeq-then fun))))))

;*---------------------------------------------------------------------*/
;*    intify ...                                                       */
;*---------------------------------------------------------------------*/
(define (intify x)
   (cond
    ((fixnum? x) x)
    ((uint32? x) (uint32->fixnum x))
    ((int32? x) (int32->fixnum x))
    (else x)))

;*---------------------------------------------------------------------*/
;*    gen-switch ...                                                   */
;*---------------------------------------------------------------------*/
(define (gen-switch fun type patterns labels args gen-go gen-block-label)
   
   (define else-bb #unspecified)
   (define num2bb '())
   
   (define (add n bb)
      (set! num2bb (cons (cons (intify n) bb) num2bb)))
   
   (for-each (lambda (pat bb)
		(if (eq? pat 'else)
		    (set! else-bb bb)
		    (for-each (lambda (n) (add n bb)) pat)))
      patterns labels)
   
   (set! num2bb (sort num2bb (lambda (x y) (<fx (car x) (car y)))))
   
   (let* ((nums (map car num2bb))
	  (min (car nums))
	  (max (car (last-pair nums)))
	  (n (length nums)))
      
      ;; TODO: generate a binary search if the density is too low.
      ;;       see SawJvm/code.scm: rtl_switch gen-fun
      (if gen-block-label
	  ;; Jump table using br_table
	  (with-fun-loc fun
	     `(br_table 
		 ,@(map gen-block-label (flat num2bb else-bb)) 
		 ,(gen-block-label else-bb) 
		 (i32.sub 
		    ,(if (need-cast-to-i32? type)
			 `(i32.wrap_i64 ,@(gen-args args))
			 (car (gen-args args)))
		    (i32.const ,min))))
	  ;; Binary search
	  (with-fun-loc fun 
	     `(comment "Binary search for switch"
		 ,(emit-binary-search gen-go type 
		     (gen-args args) 
		     else-bb 
		     (list->vector (map car num2bb)) 
		     (list->vector (map cdr num2bb))))))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_switch ...                                        */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_switch args)
   (with-access::rtl_switch fun (type patterns labels)
      (gen-switch fun type patterns labels args gen-go #f)))

;*---------------------------------------------------------------------*/
;*    cmp-ops-for-type ...                                             */
;*---------------------------------------------------------------------*/
(define (cmp-ops-for-type type)
  (case (type-id type)
    ((char) (values 'i32.lt_s 'i32.gt_s 'i32.const))
    ((uchar) (values 'i32.lt_u 'i32.gt_u 'i32.const))
    ((byte) (values 'i32.lt_s 'i32.gt_s 'i32.const))
    ((ubyte) (values 'i32.lt_u 'i32.gt_u 'i32.const))
    ((int8) (values 'i32.lt_s 'i32.gt_s 'i32.const))
    ((uint8) (values 'i32.lt_u 'i32.gt_u 'i32.const))
    ((int16) (values 'i32.lt_s 'i32.gt_s 'i32.const))
    ((uint16) (values 'i32.lt_u 'i32.gt_u 'i32.const))
    ((int32) (values 'i32.lt_s 'i32.gt_s 'i32.const))
    ((uint32) (values 'i32.lt_u 'i32.gt_u 'i32.const))
    ((int) (values 'i32.lt_s 'i32.gt_s 'i32.const))
    ((int64) (values 'i64.lt_s 'i64.gt_s 'i64.const))
    ((uint64) (values 'i64.lt_u 'i64.gt_u 'i64.const))
    (else (values 'i64.lt_s 'i64.gt_s 'i64.const))))

;*---------------------------------------------------------------------*/
;*    need-cast-to-i32? ...                                            */
;*---------------------------------------------------------------------*/
(define (need-cast-to-i32? type)
  (case (wasm-type type)
    ((i32) #f)
    ((i64) #t)
    (else (error "wasm" "Not a WASM integer type." type))))

;*---------------------------------------------------------------------*/
;*    emit-binary-search ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-binary-search gen-go type args else-bb patterns blocks)
  (multiple-value-bind (lt gt const) (cmp-ops-for-type type)
    (let helper ((low 0)
                (high (-fx (vector-length patterns) 1)))
      (if (<fx high low)
        `(block ,@(gen-go else-bb))
        (let ((middle (quotient (+fx low high) 2)))
          `(if (,gt (,const ,(vector-ref patterns middle)) ,@args)
            (then ,(helper low (-fx middle 1)))
            (else
              (if (,lt (,const ,(vector-ref patterns middle)) ,@args)
                (then ,(helper (+fx middle 1) high))
                (else ,@(gen-go (vector-ref blocks middle)))))))))))

;*---------------------------------------------------------------------*/
;*    flat ...                                                         */
;*---------------------------------------------------------------------*/
(define (flat al ldef)
  (define (walk al i r)
    (cond
      ((null? al) (reverse! r))
      ((=fx i (caar al)) (walk (cdr al) (+fx i 1) (cons (cdar al) r)))
      ((>fx i (caar al)) (walk (cdr al) i r))
      (else (walk al (+fx i 1) (cons ldef r)))))
  (walk al (caar al) '()))

(define-method (gen-expr fun::rtl_loadfun args)
  (with-fun-loc fun
    `(ref.func ,(wasm-sym (global-name (rtl_loadfun-var fun))))))

(define (cast-to-i32-if-needed reg)
  (if (eq? (wasm-typeof-arg reg) 'i64)
    `(i32.wrap_i64 ,(gen-reg reg))
    (gen-reg reg)))

(define-method (gen-expr fun::rtl_valloc args)
  (with-access::rtl_valloc fun (type vtype)
    (with-fun-loc fun
      `(array.new
        ,(wasm-vector-type vtype)
	,(wasm-default-value vtype)
        ,(cast-to-i32-if-needed (car args))))))

;*---------------------------------------------------------------------*/
;*    as-vector ...                                                    */
;*---------------------------------------------------------------------*/
(define (as-vector arg vtype::type)
   (if (or (eq? (type-id vtype) 'vector)
	   (isa? vtype tvec))
       (gen-reg arg)
       (let ((vec-type (wasm-vector-type vtype)))
	  `(ref.cast (ref ,vec-type) ,(gen-reg arg)))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_vref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_vref args)
   ; Bigloo generate 64-bit indices, but Wasm expect 32-bit indices,
   ;; thus the i32.wrap_i64.
   (with-access::rtl_vref fun (type vtype)
      (let ((vec-type (wasm-vector-type vtype)))
	 ;; If vtype is a vector of (ref eq) (type vector), then
	 ;; we need to explicitly insert a cast to the expected element
	 ;; type. This is not required for typed vectors
	 ;; as the WASM type has already the correct type.
	 (let ((array-code 
		  `(array.get ,vec-type
		      ,(as-vector (car args) vtype)
		      
		      ,(cast-to-i32-if-needed (cadr args)))))
	    (with-fun-loc fun 
	       (if (and #f
			(eq? (type-id vtype) 'vector)
			(not (eq? type *obj*)))
		   `(ref.cast ,(wasm-type type) ,array-code)
		   array-code))))))

(define-method (gen-expr fun::rtl_vset args)
  ; Bigloo generate 64-bit indices, but Wasm expect 32-bit indices, thus the i32.wrap_i64.
  (with-access::rtl_vset fun (type vtype)
    (let ((vec-type (wasm-vector-type vtype)))
      (with-fun-loc fun
        `(array.set ,vec-type 
          ,(as-vector (car args) vtype)
          ,(cast-to-i32-if-needed (cadr args))
          ,@(gen-args (cddr args)))))))

(define-method (gen-expr fun::rtl_vlength args)
  (with-fun-loc fun
    `(i64.extend_i32_u (array.len ,@(gen-args args)))))

;*---------------------------------------------------------------------*/
;*    emit-wasm-atom-value ...                                         */
;*---------------------------------------------------------------------*/
(define (emit-wasm-atom-value type value)
   (cond
      ; TODO: better reusable code? maybe use a macro, too many repetitions
      ((boolean? value)
       `(i32.const ,(if value 1 0)))
      ((char? value)
       `(i32.const ,(char->integer value)))
      ((int8? value)
       `(i32.const ,(int8->fixnum value)))
      ((uint8? value)
       `(i32.const ,(uint8->fixnum value)))
      ((int16? value)
       `(i32.const ,(int16->fixnum value)))
      ((uint16? value)
       `(i32.const ,(uint16->fixnum value)))
      ((int32? value)
       `(i32.const ,(int32->llong value)))
      ((uint32? value)
       `(i32.const ,(uint32->llong value)))
      ((int64? value)
       `(i64.const ,(int64->llong value)))
      ((uint64? value)
       `(i64.const ,(uint64->llong value)))
      ((elong? value)
       `(i64.const ,value))
      ((llong? value)
       `(i64.const ,value))
      ((ucs2? value)
       `(struct.new $bucs2 (i32.const ,(ucs2->integer value))))
      ((fixnum? value)
       ;; TODO: support other types
       (if (eq? (type-id type) 'int)
	   `(i32.const ,value)
	   `(i64.const ,value)))
      ((flonum? value) 
       (cond
          ((nanfl? value) `(f64.const nan))
          ((and (infinitefl? value) (>fl value 0.0)) `(f64.const inf))
          ((infinitefl? value) `(f64.const -inf))
          (else `(f64.const ,value))))
      ((null? value)
       (wasm-cnst-nil))
      ((eq? value #unspecified)
       (wasm-cnst-unspec))
      ((eq? value __eoa__)
       (wasm-cnst-eoa))
      ((eq? value boptional)
       (wasm-cnst-optional))
      ((eq? value bkey)
       (wasm-cnst-key))
      ((eq? value brest)
       (wasm-cnst-rest))
      ((bignum? value)
       (if (=bx (fixnum->bignum (bignum->fixnum value)) value)
	   `(call $bgl_long_to_bignum (i64.const ,(bignum->fixnum value)))
	   (let* ((str (bignum->string value 16))
		  (info (allocate-string str))
		  (section (car info))
		  (offset (cdr info)))
	      `(call $bgl_jsstring_to_bignum
		  (i32.load ,section)
		  (i32.const 0) (i32.const ,(string-length str))))))
      ((string? value)
       ;; FIXME: implement C string constants
       `(array.new_default $bstring (i32.const ,(string-length value)))) 
      ((eof-object? value)
       '(global.get $BEOF))
      ((cnst? value)
       `(i31.ref (i32.const ,(cnst->integer value))))
      (else 
       (error "wasm-gen" "unimplemented scheme atom value for WASM"
	  (typeof value)))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_loadi ...                                         */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_loadi args)
  (let ((atom (rtl_loadi-constant fun)))
    (with-fun-loc fun 
      (emit-wasm-atom-value (atom-type atom) (atom-value atom)))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_loadg ...                                         */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_loadg args)
   (let* ((var (rtl_loadg-var fun))
          (name (global-name var))
          (macro-code (global-qualified-type-name var)))
      (with-fun-loc fun
	 (if (and (isa? (global-value var) cvar)
		  (not (string-null? macro-code)))
	     (expand-wasm-macro (call-with-input-string macro-code read)
		(gen-args args))
	     `(global.get ,(wasm-sym (global-name (rtl_loadg-var fun))))))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_globalref ...                                     */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_globalref args)
   ;; TODO: what is the difference with rtl_loadg?
   ;; Should we also support WASM macro expansion here?
   (with-fun-loc fun 
      `(global.get ,(wasm-sym (global-name (rtl_globalref-var fun))))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_storeg ...                                        */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_storeg args)
   (with-fun-loc fun 
      `(global.set 
	  ,(wasm-sym (global-name (rtl_storeg-var fun))) 
	  ,@(gen-args args))))

;*---------------------------------------------------------------------*/
;*    wasm-sym ...                                                     */
;*---------------------------------------------------------------------*/
(define (wasm-sym ident)
  ;; All symbolic references are prefixed by $ in Wasm textual format.
  (string->symbol (string-append "$" ident)))

;*---------------------------------------------------------------------*/
;*    gen-go ...                                                       */
;*---------------------------------------------------------------------*/
(define (gen-go to::block)
   ;; Generate something like:
   ;;   (local.set $label (i32.const BLOCK_LABEL))
   ;;   (br $dispatcher)
   ;; This is used to simulate a goto. It is only used if Relooper
   ;; is not enabled. See (gen-body) to understand what $dispatcher is.
   `((local.set $__label (i32.const ,(block-label to)))
     (br $__dispatcher)))

;*---------------------------------------------------------------------*/
;*    expand-wasm-macro ...                                            */
;*    -------------------------------------------------------------    */
;*    Takes a Scheme list, symbol or atom value and replaces all       */
;*    occurrences of symbols ~k (with k an integer) to the k-th        */
;*    argument of this function.                                       */
;*    Example:                                                         */
;*      (expand-wasm-macro `(local.get ~0) `(1 2 3) #t "hello")        */
;*    gives                                                            */
;*      (local.get (1 2 3))                                            */
;*---------------------------------------------------------------------*/
(define (expand-wasm-macro macro args)
   (cond 
      ((symbol? macro)
       (let ((name (symbol->string macro)))
	  (if (string-prefix? "~" name)
	      (let ((index (string->integer (substring name 1))))
		 (list-ref args index))
	      macro)))
      ((pair? macro)
       (map (lambda (n) (expand-wasm-macro n args)) macro))
      (else macro)))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_call ...                                          */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_call args)
   (let* ((var (rtl_call-var fun))
          (name (global-name var))
          (macro-code (global-qualified-type-name var)))
      (with-fun-loc fun
	 (if (and (isa? (global-value var) cfun)
		  (not (string-null? macro-code)))
	     (expand-wasm-macro (call-with-input-string macro-code read)
		(gen-args args))
	     (case (global-id var)
		((make-fx-procedure) (inline-make-procedure args))
		((make-va-procedure) (inline-make-procedure args))
		((make-l-procedure) (inline-make-l-procedure args))
		((make-el-procedure) (inline-make-el-procedure args))
		((cnst-table-set!) (inline-cnst-table-set! args))
		((cnst-table-ref) (inline-cnst-table-ref args))
		(else (peephole `(call ,(wasm-sym name) ,@(gen-args args)))))))))

;*---------------------------------------------------------------------*/
;*    peephole ...                                                     */
;*---------------------------------------------------------------------*/
(define (peephole expr)
   (if *wasm-peephole*
       (match-case expr
	  ((BGl_2zc3zc3zz__r4_numbers_6_5z00
	      (call $I64_TO_BINT ?num)
	      (and ?tmp (local.get ?-)))
	   ;; 2<
	   `(if (ref.test (ref $bint) ,tmp)
		(i64.lt_s ,num ,tmp)
		,expr))
	  ((BGl_2zc3zc3zz__r4_numbers_6_5z00
	      (and ?tmp (local.get ?-))
	      (call $I64_TO_BINT ?num))
	   ;; 2<
	   `(if (ref.test (ref $bint) ,tmp)
		(i64.lt_s ,tmp ,num)
		,expr))
	  ((BGl_2ze3ze3zz__r4_numbers_6_5z00
	      (call $I64_TO_BINT ?num)
	      (and ?tmp (local.get ?-)))
	   ;; 2>
	   `(if (ref.test (ref $bint) ,tmp)
		(i64.gt_s ,num ,tmp)
		,expr))
	  ((BGl_2zc3zc3zz__r4_numbers_6_5z00
	      (and ?tmp (local.get ?-))
	      (call $I64_TO_BINT ?num))
	   ;; 2>
	   `(if (ref.test (ref $bint) ,tmp)
		(i64.gt_s ,tmp ,num)
		,expr))
	  ((or (BGl_2zb2zb2zz__r4_numbers_6_5z00
		  (call $I64_TO_BINT ?num)
		  (and ?tmp (local.get ?-)))
	       (BGl_2zb2zb2zz__r4_numbers_6_5z00
		  (and ?tmp (local.get ?-))
		  (call $I64_TO_BINT ?num)))
	   ;; 2+
	   `(if (ref.test (ref $bint) ,tmp)
		(call $I64_TO_BINT (i64.add ,num ,tmp))
		,expr))
	  (else
	   expr))
       expr))

;*---------------------------------------------------------------------*/
;*    inline-make-procedure ...                                        */
;*---------------------------------------------------------------------*/
(define (inline-make-procedure args)
   `(struct.new $procedure
       ;; entry
       ,(gen-reg (car args))
       ;; attr
       (global.get $BUNSPEC)
       ;; arity
       ,(gen-reg (cadr args))
       ;; env
       (array.new $vector (global.get $BUNSPEC) ,(gen-reg (caddr args)))))

;*---------------------------------------------------------------------*/
;*    inline-make-l-procedure ...                                      */
;*---------------------------------------------------------------------*/
(define (inline-make-l-procedure args)
   `(struct.new $procedure-l
       ;; entry
       ,(gen-reg (car args))
       ;; env
       (array.new $vector (global.get $BUNSPEC) ,(gen-reg (cadr args)))))

;*---------------------------------------------------------------------*/
;*    inline-make-el-procedure ...                                     */
;*---------------------------------------------------------------------*/
(define (inline-make-el-procedure args)
   `(array.new $procedure-el (global.get $BUNSPEC) ,(gen-reg (car args))))

;*---------------------------------------------------------------------*/
;*    cnst-table-sym ...                                               */
;*---------------------------------------------------------------------*/
(define (cnst-table-sym)
   (wasm-sym (string-append "__cnsts_table_" (symbol->string *module*))))

;*---------------------------------------------------------------------*/
;*    inline-cnst-table-set! ...                                       */
;*---------------------------------------------------------------------*/
(define (inline-cnst-table-set! args) 
   `(block (result (ref eq)) 
       (array.set $cnst-table 
	  (global.get ,(cnst-table-sym))
	  ,@(gen-args args)) 
       (global.get $BUNSPEC)))

;*---------------------------------------------------------------------*/
;*    inline-cnst-table-ref ...                                        */
;*---------------------------------------------------------------------*/
(define (inline-cnst-table-ref args) 
   `(array.get $cnst-table
       (global.get ,(cnst-table-sym))
       ,@(gen-args args)))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_funcall args)
   (with-fun-loc fun (gen-expr-funcall/lightfuncall '(ref eq) args)))

;*---------------------------------------------------------------------*/
;*    gen-expr-funcall/lightfuncall ...                                */
;*---------------------------------------------------------------------*/
(define (gen-expr-funcall/lightfuncall type args)
   (let* ((arg_count (length args))
	  (func_type (wasm-sym
			(string-append "func"
			   (fixnum->string (-fx arg_count 1)))))
	  (proc `(ref.cast (ref $procedure) ,(gen-reg (car args)))))
      `(if (result ,type)
	   (i32.lt_s (struct.get $procedure $arity ,proc) (i32.const 0)) 
	   (then ;; Is a variadic function!
	      (call $generic_va_call 
		 ,proc 
		 (array.new_fixed $vector ,(-fx arg_count 1) ,@(gen-args (cdr args)))))
	   (else
	    (call_ref 
	       ,func_type
	       ,proc
	       ,@(gen-args (cdr args)) 
	       (ref.cast 
		  (ref ,func_type) 
		  (struct.get $procedure $entry ,proc)))))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_lightfuncall ...                                  */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_lightfuncall args)
   ;; TODO: implement lightfuncall
   (let ((functy (create-ref-func-type
		    (wasm-type (rtl_lightfuncall-rettype fun))
		    (map wasm-typeof-arg args)))
	 (proc `(ref.cast (ref $procedure-l) ,(gen-reg (car args)))))
      (with-fun-loc fun 
	 `(call_ref ,functy
	     ,proc
	     ,@(gen-args (cdr args))
	     (ref.cast
		(ref ,functy)
		(struct.get $procedure-l $entry ,proc))))))

;*---------------------------------------------------------------------*/
;*    typeof-arg ...                                                   */
;*---------------------------------------------------------------------*/
(define (typeof-arg o)
   (cond
      ((isa? o rtl_reg) (type-id (rtl_reg-type o)))
      ((isa? o rtl_ins)
       (if (rtl_ins-dest o)
	   (typeof-arg (rtl_ins-dest o))
	   'obj))
      (else
       'obj)))

;*---------------------------------------------------------------------*/
;*    wasm-typeof-arg ...                                              */
;*---------------------------------------------------------------------*/
(define (wasm-typeof-arg o)
   (cond
      ((isa? o rtl_reg) (wasm-type (rtl_reg-type o)))
      ((isa? o rtl_ins)
       (if (rtl_ins-dest o)
	   (wasm-typeof-arg (rtl_ins-dest o))
	   '(ref eq)))
      (else
       '(ref eq))))

;*---------------------------------------------------------------------*/
;*    create-ref-func-type ...                                         */
;*---------------------------------------------------------------------*/
(define (create-ref-func-type retty params)
   (let ((sym (gensym (string-append "$functy@" (symbol->string! *module*)))))
      (set! *extra-types*
	 (cons `(type ,sym (func (param ,@params) (result ,retty)))
	    *extra-types*))
      sym))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_apply ...                                         */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_apply args)
   (with-fun-loc fun
      `(call $apply ,@(gen-args args))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_mov ...                                           */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_mov args)
   (gen-reg (car args)))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_new ...                                           */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_new args)
   
   (define (gen-new-tclass fun args type constr)
      (with-access::tclass type (slots)
	 (let* ((clazz (wasm-sym (type-class-name type)))
		(alloc (if (and #f
				(every (lambda (s)
					  (let ((t (slot-type s)))
					     (or (eq? t *long*)
						 (eq? t *bool*)
						 (eq? t *char*)
						 (eq? t *real*))))
				   slots))
			   `(struct.new_default ,clazz)
			   `(struct.new ,clazz
			       ;; header
			       (i64.const 0)
			       ;; widening
			       (global.get $BUNSPEC)
			       ;; class fields
			       ,@(filter-map (lambda (s)
						(unless (>=fx (slot-virtual-num s) 0)
						   (wasm-default-value (slot-type s))))
				    slots)))))
	    (when (pair? constr)
	       (error "gen-expr" "Not supported." (shape constr)))
	    (with-fun-loc fun alloc))))
   
   (with-access::rtl_new fun (type constr)
      (if (isa? type tclass)
	  (gen-new-tclass fun args type constr)
	  (let ((alloc `(struct.new_default
			   ,(wasm-sym (type-class-name type)))))
	     (when (pair? constr)
		(error "gen-expr" "Not supported." (shape constr)))
	     (with-fun-loc fun alloc)))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_cast args)
   (let ((ty (rtl_cast-totype fun)))
      (cond
	 ((eq? (type-id ty) 'obj)
	  (if (eq? (typeof (car args)) ty)
	      (gen-reg (car args))
	      `(ref.cast ,(wasm-type ty #f) ,(gen-reg (car args)))))
	 ((or (eq? (wasm-type ty) 'i32) (eq? (wasm-type ty) 'i64))
	  (gen-reg (car args)))
	 ((and (eq? (type-id ty) 'pair-nil)
	       (memq (typeof-arg (car args)) '(pair nil)))
	  (gen-reg (car args)))
	 (else
	  `(comment ,(string-append "CAST "
			(symbol->string (type-id ty))
			" "
			(symbol->string (type-id (rtl_cast-fromtype fun))))
	      (ref.cast ,(wasm-type ty #f) ,(gen-reg (car args))))))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_cast_null ...                                     */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_cast_null args)
   ;; TODO: NOT IMPLEMENTED
   (with-fun-loc fun `(ref.null none)))

;*---------------------------------------------------------------------*/
;*    Literal wasm                                                     */
;*    strings                                                          */
;*---------------------------------------------------------------------*/
(define *allocated-strings* (make-hashtable))
(define *string-current-offset* 0)

;*---------------------------------------------------------------------*/
;*    allocate-string ...                                              */
;*---------------------------------------------------------------------*/
(define (allocate-string str::bstring)
   ;; Allocate space inside WebAssembly module's linear memory for the string
   ;; and return its offset.
   (let ((info (hashtable-get *allocated-strings* str)))
      (if info
	  info
	  (let* ((length (string-length str))
		 (modname (symbol->string *module*))
		 (hash (integer->string (string-hash str)))
		 (offset (integer->string *string-current-offset*))
		 (section (string->symbol (string-append "$sd" modname hash offset)))
		 (new-info (cons section *string-current-offset*)))
	     (set! *string-current-offset* (+fx *string-current-offset* length))
	     (hashtable-put! *allocated-strings* str new-info)
	     new-info))))

;*---------------------------------------------------------------------*/
;*    gen-string-literal ...                                           */
;*---------------------------------------------------------------------*/
(define (gen-string-literal str::bstring)
   (let* ((info (allocate-string str))
	  (section (car info))
	  (offset (cdr info)))
      `(array.new_data $bstring ,section
	  (i32.const 0)
	  (i32.const ,(string-length str)))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_pragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_pragma args)
   (with-fun-loc fun
      (if (eq? (rtl_pragma-srfi0 fun) 'bigloo-wasm)
	  (let ((format (rtl_pragma-format fun)))
	     (if (string-prefix? "string:" format)
		 (gen-string-literal (substring format 7))
		 (call-with-input-string format read)))
	  ;; TODO: implement pragma default value depending on type
	  ;; FIXME: use throw $unimplemented
	  (wasm-cnst-unspec))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_jumpexit ...                                      */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_jumpexit args)
   (with-fun-loc fun `(throw $bexception ,@(gen-args args))))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_protect ...                                       */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_protect args)
   ;; TODO: correctly initialize exit object
   (with-fun-loc fun '(call $bgl_make_exit)))

;*---------------------------------------------------------------------*/
;*    gen-expr ::rtl_protected ...                                     */
;*---------------------------------------------------------------------*/
(define-method (gen-expr fun::rtl_protected args)
   ;; Strange, nothing to do...
   (gen-reg (car args)))

;*---------------------------------------------------------------------*/
;*    head-loop? ...                                                   */
;*---------------------------------------------------------------------*/
(define (head-loop? hd::block blocks::pair-nil)
   ;; is any of blocks pointing to hd?
   (any (lambda (b)
	   (with-access::block b (succs)
	      (memq hd succs)))
      blocks))

;*---------------------------------------------------------------------*/
;*    split-blocks ...                                                 */
;*---------------------------------------------------------------------*/
(define (split-blocks blocks::pair-nil)
   (with-trace 'wasm "split-blocks"
      (cond
	 ((null? blocks)
	  (trace-item "empty body")
	  blocks)
	 ((head-loop? (car blocks) (cdr blocks))
	  (trace-item "is loop head...")
	  blocks)
	 (else
	  (with-access::block (car blocks) (first)
	     (let loop ((first first)
			(prelude '()))
		(if (null? first)
		    ;; no if in this block!
		    blocks
		    (let ((i (car first)))
		       (with-access::rtl_ins i (fun)
			  (if (or (isa? fun rtl_go)
				  (isa? fun rtl_switch)
				  (isa? fun rtl_ifeq)
				  (isa? fun rtl_ifne))
			      (let* ((go (instantiate::rtl_ins
					    (fun (instantiate::rtl_go
						    (to (car blocks))))
					    (args '())))
				     (nb (instantiate::block
					    (label 0)
					    (succs (list (car blocks)))
					    (first (reverse (cons go prelude))))))
				 (trace-item "splitting first block...")
				 (with-access::block (car blocks) (preds (ofirst first))
				    (set! ofirst first)
				    (set! preds (cons nb preds)))
				 ;; shift all the block labels by one
				 (for-each (lambda (b)
					      (with-access::block b (label)
						 (set! label (+fx label 1))))
				    blocks)
				 (cons nb blocks))
			      (loop (cdr first) (cons i prelude))))))))))))
   
