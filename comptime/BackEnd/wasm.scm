;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/BackEnd/wasm.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Hubert Gruniaux                                   */
;*    Creation    :  Thu Aug 29 16:30:13 2024                          */
;*    Last change :  Sun Feb  2 07:10:56 2025 (serrano)                */
;*    Copyright   :  2024-25 Hubert Gruniaux and Manuel Serrano        */
;*    -------------------------------------------------------------    */
;*    Bigloo WASM backend driver                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_wasm
   
   (include "Engine/pass.sch"
	    "Tools/location.sch"
	    "Tvector/tvector.sch")
   
   (import engine_param
	   engine_configure
	   tools_license
	   tools_error
	   tools_shape
	   tools_location
	   backend_backend
	   backend_cvm
	   cnst_alloc
	   cc_exec
	   
	   backend_cplib
	   module_module
	   type_type
	   type_cache
	   type_tools
	   type_env
	   object_class
	   object_slots
	   ast_var
	   ast_node
	   ast_env
	   ast_ident
	   ast_occur
	   
	   tvector_tvector
	   tvector_cnst
	   
	   saw_register-allocation
	   
	   ast_type-occur
	   ast_pragma
	   cgen_cop
	   saw_wasm_compile
	   saw_wasm_code
	   saw_defs
	   (emit-bdb-loc cgen_emit-cop)
	   type_tools
	   cnst_node)
   
   (export (build-wasm-backend)))

;*---------------------------------------------------------------------*/
;*    The backend                                                      */
;*---------------------------------------------------------------------*/
(register-backend! 'wasm build-wasm-backend)

;*---------------------------------------------------------------------*/
;*    build-wasm-backend ...                                           */
;*---------------------------------------------------------------------*/
(define (build-wasm-backend)
   (instantiate::wasm
      (language 'wasm)
      (heap-compatible 'c)
      (trace-support #f)
      (srfi0 'bigloo-wasm)
      (foreign-clause-support '(wasm extern))
      (strict-type-cast #t)
      (pragma-support #t)
      (require-tailc #t)
      (typed-eq #t)
      ; TODO: maybe remove these two checks
      (bound-check #t)
      (type-check #t)
      (force-register-gc-roots #f)
      (tvector-descr-support #f)
      (string-literal-support #f)
      (boxed-fixnums (=fx *wasm-fixnum* 64))
      ;; no subtyping for (ref func)
      ;; see https://github.com/WebAssembly/function-references
      (typed-closures #f)
      (varargs #f)
      (mangling #f)
      (local-exit #f)))

;*---------------------------------------------------------------------*/
;*    backend-compile ...                                              */
;*---------------------------------------------------------------------*/
(define-method (backend-compile me::wasm)
   (let ((wat (profile wat (wasm-walk me))))
      (stop-on-pass 'wat (lambda () 'done))
      wat))

;*---------------------------------------------------------------------*/
;*    backend-link ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (backend-link me::wasm result)
   (when (string? result)
      (if (or (eq? *pass* 'cc)
	      (and (string? *dest*) (string=? (suffix *dest*) "wat")))
	  (when (string? *dest*) (rename-file result *dest*))
	  (with-handler
	     (lambda (e)
		(exception-notify e)
		(raise e))
	     (backend-link-objects me (list result))))))

;*---------------------------------------------------------------------*/
;*    backend-link-objects ...                                         */
;*---------------------------------------------------------------------*/
(define-method (backend-link-objects me::wasm sources)
   (let ((wasmas (format "~a ~( )" *wasmas* *wasmas-options*)))
      (verbose 1 "   . Wasm" #\Newline)
      (cond
	 ((and (null? *o-files*) (eq? *pass* 'so))
	  (error "wasm-link" "cannot build a library without object files" sources))
	 ((and (eq? *pass* 'so) (not (pair? *o-files*)) (not (pair? sources)))
	  (error "wasm-link" "cannot build a library with sources" sources))
	 ((eq? *pass* 'so)
	  (if (string=? (suffix *dest*) "wat")
	      (wat-merge *o-files* *dest*)
	      (let ((tmp (make-tmp-file-name (or *dest* "bigloo") "wat")))
		 (wat-merge *o-files* tmp)
		 (let ((cmd (format "~a ~a -o ~a" wasmas tmp *dest*)))
		    (verbose 2 "      [" cmd #\] #\Newline)
		    (unwind-protect
		       (exec cmd #f "wasm-as")
		       (when *rm-tmp-files*
			  (delete-file tmp)))))))
	 ((null? sources)
	  (error "wasm-link" "No source file provided" *dest*))
;* 	 ((pair? (cdr sources))                                        */
;* 	  (error "wasm-link" "More than one source file provided" sources)) */
	 (else
	  (let* ((tmp (make-tmp-file-name (or *dest* "bigloo") "wat"))
		 (lib (if *unsafe-library* "bigloo_u.wat" "bigloo_s.wat"))
		 (runtime-file (find-file-in-path lib *lib-dir*))
		 (runtime-mjs (find-file-in-path "runtime.mjs" *lib-dir*))
		 (srcobj (map (lambda (e) (if (pair? e) (cdr e) e)) sources))
		 (objects (delete-duplicates!
			     (append srcobj *o-files*) string=?)))
	     (verbose 2 "      merging ["
		(format "~a ~( )" runtime-file objects)
		" -> " tmp #\] #\Newline)
	     (wat-merge (cons runtime-file objects) tmp)
	     (let* ((target (or *dest* "a.out"))
		    (wasm (string-append target ".wasm"))
		    (cmd (format "~a ~a -o ~a" wasmas tmp wasm)))
		(verbose 2 "      assembling [" cmd #\] #\Newline)
		(unwind-protect
		   (begin
		      (exec cmd #t "wasm-as")
		      (verbose 2 "      generating [" target #\] #\Newline)
		      (with-output-to-file target
			 (lambda ()
			    (display "#!/bin/sh\n")
			    (display "if [ \"$BIGLOOWASMOPT \" = \" \" ]; then\n")
			    (display "  BIGLOOWASMOPT=--stack-size=8192\n")
			    (display "fi\n")
			    (display* (format "~a ~( ) $BIGLOOWASMOPT "
					 *wasm-engine* *wasm-options*)
			       (if *wasm-unsafe* *wasm-unsafe-options* "")
			       " " runtime-mjs " " wasm " $*")
			    (newline)))
		      (chmod target 'read 'write 'execute))
		   (when *rm-tmp-files*
		      (delete-file tmp)))))))))

;*---------------------------------------------------------------------*/
;*    wat-merge ...                                                    */
;*---------------------------------------------------------------------*/
(define (wat-merge files target)

   (define exports (create-hashtable :weak 'open-string))
   (define imports (create-hashtable :weak 'open-string))
   (define tags (create-hashtable :weak 'open-string))
   (define recs (create-hashtable :weak 'open-string))
   (define types (create-hashtable :weak 'open-string))
   (define exports-whitelist (create-hashtable :weak 'open-string))
   (define initial-order (create-hashtable :weak 'open-string))
   
   (define (collect-exports modules)
      (for-each (lambda (d)
		   (match-case d
		      ((export ?n ???-)
		       (hashtable-put! exports n #t))
		      ((global ?- (export ?n) ???-)
		       (hashtable-put! exports n #t))
		      ((func ?- (export ?n) ???-)
		       (hashtable-put! exports n #t))))
	 modules))

   (define (remove-scheme-imports! modules)
      (filter! (lambda (d)
		  (match-case d
		     ((import ?m ?n ???-)
		      (or (string-prefix? "__js" m)
			  (not (hashtable-contains? exports n))))
		     (else #t)))
	 modules))

   (define (remove-duplicate-imports! modules)
      (filter! (lambda (d)
		  (match-case d
		     ((import ?m ?n ???-)
		      (let ((key (string-append m "@" n)))
			 (unless (hashtable-contains? imports key)
			    (hashtable-put! imports key #t)
			    #t)))
		     (else #t)))
	 modules))

   (define (remove-duplicate-tags! modules)
      (filter! (lambda (d)
		  (match-case d
		     ((tag ?n ???-)
		      (let ((key (symbol->string! n)))
			 (unless (hashtable-contains? tags key)
			    (hashtable-put! tags key #t)
			    #t)))
		     (else #t)))
	 modules))

   (define (remove-duplicate-recs! modules)
      (filter! (lambda (d)
		  (match-case d
		     ((rec . ?c)
		      (let ((key (hash-key-of c)))
			 (unless (hashtable-contains? recs key)
			    (hashtable-put! recs key #t)
			    #t)))
		     (else #t)))
	 modules))

    (define (remove-duplicate-types! modules)
       (filter-map (lambda (d)
		      (match-case d
			 ((type ?n ???-)
			  (let ((key (symbol->string! n)))
			     (unless (hashtable-contains? types key)
				(hashtable-put! types key #t)
				d)))
			 ((rec . ?types)
			  (let ((tys (remove-duplicate-types! types)))
			     (if (pair? tys)
				 `(rec ,@tys)
				 #f)))
			 (else
			  d)))
	  modules))

    (define (remove-exports! modules)
       
       (hashtable-put! exports-whitelist "memory" #t)

       ;; trivial exports
       (filter! (lambda (d)
		   (match-case d
		      ((export ?n ???-) (hashtable-contains? exports-whitelist n))
		      (else #t)))
	  modules)
       
       (for-each (lambda (d)
		    (match-case d
		       ((global ?id (export ?n) . ?r)
			(unless (hashtable-contains? exports-whitelist n)
			   (set-cdr! d (cons id r))))
		       ((func ?id (export ?n) . ?r)
			(unless (hashtable-contains? exports-whitelist n)
			   (set-cdr! d (cons id r))))))
	  modules))
    
    (define (compute-initial-order modules)
       (let ((index 0))
	  (for-each (lambda (d)
		       (let ((key (cer d)))
			  (unless (hashtable-contains? initial-order key)
			     (hashtable-put! initial-order key index)
			     (set! index (+fx index 1)))))
	     modules)))

    (define (sort-modules! modules)
       
       (define orders
	  '((import 0)
	    (memory 1)
	    (type 2)
	    (rec 2)
	    (tag 3)
	    (export 4)
	    (global 5)
	    (data 6)
	    (func 7)))
       
       (define (order-of x)
	  (let ((r (assq (car x) orders)))
	     (if r
		 (cadr r)
		 1000)))
       
       (sort modules
	  (lambda (x y)
	     (let ((xorder (order-of x))
		   (yorder (order-of y)))
		(if (=fx xorder yorder)
		    (<fx (hashtable-get initial-order (cer x))
		       (hashtable-get initial-order (cer y)))
		    (<fx xorder yorder))))))
    
  (define (hash-key-of x)
      (let ((p (open-output-string)))
	 (write x p)
	 (get-output-string p)))
   
   (define (prehash l)
      (when (pair? l)
	 (econs (car l) (cdr l) (hash-key-of l))))
   
   (define (read-module f)
      (if (file-exists? f)
	  (let ((strict (bigloo-strict-r5rs-strings)))
	     (bigloo-strict-r5rs-strings-set! #t)
	     (unwind-protect
		(let ((m (call-with-input-file f read)))
		   (filter-map prehash (if (symbol? (cadr m)) (cddr m) (cdr m))))
		(bigloo-strict-r5rs-strings-set! strict)))
	  (error "wasm" "Cannot find wasm module" f)))

   (define (collect-module modules key)
      (filter (lambda (e) (eq? (car e) key)) modules))

   (define (sort-classes classes)

      (define supers (create-hashtable :weak 'open-string))
      (define depths (create-hashtable :weak 'open-string))
      (define all (create-hashtable :weak 'open-string))

      (define (class-super klass)
	 (hashtable-get supers klass))
      
      (define (class-depth super)
	 (let ((k (hashtable-get depths super)))
	    (cond
	       ((not k) 0)
	       ((>fx k 0) k)
	       (else (+fx 1 (class-depth (class-super super)))))))
      
      ;; mark all the classes
      (for-each (lambda (k)
		   (match-case k
		      ((?- ?n (sub final ?super . ?-))
		       (hashtable-put! all (symbol->string! n) k)
		       (hashtable-put! supers (symbol->string! n)
			  (symbol->string! super))
		       (hashtable-put! depths (symbol->string! n) 0))
		      ((?- ?n (sub ?super . ?-))
		       (hashtable-put! all (symbol->string! n) k)
		       (hashtable-put! supers (symbol->string! n)
			  (symbol->string! super))
		       (hashtable-put! depths (symbol->string! n) 0))))
	 classes)

      ;; compute the depth of each class
      (for-each (lambda (k)
		   (match-case k
		      ((?- ?n (sub final ?super . ?-))
		       (hashtable-put! depths (symbol->string! n)
			  (+fx 1 (class-depth (symbol->string! super)))))
		      ((?- ?n (sub ?super . ?-))
		       (hashtable-put! depths (symbol->string! n)
			  (+fx 1 (class-depth (symbol->string! super)))))))
	 classes)

      ;; sort the classes by depths
      (map (lambda (k)
	      (hashtable-get all k))
	 (sort (lambda (k1 k2)
		  (<fx (class-depth k1) (class-depth k2)))
	    (map (lambda (k) (symbol->string! (cadr k))) classes))))
      
   (define (collect-types modules)
      (filter (lambda (e)
		 (cond
		    ((eq? (car e) 'type) e)
		    ((eq? (car e) 'rec) e)
		    (else #f)))
	 modules))

   (define (split-type-classes types)
      
      (define (class-declaration? ty)
	 (match-case ty
	    ((type ?n (sub final ?- (struct (field $header . ?-) (field $widening . ?-) . ?-)))
	     #t)
	    ((type ?n (sub ?- (struct (field $header . ?-) (field $widening . ?-) . ?-)))
	     #t)
	    (else
	     #f)))
      
      (let loop ((types types)
		 (noklass '())
		 (klass '()))
	 (cond
	    ((null? types)
	     (if (null? klass)
		 (reverse! noklass)
		 (reverse! (cons `(rec ,@(sort-classes klass)) noklass))))
	    ((class-declaration? (car types))
	     (loop (cdr types) noklass (cons (car types) klass)))
	    (else
	     (loop (cdr types) (cons (car types) noklass) klass)))))
   
   (let ((modules (append-map read-module files)))
      (collect-exports modules)
      (remove-scheme-imports! modules)
;*       (remove-duplicate-imports! modules)                           */
;*       (remove-duplicate-tags! modules)                              */
;*       (remove-duplicate-recs! modules)                              */
;*       (remove-duplicate-types! modules)                             */

      ;; MS 30 aug2024, I don't know when the global variable *generate-exe*
      ;; (currently undefined because useless) should be true
      ;; (when *generate-exe* (remove-exports! modules))
      
      ;; (compute-initial-order! modules)
      
      (with-output-to-file target
	 (lambda ()
	    (wasm-pp
	       `(module ,(wasm-module-name target)
		  ,@(remove-duplicate-imports! (collect-module modules 'import))
		  ,@(collect-module modules 'memory)
		  ,@(split-type-classes
		       (remove-duplicate-types! (collect-types modules)))
;* 		  ,@(remove-duplicate-recs! (collect-module modules 'rec)) */
		  ,@(remove-duplicate-tags! (collect-module modules 'tag))
		  ,@(collect-module modules 'export)
		  ,@(collect-module modules 'global)
		  ,@(collect-module modules 'data)
		  ,@(collect-module modules 'func)
		  ;;(sort-modules! modules)
		  )
	       :scheme-string #f)))))

;*---------------------------------------------------------------------*/
;*    wasm-module-name ...                                             */
;*---------------------------------------------------------------------*/
(define (wasm-module-name file)
   (string->symbol (string-append "$" (prefix (basename file)))))

;*---------------------------------------------------------------------*/
;*    type-interference! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (type-interference! back::wasm regs)
   (when (pair? regs)
      (let loop ((regs regs))
	 (when (pair? (cdr regs))
	    (let* ((r1 (car regs))
		   (t1 (rtl_reg-type r1)))
	       (when (type? t1)
		  (for-each (lambda (r2)
			       (let ((t2 (rtl_reg-type r2)))
				  (unless (eq? t1 t2)
				     (interfere-reg! r1 r2))))
		     (cdr regs)))
	       (loop (cdr regs)))))))

;*---------------------------------------------------------------------*/
;*    require-prototye? ...                                            */
;*---------------------------------------------------------------------*/
(define (require-prototype? global)
   (and (or (eq? (global-module global) *module*)
	    (not (eq? (global-import global) 'static)))
	(or (and (eq? (global-module global) *module*)
		 (eq? (global-import global) 'export))
	    (>fx (global-occurrence global) 0)
	    (eq? (global-removable global) 'never))))

;*---------------------------------------------------------------------*/
;*    *defined-ids* ...                                                */
;*    -------------------------------------------------------------    */
;*    Maintain a hashtable of imported globals and functions in WASM   */
;*    as it is illegal to import twice the same item (this occurs in   */
;*    the standard library).                                           */
;*---------------------------------------------------------------------*/
(define *defined-ids* (create-hashtable :weak 'open-string))

;*---------------------------------------------------------------------*/
;*    wasm-walk ...                                                    */
;*---------------------------------------------------------------------*/
(define (wasm-walk me::wasm)
   
   (pass-prelude "Wat"
      (lambda () (start-emission! ".wat")))
   
   (for-each-type! (lambda (t) (type-occurrence-set! t 0)))
   
   (for-each-global!
      (lambda (global)
	 (cond
	    ((and (eq? (global-module global) *module*)
		  (>fx (global-occurrence global) 0))
	     (type-increment-global! global))
	    ((require-prototype? global)
	     (type-increment-global! global)
	     (type-occurrence-increment! (global-type global))
	     (when (sfun? (global-value global))
		(for-each (lambda (a)
			     (cond
				((type? a)
				 (type-occurrence-increment! a))
				((local? a)
				 (type-occurrence-increment! (local-type a)))))
		   (sfun-args (global-value global))))))))
   
   (let ((fixpoint #f))
      (let loop ()
	 (unless fixpoint
	    (set! fixpoint #t)
	    (for-each (lambda (t::tclass)
			 (when (>fx (type-occurrence t) 0)
			    (with-access::tclass t (its-super slots)
			       (when (and its-super (=fx (type-occurrence its-super) 0))
				  (type-occurrence-increment! its-super)
				  (set! fixpoint #f))
			       (for-each (lambda (t::slot)
					    (with-access::slot t (type)
					       (when (=fx (type-occurrence type) 0)
						  (type-occurrence-increment! type)
						  (set! fixpoint #f))))
				  slots))))
	       (get-class-list)))))
   
   ;; Registers functions defined in this module to avoid importing them.
   ;; This is required as some files in the standard library redefine in Scheme
   ;; some C functions used by the C backend.
   (for-each (lambda (fun) 
		(set-variable-name! fun)
		(hashtable-put! *defined-ids* (global-name fun) #t)) 
      (cvm-functions me))
   
   (let ((compiled-funcs (backend-compile-functions me))
	 (classes (filter (lambda (t) (>fx (type-occurrence t) 0)) (get-class-list))))
      
      (hashtable-put! *defined-ids* "BFALSE" #t)
      (hashtable-put! *defined-ids* "BTRUE" #t)
      (hashtable-put! *defined-ids* "BUNSPEC" #t)
      (hashtable-put! *defined-ids* "BOPTIONAL" #t)
      (hashtable-put! *defined-ids* "BKEY" #t)
      (hashtable-put! *defined-ids* "BREST" #t)
      (hashtable-put! *defined-ids* "BEOA" #t)
      
      (with-output-to-port *wasm-port*
	 (lambda ()
	    (wasm-pp
	       `(module ,(wasm-sym (symbol->string *module*))
		   
		   (comment "Imports"
		      (import "__bigloo" "generic_va_call"
			 (func $generic_va_call
			    (param (ref $procedure))
			    (param (ref $vector))
			    (result (ref eq))))
		      (import "__bigloo" "bgl_make_exit"
			 (func $bgl_make_exit
			    (result (ref $exit))))
		      (import "__bigloo" "make_bint"
			 (func $make-bint
			    (result ,(wasm-type *bint*))))
		      (import "__bigloo" "BNIL" (global $BNIL ,(wasm-type *bnil*)))
		      (import "__bigloo" "BFALSE" (global $BFALSE ,(wasm-type *bbool*)))
		      (import "__bigloo" "BTRUE" (global $BTRUE ,(wasm-type *bbool*)))
		      (import "__bigloo" "BUNSPEC" (global $BUNSPEC ,(wasm-type *unspec*)))
		      (import "__bigloo" "BOPTIONAL" (global $BOPTIONAL ,(wasm-type *cnst*)))
		      (import "__bigloo" "BKEY" (global $BKEY ,(wasm-type *cnst*)))
		      (import "__bigloo" "BREST" (global $BREST ,(wasm-type *cnst*)))
		      (import "__bigloo" "BEOA" (global $BEOA ,(wasm-type *cnst*)))
		      (import "__js" "trace" (func $__trace (param i32)))

		      (import "__bigloo" "BGL_FUNPTR_DEFAULT_VALUE" (global $funptr-default-value (ref func)))
		      (import "__bigloo" "BGL_BINT_DEFAULT_VALUE" (global $bint-default-value ,(wasm-type *bint*)))
		      (import "__bigloo" "BGL_BINT8_DEFAULT_VALUE" (global $bint8-default-value (ref $bint8)))
		      (import "__bigloo" "BGL_BINT16_DEFAULT_VALUE" (global $bint16-default-value (ref $bint16)))
		      (import "__bigloo" "BGL_BINT32_DEFAULT_VALUE" (global $bint32-default-value (ref $bint32)))
		      (import "__bigloo" "BGL_BINT64_DEFAULT_VALUE" (global $bint64-default-value (ref $bint64)))
		      (import "__bigloo" "BGL_BUINT8_DEFAULT_VALUE" (global $buint8-default-value (ref $buint8)))
		      (import "__bigloo" "BGL_BUINT16_DEFAULT_VALUE" (global $buint16-default-value (ref $buint16)))
		      (import "__bigloo" "BGL_BUINT32_DEFAULT_VALUE" (global $buint32-default-value (ref $buint32)))
		      (import "__bigloo" "BGL_BUINT64_DEFAULT_VALUE" (global $buint64-default-value (ref $buint64)))
		      (import "__bigloo" "BGL_BCHAR_DEFAULT_VALUE" (global $bchar-default-value ,(wasm-type *bchar*)))
		      (import "__bigloo" "BGL_BUCS2_DEFAULT_VALUE" (global $bucs2-default-value (ref $bucs2)))
		      (import "__bigloo" "BGL_BELONG_DEFAULT_VALUE" (global $belong-default-value (ref $belong)))
		      (import "__bigloo" "BGL_PAIR_DEFAULT_VALUE" (global $pair-default-value (ref $pair)))
		      (import "__bigloo" "BGL_EPAIR_DEFAULT_VALUE" (global $epair-default-value (ref $epair)))
		      (import "__bigloo" "BGL_CELL_DEFAULT_VALUE" (global $cell-default-value (ref $cell)))
		      (import "__bigloo" "BGL_BSTRING_DEFAULT_VALUE" (global $bstring-default-value (ref $bstring)))
		      (import "__bigloo" "BGL_SYMBOL_DEFAULT_VALUE" (global $symbol-default-value (ref $symbol)))
		      (import "__bigloo" "BGL_KEYWORD_DEFAULT_VALUE" (global $keyword-default-value (ref $keyword)))
		      (import "__bigloo" "BGL_VECTOR_DEFAULT_VALUE" (global $vector-default-value (ref $vector)))
		      (import "__bigloo" "BGL_U8VECTOR_DEFAULT_VALUE" (global $u8vector-default-value (ref $u8vector)))
		      (import "__bigloo" "BGL_S8VECTOR_DEFAULT_VALUE" (global $s8vector-default-value (ref $s8vector)))
		      (import "__bigloo" "BGL_U16VECTOR_DEFAULT_VALUE" (global $u16vector-default-value (ref $u16vector)))
		      (import "__bigloo" "BGL_S16VECTOR_DEFAULT_VALUE" (global $s16vector-default-value (ref $s16vector)))
		      (import "__bigloo" "BGL_U32VECTOR_DEFAULT_VALUE" (global $u32vector-default-value (ref $u32vector)))
		      (import "__bigloo" "BGL_S32VECTOR_DEFAULT_VALUE" (global $s32vector-default-value (ref $s32vector)))
		      (import "__bigloo" "BGL_U64VECTOR_DEFAULT_VALUE" (global $u64vector-default-value (ref $u64vector)))
		      (import "__bigloo" "BGL_S64VECTOR_DEFAULT_VALUE" (global $s64vector-default-value (ref $s64vector)))
		      (import "__bigloo" "BGL_F32VECTOR_DEFAULT_VALUE" (global $f32vector-default-value (ref $f32vector)))
		      (import "__bigloo" "BGL_F64VECTOR_DEFAULT_VALUE" (global $f64vector-default-value (ref $f64vector)))
		      (import "__bigloo" "BGL_STRUCT_DEFAULT_VALUE" (global $struct-default-value (ref $struct)))
		      (import "__bigloo" "BGL_CLASS_DEFAULT_VALUE" (global $class-default-value (ref $class)))
		      (import "__bigloo" "BGL_PROCEDURE_DEFAULT_VALUE" (global $procedure-default-value (ref $procedure)))
		      (import "__bigloo" "BGL_PROCEDURE_EL_DEFAULT_VALUE" (global $procedure-el-default-value (ref $procedure-el)))
		      (import "__bigloo" "BGL_MUTEX_DEFAULT_VALUE" (global $mutex-default-value (ref $mutex)))
		      (import "__bigloo" "BGL_CONDVAR_DEFAULT_VALUE" (global $condvar-default-value (ref $condvar)))
		      (import "__bigloo" "BGL_DATE_DEFAULT_VALUE" (global $date-default-value (ref $date)))
		      (import "__bigloo" "BGL_REAL_DEFAULT_VALUE" (global $real-default-value (ref $real)))
		      (import "__bigloo" "BGL_BIGNUM_DEFAULT_VALUE" (global $bignum-default-value (ref $bignum)))
		      (import "__bigloo" "BGL_PORT_DEFAULT_VALUE" (global $port-default-value (ref $port)))
		      (import "__bigloo" "BGL_OUTPUT_PORT_DEFAULT_VALUE" (global $output-port-default-value (ref $output-port)))
		      (import "__bigloo" "BGL_FILE_OUTPUT_PORT_DEFAULT_VALUE" (global $file-output-port-default-value (ref $file-output-port)))
		      (import "__bigloo" "BGL_INPUT_PORT_DEFAULT_VALUE" (global $input-port-default-value (ref $input-port)))
		      (import "__bigloo" "BGL_FILE_INPUT_PORT_DEFAULT_VALUE" (global $file-input-port-default-value (ref $file-input-port)))
		      (import "__bigloo" "BGL_BINARY_PORT_DEFAULT_VALUE" (global $binary-port-default-value (ref $file-input-port)))
		      (import "__bigloo" "BGL_SOCKET_DEFAULT_VALUE" (global $socket-default-value (ref $socket)))
		      (import "__bigloo" "BGL_DATAGRAM_SOCKET_DEFAULT_VALUE" (global $datagram-socket-default-value (ref $datagram-socket)))
		      (import "__bigloo" "BGL_WEAKPTR_DEFAULT_VALUE" (global $weakptr-default-value (ref $weakptr)))
		      (import "__bigloo" "BGL_MMAP_DEFAULT_VALUE" (global $mmap-default-value (ref $mmap)))
		      (import "__bigloo" "BGL_PROCESS_DEFAULT_VALUE" (global $process-default-value (ref $process)))
		      (import "__bigloo" "BGL_CUSTOM_DEFAULT_VALUE" (global $custom-default-value (ref $custom)))
		      (import "__bigloo" "BGL_FOREIGN_DEFAULT_VALUE" (global $foreign-default-value (ref $foreign)))
		      (import "__bigloo" "BGL_DYNAMIC_ENV_DEFAULT_VALUE" (global $dynamic-env-default-value (ref $dynamic-env)))
		      (import "__bigloo" "BGL_EXIT_DEFAULT_VALUE" (global $exit-default-value (ref $exit)))
		      (import "__bigloo" "BGL_OBJECT_DEFAULT_VALUE" (global $object-default-value (ref $BgL_objectz00_bglt)))
		      (import "__bigloo" "BGL_PROCEDURE_EL_EMPTY" (global $procedure-el-empty (ref $procedure-el)))

		      (import "__bigloo" "BGL_CLASS_INSTANCE_DEFAULT_VALUE" (func $BGL_CLASS_INSTANCE_DEFAULT_VALUE (param (ref $class)) (result (ref eq))))
		      ;;(import "$__object" "BGl_classzd2nilzd2zz__objectz00" (func $BGl_classzd2nilzd2zz__objectz00 (param (ref $class)) (result (ref eq))))
		      (import "$__object" "class-nil@__object" (func $class-nil@__object (param (ref $class)) (result (ref eq))))

		      ,@(emit-imports))
		   
		   (comment "Memory" ,@(emit-memory))
		   
		   ;; FIXME: allow custom name for types.wat file
		   (comment "Primitive types"
		      ,@(let ((tname (find-file-in-path "types.wat" *lib-dir*)))
			   (if tname
			       (match-case (call-with-input-file tname read)
				  ((module (? symbol?) . ?body)
				   body)
				  (else
				   (error "wasm" "Illegal module format" tname)))
			       (error "wasm" "Cannot find types.wat file in path"
				  *lib-dir*))))
		   
		   (comment "Class types" ,@(emit-class-types classes))
		   (comment "Extra types" ,@(reverse *extra-types*))
		   (comment "Globals" ,@(emit-prototypes))
		   (comment "Constants" ,@(emit-cnsts))
		   (comment "String data" ,@(emit-strings))
		   (comment "Functions" ,@compiled-funcs))))))
   
   (stop-emission!))

;*---------------------------------------------------------------------*/
;*    *wasm-port* ...                                                  */
;*---------------------------------------------------------------------*/
(define *wasm-port* #f)

;*---------------------------------------------------------------------*/
;*    wasm-pp ...                                                      */
;*---------------------------------------------------------------------*/
(define (wasm-pp l #!key (scheme-string #t))
   
   (define margins
      '#(""
	 "  "
	 "    "
	 "      "
	 "        "
	 "          "
	 "            "
	 "              "
	 "                "
	 "                  "
	 "                    "
	 "                      "
	 "                        "
	 "                          "
	 "                            "
	 "                              "
	 "                                "
	 "                                  "))
   
   (define (ppindent depth)
      (let ((vlen (vector-length margins)))
	 (if (<fx depth vlen)
	     (display (vector-ref margins depth))
	     (begin
		(display (vector-ref margins (-fx vlen 1)))
		(let loop ((depth (-fx depth vlen)))
		   (unless (=fx depth 0)
		      (display "  ")
		      (loop (-fx depth 1))))))))
   
   (define (pp-args l depth)
      (for-each (lambda (n)
		   (newline)
		   (pp n (+fx depth 1)))
	 l))

   (define (char-visible? c)
      (and (char>=? c #\x20) 
	   (char<? c #\x7F) ;; exclude the DEL character (illegal in WASM text format)
	   (not (char=? c #\")) 
	   (not (char=? c #\\))))
   
   (define (xdigit? c)
      (or (and (char>=? c #\0) (char<=? c #\9))
	  (and (char>=? c #\a) (char<=? c #\f))
	  (and (char>=? c #\A) (char<=? c #\F))))
       
   (define (dump-scheme-string s)
      (let ((l (string-length s)))
	 (let loop ((i 0))
	    (when (<fx i l)
	       (let ((c (string-ref s i))
		     (hex "0123456789abcdef"))
		  (cond 
		     ((char-visible? c)
		      (display c)
		      (loop (+fx i 1)))
		     ((char=? c #\")
		      (display "\\22")
		      (loop (+fx i 1)))
		     ((char=? c #\\)
		      (display "\\5c")
		      (loop (+fx i 1)))
		     ((char=? c #\newline)
		      (display "\\0a")
		      (loop (+fx i 1)))
		     (else
		      (let ((n (char->integer c)))
			 (display "\\")
			 (display (string-ref hex (bit-rsh n 4)))
			 (display (string-ref hex (bit-and n #xf)))
			 (loop (+fx i 1))))))))))

   (define (dump-string s)
      (display "\"")
      (if scheme-string
	  (dump-scheme-string s)
	  (display s))
      (display "\""))
   
   (define (pp-arg a)
      (cond
	 ((elong? a) (display a))
	 ((llong? a) (display a))
	 ((bignum? a) (display a))
	 ((string? a) (dump-string a))
	 (else (write a))))
   
   (define (pp-0 l depth)
      (display "(")
      (write (car l))
      (pp-args (cdr l) depth)
      (display ")"))
   
   (define (pp-1 l depth)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (pp-args (cddr l) depth)
      (display ")"))
   
   (define (pp-2 l depth)
      (display "(")
      (write (car l))
      (display " ")
      (pp-arg (cadr l))
      (display " ")
      (pp-arg (car (cddr l)))
      (pp-args (cdr (cddr l)) depth)
      (display ")"))
   
   (define (pp-oneline l)
      (display "(")
      (write (car l))
      (map (lambda (a) (display " ") (pp-arg a)) (cdr l))
      (display ")"))
   
   (define (pp-comment l depth)
      (match-case l
	 ((comment ?comment . ?nodes)
          (display* ";; " comment)
          (for-each (lambda (node) (newline) (pp node depth)) nodes))
	 (else
          (error "wasm-pp" "Illegal form for comment node" l))))

   (define (pp-location l depth)
      (match-case l
	 ((@ ?loc ?node)
          (display ";;@ ")
          (display* (location-fname loc) ":" (location-lnum loc) ":1\n")
          (pp node depth))
	 (else
          (error "wasm-pp" "Illegal form for location node" l))))
   
   (define (pp l depth)
      (ppindent depth)
      (cond 
	 ((pair? l)
	  (case (car l)
	     ((comment) (pp-comment l depth))
	     ((@) (pp-location l depth))
 	     ((import) (pp-2 l depth))
	     ((func) (pp-1 l depth))
	     ((type) (pp-1 l depth))
	     ((sub) (pp-1 l depth))
	     ((global) (pp-1 l depth))
	     ((memory) (pp-oneline l))
	     ((data) (pp-1 l depth))
	     ((elem) (pp-oneline l))
	     ((export) (pp-oneline l))
	     ((param) (pp-oneline l))
	     ((result) (pp-oneline l))
	     ((local) (pp-oneline l))
	     ((field) (pp-oneline l))
	     ((mut) (pp-oneline l))
	     ((i32.const) (pp-oneline l))
	     ((i64.const) (pp-oneline l))
	     ((f32.const) (pp-oneline l))
	     ((f64.const) (pp-oneline l))
	     ((ref) (pp-oneline l))
	     ((ref.null) (pp-oneline l))
	     ((local.get) (pp-oneline l))
	     ((global.get) (pp-oneline l))
	     ((local.set) (pp-1 l depth))
	     ((global.set) (pp-1 l depth))
	     ((br) (pp-oneline l))
	     ((unreachable) (pp-oneline l))
	     ((block) (if (symbol? (cadr l)) (pp-1 l depth) (pp-0 l depth)))
	     ((loop) (if (symbol? (cadr l)) (pp-1 l depth) (pp-0 l depth)))
	     ((call) (pp-1 l depth))
	     ((return_call) (pp-1 l depth))
	     ((call_ref) (pp-1 l depth))
	     ((return_call_ref) (pp-1 l depth))
	     ((struct.new) (pp-1 l depth))
	     ((struct.get) (pp-2 l depth))
	     ((array.get) (pp-1 l depth))
	     ((array.new) (pp-1 l depth))
	     ((array.new_elem) (pp-2 l depth))
	     (else (pp-0 l depth))))
	 ((not l) 'nothing)
	 (else (pp-arg l))))
   
   (pp l 0)
   (newline))

;*---------------------------------------------------------------------*/
;*    *dest-wat* ...                                                   */
;*---------------------------------------------------------------------*/
(define *dest-wat* #f)

;*---------------------------------------------------------------------*/
;*    start-emission! ...                                              */
;*---------------------------------------------------------------------*/
(define (start-emission! suffix)
   (let ((prefix (cond
		    ((and (string? *dest*) (memq *pass* '(wat)))
		     (prefix *dest*))
		    ((and (pair? *src-files*) (string? (car *src-files*)))
		     (prefix (car *src-files*)))
		    ((and (string? *dest*) (eq? *pass* 'ld))
		     (prefix *dest*))
		    ((and (string? *dest*) (eq? *pass* 'so))
		     (prefix *dest*))
		    (else
		     #f))))
      (if (or (eq? *dest* '--to-stdout) (not (string? prefix)))
        (set! *wasm-port* (current-output-port))
        (let ((f-name (string-append prefix suffix)))
	   (set! *dest-wat* f-name)
	   (set! *wasm-port* (open-output-file f-name))
	   (if (not (output-port? *wasm-port*))
	       (error *bigloo-name* "Can't open file for output" f-name)
	       #unspecified)))))

;*---------------------------------------------------------------------*/
;*    stop-emission! ...                                               */
;*---------------------------------------------------------------------*/
(define (stop-emission!)
   (cond
      ((not (output-port? *wasm-port*))
       #f)
      ((eq? *wasm-port* (current-output-port))
       #f)
      (else
       (flush-output-port *wasm-port*)
       (close-output-port *wasm-port*)
       (set! *wasm-port* #f)
       *dest-wat*)))

;*---------------------------------------------------------------------*/
;*    class-refs ...                                                   */
;*---------------------------------------------------------------------*/
(define (class-refs class)
   (filter-map (lambda (slot)
		  (with-access::slot slot (type)
		     (isa? type tclass))) 
      (tclass-slots class)))

;*---------------------------------------------------------------------*/
;*    scc-classes ...                                                  */
;*---------------------------------------------------------------------*/
(define (scc-classes class-list)
   
   (define (splitf-at list f)
      (let helper ((left '())
		   (right list))
	 (if (and (not (null? right)) (f (car right)))
	     (helper (cons (car right) left) (cdr right))
	     (values (reverse left) right))))
   
   (define indexes '())
   (define index 0)
   (define low-links '())
   (define on? '())
   (define stack '())
   (define sccs '())
   
   (define (dfs class)
      (set! indexes (cons (cons class index) indexes))
      (set! low-links (cons (cons class index) low-links))
      (set! on? (cons (cons class #t) on?))
      (set! index (+fx index 1))
      (set! stack (cons class stack))
      
      (for-each (lambda (w) 
		   (let ((found-index (assq w indexes))
			 (found-low-link (assq w low-links))
			 (on-stack? (assq w on?)))
		      (cond
			 ((not found-index)
			  (dfs w)
			  (set! low-links (cons (cons class (min (cdr found-low-link))) low-links)))
			 ((and on-stack? (cdr on-stack?)) 
			  (set! low-links (cons (cons class (min (cdr found-index))) low-links)))))
		   ) 
	 (class-refs class))
      
      (let ((found-index (assq class indexes))
	    (found-low-link (assq class low-links)))
	 (if (and found-index
		  found-low-link
		  (=fx (cdr found-index) (cdr found-low-link)))
	     (multiple-value-bind (scc* stack*)
		(splitf-at stack (lambda (w) (not (eq? w class))))
		(set! stack (cdr stack*))
		(let ((scc (cons (car stack*) scc*)))
		   (for-each (lambda (n) (set! on? (cons (cons n #f) on?))) scc)
		   (set! sccs (cons scc sccs)))))))
   
   (for-each (lambda (n) (when (not (assq n indexes)) (dfs n))) class-list)
   
   sccs)

;*---------------------------------------------------------------------*/
;*    emit-class-types ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-class-types class-list)
   
   ;; Sorts classes such that all classes appear after their super class
   ;; (if any). This is required by WASM: struct types must be defined after
   ;; their supertype.
   (let ((orders (make-hashtable))
	 (current-order 0))
      
      (define (dfs class)
	 (unless (hashtable-contains? orders class)
	    (let ((super (tclass-its-super class)))
	       (when super (dfs super)))
	    (hashtable-put! orders class current-order)
	    (set! current-order (+fx current-order 1))))
      
      (for-each dfs class-list)
      
      (filter-map emit-class-type 
	 (sort (lambda (x y)
		  (<fx (hashtable-get orders x) (hashtable-get orders y)))
	    class-list))))

;*---------------------------------------------------------------------*/
;*    emit-class-type ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-class-type class)
   
   (define (emit-slot slot)
      (with-access::slot slot (type virtual-num name read-only?)
	 (let ((cname (slot-type slot)))
	    (if (>=fx virtual-num 0)
		;; TODO: what to do with virtual-num >= 0
		#f 
		`(field 
		    ,(wasm-sym (slot-name slot))
		    ,(if (and #f read-only?)
			 ;; the current initialization of class
			 ;; instances does not permit to mark
			 ;; class field immutable
			 (wasm-type cname)
			 `(mut ,(wasm-type cname
				   :nullable (wasm-slot-recursive? slot)))))))))

   (define (emit-regular-class class)
      (let ((super (tclass-its-super class))
	    (name (type-class-name class))
	    (struct `(struct
			;; MUST BE the same fields as defined in runtime.types.
			(field $header (mut i64))
			(field $widening (mut (ref eq)))
			,@(filter-map emit-slot (tclass-slots class)))))
	 (if super
	     `(type ,(wasm-sym name) 
		 (sub ,@(if (tclass-final? class) '(final) '())
		    ,(wasm-sym (type-class-name super)) ,struct))
	     #f)))

   (define (emit-wide-class class)
      (let ((name (type-class-name class))
	    (struct `(struct
			,@(filter-map emit-slot
			     (filter (lambda (s)
					(eq? (slot-class-owner s) class))
				(tclass-slots class))))))
	 `(type ,(wasm-sym name) ,struct)))

   (with-access::tclass class (widening)
      (if widening
	  (emit-wide-class class)
	  (emit-regular-class class))))
   
;*---------------------------------------------------------------------*/
;*    emit-prototypes ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-prototypes)
   ;; set the proper name for bigloo-initialized! that is used
   ;; when a main is produced
   (let ((init (find-global 'bigloo-initialized! '__param)))
      (when init (set-variable-name! init)))
   
   (let ((globals '()))
      (let ((cnst-init (get-cnst-table)))
	 (set! globals
	    (cons 
	       `(global ,(cnst-table-sym)
		   (ref $cnst-table)
		   (array.new $cnst-table
		      (global.get $BUNSPEC)
		      (i32.const ,(get-cnst-offset)))) globals)))
      
      (for-each-global!
	 (lambda (global)
	    (if (and (require-prototype? global)
		     (not (scnst? (global-value global)))
		     (not (require-import? global)))
		(let ((prototype (emit-prototype (global-value global) global)))
		   (when prototype (set! globals (cons prototype globals)))))))
      globals))

;*---------------------------------------------------------------------*/
;*    emit-prototype ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (emit-prototype value::value variable::variable) #f)

;*---------------------------------------------------------------------*/
;*    emit-prototype ::svar ...                                        */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::svar variable)
   (emit-prototype/svar/scnst value variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype ::scnst ...                                       */
;*---------------------------------------------------------------------*/
(define-method (emit-prototype value::scnst variable)
   ;; TODO: maybe remove this function as scnst should never be emitted
   ;; as prototypes (see conditions in emit-prototypes).
   (emit-prototype/svar/scnst value variable))

;*---------------------------------------------------------------------*/
;*    emit-prototype/svar/scnst ...                                    */
;*---------------------------------------------------------------------*/
(define (emit-prototype/svar/scnst value variable)
   (with-access::variable variable (type id name pragma)
      (set-variable-name! variable)
      ;; TODO: for now, all global variables are mutable...
      `(global 
	  ,(wasm-sym name)
	  ,@(if (eq? (global-import variable) 'export)
		`((export ,(global-name variable)))
		'())
	  (mut ,(wasm-type type)) ,(wasm-default-value type))))

;*---------------------------------------------------------------------*/
;*    emit-cnsts ...                                                   */
;*---------------------------------------------------------------------*/
(define (emit-cnsts)
   (let ((cnsts '()))
      (for-each-global!
	 (lambda (global)
	    (if (and (require-prototype? global)
		     (scnst? (global-value global)))
		(let ((cnst (emit-cnst (global-value global) global)))
		   (when cnst (set! cnsts (cons cnst cnsts)))))))
      (apply append cnsts)))

;*---------------------------------------------------------------------*/
;*    emit-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (emit-cnst value::scnst variable::global)
   (with-trace 'wasm "Emit CNST"
      (with-access::scnst value (class node)
	 (trace-item "class=" class)
	 (case class
	    ((sinteger)
	     (emit-cnst-integer node variable))
	    ((sreal)
	     (emit-cnst-real node variable))
	    ((selong)
	     (emit-cnst-i64 '$belong node variable))
	    ((sllong)
	     (emit-cnst-i64 '$bllong node variable))
	    ((sint32)
	     (emit-cnst-i32 '$bint32 node variable))
	    ((suint32)
	     (emit-cnst-i32 '$buint32 node variable))
	    ((sint64)
	     (emit-cnst-i64 '$bint64 node variable))
	    ((suint64)
	     (emit-cnst-i64 '$buint64 node variable))
	    ((sstring) 
	     (emit-cnst-string node variable))
	    ((sfun) 
	     (emit-cnst-sfun node variable))
	    ((sgfun)
	     (emit-cnst-sgfun node variable))
	    ((selfun)
	     (emit-cnst-selfun node variable))
	    ((slfun)
	     (emit-cnst-slfun node variable))
	    ((stvector)
	     (emit-cnst-stvector node variable))
	    (else
	     (internal-error "backend:emit-cnst"
                (format "Unknown cnst class \"~a\"" class)
                (shape node)))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-string ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-cnst-string ostr global)
   (set-variable-name! global)
   `((global 
	,(wasm-sym (global-name global))
	,@(if (eq? (global-import global) 'export)
	      `((export ,(global-name global)))
	      '())
	(mut (ref $bstring)) 
	(array.new_fixed $bstring 0))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-real ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-real real global)
   (set-variable-name! global)
   (let ((value 
	    (cond
	       ((nanfl? real) 'nan)
	       ((and (infinitefl? real) (>fl real 0.0)) 'inf)
	       ((infinitefl? real) '-inf)
	       (else real))))
      `((global 
	   ,(wasm-sym (global-name global))
	   ,@(if (eq? (global-import global) 'export)
		 `((export ,(global-name global)))
		 '())
	   (ref $real) 
	   (struct.new $real (f64.const ,value))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-integer ...                                            */
;*---------------------------------------------------------------------*/
(define (emit-cnst-integer integer global)
   (set-variable-name! global)
   (if (backend-boxed-fixnums (the-backend))
       `((global ,(wasm-sym (global-name global))
	    ,@(if (eq? (global-import global) 'export)
		  `((export ,(global-name global)))
		  '())
	    (ref $bint)
	    ;; cannot use $make_bint as wasm requires global
	    ;; variables to be initialized with constants only
	    (struct.new $bint (i64.const ,integer))))
       `((global ,(wasm-sym (global-name global))
	    ,@(if (eq? (global-import global) 'export)
		  `((export ,(global-name global)))
		  '())
	    (ref i31)
	    `(ref.i31 (i32.wrap_i64 (i64.const ,integer)))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i32 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i32 type value global)
   (set-variable-name! global)
   `((global 
	,(wasm-sym (global-name global))
	,@(if (eq? (global-import global) 'export)
	      `((export ,(global-name global)))
	      '())
	(ref ,type)
	(struct.new ,type ,(emit-wasm-atom-value (global-type global) value)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i64 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i64 type value global)
   (set-variable-name! global)
   (let ((val (emit-wasm-atom-value (global-type global) value)))
      `((global 
	   ,(wasm-sym (global-name global))
	   ,@(if (eq? (global-import global) 'export)
		 `((export ,(global-name global)))
		 '())
	   (ref ,type)
	   (struct.new ,type ,val)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun sfun global)
   (emit-cnst-sfun/sgfun sfun global 'procedure))

;*---------------------------------------------------------------------*/
;*    emit-cnst-slfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-slfun fun global)
   (let* ((actuals (app-args fun))
	  (entry (car actuals))
	  (vname (set-variable-name! global))
	  (name (set-variable-name! (var-variable entry))))
      `((global ,(wasm-sym vname)
	   (mut (ref $procedure-l)) 
	   (struct.new $procedure-l 
	      (ref.func ,(wasm-sym name))
	      (ref.null none))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-selfun ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-cnst-selfun fun global)
   (let ((vname (set-variable-name! global)))
      `((global ,(wasm-sym vname) (ref $procedure-el) (global.get $procedure-el-empty)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sgfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sgfun sgfun global)
   (emit-cnst-sfun/sgfun sgfun global 'generic))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun/sgfun ...                                         */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun/sgfun fun global kind)
   (with-trace 'wasm "Emit SFUN"
      (trace-item "type=" (typeof fun))
      (trace-item "name=" (global-name global) " import=" (global-import global))
      ;; TODO: implement SFUN cnst
      (unless (eq? (global-import global) 'import)
	 (let* ((actuals (app-args fun))
		(entry (car actuals))
		(arity (if (or
			    (global-optional? (sfun-the-closure-global (global-value (var-variable entry))))
			    (global-key? (sfun-the-closure-global (global-value (var-variable entry)))))
			   -1
			   (get-node-atom-value (cadr actuals))))
		(vname (set-variable-name! global))
		(name (set-variable-name! (var-variable entry))))
	    `((global 
		 ,(wasm-sym vname)
		 ,@(if (eq? (global-import global) 'export)
		       `((export ,vname))
		       '())
		 (mut (ref $procedure)) 
		 (struct.new $procedure 
		    (ref.func ,(wasm-sym name))
		    ,(wasm-cnst-unspec)
		    (i32.const ,arity)
		    ,(if (eq? kind 'generic)
			 `(array.new_fixed $vector 3
			     ,(wasm-cnst-false)
			     ,(wasm-cnst-false)
			     ,(wasm-cnst-unspec))
			 '(ref.null none)))))))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-stvector ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-cnst-stvector tvec global)
   (let* ((vec (a-tvector-vector tvec))
          (ty (tvec-item-type (a-tvector-type tvec))))
      (set-variable-name! global)
      `((global ,(wasm-sym (global-name global))
	   ,(wasm-type (a-tvector-type tvec))
	   (array.new_fixed
	      ,(wasm-vector-type (a-tvector-type tvec)) ,(vector-length vec)
	      ,@(vector->list
		   (vector-map (lambda (v) (emit-wasm-atom-value ty v)) vec)))))))

;*---------------------------------------------------------------------*/
;*    emit-string-data ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-string-data str info)
   
   (define (split-long-data data block-size result)
      (if (=fx (string-length data) 0)
	  (reverse result)
	  (let ((len (min (string-length data) block-size)))
	     (split-long-data 
		(substring data len) 
		block-size 
		(cons (substring data 0 len) result)))))

   (let ((section (car info))
	 (offset (cdr info)))
;*       `(data ,section ,@(split-long-data str 100 '()))              */
      `(data ,section ,str)))

;*---------------------------------------------------------------------*/
;*    emit-strings ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-strings)
   (hashtable-map *allocated-strings* emit-string-data))

;*---------------------------------------------------------------------*/
;*    emit-imports ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-imports)
   (let ((imports '()))
      (for-each-global! (lambda (global)
			   (when (and (require-import? global) (not (scnst? global)))
			      (let ((import (emit-import (global-value global) global)))
				 (when (and import (not (hashtable-contains? *defined-ids* (global-name global))))
				    (hashtable-put! *defined-ids* (global-name global) #t)
				    (set! imports (cons import imports)))))))
      imports))

;*---------------------------------------------------------------------*/
;*    require-import? ...                                              */
;*---------------------------------------------------------------------*/
(define (require-import? global)
   (let ((value (global-value global))
	 (import (global-import global)))
      (when (>fx (global-occurrence global) 0)
	 (or (eq? import 'import)
	     (and (eq? import 'foreign)
		  (not (and (or (isa? value cvar)
				(isa? value cfun))
			    (not (string-null? (global-qualified-type-name global))))))))))

;*---------------------------------------------------------------------*/
;*    wasm-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (wasm-module variable)
   (let ((name (global-name variable))
	 (library (global-library variable))
	 (module (global-module variable)))
      (let ((is-macro (isa? variable cfun)))
	 (cond
	    (library (symbol->string library))
	    ((not (eq? module 'foreign)) (symbol->string module))
	    (else "__bigloo")))))

;*---------------------------------------------------------------------*/
;*    emit-import ::value ...                                          */
;*---------------------------------------------------------------------*/
(define-generic (emit-import value::value variable::variable)
   (set-variable-name! variable)
   (let ((name (global-name variable)))
      `(import ,(wasm-module variable) ,name
	  (global ,(wasm-sym name)
	     (mut ,(wasm-type (global-type variable)))))))

;*---------------------------------------------------------------------*/
;*    emit-import ::cvar ...                                           */
;*---------------------------------------------------------------------*/
(define-method (emit-import value::cvar variable)
   (set-variable-name! variable)
   `(import ,(wasm-module variable) ,(global-name variable) 
       (global ,(wasm-sym (global-name variable))
	  ,(wasm-type (global-type variable)))))

;*---------------------------------------------------------------------*/
;*    emit-import ::sfun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (emit-import value::sfun variable)
   (emit-import/sfun/cfun
      (map (lambda (arg) (if (local? arg) (local-type arg) arg)) (sfun-args value))
      variable))

;*---------------------------------------------------------------------*/
;*    emit-import ::cfun ...                                           */
;*---------------------------------------------------------------------*/
(define-method (emit-import value::cfun variable)
   (emit-import/sfun/cfun (cfun-args-type value) variable))

;*---------------------------------------------------------------------*/
;*    emit-import/sfun/cfun ...                                        */
;*---------------------------------------------------------------------*/
(define (emit-import/sfun/cfun args-type variable)
   (set-variable-name! variable)
   `(import 
       ,(wasm-module variable)
       ,(global-name variable)
       ,(emit-func-signature args-type variable)))

;*---------------------------------------------------------------------*/
;*    emit-result ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-result t)
   ;; TODO: merge this function with the one defined in code.scm (gen-result)
   (if (eq? (type-id t) 'void)
       '()
       `((result ,(wasm-type t)))))

;*---------------------------------------------------------------------*/
;*    emit-func-signature ...                                          */
;*---------------------------------------------------------------------*/
(define (emit-func-signature args-type variable)
   `(func ,(wasm-sym (global-name variable))
       ,@(map (lambda (type) `(param ,(wasm-type type))) args-type)
       ,@(emit-result (variable-type variable))))

;*---------------------------------------------------------------------*/
;*    emit-memory ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-memory)
   `((import "__bigloo" "memory" (memory 0))))

;*---------------------------------------------------------------------*/
;*    make-tmp-file-name ...                                           */
;*---------------------------------------------------------------------*/
(define (make-tmp-file-name base suffix)
   (make-file-name *bigloo-tmp*
      (string-append 
	 (let ((user (getenv "USER")))
	    (if (not (string? user))
		""
		user))
	 "_"
	 (basename (prefix base))
	 "."
	 suffix)))

;*---------------------------------------------------------------------*/
;*    find-file-in-path ...                                            */
;*---------------------------------------------------------------------*/
(define (find-file-in-path file path)
   (let ((f (find-file/path file path)))
      (if (string? f)
	  f
	  (error "wasm" (format "Cannot find \"~a\" in path" file) path))))
