;*=====================================================================*/
;*    .../bigloo/wasm/bigloo-wasm/comptime/BackEnd/wasm.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Hubert Gruniaux                                   */
;*    Creation    :  Thu Aug 29 16:30:13 2024                          */
;*    Last change :                                                    */
;*    Copyright   :  2024 Hubert Gruniaux and Manuel Serrano           */
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
   
   (export (build-wasm-backend)
	   (wasm-pp code)
	   *wasm-port*))

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
      ; TODO: maybe remove these two checks
      (bound-check #f)
      (type-check #f)
      (force-register-gc-roots #f)
      (string-literal-support #f)))

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
      (backend-link-objects me (list result))))

;*---------------------------------------------------------------------*/
;*    backend-link-objects ...                                         */
;*---------------------------------------------------------------------*/
(define-method (backend-link-objects me::wasm sources)
   (let ((wasmas "wasm-as -all"))
      (verbose 1 "   . Wasmas (" wasmas ")" #\Newline)
      (cond
	 ((and (pair? *o-files*) (not (eq? *pass* 'so)))
	  (error "wasm-link" "cannot build a library without object files" sources))
	 ((and (eq? *pass* 'so) (pair? sources))
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
	 ((pair? (cdr sources))
	  (error "wasm-link" "More than one source file provided" sources))
	 (else
	  (let* ((tmp (make-tmp-file-name (or *dest* "bigloo") "wat"))
		 (runtime-file (find-file/path "bigloo_s.wat" *lib-dir*))
		 (runtime-mjs (find-file/path "runtime.mjs" *lib-dir*)))
	     (wat-merge (cons runtime-file sources) tmp)
	     (let* ((wasm (string-append (prefix (car sources)) ".wasm"))
		    (cmd (format "~a ~a -o ~a" wasmas tmp wasm)))
		(verbose 2 "      [" cmd #\] #\Newline)
		(unwind-protect
		   (let ((target (or *dest* "a.out")))
		      (exec cmd #t "wasm-as")
		      (with-output-to-file target
			 (lambda ()
			    (display "#!/bin/sh\n")
			    (display* "node " runtime-mjs " " wasm " $*")
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
       (filter! (lambda (d)
		   (match-case d
		      ((type ?n ???-)
		       (let ((key (symbol->string! n)))
			  (unless (hashtable-contains? types key)
			     (hashtable-put! types key #t)
			     #t)))
		      (else #t)))
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
      (let ((m (call-with-input-file f read)))
	 (filter-map prehash (if (symbol? (cadr m)) (cddr m) (cdr m)))))

   (let ((modules (append-map read-module files)))
      (collect-exports modules)
      (remove-scheme-imports! modules)
      (remove-duplicate-imports! modules)
      (remove-duplicate-tags! modules)
      (remove-duplicate-recs! modules)
      (remove-duplicate-types! modules)

      ;; MS 30 aug2024, I don't know when the global variable *generate-exe*
      ;; (currently undefined because useless) should be true
      ;; (when *generate-exe* (remove-exports! modules))
      
      (compute-initial-order modules)
      (with-output-to-file target
	 (lambda ()
	    (wasm-pp
	       (cons* 'module (wasm-module-name target)
		  (sort-modules! modules)))))))

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
		      (import "__runtime" "generic_va_call"
			 (func $generic_va_call
			    (param (ref $procedure))
			    (param (ref $vector))
			    (result eqref)))
		      (import "__runtime" "BFALSE" (global $BFALSE i31ref))
		      (import "__runtime" "BTRUE" (global $BTRUE i31ref))
		      (import "__runtime" "BUNSPEC" (global $BUNSPEC i31ref))
		      (import "__runtime" "BOPTIONAL" (global $BOPTIONAL i31ref))
		      (import "__runtime" "BKEY" (global $BKEY i31ref))
		      (import "__runtime" "BREST" (global $BREST i31ref))
		      (import "__runtime" "BEOA" (global $BEOA i31ref))
		      (import "__js" "trace" (func $__trace (param i32)))
		      ,@(emit-imports))
		   
		   (comment "Memory" ,@(emit-memory))
		   
		   ;; FIXME: allow custom name for types.wat file
		   (comment "Primitive types"
		      ,@(let ((tname (find-file/path "types.wat" *lib-dir*)))
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
(define (wasm-pp l)
   
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
   
   (define (dump-string s)
      
      (define (visible? c)
	 (and 
	  (char>=? c #\x20) 
	  (char<? c #\x7F) ;; exclude the DEL character (illegal in WASM text format)
	  (not (char=? c #\")) 
	  (not (char=? c #\\))))
      
      (display "\"")
      (let iter ((i 0))
	 (when (<fx i (string-length s))
	    (let ((c (string-ref s i))
		  (hex "0123456789abcdef"))
	       (cond 
		  ((visible? c) (display c))
		  ((char=? c #\") (display "\\\""))
		  ((char=? c #\\) (display "\\\\"))
		  (else (display* "\\" 
			   (string-ref hex (bit-rsh (char->integer (char-and c #\xF0)) 4))
			   (string-ref hex (char->integer (char-and c #\x0F)))
			   ))))
	    (iter (+fx i 1))))
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

  ;; TODO: emit classes that are mutually dependent in rec groups.
  ;;       for that, compute the elementary cycles in the dependencies graph
  ;;       using the Jordan algorithm, and generate a rec for each cycle.
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
      (with-access::slot slot (type virtual-num name)
	 (let ((cname (slot-type slot)))
	    (if (>=fx virtual-num 0)
		;; TODO: what to do with virtual-num >= 0
		#f 
		`(field 
		    ,(wasm-sym (slot-name slot)) 
		    ;; TODO: consider removing WASM mut qualifier for
		    ;; read-only slots
		    (mut ,(wasm-type cname)))))))
   
   
   (let ((super (tclass-its-super class))
	 (name (type-class-name class))
	 (struct `(struct
		     ;; MUST BE the same fields as defined in runtime.types.
		     (field $header (mut i64))
		     (field $widening (mut eqref))
		     ,@(filter-map emit-slot (tclass-slots class)))))
      (if super
	  `(type
	      ,(wasm-sym name) 
	      (sub ,@(if (tclass-final? class) '(final) '())
		 ,(wasm-sym (type-class-name super)) ,struct))
	  #f)))

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
		   (array.new_default $cnst-table
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
	  (mut ,(wasm-type type)) ,(emit-default-value type))))

;*---------------------------------------------------------------------*/
;*    emit-default-value ...                                           */
;*---------------------------------------------------------------------*/
(define (emit-default-value type)
   (case (type-id type)
      ;; TODO: implement types
      ((bool) '(i32.const 0))
      ((char) '(i32.const 0))
      ((uchar) '(i32.const 0))
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
      ((llong) '(i64.const 0))
      ((float) '(f32.const 0))
      ((double) '(f64.const 0))
      (else 
       (if (foreign-type? type)
	   (error "wasm" "Unknown foreign type for default value." type)
	   '(ref.null none)))))

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
	(mut (ref null $bstring)) 
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
	   ;; FIXME: remove the mut and null qualifiers
	   (mut (ref null $real)) 
	   (struct.new $real (f64.const ,value))))))

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
	(ref ,type) (struct.new ,type ,(emit-wasm-atom-value (global-type global) value)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-i64 ...                                                */
;*---------------------------------------------------------------------*/
(define (emit-cnst-i64 type value global)
   (set-variable-name! global)
   `((global 
	,(wasm-sym (global-name global))
	,@(if (eq? (global-import global) 'export)
	      `((export ,(global-name global)))
	      '())
	(ref ,type) (struct.new ,type ,(emit-wasm-atom-value (global-type global) value)))))

;*---------------------------------------------------------------------*/
;*    emit-cnst-sfun ...                                               */
;*---------------------------------------------------------------------*/
(define (emit-cnst-sfun sfun global)
   (emit-cnst-sfun/sgfun sfun global 'procedure))

;*---------------------------------------------------------------------*/
;*    emit-cnst-slfun ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-cnst-slfun slfun global)
   (emit-cnst-sfun/sgfun slfun global 'procedure))

;*---------------------------------------------------------------------*/
;*    emit-cnst-selfun ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-cnst-selfun fun global)
   (let ((vname (set-variable-name! global)))
      `((global ,(wasm-sym vname) (ref null $vector) (ref.null none)))))

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
		(arity (get-node-atom-value (cadr actuals)))
		(vname (set-variable-name! global))
		(name (set-variable-name! (var-variable entry))))
	    `((global 
		 ,(wasm-sym vname)
		 ,@(if (eq? (global-import global) 'export)
		       `((export ,vname))
		       '())
		 ;; FIXME: should not be mutable and remove the null qualifier
		 (mut (ref null $procedure)) 
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
      `((global
	   ,(wasm-sym (global-name global))
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
      `(data ,section ,@(split-long-data str 100 '()))))

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
      (and (or 
	    (eq? import 'import)
	    (and 
	     (eq? import 'foreign)
	     (not (and (or (isa? value cvar) (isa? value cfun)) (not (string-null? (global-jvm-type-name global)))))))
	   (>fx (global-occurrence global) 0))))

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
	    (else "__runtime")))))

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
   `((import "__runtime" "memory" (memory 0))))

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

