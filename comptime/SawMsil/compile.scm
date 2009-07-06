(module msil_compile
   (include "Tvector/tvector.sch")
   (import engine_param
	   module_module
	   type_type
	   type_cache
	   type_env
	   ast_node
	   ast_var
	   ast_env
	   object_class
	   object_slots
	   cnst_node
	   cnst_alloc
	   tvector_tvector
	   tools_error
	   read_jvm
	   backend_backend
	   backend_bvm
	   backend_dotnet_class
	   backend_lib
	   backend_cplib
	   saw_defs
	   saw_node2rtl
	   saw_woodcutter
	   saw_expr
	   saw_register-allocation
	   msil_names
	   msil_out
	   msil_funcall
	   msil_maxstack
	   msil_code)
   (export (msil-compile ::dotnet ::output-port)) )

;;
;; Main function
;;
(define (msil-compile me::dotnet out::output-port)
   (dotnet-out-set! me out)
;*    (for-each-type! reset-type!)                                     */
   (for-each-global! reset-global!)
   (for-each-type! msil-set-type-names!)
   (for-each-global! msil-set-global-names!)
   (open-namespace me)
   (compile-module me)
   (for-each (lambda (c) (compile-class me c)) (get-declared-classes))
   (close-namespace me) )
   

;;
;; Assembly of a class
;;
(define (compile-class me c)
   (define (unclass c)
      (let ( (name (type-name c)) )
	 (substring name 6 (string-length name)) ))
   (let ( (cname (unclass c)) (sname (unclass (get-its-super c))) )
      ;; CARE Why we don't have private classes (and field).
      (open-class me cname sname #t)
      (for-each (lambda (f) (declare-field me f)) (get-declared-fields c))
      (constructor me 0 sname)
      (close-class me) ))

;;
;; Assembly of a module
;;
(define (compile-module me)
   (let ( (super "bigloo.procedure") )
      (open-class me (dotnet-qname me) super #t)
      (for-each (lambda (v) (declare-global me v))
		(get-declared-global-variables *module*) )
      (when (>fx (get-cnst-offset) 0) (declare-global me (get-cnst-table)))
      (if (global? *main*) (module-main me))
      (constructor me 0 super)
      (constructor me 2 super)
      (constructor me 3 super)
      (module-funcall/apply me)
      (module-init me)
      (module-functions me)
      (close-class me) ))

;; the Main function
(define (module-main me)
   (let ( (cont "_return") )
      (open-method me '(public static) 'void "Main" '(fstrings))
      (declare-entrypoint me)
      (declare-maxstack me 1)
      (print-info me "Main")
      (libcall me 'void 'bigloo.realcallback 'init '())
      (open-try me)
      (load-par me 0)
      (libcall me 'obj 'bigloo.foreign 'listargv '(fstrings))
      (call-global me (find-global 'bigloo_main *module*))
      (pop me)
      (open-catch me "System.Exception" cont)
      (libcall me 'void 'bigloo.foreign 'internalerror '(exception))
      (close-try me cont)
      (return me)
      (close-method me) ))

;; instance constructors
(define (constructor me n super)
   (let ( (l (make-list n 'int)) )
      (open-method-constructor me l)
      (declare-maxstack me (+fx n 1))
      ;(print-info me (string-append ".ctor -> " (symbol->string *module*)))
      (let push ( (i 0) )
	 (load-par me i)
	 (unless (=fx i n) (push (+fx i 1))) )
      (call-super-constructor me super l)
      (return me)
      (close-method me) ))

;; module constructor
(define (module-init me)
   (open-method-class-constructor me)
   (declare-maxstack me 4)
   (print-info me (string-append "..ctor -> " (symbol->string *module*)))
   ;; the constants table
   (let ( (size (get-cnst-offset)) )
      (when (>fx size 0)
	 (push-int me size)
	 (newarray me 'obj)
	 (store-global me (get-cnst-table)) ))
;      (for-each (lambda (v)
;		   ; CARE #unspecified is better ?
;		   (fprint out "\tldnull")
;		   (fprint out "\tstdfld\t" (global-fullname v)) )
;		(msil-initsvars me) )
   (for-each (lambda (v) (module-init-cnst me v (global-value v)))
	     (get-global-variables-to-be-initialized *module*) )
   (return me)
   (close-method me) )

(define (key-opt? v)
   (let ( (v (global-entry v)) )
      (let ( (val  (global-value v)) )
	 (when (sfun? val)
	    (let ( (clo (sfun-the-closure-global val)) )
	       (let ( (o (global-optional? clo)) (k (global-key? clo)) )
		      (or o k) ))))))

(define (module-init-cnst me var val)
   (with-access::scnst val (class node)
      (case class
	 ((sstring)
	  (push-string me node)
	  (libcall me 'bstring 'bigloo.foreign 'getbytes '(fstring)) )
	 ((sreal)
	  (push-num me node 'double)
	  (newobj me "bigloo.real" '(double)) )
	 ((selong)
	  (push-num me node 'elong)
	  (newobj me "bigloo.belong" '(elong)) )
	 ((sllong)
	  (push-num me node 'llong)
	  (newobj me "bigloo.bllong" '(llong)) )
	 ((sfun)
	  (let* ( (actuals (app-args node))
		  (arity (get-node-atom-value (cadr actuals)))
		  (index (indexed-index var)) )
	     (push-int me index)
	     (push-int me (if (key-opt? var) -1 (global-arity var)))
	     ;(push-int me arity)
	     (newobj me (dotnet-name me) '(int int)) ))
	 ((sgfun)
	  (let* ((actuals (app-args node))
		 (arity (get-node-atom-value (cadr actuals)))
		 (index (indexed-index var)) )
	     (push-int me index)
	     (push-int me (if (key-opt? var) -1 (global-arity var)))
	     ;(push-int me arity)
	     (push-int me 3)
	     (newobj me (dotnet-name me) '(int int int)) ))
	 ((stvector)
	  (let* ( (vec   (a-tvector-vector node))
		  (type (tvec-item-type (a-tvector-type node)))
		  (id (type-id type))
		  (n (vector-length vec)) )
	     (push-int me n)
	     (newarray me type)
	     (let init ( (i 0) )
		(if (not (=fx i n))
		    (begin
		       (dup me)
		       (push-int me i)
		       (case id
			  ((bool byte char ucs2 short int long float double)
			   (push-num me (vector-ref vec i) id) )
			  (else
			   (push-string me (vector-ref vec i))
			   (libcall me 'bstring 'bigloo.foreign 'getbytes '(fstring)) ))
		       ;(push-num me (vector-ref vec i) id)
		       (store-vector me type)
		       (init (+fx i 1)) )))))
	 (else
	  (internal-error "emit-cnst" "Unknown cnst class" class)) )
      (store-global me var) ))

;;
;; Real code
;;
(define (module-functions me)
   (for-each (lambda (v) (module-function me v)) (dotnet-functions me)) )

(define (module-function me v)
   (open-global-method me v)
   (let ( (code (global->blocks me v)) (args (sfun-args (global-value v))) )
      (let ( (params  (map local->reg args)) )
	 (build-tree me params code)
	 (set! code (register-allocation me v params code))
	 (declare-maxstack me (maxstack code))
	 (module-code me v params code) ))
   (close-method me) )
