(module saw_jvm_compile
   (include "Tvector/tvector.sch"
	    "Tools/trace.sch")
   (import  engine_param
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
	    backend_jvm_class
	    backend_lib
	    backend_cplib
	    saw_defs
	    saw_node2rtl
	    saw_woodcutter
	    saw_expr
	    saw_regset
	    saw_register-allocation
	    saw_bbv
	    saw_jvm_names
	    saw_jvm_out
	    saw_jvm_funcall
	    saw_jvm_code)
   (export  (saw_jvm-compile ::jvm)) )

;;
;; Main function
;;
(define (saw_jvm-compile me::jvm)
   (names-initialization me)
   (for-each (lambda (c) (compile-class me c)) (get-declared-classes))
   (compile-module me) )
   
;;
;; Assembly of a class
;;
(define (compile-class me c)
   (open-class me c (get-its-super c))
   (for-each (lambda (f) (compile-slot me f)) (get-declared-fields c))
   (constructor me)
   (close-class me c) ) 

;;
;; Assembly of a module
;;
(define (compile-module me)
   (open-module me)
   (for-each (lambda (v) (compile-global me v))
	     (get-declared-global-variables *module*) )
   (when (>fx (get-cnst-offset) 0) (compile-global me (get-cnst-table)))
   (if (global? *main*) (module-main me))
   (constructor me)
   (module-funcall/apply me)
   (module-init me)
   (module-dlopen me)
   (module-functions me)
   (module-light-funcall me)
   (close-module me) )

;;
;; the Main function
;;
(define (module-main me)
   (open-lib-method me 'main)
   (declare-locals me '(argv) '())
   (when *jvm-catch*
      (code! me '(handler from catch catch throwable))
      (code! me 'from) )
   (code! me '(aload argv))
   (code! me '(invokestatic listargv))
   (call-global me (find-global 'bigloo_main *module*))
   (code! me '(pop))
   (code! me '(return))
   (when *jvm-catch*
      (code! me 'catch)
      (code! me '(invokestatic internalerror))
      (code! me '(return)) )
   (close-method me) )

;;
;; Instance constructors
;;
(define (constructor me)
   (open-lib-method me 'init)
   (declare-locals me '(this) '())
   (code! me '(aload this))
   (code! me '(invokespecial super-init))
   (code! me '(return))
   (close-method me) )

;;
;; Module constructor
;;
(define (module-init me)
   (open-lib-method me 'clinit)
   (declare-locals me '() '())
   ;;
   (when (> *debug-module* 0)
      (code! me `(ldc ,(symbol->string *module*)))
      (code! me '(invokestatic foreign-print)) )
   ;; For the dam!
   (code! me `(ldc ,(symbol->string *module*)))
   (code! me '(putstatic myname))
   ;; the constants table
   (let ( (size (get-cnst-offset)) )
      (when (>fx size 0)
	 (push-int me size)
	 (code! me '(anewarray obj))
	 (code! me `(putstatic ,(declare-global me (get-cnst-table)))) ))
   ;; CARE do we have to put #unspecified on some global variables
   (for-each (lambda (v) (module-init-cnst me v (global-value v)))
	     (get-global-variables-to-be-initialized *module*) )
   (when *jvm-cinit-module*
      ;; Init the module right now! CARE use a flag
      (code! me '(iconst_0))
      (code! me '(aconst_null))
      (call-global me (find-global 'module-initialization *module*))
      (code! me '(pop)))
   (code! me '(return))
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
	  (code! me '(invokestatic getbytes)) )
	 ((sreal)
	  (push-num me node 'double)
	  (code! me '(invokestatic double_to_real)) )
	 ((selong)
	  (push-num me node 'long)
	  (code! me '(invokestatic elong_to_belong)) )
	 ((sllong)
	  (push-num me node 'long)
	  (code! me '(invokestatic llong_to_bllong)) )
	 ((sint64 suint64)
	  (push-num me node 'long)
	  (code! me '(invokestatic BGL_INT64_TO_BINT64)) )
	 ((sint32 suint32)
	  (push-num me node 'int)
	  (code! me '(invokestatic BGL_INT32_TO_BINT32)) )
	 ((sint16 suint16)
	  (push-num me node 'short)
	  (code! me '(invokestatic BGL_INT16_TO_BINT16)) )
	 ((sint8 suint8)
	  (push-num me node 'byte)
	  (code! me '(invokestatic BGL_INT8_TO_BINT8)) )
	 ((sfun)
	  (code! me '(new me))
	  (code! me '(dup))
	  (code! me '(invokespecial init))
	  (code! me '(dup))
	  (push-int me (indexed-index var))
	  (code! me '(putfield procindex))
	  (code! me '(dup))
	  (push-int me (if (key-opt? var) -1 (global-arity var)))
	  ;(push-int me (get-node-atom-value (cadr (app-args node))))
	  (code! me '(putfield procarity)) )
	 ((selfun)
	  (code! me '(aconst_null)))
	 ((slfun)
	  (code! me '(new me))
	  (code! me '(dup))
	  (code! me '(invokespecial init))
	  (code! me '(dup))
	  (push-int me (indexed-index var))
	  (code! me '(putfield procindex))
	  (code! me '(dup))
	  (push-int me 0)
	  (code! me '(anewarray obj))
	  (code! me '(putfield procenv)) )
	 ((sgfun)
	  (code! me '(new me))
	  (code! me '(dup))
	  (code! me '(invokespecial init))
	  (code! me '(dup))
	  (push-int me (indexed-index var))
	  (code! me '(putfield procindex))
	  (code! me '(dup))
	  (push-int me (if (key-opt? var) -1 (global-arity var)))
	  ;(push-int me (get-node-atom-value (cadr (app-args node))))
	  (code! me '(putfield procarity))
	  (code! me '(dup))
	  (push-int me 3)
	  (code! me '(anewarray obj))
	  (code! me '(putfield procenv)) )
	 ((stvector)
	  (let* ( (vec   (a-tvector-vector node))
		  (type (tvec-item-type (a-tvector-type node)))
		  (id (type-name type))
		  (n (vector-length vec)) )
	     (push-int me n)
	     (case id
		((boolean byte char short int long float double)
		 (code! me `(newarray ,(type-name type))) )
		(else
		 (code! me `(anewarray ,(compile-type me type))) ))
	     (let init ( (i 0) )
		(if (not (=fx i n))
		    (begin
		       (code! me '(dup))
		       (push-int me i)
		       (case id
			  ((boolean byte char short int long float double)
			   (push-num me (vector-ref vec i) id) )
			  (else
			   (push-string me (vector-ref vec i))
			   (code! me '(invokestatic getbytes)) ))
		       (code! me (case id
				    ((boolean byte) '(bastore))
				    ((char) '(castore))
				    ((short) '(sastore))
				    ((int) '(iastore))
				    ((long) '(lastore))
				    ((float) '(fastore))
				    ((double) '(dastore))
				    (else '(aastore)) ))
		       (init (+fx i 1)) )))))
	 (else
	  (internal-error "emit-cnst" "Unknown cnst class" class)) )
      (code! me `(putstatic ,(declare-global me var))) ))

;;
;; Just for Dam!
;;
(define (module-dlopen me)
   (open-lib-method me 'dlopen)
   (declare-locals me '() '())
   (code! me '(iconst_0))
   (code! me '(aconst_null))
   (call-global me (find-global 'module-initialization *module*))
   (code! me '(pop))
   (code! me '(return))
   (close-method me) )

;;
;; Real code
;;
(define (module-functions me)
   (for-each (lambda (v) (module-function me v)) (jvm-functions me)) )

(define (module-function me v)
   (with-access::global v (value)
      (let ( (code (global->blocks me v))
	     (params  (map local->reg (sfun-args value))) )
	 (open-global-method me v)
	 (build-tree me params code)
	 (set! code (register-allocation me v params code))
	 (set! code (bbv me v params code))
	 (module-code me params code)
	 (close-method me) )))
