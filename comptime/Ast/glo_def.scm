;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Ast/glo_def.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 09:17:44 1996                          */
;*    Last change :  Tue Apr  7 16:12:04 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements the functions used to def (define) a      */
;*    global variable (i.e. in the module language compilation).       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_glo-def
   (include "Tools/trace.sch")
   (import  type_type
	    backend_backend
	    ast_var
	    ast_node
	    ast_env
	    ast_ident
	    ast_glo-decl
	    ast_remove
	    type_env
	    type_cache
	    tools_args
	    tools_error
	    tools_shape
	    tools_location
	    tools_dsssl
	    object_class
	    engine_param)
   (export  (def-global-sfun-no-warning!::global id::symbol
	       args::obj
	       locals::obj
	       module::symbol
	       import::symbol
	       src::obj
	       removable::symbol
	       body)
	    (def-global-sfun!::global id::symbol
	       args::obj
	       locals::obj
	       module::symbol
	       import::symbol
	       src::obj
	       removable::symbol
	       body)
	    (def-global-svar!::global id::symbol
	       module::symbol
	       src::obj
	       removable::symbol)
	    (def-global-scnst!::global id::symbol
	       module::symbol
	       node
	       class::symbol
	       loc)
	    (check-method-definition::bool id args locals src)))

;*---------------------------------------------------------------------*/
;*    def-global-sfun-no-warning! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function is a simple interface for DEF-GLOBAL-SFUN. It      */
;*    prevent the global declaration from emitting warning.            */
;*---------------------------------------------------------------------*/
(define (def-global-sfun-no-warning! id args loc mod class src-exp rem node)
   (let ((warning (bigloo-warning)))
      (bigloo-warning-set! 0)
      (let ((fun (def-global-sfun! id args loc mod class src-exp rem node)))
	 (bigloo-warning-set! warning)
	 fun)))
   
;*---------------------------------------------------------------------*/
;*    def-global-sfun! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function defines a global sfunction. It is used only when   */
;*    compiling a define expression.                                   */
;*---------------------------------------------------------------------*/
(define (def-global-sfun! id args locals module class src-exp rem node)
   (trace (ast 3) "def-global-sfun!: "
	  (shape id) " " (shape args) " " (shape locals) #\Newline
	  "    src: " src-exp #\Newline
	  "    loc: " (shape (find-location src-exp)) #\newline)
   (enter-function id)
   (let* ((loc        (find-location src-exp))
	  (id-type    (parse-id id loc))
	  (type-res   (cdr id-type))
	  (id         (car id-type))
	  (import     (if (and (>=fx *bdb-debug* 3)
			       (memq 'bdb (backend-debug-support (the-backend))))
			  'export
			  'static))
	  (old-global (find-global/module id module))
	  (global     (cond
			 ((not (global? old-global))
			  (declare-global-sfun! id #f args module
			     import class src-exp #f))
			 (else
			  (check-sfun-definition old-global type-res
			     args locals class src-exp))))
	  (def-loc    (find-location src-exp)))
      (if (sfun? (global-value global))
	  ;; if global-value is not an sfun then it means that
	  ;; check-sfun-definition has failed and so there is an error
	  ;; (already reported) that will stop the compilation. we just
	  ;; return the global variable without any additional check.
	  (begin
	     ;; we set the type of the function
	     (most-defined-type! global type-res)
	     ;; and the type of the formals
	     (if (=fx (length locals)
		      (length (sfun-args (global-value global))))
		 (let ((types (map (lambda (a)
				      (cond
					 ((local? a)
					  (local-type a))
					 ((type? a)
					  a)
					 (else
					  (internal-error
					   "check-method-definition"
					   "unexpected generic arg"
					   (shape a)))))
				   (sfun-args (global-value global)))))
		    (for-each most-defined-type! locals types)))
	     ;; we set the removable field
	     (remove-var-from! rem global)
	     ;; we set the body field
	     (sfun-body-set! (global-value global) node)
	     ;; we set the arg field
	     (sfun-args-set! (global-value global) locals)
	     ;; we set the define location for this function
	     (sfun-loc-set! (global-value global) def-loc)
	     ;; and we return the global
	     (leave-function)))
      global))

;*---------------------------------------------------------------------*/
;*    check-sfun-definition ...                                        */
;*---------------------------------------------------------------------*/
(define (check-sfun-definition::global old type-res args locals class src-exp)
   (trace (ast 3) "check-sfun-definition: " (shape old) " "
	  (shape args) " " (shape locals) #\newline)
   (let ((old-value (global-value old)))
      (cond
	 ((not (sfun? old-value))
	  (mismatch-error old src-exp
	     "(not declared as function)"))
	 ((not (eq? (sfun-class old-value) class))
	  (mismatch-error old src-exp
	     (format "(declared as function of another class (~a/~a))"
		(sfun-class old-value) class)))
	 ((not (=fx (sfun-arity old-value) (global-arity args)))
	  (mismatch-error old src-exp "(arity differs)"))
	 ((not (compatible-type? (eq? 'sgfun class)
				 type-res
				 (global-type old)))
	  (mismatch-error old
			  src-exp
			  "(incompatible function type result)"))
	 ((dsssl-prototype? args)
	  (cond
	     ((dsssl-optional-only-prototype? args)
	      (if (equal? (dsssl-optionals args) (sfun-optionals old-value))
		  old
		  (mismatch-error
		     old src-exp
		     "(incompatible DSSSL #!optional prototype)")))
	     ((dsssl-key-only-prototype? args)
	      (if (equal? (dsssl-keys args) (sfun-keys old-value))
		  old
		  (mismatch-error
		     old src-exp
		     "(incompatible DSSSL #!key prototype)")))
	     ((not (equal? (sfun-dsssl-keywords old-value)
		      (dsssl-formals args)))
	      (mismatch-error old src-exp "(incompatible Dsssl prototype)"))))
	 (else
	  (let loop ((locals locals)
		     (types  (map (lambda (a)
				     (cond
					((local? a)
					 (local-type a))
					((type? a)
					 a)
					(else
					 (internal-error "check-method-definition"
					    "unexpected generic arg"
					    (shape a)))))
				(sfun-args old-value))))
	     (cond
		((null? locals)
		 ;; we save the definition for a better location in
		 ;; the source file.
		 (if (null? types)
		     (global-src-set! old src-exp)
		     (mismatch-error old src-exp "(arity differs)")))
		((or (null? types)
		     (not (compatible-type? #f
			     (local-type (car locals))
			     (car types))))
		 (mismatch-error old src-exp "(incompatible formal type)"))
		(else
		 (loop (cdr locals) (cdr types)))))))
      old))
	 
;*---------------------------------------------------------------------*/
;*    def-global-scnst! ...                                            */
;*---------------------------------------------------------------------*/
(define (def-global-scnst! id module node class loc)
   (enter-function id)
   (let* ((id-type    (parse-id id loc))
	  (id-id      (car id-type))
	  (old-global (find-global/module id-id module))
	  (global     (declare-global-scnst! id #f module
			 'static node class loc)))
      ;; we set the removable field
      (remove-var-from! 'now global)
      ;; and we return the global
      (leave-function)
      global))

;*---------------------------------------------------------------------*/
;*    def-global-svar! ...                                             */
;*---------------------------------------------------------------------*/
(define (def-global-svar! id module src-exp rem)
   (let* ((loc (find-location src-exp))
	  (id-type (parse-id id loc))
	  (id-id (car id-type))
	  (old-global (find-global/module id-id module))
	  (import (if (and (>=fx *bdb-debug* 3)
			   (memq 'bdb (backend-debug-support (the-backend))))
		      'export
		      'static))
	  (type (let ((type (cdr id-type)))
		   ;; we check that global exported variable are defined
		   ;; without type or with the obj type.
		   (if (not (eq? (type-class type) 'bigloo))
		       (user-error id-id
			  "Illegal type for global variable"
			  (shape type))
		       type)))
	  (global (cond
		     ((not (global? old-global))
		      (declare-global-svar! id #f module
			 import src-exp #f))
		     (else
		      (check-svar-definition old-global type src-exp))))
	  (def-loc (find-location src-exp)))
      ;; we set the type of the variable
      (most-defined-type! global type)
      ;; we set the location
      (if (svar? (global-value global))
	  ;; because of errors `global-value' may not be an svar
	  (svar-loc-set! (global-value global) def-loc))
      ;; we set the removable field
      (remove-var-from! rem global)
      global))

;*---------------------------------------------------------------------*/
;*    check-svar-definition ...                                        */
;*---------------------------------------------------------------------*/
(define (check-svar-definition::global old type src-exp)
   (let ((old-value (global-value old)))
      (cond
	 ((not (svar? old-value))
	  (mismatch-error old src-exp "(not declared as a variable)"))
	 ((not (compatible-type? #f type (global-type old)))
	  (mismatch-error old src-exp "(incompatible variable type)"))
	 (else
	  old))))
      
;*---------------------------------------------------------------------*/
;*    compatible-type? ...                                             */
;*---------------------------------------------------------------------*/
(define (compatible-type? sub? new::type old::type)
   (or (eq? new *_*)
       (eq? old new)
       (and sub?
	    (or (type-subclass? new old)
		(and (tclass? new) (eq? old *obj*))))))

;*---------------------------------------------------------------------*/
;*    mismatch-error ...                                               */
;*---------------------------------------------------------------------*/
(define (mismatch-error::global global::global src-exp . add-msg)
   (let ((msg "Prototype and definition don't match"))
      (user-error (if (pair? add-msg)
		      (string-append msg " " (car add-msg))
		      msg)
		  (shape (global-src global))
		  src-exp
		  global)))

;*---------------------------------------------------------------------*/
;*    most-defined-type! ...                                           */
;*---------------------------------------------------------------------*/
(define (most-defined-type! var::variable new-type::type)
   (let ((old-type (variable-type var)))
      (if (eq? old-type *_*)
	  (variable-type-set! var new-type))))

;*---------------------------------------------------------------------*/
;*    check-method-definition ...                                      */
;*---------------------------------------------------------------------*/
(define (check-method-definition id args locals src)
   (let* ((loc       (find-location src))
	  (type-res  (type-of-id id loc))
	  (method-id (id-of-id id loc))
	  (generic   (find-global id)))
      (cond
	 ((null? args)
	  (user-error id (shape src) "argument missing")
	  #t)
	 ((not (global? generic))
	  ;; this error will be signaled later hence for now, we just
	  ;; return #t, as for no error
	  #t)
	 (else
	  (let ((generic-value (global-value generic)))
	     (cond
		((not (sfun? generic-value))
		 (mismatch-error generic src "(generic not found for method)")
		 #f)
		((not (eq? (sfun-class generic-value) 'sgfun))
		 (mismatch-error generic src "(generic not found for method)")
		 #f)
		((not (=fx (sfun-arity generic-value) (global-arity args)))
		 (mismatch-error generic src "(arity differs)")
		 #f)
		((not (compatible-type? #t type-res (global-type generic)))
		 (mismatch-error generic
		    src
		    "(incompatible function type result)")
		 #f)
		((let loop ((locals locals)
			    (types  (map (lambda (a)
					    (cond
					       ((local? a)
						(local-type a))
					       ((type? a)
						a)
					       (else
						(internal-error
						   "check-method-definition"
						   "unexpected generic arg"
						   (shape a)))))
				       (sfun-args generic-value)))
			    (sub?   #t))
		    (cond
		       ((null? locals)
			#t)
		       ((null? types)
			(mismatch-error generic
			   src
			   "(incompatible formal type)"))
		       ((not (compatible-type? sub?
				(local-type (car locals))
				(car types)))
			(mismatch-error generic
			   src
			   "(incompatible formal type)")
			#f)
		       (else
			(loop (cdr locals)
			   (cdr types)
			   #f)))))
		(else
		 #t)))))))
       
