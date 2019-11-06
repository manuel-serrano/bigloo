;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Ast/glo_decl.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 09:17:44 1996                          */
;*    Last change :  Sun Apr 14 08:27:05 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implement the functions used to declare a global     */
;*    variable (i.e. in the module language compilation). Global       */
;*    function definitions are managed in ast_glob-def                 */
;*    (glo-def.scm).                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_glo-decl
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_args
	    tools_shape
	    tools_dsssl
	    tools_location
	    backend_backend
	    engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_env
	    ast_ident
	    ast_node
	    type_env
	    (module-initialization-id module_module)
	    module_module
	    ast_local)
   (export  (declare-global-sfun!::global id::symbol alias::obj args::obj
	       module::symbol import::symbol class::symbol
	       src::obj srci::obj)
	    (declare-global-svar!::global id::symbol alias::obj module::symbol
	       import::symbol src::obj srci)
	    (declare-global-scnst!::global id::symbol alias::obj module::symbol
	       import::symbol node class::symbol loc)
	    (declare-global-cfun!::global id::symbol alias::obj module::symbol
	       name::bstring type-res::symbol type-args::obj infix?::bool
	       macro::bool srce::obj srci)
	    (declare-global-cvar!::global id::symbol alias::obj
	       name::bstring type-id::symbol macro?::bool src::obj srci)))

;*---------------------------------------------------------------------*/
;*    declare-global-sfun! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function declare a global sfunction. It is used only when   */
;*    compiling module clauses. When a function is defined, this       */
;*    function is _not_used.                                           */
;*---------------------------------------------------------------------*/
(define (declare-global-sfun! id alias args module import class srce srci)
   (trace (ast 3) "declare-global-sfun!: "
      (shape id) " " (shape args) #\newline)
   (cond
      ((dsssl-optional-only-prototype? args)
       (declare-global-opt-sfun! id alias args module import class srce srci))
      ((dsssl-key-only-prototype? args)
       (declare-global-key-sfun! id alias args module import class srce srci))
      (else
       (declare-global-noopt-sfun!  id alias args module import class srce srci))))

;*---------------------------------------------------------------------*/
;*    declare-global-dsssl-sfun! ...                                   */
;*---------------------------------------------------------------------*/
(define (declare-global-dsssl-sfun! opts keys id alias args module import class srce srci)
   (trace (ast 3) "declare-global-dsssl-sfun!: "
	  (shape id) " opts=" (shape opts) " keys=" (shape keys) #\newline)
   (let* ((arity (global-arity args))
	  (export? (or (not (eq? import 'static))
		       (and (memq 'bdb (backend-debug-support (the-backend)))
			    (>=fx *bdb-debug* 3))))
	  (import (if (and (eq? import 'static)
			   (memq 'bdb (backend-debug-support (the-backend)))
			   (>=fx *bdb-debug* 3))
		      'export
		      import))
	  (loc (find-location srce))
	  (loci (find-location/loc srci loc))
	  (args (if (pair? keys)
		    ;; keys are sorted so we have to adjust the actual
		    ;; argument list
		    (let loop ((args args))
		       (cond
			  ((null? args)
			   '())
			  ((eq? (car args) #!key)
			   (cons (car args)
			      (append 
				 keys
				 (loop (drop (cdr args) (length keys))))))
			  (else
			   (cons (car args) (loop (cdr args))))))
		    args))
	  (args-type (let loop ((args   args)
				(res    '()))
			(cond
			   ((null? args)
			    (reverse! res))
			   ((dsssl-named-constant? (car args))
			    (loop (cdr args) res))
			   (else
			    (let* ((a (if (symbol? (car args))
					  (car args)
					  (caar args)))
				   (ty (let ((t (type-of-id/import-location
						 a loc loci)))
					  (if (and (eq? t *_*) export?)
					      *obj*
					      t))))
			       (loop (cdr args) (cons ty res)))))))
	  (args-name (let loop ((args   args)
				(res    '()))
			(cond
			   ((null? args)
			    (reverse! res))
			   ((dsssl-named-constant? (car args))
			    (loop (cdr args) res))
			   (else
			    (let* ((a (if (symbol? (car args))
					  (car args)
					  (caar args)))
				   (vid (fast-id-of-id a loc))
				   (var (if (eq? vid '||)
					    (gensym)
					    vid)))
			       (loop (cdr args) (cons var res)))))))
	  (id-type (parse-id/import-location id loc loci))
	  (type-res (cdr id-type))
	  (id (car id-type))
	  (sfun (instantiate::sfun
		   (arity arity)
		   (args args-type)
		   (args-name args-name)
		   (class class)
		   (optionals opts)
		   (keys keys)))
	  (old (find-global id))
	  (global (bind-global! id alias module sfun import srce)))
      (trace (ast 3) "*** declare-global-dsssl-sfun!: srce: " srce #\Newline)
      (trace (ast 3) "*** declare-global-dsssl-sfun!: loc: " (find-location srce)
	     #\Newline)
      (trace (ast 4) "   declare-global-dsssl-sfun!: (instantiate "
	     (shape arity) " " (shape args-type) " " (shape class) #\Newline)
      ;; we set the type of the function
      (cond
	 ((not (eq? type-res *_*))
	  (global-type-set! global type-res))
	 (export?
	  (global-type-set! global *obj*))
	 (else
	  (global-type-set! global type-res)))
      ;; and we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-opt-sfun! ...                                     */
;*---------------------------------------------------------------------*/
(define (declare-global-opt-sfun! id alias args module import class srce srci)
   (declare-global-dsssl-sfun! (dsssl-optionals args) '()
      id alias args module import class srce srci))

;*---------------------------------------------------------------------*/
;*    declare-global-key-sfun! ...                                     */
;*---------------------------------------------------------------------*/
(define (declare-global-key-sfun! id alias args module import class srce srci)
   (declare-global-dsssl-sfun! '() (dsssl-keys args)
      id alias args module import class srce srci))

;*---------------------------------------------------------------------*/
;*    declare-global-noopt-sfun! ...                                   */
;*---------------------------------------------------------------------*/
(define (declare-global-noopt-sfun! id alias args module import class srce srci)
   (let* ((arity     (global-arity args))
	  (args      (args*->args-list args))
	  (export?   (or (not (eq? import 'static))
			 (and (memq 'bdb (backend-debug-support (the-backend)))
			      (>=fx *bdb-debug* 3))))
	  (import    (if (and (eq? import 'static)
			      (memq 'bdb (backend-debug-support (the-backend)))
			      (>=fx *bdb-debug* 3))
			 'export
			 import))
	  (loc       (find-location srce))
	  (loci      (find-location/loc srci loc))
	  (args-type (let loop ((args   args)
				(res    '())
				(sgfun? (eq? class 'sgfun)))
			(cond
			   ((null? args)
			    (if (>=fx arity 0)
				(reverse! res) 
				(let ((type (car res)))
				   (cond
				      ((or (eq? type *obj*)
					   (eq? type *pair-nil*))
				       (reverse! res))
				      ((eq? type *_*)
				       (reverse! (cons *obj* (cdr res))))
				      (else
				       (user-error id
						   "Illegal nary argument type"
						   (shape type)))))))
			   ((dsssl-named-constant? (car args))
			    (reverse! (cons *obj* res)))
			   (else
			    (let ((type (let ((t (type-of-id/import-location
						  (car args) loc loci)))
					   (if (and (eq? t *_*)
						    (or export? sgfun?))
					       *obj*
					       t))))
			       (loop (cdr args)
				     (cons type res)
				     #f))))))
	  (args-name (let loop ((args args)
				(res '()))
			(cond
			   ((null? args)
			    (reverse! res))
			   ((dsssl-named-constant? (car args))
			    (loop (cdr args) res))
			   (else
			    (let ((a (fast-id-of-id (car args) loc)))
			       (loop (cdr args) (cons a res)))))))
	  (id-type   (parse-id/import-location id loc loci))
	  (type-res  (cdr id-type))
	  (id        (car id-type))
	  (sfun      (instantiate::sfun
			(arity arity)
			(args  args-type)
			(args-name args-name)
			(dsssl-keywords (dsssl-formals args))
			(class class)))
	  (old       (find-global id))
	  (global    (bind-global! id alias module sfun import srce)))
      (trace (ast 3) "*** declare-global-sfun!: srce: " srce #\Newline)
      (trace (ast 3) "*** declare-global-sfun!: loc: " (find-location srce)
	     #\Newline)
      (trace (ast 4) "   declare-global-sfun!: (instantiate "
	     (shape arity) " " (shape args-type) " " (shape class) #\Newline)
      ;; we set the type of the function
      (cond
	 ((not (eq? type-res *_*))
	  (global-type-set! global type-res))
	 (export?
	  (global-type-set! global *obj*))
	 (else
	  (global-type-set! global type-res)))
      ;; and we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-svar! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-global-svar! id alias module import srce srci)
   (let* ((loc (find-location srce))
	  (loci (find-location/loc srci loc))
	  (id-type (parse-id/import-location id loc loci))
	  (type (let ((type (cdr id-type)))
			;; we check that global exported variable are defined
			;; without type or with the obj type.
			(cond
			   ((not (eq? (type-class type) 'bigloo))
			    (user-error id
					"Illegal type for global variable"
					(shape type)
					*_*))
			   ((and (eq? type *_*)
				 (or (memq import '(export import))
				     (and (memq 'bdb
						(backend-debug-support (the-backend)))
					  (>fx *bdb-debug* 0))))
			    *obj*)
			   (else
			    type))))
	  (import (if (and (eq? import 'static)
			      (memq 'bdb (backend-debug-support (the-backend)))
			      (>=fx *bdb-debug* 3))
			 'export
			 import))
	  (id (car id-type))
	  (svar (instantiate::svar))
	  (global (bind-global! id alias module svar import srce)))
      ;; we set the type of the variable
      (global-type-set! global type)
      ;; we now set the access slot
      (global-access-set! global (if (eq? import 'static) 'read 'write))
      ;; we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-scnst! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-global-scnst! id alias module import node class loc)
   (let* ((id-type   (parse-id id loc))
	  (type      (let ((type (cdr id-type)))
			;; we check that global exported variable are defined
			;; without type or with the obj type.
			(cond
			   ((eq? type *_*)
			    (internal-error id
					    "Illegal type for global variable"
					    (shape type)))
			   (else
			    type))))
	  (id        (car id-type))    
	  (scnst     (instantiate::scnst
			(class class)
			(node node)))
	  (global    (bind-global! id alias module scnst import 'a-cnst)))
      ;; we set the type of the variable
      (global-type-set! global type)
      ;; we now set the access slot 
      (global-access-set! global 'read)
      ;; we return the global
      global))

;*---------------------------------------------------------------------*/
;*    declare-global-cfun! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-global-cfun! id alias module name tres-id targs-id infix? macro? srce srci)
   (let* ((arity     (global-arity targs-id))
	  (loc       (find-location srce))
	  (loci      (find-location/loc srci loc))
	  (type-res  (use-foreign-type/import-loc! tres-id loc loci))
	  (type-args (map (lambda (t)
			     (use-foreign-type/import-loc! t loc loci))
			  (args*->args-list targs-id)))
	  (cfun      (instantiate::cfun (arity arity)
		 			(args-type type-args)
					(macro? macro?)
					(infix? infix?)))
	  (global (bind-global! id alias module cfun 'foreign srce)))
      ;; we set the name of the global
      (global-name-set! global name)
      ;; we set the type of the variable
      (global-type-set! global type-res)
      ;; foreign variable can be evaluated
      (global-evaluable?-set! global #f)
      ;; we return the global
      global))
   
;*---------------------------------------------------------------------*/
;*    declare-global-cvar! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-global-cvar! id alias name type-id macro? srce srci)
   (let* ((loc    (find-location srce))
	  (loci   (find-location/loc srci loc))
	  (type   (use-foreign-type/import-loc! type-id loc loci))
	  (cvar   (instantiate::cvar (macro? macro?)))
	  (global (bind-global! id alias 'foreign cvar 'foreign srce)))
      ;; we set the name of the global
      (global-name-set! global name)
      ;; we set the type of the variable
      (global-type-set! global type)
      ;; foreign variable can't be evaluated
      (global-evaluable?-set! global #f)
      ;; we return the global
      global))


   

