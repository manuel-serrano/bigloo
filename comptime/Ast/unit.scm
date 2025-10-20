;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Ast/unit.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 08:35:53 1996                          */
;*    Last change :  Mon Oct 20 14:38:26 2025 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A module is composed of several unit (for instance, the user     */
;*    unit (also called the toplevel unit), the foreign unit, the      */
;*    constant unit, ...). This module takes in charge the production  */
;*    of an ast for a unit.                                            */
;*    -------------------------------------------------------------    */
;*    This module does not build node. Nodes can't be until, all       */
;*    body has been scanned and all global definitions seens. This     */
;*    is done only when all unit have been converted.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_unit
   
   (include "Ast/unit.sch"
	    "Ast/node.sch"
	    "Tools/trace.sch")
   
   (import  ast_find-gdefs
	    ast_glo-def
	    ast_ident
	    ast_env
	    ast_local
	    ast_sexp
	    ast_let
	    ast_toplevel
	    backend_backend
	    object_class
	    object_generic
	    object_method
	    tools_progn
	    tools_args
	    tools_misc
	    tools_speek
	    tools_shape
	    tools_location
	    tools_error
	    tools_dsssl
	    engine_param
	    module_module
	    module_class
	    module_include
	    module_impuse
	    type_cache
	    type_env
	    expand_eps)
   
   (export  (unit-sexp*-add! <unit> ::obj)
	    (unit-sexp*-add-head! <unit> ::obj)
	    (unit->defs <unit> ::obj)
	    (unit-initializers ::obj)
	    (unit-initializer-id id)
	    (unit-init-calls)))

;*---------------------------------------------------------------------*/
;*    unit-sexp*-add! ...                                              */
;*---------------------------------------------------------------------*/
(define (unit-sexp*-add! unit sexp)
   (if (null? (unit-sexp* unit))
       (unit-sexp*-set! unit sexp)
       (set-cdr! (last-pair (unit-sexp* unit)) sexp)))

;*---------------------------------------------------------------------*/
;*    unit-sexp*-add-head! ...                                         */
;*---------------------------------------------------------------------*/
(define (unit-sexp*-add-head! unit sexp)
   (unit-sexp*-set! unit (append sexp (unit-sexp* unit))))

;*---------------------------------------------------------------------*/
;*    unit->defs ...                                                   */
;*---------------------------------------------------------------------*/
(define (unit->defs unit genv)
   (verbose 2 "      [" (string-downcase (symbol->string (unit-id unit)))
      "]" #\Newline)
   (when (pair? (unit-sexp* unit))
      (trace (ast 2) "  sexp*=" (map shape (unit-sexp* unit)) #\Newline))
   (let* ((id (unit-id unit))
	  (weight (unit-weight unit))
	  (sexp* (unit-sexp* unit))
	  (gdefs (find-global-defs sexp*)))
      ;; we now compute the global definitions
      (let loop ((aexp* (if (procedure? sexp*)
			    (force sexp*)
			    (toplevel*->ast sexp* gdefs *module* genv)))
		 (init* (if (unit-exported? unit) (list #unspecified) '()))
		 (def*  '()))
	 (if (null? aexp*)
	     (if (pair? init*)
		 (let* ((body (if (unit-exported? unit)
				  `(if (@ ,(unit-require-init-id id) ,*module*)
				       (begin
					  (set! (@ ,(unit-require-init-id id) ,*module*) #f)
					  ,@(initialize-imported-modules
					       (lambda (m)
						  (unit-initializer-id id)))
					  ,@(reverse! init*)))
				  (normalize-progn (reverse! init*))))
			(init (def-global-sfun-no-warning! genv
				 (make-typed-ident
				    (unit-initializer-id id) 'obj)
				 '()
				 '()
				 *module*
				 'snifun
				 *module-clause*
				 'cgen
				 body)))
		    ;; exported units must be declared export
		    (when (unit-exported? unit)
		       (global-import-set! init 'export))
		    ;; all init functions but the toplevel ones are
		    ;; compiler functions.
		    (unless (eq? unit (get-toplevel-unit))
		       (global-user?-set! init #f))
		    ;; we declare the unit for the late module
		    ;; initialization (unit initializer are called
		    ;; in order they are declared).
		    (declare-unit! id weight)
		    ;; init functions cannot be sent to eval
		    (global-evaluable?-set! init #f)
		    ;; we are done, we return now a list of
		    ;; global function definitions.
		    (when (unit-exported? unit)
		       (let* ((id (make-typed-ident (unit-require-init-id id)
				     'obj))
			      (glo (def-global-svar! genv id *module*
				      *module-clause* 'now)))
			  (global-user?-set! glo #f)
			  (global-evaluable?-set! glo #f)))
		    (cons init (reverse! def*)))
		 (reverse! def*))
	     (if (global? (car aexp*))
		 (loop (cdr aexp*) init* (cons (car aexp*) def*))
		 (loop (cdr aexp*) (cons (car aexp*) init*) def*))))))
 
;*---------------------------------------------------------------------*/
;*    *unit-list* ...                                                  */
;*---------------------------------------------------------------------*/
(define *unit-list* '())

;*---------------------------------------------------------------------*/
;*    declare-unit! ...                                                */
;*---------------------------------------------------------------------*/
(define (declare-unit! id::symbol weight::long)
   (if (or (null? *unit-list*) (<fx weight (cdr (car *unit-list*))))
       (set! *unit-list* (cons (cons id weight) *unit-list*))
       (let loop ((ulist *unit-list*))
	  (cond
	     ((<fx weight (cdr (car ulist)))
	      (set-cdr! ulist (cons (car ulist) (cdr ulist)))
	      (set-car! ulist (cons id weight)))
	     ((null? (cdr ulist))
	      (set-cdr! ulist (list (cons id weight))))
	     (else
	      (loop (cdr ulist)))))))

;*---------------------------------------------------------------------*/
;*    unit-initializer-id ...                                          */
;*---------------------------------------------------------------------*/
(define (unit-initializer-id id)
   (symbol-append id '-init))

;*---------------------------------------------------------------------*/
;*    unit-require-init-id ...                                         */
;*---------------------------------------------------------------------*/
(define (unit-require-init-id id)
   (symbol-append id '-require-initialization))

;*---------------------------------------------------------------------*/
;*    unit-initializers ...                                            */
;*---------------------------------------------------------------------*/
(define (unit-initializers genv)
   (map (lambda (unit)
	   (find-global/module genv (unit-initializer-id (car unit)) *module*))
      *unit-list*))

;*---------------------------------------------------------------------*/
;*    unit-init-calls ...                                              */
;*---------------------------------------------------------------------*/
(define (unit-init-calls)
   (map (lambda (unit) `((@ ,(unit-initializer-id (car unit)) ,*module*)))
      *unit-list*))


