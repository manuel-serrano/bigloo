;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/labels.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  1 11:37:29 1995                          */
;*    Last change :  Thu Nov  3 14:22:35 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `labels->node' translator                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_labels
   (include "Ast/node.sch"
	    "Tools/trace.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_progn
	    tools_args
	    tools_location
	    tools_dsssl
	    type_cache
	    ast_sexp
	    ast_ident
	    ast_local)
   (export  (labels-sym? ::obj)
	    (labels-sym::symbol)
	    (labels->node::let-fun <sexp> <stack> ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    *labels* ...                                                     */
;*---------------------------------------------------------------------*/
(define *labels* (gensym 'labels))

;*---------------------------------------------------------------------*/
;*    labels-sym ...                                                   */
;*---------------------------------------------------------------------*/
(define (labels-sym)
   *labels*)

;*---------------------------------------------------------------------*/
;*    labels-sym? ...                                                  */
;*---------------------------------------------------------------------*/
(define (labels-sym? sym)
   (eq? sym *labels*))

;*---------------------------------------------------------------------*/
;*    labels->node ...                                                 */
;*---------------------------------------------------------------------*/
(define (labels->node exp stack loc site)
   (let ((loc (find-location/loc exp loc)))
      (match-case exp
         ((?- (and (? pair?) ?bindings) . ?body)
          (let* ((locals (allocate-sfuns bindings loc))
                 (new-stack (append locals stack))
		 (body (sexp->node (normalize-progn body) new-stack loc site))
		 (loc (find-location/loc exp loc)))
	     ;; we compute the ast for all local bodies
	     (for-each (lambda (fun b) (labels-binding fun b new-stack loc))
		       locals
		       bindings)
	     ;; and we allocate the let-fun node
	     (instantiate::let-fun
		(loc loc)
		(type (strict-node-type *_* (node-type body)))
		(locals locals)
		(body body))))
         (else
	  (error-sexp->node "Illegal `labels' expression" exp loc)))))

;*---------------------------------------------------------------------*/
;*    allocate-sfuns ...                                               */
;*---------------------------------------------------------------------*/
(define (allocate-sfuns bindings loc)
   (let loop ((bindings bindings)
	      (res      '()))
      (if (null? bindings)
	  (reverse! res)
	  (let ((src (car bindings)))
	     (match-case src
		(((and (? symbol?) ?fun) ?args . ?body)
		 (let* ((loc (find-location/loc (car bindings) loc))
			(fun (make-local-noopt-sfun loc src fun args body)))
		    (loop (cdr bindings) (cons fun res))))
		(else
		 (error-sexp->node "Illegal `binding' form" src loc)
		 '()))))))

;*---------------------------------------------------------------------*/
;*    make-local-noopt-sfun ...                                        */
;*---------------------------------------------------------------------*/
(define (make-local-noopt-sfun loc src fun args body)
   (let* ((id-type (parse-id fun loc))
	  (id      (car id-type))
	  (type    (cdr id-type))
	  (arity   (local-arity args))
	  (formals (map (lambda (a)
			   (parse-id a loc))
			(dsssl-args*->args-list args))))
      ;; we check that the last formals is correct
      (if (or (>=fx arity 0)
	      ;; empty DSSSL prototype (such as
	      ;; (define (f #!optional) have a negative arity
	      ;; and an empty formals list
	      (dsssl-arity-zero? arity formals)
	      (let* ((larg (car (last-pair formals)))
		     (type (cdr larg)))
		 (cond
		    ((eq? type *obj*)
		     #t)
		    ((eq? type *_*)
		     (set-cdr! larg *obj*)
		     #t)
		    (else
		     #f))))
	  (let* ((args (map (lambda (f)
			       (if (user-symbol? (car f))
				   (make-user-local-svar (car f) (cdr f))
				   (make-local-svar (car f) (cdr f))))
			    formals))
		 (sfun (instantiate::sfun
			  (class 'plain)
			  (arity arity)
			  (loc (find-location/loc body loc))
			  (args args)
			  (args-name (map car formals)))))
	     (if (user-symbol? id)
		 (make-user-local-sfun id type sfun)
		 (make-local-sfun id type sfun)))
	  (begin
	     (error-sexp->node "Illegal formal type" src loc)
	     '()))))

;*---------------------------------------------------------------------*/
;*    labels-binding ...                                               */
;*---------------------------------------------------------------------*/
(define (labels-binding local binding stack loc)
   (match-case binding
      ((?- ?args . ?body)
       (enter-function (local-id local))
       (let* ((loc (find-location/loc binding loc))
	      (body2 (make-dsssl-function-prelude (local-id local)
						  args
						  (normalize-progn body)
						  user-error))
	      (body3 (sexp->node body2
				 (append (sfun-args (local-value local)) stack)
				 loc
				 'value)))
	  (sfun-loc-set! (local-value local) loc)
	  (sfun-body-set! (local-value local) body3)
	  (leave-function)))
      (else
       (error-sexp->node "Illegal `labels' form" binding loc))))
