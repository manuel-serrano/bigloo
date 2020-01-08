;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Trace/isloop.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  8 13:51:43 2020                          */
;*    Last change :  Wed Jan  8 16:29:52 2020 (serrano)                */
;*    Copyright   :  2020 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    implementation of the isloop? predicate                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module trace_isloop
   (include "Ast/node.sch"
	    "Tools/location.sch")
   (import  ast_let
	    ast_dump
	    tools_shape
	    tools_error
	    tools_misc
	    tools_location
	    type_env
	    backend_backend
	    ast_sexp
	    ast_ident
	    module_module
	    engine_param
	    (mark-symbol-non-user! ast_ident)
	    (find-global ast_env)
	    (find-location tools_location))
   (export  (isloop?::bool ::let-fun)))

;*---------------------------------------------------------------------*/
;*    isloop? ...                                                      */
;*---------------------------------------------------------------------*/
(define (isloop? node::let-fun)
   (with-access::let-fun node (body locals)
      (when (and (pair? locals) (null? (cdr locals)))
	 (when (isa? body app)
	    (when (tail? body (car locals) #t)
	       (let ((fun (local-value (car locals))))
		  (with-access::sfun fun (body)
		     (tail? body (car locals) #t))))))))

;*---------------------------------------------------------------------*/
;*    tail? ::node ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (tail? node::node fun::variable tailp)
   #f)

;*---------------------------------------------------------------------*/
;*    tail? ::literal ...                                              */
;*---------------------------------------------------------------------*/
(define-method (tail? node::literal fun tailp)
   #t)

;*---------------------------------------------------------------------*/
;*    tail? ::kwote ...                                                */
;*---------------------------------------------------------------------*/
(define-method (tail? node::kwote fun tailp)
   #t)

;*---------------------------------------------------------------------*/
;*    tail? ::var ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (tail? node::var fun tailp)
   (with-access::var node (variable)
      (not (eq? variable fun))))

;*---------------------------------------------------------------------*/
;*    tail? ::sequence ...                                             */
;*---------------------------------------------------------------------*/
(define-method (tail? node::sequence fun tailp)
   (with-access::sequence node (nodes)
      (let loop ((nodes nodes))
	 (cond
	    ((null? nodes) #t)
	    ((null? (cdr nodes)) (tail? (car nodes) fun #t))
	    (else (and (tail? (car nodes) fun #f) (loop (cdr nodes))))))))

;*---------------------------------------------------------------------*/
;*    tail? ::app ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (tail? node::app fun tailp)
   (with-access::app node ((f fun) args)
      (when (every (lambda (arg) (tail? arg fun #f)) args)
	 (if (isa? f var)
	     (with-access::var f (variable)
		(or (not (eq? f fun)) tailp))
	     (tail? f fun #f)))))

;*---------------------------------------------------------------------*/
;*    tail? ::funcall ...                                              */
;*---------------------------------------------------------------------*/
(define-method (tail? node::funcall fun tailp)
   (with-access::funcall node ((f fun) args)
      (when (every (lambda (arg) (tail? arg fun #f)) args)
	 (tail? f fun #f))))

;*---------------------------------------------------------------------*/
;*    tail? ::setq ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (tail? node::setq fun tailp)
   (with-access::setq node (var value)
      (tail? value fun #f)))

;*---------------------------------------------------------------------*/
;*    tail? ::conditional ...                                          */
;*---------------------------------------------------------------------*/
(define-method (tail? node::conditional fun tailp)
   (with-access::conditional node (test true false)
      (when (tail? test fun #f)
	 (and (tail? true fun tailp) (tail? false fun tailp)))))

;*---------------------------------------------------------------------*/
;*    tail? ::let-fun ...                                              */
;*---------------------------------------------------------------------*/
(define-method (tail? node::let-fun fun tailp)
   (with-access::let-fun node (locals body)
      (when (tail? body fun tailp)
	 (if (isloop? node)
	     (with-access::sfun (local-value (car locals)) (body)
		(when (tail? body fun tailp)
		   (every (lambda (l)
			     (with-access::sfun (local-value l) (body)
				(tail? body fun #f)))
		      locals)))))))

;*---------------------------------------------------------------------*/
;*    tail? ::let-var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (tail? node::let-var fun tailp)
   (with-access::let-var node (bindings body)
      (when (every (lambda (b) (tail? (cdr b) fun #f)) bindings)
	 (tail? body fun tailp))))

;*---------------------------------------------------------------------*/
;*    tail? ::switch ...                                               */
;*---------------------------------------------------------------------*/
(define-method (tail? node::switch fun tailp)
   (with-access::switch node (test clauses)
      (when (tail? test fun #f)
	 (every (lambda (c) (tail? (cdr c) fun tailp)) clauses))))

;*---------------------------------------------------------------------*/
;*    tail? ::extern ...                                               */
;*---------------------------------------------------------------------*/
(define-method (tail? node::extern fun tailp)
   (with-access::extern node (expr*)
      (every (lambda (e) (tail? e fun #f)) expr*)))

