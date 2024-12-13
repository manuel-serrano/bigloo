;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Expand/callcc.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 13 08:19:49 2024                          */
;*    Last change :  Fri Dec 13 09:02:28 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Super limited call/cc optimization                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_callcc
   (include "Expand/expander.sch"
	    "Tools/trace.sch"
	    "Tools/location.sch")
   (import  tools_args
	    tools_speek
	    tools_misc
	    expand_expander
	    expand_eps
	    expand_lambda
	    engine_param
	    type_type
	    ast_ident
	    backend_backend
	    tools_location)
   (export  (expand-callcc ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-callcc ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-callcc x e)
   (if (not *call/cc?*)
       (map (lambda (x) (e x e)) x)
       (match-case x
	  ((?- (lambda (?k) . ?body))
	   (let ((ebody (e `(begin ,@body) e)))
	      (if (exitable? ebody (list k) k)
		  (e `(bind-exit (,k) ,@body) e)
		  (map (lambda (x) (e x e)) x))))
	  (else
	   (map (lambda (x) (e x e)) x)))))

;*---------------------------------------------------------------------*/
;*    exitable? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is #t iff:                                                       */
;*      - k is only called                                             */
;*      - not closure is involved                                      */
;*---------------------------------------------------------------------*/
(define (exitable? node env k)
   (match-case node
      ((? symbol?)
       (not (memq node env)))
      ((@ . ?-)
       #t)
      ((quote . ?-)
       #t)
      ((begin . ?nodes)
       (every (lambda (n) (exitable? n env k)) nodes))
      ((lambda . ?-)
       #f)
      ((let ?loop ?bindings , ?nodes)
       (exitable? `(let ,@bindings ,@nodes) (cons loop env) k))
      (((or let letrec let* letrec*) ?bindings . ?nodes)
       (let loop ((bindings bindings)
		  (env env))
	  (if (null? bindings)
	      (exitable? `(begin ,@nodes) env k)
	      (match-case (car bindings)
		 ((? symbol?)
		  (loop (cdr bindings) env))
		 ((?var (lambda ?a . ?body))
		  (when (exitable? `(begin ,@body) env k)
		     (loop (cdr bindings) (cons (fast-id-of-id var #f) env))))
		 ((?var . ?nodes)
		  (when (exitable? `(begin ,@nodes) env k)
		     (loop (cdr bindings) env)))))))
      ((?fun ?a)
       (and (exitable? a env k)
	    (or (symbol? fun) (exitable? fun env k))))
      ((?fun . ?args)
       (and (every (lambda (n) (exitable? n env k)) args)
	    (or (symbol? fun) (exitable? fun env k))
	    ;; bails out if the continuation is called with mode than 1 arg
	    (not (eq? fun k))))
      (else
       #t)))
       

	  
   
