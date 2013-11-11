;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  6 18:02:26 2013                          */
;*    Last change :  Sun Nov 10 18:48:32 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Narrow the variable scopes. This optimization looks like a       */
;*    register allocation. It rewrites code as follows:                */
;*                                                                     */
;*       (let ((x #unspecified))                                       */
;*           expr1                                                     */
;*           ...                                                       */
;*           exprN                                                     */
;*           (set! x 3) ;; x not used before that assignment           */
;*           ...                                                       */
;*           exprM)                                                    */
;*    =>                                                               */
;*       (begin                                                        */
;*           expr1                                                     */
;*           ...                                                       */
;*           exprN                                                     */
;*           (let ((x 3))                                              */
;*             ...                                                     */
;*             exprM))                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narrow_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    ast_walk
	    ast_alphatize
	    ast_local
	    ast_occur
	    module_module
	    engine_param
	    narrow_defuse)
   (export  (narrow-walk! ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    narrow-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (narrow-walk! globals)
   (pass-prelude "Narrow")
   (for-each narrow-function! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    narrow-function! ...                                             */
;*---------------------------------------------------------------------*/
(define (narrow-function! v::variable)
   (with-access::variable v (value)
      (with-access::sfun value (body)
	 (set! body (narrow! body)))))

;*---------------------------------------------------------------------*/
;*    narrow! ::node ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow! n::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    narrow! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow! n::let-var)
   (with-trace 1 "narrow"
      (trace-item "n=" (shape n))
      (with-access::let-var n (bindings body)
	 (let ((locals (map car bindings)))
	    (set! body (narrow-scope! body locals)))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::node ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow-scope! n::node locals::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow-scope! n::sequence locals::pair-nil)
   
   (define (defassign? s::node locals)
      (when (isa? s setq)
	 (with-access::setq s (var)
	    (with-access::var var (variable)
	       (memq variable locals)))))

   (define (copy-local variable)
      (with-access::variable variable (value id type)
	 (if (isa? value sfun)
	     (make-local-sfun id type value)
	     (make-local-svar id type))))
   
   (define (setq->let nodes type locals)
      (with-access::setq (car nodes) (var value (stype type) loc)
	 (with-access::var var (variable)
	    (let* ((newv (copy-local variable))
		   (bind (cons newv value)))
	       (if (null? (cdr nodes))
		   (instantiate::let-var
		      (loc loc)
		      (type type)
		      (bindings (list bind))
		      (body (instantiate::atom
			       (value #unspecified)
			       (type stype))))
		   (let ((seq (instantiate::sequence
				 (type type)
				 (nodes (cdr nodes)))))
		      (instantiate::let-var
			 (loc loc)
			 (type type)
			 (bindings (list bind))
			 (body (narrow-scope!
				  (alphatize (list variable)
				     (list newv)
				     loc seq)
				  (remq variable locals))))))))))
   
   (define (dead-locals locals uses)
      (filter (lambda (l)
		 (every (lambda (s)
			   (not (memq l s)))
		    uses))
	 locals))

   (with-trace 1 "narrow-scope!"
      (trace-item "n=" (shape n))
      (trace-item "locals=" (shape locals))
      (if (null? locals)
	  n
	  (with-access::sequence n (nodes type)
	     (let ((uses '()))
		(for-each (lambda (node)
			     (multiple-value-bind (def use)
				(defuse-locals node locals)
				(set! uses (cons use uses))))
		   nodes)
		(let loop ((nodes nodes)
			   (locals locals)
			   (uses (reverse! uses)))
		   (when (pair? nodes)
		      (trace-item "node=" (shape (car nodes)))
		      (trace-item "locals=" (shape locals)))
		   (cond
		      ((null? nodes)
		       n)
		      ((null? locals)
		       n)
		      ((defassign? (car nodes) locals)
		       (trace-item "new let=" (shape (setq->let nodes type locals)))
		       (tprint "SETQ->LET: " (shape (car nodes)))
		       (set-car! nodes (setq->let nodes type locals))
		       (set-cdr! nodes '())
		       n)
		      (else
		       (let ((deads (dead-locals locals uses)))
			  (when (pair? deads)
			     (trace-item "deads=" (shape deads))
			     (trace-item "trav=" (shape (car nodes)))
			     (set-car! nodes (narrow-scope! (car nodes) deads)))
			  (loop (cdr nodes)
			     locals
			     (cdr uses)))))))))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::let-fun locals::pair-nil)
   ;; eliminate from the locals set all the variables appearing free
   ;; ine one function and also used in the body
   (with-access::let-fun n ((funs locals) body)
      (multiple-value-bind (defbody usebody)
	 (defuse-locals body locals)
	 (let ((locals-sans (filter (lambda (l)
				       (or (memq l defbody)
					   (memq l usebody)))
			       locals)))
	    (for-each (lambda (fun)
			 (with-access::local fun (value)
			    (with-access::sfun value (body)
			       (set! body (narrow-scope! body locals-sans)))))
	       funs)
	    (set! body (narrow-scope! body locals-sans))
	    n))))
		      





