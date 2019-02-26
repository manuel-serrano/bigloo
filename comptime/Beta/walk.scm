;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Beta/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 08:46:28 1996                          */
;*    Last change :  Tue Feb 26 13:54:42 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements a very simple beta reduction. It reduces  */
;*    read-only local variables bound to atom (e.g., bool, number)     */
;*    used in application nodes.                                       */ 
;*    -------------------------------------------------------------    */
;*    This stage is designed to be used just after the inlining.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module beta_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch"
	    "Ast/node.sch")
   (import  tools_error
	    tools_speek
	    tools_shape
	    effect_effect
	    engine_param
	    ast_occur
	    ast_remove)
   (export  (beta-walk!::obj ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    beta-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (beta-walk! globals)
   (pass-prelude "Constant Beta")
   (beta-globals! globals)
   (pass-postlude (remove-var 'beta globals)))

;*---------------------------------------------------------------------*/
;*    beta-globals! ...                                                */
;*---------------------------------------------------------------------*/
(define (beta-globals! globals)
   (for-each (lambda (global)
		(let ((sfun (global-value global)))
		   (sfun-body-set! sfun (node-beta! (sfun-body sfun) '()))))
	     globals))
   
;*---------------------------------------------------------------------*/
;*    node-beta! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node-beta!::node node::node stack)
   node)

;*---------------------------------------------------------------------*/
;*    node-beta*! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::sequence stack)
   (node-beta*! (sequence-nodes node) stack)
   node)

;*---------------------------------------------------------------------*/
;*    node-beta*! ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::sync stack)
   (sync-mutex-set! node (node-beta! (sync-mutex node) stack))
   (sync-prelock-set! node (node-beta! (sync-prelock node) stack))
   (sync-body-set! node (node-beta! (sync-body node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::app stack)
   (app-fun-set! node (node-beta! (app-fun node) stack))
   (map! (lambda (n)
	    (if (and (var? n) (local? (var-variable n)))
		(let ((red (assq (var-variable n) stack)))
		   (cond
		      ((not (pair? red))
		       n)
		      ((literal? (cdr red))
		       (duplicate::literal (cdr red)))
		      ((patch? (cdr red))
		       (duplicate::patch (cdr red)))
		      (else
		       (error "node-beta!" "wrong node" (typeof (cdr red))))))
		(node-beta! n stack)))
	 (app-args node))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::app-ly stack)
   (app-ly-fun-set! node (node-beta! (app-ly-fun node) stack))
   (app-ly-arg-set! node (node-beta! (app-ly-arg node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::funcall stack)
   (funcall-fun-set! node (node-beta! (funcall-fun node) stack))
   (node-beta*! (funcall-args node) stack)
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::extern stack)
   (node-beta*! (extern-expr* node) stack)
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::cast stack)
   (cast-arg-set! node (node-beta! (cast-arg node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::setq stack)
   (setq-value-set! node (node-beta! (setq-value node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::conditional stack)
   (conditional-test-set! node (node-beta! (conditional-test node) stack))
   (conditional-true-set! node (node-beta! (conditional-true node) stack))
   (conditional-false-set! node (node-beta! (conditional-false node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::fail stack)
   (fail-proc-set! node (node-beta! (fail-proc node) stack))
   (fail-msg-set! node (node-beta! (fail-msg node) stack))
   (fail-obj-set! node (node-beta! (fail-obj node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::switch stack)
   (switch-test-set! node (node-beta! (switch-test node) stack))
   (for-each (lambda (clause)
		(set-cdr! clause (node-beta! (cdr clause) stack)))
	     (switch-clauses node))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::make-box stack)
   (make-box-value-set! node (node-beta! (make-box-value node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::box-ref stack)
   (box-ref-var-set! node (node-beta! (box-ref-var node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::box-set! stack)
   (box-set!-var-set! node (node-beta! (box-set!-var node) stack))
   (box-set!-value-set! node (node-beta! (box-set!-value node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::let-fun stack)
   (with-access::let-fun node (locals body)
      (set! body (node-beta! body stack))
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (node-beta! (sfun-body fun) stack))))
		locals)
      node))

;*---------------------------------------------------------------------*/
;*    node-beta! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::let-var stack)
   (with-access::let-var node (bindings body removable?)
      (let loop ((bindings bindings)
		 (new-stack stack))
	 (if (null? bindings)
	     (begin
		(set! body (node-beta! body new-stack))
		node)
	     (let* ((binding (car bindings))
		    (var (car binding))
		    (val (node-beta! (cdr binding) stack)))
		(if (and (atom? (cdr binding))
			 (eq? (local-access (car binding)) 'read)
			 (let ((val (atom-value (cdr binding))))
			    (or (number? val)
				(boolean? val)
				(char? val)
				(symbol? val)
				(keyword? val)
				(cnst? val))))
		    (loop (cdr bindings) (cons binding new-stack))
		    (loop (cdr bindings) new-stack)))))))

;*---------------------------------------------------------------------*/
;*    node-beta! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::set-ex-it stack)
   (set-ex-it-body-set! node (node-beta! (set-ex-it-body node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-beta! node::jump-ex-it stack)
   (jump-ex-it-exit-set! node (node-beta! (jump-ex-it-exit node) stack))
   (jump-ex-it-value-set! node (node-beta! (jump-ex-it-value node) stack))
   node)

;*---------------------------------------------------------------------*/
;*    node-beta*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-beta*! node* stack)
   (map! (lambda (node) (node-beta! node stack)) node*)
   node*)
