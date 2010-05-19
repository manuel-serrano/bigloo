;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/check-sharing.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 28 17:38:10 2000                          */
;*    Last change :  Wed Aug  7 15:28:22 2002 (serrano)                */
;*    Copyright   :  2000-02 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This module implements a simple self debug module. It reports on */
;*    nodes that appears several times (because of illegal sharing)    */
;*    in the AST.                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module ast_check-sharing
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   (import  ast_env
	    ast_var
	    ast_node
	    engine_param
	    tools_error
	    tools_shape
	    type_type
	    object_class
	    module_module
	    module_include
	    module_class)
   (export  (check-sharing ::bstring ::pair-nil)
	    (generic check-node-sharing ::node context)
	    (check-node-sharing-reset!)))

;*---------------------------------------------------------------------*/
;*    check-sharing ...                                                */
;*---------------------------------------------------------------------*/
(define (check-sharing when::bstring globals)
   (if *compiler-sharing-debug?*
       (begin
	  (pass-prelude (string-append "Check sharing (" when ")"))
	  (for-each (lambda (global)
		       (check-sharing-fun global #unspecified))
		    globals)
	  (check-node-sharing-reset!)
	  (pass-postlude globals))))

;*---------------------------------------------------------------------*/
;*    check-sharing-fun ...                                            */
;*---------------------------------------------------------------------*/
(define (check-sharing-fun var context)
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (check-node-sharing body context)))

;*---------------------------------------------------------------------*/
;*    *previous* ...                                                   */
;*---------------------------------------------------------------------*/
(define *previous* '())

;*---------------------------------------------------------------------*/
;*    check-node-sharing-reset! ...                                    */
;*---------------------------------------------------------------------*/
(define (check-node-sharing-reset!)
   (set! *previous* '()))

;*---------------------------------------------------------------------*/
;*    check-node-sharing :: ...                                        */
;*---------------------------------------------------------------------*/
(define-generic (check-node-sharing node::node context)
   (if (memq node *previous*)
       (begin
	  (user-warning/location (node-loc node)
				 "check-node-sharing"
				 "shared node"
				 (find-runtime-type node))
	  (fprint (current-error-port) "node    : " (shape node))
	  (fprint (current-error-port) "context : " (shape context))
	  (fprint (current-error-port) "================================"))
       (set! *previous* (cons node *previous*)))
   'done)

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::sequence ...                                */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::sequence context)
   (call-next-method)
   (check-node-sharing* (sequence-nodes node) node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::app ...                                     */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::app context)
   (call-next-method)
   (with-access::app node (fun args)
      (check-node-sharing* args node)))
 
;*---------------------------------------------------------------------*/
;*    check-node-sharing ::app-ly ...                                  */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::app-ly context)
   (call-next-method)
   (with-access::app-ly node (fun arg)
      (check-node-sharing fun node)
      (check-node-sharing arg node)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::funcall ...                                 */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::funcall context)
   (call-next-method)
   (with-access::funcall node (fun args)
      (check-node-sharing fun node)
      (check-node-sharing* args node)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::extern ...                                  */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::extern context)
   (call-next-method)
   (check-node-sharing* (extern-expr* node) node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::cast ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::cast context)
   (call-next-method)
   (check-node-sharing (cast-arg node) node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::setq ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::setq context)
   (call-next-method)
   (check-node-sharing (setq-value node) node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::conditional ...                             */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::conditional context)
   (call-next-method)
   (with-access::conditional node (test true false)
       (check-node-sharing test node)
       (check-node-sharing true node)
       (check-node-sharing false node)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::fail ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::fail context)
   (call-next-method)
   (with-access::fail node (proc msg obj)
      (check-node-sharing proc node)
      (check-node-sharing msg node)
      (check-node-sharing obj node)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::select ...                                  */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::select context)
   (call-next-method)
   (with-access::select node (clauses test)
      (check-node-sharing test node)
      (for-each (lambda (clause)
		   (check-node-sharing (cdr clause) node))
		clauses)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::let-fun ...                                 */
;*    -------------------------------------------------------------    */
;*    We do not go inside local function. Thus, we simply check for    */
;*    the body of the let-fun.                                         */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::let-fun context)
   (call-next-method)
   (with-access::let-fun node (body locals)
      (for-each (lambda (f) (check-sharing-fun f node)) locals)
      (check-node-sharing body node)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::let-var ...                                 */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::let-var context)
   (call-next-method)
   (with-access::let-var node (body bindings)
      (check-node-sharing body node)
      (for-each (lambda (binding)
		   (check-node-sharing (cdr binding) node))
		bindings)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::set-ex-it ...                               */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::set-ex-it context)
   (call-next-method)
   (check-node-sharing (set-ex-it-body node) node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::jump-ex-it ...                              */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::jump-ex-it context)
   (call-next-method)
   (with-access::jump-ex-it node (exit value)
      (check-node-sharing exit node)) 
      (check-node-sharing value node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::make-box ...                                */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::make-box context)
   (call-next-method)
   (check-node-sharing (make-box-value node) node))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ::box-set! ...                                */
;*---------------------------------------------------------------------*/
(define-method (check-node-sharing node::box-set! context)
   (call-next-method)
   (with-access::box-set! node (value)
      (check-node-sharing value node)))

;*---------------------------------------------------------------------*/
;*    check-node-sharing ...                                           */
;*---------------------------------------------------------------------*/
(define (check-node-sharing* node* context)
   (for-each (lambda (node) (check-node-sharing node context)) node*))
