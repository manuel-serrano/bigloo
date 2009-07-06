;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Ast/shrinkify.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  5 11:09:52 1996                          */
;*    Last change :  Thu Jul 13 11:16:33 2000 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We shrink all the ast to get rid off all the pass info for the   */
;*    following passes.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_shrinkify
   (import type_type
	   ast_var
	   ast_node
	   ast_env)
   (export (shrinkify! globals)))

;*---------------------------------------------------------------------*/
;*    shrinkify! ...                                                   */
;*---------------------------------------------------------------------*/
(define (shrinkify! globals)
   (for-each-global! shrink-variable!)
   (for-each (lambda (global)
		(let ((sfun (global-value global)))
		   (for-each shrink-variable! (sfun-args sfun))
		   (shrink-node! (sfun-body (global-value global)))))
	     globals)
   globals)

;*---------------------------------------------------------------------*/
;*    shrink-variable! ...                                             */
;*---------------------------------------------------------------------*/
(define-generic (shrink-variable! variable::variable)
   (if (wide-object? variable)
       (shrink! variable))
   (if (wide-object? (variable-value variable))
       (shrink! (variable-value variable))))

;*---------------------------------------------------------------------*/
;*    shrink-node! ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (shrink-node!::unspecified node::node))

;*---------------------------------------------------------------------*/
;*    shrink-node! ::atom ...                                          */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::atom)
   #unspecified)
 
;*---------------------------------------------------------------------*/
;*    shrink-node! ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::var)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::kwote)
   #unspecified)
       
;*---------------------------------------------------------------------*/
;*    shrink-node*! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::sequence)
   (shrink-node*! (sequence-nodes node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::app)
   (if (wide-object? node) (shrink! node))
   (shrink-node! (app-fun node))
   (shrink-node*! (app-args node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::app-ly)
   (if (wide-object? node) (shrink! node))
   (shrink-node! (app-ly-fun node))
   (shrink-node! (app-ly-arg node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::funcall ...                                       */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::funcall)
   (if (wide-object? node) (shrink! node))
   (shrink-node! (funcall-fun node))
   (shrink-node*! (funcall-args node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::extern ...                                        */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::extern)
   (if (wide-object? node) (shrink! node))
   (shrink-node*! (extern-expr* node))
   node)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::cast)
   (shrink-node! (cast-arg node))
   node)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::setq)
   (shrink-node! (setq-value node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::conditional)
   (shrink-node! (conditional-test node))
   (shrink-node! (conditional-true node))
   (shrink-node! (conditional-false node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::fail)
   (shrink-node! (fail-proc node))
   (shrink-node! (fail-msg node))
   (shrink-node! (fail-obj node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::select ...                                        */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::select)
   (shrink-node! (select-test node))
   (for-each (lambda (clause)
		(shrink-node! (cdr clause)))
	     (select-clauses node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::make-box)
   (if (wide-object? node) (shrink! node))
   (shrink-node! (make-box-value node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::box-ref)
   (if (wide-object? node) (shrink! node))
   (shrink-node! (box-ref-var node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::box-set!)
   (if (wide-object? node) (shrink! node))
   (shrink-node! (box-set!-var node))
   (shrink-node! (box-set!-value node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node! ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::let-fun)
   (with-access::let-fun node (locals body)
      (shrink-node! body)
      (for-each (lambda (local)
		   (let ((sfun (local-value local)))
		      (shrink-variable! local)
		      (for-each shrink-variable! (sfun-args sfun))
		      (shrink-node! (sfun-body sfun))))
		locals)
      #unspecified))

;*---------------------------------------------------------------------*/
;*    shrink-node! ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::let-var)
   (with-access::let-var node (bindings body removable?)
      (shrink-node! body)
      (for-each (lambda (binding)
		   (shrink-variable! (car binding))
		   (shrink-node! (cdr binding)))
		bindings)
      #unspecified))

;*---------------------------------------------------------------------*/
;*    shrink-node! ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (shrink-node! var)
      (shrink-node! body)
      #unspecified)) 

;*---------------------------------------------------------------------*/
;*    shrink-node! ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (shrink-node! node::jump-ex-it)
   (shrink-node! (jump-ex-it-exit node))
   (shrink-node! (jump-ex-it-value node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    shrink-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (shrink-node*! node*)
   (for-each shrink-node! node*))
