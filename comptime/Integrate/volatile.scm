;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/Integrate/volatile.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 12 08:55:10 2021                          */
;*    Last change :  Fri Jul  9 14:43:08 2021 (serrano)                */
;*    Copyright   :  2021 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Flag as "volatile" local variables that survive a set-exit       */
;*    node. This is used only when  *local-exit?* is true.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_volatile
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  type_type
	    engine_param
	    ast_var
	    ast_node
	    ast_walk
	    liveness_liveness
	    tools_shape
	    tools_speek)
   (export  (volatile!::global ::global)))

;*---------------------------------------------------------------------*/
;*    volatile! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Current all local variables of a function that uses a set-exit   */
;*    are marked as volatile. An def/use analysis that works on        */
;*    the node tree needs to be implemented to improve allocate        */
;*    temporaries in registers.                                        */
;*---------------------------------------------------------------------*/
(define (volatile! global)
   (with-access::global global (value id)
      (multiple-value-bind (def use)
	 (liveness-sfun! value)
	 (with-access::sfun value (body)
	    (when (use-set-exit? body)
	       (volatile body '())))))
   global)

;*---------------------------------------------------------------------*/
;*    volalite-sfun ...                                                */
;*---------------------------------------------------------------------*/
(define (volatile-sfun node env)
   (with-access::sfun node (args body)
      (volatile body (append args env))))
   
;*---------------------------------------------------------------------*/
;*    volatile ::node ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (volatile node::node env)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    volatile ...                                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (volatile node::closure env)
   (with-access::closure node (variable)
      (with-access::variable variable (value)
	 (volatile-sfun value env))))
      
;*---------------------------------------------------------------------*/
;*    volatile ::let-var ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (volatile node::let-var env)
   (with-access::let-var node (bindings body)
      (for-each (lambda (b)
		   (with-access::local (car b) (volatile)
		      (set! volatile #f)))
	 bindings)
      (for-each (lambda (b)
		   (volatile (cdr b) env))
	 bindings)
      (volatile body (append (map car bindings) env))))

;*---------------------------------------------------------------------*/
;*    volatile ::let-fun ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (volatile node::let-fun env)
   (with-access::let-fun node (locals body)
      (for-each (lambda (l)
		   (with-access::local l (value)
		      (volatile-sfun value env)))
	 locals)
      (volatile body
	 (append (append-map (lambda (l)
				(with-access::local l (value)
				   (sfun-args value)))
		    locals)
	    locals
	    env))))

;*---------------------------------------------------------------------*/
;*    volatile ::set-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (volatile node::set-ex-it env)
   (let ((live (liveness-live node)))
      (for-each (lambda (l)
		   (when (memq l env)
		      (with-access::local l (volatile)
			 (set! volatile #t))))
	 live)
      (with-access::set-ex-it node (body onexit)
	 (volatile body env))))
      
;*---------------------------------------------------------------------*/
;*    use-set-exit? ...                                                */
;*---------------------------------------------------------------------*/
(define (use-set-exit? node)
   (let ((cell (make-cell #f)))
      (use-set-exit node cell)
      (cell-ref cell)))

;*---------------------------------------------------------------------*/
;*    use-set-exit ...                                                 */
;*---------------------------------------------------------------------*/
(define-walk-method (use-set-exit node::node cell)
   (unless (cell-ref cell)
      (call-default-walker)))

;*---------------------------------------------------------------------*/
;*    volatile ::set-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (use-set-exit node::set-ex-it cell)
   (cell-set! cell #t))
      
