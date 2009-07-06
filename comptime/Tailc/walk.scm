;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tailc/walk.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Feb 21 08:37:48 1995                          */
;*    Last change :  Thu Sep  2 15:51:56 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `tail-call' pass.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tailc_walk
   (include "Engine/pass.sch")
   (import  tools_error
	    tools_shape
	    engine_pass
	    type_type
	    ast_var
	    ast_remove
	    ast_node
	    ast_local)
   (static  (wide-class local-tail::local))
   (export  (tailc-walk! globals)))

;*---------------------------------------------------------------------*/
;*    tailc-walk! ...                                                  */
;*---------------------------------------------------------------------*/
(define (tailc-walk! globals)
   (pass-prelude "Tailc")
   (for-each tailc-global globals)
   globals)

;*---------------------------------------------------------------------*/
;*    tailc-global ...                                                 */
;*---------------------------------------------------------------------*/
(define (tailc-global global)
   (enter-function (global-id global))
   (let ((calls (get-tail-calls (sfun-body (global-value global)) global '())))
      (when (pair? calls)
	 (verbose 3 "        " (shape global) ": " (length calls)
		  " self tail call(s)" #\Newline)
	 (global->local! global calls)))
   (leave-function))

;*---------------------------------------------------------------------*/
;*    global->local! ...                                               */
;*---------------------------------------------------------------------*/
(define (global->local! global calls)
   (let* ((gfun (global-value global))
	  (gtype (global-type global))
	  (lbody (sfun-body gfun))
	  (largs (sfun-args gfun))
	  (gargs (map (lambda (old)
			 (clone-local old
				      (duplicate::svar
					    (local-value old))))
		      largs))
	  (lfun (duplicate::sfun gfun
		   (arity (sfun-arity gfun))
		   (args largs)
		   (body lbody)))
	  (local (make-local-sfun (global-id global) gtype lfun))
	  (loc (node-loc lbody))
	  (gbody (instantiate::let-fun
		    (loc loc)
		    (type gtype)
		    (locals (list local))
		    (body (instantiate::app
			     (loc loc)
			     (type gtype)
			     (fun (instantiate::var
				     (loc loc)
				     (type gtype)
				     (variable local)))
			     (args (map (lambda (v)
					   (instantiate::var
					      (loc loc)
					      (type (local-type v))
					      (variable v)))
					gargs)))))))
      ;; patch the body of the global function
      (sfun-args-set! gfun gargs)
      (sfun-body-set! gfun gbody)
      ;; patch the body of the local function
      (for-each (lambda (call)
		   (let* ((loc (node-loc call))
			  (var (instantiate::var
				  (loc loc)
				  (type (local-type local))
				  (variable local))))
		      (with-access::app call (fun)
			 (set! fun var))))
		calls)
      fun))
      
;*---------------------------------------------------------------------*/
;*    get-tail-calls ...                                               */
;*    -------------------------------------------------------------    */
;*    This function traverse the body of a global function searching   */
;*    the global self tail calls.                                      */
;*---------------------------------------------------------------------*/
(define-generic (get-tail-calls::pair-nil n::node h::variable tails::pair-nil)
   tails)

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::closure ...                                     */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::closure host tails)
   (internal-error "get-tail-calls" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::sequence ...                                    */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::sequence host tails)
   (with-access::sequence node (nodes) 
      (if (null? nodes)
	  tails
	  (get-tail-calls (car (last-pair nodes)) host tails))))

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::app host tails)
   (with-access::app node (fun)
      (let ((callee (var-variable fun)))
	 (cond
	    ((eq? callee host) 
	     (cons node tails))
	    ((local-tail? callee)
	     tails)
	    ((local? callee)
	     (widen!::local-tail callee)
	     (get-tail-calls (sfun-body (local-value callee)) host tails))
	    (else
	     tails)))))

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::conditional ...                                 */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::conditional host tails)
   (with-access::conditional node (test true false)
      (get-tail-calls true host (get-tail-calls false host tails))))

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::select ...                                      */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::select host tails)
   (with-access::select node (test item-type)
      (let liip ((clauses (select-clauses node))
		 (tails tails))
	 (if (null? clauses)
	     tails
	     (liip (cdr clauses)
		   (get-tail-calls (cdr (car clauses)) host tails))))))

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::let-fun ...                                     */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::let-fun host tails)
   (with-access::let-fun node (body)
      ;; local functions body are not traversed in this function.
      ;; they are traversed when the local are called
      ;; (see the ::app get-tail-calls method) 
      (get-tail-calls body host tails)))

;*---------------------------------------------------------------------*/
;*    get-tail-calls ::let-var ...                                     */
;*---------------------------------------------------------------------*/
(define-method (get-tail-calls node::let-var host tails)
   (with-access::let-var node (body)
      (get-tail-calls body host tails)))
