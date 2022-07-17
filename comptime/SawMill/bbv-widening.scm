;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-widening.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Fri Jul 15 13:00:42 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV widening                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-widening
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch")
   
   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    tools_shape
	    tools_speek
	    backend_backend
	    saw_lib
	    saw_defs
	    saw_regset
	    saw_bbv-types
	    saw_bbv-utils)

   (export  (mark-widener! ::blockV)))

;*---------------------------------------------------------------------*/
;*    mark-widener! ...                                                */
;*    -------------------------------------------------------------    */
;*    Mark the BB where widening is allowed. These blocks are the      */
;*    loop heads.                                                      */
;*---------------------------------------------------------------------*/
(define (mark-widener! block)
   (let loop ((block block)
	      (stack '()))
      (with-access::block block (succs)
	 (let liip ((succs succs))
	    (when (pair? succs)
	       (let ((n (car succs)))
		  (with-access::blockV n (widener)
		     (cond
			(widener #unspecified)
			((memq n stack) (set! widener #t))
			(else (loop n (cons block stack))))
		     (liip (cdr succs)))))))))

;*---------------------------------------------------------------------*/
;*    widen-block! ...                                                 */
;*---------------------------------------------------------------------*/
(define (widen-block!::blockS b::blockV ctx)
   (with-access::blockV b (versions)
      (let ((bs (filter (lambda (v)
			   (with-access::blockV (cdr b) (wblock)
			      (not wblock)))
		   versions)))
	 (multiple-value-bind (x y)
	    (find-closest-blocks bs)
	    ...))))

;*---------------------------------------------------------------------*/
;*    find-closest-blocks ...                                          */
;*    -------------------------------------------------------------    */
;*    Find two blocks at the minimum distance.                         */
;*---------------------------------------------------------------------*/
(define (find-closest-blocks bs::pair-nil)
   
   (define (lst-min-distance x lst::pair)
      (let loop ((lst (cdr lst))
		 (d (ctx-distance x (car lst)))
		 (y (car lst)))
	 (if (null? lst)
	     (values y d)
	     (let ((nd (ctx-distance x (car lst))))
		(if (<fx nd d)
		    (loop (cdr lst) nd (car lst))
		    (loop (cdr lst) d y))))))
   
   (multiple-value-bind (d y)
      (lst-min-distance (car bs) (cdr bs))
      (let loop ((bs (cdr bs))
		 (x (car bs))
		 (y y)
		 (d d))
	 (if (null? (cdr bs))
	     (values x y)
	     (multiple-value-bind (ny nd)
		(lst-min-distance (car bs) (cdr bs))
		(if (<fx nd d)
		    (loop (cdr bs) (car bs) ny nd)
		    (loop (cdr bs) x y d)))))))
   
;*---------------------------------------------------------------------*/
;*    ctx-distance ...                                                 */
;*    -------------------------------------------------------------    */
;*    The distance between two block contexts.                         */
;*---------------------------------------------------------------------*/
(define (ctx-distance x::pair-nil y::pair-nil)
   (let loop ((x x)
	      (acc 0))
      (if (null? x)
	  acc
	  (let ((e (car x)))
	     (let ((f (ctx-get y (bbx-ctxentry-reg e))))
		(if (not f)
		    (error "ctx-distance" "inconsistent contexts" (shape y))
		    (loop (cdr x) (+fx acc (bbc-ctxentry-distance e f)))))))))
      
;*---------------------------------------------------------------------*/
;*    bbv-ctxentry-distance ...                                        */
;*    -------------------------------------------------------------    */
;*    Compute the distance between two entries associated with the     */
;*    same register.                                                   */
;*---------------------------------------------------------------------*/
(define (bbv-ctxentry-distance x::bbv-ctxentry y::bbv-ctxentry)
   (with-access::bbv-ctxentry x ((tx types) (px polarity) (vx value))
      (with-access::bbv-ctxentry y ((ty types) (py polarity) (vy value))
	 (cond
	    ((equal? tx ty)
	     (cond
		((not (eq? px py))
		 ;; opposite polarities
		 20)
		((equal? vx vy)
		 ;; same everything
		 0)
		(else
		 ;; values differ
		 5)))
	    (else
	     10)))))
		 
		
   
