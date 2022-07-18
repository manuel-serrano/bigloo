;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-merge.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Mon Jul 18 13:48:34 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV merge                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-merge
   
   (include "Tools/trace.sch"
	    "SawMill/bbv-types.sch"
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
	    saw_bbv-utils
	    saw_bbv-range
	    saw_bbv-cost)

   (export  (mark-widener! ::blockV)
	    (block-merge-contexts ::blockV)))

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
		  (with-access::blockV n (merge)
		     (cond
			(merge #unspecified)
			((memq n stack) (set! merge #t))
			(else (loop n (cons block stack))))
		     (liip (cdr succs)))))))))

;*---------------------------------------------------------------------*/
;*    block-merge-contexts ...                                         */
;*---------------------------------------------------------------------*/
(define (block-merge-contexts b::blockV)
   
   (define (path-cost! v)
      (let ((b (cdr v)))
	 (with-access::blockS b (cost)
	    (if (>=fx cost 0)
		cost
		(let ((c (path-cost b)))
		   (set! cost c)
		   c)))))
   
   (with-access::blockV b (label)
      (with-trace 'bbv-merge (format "block-merge-contexts ~a" label)
	 (with-access::blockV b (versions)
	    (let ((bs (ctx-live-versions versions)))
	       ;; compute the cost of each version
	       (for-each path-cost! bs)
	       (let ((s (sort (lambda (vx vy)
				 (with-access::blockS (cdr vx) ((costx cost))
				    (with-access::blockS (cdr vy) ((costy cost))
				       (<=fx costy costx))))
			   bs)))
		  (let ((wctx (merge-ctx (caar s) (caadr s))))
		     (trace-item "wctx=" (shape wctx))
		     (values wctx (list (car s) (cadr s))))))))))

;*---------------------------------------------------------------------*/
;*    find-closest-blocks ...                                          */
;*    -------------------------------------------------------------    */
;*    Find two blocks at the minimum distance.                         */
;*---------------------------------------------------------------------*/
(define (find-closest-blocks ctx::pair-nil bs::pair-nil)
   (let loop ((bs (cdr bs))
	      (d (ctx-distance ctx (caar bs)))
	      (v (car bs)))
      (if (null? bs)
	  v
	  (let ((nd (ctx-distance ctx (caar bs))))
	     (if (<fx nd d)
		 (loop (cdr bs) nd (car bs))
		 (loop (cdr bs) d v))))))
   
;*---------------------------------------------------------------------*/
;*    ctx-distance ...                                                 */
;*    -------------------------------------------------------------    */
;*    The distance between two contexts.                               */
;*---------------------------------------------------------------------*/
(define (ctx-distance x::pair-nil y::pair-nil)
   (let loop ((x x)
	      (acc 0))
      (if (null? x)
	  acc
	  (let ((e (car x)))
	     (let ((f (ctx-get y (bbv-ctxentry-reg e))))
		(if (not f)
		    (error "ctx-distance" "inconsistent contexts" (shape y))
		    (loop (cdr x) (+fx acc (bbv-ctxentry-distance e f)))))))))
      
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
		 
;*---------------------------------------------------------------------*/
;*    merge-ctx ...                                                    */
;*---------------------------------------------------------------------*/
(define (merge-ctx x::pair-nil y::pair-nil)
   [assert (x y) (=fx (length x) (length y))]
   (map (lambda (ex)
	   (let ((ey (ctx-get y (bbv-ctxentry-reg ex))))
	      (cond
		 ((equal? ex ey)
		  ex)
		 ((not (equal? (bbv-ctxentry-types ex) (bbv-ctxentry-types ey)))
		  (duplicate::bbv-ctxentry ex
		     (types (list *obj*))
		     (polarity #t)
		     (value '_)))
		 ((not (eq? (bbv-ctxentry-polarity ex) (bbv-ctxentry-polarity ey)))
		  (duplicate::bbv-ctxentry ex
		     (types (list *obj*))
		     (polarity #t)
		     (value '_)))
		 ((or (not (every range-type? (bbv-ctxentry-types ex)))
		      (not (isa? (bbv-ctxentry-value ex) bbv-range))
		      (not (isa? (bbv-ctxentry-value ey) bbv-range)))
		  (duplicate::bbv-ctxentry ex
		     (types (list *obj*))
		     (polarity #t)
		     (value '_)))
		 (else
		  (let* ((rx (bbv-ctxentry-value ex))
			 (ry (bbv-ctxentry-value ey)))
		     (duplicate::bbv-ctxentry ex
			(value (bbv-range-widen rx ry))))))))
      x))
      
