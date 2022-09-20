;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-merge.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Fri Sep 16 09:05:13 2022 (serrano)                */
;*    Copyright   :  2022 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV merge                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-merge
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawBbv/bbv-types.sch")
   
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
	    saw_bbv-cost
	    saw_bbv-config
	    saw_bbv-specialize
	    saw_bbv-range)

   (export  (mark-widener! ::blockV)
	    (block-merge-contexts ::blockV ::bbv-ctx)
	    (merge-contexts ::pair-nil)))

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
;*    reg-ranges-get ...                                               */
;*    -------------------------------------------------------------    */
;*    Get all the ranges of variable X in CTX                          */
;*---------------------------------------------------------------------*/
(define (reg-ranges-get reg versions::pair-nil)
   (filter-map (lambda (v)
		  (let ((e (bbv-ctx-get (car v) reg)))
		     (with-access::bbv-ctxentry e (polarity value)
			(when (and polarity (isa? value bbv-range))
			   value))))
      versions))

;*---------------------------------------------------------------------*/
;*    reg-range-widening ...                                           */
;*    -------------------------------------------------------------    */
;*    Range widening. The produced range is larger than [o..u].        */
;*---------------------------------------------------------------------*/
(define (reg-range-widening o u ranges)

   (define (low-widening v)
      (cond
	 ((>fx v 1) (/fx v 2))
	 ((=fx v 1) 0)
	 ((=fx v 0) -1)
	 ((>fx v -16) (*fx v 2))
	 ((>fx v -255) -255)
	 (else (bbv-min-fixnum))))

   (define (up-widening v)
      (cond
	 ((<fx v -1) (/fx v 2))
	 ((=fx v -1) 0)
	 ((=fx v 0) 1)
	 ((<fx v 16) (*fx v 2))
	 ((<fx v 255) 255)
	 ((<fx v 65535) 65535)
	 (else (bbv-max-fixnum))))
   
   (let ((no (if (every (lambda (r)
			   (with-access::bbv-range r (lo)
			      (= o lo)))
		    ranges)
		 o
		 (low-widening o)))
	 (nu (if (every (lambda (r)
			   (with-access::bbv-range r (up)
			      (= u up)))
		    ranges)
		 u
		 (up-widening u))))
      (unless (and (= no o) (= nu u))
	 ;; no widening possible because we are dealing with a constant
	 (instantiate::bbv-range
	    (lo no)
	    (up nu)))))
   
;*---------------------------------------------------------------------*/
;*    reg-range-merge ...                                              */
;*    -------------------------------------------------------------    */
;*    Find the smallest range for variable X that is compatible        */
;*    with all the variable context ranges.                            */
;*---------------------------------------------------------------------*/
(define (reg-range-merge reg versions::pair-nil)
   (let ((ranges (reg-ranges-get reg versions)))
      (cond
	 ((not (pair? ranges))
	  ;; the variable is not a fixnum
	  #f)
	 ((null? (cdr ranges))
	  ;; just one range, which means that the variable is unmodified
	  #f)
	 (else
	  (let loop ((o (bbv-max-fixnum))
		     (u (bbv-min-fixnum))
		     (r ranges))
	     (if (null? r)
		 (reg-range-widening o u ranges)
		 (with-access::bbv-range (car r) (lo up)
		    (loop (min o lo) (max u up) (cdr r)))))))))

;*---------------------------------------------------------------------*/
;*    block-merge-contexts ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns a merge context and a list of blocks to be replaced.     */
;*    -------------------------------------------------------------    */
;*    b is a merge block (i.e., in a loop) and the number of           */
;*    specialized version has exceeded the maximum threshold, some     */
;*    versions have to be collapsed and widened.                       */
;*---------------------------------------------------------------------*/
(define (block-merge-contexts b::blockV ctx::bbv-ctx)
   (with-access::blockV b (versions label)
      (with-trace 'bbv-merge (format "block-merge-contexts! block=~a" label)
	 (let ((versions (live-versions versions)))
	    (for-each (lambda (v num)
			 (trace-item (format "ctx.~a=" num) (shape (car v))))
	       versions (iota (length versions)))
	    (let loop ((regs (map bbv-ctxentry-reg
				(bbv-ctx-entries (caar versions))))
		       (mctx (instantiate::bbv-ctx))
		       (rversions '()))
	       (if (null? regs)
		   (if (null? rversions)
		       ;; no replacement
		       (begin
			  (trace-item "mctx=" #f)
			  (values #f #f))
		       (begin
			  (trace-item "mctx=" (shape mctx))
			  (values mctx
			     (delete-duplicates (map cdr rversions)))))
		   (let* ((reg (car regs))
			  (rng (reg-range-merge reg versions)))
		      (trace-item "reg=" (shape reg) " rng=" (shape rng))
		      (if rng
			  (loop (cdr regs)
			     (extend-ctx ctx reg (list *bint*) #t :value rng)
			     (append rversions
				(filter (lambda (v)
					   (let ((e (bbv-ctx-get (car v) reg)))
					      (with-access::bbv-ctxentry e (polarity value)
						 (and polarity (isa? value bbv-range)))))
				   versions)))
			  (let ((ce (bbv-ctx-get ctx reg)))
			     (loop (cdr regs)
				(extend-ctx/entry mctx ce)
				rversions))))))))))

;* (define (block-merge-contexts b::blockV)                            */
;*                                                                     */
;*    (define (list-replace::pair-nil lst::pair-nil old new)           */
;*       ;; return a copy of lst where old has been replaced with new  */
;*       (let loop ((lst lst))                                         */
;* 	 (cond                                                         */
;* 	    ((null? lst) lst)                                          */
;* 	    ((eq? (car lst) old) (cons new (loop (cdr lst))))          */
;* 	    (else (cons (car lst) (loop (cdr lst)))))))                */
;*                                                                     */
;*    (define (bbv-ctx-replace ctx old new)                            */
;*       (with-access::bbv-ctx ctx (entries)                           */
;* 	 (duplicate::bbv-ctx ctx                                       */
;* 	    (entries (list-replace entries old new)))))                */
;*                                                                     */
;*    (with-access::blockV b (versions label)                          */
;*       (with-trace 'bbv-merge (format "block-merge-contexts! ~a" label) */
;* 	 ;; step1: reduce singleton approximation into intervals       */
;* 	 (merge-singletons! versions))))                               */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    block-merge-contexts ...                                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define (block-merge-contexts b::blockV)                            */
;*                                                                     */
;*    (define (path-cost! v)                                           */
;*       (let ((b (cdr v)))                                            */
;* 	 (with-access::blockS b (cost)                                 */
;* 	    (if (>=fx cost 0)                                          */
;* 		cost                                                   */
;* 		(let ((c (path-cost b)))                               */
;* 		   (set! cost c)                                       */
;* 		   c)))))                                              */
;*                                                                     */
;*    (with-access::blockV b (label)                                   */
;*       (with-trace 'bbv-merge (format "block-merge-contexts ~a" label) */
;* 	 (with-access::blockV b (versions)                             */
;* 	    (let ((bs (ctx-live-versions versions)))                   */
;* 	       ;; compute the cost of each version                     */
;* 	       (for-each path-cost! bs)                                */
;* 	       (let ((s (sort (lambda (vx vy)                          */
;* 				 (with-access::blockS (cdr vx) ((costx cost)) */
;* 				    (with-access::blockS (cdr vy) ((costy cost)) */
;* 				       (<=fx costy costx))))           */
;* 			   bs)))                                       */
;* 		  (let ((wctx (merge-ctx (caar s) (caadr s))))         */
;* 		     (trace-item "wctx=" (shape wctx))                 */
;* 		     (values wctx (list (car s) (cadr s))))))))))      */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    find-closest-blocks ...                                          *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Find two blocks at the minimum distance.                         *} */
;* {*---------------------------------------------------------------------*} */
;* (define (find-closest-blocks ctx::pair-nil bs::pair-nil)            */
;*    (let loop ((bs (cdr bs))                                         */
;* 	      (d (ctx-distance ctx (caar bs)))                         */
;* 	      (v (car bs)))                                            */
;*       (if (null? bs)                                                */
;* 	  v                                                            */
;* 	  (let ((nd (ctx-distance ctx (caar bs))))                     */
;* 	     (if (<fx nd d)                                            */
;* 		 (loop (cdr bs) nd (car bs))                           */
;* 		 (loop (cdr bs) d v))))))                              */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    ctx-distance ...                                                 *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    The distance between two contexts.                               *} */
;* {*---------------------------------------------------------------------*} */
;* (define (ctx-distance x::pair-nil y::pair-nil)                      */
;*    (let loop ((x x)                                                 */
;* 	      (acc 0))                                                 */
;*       (if (null? x)                                                 */
;* 	  acc                                                          */
;* 	  (let ((e (car x)))                                           */
;* 	     (let ((f (ctx-get y (bbv-ctxentry-reg e))))               */
;* 		(if (not f)                                            */
;* 		    (error "ctx-distance" "inconsistent contexts" (shape y)) */
;* 		    (loop (cdr x) (+fx acc (bbv-ctxentry-distance e f))))))))) */
;*                                                                     */
;* {*---------------------------------------------------------------------*} */
;* {*    bbv-ctxentry-distance ...                                        *} */
;* {*    -------------------------------------------------------------    *} */
;* {*    Compute the distance between two entries associated with the     *} */
;* {*    same register.                                                   *} */
;* {*---------------------------------------------------------------------*} */
;* (define (bbv-ctxentry-distance x::bbv-ctxentry y::bbv-ctxentry)     */
;*    (with-access::bbv-ctxentry x ((tx types) (px polarity) (vx value)) */
;*       (with-access::bbv-ctxentry y ((ty types) (py polarity) (vy value)) */
;* 	 (cond                                                         */
;* 	    ((equal? tx ty)                                            */
;* 	     (cond                                                     */
;* 		((not (eq? px py))                                     */
;* 		 ;; opposite polarities                                */
;* 		 20)                                                   */
;* 		((equal? vx vy)                                        */
;* 		 ;; same everything                                    */
;* 		 0)                                                    */
;* 		(else                                                  */
;* 		 ;; values differ                                      */
;* 		 5)))                                                  */
;* 	    (else                                                      */
;* 	     10)))))                                                   */

;*---------------------------------------------------------------------*/
;*    merge-contexts ...                                               */
;*---------------------------------------------------------------------*/
(define (merge-contexts lst::pair-nil)
   (let loop ((m (car lst))
	      (lst (cdr lst)))
      (if (pair? lst)
	  (loop (merge-ctx m (car lst)) (cdr lst))
	  m)))

;*---------------------------------------------------------------------*/
;*    merge-ctx ...                                                    */
;*---------------------------------------------------------------------*/
(define (merge-ctx x::bbv-ctx y::bbv-ctx)
   
   (define (merge-entries x y)
      (map (lambda (ex)
	      (let ((ey (bbv-ctx-get y (bbv-ctxentry-reg ex))))
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
	 (bbv-ctx-entries x)))
   
   [assert (x y) (=fx (length (bbv-ctx-entries x)) (length (bbv-ctx-entries y)))]
   (instantiate::bbv-ctx
      (entries (merge-entries x y))))
      
