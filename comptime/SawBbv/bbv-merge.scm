;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-merge.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Tue Oct 25 16:34:56 2022 (serrano)                */
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
	    (bbv-block-merge-ctx ::blockV)
	    (merge-contexts ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    mark-widener! ...                                                */
;*    -------------------------------------------------------------    */
;*    Mark the BB where widening is allowed. These blocks are the      */
;*    loop heads.                                                      */
;*---------------------------------------------------------------------*/
(define (mark-widener! block)
   (with-trace 'bbv "mark-widener!"
      (let loop ((block block)
		 (stack '()))
	 (with-access::blockV block (merge label succs)
	    (cond
	       ((not (eq? merge #unspecified))
		#unspecified)
	       ((memq block stack)
		(trace-item "mark merge " label)
		(set! merge #t))
	       (else
		(let ((nstack (cons block stack)))
		   (for-each (lambda (n)
				(loop n nstack))
		      succs)
		   (unless (eq? merge #t)
		      (set! merge #f)))))))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-ctx ...                                          */
;*    -------------------------------------------------------------    */
;*    Returns a list of <blockS, ctx> where blockS is the block to be  */
;*    replaced with a block generated for context ctx.                 */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-ctx bv::blockV) 
   (with-access::blockV bv (label)
      (with-trace 'bbv-merge (format "bbv-block-merge-ctx block=~a" label)
	 (let ((lvs (blockV-live-versions bv)))
	    (when (pair? lvs)
	       (with-access::blockS (car lvs) (ctx)
		  (when-trace 'bbv-merge
		     (for-each (lambda (bs num)
				  (with-access::blockS bs (ctx)
				     (trace-item (format "ctx.~a=" num)
					(shape ctx))))
			lvs (iota (length lvs))))
		  (let* ((regs (map bbv-ctxentry-reg (bbv-ctx-entries ctx)))
			 (rrs (map (lambda (r)
				      (let ((range (reg-range-merge r lvs)))
					 (cons r range)))
				 regs))
			 (typs (map (lambda (r)
				       (let ((typ (reg-type-merge r lvs)))
					  (cons r typ)))
				  regs)))
		     (or (merge-ranges lvs regs rrs)
			 (merge-types lvs regs typs)
			 (merge-top lvs regs)))))))))

;*---------------------------------------------------------------------*/
;*    merge-ranges ...                                                 */
;*---------------------------------------------------------------------*/
(define (merge-ranges lvs regs rrs)
   (let ((merge (filter-map (lambda (bs::blockS)
			       (let ((mctx (bbv-ctx-range-widen bs rrs)))
				  (when mctx
				     (with-access::blockS bs (ctx)
					(with-access::bbv-ctx ctx (id)
					   (trace-item "mctx(" id ") => "
					      (shape mctx))))
				     (cons bs mctx))))
		   lvs)))
      (when (pair? merge)
	 merge)))

;*---------------------------------------------------------------------*/
;*    merge-types ...                                                  */
;*---------------------------------------------------------------------*/
(define (merge-types lvs regs rrs)
   (let ((merge (filter-map (lambda (bs::blockS)
			       (let ((mctx (bbv-ctx-type-widen bs rrs)))
				  (when mctx
				     (with-access::blockS bs (ctx)
					(with-access::bbv-ctx ctx (id)
					   (trace-item "mctx(" id ") => "
					      (shape mctx))))
				     (cons bs mctx))))
		   lvs)))
      (when (pair? merge)
	 merge)))

;*---------------------------------------------------------------------*/
;*    merge-top ...                                                    */
;*---------------------------------------------------------------------*/
(define (merge-top lvs regs)
   (map (lambda (bs::blockS)
	   (let ((mctx (bbv-ctx-top-widen bs)))
	      (with-access::blockS bs (ctx)
		 (with-access::bbv-ctx ctx (id)
		    (trace-item "mctx(" id ") => "
		       (shape mctx)))
		 (cons bs mctx))))
      lvs))

;*---------------------------------------------------------------------*/
;*    reg-type-merge ...                                               */
;*    -------------------------------------------------------------    */
;*    Find the smallest type for variable X that is compatible         */
;*    with all the variable context types.                             */
;*---------------------------------------------------------------------*/
(define (reg-type-merge reg versions::pair-nil)
   (with-trace 'bbv-merge (format "reg-type-merge ~a" (shape reg))
      (let ((types (delete-duplicates (reg-types-get reg versions) eq?)))
	 (trace-item "types=" (map shape types))
	 types)))

;*---------------------------------------------------------------------*/
;*    reg-types-get ...                                                */
;*    -------------------------------------------------------------    */
;*    Get all the types of variable X in CTX                           */
;*---------------------------------------------------------------------*/
(define (reg-types-get reg versions::pair-nil)
   (append-map (lambda (bs)
		  (with-access::blockS bs (ctx)
		     (let ((e (bbv-ctx-get ctx reg)))
			(with-access::bbv-ctxentry e (polarity types)
			   (if polarity types (list *obj*))))))
      versions))

;*---------------------------------------------------------------------*/
;*    reg-range-merge ...                                              */
;*    -------------------------------------------------------------    */
;*    Find the smallest range for variable X that is compatible        */
;*    with all the variable context ranges.                            */
;*---------------------------------------------------------------------*/
(define (reg-range-merge reg versions::pair-nil)
   (with-trace 'bbv-merge (format "reg-range-merge ~a" (shape reg))
      (let ((ranges (reg-ranges-get reg versions)))
	 (trace-item "range=" (map shape ranges))
	 (when (pair? ranges)
	    (let loop ((o (bbv-max-fixnum))
		       (u (bbv-min-fixnum))
		       (r ranges))
	       (if (null? r)
		   (let ((range (reg-range-widening o u ranges)))
		      (trace-item "widening=" (shape range))
		      range)
		   (with-access::bbv-range (car r) (lo up)
		      (loop (min o lo) (max u up) (cdr r)))))))))

;*---------------------------------------------------------------------*/
;*    reg-ranges-get ...                                               */
;*    -------------------------------------------------------------    */
;*    Get all the ranges of variable X in CTX                          */
;*---------------------------------------------------------------------*/
(define (reg-ranges-get reg versions::pair-nil)
   (filter-map (lambda (bs)
		  (with-access::blockS bs (ctx)
		     (let ((e (bbv-ctx-get ctx reg)))
			(with-access::bbv-ctxentry e (polarity value)
			   (when (and polarity (isa? value bbv-range))
			      value)))))
      versions))

;*---------------------------------------------------------------------*/
;*    bbv-ctx-top-widen ...                                            */
;*    -------------------------------------------------------------    */
;*    Widen the register types of a version. Return either a widened   */
;*    context or #f. The arguments are:                                */
;*      - bs: the specialized block                                    */
;*      - tys: a list of <reg, types>                                  */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-top-widen bs::blockS)
   (with-access::blockS bs (ctx)
      (instantiate::bbv-ctx
	 (entries (map (lambda (e)
			  (duplicate::bbv-ctxentry e
			     (types (list *obj*))
			     (value '_)))
		     (with-access::bbv-ctx ctx (entries)
			entries))))))

;*---------------------------------------------------------------------*/
;*    bbv-ctx-type-widen ...                                           */
;*    -------------------------------------------------------------    */
;*    Widen the register types of a version. Return either a widened   */
;*    context or #f. The arguments are:                                */
;*      - bs: the specialized block                                    */
;*      - tys: a list of <reg, types>                                  */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-type-widen bs::blockS tys::pair-nil)
   (with-access::blockS bs (ctx)
      (let loop ((tys tys)
		 (mctx (instantiate::bbv-ctx))
		 (equal #t))
	 (cond
	    ((pair? tys)
	     (let* ((rr (car tys))
		    (reg (car rr))
		    (typs (cdr rr))
		    (oe (bbv-ctx-get ctx reg)))
		(if (and (pair? typs)
			 (not (pair? (cdr typs)))
			 (not (memq *obj* typs)))
		    (loop (cdr tys)
		       (extend-ctx/entry mctx oe)
		       equal)
		    (with-access::bbv-ctxentry oe (value)
		       (loop (cdr tys)
			  (let ((ne (duplicate::bbv-ctxentry oe
				       (types (list *obj*))
				       (value '_))))
			     (extend-ctx/entry mctx ne))
			  #f)))))
	    (equal
	     #f)
	    (else
	     mctx)))))

;*---------------------------------------------------------------------*/
;*    bbv-ctx-range-widen ...                                          */
;*    -------------------------------------------------------------    */
;*    Widen the register ranges of a version. Return either a widened  */
;*    context or #f. The arguments are:                                */
;*      - bs: the specialized block                                    */
;*      - rrs: a list of <reg, range>                                  */
;*---------------------------------------------------------------------*/
(define (bbv-ctx-range-widen bs::blockS rrs::pair-nil)
   (with-access::blockS bs (ctx)
      (let loop ((rrs rrs)
		 (mctx (instantiate::bbv-ctx))
		 (equal #t))
	 (cond
	    ((pair? rrs)
	     (let* ((rr (car rrs))
		    (reg (car rr))
		    (rng (cdr rr))
		    (oe (bbv-ctx-get ctx reg)))
		(if (not rng)
		    (loop (cdr rrs)
		       (extend-ctx/entry mctx oe)
		       equal)
		    (with-access::bbv-ctxentry oe (polarity value)
		       (cond
			  ((and polarity (isa? value bbv-range))
			   (loop (cdr rrs)
			      (let ((ne (duplicate::bbv-ctxentry oe
					   (value rng))))
				 (extend-ctx/entry mctx ne))
			      #f))
			  (else
			   (loop (cdr rrs)
			      (extend-ctx/entry mctx oe)
			      equal)))))))
	    (equal
	     #f)
	    (else
	     mctx)))))

;*---------------------------------------------------------------------*/
;*    reg-range-widening ...                                           */
;*    -------------------------------------------------------------    */
;*    Range widening. The produced range is larger than [o..u].        */
;*---------------------------------------------------------------------*/
(define (reg-range-widening o u ranges)

   (define widening #f)
   
   (define (low-widening v)
      (set! widening #t)
      (cond
	 ((>fx v 1) (/fx v 2))
	 ((=fx v 1) 0)
	 ((=fx v 0) -1)
	 ((>fx v -16) (*fx v 2))
	 ((>fx v -255) -255)
	 (else (bbv-min-fixnum))))

   (define (up-widening v)
      (set! widening #t)
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
      (unless (and (not widening) (= no o) (= nu u))
	 ;; no widening possible because we are dealing with a constant
	 (instantiate::bbv-range
	    (lo no)
	    (up nu)))))

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
      
