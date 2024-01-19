;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-merge.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Thu Jan 18 10:47:57 2024 (serrano)                */
;*    Copyright   :  2022-24 Manuel Serrano                            */
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

   (export  (bbv-block-merge ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge ...                                              */
;*    -------------------------------------------------------------    */
;*    lvs is a list of blockS. The result is three values. Two         */
;*    blocks to be replaced and a ctx of the new block.                */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge bs::pair-nil)
   (with-trace 'bbv-merge
	 (format "bbv-block-merge {~a} ~( )"
	    (block-label (blockS-parent (car bs)))
	    (map block-label bs))
      (multiple-value-bind (bs1 bs2)
	 (bbv-block-merge-select bs)
	 (with-access::blockS bs1 ((ctx1 ctx))
	    (with-access::blockS bs2 ((ctx2 ctx))
	       (trace-item (block-label bs1) " ctx: " (shape ctx1))
	       (trace-item (block-label bs2) " ctx: " (shape ctx2))
	       (let ((mctx (merge-ctx ctx1 ctx2)))
		  (trace-item "mctx=" (shape mctx))
		  (values bs1 bs2 mctx)))))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select ...                                       */
;*    -------------------------------------------------------------    */
;*    Select two versions to merge.                                    */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select bs::pair-nil)
   (if (and (pair? bs) (pair? (cdr bs)) (null? (cddr bs)))
       ;; After this test length(bs) > 2. This is assumed in all
       ;; the strategies implementation
       (values (car bs) (cadr bs))
       (case *bbv-merge-strategy*
	  ((nearobj) (bbv-block-merge-select-strategy-nearobj bs))
	  ((samepositive) (bbv-block-merge-select-strategy-samepositive bs))
	  ((size) (bbv-block-merge-select-strategy-size bs))
	  ((distance) (bbv-block-merge-select-strategy-distance bs))
	  ((random) (bbv-block-merge-select-strategy-random bs))
	  ((first) (bbv-block-merge-select-strategy-first bs))
	  (else (error "bbv-block-merge-select" "strategy not implemented" *bbv-merge-strategy*)))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-nearobj ...                      */
;*    -------------------------------------------------------------    */
;*    Before applying the "size" strategy checks if two contexts       */
;*    are "near obj". A context is near obj if all its types are       */
;*    either obj or negative polarities.                               */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-nearobj bs::pair)
   
   (define (nearobj b::blockS)
      (with-access::blockS b (ctx)
	 (with-access::bbv-ctx ctx (entries)
	    (every (lambda (e::bbv-ctxentry)
		      (with-access::bbv-ctxentry e (types polarity)
			 (or (eq? (car types) *obj*) (not polarity))))
	       entries))))
   
   (define (fullobj b::blockS)
      (with-access::blockS b (ctx)
	 (with-access::bbv-ctx ctx (entries)
	    (every (lambda (e::bbv-ctxentry)
		      (with-access::bbv-ctxentry e (types polarity)
			 (eq? (car types) *obj*)))
	       entries))))
   
   (with-trace 'bbv-merge "bbv-block-merge-select-strategy-nearobj"
      (for-each (lambda (b)
		   (trace-item "{" (block-label b)
		      "} fullobj: " (fullobj b)
		      " nearobj: " (nearobj b)
		      " " (with-access::blockS b (ctx) (shape ctx))))
	 bs)
      (let ((bnb (filter nearobj bs)))
	 (if (and (pair? bnb) (pair? (cdr bnb)))
	     (let ((fb (find fullobj bs)))
		(cond
		   (fb
		    (let ((nfb (find (lambda (b) (not (eq? b fb))) bnb)))
		       (values nfb fb)))
		   ((pair? (cdr bnb))
		    (values (car bnb) (cadr bnb)))
		   (else
		    (bbv-block-merge-select-strategy-samepositive bs))))
	     (bbv-block-merge-select-strategy-samepositive bs)))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-samepositive ...                 */
;*    -------------------------------------------------------------    */
;*    Select two contexts that are only distinguished by negative      */
;*    polarities                                                       */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-samepositive bs::pair)

   (define (list-eq? x y)
      (every eq? x y))
   
   (define (same-positive? x y)
      (with-access::blockS x ((xctx ctx))
	 (with-access::blockS y ((yctx ctx))
	    (with-access::bbv-ctx xctx ((xentries entries))
	       (with-access::bbv-ctx yctx ((yentries entries))
		  (every (lambda (ex ey)
			    (with-access::bbv-ctxentry ex ((xtypes types)
							   (xpol polarity))
			       (with-access::bbv-ctxentry ey ((ytypes types)
							      (ypol polarity))
				  (if (not xpol)
				      (not ypol)
				      (list-eq? xtypes xtypes)))))
		     xentries yentries))))))
   
   (with-trace 'bbv-merge "bbv-block-merge-select-strategy-samepositive"
      (let loop ((pbs bs))
	 (if (null? pbs)
	     (bbv-block-merge-select-strategy-size bs)
	     (let ((sb (find (lambda (b)
				(same-positive? (car pbs) b))
			  (cdr pbs))))
		(if sb
		    (values (car pbs) sb)
		    (loop (cdr pbs))))))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-size ...                         */
;*    -------------------------------------------------------------    */
;*    Select the two smallest contexts.                                */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-size bs::pair)
   
   (define (range-size r::bbv-range)
      (with-access::bbv-range r (lo up)
	 (cond
	    ((and (isa? lo bbv-vlen) (isa? up bbv-vlen)) 4)
	    ((or (not (fixnum? lo)) (not (fixnum? up))) 6)
	    ((and (>=fx lo -128) (<=fx up 127)) 1)
	    ((and (>=fx lo 0) (<=fx up 255)) 1)
	    ((and (>=fx lo -65536) (<=fx up -65535)) 2)
	    ((and (>=fx lo 0) (<=fx up 536870912)) 3)
	    (else 5))))
   
   (define (entry-size e::bbv-ctxentry)
      (with-access::bbv-ctxentry e (types polarity value)
	 (cond
	    ((not polarity) (minfx 100 (*fx 50 (length types))))
	    ((eq? (car types) *obj*) 100)
	    ((isa? value bbv-range) (range-size value))
	    (else (*fx 10 (length types))))))
   
   (define (ctx-size ctx::bbv-ctx)
      (with-access::bbv-ctx ctx (entries)
	 (apply + (map entry-size entries))))
   
   (define (block-size b::blockS)
      (with-access::blockS b (ctx)
	 (ctx-size ctx)))
   
   (define (block-max-size b::blockS)
      (with-access::blockS b (ctx)
	 (with-access::bbv-ctx ctx (entries)
	    (*fx 100 (length entries)))))
   
   (with-trace 'bbv-merge "bbv-block-merge-select-strategy-size"
      (for-each (lambda (b)
		   (trace-item "{" (block-label b) "} size: "
		      (block-size b)
		      " " (with-access::blockS b (ctx) (shape ctx))))
	 bs)
      (let* ((maxs (block-max-size (car bs)))
	     (bsz (map (lambda (b) (cons (block-size b) b)) bs))
	     (mbsz (filter (lambda (p) (=fx (car p) maxs)) bsz)))
	 (if (and (pair? mbsz) (pair? (cdr mbsz)))
	     ;; two contexts are "top-equivalent", pick these ones
	     (values (cdr (car mbsz)) (cdr (cadr mbsz)))
	     ;; pick the two smallest context
	     (let ((l (sort bsz (lambda (x y) (<=fx (car x) (car y))))))
		(values (cdr (car l)) (cdr (cadr l))))))))
	   
;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-distance ...                     */
;*    -------------------------------------------------------------    */
;*    Select the two closest contexts.                                 */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-distance bs::pair)

   (define (dist-entry x::bbv-ctxentry y::bbv-ctxentry)
      (with-access::bbv-ctxentry x ((xpolarity polarity)
				    (xtypes types)
				    (xvalue value))
	 (with-access::bbv-ctxentry y ((ypolarity polarity)
				       (ytypes types)
				       (yvalue value))
	    (cond
	       ((and (equal? xtypes ytypes) (eq? xpolarity ypolarity))
		(cond
		   ((and (isa? xvalue bbv-range) (isa? yvalue bbv-range))
		    (with-access::bbv-range xvalue ((lo1 lo) (up1 up))
		       (with-access::bbv-range yvalue ((lo2 lo) (up2 up))
			  (if (and (=fx lo1 lo2) (=fx up1 up2))
			      0
			      1))))
		   ((or (isa? xvalue bbv-range) (isa? yvalue bbv-range))
		    3)
		   (else
		    2)))
	       ((not (eq? xpolarity ypolarity))
		10)
	       ((not xpolarity)
		(if (or (every (lambda (t) (memq t ytypes)) xtypes)
			(every (lambda (t) (memq t xtypes)) ytypes))
		    4
		    5))
	       ((or (memq *obj* xtypes) (memq *obj* ytypes))
		4)
	       (else
		11)))))
      
   (define (ctx-dist xctx::bbv-ctx yctx::bbv-ctx)
      (with-access::bbv-ctx xctx (entries)
	 (apply +
	    (map (lambda (e)
		    (with-access::bbv-ctxentry e (reg)
		       (dist-entry e (bbv-ctx-get yctx reg))))
	       entries))))
   
   (define (block-dist p::pair)
      (let ((x (car p))
	    (y (cdr p)))
	 (if (eq? x y)
	     (cons (maxvalfx) p)
	     (with-access::blockS x ((xctx ctx))
		(with-access::blockS y ((yctx ctx))
		   (cons (ctx-dist xctx yctx) p))))))
   
   (let ((l (sort (map block-dist
		     (append-map (lambda (x)
				    (map (lambda (y) (cons x y)) bs))
			bs))
	       (lambda (x y) (<=fx (car x) (car y))))))
      (values (car (cdar l)) (cdr (cdar l)))))
   
;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-random ...                       */
;*    -------------------------------------------------------------    */
;*    Peek two random blocks for merging.                              */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-random bs::pair)
   (let* ((len (length bs))
	  (x (random len)))
      (let loop ()
	 (let ((y (random len)))
	    (if (=fx x y)
		(loop)
		(values (list-ref bs x) (list-ref bs y)))))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-first ...                        */
;*    -------------------------------------------------------------    */
;*    Peek two first blocks for merging.                               */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-first bs::pair)
   (values (car bs) (cadr bs)))

;*---------------------------------------------------------------------*/
;*    merge-ctx ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function merges two bbv ctx and widen the result.           */
;*    Each ctx is a list of entries. An entry is a register and        */
;*    a property. Each list has exactly the same list of               */
;*    registers. So, merging the ctx means merge the register          */
;*    information.                                                     */
;*---------------------------------------------------------------------*/
(define (merge-ctx ctx1::bbv-ctx ctx2::bbv-ctx)
   (with-access::bbv-ctx ctx1 (entries)
      (instantiate::bbv-ctx
	 (entries (map (lambda (e)
			  (with-access::bbv-ctxentry e (reg)
			     (merge-ctxentry e (bbv-ctx-get ctx2 reg))))
		     entries)))))

;*---------------------------------------------------------------------*/
;*    merge-ctxentry ...                                               */
;*---------------------------------------------------------------------*/
(define (merge-ctxentry e1::bbv-ctxentry e2::bbv-ctxentry)

   (define (eq-type? x y)
      (or (eq? x y)
	  (and (eq? x *long*) (eq? y *bint*))
	  (and (eq? x *bint*) (eq? y *long*))))
   
   (define (same-types? types1 types2)
      (when (=fx (length types1) (length types2))
	 (every (lambda (t)
		   (let loop ((types2 types2))
		      (cond
			 ((null? types2) #f)
			 ((eq-type? t (car types2)) #t)
			 (else (loop (cdr types2))))))
	    types1)))
   
   (define (bbv-ctxentry-top)
      (duplicate::bbv-ctxentry e1
	 (types (list *obj*))
	 (polarity #t)
	 (value '_)))
   
   (define (merge-range range1 range2)
      (with-access::bbv-range range1 ((lo1 lo) (up1 up))
	 (with-access::bbv-range range2 ((lo2 lo) (up2 up))
	    ;; widening
	    (range-widening (minrv lo1 lo2 (bbv-min-fixnum))
	       (maxrv up1 up2 (bbv-max-fixnum))
	       (list range1 range2)))))
   
   (define (types-intersection ts1 ts2)
      ;; intersection of ts1 types and ts2 types
      (filter (lambda (t) (memq t ts2)) ts1))
   
   (define (list-eq? l1 l2)
      (when (=fx (length l1) (length l2))
	 (every eq? l1 l2)))
   
   (with-trace 'bbv-merge "merge-ctxentry"
      (with-access::bbv-ctxentry e1 ((polarity1 polarity)
				     (types1 types)
				     (value1 value)
				     (aliases1 aliases))
	 (with-access::bbv-ctxentry e2 ((polarity2 polarity)
					(types2 types)
					(value2 value)
					(aliases2 aliases))
	    (cond
	       ((not (eq? polarity1 polarity2))
		(trace-item "polarities differ")
		(bbv-ctxentry-top))
	       ((not polarity1)
		(trace-item "merge-range.not polarity")
		(let ((ts (types-intersection types1 types2)))
		   (if (null? ts)
		       (bbv-ctxentry-top)
		       (duplicate::bbv-ctxentry e1
			  (aliases (if (list-eq? aliases1 aliases2) aliases1 '()))
			  (types ts)
			  (value '_)))))
	       ((not (same-types? types1 types2))
		(trace-item "not same types types1="
		   (map shape types1) " " (map shape types2))
		(let ((ts (types-intersection types1 types2)))
		   (if (null? ts)
		       (bbv-ctxentry-top)
		       (duplicate::bbv-ctxentry e1
			  (aliases (if (list-eq? aliases1 aliases2) aliases1 '()))
			  (types ts)
			  (value '_)))))
	       ((or (not (bbv-range? value1)) (not (bbv-range? value2)))
		(trace-item "not both ranges value1=" (shape value1)
		   " value2=" (shape value2))
		(duplicate::bbv-ctxentry e1
		   (aliases (if (list-eq? aliases1 aliases2) aliases1 '()))
		   (value '_)))
	       (else
		(let ((range (merge-range value1 value2)))
		   (trace-item "merge-range " (shape value1) " " (shape value2)
		      " -> " (shape range))
		   (duplicate::bbv-ctxentry e1
		      (aliases (if (list-eq? aliases1 aliases2) aliases1 '()))
		      (value (or range (fixnum-range)))))))))))

;*---------------------------------------------------------------------*/
;*    range-widening ...                                               */
;*---------------------------------------------------------------------*/
(define (range-widening l u ranges)
   
   (define widening #f)
   
   (define (low-widening-vlen v)
      (if (< (bbv-vlen-offset v) -10)
	  (bbv-min-fixnum)
	  v))
   
   (define (up-widening-vlen v)
      (if (> (bbv-vlen-offset v) 10)
	  (bbv-max-fixnum)
	  v))
   
   (define (low-widening v)
      (set! widening #t)
      (cond
	 ((bbv-vlen? v) (low-widening-vlen v))
	 ((not (fixnum? v)) v)
	 ((>fx v 1) (/fx v 2))
	 ((=fx v 1) 0)
	 ((=fx v 0) v)
	 ((>fx v -16) (*fx v 2))
	 ((>fx v -255) -255)
	 ((>=fx v (+fx (bbv-min-fixnum) 1)) (+fx (bbv-min-fixnum) 1))
	 (else (bbv-min-fixnum))))
   
   (define (low-widening-vlen v)
      (if (< (bbv-vlen-offset v) -10)
	  (bbv-min-fixnum)
	  v))
   
   (define (up-widening v)
      (set! widening #t)
      (cond
	 ((bbv-vlen? v) (up-widening-vlen v))
	 ((not (fixnum? v)) v)
	 ((<fx v -1) (/fx v 2))
	 ((=fx v -1) 0)
	 ((=fx v 0) v)
	 ((<fx v 16) (*fx v 2))
	 ((<fx v 255) 255)
	 ((<fx v 65535) 65535)
	 ((<=fx v (-fx (bbv-max-fixnum) 1)) (-fx (bbv-max-fixnum) 1))
	 (else (bbv-max-fixnum))))
   
   (let ((nl (if (every (lambda (r)
			   (with-access::bbv-range r (lo up)
			      (eq? (=rv l lo) #t)))
		    ranges)
		 l
		 (low-widening l)))
	 (nu (if (every (lambda (r)
			   (with-access::bbv-range r (up)
			      (eq? (=rv u up) #t)))
		    ranges)
		 u
		 (up-widening u))))
      (cond
	 ((and (eq? (=rv nl l) #t) (eq? (=rv nu u) #t) (not widening))
	  (car ranges))
	 ((every (lambda (r)
		    (with-access::bbv-range (car ranges) ((up0 up))
		       (with-access::bbv-range r (up)
			  (and (bbv-vlen? up0)
			       (bbv-vlen? up)
			       (eq? (bbv-vlen-vec up0) (bbv-vlen-vec up))))))
	     ranges)
	  ;; ensute that at least one bound change
	  (if (eq? nu u)
	      (instantiate::bbv-range
		 (lo nl)
		 (up u))
	      (instantiate::bbv-range
		 (lo l)
		 (up nu))))
	 (else
	  (instantiate::bbv-range
	     (lo nl)
	     (up nu))))))


