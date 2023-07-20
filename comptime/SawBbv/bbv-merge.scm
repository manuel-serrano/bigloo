;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-merge.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Thu Jul 20 08:01:00 2023 (serrano)                */
;*    Copyright   :  2022-23 Manuel Serrano                            */
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
   (multiple-value-bind (bs1 bs2)
      (bbv-block-merge-select bs)
      (with-access::blockS bs1 ((ctx1 ctx))
	 (with-access::blockS bs2 ((ctx2 ctx))
	    (values bs1 bs2 (merge-ctx ctx1 ctx2))))))

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
	  ((size) (bbv-block-merge-select-strategy-size bs))
	  ((distance) (bbv-block-merge-select-strategy-distance bs))
	  ((random) (bbv-block-merge-select-strategy-random bs))
	  ((first) (bbv-block-merge-select-strategy-first bs))
	  (else (error "bbv-block-merge-select" "strategy not implemented" *bbv-merge-strategy*)))))

;*---------------------------------------------------------------------*/
;*    bbv-block-merge-select-strategy-size ...                         */
;*    -------------------------------------------------------------    */
;*    Select the two smallest contexts.                                */
;*---------------------------------------------------------------------*/
(define (bbv-block-merge-select-strategy-size bs::pair)
   
   (define (range-size r::bbv-range)
      (with-access::bbv-range r (lo up)
	 (cond
	    ((or (not (fixnum? lo)) (not (fixnum? up))) 5)
	    ((and (>=fx lo -128) (<=fx up 127)) 1)
	    ((and (>=fx lo 0) (<=fx up 255)) 1)
	    ((and (>=fx lo -65536) (<=fx up -65535)) 2)
	    ((and (>=fx lo 0) (<=fx up 536870912)) 3)
	    (else 4))))
   
   (define (entry-size e::bbv-ctxentry)
      (with-access::bbv-ctxentry e (types polarity value)
	 (cond
	    ((not polarity) (length types))
	    ((eq? (car types) *obj*) 100)
	    ((isa? value bbv-range) (range-size value))
	    (else (*fx 10 (length types))))))
   
   (define (ctx-size ctx::bbv-ctx)
      (with-access::bbv-ctx ctx (entries)
	 (apply + (map entry-size entries))))
   
   (define (block-size b::blockS)
      (with-access::blockS b (ctx)
	 (ctx-size ctx)))
   
   (let ((l (sort (map (lambda (b) (cons (block-size b) b)) bs)
	       (lambda (x y) (<=fx (car x) (car y))))))
      (values (cdr (car l)) (cdr (cadr l)))))
	   
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
   
   (define (same-types? types1 types2)
      (when (=fx (length types1) (length types2))
	 (every (lambda (t) (memq t types2)) types1)))
   
   (define (bbv-ctxentry-top)
      (duplicate::bbv-ctxentry e1
	 (types (list *obj*))
	 (polarity #t)
	 (value '_)))
   
   (define (merge-range range1 range2)
      (with-access::bbv-range range1 ((lo1 lo) (up1 up))
	 (with-access::bbv-range range2 ((lo2 lo) (up2 up))
	    ;; widening
	    (range-widening (min lo1 lo2) (max up1 up2)
	       (list range1 range2)))))
   
   (define (types-intersection ts1 ts2)
      ;; intersection of ts1 types and ts2 types
      (filter (lambda (t) (memq t ts2)) ts1))
   
   (with-access::bbv-ctxentry e1 ((polarity1 polarity)
				  (types1 types)
				  (value1 value))
      (with-access::bbv-ctxentry e2 ((polarity2 polarity)
				     (types2 types)
				     (value2 value))
	 (cond
	    ((not (eq? polarity1 polarity2))
	     (bbv-ctxentry-top))
	    ((not polarity1)
	     (let ((ts (types-intersection types1 types2)))
		(if (null? ts)
		    (bbv-ctxentry-top)
		    (duplicate::bbv-ctxentry e1
		       (types ts)
		       (value '_)))))
	    ((not (same-types? types1 types2))
	     (let ((ts (types-intersection types1 types2)))
		(if (null? ts)
		    (bbv-ctxentry-top)
		    (duplicate::bbv-ctxentry e1
		       (types ts)
		       (value '_)))))
	    ((or (not (bbv-range? value1)) (not (bbv-range? value2)))
	     (duplicate::bbv-ctxentry e1
		(value '_)))
	    (else
	     (let ((range (merge-range value1 value2)))
		(duplicate::bbv-ctxentry e1
		   (value (or range (fixnum-range))))))))))

;*---------------------------------------------------------------------*/
;*    range-widening ...                                               */
;*    -------------------------------------------------------------    */
;*    Range widening. The produced range is larger than [o..u].        */
;*---------------------------------------------------------------------*/
(define (range-widening o u ranges)

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

