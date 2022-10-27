;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-merge.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 13 08:00:37 2022                          */
;*    Last change :  Thu Oct 27 11:47:47 2022 (serrano)                */
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

   (export  (bbv-block-merge-ctx ::blockV)
	    (bbv-block-merge ::pair-nil)))

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
   (tprint "TOP")
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
		   (let ((range (range-widening o u ranges)))
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

;*---------------------------------------------------------------------*/
;*    bbv-block-merge ...                                              */
;*    -------------------------------------------------------------    */
;*    lvs is a list of blockS. The result is three values. Two         */
;*    block to be replaced and a ctx of the new block.                 */
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
   
   (with-access::bbv-ctxentry e1 ((polarity1 polarity)
				  (types1 types)
				  (value1 value))
      (with-access::bbv-ctxentry e2 ((polarity2 polarity)
				     (types2 types)
				     (value2 value))
	 (cond
	    ((not (eq? polarity1 polarity2))
	     (bbv-ctxentry-top))
	    ((not (same-types? types1 types2))
	     (bbv-ctxentry-top))
	    ((not (and (bbv-range? value1) (bbv-range? value2)))
	     (bbv-ctxentry-top))
	    ((not polarity1)
	     (bbv-ctxentry-top))
	    (else
	     (let ((range (merge-range value1 value2)))
		(if (isa? range bbv-range)
		    (duplicate::bbv-ctxentry e1
		       (value range))
		    (bbv-ctxentry-top))))))))
