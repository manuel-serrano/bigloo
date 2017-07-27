;*=====================================================================*/
;*    .../prgm/project/bigloo/comptime/SawMill/bbv-specialize.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 07:42:00 2017                          */
;*    Last change :  Thu Jul 27 13:25:59 2017 (serrano)                */
;*    Copyright   :  2017 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV instruction specialization                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-specialize
   
   (include "Tools/trace.sch"
	    "SawMill/regset.sch"
	    "SawMill/bbv-types.sch"
	    "SawMill/bbv-interval.sch")
   
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
	    saw_regutils
	    saw_bbv-types
	    saw_bbv-cache)
   
   (export (rtl_ins-specialize ::rtl_ins ::pair-nil)
	   (rtl_ins-typecheck?::bool ::rtl_ins)
	   (rtl_ins-intcmp?::bool ::rtl_ins)
	   (rtl_ins-specialize-intcmp i ctx)))

;*---------------------------------------------------------------------*/
;*    basic-block versionning configuration                            */
;*---------------------------------------------------------------------*/
(define *type-call* #t)

;*---------------------------------------------------------------------*/
;*    intervals                                                        */
;*---------------------------------------------------------------------*/
(define *infinity-intv* (interval *-inf.0* *+inf.0*))
(define *length-intv* (interval #l0 *max-length*))
(define *fixnum-intv* (interval *min-fixnum* *max-fixnum*))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize ...                                           */
;*    -------------------------------------------------------------    */
;*    Specialize a instruction according to the typing context.        */
;*    Returns the new instruction and the new context.                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize i::rtl_ins ctx::pair-nil)
   
   (with-trace 'bbv-ins "ins"
      (trace-item "ins=" (shape i))
      (trace-item "ctx=" (ctx->string ctx))
      (cond
	 ((rtl_ins-typecheck? i)
	  (rtl_ins-specialize-typecheck i ctx))
;* 	 ((rtl_ins-vector-bound-check? i)                              */
;* 	  (rtl_ins-specialize-vector-bound-check i ctx))               */
;*  	 ((rtl_ins-bool? i)                                            */
;* 	  (rtl_ins-specialize-bool i ctx))                             */
	 ((rtl_ins-go? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (with-access::rtl_ins i (fun)
	     (let ((s (duplicate::rtl_ins/bbv i
			 (fun (duplicate::rtl_go fun)))))
		(values s ctx))))
	 ((rtl_ins-ifeq? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (case (bool-value (car (rtl_ins-args i)) ctx)
	     ((true)
	      (let ((s (duplicate::rtl_ins/bbv i
			 (fun (instantiate::rtl_nop)))))
		 (values s ctx)))
	     ((false)
	      (with-access::rtl_ins i (fun)
		 (with-access::rtl_ifeq fun (then)
		    (let ((s (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_go
					(to then))))))
		       (values s ctx)))))
	     (else
	      (with-access::rtl_ins i (fun)
		 (let ((s (duplicate::rtl_ins/bbv i
			     (fun (duplicate::rtl_ifeq fun)))))
		    (values s ctx))))))
	 ((rtl_ins-ifne? i)
	  ;; have to duplicate the instruction to break "to" sharing
	  (case (bool-value (car (rtl_ins-args i)) ctx)
	     ((true)
	      (with-access::rtl_ins i (fun)
		 (with-access::rtl_ifeq fun (then)
		    (let ((s (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_go
					(to then))))))
		       (values s ctx)))))
	     ((false)
	      (let ((s (duplicate::rtl_ins/bbv i
			  (fun (instantiate::rtl_nop)))))
		 (values s ctx)))
	     (else
	      (with-access::rtl_ins i (fun)
		 (let ((s (duplicate::rtl_ins/bbv i
			     (fun (duplicate::rtl_ifne fun)))))
		    (values s ctx))))))
	 ((not (rtl_reg? (rtl_ins-dest i)))
	  (values i ctx))
	 ((rtl_ins-mov? i)
	  (with-access::rtl_ins i (dest args fun)
	     (cond
		((and (pair? args) (null? (cdr args)) (rtl_reg/ra? (car args)))
		 (values i (alias-ctx ctx dest (car args))))
		((and *type-call* (pair? args) (rtl_ins-call? (car args)))
		 (with-access::rtl_ins (car args) (fun)
		    (with-access::rtl_call fun (var)
		       (with-access::global var (value type)
			  (values i (extend-normalize-ctx ctx dest type #t))))))
		(else
		 (values i (extend-ctx ctx dest *obj* #t))))))
	 ((and *type-call* (rtl_ins-call? i))
	  (with-access::rtl_ins i (dest fun)
	     (with-access::rtl_call fun (var)
		(with-access::global var (value type)
		   (if (fun? value)
		       (values i (extend-normalize-ctx ctx dest type #t))
		       (values i (extend-ctx ctx dest *obj* #t)))))))
	 ((rtl_ins-vlen? i)
	  (rtl_ins-specialize-vlength i ctx))
	 (else
	  (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-typecheck? ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns the type checked by the instruction or false.            */
;*---------------------------------------------------------------------*/
(define (rtl_ins-typecheck? i::rtl_ins)
   (when (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))
      (with-access::rtl_ins i (args)
	 (when (and (isa? (car args) rtl_ins) (rtl_ins-call? (car args)))
	    (let ((typ (rtl_call-predicate (car args))))
	       (when typ
		  (let ((args (rtl_ins-args* i)))
		     (and (pair? args) (null? (cdr args)) (rtl_reg? (car args))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-typecheck ...                                 */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-typecheck i::rtl_ins ctx::pair-nil)
   (multiple-value-bind (reg type flag)
      (rtl_ins-typecheck i)
      (trace-item "typ=" (shape type) " flag=" flag)
      (let ((e (ctx-get ctx reg)))
	 (cond
	    ((or (not e) (eq? (bbv-ctxentry-typ e) *obj*))
	     (with-access::rtl_ins i (fun)
		(let ((s (duplicate::rtl_ins/bbv i
			    (fun (if (isa? fun rtl_ifeq)
				     (duplicate::rtl_ifeq fun)
				     (duplicate::rtl_ifne fun))))))
		   (values s ctx))))
	    ((and (eq? (bbv-ctxentry-typ e) type) (bbv-ctxentry-flag e))
	     (with-access::rtl_ins/bbv i (fun)
		(let ((s (if (isa? fun rtl_ifeq)
			     (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_nop))
				(dest #f)
				(args '()))
			     (with-access::rtl_ifne fun (then)
				(duplicate::rtl_ins/bbv i
				   (fun (instantiate::rtl_go (to then)))
				   (dest #f)
				   (args '()))))))
		   (values s ctx))))
	    ((and (eq? (bbv-ctxentry-typ e) type) (not (bbv-ctxentry-flag e)))
	     (with-access::rtl_ins/bbv i (fun)
		(let ((s (if (isa? fun rtl_ifeq)
			     (with-access::rtl_ifeq fun (then)
				(duplicate::rtl_ins/bbv i
				   (fun (instantiate::rtl_go (to then)))
				   (dest #f)
				   (args '())))
			     (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_nop))
				(dest #f)
				(args '())))))
		   (values s ctx))))
	    ((and (not (eq? (bbv-ctxentry-typ e) type)) (bbv-ctxentry-flag e))
	     (with-access::rtl_ins/bbv i (fun)
		(let ((s (if (isa? fun rtl_ifne)
			     (duplicate::rtl_ins/bbv i
				(fun (instantiate::rtl_nop))
				(dest #f)
				(args '()))
			     (with-access::rtl_ifeq fun (then)
				(duplicate::rtl_ins/bbv i
				   (fun (instantiate::rtl_go (to then)))
				   (dest #f)
				   (args '()))))))
		   (values s ctx))))
	    (else
	     ;; branch used the the flag differs, might be improved
	     ;; in the future
	     (with-access::rtl_ins i (fun)
		(let ((s (duplicate::rtl_ins/bbv i
			    (fun (if (isa? fun rtl_ifeq)
				     (duplicate::rtl_ifeq fun)
				     (duplicate::rtl_ifne fun))))))
		   (values s ctx))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-bool ...                                      */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-bool i ctx)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))
   
   (with-access::rtl_ins i (dest fun args)
      (with-access::rtl_call fun (var)
	 (cond
	    ((and (eq? var *<fx*) (=fx (length args) 2))
	     (let ((left (interval-value (car args) ctx))
		   (right (interval-value (cadr args) ctx)))
		(cond
		   ((not (and (interval? left) (interval? right)))
		    (values i (extend-ctx ctx dest *bool* #t)))
		   ((interval<? left right)
		    (let ((ni (duplicate::rtl_ins/bbv i
				 (fun (true)))))
		       (values i (extend-ctx ctx dest *bool* #t #t))))
		   ((interval>=? left right)
		    (let ((ni (duplicate::rtl_ins/bbv i
				 (fun (false)))))
		       (values i (extend-ctx ctx dest *bool* #t #f))))
		   (else
		    (values i (extend-ctx ctx dest *bool* #t))))))
	    (else
	     (with-access::rtl_ins i (dest)
		(values i (extend-ctx ctx dest *bool* #t))))))))
   
;*---------------------------------------------------------------------*/
;*    interval-value ...                                               */
;*---------------------------------------------------------------------*/
(define (interval-value i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (ctx-get ctx i)))
	  (when (and e (eq? (bbv-ctxentry-typ e) *int*))
	     (let ((v (bbv-ctxentry-value e)))
		(cond
		   ((interval? v) v)
		   ((vector? v) *length-intv*))))))
      ((rtl_ins-mov? i)
       (interval-value (car (rtl_ins-args i)) ctx))
      ((rtl_ins-call? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_call fun (var)
	     (cond
		((eq? var *int->long*)
		 (interval-value (car (rtl_ins-args i)) ctx))
		(else
		 #f)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bool-value ...                                                   */
;*---------------------------------------------------------------------*/
(define (bool-value i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (ctx-get ctx i)))
	  (cond
	     ((or (not e) (not (eq? (bbv-ctxentry-typ e) *bool*))) '_)
	     ((eq? (bbv-ctxentry-value e) #t) 'true)
	     ((eq? (bbv-ctxentry-value e) #f) 'false)
	     (else '_))))
      ((rtl_ins-mov? i)
       (bool-value (car (rtl_ins-args i)) ctx))
      ((rtl_ins-loadi? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_loadi fun (constant)
	     (cond
		((not (isa? constant literal)) '_)
		((eq? (literal-value constant) #t) 'true)
		((eq? (literal-value constant) #f) 'false)
		(else '_)))))
      (else
       '_)))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vlength ...                                   */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vlength i ctx)
   (with-access::rtl_ins i (dest args)
      (cond
	 ((not (rtl_reg? (car args)))
	  (values i (extend-ctx ctx dest *int* #t *length-intv*)))
	 ((find (lambda (e)
		   (with-access::bbv-ctxentry e (typ value)
		      (and (eq? type *int*)
			   (vector? value)
			   (eq? (vector-ref value 0) (car args)))))
	     ctx)
	  =>
	  (lambda (e)
	     (values (duplicate::rtl_ins i
			(fun (instantiate::rtl_mov)))
		(alias-ctx ctx dest (car args)))))
	 (else
	  (values i (extend-ctx ctx dest *int* #t (vector (car args))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-intcmp? ...                                              */
;*---------------------------------------------------------------------*/
(define (rtl_ins-intcmp? i)

   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (rtl_reg? dest))))))
   
   (with-access::rtl_ins i (dest fun args)
      (when (isa? fun rtl_call)
	 (with-access::rtl_call fun (var)
	    (and (=fx (length args) 2)
		 (or (eq? var *<fx*) (eq? var *<=fx*)
		     (eq? var *>fx*) (eq? var *>=fx*)
		     (eq? var *=fx*))
		 (or (reg? (car args)) (rtl_ins-loadi? (car args)))
		 (or (reg? (cadr args)) (rtl_ins-loadi? (cadr args))))))))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-intcmp ...                                    */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-intcmp i ctx)
   
   (define (true)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #t)))))
   
   (define (false)
      (instantiate::rtl_loadi
	 (constant (instantiate::literal (type *bool*) (value #f)))))

   (define (rtl_bint->long? a)
      (when (isa? a rtl_ins)
	 (with-access::rtl_ins a (fun)
	    (when (isa? fun rtl_call)
	       (with-access::rtl_call fun (var)
		  (eq? var *bint->long*))))))
   
   (define (reg? a)
      (or (rtl_reg? a)
	  (and (rtl_ins? a)
	       (with-access::rtl_ins a (fun args dest)
		  (when (isa? fun rtl_call)
		     (with-access::rtl_call fun (var)
			(if (eq? var *bint->long*)
			    (rtl_reg? (car args))
			    (rtl_reg? dest))))))))

   (define (reg a)
      (cond
	 ((rtl_reg? a) a)
	 ((rtl_bint->long? a) (car (rtl_ins-args a)))
	 (else (rtl_ins-dest a))))

   (define (intcmp-op i)
      (with-access::rtl_ins i (fun)
	 (with-access::rtl_call fun (var)
	    (cond
	       ((eq? var *<fx*) '<)
	       ((eq? var *<=fx*) '<=)
	       ((eq? var *>fx*) '>)
	       ((eq? var *>=fx*) '>=)
	       ((eq? var *=fx*) '=)))))
   
   (define (inv-op op)
      (case op
	 ((<) '>)
	 ((<=) '>=)
	 ((>) '<)
	 ((>=) '<=)
	 (else op)))
   
   (define (resolve/op i op intl intr)
      (case op
	 ((<)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval<? intl intr) (true) (false)))))
	 ((<=)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval<=? intl intr) (true) (false)))))
	 ((>=)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval>=? intl intr) (true) (false)))))
	 ((>)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval>? intl intr) (true) (false)))))
	 ((=)
	  (duplicate::rtl_ins/bbv i
	     (fun (if (interval=? intl intr) (true) (false)))))
	 (else
	  #f)))

   (define (test-ctxs-ref reg intl intr op ctx)
      (if (or (not (interval? intl)) (not (interval? intr)))
	  (values ctx ctx)
	  (case op
	     ((<)
	      (let ((intrt (interval-lt intl intr))
		    (intro (interval-gte intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((<=)
	      (let ((intrt (interval-lte intl intr))
		    (intro (interval-gt intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((>)
	      (let ((intrt (interval-gt intl intr))
		    (intro (interval-lte intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((>=)
	      (let ((intrt (interval-gte intl intr))
		    (intro (interval-lt intl intr)))
		 (values (extend-ctx ctx reg *int* #t intrt)
		    (extend-ctx ctx reg *int* #t intro))))
	     ((== ===)
	      (let ((ieq (interval-eq intl intr)))
		 (values (if (interval? ieq)
			     (extend-ctx ctx reg *int* #t ieq)
			     ctx)
		    ctx)))
	     ((!= !==)
	      (let ((ieq (interval-eq intl intr)))
		 (values
		    ctx
		    (if (interval? ieq)
			(extend-ctx ctx reg *int* #t ieq)
			ctx))))
	     (else
	      (values ctx ctx)))))
   
   (define (specialize/op op lhs intl rhs intr sctx)
      (cond
	 ((and (reg? lhs) (reg? rhs))
	  (multiple-value-bind (lctxt lctxo)
	     (test-ctxs-ref (reg lhs) intl intr op sctx)
	     (multiple-value-bind (rctxt rctxo)
		(test-ctxs-ref (reg rhs) intr intl (inv-op op) '())
		(values (append rctxt lctxt) (append rctxo lctxo)))))
	 ((reg? lhs)
	  (test-ctxs-ref (reg lhs) intl intr op sctx))
	 ((reg? rhs)
	  (test-ctxs-ref (reg rhs) intr intl (inv-op op) sctx))
	 (else
	  (values #f #f))))
   
   (with-access::rtl_ins i (dest fun args)
      (with-access::rtl_call fun (var)
	 (let* ((lhs (car args))
		(rhs (cadr args))
		(intl (interval-value lhs ctx))
		(intr (interval-value rhs ctx))
		(sctx (extend-ctx ctx dest *bool* #t))
		(op (intcmp-op i)))
	    (cond
	       ((not (and (interval? intl) (interval? intr)))
		(multiple-value-bind (ctxt ctxo)
		   (specialize/op op lhs (or intl *fixnum-intv*)
		      rhs (or intr *fixnum-intv*) sctx)
		   (values i sctx ctxt ctxo)))
	       ((resolve/op i op intl intr)
		=>
		(lambda (ni) (values ni sctx #f #f)))
	       (else
		(multiple-value-bind (ctxt ctxo)
		   (specialize/op op lhs intl rhs intr sctx)
		   (values i sctx ctxt ctxo))))))))

;*---------------------------------------------------------------------*/
;*    interval<? ...                                                   */
;*---------------------------------------------------------------------*/
(define (interval<? left right)
   (<fx (interval-max left) (interval-min right)))

;*---------------------------------------------------------------------*/
;*    interval<=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval<=? left right)
   (<=fx (interval-max left) (interval-min right)))

;*---------------------------------------------------------------------*/
;*    interval>? ...                                                   */
;*---------------------------------------------------------------------*/
(define (interval>? left right)
   (>fx (interval-min left) (interval-max right)))

;*---------------------------------------------------------------------*/
;*    interval>=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval>=? left right)
   (>=fx (interval-min left) (interval-max right)))

;*---------------------------------------------------------------------*/
;*    interval=? ...                                                   */
;*---------------------------------------------------------------------*/
(define (interval=? left right)
   (and (=fx (interval-min left) (interval-min right))
	(=fx (interval-max left) (interval-max right))))

;*---------------------------------------------------------------------*/
;*    interval-lts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-lts left::struct right::struct shift::int)
   (let ((ra (- (interval-max right) shift)))
      (if (< ra (interval-max left))
	  (if (>= ra (interval-min left))
	      (interval (min (interval-min left) ra) ra)
	      (interval ra ra))
	  left)))

(define (interval-lt left right)
   (interval-lts left right 1))

(define (interval-lte left right)
   (interval-lts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-gts ...                                                 */
;*---------------------------------------------------------------------*/
(define (interval-gts left::struct right::struct shift::int)
   (let ((ri (+ (interval-min right) shift)))
      (if (> ri (interval-min left))
	  (if (<= ri (interval-max left))
	      (interval ri (max (interval-max left) ri))
	      (interval ri ri))
	  left)))

(define (interval-gt left right)
   (interval-gts left right 1))

(define (interval-gte left right)
   (interval-gts left right 0))

;*---------------------------------------------------------------------*/
;*    interval-eq ...                                                  */
;*---------------------------------------------------------------------*/
(define (interval-eq left::struct right::struct)
   (let* ((ri (interval-min right))
	  (ra (interval-max right))
	  (li (interval-min left))
	  (la (interval-max left))
	  (oi (max ri li))
	  (oa (min ra la)))
      (if (<= oi oa)
	  (interval oi oa)
	  #f)))
   
;*---------------------------------------------------------------------*/
;*    rtl_ins-vector-bound-check? ...                                  */
;*---------------------------------------------------------------------*/
(define (rtl_ins-vector-bound-check? i::rtl_ins)
   (when (or (rtl_ins-ifeq? i) (rtl_ins-ifne? i))
      (with-access::rtl_ins i (args fun)
	 (tprint "YAP" (shape i) " " (typeof fun))
	 (when (isa? fun rtl_call)
	    (with-access::rtl_call fun (var)
	       (when (eq? var *vector-bound-check*)
		  (tprint "YIP")
		  (let ((args (rtl_ins-args* i)))
		     (and (rtl_reg? (car args)) (rtl_reg? (cadr args))))))))))

;*---------------------------------------------------------------------*/
;*    rtl_ins-specialize-vector-bound-check ...                        */
;*---------------------------------------------------------------------*/
(define (rtl_ins-specialize-vector-bound-check i::rtl_ins ctx)
   (tprint "VECTOR-BOUND-CHECK" (shape i))
   (tprint "                  " (ctx->string ctx))
   (values i (extend-ctx ctx (rtl_ins-dest i) *obj* #t)))
   
   
