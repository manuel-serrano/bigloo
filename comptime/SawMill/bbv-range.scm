;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-range.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jul  8 09:57:32 2022                          */
;*    Last change :  Fri Jul  8 11:43:15 2022 (serrano)                */
;*    Copyright   :  2022 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    BBV range abstraction                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module saw_bbv-range

   (import  engine_param
	    ast_var
	    ast_node
	    type_type
	    type_cache
	    tools_shape
	    saw_defs
	    saw_regset
	    saw_bbv-cache
	    saw_bbv-types)
   
   (export (class bbv-range
	      (min read-only)
	      (max read-only))

	   (inline bbv-range?::bool ::obj)
	   (fixnum-range::bbv-range)
	   (fixnum->range::bbv-range ::long)
	   (rtl-range::obj ::obj ::pair-nil)
	   (bbv-range<?::bool ::bbv-range ::bbv-range)
	   (bbv-range<=?::bool ::bbv-range ::bbv-range)
	   (bbv-range>?::bool ::bbv-range ::bbv-range)
	   (bbv-range>=?::bool ::bbv-range ::bbv-range)
	   (bbv-range=?::bool ::bbv-range ::bbv-range)
	   (bbv-range-lt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-lte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-eq::bool ::bbv-range ::bbv-range)))

;*---------------------------------------------------------------------*/
;*    bbv-range? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range? o)
   (isa? o bbv-range))

;*---------------------------------------------------------------------*/
;*    bbv-range-min ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-min o)
   (with-access::bbv-range o (min)
      min))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-max ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-max o)
   (with-access::bbv-range o (max)
      max))
   
;*---------------------------------------------------------------------*/
;*    shape ::bbv-range ...                                            */
;*---------------------------------------------------------------------*/
(define-method (shape e::bbv-range)
   (with-access::bbv-range e (min max)
      (format "\"[~a..~a]\"" min max)))

;*---------------------------------------------------------------------*/
;*    *fixnum-range* ...                                               */
;*---------------------------------------------------------------------*/
(define *fixnum-range*
   (instantiate::bbv-range
      (min *-inf.0*)
      (max *+inf.0*)))

;*---------------------------------------------------------------------*/
;*    fixnum-range ...                                                 */
;*---------------------------------------------------------------------*/
(define (fixnum-range)
   *fixnum-range*)

;*---------------------------------------------------------------------*/
;*    fixnum->range ...                                                */
;*---------------------------------------------------------------------*/
(define (fixnum->range n)
   (instantiate::bbv-range
      (min n)
      (max n)))

;*---------------------------------------------------------------------*/
;*    rtl-range ...                                                    */
;*---------------------------------------------------------------------*/
(define (rtl-range i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (ctx-get ctx i)))
	  (with-access::bbv-ctxentry e (typ value)
	     (when (and e (eq? typ *int*))
		(when (bbv-range? value)
		   value)))))
      ((rtl_ins-mov? i)
       (rtl-range (car (rtl_ins-args i)) ctx))
      ((rtl_ins-call? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_call fun (var)
	     (cond
		((eq? var *int->long*)
		 (rtl-range (car (rtl_ins-args i)) ctx))
		(else
		 #f)))))
      ((isa? i rtl_loadi)
       (tprint "loadi...")
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_loadi fun (constant)
	     (with-access::atom constant (value)
		(tprint "VAL=" (typeof value))
		(when (fixnum? value)
		   (fixnum->range value))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range<? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range<? left right)
   (<fx (bbv-range-max left) (bbv-range-min right)))

;*---------------------------------------------------------------------*/
;*    bbv-range<=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range<=? left right)
   (<=fx (bbv-range-max left) (bbv-range-min right)))

;*---------------------------------------------------------------------*/
;*    bbv-range>? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range>? left right)
   (>fx (bbv-range-min left) (bbv-range-max right)))

;*---------------------------------------------------------------------*/
;*    bbv-range>=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range>=? left right)
   (>=fx (bbv-range-min left) (bbv-range-max right)))

;*---------------------------------------------------------------------*/
;*    bbv-range=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range=? left right)
   (and (=fx (bbv-range-min left) (bbv-range-min right))
	(=fx (bbv-range-max left) (bbv-range-max right))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lts ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-lts left::bbv-range right::bbv-range shift::int)
   (let ((ra (- (bbv-range-max right) shift)))
      (if (< ra (bbv-range-max left))
	  (if (>= ra (bbv-range-min left))
	      (instantiate::bbv-range
		 (min (min (bbv-range-min left) ra))
		 (max ra))
	      (instantiate::bbv-range
		 (min ra)
		 (max ra)))
	  left)))

(define (bbv-range-lt left right)
   (bbv-range-lts left right 1))

(define (bbv-range-lte left right)
   (bbv-range-lts left right 0))

;*---------------------------------------------------------------------*/
;*    bbv-range-gts ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-gts left::bbv-range right::bbv-range shift::int)
   (let ((ri (+ (bbv-range-min right) shift)))
      (if (> ri (bbv-range-min left))
	  (if (<= ri (bbv-range-max left))
	      (instantiate::bbv-range
		 (min ri)
		 (max (max (bbv-range-max left) ri)))
	      (instantiate::bbv-range
		 (min ri)
		 (max ri)))
	  left)))

(define (bbv-range-gt left right)
   (bbv-range-gts left right 1))

(define (bbv-range-gte left right)
   (bbv-range-gts left right 0))

;*---------------------------------------------------------------------*/
;*    bbv-range-eq ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-eq left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-min right))
	  (ra (bbv-range-max right))
	  (li (bbv-range-min left))
	  (la (bbv-range-max left))
	  (oi (max ri li))
	  (oa (min ra la)))
      (when (<= oi oa)
	 (instantiate::bbv-range
	    (min oi)
	    (max oa)))))

   
