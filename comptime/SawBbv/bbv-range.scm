;*=====================================================================*/
;*    .../project/bigloo/bigloo/comptime/SawMill/bbv-range.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jul  8 09:57:32 2022                          */
;*    Last change :  Mon Aug 29 10:14:42 2022 (serrano)                */
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

	   (inline bbv-max-fixnum::long)
	   (inline bbv-min-fixnum::long)
	   (inline bbv-range?::bool ::obj)
	   (bbv-range-fixnum?::bool ::bbv-range)
	   (fixnum-range::bbv-range)
	   (fixnum->range::bbv-range ::long)
	   (rtl-range::obj ::obj ::bbv-ctx)
	   (range-type?::bool ::obj)
	   (bbv-singleton?::bool ::obj)
	   (bbv-range<? ::bbv-range ::bbv-range)
	   (bbv-range<=? ::bbv-range ::bbv-range)
	   (bbv-range>? ::bbv-range ::bbv-range)
	   (bbv-range>=? ::bbv-range ::bbv-range)
	   (bbv-range=? ::bbv-range ::bbv-range)
	   (bbv-range-lt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-lte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gt::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-gte::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-eq::bool ::bbv-range ::bbv-range)
	   (bbv-range-add::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-sub::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-mul::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-intersection::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-merge::bbv-range ::bbv-range ::bbv-range)
	   (bbv-range-widen::bbv-range ::bbv-range ::bbv-range)))

;*---------------------------------------------------------------------*/
;*    integer boundaries ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (bbv-max-fixnum)
   (bit-lsh 1 (-fx (bigloo-config 'int-size) 2)))
(define-inline (bbv-min-fixnum)
   (-fx (negfx (bbv-max-fixnum)) 1))

;*---------------------------------------------------------------------*/
;*    bbv-range? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range? o)
   (isa? o bbv-range))

;*---------------------------------------------------------------------*/
;*    bbv-range-fixnum? ...                                            */
;*---------------------------------------------------------------------*/
(define (bbv-range-fixnum? o)
   (with-access::bbv-range o (min max)
      (and (fixnum? min) (fixnum? max))))

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
   
   (define (shape n)
      (cond
	 ((eq? n (bbv-max-fixnum)) "maxfx")
	 ((eq? n (-fx (bbv-max-fixnum) 1)) "maxfx-1")
	 ((eq? n (-fx (bbv-max-fixnum) 2)) "maxfx-2")
	 ((eq? n (bbv-min-fixnum)) "minfx")
	 ((eq? n (+fx (bbv-min-fixnum) 1)) "minfx+1")
	 ((eq? n (+fx (bbv-min-fixnum) 2)) "minfx+2")
	 ((fixnum? n) n)
	 ((not (flonum? n)) n)
	 ((infinitefl? n) (if (<fl n 0.0) "-inf" "+inf"))
	 (else n)))
   
   (with-access::bbv-range e (min max)
      (format "[~a..~a]" (shape min) (shape max))))

;*---------------------------------------------------------------------*/
;*    *fixnum-range* ...                                               */
;*---------------------------------------------------------------------*/
(define *fixnum-range*
   (instantiate::bbv-range
      (min (bbv-min-fixnum))
      (max (bbv-max-fixnum))))

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
;*    range-type? ...                                                  */
;*---------------------------------------------------------------------*/
(define (range-type? ty)
   (or (eq? ty *bint*) (eq? ty *long*)))

;*---------------------------------------------------------------------*/
;*    rtl-range ...                                                    */
;*---------------------------------------------------------------------*/
(define (rtl-range i ctx)
   (cond
      ((isa? i rtl_reg)
       (let ((e (bbv-ctx-get ctx i)))
	  (when e
	     (with-access::bbv-ctxentry e (types value polarity)
		(when (and polarity (any range-type? types))
		   (when (bbv-range? value)
		      value))))))
      ((rtl_ins-mov? i)
       (rtl-range (car (rtl_ins-args i)) ctx))
      ((rtl_ins-call? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_call fun (var)
	     (cond
		((eq? var *int->long*)
		 (rtl-range (car (rtl_ins-args i)) ctx))
		((eq? var *bint->long*)
		 (rtl-range (car (rtl_ins-args i)) ctx))
		(else
		 #f)))))
      ((rtl_ins-loadi? i)
       (with-access::rtl_ins i (fun)
	  (with-access::rtl_loadi fun (constant)
	     (with-access::atom constant (value)
		(when (fixnum? value)
		   (fixnum->range value))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bbv-singleton? ...                                               */
;*---------------------------------------------------------------------*/
(define (bbv-singleton? rng)
   (and (isa? rng bbv-range)
	(=fx (bbv-range-max rng) (bbv-range-min rng))
	(> (bbv-range-max rng) (+fx (bbv-min-fixnum) 1))
	(< (bbv-range-max rng) (-fx (bbv-max-fixnum) 1))))

;*---------------------------------------------------------------------*/
;*    bbv-range<? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range<? left right)
   (cond
      ((<fx (bbv-range-max left) (bbv-range-min right)) 'true)
      ((>=fx (bbv-range-min left) (bbv-range-max right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range<=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range<=? left right)
   (cond
      ((<=fx (bbv-range-max left) (bbv-range-min right)) 'true)
      ((>fx (bbv-range-min left) (bbv-range-max right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range>? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range>? left right)
   (cond
      ((>fx (bbv-range-min left) (bbv-range-max right)) 'true)
      ((<=fx (bbv-range-max left) (bbv-range-min right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range>=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range>=? left right)
   (cond
      ((>=fx (bbv-range-min left) (bbv-range-max right)) 'true)
      ((<fx (bbv-range-max left) (bbv-range-min right)) 'false)
      (else #f)))
      

;*---------------------------------------------------------------------*/
;*    bbv-range=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range=? left right)
   (cond
      ((and (=fx (bbv-range-min left) (bbv-range-min right))
	    (=fx (bbv-range-max left) (bbv-range-max right)))
       'true)
      (else
       'false)))

;*---------------------------------------------------------------------*/
;*    bbv-range-lts ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-lts left::bbv-range right::bbv-range shift::int)
   (let ((ra (- (bbv-range-max right) shift)))
      (if (< ra (bbv-range-max left))
	  (instantiate::bbv-range
	     (min (min ra (bbv-range-min left)))
	     (max ra))
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
	  (instantiate::bbv-range
	     (min ri)
	     (max (max ri (bbv-range-max left))))
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

;*---------------------------------------------------------------------*/
;*    normalize ...                                                    */
;*---------------------------------------------------------------------*/
(define (normalize n)
   (cond
      ((< n (bbv-min-fixnum)) -inf.0)
      ((> n (bbv-max-fixnum)) +inf.0)
      (else n)))

;*---------------------------------------------------------------------*/
;*    +rng ...                                                         */
;*---------------------------------------------------------------------*/
(define (+rng x y)
   (cond
      ((=fx x (bbv-max-fixnum)) (bbv-max-fixnum))
      ((=fx y (bbv-max-fixnum)) (bbv-max-fixnum))
      ((=fx x (bbv-min-fixnum)) (+fx (bbv-min-fixnum) 1))
      ((=fx x (+fx (bbv-min-fixnum) 1)) x)
      ((=fx y (+fx (bbv-min-fixnum) 1)) y)
      ((and (=fx x (-fx (bbv-max-fixnum) 1)) (>fx y 0)) (bbv-max-fixnum))
      ((and (=fx y (-fx (bbv-max-fixnum) 1)) (>fx x 0)) (bbv-max-fixnum))
      (else (+fx x y))))
      
;*---------------------------------------------------------------------*/
;*    -rng ...                                                         */
;*---------------------------------------------------------------------*/
(define (-rng x y)
   (cond
      ((=fx x (bbv-min-fixnum)) (bbv-min-fixnum))
      ((=fx y (bbv-min-fixnum)) (bbv-min-fixnum))
      ((=fx x (-fx (bbv-max-fixnum) 1)) x)
      ((=fx y (-fx (bbv-max-fixnum) 1)) y)
      ((and (=fx x (-fx (bbv-min-fixnum) 1)) (<fx y 0)) (bbv-min-fixnum))
      ((and (=fx y (-fx (bbv-min-fixnum) 1)) (<fx x 0)) (bbv-min-fixnum))
      (else (-fx x y))))
      
;*---------------------------------------------------------------------*/
;*    *rng ...                                                         */
;*---------------------------------------------------------------------*/
(define (*rng x y)
   (cond
      (else (*fx x y))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-add ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-add left::bbv-range right::bbv-range)
   (let ((ri (bbv-range-min right))
	 (ra (bbv-range-max right))
	 (li (bbv-range-min left))
	 (la (bbv-range-max left)))
      (instantiate::bbv-range
	 (min (+rng li ri))
	 (max (+rng la ra)))))

;*---------------------------------------------------------------------*/
;*    bbv-range-sub ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-sub left::bbv-range right::bbv-range)
   (let ((ri (bbv-range-min right))
	 (ra (bbv-range-max right))
	 (li (bbv-range-min left))
	 (la (bbv-range-max left)))
      (instantiate::bbv-range
	 (min (-rng li ra))
	 (max (-rng la ri)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-mul ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-mul left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-min right))
	  (ra (bbv-range-max right))
	  (li (bbv-range-min left))
	  (la (bbv-range-max left))
	  (v0 (*rng li ri))
	  (v1 (*rng li ra))
	  (v2 (*rng la ra))
	  (v3 (*rng ra ri))
	  (mi0 (min v0 v1))
	  (mi1 (min v2 v3))
	  (ma0 (max v0 v1))
	  (ma1 (max v2 v3)))
      (instantiate::bbv-range
	 (min (min mi0 mi1 li ri))
	 (max (max ma0 ma1 la ra)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-intersection ...                                       */
;*---------------------------------------------------------------------*/
(define (bbv-range-intersection::bbv-range x::bbv-range y::bbv-range)
   (with-access::bbv-range x ((minx min) (maxx max))
      (with-access::bbv-range y ((miny min) (maxy max))
	 (instantiate::bbv-range
	    (min (max minx miny))
	    (max (min maxx maxy))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-merge ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-merge x::bbv-range y::bbv-range)
   (with-access::bbv-range x ((minx min) (maxx max))
      (with-access::bbv-range y ((miny min) (maxy max))
	 (instantiate::bbv-range
	    (min (min minx miny))
	    (max (max maxx maxy))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-widen ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-widen x::bbv-range y::bbv-range)
   
   (define (widen-min minx miny)
      (let ((i (min minx miny)))
	 (cond
	    ((>fx i 16) (/fx i 2))
	    ((>=fx i -2) (-fx i 1))
	    ((>=fx i -16) (*fx i 2))
	    ((=fx i (+fx (bbv-min-fixnum) 1)) (bbv-min-fixnum))
	    ((<=fx i -256) (+fx (bbv-min-fixnum) 1))
	    (else (bbv-min-fixnum)))))

   (define (widen-max maxx maxy)
      (let ((i (max maxx maxy)))
	 (cond
	    ((<=fx i 2) (+fx i 1))
	    ((<=fx i 256) (*fx i 2))
	    ((=fx i (-fx (bbv-max-fixnum) 1)) (bbv-max-fixnum))
	    ((>=fx i 256) (-fx (bbv-max-fixnum) 1))
	    (else (bbv-max-fixnum)))))
   
   (with-access::bbv-range x ((minx min) (maxx max))
      (with-access::bbv-range y ((miny min) (maxy max))
	 (instantiate::bbv-range
	    (min (if (=fx minx miny) minx (widen-min minx miny)))
	    (max (if (=fx maxx maxy) maxx (widen-max maxx maxy)))))))
