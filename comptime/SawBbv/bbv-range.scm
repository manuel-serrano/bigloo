;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-range.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jul  8 09:57:32 2022                          */
;*    Last change :  Thu Sep 29 14:42:46 2022 (serrano)                */
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
	      (lo read-only)
	      (up read-only))

	   (inline bbv-max-fixnum::long)
	   (inline bbv-min-fixnum::long)
	   (inline bbv-range?::bool ::obj)
	   (bbv-range-fixnum?::bool ::bbv-range)
	   (fixnum-range::bbv-range)
	   (fixnum->range::bbv-range ::long)
	   (vlen-range::bbv-range)
	   (vlen->range::bbv-range ::rtl_reg)
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

(define (++ i)
   (if (eq? i bbv-max-fixnum)
       bbv-max-fixnum
       (+ i 1)))

(define (-- i)
   (if (eq? i bbv-min-fixnum)
       bbv-min-fixnum
       (- i 1)))

;*---------------------------------------------------------------------*/
;*    bbv-range? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range? o)
   (isa? o bbv-range))

;*---------------------------------------------------------------------*/
;*    bbv-range-fixnum? ...                                            */
;*---------------------------------------------------------------------*/
(define (bbv-range-fixnum? o)
   (with-access::bbv-range o (lo up)
      (and (fixnum? lo) (fixnum? up))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lo ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-lo o)
   (with-access::bbv-range o (lo)
      lo))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-up ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-up o)
   (with-access::bbv-range o (up)
      up))
   
;*---------------------------------------------------------------------*/
;*    shape ::bbv-range ...                                            */
;*---------------------------------------------------------------------*/
(define-method (shape e::bbv-range)

;*    (define (vector-str n)                                           */
;*       (format "#(~a ~a)"                                            */
;* 	 (shape (vector-ref n 0))                                      */
;* 	 (shape (vector-ref n 1))))                                    */
   
   (define (str n)
      (cond
	 ((eq? n (bbv-max-fixnum)) "upfx")
	 ((eq? n (-fx (bbv-max-fixnum) 1)) "upfx-1")
	 ((eq? n (-fx (bbv-max-fixnum) 2)) "upfx-2")
	 ((eq? n (bbv-min-fixnum)) "lofx")
	 ((eq? n (+fx (bbv-min-fixnum) 1)) "lofx+1")
	 ((eq? n (+fx (bbv-min-fixnum) 2)) "lofx+2")
	 ((fixnum? n) n)
;* 	 ((vector? n) (vector-str n))                                  */
	 ((not (flonum? n)) (shape n))
	 ((infinitefl? n) (if (<fl n 0.0) "-inf" "+inf"))
	 (else n)))
   
   (with-access::bbv-range e (lo up)
      (format "[~a..~a]" (str lo) (str up))))

;*---------------------------------------------------------------------*/
;*    *fixnum-range* ...                                               */
;*---------------------------------------------------------------------*/
(define *fixnum-range*
   (instantiate::bbv-range
      (lo (bbv-min-fixnum))
      (up (bbv-max-fixnum))))

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
      (lo n)
      (up n)))

;*---------------------------------------------------------------------*/
;*    *vlen-range* ...                                                 */
;*---------------------------------------------------------------------*/
(define *vlen-range*
   (instantiate::bbv-range
      (lo 0)
      (up (bbv-max-fixnum))))

;*---------------------------------------------------------------------*/
;*    vlen-range ...                                                   */
;*---------------------------------------------------------------------*/
(define (vlen-range)
   *vlen-range*)

;*---------------------------------------------------------------------*/
;*    vlen->range ...                                                  */
;*---------------------------------------------------------------------*/
(define (vlen->range v)
   (vlen-range))

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
	(=fx (bbv-range-up rng) (bbv-range-lo rng))
	(> (bbv-range-up rng) (+fx (bbv-min-fixnum) 1))
	(< (bbv-range-up rng) (-fx (bbv-max-fixnum) 1))))

;*---------------------------------------------------------------------*/
;*    binoprv ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (rv op u v)
   (let ((x (gensym 'x))
	 (y (gensym 'y)))
      `(let ((,x ,u)
	     (,y ,v))
	  (cond
	     ((and (fixnum? ,x) (fixnum? ,y))
	      (,op ,x ,y))
	     (else #f)))))

(define (<rv x y) (rv <fx x y))
(define (<=rv x y) (rv <=fx x y))
(define (>rv x y) (rv >fx x y))
(define (>=rv x y) (rv >=fx x y))

;*---------------------------------------------------------------------*/
;*    bbv-range<? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range<? left right)
   (cond
      ((<rv (bbv-range-up left) (bbv-range-lo right)) 'true)
      ((>=rv (bbv-range-lo left) (bbv-range-up right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range<=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range<=? left right)
   (cond
      ((<=rv (bbv-range-up left) (bbv-range-lo right)) 'true)
      ((>rv (bbv-range-lo left) (bbv-range-up right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range>? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range>? left right)
   (cond
      ((>rv (bbv-range-lo left) (bbv-range-up right)) 'true)
      ((<=rv (bbv-range-up left) (bbv-range-lo right)) 'false)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range>=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range>=? left right)
   (cond
      ((>=rv (bbv-range-lo left) (bbv-range-up right)) 'true)
      ((<rv (bbv-range-up left) (bbv-range-lo right)) 'false)
      (else #f)))
      

;*---------------------------------------------------------------------*/
;*    bbv-range=? ...                                                  */
;*---------------------------------------------------------------------*/
(define (bbv-range=? left right)
   (cond
      ((and (rv =fx (bbv-range-lo left) (bbv-range-lo right))
	    (rv =fx (bbv-range-up left) (bbv-range-up right)))
       'true)
      (else
       'false)))

;*---------------------------------------------------------------------*/
;*    bbv-range-lt ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-lt left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      ;; [li..la] < [ri..ra] => [li..max(li,min(X, Y-1)]
      (instantiate::bbv-range
	 (lo li)
	 (up (max li (min la (-- ra)))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lte ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-lte left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      ;; [30..X] <= [Y..?] => [li..max(li,min(X, Y))]
      (instantiate::bbv-range
	 (lo li)
	 (up (max li (min la ra))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-gt ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-gt left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      ;; [li..la] > [ri..ra] => [min(la,max(li,ri++))..la]
      (instantiate::bbv-range
	 (lo (min la (max li (++ ri))))
	 (up la))))

;*---------------------------------------------------------------------*/
;*    bbv-range-gte ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-gte left right)
   (let ((li (bbv-range-lo left))
	 (la (bbv-range-up left))
	 (ri (bbv-range-lo right))
	 (ra (bbv-range-up right)))
      ;; [li..la] > [ri..ra] => [min(la,max(li,ri))..la]
      (instantiate::bbv-range
	 (lo (min la (max li ri)))
	 (up la))))

;*---------------------------------------------------------------------*/
;*    bbv-range-eq ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-eq left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-lo right))
	  (ra (bbv-range-up right))
	  (li (bbv-range-lo left))
	  (la (bbv-range-up left))
	  (oi (max ri li))
	  (oa (min ra la)))
      (when (<= oi oa)
	 (instantiate::bbv-range
	    (lo oi)
	    (up oa)))))

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
   (let ((ri (bbv-range-lo right))
	 (ra (bbv-range-up right))
	 (li (bbv-range-lo left))
	 (la (bbv-range-up left)))
      (instantiate::bbv-range
	 (lo (+rng li ri))
	 (up (+rng la ra)))))

;*---------------------------------------------------------------------*/
;*    bbv-range-sub ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-sub left::bbv-range right::bbv-range)
   (let ((ri (bbv-range-lo right))
	 (ra (bbv-range-up right))
	 (li (bbv-range-lo left))
	 (la (bbv-range-up left)))
      (instantiate::bbv-range
	 (lo (-rng li ra))
	 (up (-rng la ri)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-mul ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-mul left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-lo right))
	  (ra (bbv-range-up right))
	  (li (bbv-range-lo left))
	  (la (bbv-range-up left))
	  (v0 (*rng li ri))
	  (v1 (*rng li ra))
	  (v2 (*rng la ra))
	  (v3 (*rng ra ri))
	  (mi0 (min v0 v1))
	  (mi1 (min v2 v3))
	  (ma0 (max v0 v1))
	  (ma1 (max v2 v3)))
      (instantiate::bbv-range
	 (lo (min mi0 mi1 li ri))
	 (up (max ma0 ma1 la ra)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-intersection ...                                       */
;*---------------------------------------------------------------------*/
(define (bbv-range-intersection::bbv-range x::bbv-range y::bbv-range)
   (with-access::bbv-range x ((lox lo) (upx up))
      (with-access::bbv-range y ((loy lo) (upy up))
	 (instantiate::bbv-range
	    (lo (max lox loy))
	    (up (min upx upy))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-merge ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-merge x::bbv-range y::bbv-range)
   (with-access::bbv-range x ((lox lo) (upx up))
      (with-access::bbv-range y ((loy lo) (upy up))
	 (instantiate::bbv-range
	    (lo (min lox loy))
	    (up (max upx upy))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-widen ...                                              */
;*---------------------------------------------------------------------*/
(define (bbv-range-widen x::bbv-range y::bbv-range)
   
   (define (widen-lo lox loy)
      (let ((i (min lox loy)))
	 (cond
	    ((>fx i 16) (/fx i 2))
	    ((>=fx i -2) (-fx i 1))
	    ((>=fx i -16) (*fx i 2))
	    ((=fx i (+fx (bbv-min-fixnum) 1)) (bbv-min-fixnum))
	    ((<=fx i -256) (+fx (bbv-min-fixnum) 1))
	    (else (bbv-min-fixnum)))))

   (define (widen-up upx upy)
      (let ((i (max upx upy)))
	 (cond
	    ((<=fx i 2) (+fx i 1))
	    ((<=fx i 256) (*fx i 2))
	    ((=fx i (-fx (bbv-max-fixnum) 1)) (bbv-max-fixnum))
	    ((>=fx i 256) (-fx (bbv-max-fixnum) 1))
	    (else (bbv-max-fixnum)))))
   
   (with-access::bbv-range x ((lox lo) (upx up))
      (with-access::bbv-range y ((loy lo) (upy up))
	 (instantiate::bbv-range
	    (lo (if (=fx lox loy) lox (widen-lo lox loy)))
	    (up (if (=fx upx upy) upx (widen-up upx upy)))))))
