;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/SawBbv/bbv-range.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Jul  8 09:57:32 2022                          */
;*    Last change :  Thu Jul 20 07:53:52 2023 (serrano)                */
;*    Copyright   :  2022-23 manuel serrano                            */
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
	    saw_bbv-config
	    saw_bbv-cache
	    saw_bbv-types)
   
   (export (class bbv-range
	      (lo read-only)
	      (up read-only))

	   (class bbv-vlen
	      (vec read-only)
	      (offset::int read-only))

	   (inline bbv-vlen?::bool ::obj)
	   (inline bbv-vlen-vec::obj ::bbv-vlen)
	   (inline bbv-vlen-offset::int ::bbv-vlen)
	   (inline bbv-max-fixnum::long)
	   (inline bbv-min-fixnum::long)
	   (inline bbv-range?::bool ::obj)
	   (=rv::bool ::obj ::obj)
	   (<rv::bool ::obj ::obj)
	   (<=rv::bool ::obj ::obj)
	   (>rv::bool ::obj ::obj)
	   (>=rv::bool ::obj ::obj)
	   (minrv::obj ::obj ::obj)
	   (maxrv::obj ::obj ::obj)
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
	   (bbv-range-intersection::bbv-range ::bbv-range ::bbv-range)))

;*---------------------------------------------------------------------*/
;*    object-print ::bbv-vlen ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-print o::bbv-vlen p proc)
   (let ((str (format "#(~a ~a)"
		 (shape (bbv-vlen-vec o))
		 (bbv-vlen-offset o))))
      (display str p)))

;*---------------------------------------------------------------------*/
;*    bbv-vlen? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (bbv-vlen? o)
   (isa? o bbv-vlen))

(define-inline (bbv-vlen-vec o)
   (with-access::bbv-vlen o (vec) vec))

(define-inline (bbv-vlen-offset o)
   (with-access::bbv-vlen o (offset) offset))

;*---------------------------------------------------------------------*/
;*    binoprv ...                                                      */
;*---------------------------------------------------------------------*/
(define (=rv x y)
   (define (=vlen x y)
      (and (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	   (=fx (bbv-vlen-offset x) (bbv-vlen-offset y))))
   (cond
      ((and (fixnum? x) (fixnum? y)) (=fx x y))
      ((and (bbv-vlen? x) (bbv-vlen? y)) (=vlen x y))
      (else #f)))

(define (>rv x y)
   (define (>vlen x y)
      (when (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	 (>fx (bbv-vlen-offset x) (bbv-vlen-offset y))))
   (define (>vlenfx x y)
      (>fx (bbv-vlen-offset x) y))
   (cond
      ((and (fixnum? x) (fixnum? y)) (>fx x y))
      ((and (bbv-vlen? x) (bbv-vlen? y)) (>vlen x y))
      ((and (bbv-vlen? x) (fixnum? y)) (>vlenfx x y))
      ((and (fixnum? x) (bbv-vlen? y)) #f)
      (else #f)))
	 
(define (>=rv x y)
   (define (>=vlen x y)
      (when (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	 (>=fx (bbv-vlen-offset x) (bbv-vlen-offset y))))
   (define (>=vlenfx x y)
      (>=fx (bbv-vlen-offset x) y))
   (cond
      ((and (fixnum? x) (fixnum? y)) (>=fx x y))
      ((and (bbv-vlen? x) (bbv-vlen? y)) (>=vlen x y))
      ((and (bbv-vlen? x) (fixnum? y)) (>=vlenfx x y))
      ((and (fixnum? x) (bbv-vlen? y)) #f)
      (else #f)))

(define (<rv x y)
   (>rv y x))

(define (<=rv x y)
   (>=rv y x))

(define (minrv x y)
   (cond
      ((and (number? x) (number? y))
       (min x y))
      ((and (bbv-vlen? x) (bbv-vlen? y))
       (if (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	   (duplicate::bbv-vlen x
	      (offset (min (bbv-vlen-offset x) (bbv-vlen-offset y))))
	   (min (bbv-vlen-offset x) (bbv-vlen-offset y))))
      ((and (bbv-vlen? x) (number? y))
       (cond
	  ((< (bbv-vlen-offset x) y) x)
;* 	  ((= y (bbv-max-fixnum)) x)                                   */
	  (else (min (bbv-vlen-offset x) y))))
      ((and (number? x) (bbv-vlen? y))
       (minrv y x))
      (else
       -inf.0)))

(define (maxrv x y)
   (cond
      ((and (number? x) (number? y))
       (max x y))
      ((and (bbv-vlen? x) (bbv-vlen? y))
       (if (eq? (bbv-vlen-vec x) (bbv-vlen-vec y))
	   (duplicate::bbv-vlen x
	      (offset (max (bbv-vlen-offset x) (bbv-vlen-offset y))))
	   (max (- (bbv-max-fixnum) (bbv-vlen-offset x))
	      (- (bbv-max-fixnum) (bbv-vlen-offset y)))))
      ((and (bbv-vlen? x) (number? y))
       (cond
	  ((> (- (bbv-max-fixnum) (bbv-vlen-offset x)) y) x)
;* 	  ((> y 0) (if (> (- (bbv-max-fixnum) (bbv-vlen-offset x)) y) x y)) */
;* 	  ((> (bbv-vlen-offset x) y) x)                                */
	  (else (bbv-max-fixnum))))
      ((and (number? x) (bbv-vlen? y))
       (maxrv y x))
      (else
       +inf.0)))

;*---------------------------------------------------------------------*/
;*    integer boundaries ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (bbv-max-fixnum)
   (bit-lsh 1 (-fx (bigloo-config 'int-size) 2)))
(define-inline (bbv-min-fixnum)
   (-fx (negfx (bbv-max-fixnum)) 1))

(define (++ i)
   (cond
      ((eq? i bbv-max-fixnum)
       bbv-max-fixnum)
      ((bbv-vlen? i)
       (duplicate::bbv-vlen i
	  (offset (+fx (bbv-vlen-offset i) 1))))
      (else
       (+ i 1))))

(define (-- i)
   (cond
      ((eq? i bbv-min-fixnum)
       bbv-min-fixnum)
      ((bbv-vlen? i)
       (duplicate::bbv-vlen i
	  (offset (-fx (bbv-vlen-offset i) 1))))
      (else
       (- i 1))))

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
      (and (or (bbv-vlen? lo) (and (fixnum? lo) (>=fx lo (bbv-min-fixnum))))
	   (or (bbv-vlen? up) (and (fixnum? up) (<=fx up (bbv-max-fixnum)))))))

;*---------------------------------------------------------------------*/
;*    bbv-range-lo ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-lo o)
   (with-access::bbv-range o (lo)
      lo))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-lonum ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-lonum o)
   (with-access::bbv-range o (lo)
      (if (bbv-vlen? lo)
	  (bbv-vlen-offset lo)
	  lo)))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-up ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-up o)
   (with-access::bbv-range o (up)
      up))
   
;*---------------------------------------------------------------------*/
;*    bbv-range-upnum ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (bbv-range-upnum o)
   (with-access::bbv-range o (up)
      (if (bbv-vlen? o)
	  (bbv-max-fixnum)
	  up)))
   
;*---------------------------------------------------------------------*/
;*    shape ::bbv-range ...                                            */
;*---------------------------------------------------------------------*/
(define-method (shape e::bbv-range)

   (define (bbv-vlen-str n)
      (format "#(~a ~a)"
	 (shape (bbv-vlen-vec n))
	 (bbv-vlen-offset n)))
   
   (define (str n)
      (cond
	 ((eq? n (bbv-max-fixnum)) "upfx")
	 ((eq? n (-fx (bbv-max-fixnum) 1)) "upfx-1")
	 ((eq? n (-fx (bbv-max-fixnum) 2)) "upfx-2")
	 ((eq? n (bbv-min-fixnum)) "lofx")
	 ((eq? n (+fx (bbv-min-fixnum) 1)) "lofx+1")
	 ((eq? n (+fx (bbv-min-fixnum) 2)) "lofx+2")
	 ((fixnum? n) n)
	 ((bbv-vlen? n) (bbv-vlen-str n))
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
   (if *bbv-optim-vlength*
       (let ((val (instantiate::bbv-vlen
		     (vec v)
		     (offset 0))))
	  (instantiate::bbv-range
	     (lo val)
	     (up val)))
       *vlen-range*))

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
	(=rv (bbv-range-up rng) (bbv-range-lo rng))
	(>rv (bbv-range-up rng) (+ (bbv-min-fixnum) 1))
	(<rv (bbv-range-up rng) (- (bbv-max-fixnum) 1))))

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
      ((and (=rv (bbv-range-lo left) (bbv-range-up left))
	    (=rv (bbv-range-lo left) (bbv-range-lo right))
	    (=rv (bbv-range-up left) (bbv-range-up right)))
	    
       'true)
      ((or (eq? (bbv-range>? left right) 'true)
	   (eq? (bbv-range<? left right) 'true))
       'false)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    bbv-range-lt ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-lt left right)
   (with-trace 'bbv-range "bbv-range-lt"
      (let ((li (bbv-range-lo left))
	    (la (bbv-range-up left))
	    (ri (bbv-range-lo right))
	    (ra (bbv-range-up right)))
	 ;; [li..la] < [ri..ra] => [li..max(li,min(la, ra-1)]
	 (let ((res (instantiate::bbv-range
		       (lo li)
		       (up (maxrv li (minrv la (-- ra)))))))
	    (trace-item (shape left) " < " (shape right))
	    (trace-item "    li=" li)
	    (trace-item "    ra=" ra)
	    (trace-item "  --ra=" (-- ra))
	    (trace-item " minrv=" (minrv la (-- ra)))
	    (trace-item " maxrv=" (maxrv li (minrv la (-- ra))))
	    (trace-item "     =>" (shape res))
	    res))))

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
	 (up (maxrv li (minrv la ra))))))

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
	 (lo (minrv la (maxrv li (++ ri))))
	 (up la))))

;*---------------------------------------------------------------------*/
;*    bbv-range-gte ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-gte left right)
   (with-trace 'bbv-range "bbv-range-gte"
      (let ((li (bbv-range-lo left))
	    (la (bbv-range-up left))
	    (ri (bbv-range-lo right))
	    (ra (bbv-range-up right)))
	 ;; [li..la] > [ri..ra] => [min(la,max(li,ri))..la]
	 (let ((res (instantiate::bbv-range
		       (lo (minrv la (maxrv li ri)))
		       (up la))))
	    (trace-item (shape left) " >= " (shape right))
	    (trace-item "    la=" la)
	    (trace-item "    li=" li)
	    (trace-item "    ri=" ri)
	    (trace-item " maxrv=" (maxrv li ri))
	    (trace-item " minrv=" (minrv la (maxrv li ri)))
	    (trace-item "     =>" (shape res))
	    res))))

;*---------------------------------------------------------------------*/
;*    bbv-range-eq ...                                                 */
;*---------------------------------------------------------------------*/
(define (bbv-range-eq left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-lo right))
	  (ra (bbv-range-up right))
	  (li (bbv-range-lo left))
	  (la (bbv-range-up left))
	  (oi (maxrv ri li))
	  (oa (minrv ra la)))
      (when (<=rv oi oa)
	 (instantiate::bbv-range
	    (lo oi)
	    (up oa)))))

;*---------------------------------------------------------------------*/
;*    ->range ...                                                      */
;*---------------------------------------------------------------------*/
(define (->range n)
   (cond
      ((fixnum? n) n)
      ((bignum? n) (if (> n 0) +inf.0 -inf.0))
      (else n)))

;*---------------------------------------------------------------------*/
;*    +rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (+rv x y def)
   (cond
      ((and (number? x) (number? y))
       (->range (+ x y)))
      ((and (bbv-vlen? x) (fixnum? y))
       (duplicate::bbv-vlen x
	  (offset (+fx (bbv-vlen-offset x) y))))
      ((and (fixnum? x) (bbv-vlen? y))
       (duplicate::bbv-vlen y
	  (offset (+fx x (bbv-vlen-offset y)))))
      (else
       def)))
      
;*---------------------------------------------------------------------*/
;*    -rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (-rv x y def)
   (cond
      ((and (number? y) (number? y))
       (+rv x (- y) def))
      ((and (bbv-vlen? x) (fixnum? y))
       (duplicate::bbv-vlen x
	  (offset (-fx (bbv-vlen-offset x) y))))
      ((and (fixnum? x) (bbv-vlen? y))
       (duplicate::bbv-vlen y
	  (offset (-fx x (bbv-vlen-offset x)))))
      (else
       def)))
      
;*---------------------------------------------------------------------*/
;*    *rv ...                                                          */
;*---------------------------------------------------------------------*/
(define (*rv x y def)
   (cond
      ((and (number? x) (number? y))
       (->range (* x y)))
      (else
       def)))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-add ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-add left::bbv-range right::bbv-range)
   (let ((rl (bbv-range-lo right))
	 (ru (bbv-range-up right))
	 (ll (bbv-range-lo left))
	 (lu (bbv-range-up left)))
      (instantiate::bbv-range
	 (lo (+rv ll rl -inf.0))
	 (up (+rv lu ru +inf.0)))))

;*---------------------------------------------------------------------*/
;*    bbv-range-sub ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-sub left::bbv-range right::bbv-range)
   (let ((ri (bbv-range-lo right))
	 (ra (bbv-range-up right))
	 (li (bbv-range-lo left))
	 (la (bbv-range-up left)))
      (instantiate::bbv-range
	 (lo (-rv li ra -inf.0))
	 (up (-rv la ri +inf.0)))))
      
;*---------------------------------------------------------------------*/
;*    bbv-range-mul ...                                                */
;*---------------------------------------------------------------------*/
(define (bbv-range-mul left::bbv-range right::bbv-range)
   (let* ((ri (bbv-range-lonum right))
	  (ra (bbv-range-upnum right))
	  (li (bbv-range-lonum left))
	  (la (bbv-range-upnum left))
	  (v0 (* li ri))
	  (v1 (* li ra))
	  (v2 (* la ra))
	  (v3 (* ra ri))
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
   (instantiate::bbv-range
      (lo (maxrv (bbv-range-lo x) (bbv-range-lo y)))
      (up (minrv (bbv-range-up x) (bbv-range-up y)))))
