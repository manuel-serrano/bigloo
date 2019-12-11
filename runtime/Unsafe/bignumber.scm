;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Unsafe/bignumber.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Marc Feeley                                       */
;*    Creation    :  Tue Mar 11 11:32:17 2008                          */
;*    Last change :  Wed Dec 11 07:43:40 2019 (serrano)                */
;*    Copyright   :  2006-19 Marc Feeley                               */
;*    -------------------------------------------------------------    */
;*    Portable implementation of bignums. This is used only when no    */
;*    native support is available. Hence, its performance is           */
;*    relatively unimportant.                                          */
;*    -------------------------------------------------------------    */
;*    This code is largely based on the "bignum.scm" implementation    */
;*    by Marc Feeley.                                                  */
;*    -------------------------------------------------------------    */
;*    Bignums are represented with vectors of "bignum digits":         */
;*                                                                     */
;*    assuming that the non-negative bignum "n" is represented by      */
;*    the vector "v" of length "k", we have                            */
;*                                                                     */
;*                          k-2                                        */
;*                         -----                                       */
;*                         \                   i                       */
;*    n  =  (v[0]*2-1)  *   >   v[i+1] * radix                         */
;*                         /                                           */
;*                         -----                                       */
;*                         i = 0                                       */
;*                                                                     */
;*    Moreover, for all n, v[k-1] != 0.                                */
;*                                                                     */
;*    note: v[0] = 0 if n is negative, v[0] = 1 if n is non-negative.  */
;*                                                                     */
;*    "radix" must be less than or equal to sqrt(max fixnum)+1.  This  */
;*    guarantees that the result of an arithmetic operation on bignum  */
;*    digits will be a fixnum (this includes the product of two digits)*/
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*    -------------------------------------------------------------    */
;*    The signatures of this module have to be *strictly* identical    */
;*    to those of Ieee/bignum.sch because this module can be used      */
;*    to replace the native implementation.                            */
;*---------------------------------------------------------------------*/
(module __bignum

   (import  __error
	    __object
	    __thread
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __dsssl
	    __bexit
	    __srfi4
	    __bit
	    
	    __r5_control_features_6_4
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_control_features_6_9

	    __evenv)

   (option
      (set! *c-user-header* '("#if( BGL_HAVE_GMP == 0 )"))
      (set! *c-user-foot* '("#endif")))
   
   (extern
      (macro $fixnum->flonum::double (::long) "(double)")
      (macro $flonum->fixnum::long (::double) "(long)")
      (macro $srand::void (::int) "srand"))

   (java
      (class foreign
	 (method static $srand::void (::int) "srand")))
	    
   (cond-expand
      ((not enable-gmp)
       (extern
	  ($make-bignum::bignum (::u16vector) "bgl_make_bignum")
	  (macro $bignum-u16vect::u16vector (::bignum) "BGL_BIGNUM_U16VECT")
	  
	  (export $$fixnum->bignum "bgl_long_to_bignum")
	  (export $$bignum->fixnum "bgl_bignum_to_long")
	  (export $$bignum->elong "bgl_bignum_to_elong")
	  (export $$bignum->llong "bgl_bignum_to_llong")
	  (export $$elong->bignum "bgl_elong_to_bignum")
	  (export $$llong->bignum "bgl_llong_to_bignum")
	  (export $$uint64->bignum "bgl_uint64_to_bignum")
	  (export $$bignum-cmp "bgl_bignum_cmp")
	  (export $$absbx "bgl_bignum_abs")
	  (export $$negbx "bgl_bignum_neg")
	  (export $$divrembx "bgl_bignum_div")
	  (export $$evenbx? "bgl_bignum_even")
	  (export $$oddbx? "bgl_bignum_odd")
	  (export $$+bx "bgl_bignum_add")
	  (export $$-bx "bgl_bignum_sub")
	  (export $$*bx "bgl_bignum_mul")
	  (export $$quotientbx "bgl_bignum_quotient")
	  (export $$remainderbx "bgl_bignum_remainder")
	  (export $$gcdbx "bgl_bignum_gcd")
	  (export $$lcmbx "bgl_bignum_lcm")
	  (export $$string->bignum "bgl_string_to_bignum")
	  (export $$string->integer-obj "bgl_string_to_integer_obj")
	  (export $$bignum->string "bgl_bignum_to_string")
	  (export $$randbx "bgl_rand_bignum")
	  (export $$seed-rand "bgl_seed_rand")
	  
	  (export $$zerobx? "BXZERO")
	  (export $$positivebx? "BXPOSITIVE")
	  (export $$negativebx? "BXNEGATIVE")
	  
	  (export $$bignum->flonum "bgl_bignum_to_flonum")
	  (export $$flonum->bignum "bgl_flonum_to_bignum"))))
   
   (cond-expand
      ((not enable-gmp)
       (export
	  (inline $$string->integer-obj::obj ::string ::long)
	  ($$fixnum->bignum::bignum ::long)
	  ($$bignum->fixnum::long ::bignum)
	  ($$bignum->elong::elong ::bignum)
	  ($$bignum->llong::llong ::bignum)
	  ($$elong->bignum::bignum ::elong)
	  ($$llong->bignum::bignum ::llong)
	  ($$uint64->bignum::bignum ::uint64)
	  ($$bignum-cmp::int ::bignum ::bignum)
	  ($$zerobx?::bool ::bignum)
	  ($$positivebx?::bool ::bignum)
	  ($$negativebx?::bool ::bignum)
	  ($$absbx::bignum ::bignum)
	  ($$negbx::bignum ::bignum)
	  ($$divrembx::obj ::bignum ::bignum)
	  ($$evenbx?::bool ::bignum)
	  ($$oddbx?::bool ::bignum)
	  ($$+bx::bignum ::bignum ::bignum)
	  ($$-bx::bignum ::bignum ::bignum)
	  ($$*bx::bignum ::bignum ::bignum)
	  ($$quotientbx::bignum ::bignum ::bignum)
	  ($$remainderbx::bignum ::bignum ::bignum)
	  ($$gcdbx::bignum ::bignum ::bignum)
	  ($$lcmbx::bignum ::bignum ::bignum)
	  ($$string->bignum::bignum ::string ::int)
	  ($$bignum->string::bstring ::bignum ::long)
	  ($$randbx::bignum ::bignum)
	  ($$seed-rand ::long)
	  
	  ($$flonum->bignum::bignum ::double)
	  ($$bignum->flonum::double ::bignum)))))

;*---------------------------------------------------------------------*/
;*    $$string->integer-obj ...                                         */
;*    -------------------------------------------------------------    */
;*    When no native bignum implementation is available, standard      */
;*    aritmethic operations do not promote their result because        */
;*    this would be too expensive.                                     */
;*---------------------------------------------------------------------*/
(define-inline ($$string->integer-obj str radix)
   (string->integer str radix))

(cond-expand
   ((not enable-gmp)

;*---------------------------------------------------------------------*/
;*    expt ...                                                         */
;*---------------------------------------------------------------------*/
(define-macro (expt v e)
   (cond
      ((and (fixnum? v) (=fx v 2))
       `(bit-lsh 1 ,e))
      ((and (elong? v) (=elong v #e2))
       `(bit-lshelong 1 ,e))
      ((and (llong? v) (=llong v #l2))
       `(bit-lshllong 1 ,e))
      (else
       (error 'expt "Illegal exponentiation" v))))

;*---------------------------------------------------------------------*/
;*    Global constants                                                 */
;*---------------------------------------------------------------------*/
(define (bignum-radix-width)
   14)
(define (bignum-radix)
   (expt 2 14))
(define (bignum-radix-minus-1)
   (-fx (expt 2 14) 1))
(define (bignum-max-fixnum)
   (-fx -1 (*fx -2 (expt 2 28))))
(define (bignum-min-fixnum)
   (*fx -2 (expt 2 28)))
(define (bignum-min-fixnum-div-radix)
   (quotientfx (*fx -2 (expt 2 28)) 16384))

(define (bignum-elong-radix)
   (expt #e2 15))
(define (bignum-min-elong)
   (*elong #e-2 (expt #e2 30)))
(define (bignum-min-elong-div-radix)
   (quotientelong (*elong #e-2 (expt #e2 30)) #e16384))

(define (bignum-llong-radix)
   (expt #l2 30))
(define (bignum-uint64-radix)
   (expt #l2 30))
(define (bignum-min-llong)
   (*llong #l-2 (expt #l2 30)))
(define (bignum-min-llong-div-radix)
   (quotientllong (*llong #l-2 (expt #l2 60)) #l32768))

;*---------------------------------------------------------------------*/
;*    Constructors and accessors                                       */
;*---------------------------------------------------------------------*/
(define (make-bignum::bignum len::int)
   ($make-bignum (make-u16vector len)))
(define (bignum-length bn::bignum)
   (uint16->fixnum (u16vector-length ($bignum-u16vect bn))))
(define (bignum-sign bn::bignum)
   (uint16->fixnum (u16vector-ref ($bignum-u16vect bn) 0)))
(define (bignum-sign-set! bn::bignum sign::int)
   (u16vector-set! ($bignum-u16vect bn) 0 (fixnum->uint16 sign)))
(define (bignum-digit-ref bn::bignum i::int)
   (uint16->fixnum (u16vector-ref ($bignum-u16vect bn) i)))
(define (bignum-digit-set! bn::bignum i::int digit::int)
   (u16vector-set! ($bignum-u16vect bn) i (fixnum->uint16 digit)))
(define (bignum-set-neg! bn::bignum)
   (bignum-sign-set! bn 0))
(define (bignum-set-nonneg! bn::bignum)
   (bignum-sign-set! bn 1))

;*---------------------------------------------------------------------*/
;*    Utilities                                                        */
;*---------------------------------------------------------------------*/
(define (bignum-shrink x len)
   (let ((y (make-bignum len)))
      (let loop ((i (-fx len 1)))
	 (if (<fx i 0)
	     y
	     (begin
		(bignum-digit-set! y i (bignum-digit-ref x i))
		(loop (-fx i 1)))))))

(define (bignum-remove-leading-zeroes x)
   (let ((sign (bignum-sign x)))
      (bignum-set-nonneg! x) ;; setup sentinel (sign = anything != 0)
      (let loop ((i (-fx (bignum-length x) 1)))
	 (cond ((=fx (bignum-digit-ref x i) 0)
		(loop (-fx i 1)))
	       ((=fx i 0)
		bignum-zero)
	       (else
		(bignum-sign-set! x sign)
		(if (=fx i (-fx (bignum-length x) 1))
		    x
		    (bignum-shrink x (+fx i 1))))))))

(define (bignum-normalize x)
   ;; (or (bignum->fixnum x) x)
   x)

(define ($$fixnum->bignum-fresh n)
   (let ((neg-n (if (<fx n 0) n (-fx 0 n))))
      ;; computing with negative n avoids overflow
      (let loop1 ((nb-digits 0) (x neg-n))
	 (if (not (=fx x 0))
	     (loop1 (+fx nb-digits 1) (quotientfx x (bignum-radix)))
	     (let ((r (make-bignum (+fx nb-digits 1))))
		(if (<fx n 0)
		    (bignum-set-neg! r)
		    (bignum-set-nonneg! r))
		(let loop2 ((i 1) (x neg-n))
		   (if (not (=fx x 0))
		       (begin
			  (bignum-digit-set!
			   r
			   i
			   (-fx 0 (remainderfx x (bignum-radix))))
			  (loop2 (+fx i 1) (quotientfx x (bignum-radix))))
		       r)))))))

(define ($$elong->bignum n::elong)
   (let ((neg-n (if (<elong n #e0) n (-elong #e0 n))))
      ;; computing with negative n avoids overflow
      (let loop1 ((nb-digits 0) (x::elong neg-n))
	 (if (not (=elong x 0))
	     (loop1 (+fx nb-digits 1) (quotientelong x (bignum-elong-radix)))
	     (let ((r (make-bignum (+fx nb-digits 1))))
		(if (<elong n #e0)
		    (bignum-set-neg! r)
		    (bignum-set-nonneg! r))
		(let loop2 ((i::int 1) (x::elong neg-n))
		   (if (not (=elong x #e0))
		       (begin
			  (bignum-digit-set!
			   r
			   i
			   (-fx 0 ($elong->long (remainderelong x ($long->elong (bignum-radix))))))
			  (loop2 (+fx i 1) (quotientelong x (bignum-radix))))
		       r)))))))

(define ($$llong->bignum n)
   (let ((neg-n (if (<llong n #l0) n (-llong #l0 n))))
      ;; computing with negative n avoids overflow
      (let loop1 ((nb-digits 0) (x::llong neg-n))
	 (if (not (=llong x 0))
	     (loop1 (+fx nb-digits 1) (quotientllong x (bignum-llong-radix)))
	     (let ((r (make-bignum (+fx nb-digits 1))))
		(if (<llong n #l0)
		    (bignum-set-neg! r)
		    (bignum-set-nonneg! r))
		(let loop2 ((i 1) (x neg-n))
		   (if (not (=llong x #l0))
		       (begin
			  (bignum-digit-set!
			   r
			   i
			   (-fx 0 ($llong->long (remainderllong x ($long->llong (bignum-radix))))))
			  (loop2 (+fx i 1) (quotientllong x (bignum-radix))))
		       r)))))))

(define ($$uint64->bignum n)
   (let ((neg-n (if (<u64 n #u64:0) n (-u64 #u64:0 n))))
      ;; computing with negative n avoids overflow
      (let loop1 ((nb-digits 0) (x::uint64 neg-n))
	 (if (not (=u64 x #u64:0))
	     (loop1 (+fx nb-digits 1) (quotientu64 x (bignum-uint64-radix)))
	     (let ((r (make-bignum (+fx nb-digits 1))))
		(if (<u64 n #u64:0)
		    (bignum-set-neg! r)
		    (bignum-set-nonneg! r))
		(let loop2 ((i 1) (x neg-n))
		   (if (not (=u64 x #u64:0))
		       (begin
			  (bignum-digit-set!
			   r
			   i
			   (-fx 0 (uint64->fixnum (remainderu64 x ($long->uint64 (bignum-radix))))))
			  (loop2 (+fx i 1) (quotientu64 x (bignum-radix))))
		       r)))))))

(define preallocated-bignums
   (let ((v (make-vector 33 #f)))
      (let loop ((i 0) (n -16))
	 (if (<fx 16 n)
	     v
	     (begin
		(vector-set! v i ($$fixnum->bignum-fresh n))
		(loop (+fx i 1) (+fx n 1)))))))

(define ($$fixnum->bignum n)
   (if (or (<fx n -16) (<fx 16 n))
       ($$fixnum->bignum-fresh n)
       (vector-ref preallocated-bignums (+fx n 16))))

(define bignum-zero
   ($$fixnum->bignum 0))

;*---------------------------------------------------------------------*/
;*    Bignum comparison                                                */
;*---------------------------------------------------------------------*/
(define ($=bx x::bignum y::bignum)
    (if (not (=fx (bignum-sign x) (bignum-sign y)))
	#f
	(let ((lenx (bignum-length x)))
	  (if (not (=fx lenx (bignum-length y)))
	      #f
	      (let loop ((i (-fx lenx 1)))
		(if (<fx 0 i)
		    (if (not (=fx (bignum-digit-ref x i)
				  (bignum-digit-ref y i)))
			#f
			(loop (-fx i 1)))
		    #t))))))

(define ($<bx x::bignum y::bignum)
   (if (not (=fx (bignum-sign x) (bignum-sign y)))
       ($$negativebx? x)
       (let ((lenx (bignum-length x))
	     (leny (bignum-length y)))
	  (cond ((<fx lenx leny)
		 (not ($$negativebx? x)))
		((<fx leny lenx)
		 ($$negativebx? x))
		(else
		 (let loop ((i (-fx lenx 1)))
		    (if (<fx 0 i)
			(let ((dx (bignum-digit-ref x i))
			      (dy (bignum-digit-ref y i)))
			   (cond ((<fx dx dy) (not ($$negativebx? x)))
				 ((<fx dy dx) ($$negativebx? x))
				 (else (loop (-fx i 1)))))
			#f)))))))

(define ($>bx x y)
   ($<bx y x))

(define ($<=bx x y)
   (not ($<bx y x)))

(define ($>=bx x y)
   (not ($<bx x y)))

(define ($$zerobx? x)
   (=fx (bignum-length x) 1))

(define ($$negativebx? x)
   (=fx (bignum-sign x) 0))

(define ($$positivebx? x)
   (not (or ($$zerobx? x) ($$negativebx? x))))

(define ($$evenbx? x)
   (or ($$zerobx? x) (even? (bignum-digit-ref x 1))))

(define ($$oddbx? x)
   (not ($$evenbx? x)))

(define ($$bignum-cmp n1 n2)
   (cond
      (($<bx n1 n2) -1)
      (($<bx n2 n1) 1)
      (else 0)))

;*---------------------------------------------------------------------*/
;*    Bignum addition and substraction                                 */
;*---------------------------------------------------------------------*/
(define (bignum-add-nonneg x y)
   
   (define (add x y lenx leny)
      (let ((r (make-bignum (+fx lenx 1))))
	 
	 (bignum-set-nonneg! r)
	 
	 (let loop1 ((i 1) (c 0)) ;; add digits in y
	    (if (<fx i leny)
		
		(let ((w (+fx (+fx (bignum-digit-ref x i)
				   (bignum-digit-ref y i))
			      c)))
		   (if (<fx w (bignum-radix))
		       (begin
			  (bignum-digit-set! r i w)
			  (loop1 (+fx i 1) 0))
		       (begin
			  (bignum-digit-set! r i (-fx w (bignum-radix)))
			  (loop1 (+fx i 1) 1))))
		
		(let loop2 ((i i) (c c)) ;; propagate carry
		   (if (<fx i lenx)
		       
		       (let ((w (+fx (bignum-digit-ref x i) c)))
			  (if (<fx w (bignum-radix))
			      (begin
				 (bignum-digit-set! r i w)
				 (loop2 (+fx i 1) 0))
			      (begin
				 (bignum-digit-set! r i (-fx w (bignum-radix)))
				 (loop2 (+fx i 1) 1))))
		       
		       (if (=fx c 0)
			   (bignum-shrink r lenx)
			   (begin
			      (bignum-digit-set! r lenx c)
			      r))))))))
   
   (let ((lenx (bignum-length x))
	 (leny (bignum-length y)))
      (if (<fx lenx leny)
	  (add y x leny lenx)
	  (add x y lenx leny))))

(define (bignum-sub-nonneg x y)
   
   (define (complement! r)
      (let ((lr (bignum-length r)))
	 (let loop ((i 1) (c 0))
	    (if (<fx i lr)
		
		(let ((w (+fx (bignum-digit-ref r i) c)))
		   (if (<fx 0 w)
		       (begin
			  (bignum-digit-set! r i (-fx (bignum-radix) w))
			  (loop (+fx i 1) 1))
		       (begin
			  (bignum-digit-set! r i 0)
			  (loop (+fx i 1) 0))))))))
   
   (define (sub x y lenx leny)
      (let ((r (make-bignum lenx)))
	 
	 (let loop1 ((i 1) (b 0)) ;; substract digits in y
	    (if (<fx i leny)
		
		(let ((w (-fx (-fx (bignum-digit-ref x i)
				   (bignum-digit-ref y i))
			      b)))
		   (if (<fx w 0)
		       (begin
			  (bignum-digit-set! r i (+fx w (bignum-radix)))
			  (loop1 (+fx i 1) 1))
		       (begin
			  (bignum-digit-set! r i w)
			  (loop1 (+fx i 1) 0))))
		
		(let loop2 ((i i) (b b)) ;; propagate borrow
		   (if (<fx i lenx)
		       
		       (let ((w (-fx (bignum-digit-ref x i) b)))
			  (if (<fx w 0)
			      (begin
				 (bignum-digit-set! r i (+fx w (bignum-radix)))
				 (loop2 (+fx i 1) 1))
			      (begin
				 (bignum-digit-set! r i w)
				 (loop2 (+fx i 1) 0))))
		       
		       (if (=fx b 0)
			   (bignum-set-nonneg! r)
			   (begin
			      (bignum-set-neg! r)
			      (complement! r)))))))
	 
	 (bignum-remove-leading-zeroes r)))
   
   (sub x y (bignum-length x) (bignum-length y)))

(define (bignum-sum2 x y sign-x sign-y)
   
   (define (adjust-sign x s)
      (if (=fx (bignum-sign x) s)
	  (bignum-set-nonneg! x)
	  (bignum-set-neg! x))
      (bignum-normalize (bignum-remove-leading-zeroes x)))
   
   (cond ((=fx sign-x sign-y) ;; same sign
	  (adjust-sign (bignum-add-nonneg x y) sign-x))
	 ((<fx (bignum-length x) (bignum-length y))
	  (adjust-sign (bignum-sub-nonneg y x) sign-y))
	 (else
	  (adjust-sign (bignum-sub-nonneg x y) sign-x))))

(define ($$+bx x y)
   (bignum-sum2 x y (bignum-sign x) (bignum-sign y)))

(define ($$-bx x y)
   (bignum-sum2 x y (bignum-sign x) (-fx 1 (bignum-sign y))))

(define ($$negbx x)
   ($$-bx bignum-zero x))

(define (bignum+ . args)
   (if (pair? args)
       (let loop ((n (car args)) (lst (cdr args)))
	  (if (pair? lst)
	      (loop ($$+bx n (car lst)) (cdr lst))
	      n))
       bignum-zero))

(define (bignum- x . args)
   (if (pair? args)
       (let loop ((n x) (lst args))
	  (if (pair? lst)
	      (loop ($$-bx n (car lst)) (cdr lst))
	      n))
       ($$negbx x)))

;*---------------------------------------------------------------------*/
;*    Bignum multiplication                                            */
;*---------------------------------------------------------------------*/
(define ($$*bx x y)
   
   (define (mul x y lenx leny)
      (let ((r (make-bignum (-fx (+fx lenx leny) 1))))
	 
	 (if (=fx (bignum-sign x) (bignum-sign y))
	     (bignum-set-nonneg! r)
	     (bignum-set-neg! r))
	 
	 (let loop1 ((j 1)) ;; for each digit in y
	    (if (<fx j leny)
		
		(let ((d (bignum-digit-ref y j)))
		   (if (=fx d 0) ;; useful optimization for powers of 2
		       (loop1 (+fx j 1))
		       (let loop2 ((i 1) (k j) (c 0)) ;; multiply and add
			  (if (<fx i lenx)
			      
			      (let ((w (+fx (+fx (bignum-digit-ref r k) c)
					    (*fx (bignum-digit-ref x i) d))))
				 (bignum-digit-set! r k (modulo w (bignum-radix)))
				 (loop2 (+fx i 1)
					(+fx k 1)
					(quotientfx w (bignum-radix))))
			      
			      (begin
				 (bignum-digit-set! r k c)
				 (loop1 (+fx j 1)))))))))
	 
	 (bignum-remove-leading-zeroes r)))
   
   (bignum-normalize (mul x y (bignum-length x) (bignum-length y))))

(define (bignum* . args)
   (if (pair? args)
       (let loop ((n (car args)) (lst (cdr args)))
	  (if (pair? lst)
	      (loop ($$*bx n (car lst)) (cdr lst))
	      n))
       ($$fixnum->bignum 1)))

;*---------------------------------------------------------------------*/
;*    Bignum division                                                  */
;*---------------------------------------------------------------------*/
(define (bignum-div x y)
   
   (define (single-digit-divisor-div x y lenx leny r)
      
      ;; simple algo for single digit divisor
      
      (let ((d (bignum-digit-ref y 1)))
	 (let loop1 ((i (-fx lenx 1)) (k 0))
	    (if (<fx 0 i)
		(let ((w (+fx (*fx k (bignum-radix)) (bignum-digit-ref x i))))
		   (bignum-digit-set! r i (quotientfx w d))
		   (loop1 (-fx i 1) (remainderfx w d)))
		(cons (bignum-remove-leading-zeroes r)
		      ($$fixnum->bignum
		       (if ($$negativebx? x) (-fx 0 k) k)))))))
   
   (define (multi-digit-divisor-div x y lenx leny r)
      
      ;; general algo from Knuth
      
      ;; STEP 1: normalize x and y
      
      (let loop2 ((shift 1)
		  (n (*fx (bignum-digit-ref y (-fx leny 1)) 2)))
	 (if (<fx n (bignum-radix))
	     (loop2 (*fx shift 2) (*fx n 2))
	     
	     (let ((nx (make-bignum (+fx lenx 1)))
		   (ny (make-bignum leny)))
		
		(bignum-sign-set! nx (bignum-sign x))
		
		(let loop3 ((i 1) (c 0))
		   (if (<fx i lenx)
		       (let ((w (+fx (*fx (bignum-digit-ref x i) shift) c)))
			  (bignum-digit-set! nx i (modulo w (bignum-radix)))
			  (loop3 (+fx i 1) (quotientfx w (bignum-radix))))
		       (bignum-digit-set! nx i c)))
		
		(let loop4 ((i 1) (c 0))
		   (if (<fx i leny)
		       (let ((w (+fx (*fx (bignum-digit-ref y i) shift) c)))
			  (bignum-digit-set! ny i (modulo w (bignum-radix)))
			  (loop4 (+fx i 1) (quotientfx w (bignum-radix))))))
		
		(let loop5 ((i lenx))
		   (if (not (<fx i leny))
		       
		       ;; STEP 2: calculate next digit in quotient
		       
		       (let ((msd-of-ny
			      (bignum-digit-ref ny (-fx leny 1)))
			     (next-msd-of-ny
			      (bignum-digit-ref ny (-fx leny 2)))
			     (msd-of-nx
			      (bignum-digit-ref nx i))
			     (next-msd-of-nx
			      (bignum-digit-ref nx (-fx i 1)))
			     (next-next-msd-of-nx
			      (bignum-digit-ref nx (-fx i 2))))
			  
			  (define (next-digit q u)
			     (if (<fx u (bignum-radix))
				 (let* ((temp1 (*fx q next-msd-of-ny))
					(temp2 (quotientfx temp1 (bignum-radix))))
				    (if (or (<fx u temp2)
					    (and (=fx temp2 u)
						 (<fx next-next-msd-of-nx
						      (remainderfx temp1 (bignum-radix)))))
					(next-digit (-fx q 1) (+fx u msd-of-ny))
					q))
				 q))
			  
			  (let ((q (if (=fx msd-of-nx msd-of-ny)
				       (next-digit (bignum-radix-minus-1)
						   (+fx msd-of-ny next-msd-of-nx))
				       (let ((temp (+fx (*fx msd-of-nx (bignum-radix))
							next-msd-of-nx)))
					  (next-digit (quotientfx temp msd-of-ny)
						      (modulofx temp msd-of-ny))))))
			     
			     ;; STEP 3: multiply and substract
			     
			     (let loop7 ((j 1)
					 (k (-fx i (-fx leny 1)))
					 (b 0))
				(if (<fx j leny)
				    
				    (let ((w (-fx (+fx (bignum-digit-ref nx k) b)
						  (*fx (bignum-digit-ref ny j) q))))
				       (bignum-digit-set!
					nx
					k
					(modulo w (bignum-radix)))
				       (loop7 (+fx j 1)
					      (+fx k 1)
					      (quotientfx (-fx w (bignum-radix-minus-1))
							  (bignum-radix))))
				    
				    (let ((w (+fx (bignum-digit-ref nx k) b)))
				       (bignum-digit-set!
					nx
					k
					(modulo w (bignum-radix)))
				       (if (<fx w 0)
					   (begin
					      (bignum-digit-set!
					       r
					       (-fx i (-fx leny 1))
					       (-fx q 1))
					      (let loop8 ((j 1)
							  (k (-fx i (-fx leny 1)))
							  (c 0))
						 (if (<fx j leny)
						     
						     (let ((w
							    (+fx
							     (+fx (bignum-digit-ref nx k)
								  (bignum-digit-ref ny j))
							     c)))
							(bignum-digit-set!
							 nx
							 k
							 (modulo w (bignum-radix)))
							(loop8
							 (+fx j 1)
							 (+fx k 1)
							 (quotientfx w (bignum-radix))))
						     (bignum-digit-set!
						      nx
						      k
						      (modulo
						       (+fx (bignum-digit-ref nx k) c)
						       (bignum-radix))))))
					   (bignum-digit-set! r (-fx i (-fx leny 1)) q))
				       (loop5 (-fx i 1)))))))))
		
		(let loop9 ((i (-fx leny 1)) (k 0))
		   (if (<fx 0 i)
		       (let ((w (+fx (*fx k (bignum-radix))
				     (bignum-digit-ref nx i))))
			  (bignum-digit-set! nx i (quotientfx w shift))
			  (loop9 (-fx i 1)
				 (remainderfx w shift)))))
		
		(cons (bignum-remove-leading-zeroes r)
		      (bignum-remove-leading-zeroes nx))))))
   
   (define (div x y lenx leny)
      (if (<fx lenx leny)
	  
	  (cons bignum-zero x)
	  
	  (let ((r (make-bignum (+fx (-fx lenx leny) 2))))
	     
	     (if (=fx (bignum-sign x) (bignum-sign y))
		 (bignum-set-nonneg! r)
		 (bignum-set-neg! r))
	     
	     (if (=fx leny 2)
		 (single-digit-divisor-div x y lenx leny r)
		 (multi-digit-divisor-div x y lenx leny r)))))
   
   (if ($$zerobx? y)
       (error '/bx "divide by zero" x)
       (div x y (bignum-length x) (bignum-length y))))

(define ($$divrembx x y)
   (let ((v (bignum-div x y)))
      (values (car v) (cdr v))))

(define ($$quotientbx x y)
   (bignum-normalize (car (bignum-div x y))))

(define ($$remainderbx x y)
   (bignum-normalize (cdr (bignum-div x y))))

(define (bignum-modulo x y)
   (let ((r (cdr (bignum-div x y))))
      (if (or ($$zerobx? r)
	      (eqv? ($$negativebx? x)
		    ($$negativebx? y)))
	  (bignum-normalize r)
	  (bignum+ r y))))

;*---------------------------------------------------------------------*/
;*    Bignum MAX, MIN, ABS, GCD, and LCM                               */
;*---------------------------------------------------------------------*/
(define (bignum-max2 x y)
   (if ($<bx x y) y x))

(define (bignum-max x . args)
   (let loop ((n x) (lst args))
      (if (pair? lst)
	  (loop (bignum-max2 n (car lst)) (cdr lst))
	  n)))

(define (bignum-min2 x y)
   (if ($<bx x y) x y))

(define (bignum-min x . args)
   (let loop ((n x) (lst args))
      (if (pair? lst)
	  (loop (bignum-min2 n (car lst)) (cdr lst))
	  n)))

(define ($$absbx x)
   (if ($$negativebx? x)
       ($$negbx x)
       x))

(define ($$gcdbx x y)
   (let loop ((x ($$absbx x)) (y ($$absbx y)))
      (if ($$zerobx? y)
	  x
	  (loop y ($$remainderbx x y)))))

(define ($$lcmbx x y)
   (if (or ($$zerobx? x) ($$zerobx? y))
       bignum-zero
       ($$quotientbx
	($$absbx ($$*bx x y))
	($$gcdbx x y))))

;*---------------------------------------------------------------------*/
;*    Bignum exponentiation                                            */
;*---------------------------------------------------------------------*/
(define (bignum-expt x y)
   (cond (($$zerobx? y)
	  ($$fixnum->bignum 1))
	 (($$evenbx? y)
	  (bignum-expt
	   ($$*bx x x)
	   ($$quotientbx y ($$fixnum->bignum 2))))
	 (else
	  ($$*bx x (bignum-expt x ($$-bx y ($$fixnum->bignum 1)))))))

;*---------------------------------------------------------------------*/
;*    bitwise operations                                               */
;*---------------------------------------------------------------------*/
(define (bignum-bitwise-not x)
   ($$-bx ($$fixnum->bignum -1) x))

;*---------------------------------------------------------------------*/
;*    bignum-integer-length ...                                        */
;*---------------------------------------------------------------------*/
(define (bignum-integer-length x::bignum)
   (let* ((n (if ($$negativebx? x) (bignum-bitwise-not x) x))
	  (nb-digits (-fx (bignum-length n) 1)))
      (if (=fx nb-digits 0)
	  0
	  (let loop ((d (bignum-digit-ref n nb-digits))
		     (len (*fx (-fx nb-digits 1) (bignum-radix-width))))
	     (if (<fx 0 d)
		 (loop (quotientfx d 2)
		       (+fx len 1))
		 len)))))

;*---------------------------------------------------------------------*/
;*    Convertion to and from string                                    */
;*---------------------------------------------------------------------*/
(define ($$bignum->string x radix) ;; 2 <= radix <= 36
   
   (define (digit->char d)
      (string-ref "0123456789abcdefghijklmnopqrstuvwxyz" d))
   
   (define (convert-non-neg sign n)
      (let ((digits
             (map digit->char
                  (if ($$zerobx? n)
                      '(0)
                      (reverse (bignum->fixnum-list n (-fx radix 1)))))))
	 (list->string (if sign (cons sign digits) digits))))
   
   (if ($$negativebx? x)
       (convert-non-neg #\- ($$negbx x))
       (convert-non-neg #f x)))

(define ($$string->bignum str radix) ;; 2 <= radix <= 36
   
   (define (char->digit c rad)
      
      (define (check d)
	 (if (<fx d rad)
	     d
	     #f))
      
      (cond ((and (char>=? c #\0) (char<=? c #\9))
	     (check (-fx (char->integer c) (char->integer #\0))))
	    ((and (char>=? c #\a) (char<=? c #\z))
	     (check (+fx 10 (-fx (char->integer c) (char->integer #\a)))))
	    ((and (char>=? c #\A) (char<=? c #\Z))
	     (check (+fx 10 (-fx (char->integer c) (char->integer #\A)))))
	    (else
	     #f)))
   
   (define (convert rad sign i)
      (if (<=fx (+fx i 1) (string-length str)) ;; need at least one digit
	  (let loop ((i i) (digits '()))
	     (if (<fx i (string-length str))
		 (let ((d (char->digit (string-ref str i) rad)))
		    (if d
			(loop (+fx i 1) (cons d digits))
			($$fixnum->bignum-fresh 0)))
		 (let ((n (fixnum-list->bignum digits (-fx rad 1))))
		    (if (and sign (char=? sign #\-))
			($$negbx n)
			n))))
	  ($$fixnum->bignum-fresh 0)))
   
   (define (sign-prefix rad i)
      (cond ((and (<=fx (+fx i 2) (string-length str)) ;; need at least two chars
		  (memv (string-ref str i) '(#\+ #\-)))
	     (convert rad (string-ref str i) (+fx i 1)))
	    (else
	     (convert rad #f i))))
   
   (define (radix-prefix i)
      (cond ((and (<=fx (+fx i 3) (string-length str)) ;; need at least three chars
		  (char=? (string-ref str i) #\#)
		  (assv (char-downcase (string-ref str (+fx i 1)))
			'((#\x . 16)
			  (#\d . 10)
			  (#\o . 8)
			  (#\b . 2))))
	     =>
	     (lambda (force-radix)
		(sign-prefix (cdr force-radix) (+fx i 2))))
	    (else
	     (sign-prefix radix i))))
   
   (radix-prefix 0))

;*---------------------------------------------------------------------*/
;*    conversion                                                       */
;*---------------------------------------------------------------------*/
(define (bignum->fixnum-list x radix-minus-1)
   (let* ((big-radix
	   ($$+bx
	    ($$fixnum->bignum radix-minus-1)
	    ($$fixnum->bignum 1)))
	  (square-series
	   (let loop ((square big-radix)
		      (square-list (list big-radix)))
	      (let ((new-square
		     ($$*bx square square)))
		 (if ($<bx x new-square)
		     square-list
		     (loop new-square
			   (cons new-square square-list)))))))
      
      (define (convert n square-series tail)
	 (if (pair? square-series)
	     (let* ((qr (bignum-div n (car square-series)))
		    (q (car qr))
		    (r (cdr qr))
		    (new-square-series (cdr square-series)))
		(convert r
			 new-square-series
			 (convert q
				  new-square-series
				  tail)))
	     (let ((d (bignum->fixnum n)))
		(if (and (null? tail) ;; avoid leading zeroes
			 (=fx d 0))
		    tail
		    (cons d tail)))))
      
      (convert x square-series '())))

(define (fixnum-list->bignum digit-list radix-minus-1)
   
   ;; Note: a divide-and-conquer algorithm would be faster for large numbers.
   
   (let ((big-radix
	  ($$+bx
	   ($$fixnum->bignum radix-minus-1)
	   ($$fixnum->bignum 1))))
      (let loop ((n bignum-zero) (lst (reverse digit-list)))
	 (if (pair? lst)
	     (loop ($$+bx ($$*bx n big-radix)
			 ($$fixnum->bignum (car lst)))
		   (cdr lst))
	     n))))

(define ($$bignum->fixnum x) ;; returns #f on fixnum overflow
   (let ((lenx-minus-1 (-fx (bignum-length x) 1)))
      (let loop ((n 0) (i lenx-minus-1))
	 (cond ((<fx 0 i)
		(if (<fx n (bignum-min-fixnum-div-radix))
		    #f
		    (let ((m (*fx n (bignum-radix)))
			  (d (bignum-digit-ref x i)))
		       (if (<fx m (+fx (bignum-min-fixnum) d))
			   #f
			   (loop (-fx m d)
				 (-fx i 1))))))
	       (($$negativebx? x)
		n)
	       ((not (=fx n (bignum-min-fixnum)))
		(-fx 0 n))
	       (else
		#f)))))

(define ($$bignum->elong x) ;; returns #f on fixnum overflow
   (let ((lenx-minus-1 (-fx (bignum-length x) 1)))
      (let loop ((n::elong #e0) (i lenx-minus-1))
	 (cond ((<fx 0 i)
		(if (<elong n (bignum-min-elong-div-radix))
		    #f
		    (let ((m (*elong n (bignum-elong-radix)))
			  (d ($long->elong (bignum-digit-ref x i))))
		       (if (<elong m (+elong (bignum-min-elong) d))
			   #f
			   (loop (-elong m d)
				 (-fx i 1))))))
	       (($$negativebx? x)
		n)
	       ((not (=elong n (bignum-min-elong)))
		(-elong #e0 n))
	       (else
		#f)))))

(define ($$bignum->llong x) ;; returns #f on fixnum overflow
   (let ((lenx-minus-1 (-fx (bignum-length x) 1)))
      (let loop ((n::llong #l0) (i lenx-minus-1))
	 (cond ((<fx 0 i)
		(if (<llong n (bignum-min-llong-div-radix))
		    #f
		    (let ((m (*llong n (bignum-llong-radix)))
			  (d ($long->llong (bignum-digit-ref x i))))
		       (if (<llong m (+llong (bignum-min-llong) d))
			   #f
			   (loop (-llong m d)
				 (-fx i 1))))))
	       (($$negativebx? x)
		n)
	       ((not (=llong n (bignum-min-llong)))
		(-llong #l0 n))
	       (else
		#f)))))

;*---------------------------------------------------------------------*/
;*    $$seed-rand ...                                                  */
;*---------------------------------------------------------------------*/
(define ($$seed-rand seed)
   ($srand seed)
   seed)

;*---------------------------------------------------------------------*/
;*    make-random-u8vector ...                                         */
;*---------------------------------------------------------------------*/
(define (make-random-u8vector len)
   (define (random-fill-u8vector! vec)
      (let loop ((i (-fx (u8vector-length vec) 1)))
	 (if (=fx i -1)
	     vec
	     (begin
		(u8vector-set! vec i (random 256))
		(loop (-fx i 1))))))
   (random-fill-u8vector! (make-u8vector len)))

;*---------------------------------------------------------------------*/
;*    $$randbx ...                                                     */
;*---------------------------------------------------------------------*/
(define ($$randbx range::bignum)
   (let* ((range-bits (bignum-integer-length range))
	  (len (quotient (+fx range-bits 20) 8))
	  (n (bignum-expt (fixnum->bignum 256) (fixnum->bignum len)))
	  (divisor ($$quotientbx n range))
	  (limit (*bx divisor range)))
      (let loop ()
	 (let* ((u8vect (make-random-u8vector len))
		(x (fixnum-list->bignum (u8vector->list u8vect) 255)))
	    (if ($>=bx x limit)
		(loop)
		($$quotientbx x divisor))))))

;*---------------------------------------------------------------------*/
;*    Float conversions                                                */
;*---------------------------------------------------------------------*/
(define ($$flonum->bignum n)
   (fixnum->bignum ($flonum->fixnum n)))

(define ($$bignum->flonum n)
   ($fixnum->flonum ($$bignum->fixnum n)))

))
