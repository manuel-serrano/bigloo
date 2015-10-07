;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/flonum.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 26 11:59:37 1992                          */
;*    Last change :  Wed Oct  7 12:28:07 2015 (serrano)                */
;*                                                                     */
;*    On test un peu les flotants (il le faut bien).                   */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module flonum
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-flonum)))

;*---------------------------------------------------------------------*/
;*    fact ...                                                         */
;*---------------------------------------------------------------------*/
(define (fact x)
   (if (< x 2)
       1
       (* (fact (- x 1)) x)))

;*---------------------------------------------------------------------*/
;*    sqrt-test ...                                                    */
;*---------------------------------------------------------------------*/
(define (sqrt-test x::double)
   (sqrt x))

;*---------------------------------------------------------------------*/
;*    check-inexact? ...                                               */
;*---------------------------------------------------------------------*/
(define (check-inexact? z)
   (=fl z (exact->inexact (inexact->exact z))))

;*---------------------------------------------------------------------*/
;*    test-flonum ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-flonum)
   (test-module "flonum" "flonum.scm")
   (test "flonum.1" 3.141592 3.141592)
   (test "flonum.2" -3.141592 -3.141592)
   (test "flonum.3" 3.141592 3.141592)
   (test "flonum.4" -3.141592 -3.141592)
   (test "flonum.5" (/ (inexact->exact (cos 0)) 2) 0.5)
   (test "flonum.6" (/ 1 2) 0.5)
   (test "flonum.7" 23.56 (+fl 23.0 0.56))
   (test "flonum.8" (let ((p (open-input-string "3.14")))
		       (read p))
	 3.14)
   (test "flonum.9" (let ((p (open-input-string "-3.14")))
		       (read p))
	 -3.14)
   (test "flonum.10" (let ((p (open-input-string "1e2")))
			(read p))
	 1e2)
   (test "flonum.11" (let ((p (open-input-string "1e-2")))
			(read p))
	 1e-2)
   (test "flonum.12" (let ((p (open-input-string "-1e2")))
			(read p))
	 -1e2)
   (test "flonum.13" (let ((p (open-input-string "-1e-2")))
			(read p))
      -1e-2)
   (test "flonum.14" (check-inexact? (-fl (fixnum->flonum $minvalfx) 1.0)) #t)
   (test "flonum.15" (check-inexact? (fixnum->flonum $minvalfx)) #t)
   (when (=fx (-fx (flonum->fixnum (+fl (fixnum->flonum $minvalfx) 1.0)) 1)
	    $minvalfx)
      (test "flonum.16" (check-inexact? (+fl (fixnum->flonum $maxvalfx) 1.0)) #t)
      (test "flonum.17" (check-inexact? (fixnum->flonum $maxvalfx)) #t))
   (test "flonum.18" (check-inexact? 5.0) #t)
   (test "flonum.18" (check-inexact? -5.0) #t)
   (test ">=fl" (if (>=fl 0.0 3.0) 1 2) 2)
   (test "cos.1" (sin 0) 0.0)
   (test "cos.2" (acos (cos 2.0)) 2.0)
   (test "round" (round 4.3) 4.0)
   (test "exact" (exact? 5) #t)
   (test "exact" (exact? 5.0) #f)
   (test "inexact" (inexact? 5) #f)
   (test "inexact" (inexact? 5.0) #t)
   (test "exact" (inexact->exact 4.3) 4)
   (test "fact" (fact 5) 120)
   (test "fact" (fact 5.0) 120.0)
   (test "floor" (floor -4.3) -5.0)
   (test "ceiling" (ceiling -4.3) -4.0)
   (test "truncate" (truncate -4.3) -4.0)
   (test "round" (round -4.3) -4.0)
   (test "floor" (floor 3.5) 3.0)
   (test "ceiling" (ceiling 3.5) 4.0)
   (test "truncate" (truncate 3.5) 3.0)
   (test "round" (round 3.5) 4.0)
   (test "sqrt" (sqrt-test 4.0) 2.0)
   (test "atan" (let ((r (atan 1.0 1.0))) (> r 0)) #t)
   (test "prec" (let ((num (+ 1e-6 1e-12)))
		   (=fl (string->number (number->string num)) num))
	 #t)
   (test "nan" (=fl +nan.0 +nan.0) #f)
   (test "inf.1" (<fl -inf.0 +inf.0) #t)
   (test "inf.2" (<fl +inf.0 -inf.0) #f)
   (test "inf.3" (<fl +inf.0 -inf.0) #f)
   (test "inf.4" (>fl +inf.0 -inf.0) #t)
   (test "inf.5" +inf.0 (/ 1 .0))

   ;; Some platforms don't use the same endianess for doubles and integers.
   ;; If the following test fails we need to modify the DOUBLE_TO_LLONG_BITS
   ;; (and similar) macros to modify the order of the bytes for this platform.
   ;; TODO: enable test, once Bigloo's double printing is fixed.
   ; (test "llong-bits.0"
   ; 	 (double->llong-bits 5.626349274901198e-221)
   ; 	 #lx123456789ABCDEF0)
   (test "llong-bits.1"
	 (llong-bits->double (double->llong-bits 3.1415))
	 3.1415)
   (test "llong-bits.2" (double->llong-bits 1.0) #lx3ff0000000000000)
   (test "llong-bits.3" (double->llong-bits +inf.0) #lx7ff0000000000000)
   (test "int-bits.0" (float->int-bits 5.6904566139035e-28) #x12345678)
   (test "int-bits.1" (int-bits->float (float->int-bits 3.1415)) 3.1415)
   (test "int-bits.2" (float->int-bits 1.0) (bit-lsh #x3f80 16))
   (test "int-bits.3" (float->int-bits +inf.0) (bit-lsh #x7f80 16))
   (test "randomfl" (let ((n (randomfl))) (and (>=fl n 0.) (<=fl n 1.))) #t))
