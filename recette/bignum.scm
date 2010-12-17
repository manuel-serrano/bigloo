;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/bignum.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 13 05:06:17 2008                          */
;*    Last change :  Fri Dec 17 09:59:35 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Test bignums                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bignum
   (import (main "main.scm"))
   (include "test.sch")
   (export (test-bignum)))

;*---------------------------------------------------------------------*/
;*    bigfactorial ...                                                 */
;*---------------------------------------------------------------------*/
(define (factorial n::bignum)
   (let loop ((n n) (f #z1))
      (if (positivebx? n)
	  (loop (-bx n #z1) (*bx f n))
	  f)))

;*---------------------------------------------------------------------*/
;*    test-bignum ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-bignum)
   (test-module "bignum" "bignum.scm")
   (test "bignum?.1" (bignum? 1) #f)
   (test "bignum?.2" (bignum? #e1) #f)
   (test "bignum?.3" (bignum? #l1) #f)
   (test "bignum?.4" (bignum? #z1) #t)
   (test "bignum?.5" (bignum? "#z1") #f)
   (test "number?.1" (number? #z1) #t)
   (test "integer?.1" (integer? #z1) #t)
   (test "write.1" (with-output-to-string (lambda () (write #z123457)))
	 "#z123457")
   (test "read.1" (with-input-from-string "#z123457" read) (+ #z123456 1))
   (test "string->bignum.1" (string->bignum "1000111010110000111110101111101010101" 2) #z76606824277)
   (test "string->bignum.2" (string->bignum "160756243201251221" 8)
	 #z3974619006849681)
   (test "string->bignum.3"  (string->bignum "7F02BC" 16)
	 #z8323772)
   (test "string->bignum.4"
	 (string->bignum "74982374918237438749182374013432141" 10)
	 #z74982374918237438749182374013432141)
   (test "factorial.1" (factorial #z10) #z3628800)
   (test "factorial.2" (factorial #z20) #z2432902008176640000)
   (test "even.1" (even? #z5327) #f)
   (test "even.2" (even? #z5328) #t)
   (test "even.3" (evenbx? #z5327) #f)
   (test "even.4" (evenbx? #z5328) #t)
   (test "odd.1" (odd? #z5327) #t)
   (test "odd.2" (odd? #z5328) #f)
   (test "odd.3" (oddbx? #z5327) #t)
   (test "odd.4" (oddbx? #z5328) #f)
   (test "cnst.1" (let ((x 444432895893458734875837457834)) (+ 1 x))
	 (string->bignum "444432895893458734875837457835"))
   (test "bignum.1" (bignum->string (string->bignum "123")) "123")
   (test "bignum.2" (bignum->string (string->bignum "-123")) "-123")
   (test "bignum.3" (=bx (string->bignum "987654321") (string->bignum "987654321")) #t)
   (test "bignum.4" (<bx (string->bignum "987654321") (string->bignum "987654322")) #t)
   (test "bignum.5" (<=bx (string->bignum "987654321") (string->bignum "987654322")) #t)
   (test "bignum.6" (>bx (string->bignum "987654321") (string->bignum "987654320")) #t)
   (test "bignum.7" (>=bx (string->bignum "987654321") (string->bignum "987654320")) #t)
   (test "bignum.8" (zerobx? (string->bignum "0")) #t)
   (test "bignum.9" (not (zerobx? (string->bignum "123"))) #t)
   (test "bignum.10" (negativebx? (string->bignum "-123")) #t)
   (test "bignum.11" (not (negativebx? (string->bignum "0"))) #t)
   (test "bignum.12" (positivebx? (string->bignum "123")) #t)
   (test "bignum.13" (not (positivebx? (string->bignum "0"))) #t)
   (test "bignum.14" (evenbx? (string->bignum "10")) #t)
   (test "bignum.15" (evenbx? (string->bignum "-10")) #t)
   (test "bignum.16" (not (evenbx? (string->bignum "9"))) #t)
   (test "bignum.17" (oddbx? (string->bignum "17")) #t)
   (test "bignum.18" (oddbx? (string->bignum "-17")) #t)
   (test "bignum.19" (not (oddbx? (string->bignum "10"))) #t)
   (test "bignum.20" (=bx (string->bignum "123")
			  (+bx (string->bignum "115") (string->bignum "8"))) #t)
   (test "bignum.21" (=bx (string->bignum "5")
			  (+bx (string->bignum "-4") (string->bignum "9"))) #t)
   (test "bignum.22" (=bx (string->bignum "100")
			  (-bx (string->bignum "115") (string->bignum "15"))) #t)
   (test "bignum.23" (=bx (string->bignum "-9")
			  (-bx (string->bignum "11") (string->bignum "20"))) #t)
   (test "bignum.24" (=bx (string->bignum "-731")
			  (*bx (string->bignum "-43") (string->bignum "17"))) #t)
   (test "bignum.25" (=bx (string->bignum "-58823")
			  (quotientbx (string->bignum "1000000")
				      (string->bignum "-17"))) #t)
   (test "bignum.26" (=bx (string->bignum "9")
			  (remainderbx (string->bignum "1000000")
				       (string->bignum "-17"))) #t)
   (test "bignum.27" (=bx (string->bignum "-8")
			  (modulobx (string->bignum "1000000")
				    (string->bignum "-17"))) #t)
   (test "bignum.28" (=bx (string->bignum "654321")
			  (maxbx (string->bignum "123456")
				 (string->bignum "654321"))) #t)
   (test "bignum.29" (=bx (string->bignum "123456")
			  (minbx (string->bignum "123456")
				 (string->bignum "654321"))) #t)
   (test "bignum.30" (>bx (string->bignum "ABCDEF" 16)
			  (string->bignum "ABCDEE" 16)) #t)
   (test "bignum.31" (*bx #z10000 #z0) #z0)
   (test "bignum.32" (*bx #z10000000000000000000000000000000000 #z0) #z0)
   (test "bignum.33" (fixnum? (+ 34 (car (list 35)))) #t)
   (test "bignum.34" (fixnum? (+ 34 (car (list #z35)))) #t)
   (test "bignum.35" (fixnum? (+ #z34 (car (list #z35)))) #t)
   (test "bignum.36" (fixnum? (+ #z34 (car (list 35)))) #t)
   (test "bignum.37" (number? (/ #z2 (if (> (current-seconds) 0) 1 2))) #t)
   (test "bignum.38" (number? (/ #z3 (if (> (current-seconds) 0) 2 2))) #t))

 
