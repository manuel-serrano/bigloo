;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/case.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 18 15:16:39 1992                          */
;*    Last change :  Tue Dec 12 11:31:43 2017 (serrano)                */
;*                                                                     */
;*    On test le case.                                                 */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module case
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-case)))

;*---------------------------------------------------------------------*/
;*    dummy-macro ...                                                  */
;*---------------------------------------------------------------------*/
(define (dummy-macro x)
   x)

;*---------------------------------------------------------------------*/
;*    test 1.                                                          */
;*---------------------------------------------------------------------*/
(define (test1 x) 
   (case (dummy-macro x)
      ((tutu) 'tutu)
      ((toto) 'toto)
      ((toto1 toto2 toto3 toto4 toto5 toto6 toto7) 'toto-bis)
      ((fofo1 fofo2 fofo3 fofo4 fofo5 fofo6 fofo7) 'toto-ter)
      ((fofo1 fofo2 fofo3 fofo4 fofo5 fofo6 fofo7 fofo8 fofo9
	      fofo10 fofo11 fofo12 fofo13 fofo14 fofo15 fofo16
	      fofo17 fofo18 fofo19)
       'toto-quar)
      (else
       'dummy
       'else)))

;*---------------------------------------------------------------------*/
;*    test 2.                                                          */
;*---------------------------------------------------------------------*/
(define (test2 x)
   (case (begin 1 2 x)
      ((tete tutu tyty) 1)
      ((toto) 2)
      ((tata) 3)))

;*---------------------------------------------------------------------*/
;*    test 3.                                                          */
;*---------------------------------------------------------------------*/
(define (test3 x)
   (case (begin 2 3 x)
      ((1 2 3 4) 1)
      ((5 6 7 8) 5)
      (else      0)))

;*---------------------------------------------------------------------*/
;*    test 4.                                                          */
;*---------------------------------------------------------------------*/
(define (test4 x)
   (case x
      ((tutu 1) "tutu ou 1")
      ((toto 1 2 #\a) "toto")
      ((2 3) "2 ou 3") 
      (else "else")))

;*---------------------------------------------------------------------*/
;*    test 5.                                                          */
;*    -------------------------------------------------------------    */
;*    Ce test est important car il permet de tester la compilation     */
;*    des cases qui comportent des symboles qui ont meme nombre de     */
;*    hash                                                             */
;*---------------------------------------------------------------------*/
(define (test5 x)
   (case (dummy-macro x)
      ((SHOW show)
       'show)
      ((compute!)
       'compute!)
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    test 6.                                                          */
;*---------------------------------------------------------------------*/
(define (test6 x)
   (case x
      ((#\o)
       #\o)
      ((#\d)
       #\d)
      ((#\x)
       #\x)))

;*---------------------------------------------------------------------*/
;*    test 7. ...                                                      */
;*---------------------------------------------------------------------*/
(define (test7 x)
   ;; set! et fibo on meme nombre de hash
   (case x
      ((set!)
       'set!)
      (else
       'else)))

;*---------------------------------------------------------------------*/
;*    test.8                                                           */
;*    -------------------------------------------------------------    */
;*    This test used to make the compiler crashes.                     */
;*---------------------------------------------------------------------*/
(define-macro push!
   (lambda (stack o)
      `(begin
	  (set! ,stack (cons ,o ,stack))
	  ,o)))

(define (test.8 data)
   (let ((elem-stack '()))
      (push! elem-stack
	     (read/rp (regular-grammar ()
			 (else 2))
		      (open-input-string data)))
      elem-stack))

;*---------------------------------------------------------------------*/
;*    test-case-cond-1 ...                                             */
;*---------------------------------------------------------------------*/
(define (test-case-cond-1 case)
   (cond
      ((=fx case 0) 1)
      ((=fx case 1) 2)
      ((=fx case 2) 3)
      ((=fx case 3) 4)
      ((=fx case 4) 5)
      ((=fx case 5) 6)
      (else 7)))

;*---------------------------------------------------------------------*/
;*    test-case-cond-2 ...                                             */
;*---------------------------------------------------------------------*/
(define (test-case-cond-2 val)
   (cond
      ((=fx val 0) 1)
      ((=fx val 1) 2)
      ((=fx val 2) 3)
      ((=fx val 3) 4)
      ((=fx val 4) 5)
      ((=fx val 5) 6)
      (else 7)))

;*---------------------------------------------------------------------*/
;*    test-uint32 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-uint32 x::uint32)
   (case x
      ((#u32:0) (begin 28))
      ((#u32:1) (begin 'un))
      ((#u32:2) (begin #f))
      (else (begin #t))))

;*---------------------------------------------------------------------*/
;*    test-case ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-case)
   (test-module "case" "case.scm")
   (test "case symbol" (test1 'tutu) 'tutu)
   (test "case symbol" (test1 'toto) 'toto)
   (test "case symbol" (test1 'tata) 'else)
   (test "case symbol" (test1 5) 'else)
   (test "case symbol" (test1 'fofo15) 'toto-quar)
   (test "case symbol" (test2 'tutu) 1)
   (test "case symbol" (test2 'toto) 2)
   (test "case symbol" (test2 'tata) 3)
   (test "case symbol" (test2 5) #unspecified)
   (test "case integer" (test3 (+ 1 2)) 1)
   (test "case integer" (test3 'toto) 0)
   (test "case mixte" (test4 'tutu) "tutu ou 1")
   (test "case mixte" (test4 1) "tutu ou 1")
   (test "case mixte "(test4 3) "2 ou 3")
   (test "case mixte" (test4 'titi) "else")
   (test "case mixte" (test4 #\a) "toto")
   (test "case mixte" (test4 2) "toto")
   (test "case hash" (test5 'show) 'show)
   (test "case hash" (test5 'SHOW) 'show)
   (test "case hash" (test5 'compute!) 'compute!)
   (test "case hash" (test5 'toto) 'else)
   (test "case char"  (test6 #\a) #unspecified)
   (test "case char" (test6 #\x) #\x)
   (test "case char" (test6 (string-ref "o" 0)) #\o)
   (test "case hash" (test7 'fibo) 'else)
   (test "case cond.1" (test-case-cond-1 5) 6)
   (test "case cond.2" (test-case-cond-2 5) 6)
   (test "case uint32.1" (test-uint32 0) 28)
   (test "case uint32.2" (test-uint32 1) 'un)
   (test "case uint32.3" (test-uint32 2) #f)
   (test "case uint32.4" (test-uint32 3) #t)
   (test "case uint32.5" (test-uint32 4) #t))
   



   
