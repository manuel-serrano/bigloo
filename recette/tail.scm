;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/tail.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  1 17:44:12 1992                          */
;*    Last change :  Thu Dec 31 18:11:43 2015 (serrano)                */
;*                                                                     */
;*    On test la facon de gerer les appels recursifs terminaux.        */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    le module                                                        */
;*---------------------------------------------------------------------*/
(module tail
   (import  (main "main.scm"))
   (include "test.sch")
   (option (set! *optim* 1))
   (export (test-tail)))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1)
   (letrec ((loop (lambda (x y)
		     (if (=fx x 0)
			 y
			 (loop (- x 1) x)))))
      (loop 2 2)))

;*---------------------------------------------------------------------*/
;*    test2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test2 x)
   (let* ((a   hux)
	  (fun (lambda (c) c)))
      (a (if x (fun 1) (fun 2)))))

(define (hux x)
   x)

;*---------------------------------------------------------------------*/
;*    tail-bar ...                                                     */
;*---------------------------------------------------------------------*/
(define (tail-bar::int b::bool n)
   (define (odd2::int n)
      (if (=fx n 0)
	  #xABCDEF
	  (even2 (-fx n 1)) ))
   (define (even2::short n)
      (if (=fx n 0)
	  10
	  (odd2 (-fx n 1)) ))
   (if b
       (even2 n)
       (odd2 n)))

;*---------------------------------------------------------------------*/
;*    tail-foo ...                                                     */
;*---------------------------------------------------------------------*/
(define (tail-foo::int b::bool n)
   (define (odd::int n)
      (if (=fx n 0)
	  #xABCDEF
	  (even (-fx n 1)) ))
   (define (even::short n)
      (if (=fx n 0)
	  10
	  (odd (-fx n 1)) ))
   (if b
       (odd n)
       (even n)))

;*---------------------------------------------------------------------*/
;*    test-tail ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-tail)
   (test-module "tail" "tail.scm")
   (test "tail" (test1) 1)
   (test "tail" (test2 #f) 2)
   (test "tail coercion" (tail-foo #t 10) (tail-bar #f 10)))

