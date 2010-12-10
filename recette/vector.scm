;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/vector.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:39:09 1992                          */
;*    Last change :  Sun Nov 28 07:10:21 2010 (serrano)                */
;*                                                                     */
;*    On test les operations primitives sur les vecteurs               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module vector
   (import  (main "main.scm"))
   (include "test.sch")
   (type (tvector array-of-int (int)))
   (export  (test-vector)))

;*---------------------------------------------------------------------*/
;*    Tvector optimization check                                       */
;*---------------------------------------------------------------------*/
(define *number-images* (vector #\0 #\1 #\2))
(define *foo*           (vector "toto" "toto"))

(define (prin-integer n)
   (let ((x (vector-ref *number-images* 2)))
      x))
 
(define (foo n)
   (vector-ref (if (equal? 5 n) *number-images* *foo*) 0)
   (prin-integer n)) 

;*---------------------------------------------------------------------*/
;*    test-vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-vector)
   (test-module "vector" "vector.scm")
   (test "vector?.1" (vector? '#()) #t)
   (test "vector?.2" (vector? '#(1)) #t)
   (test "vector?.3" (vector? '(1)) #f)
   (test "ref" (vector-ref '#(1 2 3 4) 2) 3)
   (test "set" (let ((v (make-vector 1 '())))
		  (vector-set! v 0 'toto)
		  (vector-ref  v 0))
	 'toto)
   (test "length" (vector-length '#(1 2 3 4 5)) 5)
   (test "length" (vector-length (make-vector 5 'toto)) 5)
   (test "equal vector" (let ((v (make-vector 3 '())))
			   (vector-set! v 0 '(1 2 3))
			   (vector-set! v 1 '#(1 2 3))
			   (vector-set! v 2 'hello)
			   v)
	 '#((1 2 3) #(1 2 3) hello))
   (test "vector-fill" (let ((v (make-vector 3 1)))
			  (vector-fill! v 2)
			  (+ (vector-ref v 0)
			     (vector-ref v 1)
			     (vector-ref v 2)))
	 6)
   (test "tvector.1" (let ((t '#(1 2 3)))
			(vector-ref t 2))
	 3)
   (test "tvector2"
	 (string? (with-output-to-string
		     (lambda ()
			(print (make-array-of-int 1 1)))))
	 #t)
   (test "vector-ref" (foo 10) #\2)
   (test "vector-ref" (vector-ref (let ((v (vector 0 1 2))) v) 2) 2)
   (test "sort-vector.1" (sort < '#(5 3 4 2 1)) '#(1 2 3 4 5))
   (test "sort-vector.2" (sort '#(5 3 4 2 1) <) '#(1 2 3 4 5))
   (test "vector-append.1" (vector-append '#() '#()) '#())
   (test "vector-append.2" (vector-append '#()) '#())
   (test "vector-append.3" (vector-append '#(a)) '#(a))
   (test "vector-append.4" (vector-append '#(a b) '#(c d)) '#(a b c d))
   (test "vector-append.5" (vector-append '#(a b) '#(c d) '#()) '#(a b c d))
   (test "vector-append.6" (vector-append '#(a b) '#(c d) '#(e)) '#(a b c d e)))
