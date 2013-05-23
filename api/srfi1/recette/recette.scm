;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/srfi1/recette/recette.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Dirk Lutzebaeck                                   */
;*    Creation    :  Wed Mar  4 09:27:04 1998                          */
;*    Last change :  Wed Sep  1 11:13:06 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Various format tests.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module test
   (library srfi1)
   (main    test-srfi-1))

;*---------------------------------------------------------------------*/
;*    srfi-1-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (srfi-1-test expr res)
   `(and (let ((val ,expr))
	    (print "compile: provided: " val
		   "  expected: " ,res
		   (if (equal? ,res val)
		       " ok."
		       " error.")))))
 
;*---------------------------------------------------------------------*/
;*    test-srfi-1 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-srfi-1 argv)
   (print "Testing srfi-1")
   (srfi-1-test (cond-expand
		   (srfi-1 'srfi-1)
		   (else 'no-srfi-1))
		'srfi-1)
   (srfi-1-test (xcons '(b c) 'a) '(a b c))
   (srfi-1-test (make-list 4 'c) '(c c c c))
   (srfi-1-test (append-map! (lambda (x) (list x (- x))) '(1 3 8))
		'(1 -1 3 -3 8 -8))
   (srfi-1-test (filter even? '(0 7 8 8 43 -4))
		'(0 8 8 -4))
   (srfi-1-test (find even? '(3 1 4 1 5 9))
		4)
   (srfi-1-test (find-tail even? '(3 1 37 -8 -5 0 0))
		'(-8 -5 0 0))
   (srfi-1-test (find-tail even? '(3 1 37 -5))
		#f)
   (srfi-1-test (any integer? '(a 3 b 2.7))
		#t)
   (srfi-1-test (any integer? '(a 3.1 b 2.7))
		#f)
   (srfi-1-test (any < '(3 1 4 1 5) '(2 7 1 8 2))
		#t)
   (srfi-1-test (list-index even? '(3 1 4 1 5 9))
		2)
   (srfi-1-test (list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))
		1)
   (srfi-1-test (list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))
		#f)
   (srfi-1-test (delete-duplicates '(a b a c a b c z))
		'(a b c z))
   (srfi-1-test (delete-duplicates! (list 'a 'b 'a 'c 'a 'b 'c 'z))
		'(a b c z)))
