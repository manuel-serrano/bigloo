;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/kwote.scm                    */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 10:22:02 1992                          */
;*    Last change :  Fri Jul  6 09:37:50 2001 (serrano)                */
;*                                                                     */
;*    On test l'expansion des kwote                                    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module kwote
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-kwote)))

;*---------------------------------------------------------------------*/
;*    test-kwote ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-kwote)
   (test-module "kwote" "kwote.scm")
   (test "kwote" `(list ,(+ 1 2) 4) '(list 3 4))
   (test "kwote" (let ((name 'a)) `(list ,name ',name)) '(list a (quote a)))
   (test "kwote" `(a ,(+ 1 2) ,@(map (lambda (x) (+ 10 x))
				     '(4 -5 6))
		     b)
	 '(a 3 14 5 16 b))
   (test "kwote" `((foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '((cons))))
	 '((foo 7) cons))
   (test "kwote" `(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
	 '(a `(b ,(+ 1 2) ,(foo 4 d) e) f))
   (test "kwote" (let ((name1 'x)
		       (name2 'y))
		    `(a `(b ,,name1 ,',name2 d) e))
	 '(a `(b ,x ,'y d) e))
   (test "kwote" (quasiquote (list (unquote (+ 1 2)) 4))
	 '(list 3 4))
   (test "kwote" '(quasiquote (list (unquote (+ 1 2)) 4))
	 '`(list ,(+ 1 2) 4))
   (test "kwote" `#(1 2 ,(+ 1 2) ,(+ 2 2))
	 '#(1 2 3 4))
   (test "kwote" `#(1 2 ,(+ 1 2) ,@(map (lambda (x) (+ 1 x)) '(3 4)) 6)
	 '#(1 2 3 4 5 6)))
 
