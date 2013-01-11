;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/struct.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:42:44 1992                          */
;*    Last change :  Fri Jan 11 10:37:14 2013 (serrano)                */
;*                                                                     */
;*    On test le fonctionnement des structures                         */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module struct
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-struct)))

;*---------------------------------------------------------------------*/
;*    La structure ...                                                 */
;*---------------------------------------------------------------------*/
(define-struct structure x lambda y res)
(define-struct point x y z)

;*---------------------------------------------------------------------*/
;*    test-struct ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-struct)
   (test-module "struct" "struct.scm")
   (test "struct" (let ((s (make-structure '())))
		     (if (not (struct? s))
			 #f)
		     (structure-lambda-set! s (lambda (x y) (+ x y)))
		     (structure-x-set! s 1)
		     (structure-y-set! s 2)
		     (structure-res-set! s ((structure-lambda s)
					    (structure-x s)
					    (structure-y s)))
		     (structure-res s))
	 3)
   (test "struct.eval"
	 (begin
	    (eval '(define-struct structure x lambda y res))
	    (eval '(let ((s (make-structure '())))
		    (if (not (struct? s))
			#f)
		    (structure-lambda-set! s (lambda (x y) (+ x y)))
		    (structure-x-set! s 1)
		    (structure-y-set! s 2)
		    (structure-res-set! s ((structure-lambda s)
					   (structure-x s)
					   (structure-y s)))
		    (structure-res s))))
	 3)
   (let ((p1 (point 1 2 3))
	 (p2 (point 4 5 6)))
      (test "update!" (begin
			 (struct-update! p1 p2)
			 p1)
	    p2)))


