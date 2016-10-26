;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/rgc_jm.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 12:14:43 1992                          */
;*    Last change :  Wed Oct 26 08:14:02 2016 (serrano)                */
;*                                                                     */
;*    Un essai de grammaire de Jean-Marie qui faisait planter          */
;*    bigloov1.0                                                       */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module rgc-jm
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-rgc-jm)))

;*---------------------------------------------------------------------*/
;*    test-rgc-jm ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-rgc-jm)
   (test-module "rgc-jm" "rgc-jm.scm")
   (test "rgc-jm" (let* ((level 0)
			 (my-grammar
			  (regular-grammar ((lpar (in #\( #\[ #\{))
					    (rpar (in #\) #\] #\})) )
			     (lpar
			      (the-string)
			      (set! level (+ level 1)))
			     (rpar
			      (the-string)
			      (set! level (- level 1))
			      'paren)
			     ((+ (out  #\( #\[ #\{ #\) #\] #\}))
			      (the-string))
			     (else (the-failure)))))
		     (with-input-from-file "misc/jm.txt"
			(lambda ()
			   (labels ((loop ()
			       (let ((c (read/rp my-grammar
						 (current-input-port))))
				  (if (eof-object? c)
				      level
				      (if (eq? c 'paren)
					  (loop)
					  (loop))))))
			      (loop)))))
	 7))
