;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/cell.scm                     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 29 17:16:25 1992                          */
;*    Last change :  Thu Jul 26 10:32:01 2001 (serrano)                */
;*                                                                     */
;*    Un essai de fonction qui kapture des variables qui doivent       */
;*    etre placees dans des variables d'indirection.                   */
;*---------------------------------------------------------------------*/


;*---------------------------------------------------------------------*/
;*    le module                                                        */
;*---------------------------------------------------------------------*/
(module glo_cell
   (import  (main "main.scm"))
   (static  (test1 a b)
	    (test2 a b . c)
	    (test3 . c))
   (include "test.sch")
   (export  (test-cell)))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1 a b)
   (labels ((foo (x) (set! x b) x))
      foo))

;*---------------------------------------------------------------------*/
;*    test2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test2 a b . c)
   (labels ((foo (x y) (set! c y) y)
	    (bar (z)   (foo 2 1))
	    (hux (t)   (set! a (foo t t)) c))
      hux))

;*---------------------------------------------------------------------*/
;*    test3                                                            */
;*---------------------------------------------------------------------*/
(define (test3 . c)
   (let ((a 0)
	 (b 0))
      (labels ((foo (x) (set! a x) a)
	       (bar (y) (set! b y) (foo b)))
	 (bar 1)
	 foo)))
      
;*---------------------------------------------------------------------*/
;*    test-cell ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-cell)
   (test-module "glo_cell" "cell.scm")
   (test "cell" ((test1 1 2) 3) 2)
   (test "cell" ((test2 1 2 3 4) 2) 2)
   (test "cell" ((test3 1 2 3 4) 2) 2))
   
