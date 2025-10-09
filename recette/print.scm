;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/wasm/recette/print.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:47:58 1992                          */
;*    Last change :  Thu Oct  9 15:01:37 2025 (serrano)                */
;*                                                                     */
;*    On affiche des objects pour voir si tous ce passe bien           */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module print
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-print)))

;*---------------------------------------------------------------------*/
;*    test-print ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-print)
   (test-module "print" "print.scm")
   (test "char" (begin (display #\Z *recette-port*) #t) #t)
   (test "string" (begin (display "toto n'est pas content" *recette-port*)
			 #t)
	 #t)
   (test "integer" (begin (display 1 *recette-port*) #t) #t)
   (test "integer" (begin (display -1 *recette-port*) #t) #t)
   (test "list" (begin (display '(1 2 3 4 5) *recette-port*) #t) #t)
   (test "vector" (begin (display '#(1 2 3 4 5) *recette-port*) #t) #t)
   (test "newline" (begin (fprint *recette-port* #\Newline) #t) #t))
  
