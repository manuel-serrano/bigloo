;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/sua.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 16:04:55 1998                          */
;*    Last change :  Wed Dec 26 16:44:09 2007 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `storage analysis tests'                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sua
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-sua))
   (option  (bigloo-debug-set! 0)
	    (set! *unsafe-library* #t)
	    (set! *unsafe-arity*   #t)
	    (set! *unsafe-type*    #t)
	    (set! *unsafe-eval*    #t)
	    (set! *unsafe-struct*  #t)
	    (set! *unsafe-range*   #t)
	    (set! *unsafe-version* #t)
	    (set! *optim* 6)))

;*---------------------------------------------------------------------*/
;*    test-sua ...                                                     */
;*---------------------------------------------------------------------*/
(define (test-sua)
   (test-module "sua" "sua.scm")
   (test "vector"
	 (let ((random-strings '#(#"d909ef3e" 0)))
  	    (substring (vector-ref random-strings 0) 0 1))
	 "d"))

   
