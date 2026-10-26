;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/sua.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 16:04:55 1998                          */
;*    Last change :  Mon Oct 26 11:06:21 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `storage analysis tests'                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sua
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-sua))
   (option (bigloo-debug-set! 0)
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
   (test-module "sua" "sua.scm"))

   
