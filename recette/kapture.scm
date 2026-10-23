;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/kapture.scm            */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 11 10:37:55 1992                          */
;*    Last change :  Fri Oct 23 22:25:55 2026 (serrano)                */
;*                                                                     */
;*    Des tests de capture de variables                                */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module kapture
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-kapture))
   (option (set! *inlining?* #f)))

;*---------------------------------------------------------------------*/
;*    test-kapture ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-kapture)
   (test-module "kapture" "kapture.scm"))
