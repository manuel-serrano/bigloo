;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/hygiene.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 27 12:52:59 1998                          */
;*    Last change :  Mon Oct 26 10:59:53 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Hygienic macro tests                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module hygiene
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-hygiene)))

(define (test-hygiene)
   (test-module "hygiene" "hygiene.scm"))
