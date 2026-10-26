;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/print.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:47:58 1992                          */
;*    Last change :  Mon Oct 26 10:06:40 2026 (serrano)                */
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
   (test-module "print" "print.scm"))
  
