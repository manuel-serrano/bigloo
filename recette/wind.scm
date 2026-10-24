;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/wind.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar  8 19:31:00 1998                          */
;*    Last change :  Sat Oct 24 09:48:06 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Wind test (dynamic-wind and unwind-protect).                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module wind
   (import (utils "utils.scm"))
   (export (test-wind)
      (test-wind-sans-handler end)))

;*---------------------------------------------------------------------*/
;*    test-wind ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-wind)
   (let ((exg ($get-exitd-top)))
      (test-module "wind" "wind.scm")))

(define (test-wind-sans-handler end)
   (test-module "wind" "wind.scm"))
