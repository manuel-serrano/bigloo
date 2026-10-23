;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/sqic.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 14:27:18 1992                          */
;*    Last change :  Fri Oct 23 21:38:59 2026 (serrano)                */
;*                                                                     */
;*    On fait quelque tests qui portent sur la phase sqic->c.          */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module sqic
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-sqic)))

;*---------------------------------------------------------------------*/
;*    test-sqic ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-sqic)
   (test-module "sqic" "sqic.scm"))
      
