;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/vararity.scm           */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 18 15:32:05 1992                          */
;*    Last change :  Mon Oct 26 11:09:44 2026 (serrano)                */
;*                                                                     */
;*    Les tests sur les aritees variables                              */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module vararity
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-vararity)))

;*---------------------------------------------------------------------*/
;*    test-vararity ...                                                */
;*---------------------------------------------------------------------*/
(define (test-vararity)
   (test-module "vararity" "vararity.scm"))
