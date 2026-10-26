;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/apply.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 10:58:26 1992                          */
;*    Last change :  Mon Oct 26 10:09:28 2026 (serrano)                */
;*                                                                     */
;*    On test differentes sortes d'apply                               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module apply
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-apply)))

;*---------------------------------------------------------------------*/
;*    test-apply ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-apply)
   (test-module "apply" "apply.scm"))
