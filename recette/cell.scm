;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/cell.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 29 17:16:25 1992                          */
;*    Last change :  Fri Oct 23 22:18:34 2026 (serrano)                */
;*                                                                     */
;*    Un essai de fonction qui kapture des variables qui doivent       */
;*    etre placees dans des variables d'indirection.                   */
;*---------------------------------------------------------------------*/


;*---------------------------------------------------------------------*/
;*    le module                                                        */
;*---------------------------------------------------------------------*/
(module glo_cell
   (import  (utils "utils.scm"))
   (include "test.sch")
   (export  (test-cell)))

(define (test-cell)
   (test-module "glo_cell" "cell.scm"))
   
