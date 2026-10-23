;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/globalis.scm           */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 11:30:18 1992                          */
;*    Last change :  Fri Oct 23 14:15:21 2026 (serrano)                */
;*                                                                     */
;*    Des tests qui globalisent des fonctions locales par la passe Glo */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module globalisation
   (import  (utils "utils.scm"))
   (include "test.sch")
   (export  (test-globalisation))
   (option  (set! *inlining?* #f)))

(define (test-globalisation)
   (test-module "globalisation" "globalisation.scm"))
