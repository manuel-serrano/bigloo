;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/tail.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  1 17:44:12 1992                          */
;*    Last change :  Mon Oct 26 10:07:48 2026 (serrano)                */
;*                                                                     */
;*    On test la facon de gerer les appels recursifs terminaux.        */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    le module                                                        */
;*---------------------------------------------------------------------*/
(module tail
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-tail)))

;*---------------------------------------------------------------------*/
;*    test-tail ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-tail)
   (test-module "tail" "tail.scm"))

