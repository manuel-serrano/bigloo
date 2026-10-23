;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/bexit.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 12 10:06:03 1992                          */
;*    Last change :  Fri Oct 23 22:15:49 2026 (serrano)                */
;*                                                                     */
;*    On test les trois sortes de `bind-exit'                          */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module bind-exit
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-bind-exit)))

(define (test-bind-exit)
   (test-module "bind-exit" "bind-exit.scm"))
