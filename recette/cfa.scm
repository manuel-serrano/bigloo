;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/cfa.scm                */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul  7 11:13:48 1993                          */
;*    Last change :  Mon Oct 26 10:10:44 2026 (serrano)                */
;*                                                                     */
;*    Quelques tests sur la 0cfa                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module 0cfa
   (import (utils "utils.scm")
	   (module "module.scm"))
   (include "test.sch")
   (export (test-0cfa)))

(define (test-0cfa)
   (test-module "0cfa" "0cfa.scm"))
