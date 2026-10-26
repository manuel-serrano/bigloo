;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/callcc.scm             */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 13:57:49 1992                          */
;*    Last change :  Mon Oct 26 11:21:59 2026 (serrano)                */
;*                                                                     */
;*    On test differents call/cc                                       */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module callcc
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-callcc)))

;*---------------------------------------------------------------------*/
;*    test-callcc ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-callcc)
   (test-module "callcc" "callcc.scm"))
