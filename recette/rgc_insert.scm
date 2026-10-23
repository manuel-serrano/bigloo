;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/rgc_insert.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Thu Sep  3 12:29:48 2009                          */
;*    Last change :  Fri Oct 23 20:56:19 2026 (serrano)                */
;*    Copyright   :  2009-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Some rgc-buffer-insert-string tests                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module rgc-insert
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-rgc-insert)))

(define (test-rgc-insert)
   (test-module "rgc-insert" "rgc-insert.scm"))
