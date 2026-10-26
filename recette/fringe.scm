;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/fringe.scm             */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 21 16:01:48 1992                          */
;*    Last change :  Mon Oct 26 11:20:22 2026 (serrano)                */
;*                                                                     */
;*    On test same fringe (de luis mateu)                              */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module fringe
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-fringe)))

;*---------------------------------------------------------------------*/
;*    test-fringe ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-fringe)
   (test-module "fringe" "fringe.scm"))
