;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/alias.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 15 13:30:45 1993                          */
;*    Last change :  Fri Oct 23 10:19:58 2026 (serrano)                */
;*                                                                     */
;*    On test les alias                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module alias
   (import (utils "utils.scm")
	   (alias-aux "alias_aux.scm"))
   (include "test.sch")
   (export (test-alias)))

;*---------------------------------------------------------------------*/
;*    test-alias ...                                                   */
;*---------------------------------------------------------------------*/
(define (test-alias)
   (test-module "alias" "alias.scm"))
  
