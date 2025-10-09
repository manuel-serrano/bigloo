;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/include.scm             */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 23 14:14:44 1993                          */
;*    Last change :  Thu Oct  9 15:09:26 2025 (serrano)                */
;*                                                                     */
;*    On test l'inclusion                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module include
   (import (utils "utils.scm"))
   (include "test.sch"
	    "include.sch"
	    "include2.sch")
   (export (test-include)))

;*---------------------------------------------------------------------*/
;*    test-include ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-include)
   (test-module "include" "include.scm")
   (test "sort" foo 1)
   (test "sort" bar 1))
  
  
