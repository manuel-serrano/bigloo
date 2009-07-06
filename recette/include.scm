;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/include.scm                  */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 23 14:14:44 1993                          */
;*    Last change :  Wed Apr  1 14:08:48 1998 (serrano)                */
;*                                                                     */
;*    On test l'inclusion                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module include
   (import  (main "main.scm"))
   (include "test.sch"
	    "include.sch"
	    "include2.sch")
   (export  (test-include)))

;*---------------------------------------------------------------------*/
;*    test-include ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-include)
   (test-module "include" "include.scm")
   (test "sort" foo 1)
   (test "sort" bar 1))
  
  
