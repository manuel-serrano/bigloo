;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/vector.scm             */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:39:09 1992                          */
;*    Last change :  Fri Oct 23 22:11:20 2026 (serrano)                */
;*                                                                     */
;*    On test les operations primitives sur les vecteurs               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module vector
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-vector)))

;*---------------------------------------------------------------------*/
;*    test-vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-vector)
   (test-module "vector" "vector.scm"))
