;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/struct.scm             */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 09:42:44 1992                          */
;*    Last change :  Mon Oct 26 10:07:13 2026 (serrano)                */
;*                                                                     */
;*    On test le fonctionnement des structures                         */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module struct
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-struct)))

;*---------------------------------------------------------------------*/
;*    test-struct ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-struct)
   (test-module "struct" "struct.scm"))

