;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/pregexp.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun 28 16:04:55 1998                          */
;*    Last change :  Fri Oct 23 14:26:31 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Portable regular expressions test suit for Scheme                */
;*    Dorai Sitaram                                                    */
;*    http://www.cs.rice.edu/~dorai                                    */
;*    Oct 2, 1999                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module pregexp
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-pregexp)))

(define (test-pregexp)
   (test-module "pregexp" "pregexp.scm"))
