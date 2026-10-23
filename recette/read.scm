;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/5.0.x/recette/read.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 22 16:17:03 1992                          */
;*    Last change :  Fri Oct 23 20:46:50 2026 (serrano)                */
;*                                                                     */
;*    Un essai de reader                                               */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module read
   (import (utils "utils.scm"))
   (include "test.sch")
   (export (test-read)))
 
(define (test-read)
   (test-module "read" "read.scm"))
	    
		   

