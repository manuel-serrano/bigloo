;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/speek.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 12 14:06:13 1992                          */
;*    Last change :  Wed Feb  2 14:35:41 2005 (serrano)                */
;*    Copyright   :  1992-2005 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compiler printing module                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_speek
   (import  engine_param)
   (export  (verbose level . args)))

;*---------------------------------------------------------------------*/
;*    *stdout* ...                                                     */
;*---------------------------------------------------------------------*/
(define *stdout* (current-output-port))

;*---------------------------------------------------------------------*/
;*    speek ...                                                        */
;*---------------------------------------------------------------------*/
(define (speek port flag args)
   (if flag
       (begin
	  (for-each (lambda (v) (display-circle v port)) args)
	  (flush-output-port port))))

;*---------------------------------------------------------------------*/
;*    verbose ...                                                      */
;*---------------------------------------------------------------------*/
(define (verbose level . args)
   (speek *stdout* (<= level *verbose*) args))


