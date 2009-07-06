;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/cigloo/Tools/speek.scm               */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 12 14:06:13 1992                          */
;*    Last change :  Tue Nov 16 07:24:14 1999 (serrano)                */
;*                                                                     */
;*    Le module ou l'on parle                                          */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_speek
   (import  engine_param)
   (export  (verbose level . args)
	    (fprin   port . obj)))

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
   (speek (current-output-port) (<= level *verbose*) args))

;*---------------------------------------------------------------------*/
;*    fprin ...                                                        */
;*---------------------------------------------------------------------*/
(define (fprin port . obj)
   (for-each (lambda (o) (display o port)) obj))

