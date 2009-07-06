;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/signals.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 13 09:50:26 1994                          */
;*    Last change :  Fri Nov  5 15:02:07 2004 (serrano)                */
;*    Copyright   :  1994-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    On recupere tous les signaux pour ne plus avoir de message       */
;*    intenpestifs.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module engine_signals
   (import  init_main
	    engine_param)
   (export  (install-compiler-signals!)))

;*---------------------------------------------------------------------*/
;*    install-compiler-signals! ...                                    */
;*---------------------------------------------------------------------*/
(define (install-compiler-signals!)
   (let ((kill-my-self (lambda (n)
			  (fprint (current-error-port)
				  "*** INTERNAL-ERROR: Illegal signal caught"
				  " --- aborting..."
				  n)
			  (compiler-exit 2))))
      (signal sigfpe kill-my-self)
      (signal sigill kill-my-self)
      (signal sigbus kill-my-self)
      (signal sigsegv kill-my-self)))


