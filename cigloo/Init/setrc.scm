;*=====================================================================*/
;*    serrano/prgm/project/cigloo/Init/setrc.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 11:49:35 1995                          */
;*    Last change :  Fri Dec  1 10:08:13 1995 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The runtime-command file loading.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_setrc
   (import engine_param)
   (export (setup-default-values)))

;*---------------------------------------------------------------------*/
;*    setup-default-values ...                                         */
;*---------------------------------------------------------------------*/
(define (setup-default-values)
   (if (file-exists? ".cigloorc")
       (loadq ".cigloorc")
       (let ((home (getenv "HOME")))
	  (if (string? home)
	      (begin
		 (let ((cigloorc (string-append home "/.cigloorc")))
		    (if (file-exists? cigloorc)
			(loadq cigloorc)
			'done)))))))


		     



