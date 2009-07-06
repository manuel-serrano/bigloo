;*=====================================================================*/
;*    serrano/prgm/project/bdk/kbdb/src/Init/setrc.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul 11 11:49:35 1995                          */
;*    Last change :  Wed Jun  7 07:01:08 2000 (serrano)                */
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
   (if (file-exists? *bdb-rc*)
       (loadq *bdb-rc*)
       (let ((home (getenv "HOME")))
	  (if (string? home)
	      (begin
		 (let ((bdb-rc (string-append home "/" *bdb-rc*)))
		    (if (file-exists? bdb-rc)
			(loadq bdb-rc)
			'done)))))))


		     



