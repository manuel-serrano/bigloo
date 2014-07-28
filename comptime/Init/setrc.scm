;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/setrc.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 07:53:05 1996                          */
;*    Last change :  Sun Jul 27 06:39:03 2014 (serrano)                */
;*    Copyright   :  1992-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reading of the `runtime-command' file.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module init_setrc
   (import engine_param
      tools_speek)
   (export (setup-default-values)))

;*---------------------------------------------------------------------*/
;*    setup-default-values ...                                         */
;*---------------------------------------------------------------------*/
(define (setup-default-values)
   (let* ((path  (let ((home (getenv "HOME")))
		    (if (string? home)
			(cons home *lib-dir*)
			*lib-dir*)))
	  (fname (find-file/path ".bigloorc" path)))
      (when fname (loadq fname))))



