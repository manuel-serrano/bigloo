;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/setrc.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 07:53:05 1996                          */
;*    Last change :  Fri May 24 17:39:30 2013 (serrano)                */
;*    Copyright   :  1992-2013 Manuel Serrano, see LICENSE file        */
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
			(list home *default-lib-dir*)
			*default-lib-dir*)))
	  (fname (find-file/path ".bigloorc" path)))
      (if fname
	  (loadq fname))))



