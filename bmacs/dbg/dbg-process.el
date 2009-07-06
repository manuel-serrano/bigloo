;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-process.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 19 15:08:24 1998                          */
;*    Last change :  Tue Nov 24 19:49:10 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The dbg process management.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-process)
(require 'dbg-config)
(require 'dbg)

;*---------------------------------------------------------------------*/
;*    dbg-sentinel ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function is called when the dbg running process changes     */
;*    of state. This means that the process has been killed or         */
;*    stopped or anything else like that.                              */
;*---------------------------------------------------------------------*/
(defun dbg-sentinel (proc msg)
  (cond
   ((null (buffer-name (process-buffer proc)))
    ;; the buffer has been killed. We stop displaying
    ;; arrow in the source files.
    (set-process-buffer proc nil))
   ((or (eq (process-status proc) 'signal)
	(not (eq (process-exit-status proc) 0)))
    (let ((msg (format "KILLING dbg because debugger %s" msg)))
      (ude-error msg)
      (message-box msg)
      (dbg-cleanup-dbg)))
   ((eq (process-status proc) 'exit)
    (dbg-cleanup-dbg))))

;*---------------------------------------------------------------------*/
;*    dbg-cleanup-dbg ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-cleanup-dbg ()
  (let ((window (get-buffer-window dbg-comint-buffer t)))
    (if (one-window-p window)
	(let ((frame (window-frame window)))
	  (delete-frame frame))))
  (dbg-undisplay-source-line)
  (kill-buffer dbg-comint-buffer)
  (setq dbg-comint-buffer nil))

	  
