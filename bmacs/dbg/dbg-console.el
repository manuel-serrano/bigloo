;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-console.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Aug 14 09:20:08 1998                          */
;*    Last change :  Fri Feb  7 08:50:13 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file is for debugging because I'm not able to understand    */
;*    the causality used by comint. I made this small file to trace    */
;*    what dbg is waiting for.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-console)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    dbg-console-buffer ...                                           */
;*---------------------------------------------------------------------*/
(defvar dbg-console-buffer nil
  "*The buffer for the dbg console or nil if the console as
not been launched yet.")

;*---------------------------------------------------------------------*/
;*    dbg-console-get-buffer ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-console-get-buffer ()
  (setq dbg-console-buffer (get-buffer-create "*dbg console*")))

;*---------------------------------------------------------------------*/
;*    dbg-pop-console ...                                              */
;*    -------------------------------------------------------------    */
;*    Raise the dbg console buffer or create one if none exists.       */
;*---------------------------------------------------------------------*/
(defun dbg-pop-console ()
  (interactive)
  (let ((buffer (current-buffer)))
    (if (bufferp dbg-console-buffer)
	(let ((win (get-buffer-window dbg-console-buffer t)))
	  (if (not (windowp win))
	      (pop-to-buffer dbg-console-buffer)))
      (progn
	(dbg-console-get-buffer)
	(pop-to-buffer dbg-console-buffer)))
    (set-buffer buffer)))

;*---------------------------------------------------------------------*/
;*    dbg-console-log-number ...                                       */
;*---------------------------------------------------------------------*/
(defvar dbg-console-log-number 0)

;*---------------------------------------------------------------------*/
;*    dbg-console-log ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-console-log (prompt msg &optional face)
  (save-window-excursion
    ;; we first allocate a buffer for the logs
    (if (not (bufferp dbg-console-buffer))
	(dbg-console-get-buffer))
    (setq dbg-console-log-number (+ 1 dbg-console-log-number))
    (if face
	(insert-text-property 0 (length msg) 'face face msg))
    (set-buffer dbg-console-buffer)
    (goto-char (point-max))
    (let ((num (format "%s (log num %d):\n" prompt dbg-console-log-number)))
      (insert num))
    (insert msg)
    (insert "\n\n"))
  (let ((win (get-buffer-window dbg-console-buffer)))
    (if (windowp win)
	(set-window-point win (point-max)))))

;*---------------------------------------------------------------------*/
;*    dbg-erase-console ...                                            */
;*---------------------------------------------------------------------*/
(defun dbg-erase-console ()
  (interactive)
  (set-buffer dbg-console-buffer)
  (erase-buffer))

;*---------------------------------------------------------------------*/
;*    dbg-recenter-console ...                                         */
;*---------------------------------------------------------------------*/
(defun dbg-recenter-console ()
  (interactive)
  (let ((win (get-buffer-window dbg-console-buffer)))
    (if (windowp win)
	(set-window-point win (point-max)))))

  
  

