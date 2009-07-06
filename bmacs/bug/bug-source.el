;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-source.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 14 11:06:05 2002                          */
;*    Last change :  Tue May 21 09:35:57 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    This package is in charge of the display (highlight) of          */
;*    source code lines. In particular, it is in charge of             */
;*    displaying an arrow to prompt the source line the execution      */
;*    has stopped at.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-source)
(require 'bug-config)
(require 'bug-custom)
(require 'bug-connect)
(require (if (featurep 'xemacs) 'bug-xemacs 'bug-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    bug-find-file ...                                                */
;*    -------------------------------------------------------------    */
;*    Find a file for bug. Here we can add special keymap if we        */
;*    whish to.                                                        */
;*---------------------------------------------------------------------*/
(defun bug-find-file (f)
  (interactive "Ffile: ")
  (if (file-exists-p f)
      (save-excursion
	(let ((buffer (find-file-noselect f)))
	  (if (bufferp buffer) (bug-connect-buffer buffer))
	  buffer))
    (let ((lst   (buffer-list))
	  (match (concat f "$"))
	  (buf   '()))
      (while (consp lst)
	(let* ((b (car lst))
	       (bname (buffer-file-name b)))
	  (if (and (stringp bname) (string-match match bname))
	      (progn
		(bug-connect-buffer buf)
		(setq buf b)
		(setq lst '()))
	    (setq lst (cdr lst)))))
      buf)))

;*---------------------------------------------------------------------*/
;*    bug-highlight-line ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-highlight-line (true-file line)
  (let* ((buffer (save-excursion (bug-find-file true-file)))
	 (window (and buffer
		      (if bug-display-source-in-frame-p
			  (let ((pop-up-frames t))
			    (display-buffer buffer))
			(display-buffer buffer)))))
    (if buffer
	(progn
	  (bug-connect-buffer buffer)
	  (bug-show-line window buffer line)
	  (if bug-raise-active-source-frame-p
	      (let ((frame (window-frame window)))
		(if (framep frame)
		    (raise-frame frame)))))
      nil)))

;*---------------------------------------------------------------------*/
;*    bug-display-file-line ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-display-file-line (true-file line)
  (let ((buffer (bug-find-file true-file)))
    (if buffer
	(let ((window (if bug-display-source-in-frame-p
			  (let ((pop-up-frames t))
			    (display-buffer buffer))
			(display-buffer buffer))))
	  (set-buffer buffer)
	  (bug-connect-buffer buffer)
	  (goto-line line)
	  (set-window-point window (point))
	  (if bug-raise-active-source-frame-p
	      (let ((frame (window-frame window)))
		(if (framep frame)
		    (raise-frame frame))))))))

;*---------------------------------------------------------------------*/
;*    bug-prompt-file-line ...                                         */
;*    -------------------------------------------------------------    */
;*    This function is called when a source line must be prompted. It  */
;*    particular this function is called by the bugloo filter when a   */
;*    source location is raised.                                       */
;*---------------------------------------------------------------------*/
(defun bug-prompt-file-line (file line)
  (let ((buffer (bug-find-file file)))
    (if buffer
	(progn
	  (bug-highlight-line file line)
	  (set-buffer bug-comint-buffer)
	  (let ((height (window-height)))
	    (if (and (not bug-display-source-in-frame-p)
		     (> height bug-window-height)
		     (consp (window-list))
		     (consp (cdr (window-list))))
		(shrink-window (- height bug-window-height))))
	  (set-buffer buffer)))))
