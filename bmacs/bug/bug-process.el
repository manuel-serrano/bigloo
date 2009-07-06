;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-process.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:34:44 2002                          */
;*    Last change :  Fri May 24 09:27:26 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Bigloo Debugger process management                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-process)
(require 'comint)
(require 'bug-config)
(require 'bug-hooking)

;*---------------------------------------------------------------------*/
;*    bug-send-input ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-send-input ()
  (interactive)
  (comint-send-input)
  (redisplay-frame))

;*---------------------------------------------------------------------*/
;*    bug-remote-call ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-remote-call (command)
  (if (functionp bug-wrapper-caller)
      (funcall bug-wrapper-caller command)
    (if bug-verbose-remote
	(bug-verbose-remote-call command)
      (bug-silent-remote-call command))))

;*---------------------------------------------------------------------*/
;*    bug-verbose-remote-call ...                                      */
;*    -------------------------------------------------------------    */
;*    This function is used for sending a bug call from a remote       */
;*    buffer.                                                          */
;*---------------------------------------------------------------------*/
(defun bug-verbose-remote-call (command)
  (if (not (bufferp bug-comint-buffer))
      (ude-error "No debugger running...")
    (progn
      (set-buffer bug-comint-buffer)
      (let ((proc (get-buffer-process bug-comint-buffer)))
	(goto-char (process-mark proc))
	(delete-region (point) (point-max))
	(insert command)
	(goto-char (point-max))
	(bug-send-input)
	(goto-char (point-max))))))

;*---------------------------------------------------------------------*/
;*    bug-silent-remote-call ...                                       */
;*---------------------------------------------------------------------*/
(defun bug-silent-remote-call (command)
  (message "Command: %s" command)
  (sit-for 0)
  (let ((proc (get-buffer-process bug-comint-buffer)))
    (or proc (ude-error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer bug-comint-buffer)
      (goto-char (process-mark proc))
      (delete-region (point) (point-max))
      (process-send-string proc (concat command "\n")))))

;*---------------------------------------------------------------------*/
;*    bug-waiting ...                                                  */
;*---------------------------------------------------------------------*/
(defvar bug-waiting nil)

;*---------------------------------------------------------------------*/
;*    bug-wait-process ...                                             */
;*    -------------------------------------------------------------    */
;*    We sit for the prompt is printed. This function is called        */
;*    by the send hooks.                                               */
;*---------------------------------------------------------------------*/
(defun bug-wait-process (from timeout)
  (if (not bug-comint-buffer)
      (ude-error "No debugger running")
    (save-excursion
      (save-restriction
	(widen)
	(let ((count 0)
	      (str   "/-\\|")
	      (l (length bug-prompt-regexp)))
	  (setq str str)
	  (set-buffer bug-comint-buffer)
	  ;; we mark bug waiting
	  (setq bug-waiting t)
	  (accept-process-output)
	  (goto-char (point-max))
	  (while (progn
		   (goto-char (point-max))
		   (or (<= (point) l)
		       (and (save-excursion
			      (backward-char l)
			      (not (re-search-forward bug-prompt-eol-regexp
						      (point-max)
						      t)))
			    (or (not (numberp timeout))
				(< count timeout)))))
	    (accept-process-output)
	    (setq count (+ 1 count))
	    (if (numberp timeout)
		(display-message 'no-log
				 (format "waiting for `%s' [%c] (timeout %d/%S)"
					 from
					 (aref str (% count 4))
					 count
					 timeout))
	      (display-message 'no-log
			       (format "waiting for `%s' [%c]"
				       from
				       (aref str (% count 4)))))
	    (sit-for 0.3)
	    (redisplay-frame (selected-frame) t))
	  ;; we remove the last wait message
	  (display-message 'no-log " ")
	  ;; bug is now ready
	  (setq bug-waiting nil)
	  (or (not (numberp timeout))
	      (< count timeout)))))))

