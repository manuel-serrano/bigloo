;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-hooking.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 23 11:40:46 2002                          */
;*    Last change :  Fri May 24 09:27:41 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    General facilities for Bugloo hooking.                           */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-hooking)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-custom)
(require 'bug-config)
(require 'bug-filter)

;*---------------------------------------------------------------------*/
;*    Global variables                                                 */
;*---------------------------------------------------------------------*/
(defvar bug-command-hooks '())
(defvar bug-command-hook-p nil)

(defvar bug-hook-in-progress nil)

;*---------------------------------------------------------------------*/
;*    bug-add-command-hook ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-add-command-hook (hook)
  (setq bug-command-hooks (cons hook bug-command-hooks)))

;*---------------------------------------------------------------------*/
;*    bug-remove-command-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun bug-remove-command-hook (hook)
  (setq bug-command-hooks (delq hook bug-command-hooks)))

;*---------------------------------------------------------------------*/
;*    bug-run-command-hooks ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-run-command-hooks ()
  (if (and (not bug-command-hook-p) (consp bug-command-hooks))
      (progn
	(setq bug-command-hook-p t)
	(redisplay-frame)
	(sit-for 0.2)
	(bug-wait-process "hooks" bug-hook-timeout)
	(mapcar (lambda (hook) (funcall hook)) bug-command-hooks)
	(redisplay-frame)
	(setq bug-command-hook-p nil))))

;*---------------------------------------------------------------------*/
;*    bug-installed-hook-p ...                                         */
;*    -------------------------------------------------------------    */
;*    Is a hook all set?                                               */
;*---------------------------------------------------------------------*/
(defun bug-installed-hook-p (hook)
  (memq hook bug-command-hooks))

;*---------------------------------------------------------------------*/
;*    bug-hook-command ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-hook-command (cmd buffer)
  (interactive)
  (let ((proc (get-buffer-process bug-comint-buffer)))
    ;; we wait for comint to be ready
    (sit-for 0.1)
    (if (bug-wait-process "bug-hook-command" bug-hook-timeout)
	(save-excursion
	  (set-buffer buffer)
	  (let ((buffer-read-only nil))
	    (erase-buffer)
	    ;; Temporarily install our filter function.
	    (let ((old-filter-output bug-filter-output)
		  (old-prompt-output bug-prompt-output)
		  (old-prompt-hook bug-prompt-hook))
	      (setq bug-filter-output
		    #'(lambda (proc str)
			(save-excursion
			  (set-buffer buffer)
			  (goto-char (point-max))
			  (insert str))))
	      (setq bug-prompt-hook
		    #'(lambda () (setq bug-hook-in-progress nil)))
	      (setq bug-prompt-output
		    #'(lambda (proc str) nil))
	      ;; mark the hook
	      (setq bug-hook-in-progress t)
	      ;; Issue the command to BUG.
	      (bug-silent-remote-call cmd)
	      ;; Slurp the output.
	      (while bug-hook-in-progress
		(accept-process-output proc bug-sec-timeout bug-msec-timeout))
	      ;; restore the old filtering functions
	      (setq bug-filter-output old-filter-output)
	      (setq bug-prompt-hook old-prompt-hook)
	      (setq bug-prompt-output old-prompt-output)))))))

;*---------------------------------------------------------------------*/
;*    bug-make-hook-buffer ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-make-hook-buffer (name frame-p height mode)
  (let ((buf (get-buffer-create name)))
    (if frame-p
	(bug-make-hook-frame-buffer buf height mode)
      (bug-make-hook-window-buffer buf height mode))))

;*---------------------------------------------------------------------*/
;*    bug-make-hook-frame-buffer ...                                   */
;*---------------------------------------------------------------------*/
(defun bug-make-hook-frame-buffer (buf height mode)
  (let ((height (if (numberp height) (+ height 3) 10)))
    (let ((default-frame-alist (ude-default-frame-alist 'height height)))
      (switch-to-buffer-other-frame buf)
      (set-buffer buf)
      (funcall mode)
      buf)))

;*---------------------------------------------------------------------*/
;*    bug-make-hook-window-buffer ...                                  */
;*---------------------------------------------------------------------*/
(defun bug-make-hook-window-buffer (buf height mode)
  (set-buffer bug-comint-buffer)
  (split-window (selected-window))
  (let ((buffer (switch-to-buffer-other-window buf)))
    (set-buffer buffer)
    (let ((window (selected-window)))
      (if (and (numberp height) (> (window-height window) height))
	  (shrink-window (- (window-height window) height)))
      (funcall mode)
      (current-buffer))))

