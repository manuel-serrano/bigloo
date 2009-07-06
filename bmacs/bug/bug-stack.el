;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-stack.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 22 12:04:08 2002                          */
;*    Last change :  Thu May 23 14:22:47 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Bugloo stack printing                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bug-stack)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'comint)
(require 'ude-custom)
(require 'bug-config)
(require 'bug-custom)
(require 'bug-autoload)
(require 'bug-filter)

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; this variable holds a state: are we currently inquirying for stacks
(defvar bug-stack-command-armed-p nil)

;; the buffer for stack
(defvar bug-stack-buffer nil)

;; display in frame or window
(defvar bug-stack-in-frame-p t)

;*---------------------------------------------------------------------*/
;*    bug-stack-hook ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-stack-hook ()
  (if (not bug-stack-command-armed-p)
      (progn
	(setq bug-stack-command-armed-p t)
	(if (not (bufferp bug-stack-buffer))
	    (setq bug-stack-buffer (bug-make-stack-buffer "*Bugloo stack*"))
	  (bug-popup-stack bug-stack-buffer))
	(bug-hook-command (bug-stack-hook-command bug-stack-depth)
			  bug-stack-buffer)
	(setq bug-stack-command-armed-p nil))))

;*---------------------------------------------------------------------*/
;*    bug-make-stack-buffer ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-make-stack-buffer (name)
  (bug-make-hook-buffer name
			bug-stack-in-frame-p
			bug-stack-depth
			'bug-stack-mode))
	
;*---------------------------------------------------------------------*/
;*    bug-popup-stack ...                                              */
;*    -------------------------------------------------------------    */
;*    If the frame/window displaying a stack buffer has been killed,   */
;*    we popup a fresh one.                                            */
;*---------------------------------------------------------------------*/
(defun bug-popup-stack (buffer)
  (let ((window (get-buffer-window buffer 0)))
    (if (not (windowp window))
	(bug-make-stack-buffer (buffer-name buffer)))))

;*---------------------------------------------------------------------*/
;*    The stack mode map                                               */
;*---------------------------------------------------------------------*/
(defvar bug-stack-mode-map (make-sparse-keymap))
(define-key bug-stack-mode-map ude-mouse-binding 'bug-stack-mode-menu)
(define-key bug-stack-mode-map "\C-x\C-c" 'bug-stack-quit)

;*---------------------------------------------------------------------*/
;*    bug-stack-mode ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-stack-mode ()
  "Major mode for bug frames.

\\{bug-stack-mode-map}"
  (setq major-mode 'bug-stack-mode)
  (setq mode-name "Bug Stack")
  (setq buffer-read-only t)
  (setq comint-prompt-regexp bug-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (use-local-map bug-stack-mode-map)
  (suppress-keymap bug-stack-mode-map)
  (if (one-window-p) (frame-getrid-toolbar (selected-frame))))
  
;*---------------------------------------------------------------------*/
;*    bug-stack-mode-menu ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-stack-mode-menu (event)
  (interactive "e")
  (popup-menu (list "bug stack"
		    "--:shadowEtchedOut"
		    (vector "Close stack frame display"
			    '(bug-stack-quit)
			    t))))

;*---------------------------------------------------------------------*/
;*    bug-stack-start ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-stack-start (framep)
  (interactive)
  (setq bug-stack-in-frame-p framep)
  (if (not (bug-installed-hook-p 'bug-stack-hook))
      (progn
	(bug-add-command-hook 'bug-stack-hook)
	(save-excursion
	  (bug-stack-hook)))))

;*---------------------------------------------------------------------*/
;*    bug-stack-quit ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-stack-quit ()
  (interactive)
  (if (bufferp bug-stack-buffer) (bug-delete-window-or-frame bug-stack-buffer))
  (setq bug-stack-buffer nil)
  (bug-remove-command-hook 'bug-stack-hook))

;*---------------------------------------------------------------------*/
;*    bug-stack-toggle ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-stack-toggle ()
  (interactive)
  (if (not (bug-installed-hook-p 'bug-stack-hook))
      (bug-stack-start t)
    (bug-stack-quit)))
