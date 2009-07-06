;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-args.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 22 12:04:08 2002                          */
;*    Last change :  Thu May 23 14:18:36 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Bugloo args printing                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bug-args)
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
;; this variable holds a state: are we currently inquirying for argss
(defvar bug-args-command-armed-p nil)

;; the buffer for args
(defvar bug-args-buffer nil)

;; display in frame or window
(defvar bug-args-in-frame-p t)

;*---------------------------------------------------------------------*/
;*    bug-args-hook ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-args-hook ()
  (if (not bug-args-command-armed-p)
      (progn
	(setq bug-args-command-armed-p t)
	(if (not (bufferp bug-args-buffer))
	    (setq bug-args-buffer (bug-make-args-buffer "*Bugloo args*"))
	  (bug-popup-args bug-args-buffer))
	(bug-hook-command (bug-args-hook-command) bug-args-buffer)
	(setq bug-args-command-armed-p nil))))

;*---------------------------------------------------------------------*/
;*    bug-make-args-buffer ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-make-args-buffer (name)
  (bug-make-hook-buffer name
			bug-args-in-frame-p
			bug-args-height
			'bug-args-mode))
	
;*---------------------------------------------------------------------*/
;*    bug-popup-args ...                                               */
;*    -------------------------------------------------------------    */
;*    If the frame/window displaying a args buffer has been killed,    */
;*    we popup a fresh one.                                            */
;*---------------------------------------------------------------------*/
(defun bug-popup-args (buffer)
  (let ((window (get-buffer-window buffer 0)))
    (if (not (windowp window))
	(bug-make-args-buffer (buffer-name buffer)))))

;*---------------------------------------------------------------------*/
;*    The args mode map                                                */
;*---------------------------------------------------------------------*/
(defvar bug-args-mode-map (make-sparse-keymap))
(define-key bug-args-mode-map ude-mouse-binding 'bug-args-mode-menu)
(define-key bug-args-mode-map "\C-x\C-c" 'bug-args-quit)

;*---------------------------------------------------------------------*/
;*    bug-args-mode ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-args-mode ()
  "Major mode for bug frames.

\\{bug-args-mode-map}"
  (setq major-mode 'bug-args-mode)
  (setq mode-name "Bug Args")
  (setq buffer-read-only t)
  (setq comint-prompt-regexp bug-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (use-local-map bug-args-mode-map)
  (suppress-keymap bug-args-mode-map)
  (if (one-window-p) (frame-getrid-toolbar (selected-frame))))
  
;*---------------------------------------------------------------------*/
;*    bug-args-mode-menu ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-args-mode-menu (event)
  (interactive "e")
  (popup-menu (list "bug args"
		    "--:shadowEtchedOut"
		    (vector "Close args frame display"
			    '(bug-args-quit)
			    t))))

;*---------------------------------------------------------------------*/
;*    bug-args-start ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-args-start (framep)
  (interactive)
  (setq bug-args-in-frame-p framep)
  (if (not (bug-installed-hook-p 'bug-args-hook))
      (progn
	(bug-add-command-hook 'bug-args-hook)
	(save-excursion
	  (bug-args-hook)))))

;*---------------------------------------------------------------------*/
;*    bug-args-quit ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-args-quit ()
  (interactive)
  (if (bufferp bug-args-buffer) (bug-delete-window-or-frame bug-args-buffer))
  (setq bug-args-buffer nil)
  (bug-remove-command-hook 'bug-args-hook))

;*---------------------------------------------------------------------*/
;*    bug-args-toggle ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-args-toggle ()
  (interactive)
  (if (not (bug-installed-hook-p 'bug-args-hook))
      (bug-args-start t)
    (bug-args-quit)))
