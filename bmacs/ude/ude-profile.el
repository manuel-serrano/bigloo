;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-profile.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 12 08:40:54 1998                          */
;*    Last change :  Sun Apr 10 09:59:53 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The BEE profiler.                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-profile)
(require 'ude-config)
(require 'ude-custom)
(require 'ude-autoload)
(require 'ude-icon)
(require 'ude-compile)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-compile-for-profile ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-compile-for-profile ()
  (interactive)
  (let ((ude-compile-command (format "%s -f %s %s"
				     ude-make
				     ude-makefile
				     ude-makefile-profile-entry))
	(ude-compile-mode 'prof))
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-compile-for-extra-profile ...                                */
;*---------------------------------------------------------------------*/
(defun ude-compile-for-extra-profile ()
  (interactive)
  (let ((ude-compile-command (format "%s -f %s %s"
				     ude-make
				     ude-makefile
				     ude-makefile-extra-profile-entry))
	(ude-compile-mode 'Xprof))
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-compile-for-clean-profile ...                                */
;*---------------------------------------------------------------------*/
(defun ude-compile-for-clean-profile ()
  (interactive)
  (let ((ude-compile-command (format "%s -f %s %s"
				     ude-make
				     ude-makefile
				     ude-makefile-clean-profile-entry)))
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-profile-default-args ...                                     */
;*---------------------------------------------------------------------*/
(defvar ude-profile-default-args "")

;*---------------------------------------------------------------------*/
;*    ude-global-profile-success-hook ...                              */
;*---------------------------------------------------------------------*/
(defvar ude-global-profile-success-hook nil)

;*---------------------------------------------------------------------*/
;*    ude-profile-load-hooks ...                                       */
;*---------------------------------------------------------------------*/
(defvar ude-profile-load-hooks nil)

;*---------------------------------------------------------------------*/
;*    ude-run-for-profile ...                                          */
;*---------------------------------------------------------------------*/
(defun ude-run-for-profile (arg)
  (interactive
   (let ((arg (read-string (format "Profiling run argument: [%s] "
				   ude-profile-default-args))))
     (list (if (and (stringp arg) (> (length arg) 0))
	       arg
	     ude-profile-default-args))))
  ;; we remember the buffer local highlighting function
  (setq ude-global-profile-success-hook ude-profile-success-hook)
  ;; we remember the run argument for later profile
  (setq ude-profile-default-args arg)
  ;; we setup success hook
  (ude-success-hook 'ude-profile-success)
  ;; the run for profiling
  (let* ((ude-compile-command (format "%s -f %s %s %s=\"%s\""
				      ude-make
				      ude-makefile
				      ude-makefile-run-profile-entry
				      ude-makefile-run-profile-args
				      arg))) 
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-open-profile ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-open-profile (fname)
  (interactive "Ffile name: ")
  (find-alternate-file fname)
  (ude-profile-init-toolbar)
  (run-hooks 'ude-profile-load-hooks))
  
;*---------------------------------------------------------------------*/
;*    ude-reload-profile ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-reload-profile ()
  (interactive)
  (ude-open-profile (buffer-name)))

;*---------------------------------------------------------------------*/
;*    ude-profile-toolbar ...                                          */
;*---------------------------------------------------------------------*/
(defvar ude-profile-toolbar 
  `(;; the quit button
    (,ude-quit-icon ude-tool-bar-delete-frame "Close Profile Frame")
    --

    ;; open profile button
    (,ude-dbg-file-icon ude-reload-profile "Reload Profile")
    (,ude-open-icon ude-open-profile "Open Profile")
    --

    ;; compilation
    (,ude-profile-compile-icon ude-compile-for-profile "Compile for profile")
    (,ude-profile-extra-compile-icon ude-compile-for-extra-profile "High profile Compile")
    --

    ;; clean
    (,ude-clean-icon ude-compile-for-clean-profile "Clean profile")
    --

    ;; the record button
    (,ude-record-icon ude-run-for-profile "Record execution")
    --

    ;; flushing right
    -->
    --
    ;; the help action
    (,ude-help-icon describe-mode "Help")))

;*---------------------------------------------------------------------*/
;*    ude-profile-success ...                                          */
;*---------------------------------------------------------------------*/
(defun ude-profile-success (buffer msg)
  (setq ude-profile-success-hook ude-global-profile-success-hook)
  (if (functionp ude-profile-success-hook)
      (funcall ude-profile-success-hook buffer msg)))
      
;*---------------------------------------------------------------------*/
;*    ude-load-profile-file ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-load-profile-file (buffer msg)
  ;; we load the PROF file
  (if (file-exists-p "PROF")
      ;; we have to fetch the function that highlight profile
      ;; buffer while we are in the source buffer because the
      ;; highlighting function is buffer local
      (let ((buffer (let ((buf (find-buffer-visiting "PROF")))
		      (if (bufferp buf)
			  (let ((win (get-buffer-window buf t)))
			    (if (windowp win)
				(progn
				  (select-window win)
				  (switch-to-buffer "*scratch*")))
			    (kill-buffer (buffer-name buf))
			    (find-alternate-file "PROF")
			    (current-buffer))
			(progn
			  (find-file-other-frame "PROF")
			  (current-buffer))))))
	(set-buffer buffer)
	(ude-profile-init-toolbar)
	buffer)
    t))

;*---------------------------------------------------------------------*/
;*    ude-create-profile-buffer ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-create-profile-buffer ()
  (let ((buffer (create-file-buffer "PROF")))
    (set-buffer buffer)
    (ude-profile-init-toolbar)
    buffer))
    
;*---------------------------------------------------------------------*/
;*    ude-profile-init-toolbar ...                                     */
;*    -------------------------------------------------------------    */
;*    This hook simply set the UDE profile toolbar for the buffer      */
;*---------------------------------------------------------------------*/
(defun ude-profile-init-toolbar ()
  (ude-toolbar-set ude-profile-toolbar))

  



