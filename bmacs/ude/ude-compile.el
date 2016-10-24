;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-compile.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 27 10:41:34 1998                          */
;*    Last change :  Mon Jul  5 12:01:40 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ude compilation                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-compile)
(require (if (featurep 'xemacs) 'font-lock 'compile))
(require 'ude-autoload)
(require 'ude-custom)
(require 'ude-icon)
(require 'ude-config)
(require 'font-lock)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-compile-initialized-p ...                                    */
;*    -------------------------------------------------------------    */
;*    Is ude-compile initialized for that buffer?                      */
;*---------------------------------------------------------------------*/
(defvar ude-compile-initialized-p nil)
(make-variable-buffer-local 'ude-compile-initialized-p)

;*---------------------------------------------------------------------*/
;*    compilation-font-lock-keywords ...                               */
;*---------------------------------------------------------------------*/
(defvar compilation-font-lock-keywords '())
(defvar compilation-font-lock-keywords-tmp '())


;*---------------------------------------------------------------------*/
;*    ude-compile-init ...                                             */
;*    -------------------------------------------------------------    */
;*    The Ude compilation setup. This function simply add the          */
;*    hooking to compile and transforms some variables into buffer     */
;*    local ones. This function does not set any value that is         */
;*    relative to one specific mode.                                   */
;*---------------------------------------------------------------------*/
(defun ude-compile-init ()
  (if (not ude-compile-initialized-p)
      (progn
	(setq ude-compile-initialized-p t)
	;; we set the goto-error funtion
	(fset 'compilation-goto-locus
	      (if (featurep 'xemacs)
		  'ude-compilation-goto-locus
		'ude-compilation-goto-locus-gnu-emacs))
	;; we prepare the hook for toolbar setting
	(add-hook 'compilation-mode-hook 'ude-compilation-init-toolbar)
	(add-hook 'compilation-mode-hook 'ude-set-compilation-modeline)
	;; we make variable for error regexp and keywords local variable
	;; so programming mode are able to use their own value for these
	;; variables
	(make-variable-buffer-local 'compilation-error-regexp-alist)
	(make-variable-buffer-local 'compilation-font-lock-keywords)
	;; the finish function
	(custom-set-variables
	 '(compilation-finish-function 'ude-compilation-finish-function)))))

;*---------------------------------------------------------------------*/
;*    ude-compile-p ...                                                */
;*    -------------------------------------------------------------    */
;*    This variable act as a mark. It is used with UDE-COMPILE and     */
;*    the handlers associated to UDE-COMPILE.                          */
;*---------------------------------------------------------------------*/
(defvar ude-compile-p nil)

;*---------------------------------------------------------------------*/
;*    ude-last-compile-buffer ...                                      */
;*    -------------------------------------------------------------    */
;*    This variable holds the last compilation buffer. It this buffer  */
;*    is still live when a new compilation is spawn that buffer will   */
;*    be used.                                                         */
;*---------------------------------------------------------------------*/
(defvar ude-last-compile-buffer '()
  "The buffer used by the last compilation.")

;*---------------------------------------------------------------------*/
;*    ude-success-hook ...                                             */
;*    -------------------------------------------------------------    */
;*    Function to be called when compilation succeed (e.g. from the    */
;*    profiler).                                                       */
;*---------------------------------------------------------------------*/
(defvar ude-success-hook '()
  "A function to be called (once) on success.
Executed hooks are removed from that list before being executed.")

;*---------------------------------------------------------------------*/
;*    Buffer local compile variables ...                               */
;*---------------------------------------------------------------------*/
(defvar ude-compile-command nil)
(make-variable-buffer-local 'ude-compile-command)

(defvar ude-makefile-entries nil)
(make-variable-buffer-local 'ude-makefile-entries)

(defvar ude-makefile nil)
(make-variable-buffer-local 'ude-makefile)

(defvar ude-external-bmake-process '()
  "The external Ude bmake process")

(defvar ude-compilation-frame-left nil)
(defvar ude-compilation-frame-top nil)

;*---------------------------------------------------------------------*/
;*    Non buffer local variable to hold the ude-root-directory of the  */
;*    buffer that as requested a compilation.                          */
;*---------------------------------------------------------------------*/
(defvar ude-compile-root-directory nil)
(defvar ude-compile-compile-mode nil)

;*---------------------------------------------------------------------*/
;*    ude-compile ...                                                  */
;*    -------------------------------------------------------------    */
;*    We have to mark that the compilation as been requested by UDE.   */
;*    the consequence will be that a correct compilation will see      */
;*    its buffer to disapear after 5 seconds and an incorrect          */
;*    compilation will be signaled with a frame popup or a sound       */
;*    playing or a window enlargement                                  */
;*---------------------------------------------------------------------*/
(defun ude-compile ()
  (interactive)
  ;; arm the ude compilation
  (setq ude-compile-p t)
  ;; initialize the compile package
  (ude-compile-init)
  ;; start the real compilation process
  (setq ude-compile-root-directory ude-root-directory)
  (setq ude-compile-compile-mode ude-compile-mode)
  ;; do process the compilation
  (if (or (and (not (ude-detached-compile-buffer-p))
	       (consp (cdr (window-list))))
	  ude-always-detach-compile-buffer)
      (ude-frame-compile)
    (ude-window-compile)))

;*---------------------------------------------------------------------*/
;*    ude-detached-compile-buffer-p ...                                */
;*    -------------------------------------------------------------    */
;*    Is the compilation buffer detached.                              */
;*---------------------------------------------------------------------*/
(defun ude-detached-compile-buffer-p ()
  (or ude-always-detach-compile-buffer
      (and (bufferp ude-last-compile-buffer)
	   (ude-one-frame-buffer-p ude-last-compile-buffer))))

;*---------------------------------------------------------------------*/
;*    ude-window-compile ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-window-compile ()
  (let* ((win        (if (bufferp ude-last-compile-buffer)
			 (get-buffer-window ude-last-compile-buffer t)
		       nil))
	 (new-height (if (windowp win)
			 (window-height win)
		       ude-compilation-window-height))
	 (compilation-window-height new-height)
	 (compilation-read-command nil)
	 (compile-command ude-compile-command)
	 (frame (if (windowp win) (window-frame win))))
    ;; we raise the compilation window before compiling
    (if (framep frame)
	(progn
	  (ude-grab-compilation-frame-configuration frame)
	  (raise-frame frame)))
    (if (eq (aref ude-root-directory (- (length ude-root-directory) 1)) ?/)
	(let ((default-directory ude-root-directory))
	  (call-interactively 'compile))
      (let ((default-directory (concat ude-root-directory "/")))
	(call-interactively 'compile)))))

;*---------------------------------------------------------------------*/
;*    ude-frame-compile ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-frame-compile ()
  ;; if there is a frame for compilation, we raise it
  (let* ((win (if (bufferp ude-last-compile-buffer)
		  (get-buffer-window ude-last-compile-buffer t)
		nil))
	 (frame (if (windowp win) (window-frame win))))
    ;; we raise the compilation window before compiling
    (if (framep frame)
	(progn
	  (ude-grab-compilation-frame-configuration frame)
	  (raise-frame frame))))
  ;; the we start the regular compilation stuff
  (let* ((default-frame-alist (ude-compilation-frame-alist))
	 (compilation-read-command nil)
	 (compile-command ude-compile-command)
	 (pop-up-frames t))
    (if (eq (aref ude-root-directory (- (length ude-root-directory) 1)) ?/)
	(let ((default-directory ude-root-directory))
	  (call-interactively 'compile))
      (let ((default-directory (concat ude-root-directory "/")))
	(call-interactively 'compile)))))

;*---------------------------------------------------------------------*/
;*    ude-compile-compile ...                                          */
;*    -------------------------------------------------------------    */
;*    Change to the proper ude-root-directory then compile.            */
;*---------------------------------------------------------------------*/
(defun ude-compile-compile ()
  (interactive)
  (let ((ude-root-directory ude-compile-root-directory)
	(ude-compile-mode ude-compile-compile-mode))
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-compile-clean ...                                            */
;*    -------------------------------------------------------------    */
;*    Change to the proper ude-root-directory then compile.            */
;*---------------------------------------------------------------------*/
(defun ude-compile-clean ()
  (interactive)
  (let ((ude-root-directory ude-compile-root-directory)
	(ude-compile-mode ude-compile-compile-mode))
    (if (yes-or-no-p-dialog-box (format "Clean up directory %s" ude-root-directory))
	(let ((old ude-compile-command))
	  (setq ude-compile-command (concat ude-make " clean"))
	  (ude-compile)
	  (setq ude-compile-command old))
      (message "Clean aborted."))))

;*---------------------------------------------------------------------*/
;*    ude-compile-edit-makefile ...                                    */
;*    -------------------------------------------------------------    */
;*    Change to the proper ude-root-directory then compile.            */
;*---------------------------------------------------------------------*/
(defun ude-compile-edit-makefile ()
  (interactive)
  (let ((ude-root-directory ude-compile-root-directory)
	(ude-compile-mode ude-compile-compile-mode))
    (ude-edit-makefile)))

;*---------------------------------------------------------------------*/
;*    ude-compile-update-makefile ...                                  */
;*    -------------------------------------------------------------    */
;*    Change to the proper ude-root-directory then compile.            */
;*---------------------------------------------------------------------*/
(defun ude-compile-update-makefile ()
  (interactive)
  (let ((ude-root-directory ude-compile-root-directory)
	(ude-compile-mode ude-compile-compile-mode))
    (ude-update-makefile)))

;*---------------------------------------------------------------------*/
;*    ude-mode-compile ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-mode-compile (err-regexp fl-keyword)
  (interactive)
  (let ((compilation-error-regexp-alist (cons
					 err-regexp
					 compilation-error-regexp-alist))
	(compilation-font-lock-keywords (append
					 fl-keyword
					 compilation-font-lock-keywords)))
    (setq compilation-font-lock-keywords-tmp compilation-font-lock-keywords)
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-mode-compile-from-menu ...                                   */
;*---------------------------------------------------------------------*/
(defun ude-mode-compile-from-menu ()
  (interactive)
  (if (functionp ude-mode-menu-compile)
      (funcall ude-mode-menu-compile)
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-mode-jcompile-from-menu ...                                  */
;*---------------------------------------------------------------------*/
(defun ude-mode-jcompile-from-menu ()
  (interactive)
  ;; use a dynamic variable in order to change the value of the
  ;; UDE-COMPILE-COMMAND variable
  (let ((ude-compile-command ude-jcompile-command))
    (if (functionp ude-mode-menu-compile)
	(funcall ude-mode-menu-compile)
      (ude-compile))))

;*---------------------------------------------------------------------*/
;*    ude-set-compile-command ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-set-compile-command (string)
  (interactive "sCompile command: ")
  (setq ude-compile-command string)) 

;*---------------------------------------------------------------------*/
;*    ude-get-compile-command ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-get-compile-command ()
  ude-compile-command)

;*---------------------------------------------------------------------*/
;*    ude-success-hook ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-success-hook (fun)
  (setq ude-success-hook fun))

;*---------------------------------------------------------------------*/
;*    ude-compilation-finish-function ...                              */
;*    -------------------------------------------------------------    */
;*    This function is invoke each time a compilation complete. The    */
;*    first check is to discover if the compilation has been requested */
;*    by Ude. In the negative, we don't process anything.              */
;*---------------------------------------------------------------------*/
(defun ude-compilation-finish-function (buffer msg)
  (if ude-compile-p
      (progn
	(setq font-lock-defaults '(compilation-font-lock-keywords-tmp t))
	(font-lock-fontify-buffer)
	(setq ude-last-compile-buffer buffer)
	;; MS: 5 jul 2010, commented out
	;; (compile-error-at-point)
	(if (string-match "abnormally" msg)
	    ;; an error has occured
	    (ude-compilation-abort buffer msg)
	  ;; is compilation completed without error
	  (ude-compilation-succeed buffer msg))
	(setq ude-compile-p nil))))

;*---------------------------------------------------------------------*/
;*    ude-compilation-abort ...                                        */
;*    -------------------------------------------------------------    */
;*    This function is called when a compilation has failed. We        */
;*    process according to the user configuration.                     */
;*---------------------------------------------------------------------*/
(defun ude-compilation-abort (buffer msg)
  (set-buffer buffer)
  ;; if the user has configured a sound for compilation error
  ;; we produce an error
  (if ude-sound-compilation-on-error (ude-error "compile: %s" msg))
  ;; first we check if we have to popup a frame or
  ;; to enlarge a window
  (cond
   (ude-pop-compilation-frame-on-error
    ;; we have to pop up a frame
    (let* ((pop-up-frames t)
	   (height (save-excursion
		     (set-buffer buffer)
		     (+ 2 (count-lines (point-min) (point-max)))))
	   (cur-height (frame-height (selected-frame)))
	   (err-height (if (> height cur-height) cur-height height))
	   (default-frame-alist (ude-compilation-frame-alist))
	   (window (get-buffer-window buffer)))
      (if (windowp window)
	  (if (not (one-window-p window))
	      (progn
		(delete-window window)
		(pop-to-buffer buffer))))))
   (ude-enlarge-compilation-window-on-error
    ;; we have to enlarge the compilation height to its emacs default value
    (let ((window (get-buffer-window buffer)))
      (if (windowp window)
	  (let ((max-size (if (numberp compilation-window-height)
			      compilation-window-height
			    (/ (frame-height (selected-frame)) 2)))
		(height (window-height window)))
	    (if (< height max-size)
		(let ((o-window (selected-window)))
		  (select-window window)
		  (enlarge-window (- max-size height))
		  (select-window o-window)))))))))

;*---------------------------------------------------------------------*/
;*    ude-compilation-all-done-p ...                                   */
;*    -------------------------------------------------------------    */
;*    Are all the current compilation done?                            */
;*---------------------------------------------------------------------*/
(defun ude-compilation-all-done-p (lst)
  (let ((res t))
    (while (consp lst)
      (let ((proc (car lst)))
	(if (and (processp proc)
		 (eq (process-status proc) 'run))
	    (progn
	      (setq res nil)
	      (setq lst nil))
	  (setq lst (cdr lst)))))
    res))

;*---------------------------------------------------------------------*/
;*    ude-compilation-succeed ...                                      */
;*    -------------------------------------------------------------    */
;*    This function is called for Ude compilation that succeed.        */
;*---------------------------------------------------------------------*/
(defun ude-compilation-succeed (buffer msg)
  (let ((window (get-buffer-window buffer t)))
    (if (functionp ude-success-hook)
	(let ((hook ude-success-hook))
	  (setq ude-success-hook nil)
	  (funcall hook buffer msg)))
    (if ude-delete-compilation-window-on-success
	(run-with-timer 5 nil
			'ude-compilation-close-success-window
			buffer window))
    (message "compilation completed..."))
    t)

;*---------------------------------------------------------------------*/
;*    ude-compilation-close-success-window ...                         */
;*---------------------------------------------------------------------*/
(defun ude-compilation-close-success-window (buffer window)
  (if (and (windowp window)
	   (or (null compilation-in-progress)
	       (ude-compilation-all-done-p compilation-in-progress)))
      (if (and (ude-detached-compile-buffer-p)
	       (ude-one-frame-buffer-p buffer))
	  (condition-case nil
	      (let* ((old (selected-frame))
		     (frame (window-frame window)))
		(select-frame frame)
		(ude-compilation-delete-frame-or-quit)
		(if (not (eq old frame))
		    (select-frame old)))
	    (error t))
	(delete-window window))))

;*---------------------------------------------------------------------*/
;*    ude-view-last-compile-messages ...                               */
;*---------------------------------------------------------------------*/
(defun ude-view-last-compile-messages ()
  "Pops up the last compilation buffer.
If such a buffer does not exists, raise an error."
  (interactive)
  (if (and (bufferp ude-last-compile-buffer)
	   (buffer-live-p ude-last-compile-buffer))
      (progn
	(set-buffer ude-last-compile-buffer)
	(let* ((pop-up-frames t)
	       (height (+ 2 (count-lines (point-min) (point-max))))
	       (cur-height (frame-height (selected-frame)))
	       (new-height (if (> height cur-height) cur-height height))
	       (default-frame-alist (ude-compilation-frame-alist)))
	  (goto-char (point-min))
	  (pop-to-buffer ude-last-compile-buffer)))
    (ude-error "compilation buffer does not exists!")))
  
;*---------------------------------------------------------------------*/
;*    ude-compile-messages-available-p ...                             */
;*---------------------------------------------------------------------*/
(defun ude-compile-messages-available-p ()
  "Is there a compilation buffer?"
  (interactive)
  (and (bufferp ude-last-compile-buffer)
       (buffer-live-p ude-last-compile-buffer)))

;*---------------------------------------------------------------------*/
;*    ude-compilation-goto-locus ...    .                              */
;*    -------------------------------------------------------------    */
;*    Emacs lisp code is always so well written. Really sometimes I    */
;*    don't understand. I'm not able to find a way to pop up an        */
;*    error buffer from compile without modification the source        */
;*    code of compile.el!                                              */
;*---------------------------------------------------------------------*/
(defun ude-compilation-goto-locus (next-error)
  "Jump to an error locus returned by `compilation-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR."
  (let* ((pop-up-frames t)
	 (compilation-buffer (marker-buffer (car next-error)))
	 (source-buffer (marker-buffer (cdr next-error)))
	 ;; make sure compilation buffer is visible ...
	 (compilation-window
	 ;; Use an existing window if it is in a visible frame.
	  (or (get-buffer-window compilation-buffer 'visible)
	      ;; Pop up a window.
	      (pop-to-buffer compilation-buffer))))

    ;; now, make the compilation buffer **STAY WHERE IT IS** and
    ;; make sure the source buffer is visible
    (select-window compilation-window)
    (pop-to-buffer source-buffer)

    ;; now put things aright in the compilation window.
    (set-window-point compilation-window (car next-error))
    (set-window-start compilation-window (car next-error))
    (compilation-set-window-height compilation-window)

    ;; now put things aright in the source window.
    (set-buffer source-buffer)
    (goto-char (cdr next-error))
    
    ;; If narrowing got in the way of
    ;; going to the right place, widen.
    (or (= (point) (marker-position (cdr next-error)))
	(progn
	  (widen)
	  (goto-char (cdr next-error))))))

;*---------------------------------------------------------------------*/
;*    ude-compilation-goto-locus-gnu-emacs ...                         */
;*---------------------------------------------------------------------*/
(defun ude-compilation-goto-locus-gnu-emacs (msg mk end-mk)
  "Jump to an error locus returned by `compilation-next-error-locus'.
Takes one argument, a cons (ERROR . SOURCE) of two markers.
Selects a window with point at SOURCE, with another window displaying ERROR."
  (let* ((next-error (cons msg mk))
         (pop-up-frames t)
	 (compilation-buffer (marker-buffer (car next-error)))
	 (source-buffer (marker-buffer (cdr next-error)))
	 ;; make sure compilation buffer is visible ...
	 (compilation-window
	 ;; Use an existing window if it is in a visible frame.
	  (or (get-buffer-window compilation-buffer 'visible)
	      ;; Pop up a window.
	      (pop-to-buffer compilation-buffer))))

    ;; now, make the compilation buffer **STAY WHERE IT IS** and
    ;; make sure the source buffer is visible
    (select-window compilation-window)
    (pop-to-buffer source-buffer)

    ;; now put things aright in the compilation window.
    (set-window-point compilation-window (car next-error))
    (set-window-start compilation-window (car next-error))
    (compilation-set-window-height compilation-window)

    ;; now put things aright in the source window.
    (set-buffer source-buffer)
    (goto-char (cdr next-error))
    
    ;; If narrowing got in the way of
    ;; going to the right place, widen.
    (or (= (point) (marker-position (cdr next-error)))
	(progn
	  (widen)
	  (goto-char (cdr next-error))))))

;*---------------------------------------------------------------------*/
;*    ude-compilation-attached-toolbar ...                             */
;*---------------------------------------------------------------------*/
(defvar ude-compilation-attached-toolbar
  `( ;; the quit button
    (,ude-quit-icon delete-window-or-quit "Close Compile Window")
    --

    ;; the detach button
    (,ude-compile-detach-icon ude-detach-compile-buffer "Pop a new frame for Compile Buffer")
    --
    
    ;; edit makefile
    (,ude-edit-makefile-icon ude-compile-edit-makefile "Edit Makefile")
    --

    ;; the stop button
    (,ude-stop-icon ude-kill-compilation "Abort Compilation")
    --
    
    ;; next error
    (,ude-error-icon ude-next-error "Jump to Next Error")
    --
    
    ;; the compile button
    (,ude-compile-icon ude-compile-compile "Compile")
    ;; the make-make button
    (,ude-make-make-icon ude-compile-update-makefile "Update Makefile")
    --

    ;; the execution button
    (,ude-execute-icon ude-execute "Execute")
    --
    
    ;; flushing right
    -->
    --
    ;; the help action
    (,ude-pref-icon ude-compile-pref "Preferences")
    (,ude-help-icon describe-mode "Help")))

;*---------------------------------------------------------------------*/
;*    ude-compilation-detached-toolbar ...                             */
;*---------------------------------------------------------------------*/
(defvar ude-compilation-detached-toolbar 
  `( ;; the quit button
    (,ude-quit-icon ude-compilation-delete-frame-or-quit "Close Compilation Frame")
    --
    
    ;; edit makefile
    (,ude-edit-makefile-icon ude-compile-edit-makefile "Edit Makefile")
    --

    ;; next error
    (,ude-error-icon ude-next-error "Jump to Next Error")
    --
    
    ;; the stop button
    (,ude-stop-icon ude-kill-compilation "Abort Compilation")
    --
    
    ;; the compile button
    (,ude-compile-icon ude-compile-compile "Compile")
    ;; the compile button
    (,ude-make-make-icon ude-compile-update-makefile "Update Makefile")
    --

    ;; the execution button
    (,ude-execute-icon ude-execute "Execute")
    --
    
    ;; make clean
    (,ude-clean-icon ude-compile-clean "Clean root directory")
    --
    
    ;; flushing right
    -->
    --
    ;; the help action
    (,ude-pref-icon ude-compile-pref "Preferences")
    (,ude-help-icon describe-mode "Help")))

;*---------------------------------------------------------------------*/
;*    ude-compilation-toolbar ...                                      */
;*---------------------------------------------------------------------*/
(defvar ude-compilation-toolbar ude-compilation-attached-toolbar)
  
;*---------------------------------------------------------------------*/
;*    ude-compilation-init-toolbar ...                                 */
;*    -------------------------------------------------------------    */
;*    This hook simply set the UDE compilation toolbar for the buffer  */
;*---------------------------------------------------------------------*/
(defun ude-compilation-init-toolbar ()
  (setq ude-compilation-toolbar
	(if (ude-detached-compile-buffer-p)
	    ude-compilation-detached-toolbar
	    ude-compilation-attached-toolbar))
  (ude-toolbar-set ude-compilation-toolbar))
  
;*---------------------------------------------------------------------*/
;*    ude-detach-compile-buffer ...                                    */
;*    -------------------------------------------------------------    */
;*    This function popups detach a compilation buffer, that is        */
;*    pops a new frame contain the only compilation buffer.            */
;*---------------------------------------------------------------------*/
(defun ude-detach-compile-buffer ()
  (interactive)
  (let ((buffer (if compilation-in-progress
		    compilation-last-buffer
		  ude-last-compile-buffer)))
    (if (and (bufferp buffer) (not (ude-one-frame-buffer-p buffer)))
	;; the compilation buffer
	(progn
	  (delete-windows-on buffer)
	  (setq ude-compilation-toolbar ude-compilation-detached-toolbar)
	  (ude-toolbar-set ude-compilation-toolbar)
	  (let ((default-frame-alist (ude-compilation-frame-alist)))
	    (switch-to-buffer-other-frame buffer)
	    ())))))

;*---------------------------------------------------------------------*/
;*    ude-compile-makefile-entry ...                                   */
;*---------------------------------------------------------------------*/
(defun ude-compile-makefile-entry (entry)
  (let ((mkmf (concat ude-root-directory ude-makefile)))
    (if (file-exists-p mkmf)
	(let* ((res (shell-command-to-string (format "cd %s; %s -f %s %s"
						     ude-root-directory
						     ude-make
						     ude-makefile
						     entry))))
	  (if (and (stringp res)
		   (not (string= res ""))
		   (not (string-match ude-make-error-output res)))
	      res
	    ""))
	"")))

;*---------------------------------------------------------------------*/
;*    ude-compile-last-entry ...                                       */
;*---------------------------------------------------------------------*/
(defvar ude-compile-last-entry nil)
(make-variable-buffer-local 'ude-compile-last-entry)

;*---------------------------------------------------------------------*/
;*    ude-compile-menu ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-compile-menu ()
  (let ((entries (ude-fetch-makefile-entries))
	(mkmf    (concat ude-root-directory ude-makefile))
	(mode    (ude-fetch-makefile-mode-entry)))
    (if (stringp mode)
	(setq ude-compile-mode (intern mode)))
    (if (and (file-exists-p mkmf) (consp entries))
	(progn
	  (if (not (stringp ude-compile-last-entry))
	      (setq ude-compile-last-entry (car entries)))
	  (list
	   (list (let ((string "Compilation"))
		   (insert-text-property 0 5
					 'face 'font-lock-function-name-face
					 string)
		   string)
		 (cons (concat ude-make "...")
		       (mapcar
			#'(lambda (entry)
			   (vector entry
				   (if (equal entry ude-compile-last-entry)
				       'ude-mode-compile-from-menu
				     (list 'progn
					   (list 'setq
						 'ude-compile-last-entry
						 entry)
					   (list 'ude-set-compile-command
						 (concat ude-make " " entry))
					   '(ude-mode-compile-from-menu)))
				   t))
			entries))
		 ["Execute" ude-execute t]
		 ["Kill compilation" kill-compilation compilation-in-progress]
		 ["View last messages" ude-view-last-compile-messages
		 (ude-compile-messages-available-p)]
		 '("Development mode"
		   ["Debug" ude-makefile-debug-mode
		   :style toggle :selected (eq ude-compile-mode 'debug)]
		   ["Devel" ude-makefile-devel-mode
		   :style toggle :selected (eq ude-compile-mode 'devel)]
		   ["Final" ude-makefile-final-mode
		   :style toggle :selected (eq ude-compile-mode 'final)]))))
      (list
       (list "Compilation..."
	     (vector (ude-string-excerpt (ude-get-compile-command))
		     'ude-mode-compile-from-menu
		     t)
	     ["Execute" ude-execute t]
	     ["Kill compilation" kill-compilation compilation-in-progress]
	     ["Set compile command" ude-set-compile-command t]
	     '("Development mode"
	       ["Debug" ude-makefile-debug-mode
	       :style toggle :selected (eq ude-compile-mode 'debug)]
	       ["Devel" ude-makefile-devel-mode
	       :style toggle :selected (eq ude-compile-mode 'devel)]
	       ["Final" ude-makefile-final-mode
	       :style toggle :selected (eq ude-compile-mode 'final)]))))))

;*---------------------------------------------------------------------*/
;*    ude-execute-default-args ...                                     */
;*---------------------------------------------------------------------*/
(defvar ude-execute-default-args "")

;*---------------------------------------------------------------------*/
;*    ude-execute ...                                                  */
;*---------------------------------------------------------------------*/
(defun ude-execute (arg)
  (interactive
   (let ((arg (read-string (format "Run argument: [%s] "
				   ude-execute-default-args))))
     (list (if (and (stringp arg) (> (length arg) 0))
	       arg
	     ude-execute-default-args))))
  ;; we remember the run argument for later profile
  (setq ude-execute-default-args arg)
  ;; the run for profiling
  (let* ((ude-compile-command (format "%s %s"
				      (ude-fetch-makefile-binary-entry)
				      arg)))
    (ude-compile))) 

;*---------------------------------------------------------------------*/
;*    ude-compilation-delete-frame-or-quit ...                         */
;*---------------------------------------------------------------------*/
(defun ude-compilation-delete-frame-or-quit ()
  (interactive)
  (ude-grab-compilation-frame-configuration (selected-frame))
  (delete-frame-or-quit))

;*---------------------------------------------------------------------*/
;*    ude-kill-compilation ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-kill-compilation ()
  (interactive)
  (ude-grab-compilation-frame-configuration (selected-frame))
  (kill-compilation))

;*---------------------------------------------------------------------*/
;*    ude-next-error ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-next-error ()
  (interactive)
  (ude-grab-compilation-frame-configuration (selected-frame))
  (next-error))

;*---------------------------------------------------------------------*/
;*    ude-grab-compilation-frame-configuration ...                     */
;*---------------------------------------------------------------------*/
(defun ude-grab-compilation-frame-configuration (frame)
  (let ((height (frame-parameter frame 'height))
	(left (frame-parameter frame 'left))
	(top (+ ude-compilation-frame-top-correct
		(eval (frame-parameter frame 'top)))))
    (if height (setq ude-compilation-frame-height height))
    (if left (setq ude-compilation-frame-left left))
    (if top (setq ude-compilation-frame-top (if (< top 0) 0 top)))))

;*---------------------------------------------------------------------*/
;*    ude-compilation-frame-alist ...                                  */
;*---------------------------------------------------------------------*/
(defun ude-compilation-frame-alist ()
  (let* ((alist0 default-frame-alist)
	 (alist1 (if (numberp ude-compilation-frame-height)
		     (cons (cons 'height ude-compilation-frame-height)
			   alist0)
		   alist0))
	 (alist2 (if (numberp ude-compilation-frame-left)
		     (cons (cons 'left ude-compilation-frame-left)
			   alist1)
		   alist1))
	 (alist3 (if (numberp ude-compilation-frame-top)
		     (cons (cons 'top ude-compilation-frame-top)
			   alist2)
		   alist2)))
    alist3))
		     
;*---------------------------------------------------------------------*/
;*    ude-set-compilation-modeline ...                                 */
;*    -------------------------------------------------------------    */
;*    This function sets the modeline according to the compile mode.   */
;*---------------------------------------------------------------------*/
(defun ude-set-compilation-modeline ()
  (ude-set-root-modeline))

;*---------------------------------------------------------------------*/
;*    ude-compile-pref ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-compile-pref ()
  (interactive)
  (popup-menu
   (list "Preferences"
	 ["Auto-close on success" ude-compile-autoclose-toggle
	 :style toggle :selected ude-delete-compilation-window-on-success])))

;*---------------------------------------------------------------------*/
;*    ude-compile-autoclose-toggle ...                                 */
;*---------------------------------------------------------------------*/
(defun ude-compile-autoclose-toggle ()
  (interactive)
  (setq ude-delete-compilation-window-on-success
	(not ude-delete-compilation-window-on-success)))

