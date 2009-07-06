;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-profile.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  1 11:57:44 1999                          */
;*    Last change :  Tue Sep 20 08:35:36 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between the BEE and a visual profiler.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-profile)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-autoload)
(require 'bee-config)
(require 'ude-autoload)
(require 'ude-config)
(require 'ude-profile)
(require 'plugin)

;*---------------------------------------------------------------------*/
;*    bee-profiler-ready-p ...                                         */
;*    -------------------------------------------------------------    */
;*    This true predicate returns true iff an interface builder        */
;*    is ready. That is if the binary file for the interface           */
;*    builder exists and may be executed.                              */
;*---------------------------------------------------------------------*/
(defun bee-profiler-ready-p ()
  (and (stringp bee-profiler)
       (file-exists-p bee-profiler)
       (file-executable-p bee-profiler)))

;*---------------------------------------------------------------------*/
;*    bee-find-profiler ...                                            */
;*    -------------------------------------------------------------    */
;*    Finds a profiler associated to the project. If such a profiler   */
;*    does not exist, returns '()                                      */
;*---------------------------------------------------------------------*/
(defun bee-find-profiler (project)
  (plugin-find-process-from-key bee-profiler project))

;*---------------------------------------------------------------------*/
;*    bee-profiler-start ...                                           */
;*    -------------------------------------------------------------    */
;*    This function start a new interface profiler. It first creates   */
;*    a module name and a file name and launch a new process.          */
;*---------------------------------------------------------------------*/
(defun bee-profiler-start ()
  (interactive)
  (bee-run-profiler))

;*---------------------------------------------------------------------*/
;*    bee-run-profiler ...                                             */
;*    -------------------------------------------------------------    */
;*    This functions spawn a new profiler process.                     */
;*---------------------------------------------------------------------*/
(defun bee-run-profiler ()
  (if (bee-profiler-ready-p)
      (bee-run-external-profiler)
    (bee-run-internal-profiler)))

;*---------------------------------------------------------------------*/
;*    bee-run-internal-profiler ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-run-internal-profiler ()
  (add-hook 'ude-profile-load-hooks
	     #'(lambda () (bee-profile-highlight-buffer (current-buffer))))
  (let ((buffer (ude-load-profile-file nil "")))
    (if (bufferp buffer)
	(bee-profile-highlight-buffer buffer)
      (let ((buffer (ude-create-profile-buffer)))
	(if (bufferp buffer)
	    (let ((pop-up-frames t))
	      (pop-to-buffer buffer)))))))

;*---------------------------------------------------------------------*/
;*    bee-profile-root-directory ...                                   */
;*---------------------------------------------------------------------*/
(defvar bee-profile-root-directory ".")

;*---------------------------------------------------------------------*/
;*    bee-run-external-profiler ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-run-external-profiler ()
  (let ((frame (selected-frame))
	(emacs-opt (plugin-color-configuration)))
    ;; width
    (let ((width (frame-pixel-width frame)))
      (if (numberp width)
	  (setq emacs-opt (cons "-width"
				(cons (number-to-string width)
				      emacs-opt)))))
    ;; height
    (let ((height (frame-pixel-height frame)))
      (if (numberp height)
	  (setq emacs-opt (cons "-height"
				(cons (number-to-string
				       (round (/ (* 2 height) 3)))
				      emacs-opt)))))
    ;; regular arguments
    (let* ((a.out   (let ((name (ude-fetch-makefile-binary_p-entry)))
		      (if (stringp name)
			  name
			"a.out")))
	   (opts    (cons a.out
			  (cons "-root"
				(cons ude-root-directory
				      (append bee-profiler-bee-options
					      emacs-opt))))))
      (condition-case err
	  (make-plugin bee-profiler
		       opts
		       (function bee-profiler-callback)
		       nil
		       ude-root-directory)
	(error
	 (if (stringp (car (cdr err)))
	     (ude-error (car (cdr err)))
	   (ude-error "Can't start profiler")))))))

;*---------------------------------------------------------------------*/
;*    bee-profile-requesting-process ...                               */
;*---------------------------------------------------------------------*/
(defvar bee-profile-requesting-process nil)

;*---------------------------------------------------------------------*/
;*    bee-profiler-callback ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-profiler-callback (proc command)
  ;; we parse the command
  (cond
   ((not (consp command))
    '())
   ((memq (car command) '(COMPILE compile))
    (let ((ude-root-directory (car (cdr command))))
      (ude-compile-for-profile)
      t))
   ((memq (car command) '(EXTRA-COMPILE extra-compile))
    (let ((ude-root-directory (car (cdr command))))
      (ude-compile-for-extra-profile)
      t))
   ((memq (car command) '(CLEAN-PROFILE clean-profile))
    (let ((ude-root-directory (car (cdr command))))
      (ude-compile-for-clean-profile)
      t))
   ((memq (car command) '(RECORD record))
    (let ((ude-root-directory (car (cdr command))))
      (setq bee-profile-requesting-process proc)
      (ude-run-for-profile (car (cdr (cdr command))))
      t))
   ((memq (car command) '(EDIT edit))
    (let ((def  (car (cdr (cdr command))))
	  (root (car (cdr command))))
      (cond
       ((eq (aref def 0) ?@)
	;; this is a module we want to edit
	(let ((str (substring def 1 (length def))))
	  (cond
	   ((string-match "(toplevel)\\(.+\\)" str)
	    (bee-find-module (substring str
					(match-beginning 1)
					(match-end 1))))
	   (t
	    (bee-find-module str)))))
       ((string-match "[^:]+:\\([^:]+\\):\\([0-9]+\\)" def)
	;; this is a lambda
	(let* ((fname (concat root
			      (substring def
					 (match-beginning 1)
					 (match-end 1))))
	       (pos   (substring def (match-beginning 2) (match-end 2)))
	       (npos  (string-to-number pos))
	       (buf   (find-buffer-visiting fname)))
	  (if (bufferp buf)
	      (let ((win (get-buffer-window buf t))
		    (bname (buffer-name buf)))
		(if (not (windowp win))
		    (setq buf (switch-to-buffer-other-frame bname))
		  (let ((frame (window-frame win)))
		    (if (not (framep frame))
			(setq buf (switch-to-buffer-other-frame bname))
		      (raise-frame frame)
		      (select-frame frame)
		      (select-window win)
		      (set-buffer buf)
		      (goto-char npos)
		      (set-window-point win npos)))))
	    (setq buf (progn
			(find-file-other-frame fname)
			(current-buffer))))
	  (if (bufferp buf)
	      (progn
		(set-buffer buf)
		(goto-char npos)))))
       (t
	(if (not (bee-tags-find-if-exists (point-min) def))
	    (plugin-error proc
			  ":Can't find global definition for variable -- "
			  def)))))
    t)
   (t '())))

;*---------------------------------------------------------------------*/
;*    bee-profile-success-hook ...                                     */
;*---------------------------------------------------------------------*/
(defun bee-profile-success-hook (buffer msg)
  (if (bee-profiler-ready-p)
      (bee-external-profile-success buffer msg)
    (bee-load-profile-file buffer msg)))

;*---------------------------------------------------------------------*/
;*    bee-external-profile-success ...                                 */
;*---------------------------------------------------------------------*/
(defun bee-external-profile-success (buffer msg)
  (if (processp bee-profile-requesting-process)
      (plugin-send-string bee-profile-requesting-process
			   (format "(record-completed %S)" msg))))
  
;*---------------------------------------------------------------------*/
;*    bee-load-profile-file ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-load-profile-file (buffer msg)
  (let ((buffer (ude-load-profile-file buffer msg)))
    (if (bufferp buffer)
	(bee-profile-highlight-buffer buffer))))
  
;*---------------------------------------------------------------------*/
;*    doc source keymap                                                */
;*---------------------------------------------------------------------*/
(defvar bee-profile-mouse-map (make-sparse-keymap))
(define-key bee-profile-mouse-map ude-mouse-2-binding
  (function bee-profile-find))

;*---------------------------------------------------------------------*/
;*    bee-profile-highlight-buffer ...                                 */
;*---------------------------------------------------------------------*/
(defun bee-profile-highlight-buffer (buffer)
  (set-buffer buffer)
  (let* ((scm-id-regexp "^[][0-9./ \t]+\\([A-Z_!?+.0-9@#$%^&*/<>-]+\\)[ \t]*\\(\\[[0-9]+\\]\\)?$")
	 (scm-mod-regexp "^[][0-9./ \t]+@\\([A-Z_!?+.0-9@#$%^&*/<>-]+\\)[ \t]*\\(\\[[0-9]+\\]\\)?$")
	 (c-regexp "^[][0-9./ \t]+[a-zA-Z_0-9]+[ t]*\\(\\[[0-9]+\\]\\)?$")
	 (union-regexp (concat "\\(" scm-id-regexp "\\)\\|\\(" c-regexp "\\)"))
	 (case-fold-search nil))
    (goto-char (point-min))
    (while (re-search-forward union-regexp (point-max) t)
      (goto-char (match-beginning 0))
      (let ((end (match-end 0)))
	(cond
	 ((re-search-forward scm-mod-regexp end t)
	  ;; a path addition
	  (let ((ident (buffer-substring (match-beginning 1) (match-end 1))))
	    (put-text-properties (match-beginning 0) (match-end 0)
				 'face bee-module-profile-face
				 'prof (cons 'module ident)
				 'mouse-face 'highlight
				 'keymap bee-profile-mouse-map)))
	 ((re-search-forward scm-id-regexp end t)
	  ;; a path addition
	  (let ((ident (buffer-substring (match-beginning 1) (match-end 1))))
	    (put-text-properties (match-beginning 0) (match-end 0)
				 'face bee-ident-profile-face
				 'prof (cons 'var ident)
				 'mouse-face 'highlight
				 'keymap bee-profile-mouse-map)))
	 ((re-search-forward c-regexp end t)
	  ;; a C function
	  (put-text-properties (match-beginning 0) (match-end 0)
			       'face 'bee-c-profile-face)) 
	 (t
	  (ude-error (format "Illegal match %S" end))))
	(goto-char end)))
    (goto-char (point-min))))

;*---------------------------------------------------------------------*/
;*    bee-profile-find ...                                             */
;*---------------------------------------------------------------------*/
(defun bee-profile-find (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event))
	 (prop   (find-text-property point 'prof buffer)))
    (cond
     ((and (consp prop) (eq (car prop) 'var))
      (bee-find-definition (cdr prop)))
     ((and (consp prop) (eq (car prop) 'module))
      (bee-find-module (cdr prop))))))

;*---------------------------------------------------------------------*/
;*    bee-external-profiler-inspect ...                                */
;*---------------------------------------------------------------------*/
(defun bee-external-profiler-inspect (ident)
  (let ((proc (bee-find-profiler ude-root-directory)))
    (if (and (processp proc) (eq (process-status proc) 'run))
	(plugin-send-string proc (format "(inspect %S)" (upcase ident)))
      (message "No running profiler."))))

;*---------------------------------------------------------------------*/
;*    bee-profiler-inspect ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-profiler-inspect ()
  "Inspect function"
  (interactive)
  (let* ((pos (point))
	 (ident (ude-fetch-identifier pos)))
    (if (stringp ident)
	(bee-external-profiler-inspect ident)
      (let ((ident (read-string "Inspect function: ")))
	(if (not (string= ident ""))
	    (bee-external-profiler-inspect ident))))))

