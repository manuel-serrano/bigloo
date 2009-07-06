;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-profile.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  1 11:57:44 1999                          */
;*    Last change :  Wed Jan 23 17:02:15 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between the CEE and a visual profiler.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-profile)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'cee-autoload)
(require 'cee-config)
(require 'ude-autoload)
(require 'ude-config)
(require 'plugin)

;*---------------------------------------------------------------------*/
;*    cee-profiler-ready-p ...                                         */
;*    -------------------------------------------------------------    */
;*    This true predicate returns true iff an interface builder        */
;*    is ready. That is if the binary file for the interface           */
;*    builder exists and may be executed.                              */
;*---------------------------------------------------------------------*/
(defun cee-profiler-ready-p ()
  (and (stringp cee-profiler)
       (file-exists-p cee-profiler)
       (file-executable-p cee-profiler)))

;*---------------------------------------------------------------------*/
;*    cee-find-profiler ...                                            */
;*    -------------------------------------------------------------    */
;*    Finds a profiler associated to the project. If such a profiler   */
;*    does not exist, returns '()                                      */
;*---------------------------------------------------------------------*/
(defun cee-find-profiler (project)
  (plugin-find-process-from-key cee-profiler project))

;*---------------------------------------------------------------------*/
;*    cee-profiler-start ...                                           */
;*    -------------------------------------------------------------    */
;*    This function start a new interface profiler. It first creates   */
;*    a module name and a file name and launch a new process.          */
;*---------------------------------------------------------------------*/
(defun cee-profiler-start ()
  (interactive)
  (cee-run-profiler))

;*---------------------------------------------------------------------*/
;*    cee-run-profiler ...                                             */
;*    -------------------------------------------------------------    */
;*    This functions spawn a new profiler process.                     */
;*---------------------------------------------------------------------*/
(defun cee-run-profiler ()
  (if (cee-profiler-ready-p)
      (cee-run-external-profiler)
    (ude-error "Can't find profiler")))

;*---------------------------------------------------------------------*/
;*    cee-run-external-profiler ...                                    */
;*---------------------------------------------------------------------*/
(defun cee-run-external-profiler ()
  (let* ((frame     (selected-frame))
	 (emacs-opt '()))
    ;; background color
    (let ((bg (face-foreground-name 'text-cursor)))
      (if (stringp bg)
	  (setq emacs-opt
		(cons "-bg" (cons bg emacs-opt)))))
    ;; toolbar color
    (let ((tl (frame-toolbar-background frame)))
      (if (stringp tl)
	  (setq emacs-opt
		(cons "-tl" (cons tl emacs-opt)))))
    ;; active background
    (let ((abg (face-background-name 'highlight)))
      (if (stringp abg)
	  (setq emacs-opt
		(cons "-ac" (cons abg emacs-opt)))))
    ;; modeline background
    (let ((mbg (face-background-name 'modeline)))
      (if (stringp mbg)
	  (setq emacs-opt
		(cons "-ml" (cons mbg emacs-opt)))))
    ;; modeline foreground
    (let ((mfg (face-foreground-name 'modeline-buffer-id)))
      (if (stringp mfg)
	  (setq emacs-opt
		(cons "-mf" (cons mfg emacs-opt)))))
    ;; modeline buffer id
    (let ((mid (face-foreground-name 'modeline-mousable)))
      (if (stringp mid)
	  (setq emacs-opt
		(cons "-mi" (cons mid emacs-opt)))))
    ;; modeline project
    (let ((mid (face-foreground-name 'ude-modeline-root-face)))
      (if (stringp mid)
	  (setq emacs-opt
		(cons "-mp" (cons mid emacs-opt)))))
    ;; font
    (let ((font (face-font-name 'text-cursor)))
      (if (stringp font)
	  (setq emacs-opt
		(cons "-fn" (cons font emacs-opt)))))
    ;; modeline font
    (let ((font (face-font-name 'modeline)))
      (if (stringp font)
	  (setq emacs-opt
		(cons "-fmn" (cons font emacs-opt)))))
    ;; width
    (let ((width (frame-pixel-width (selected-frame))))
      (if (numberp width)
	  (setq emacs-opt (cons "-width"
				(cons (number-to-string width)
				      emacs-opt)))))
    ;; height
    (let ((height (frame-pixel-height (selected-frame))))
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
	   (opts    (cons a.out (append cee-profiler-cee-options
					emacs-opt))))
      (condition-case err
	  (make-plugin cee-profiler
		       opts
		       (function cee-profiler-callback)
		       nil
		       ude-root-directory)
	(error
	 (if (stringp (car (cdr err)))
	     (ude-error (car (cdr err)))
	   (ude-error "Can't start profiler")))))))

;*---------------------------------------------------------------------*/
;*    cee-profile-requesting-process ...                               */
;*---------------------------------------------------------------------*/
(defvar cee-profile-requesting-process nil)

;*---------------------------------------------------------------------*/
;*    cee-profiler-callback ...                                        */
;*---------------------------------------------------------------------*/
(defun cee-profiler-callback (proc command)
  (cond
   ((not (consp command))
    '())
   ((equal command '(COMPILE))
    (ude-compile-for-profile)
    t)
   ((equal command '(EXTRA-COMPILE))
    (ude-compile-for-extra-profile)
    t)
   ((equal command '(CLEAN-PROFILE))
    (ude-compile-for-clean-profile)
    t)
   ((eq (car command) 'RECORD)
    (setq cee-profile-requesting-process proc)
    (ude-run-for-profile (car (cdr command)))
    t)
   ((eq (car command) 'EDIT)
    (let ((def (car (cdr command))))
      (cond
       ((string-match "[^:]+:\\([^:]+\\):\\([0-9]+\\)" def)
	;; this is a lambda
	(let* ((fname (substring def (match-beginning 1) (match-end 1)))
	       (pos   (substring def (match-beginning 2) (match-end 2)))
	       (buf   (get-buffer fname)))
	  (if (bufferp buf)
	      (let ((win (get-buffer-window buf)))
		(if (not (windowp win))
		    (setq buf (switch-to-buffer-other-frame fname))
		  (let ((frame (window-frame win)))
		    (if (not (framep frame))
			(setq buf (switch-to-buffer-other-frame fname))
		      (raise-frame frame)))))
	    (setq buf (switch-to-buffer-other-frame fname)))
	  (if (bufferp buf)
	      (progn
		(set-buffer buf)
		(goto-char (string-to-number pos))))))
       (t
	(plugin-error proc
		      ":Can't find global definition for variable -- "
		      def))))
    t)
   (t '())))

;*---------------------------------------------------------------------*/
;*    cee-profile-success-hook ...                                     */
;*---------------------------------------------------------------------*/
(defun cee-profile-success-hook (buffer msg)
  (if (cee-profiler-ready-p)
      (cee-external-profile-success buffer msg)))

;*---------------------------------------------------------------------*/
;*    cee-external-profile-success ...                                 */
;*---------------------------------------------------------------------*/
(defun cee-external-profile-success (buffer msg)
  (if (processp cee-profile-requesting-process)
      (plugin-send-string cee-profile-requesting-process
			   (format "(record-completed %S)" msg))))
  
;*---------------------------------------------------------------------*/
;*    cee-external-profiler-inspect ...                                */
;*---------------------------------------------------------------------*/
(defun cee-external-profiler-inspect (ident)
  (let ((proc (cee-find-profiler ude-root-directory)))
    (if (and (processp proc) (eq (process-status proc) 'run))
	(plugin-send-string proc (format "(inspect %S)" ident))
      (message "No running profiler."))))

;*---------------------------------------------------------------------*/
;*    cee-profiler-inspect ...                                         */
;*---------------------------------------------------------------------*/
(defun cee-profiler-inspect ()
  "Inspect function"
  (interactive)
  (let* ((pos (point))
	 (ident (ude-fetch-identifier pos)))
    (if (stringp ident)
	(cee-external-profiler-inspect ident)
      (let ((ident (read-string "Inspect function: ")))
	(if (not (string= ident ""))
	    (cee-external-profiler-inspect ident))))))

