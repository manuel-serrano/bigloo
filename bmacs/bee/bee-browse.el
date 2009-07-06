;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-browse.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  1 11:57:44 1999                          */
;*    Last change :  Sat Aug 30 06:07:24 2003 (serrano)                */
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
(require 'plugin)

;*---------------------------------------------------------------------*/
;*    bee-browser-ready-p ...                                          */
;*    -------------------------------------------------------------    */
;*    This true predicate returns true iff an interface builder        */
;*    is ready. That is if the binary file for the interface           */
;*    builder exists and may be executed.                              */
;*---------------------------------------------------------------------*/
(defun bee-browser-ready-p ()
  (and (stringp bee-browser)
       (file-exists-p bee-browser)
       (file-executable-p bee-browser)))

;*---------------------------------------------------------------------*/
;*    bee-browser-start ...                                            */
;*    -------------------------------------------------------------    */
;*    This function start a new interface profiler. It first creates   */
;*    a module name and a file name and launch a new process.          */
;*---------------------------------------------------------------------*/
(defun bee-browser-start ()
  (interactive)
  (bee-run-browser))

;*---------------------------------------------------------------------*/
;*    bee-run-browser ...                                              */
;*    -------------------------------------------------------------    */
;*    This functions spawn a new browser process.                      */
;*---------------------------------------------------------------------*/
(defun bee-run-browser ()
  (if (bee-browser-ready-p)
      (bee-run-external-browser)
    (ude-error "No browser available")))

;*---------------------------------------------------------------------*/
;*    bee-run-external-browser ...                                     */
;*---------------------------------------------------------------------*/
(defun bee-run-external-browser ()
  (let* ((frame     (selected-frame))
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
				(cons (number-to-string height)
				      emacs-opt)))))
    ;; regular arguments
    (let* ((root  ude-root-directory)
	   (afile ".afile")
	   (etags ".etags")
	   (opts    (cons root
			  (cons afile
				(cons etags
				      (append bee-browser-bee-options
					      emacs-opt))))))
      (condition-case err
	  (let ((default-directory ude-root-directory))
	    (make-plugin bee-browser
			 opts
			 (function bee-browser-callback)
			 t))
	(error
	 (if (stringp (car (cdr err)))
	     (ude-error (car (cdr err)))
	   (ude-error "Can't start browser")))))))

;*---------------------------------------------------------------------*/
;*    bee-browser-callback ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-browser-callback (proc command)
  ;; fetch the correct root directory before anything else
  (ude-auto-set-root-directory bee-root)
  ;; we parse the command
  (cond
   ((not (consp command))
    '())
   ((memq (car command) '(EDIT edit))
    (let ((def (car (cdr command))))
      (cond
       ((eq (aref def 0) ?@)
	;; this is a module we want to edit
	(bee-find-module (substring def 1 (length def))))
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
	(if (not (bee-tags-find-if-exists (point-min) def))
	    (plugin-error proc
			  ":Can't find global definition for variable -- "
			  def)))))
    t)
   ((and (consp command) (memq (car command) '(EDIT-FILE-LINE edit-file-line)))
    (let* ((file  (cadr command))
	   (line  (car (cddr command)))
	   (aname (expand-file-name (concat ude-root-directory file))))
      (let ((buffer (find-buffer-visiting aname)))
	(if (not (and (bufferp buffer)
		      (let ((win (get-buffer-window buffer t)))
			(and (windowp win)
			     (let ((frame (window-frame win)))
			       (if (framep frame)
				   (progn
				     (raise-frame frame)
				     (select-frame frame)
				     (select-window win)
				     (set-buffer buffer)
				     (goto-line line)
				     (recenter)
				     t)
				 nil))))))
	    (let ((pop-up-frames t)
		  (buffer (find-file-noselect aname)))
	      (pop-to-buffer buffer)
	      (goto-line line)
	      (recenter)))))
    t)
   ((and (consp command) 
	 (memq (car command) '(FOOTPRINT
			       INSPECT-OTHER-WINDOW
			       INSPECT
			       footprint
			       inspect-other-window
			       inspect)))
    ;; When we spawn a bbrowse session for FOOTPRINT it may happens
    ;; that bbrowse is not ready when the FOOTPRINT command is emited.
    ;; In such a siutation FOOTPRINT is read by the plugin. To avoid
    ;; printing an error, we catch that message. One may notice that
    ;; this error can be safely hiden because eventually the FOOTPRINT
    ;; message reached bbrowse when it is ready.
    t)
   (t '())))

;*---------------------------------------------------------------------*/
;*    doc source keymap                                                */
;*---------------------------------------------------------------------*/
(defvar bee-profile-mouse-map (make-sparse-keymap))
(define-key bee-profile-mouse-map ude-mouse-2-binding
  (function bee-profile-find))

;*---------------------------------------------------------------------*/
;*    bee-browser-find-usage ...                                       */
;*    -------------------------------------------------------------    */
;*    Request a running browser to print usage information for         */
;*    IDENT. If no browser is running, spawn one.                      */
;*---------------------------------------------------------------------*/
(defun bee-browser-find-usage (ident)
  (if (>= bee-browser-version 2)
      (bee-browser-v2-find-usage ident)
    (bee-browser-v1-find-usage ident)))

;*---------------------------------------------------------------------*/
;*    bee-browser-v1-find-usage ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-browser-v1-find-usage (ident)
  (let ((proc (plugin-find (intern bee-browser))))
    (if (not (and (consp proc) (consp (car proc)) (processp (car (car proc)))))
	(setq proc (bee-run-external-browser))
      (setq proc (car (car proc))))
    (if (processp proc)
	(let ((pcmd (concat "(footprint \"" ident "\")")))
	  (plugin-send-string proc pcmd)))))

;*---------------------------------------------------------------------*/
;*    bee-browser-v2-find-usage ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-browser-v2-find-usage (ident)
  (let ((proc (plugin-find (intern bee-browser))))
    (if (not (and (consp proc) (consp (car proc)) (processp (car (car proc)))))
	(progn
	  (setq proc (bee-run-external-browser))
	  (if (processp proc)
	      (let ((pcmd (concat "(inspect \"" ident "\")")))
		(plugin-send-string proc pcmd))))
      (progn
	(setq proc (car (car proc)))
	(if (processp proc)
	    (let ((pcmd (concat "(inspect-other-window \"" ident "\")")))
	      (plugin-send-string proc pcmd)))))))

