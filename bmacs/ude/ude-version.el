;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-version.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 10 16:10:24 1998                          */
;*    Last change :  Tue Sep 20 08:36:41 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bee versionning                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-version)
(require 'ude-config)
(require 'ude-custom)
(require 'ude-autoload)
(require 'ude-compile)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-checkin-project ...                                          */
;*---------------------------------------------------------------------*/
(defun ude-checkin-project ()
  (interactive)
  (if (yes-or-no-p-dialog-box "Checkin new version?")
      (let ((ude-compile-command (format "%s -f %s %s"
					 ude-make
					 ude-makefile
					 ude-makefile-revision-entry)))
	(ude-compile))))

;*---------------------------------------------------------------------*/
;*    ude-tar-gz-project ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-tar-gz-project ()
  (interactive)
  (let ((ude-compile-command (format "%s -f %s %s"
				     ude-make
				     ude-makefile
				     ude-makefile-tar-gz-entry)))
    (ude-compile)))

;*---------------------------------------------------------------------*/
;*    ude-version-highlight-buffer ...                                 */
;*---------------------------------------------------------------------*/
(defun ude-version-highlight-buffer (buffer msg mkmf file regexp keymap)
  (set-buffer buffer)
  (erase-buffer)
  (insert msg)
  (let* ((height (save-excursion (+ 2 (count-lines (point-min) (point-max)))))
	 (cur-height (frame-height (selected-frame)))
	 (new-height (if (> height cur-height) cur-height height))
	 (default-frame-alist (cons 'height
				    (cons new-height default-frame-alist)))
	 (pop-up-frames t))
    (goto-char (point-min))
    (while (re-search-forward regexp (point-max) t)
      (let* ((version (buffer-substring (match-beginning 1) (match-end 1)))
	     (end  (match-end 0)))
	(put-text-properties (match-beginning 0) end
			     'version (list mkmf file version)
			     'mouse-face 'highlight
			     'keymap keymap)
	(goto-char end)))
    (pop-to-buffer buffer)
    buffer))

;*---------------------------------------------------------------------*/
;*    checkout map                                                     */
;*---------------------------------------------------------------------*/
(defvar ude-checkout-map (make-sparse-keymap))
(define-key ude-checkout-map ude-mouse-2-binding
  (function ude-checkout-file))

;*---------------------------------------------------------------------*/
;*    ude-checkout-file-version ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-checkout-file-version ()
  (interactive)
  (let* ((file (file-relative-name (buffer-file-name (current-buffer))
				   ude-root-directory))
	 (cmd (format "cd %s; %s -f %s %s %s=%s"
		      ude-root-directory
		      ude-make
		      ude-makefile
		      ude-makefile-infofile-entry
		      ude-makefile-infofile-args
		      file))
	 (res (shell-command-to-string cmd)))
    (if (not (string= res ""))
	(let* ((regexp "^[^ \t]+[ \t]+\\([^ \t]+\\).*$")
	       (buffer (get-buffer-create (format "*%s-versions*" file))))
	  (ude-version-highlight-buffer buffer
					res
					ude-makefile
					file
					regexp
					ude-checkout-map))
      (ude-error (format "No version of %S found" file)))))

;*---------------------------------------------------------------------*/
;*    ude-checkout-file ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-checkout-file (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event))
	 (prop   (find-text-property point 'version buffer)))
    (if (consp prop)
	(let* ((mkmf    (car prop))
	       (file    (car (cdr prop)))
	       (version (car (cdr (cdr prop))))
	       (curname (concat file ".current")))
	  ;; we first have to rename the current file
	  (rename-file file curname)
	  (unwind-protect
	      (let* ((cmd (format "cd %s; %s -f %s %s %s=%s %s=%s"
				  ude-root-directory
				  ude-make
				  mkmf
				  ude-makefile-checkout-entry
				  ude-makefile-infofile-args
				  file
				  ude-makefile-fileversion-args
				  version))
		     (res (shell-command-to-string cmd)))
		;; we rename the checked out file
		(if (file-exists-p file)
		    (let* ((name (file-name-sans-extension file))
			   (oldname (if (string= name file)
					(concat file "-" version)
				      (concat file
					      "-" version
					      (substring file
							 (length name)
							 (length file))))))
		      (if (file-exists-p oldname)
			  (delete-file oldname))
		      (rename-file file oldname)
		      ;; we open a buffer for the checked out file
		      (let* ((default-directory ude-root-directory)
			     (buffer (find-file-other-frame oldname)))
			(delete-file oldname)
			buffer))
		  (ude-error (format "Can't restore version %S for file %S because of %S"
				 version
				 file
				 res))))
	    (rename-file curname file))))))
		
;*---------------------------------------------------------------------*/
;*    diff map                                                         */
;*---------------------------------------------------------------------*/
(defvar ude-diff-map (make-sparse-keymap))
(define-key ude-diff-map ude-mouse-2-binding
  (function ude-diff-file))

;*---------------------------------------------------------------------*/
;*    ude-diff-file-version ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-diff-file-version ()
  (interactive)
  (let* ((file (file-relative-name (buffer-file-name (current-buffer))
				   ude-root-directory))
	 (cmd (format "cd %s; %s -f %s %s %s=%s"
		      ude-root-directory
		      ude-make
		      ude-makefile
		      ude-makefile-infofile-entry
		      ude-makefile-infofile-args
		      file))
	 (res (shell-command-to-string cmd)))
    (if (not (string= res ""))
	(let* ((regexp "^[^ \t]+[ \t]+\\([^ \t]+\\).*$")
	       (buffer (get-buffer-create (format "*%s-versions*" file))))
	  (ude-version-highlight-buffer buffer
					res
					ude-makefile
					file
					regexp
					ude-diff-map))
      (ude-error (format "No version of %S found" file)))))

;*---------------------------------------------------------------------*/
;*    ude-diff-file ...                                                */
;*---------------------------------------------------------------------*/
(defun ude-diff-file (event)
  (interactive "e")
  (let* ((point   (event-closest-point event))
	 (buffer  (event-buffer event))
	 (obuffer (ude-checkout-file event))
	 (prop    (find-text-property point 'version buffer))
	 (file    (car (cdr prop)))
	 (fbuffer (get-buffer file)))
    (if (bufferp obuffer)
	(unwind-protect
	    (progn
	      (set-buffer fbuffer)
	      (ediff-buffers fbuffer obuffer))))))

	
