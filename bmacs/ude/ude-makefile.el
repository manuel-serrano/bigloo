;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-makefile.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Nov 13 08:19:36 1998                          */
;*    Last change :  Wed Jan 29 14:08:34 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Ude Makefile handling                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-makefile)
(require 'ude-custom)
(require 'ude-config)
(require 'ude-compile)

;*---------------------------------------------------------------------*/
;*    ude-fetch-makefile-entries ...                                   */
;*    -------------------------------------------------------------    */
;*    This function scans a Makefile a build the list contains         */
;*    all the Makefile entries.                                        */
;*---------------------------------------------------------------------*/
(defun ude-fetch-makefile-entries ()
  (let ((mkmf (concat ude-root-directory ude-makefile)))
    (if (file-exists-p mkmf)
	(let* ((cmd     (concat ude-egrep " \""
				ude-makefile-entry-regexp
				":\" " mkmf))
	       (entries (shell-command-to-string cmd))
	       (pat     (concat "^\\(" ude-makefile-entry-regexp "\\):"))
	       (index   0)
	       (res     '()))
	  (setq res '())
	  (while (string-match pat entries index)
	    (let ((entry (substring entries
				    (match-beginning 1)
				    (match-end 1)))
		  (end   (match-end 0)))
	      (if (not (string-match ude-makefile-entry-exclude-regexp entry))
		  (setq res (cons entry res)))
	      (setq index (+ 1 end))))
	  (if ude-sort-makefile-entries
	      (sort res 'string<)
	    (nreverse res))))))

;*---------------------------------------------------------------------*/
;*    ude-fetch-makefile-one-entry ...                                 */
;*---------------------------------------------------------------------*/
(defun ude-fetch-makefile-one-entry (name)
  (interactive)
  (let ((mkmf (concat ude-root-directory ude-makefile)))
    (if (file-exists-p mkmf)
	(let* ((cmd (format "%s -f %s -s %s"
			    ude-make
			    mkmf
			    name))
	       (default-directory ude-root-directory)
	       (exec (shell-command-to-string cmd)))
	  (cond
	   ((string= exec "")
	    nil)
	   ((string-match (concat ude-makefile-binary-entry "\\|stop") exec)
	    nil)
	   ((eq (aref exec (- (length exec) 1)) ?\n)
	    (substring exec 0 (- (length exec) 1)))
	   (t
	    exec))))))

;*---------------------------------------------------------------------*/
;*    ude-fetch-makefile-binary-entry ...                              */
;*---------------------------------------------------------------------*/
(defun ude-fetch-makefile-binary-entry ()
  (ude-fetch-makefile-one-entry ude-makefile-binary-entry))

;*---------------------------------------------------------------------*/
;*    ude-fetch-makefile-binary_p-entry ...                            */
;*---------------------------------------------------------------------*/
(defun ude-fetch-makefile-binary_p-entry ()
  (interactive)
  (ude-fetch-makefile-one-entry ude-makefile-binary_p-entry))

;*---------------------------------------------------------------------*/
;*    ude-fetch-makefile-mode-entry ...                                */
;*---------------------------------------------------------------------*/
(defun ude-fetch-makefile-mode-entry ()
  (interactive)
  (ude-fetch-makefile-one-entry ude-makefile-mode-entry))

;*---------------------------------------------------------------------*/
;*    ude-edit-makefile ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-edit-makefile ()
  (interactive)
  (let ((default-directory ude-root-directory))
    (find-file-other-frame ude-makefile)))

;*---------------------------------------------------------------------*/
;*    ude-remove-makefile ...                                          */
;*---------------------------------------------------------------------*/
(defun ude-remove-makefile ()
  (interactive)
  (let ((name (concat ude-root-directory "/" ude-makefile)))
    (if (yes-or-no-p-dialog-box
	 (format "Delete the Makefile [%s]?" name))
	(delete-file name))))

;*---------------------------------------------------------------------*/
;*    ude-make-ude ...                                                 */
;*---------------------------------------------------------------------*/
(defun ude-make-ude ()
  (interactive)
  (if (file-exists-p (concat ude-root-directory ude-makefile))
      (let ((ude-compile-command (format "%s -f %s %s"
					 ude-make
					 ude-makefile
					 ude-make-ude-entry)))
	(ude-compile))
    (ude-error "Can't find %s%s" ude-root-directory ude-makefile)))
  
;*---------------------------------------------------------------------*/
;*    ude-update-makefile ...                                          */
;*---------------------------------------------------------------------*/
(defun ude-update-makefile (&optional name)
  (interactive)
  (if (file-exists-p (concat ude-root-directory ude-makefile))
      (let ((ude-compile-command (format "%s -f %s%s %s; %s -f %s %s"
					 ude-make
					 ude-root-directory
					 ude-makefile
					 ude-makefile-update-entry
					 ude-make
					 ude-makefile
					 ude-make-ude-entry)))
	(ude-compile))
    (ude-error "Can't find %s%s" ude-root-directory ude-makefile)))

;*---------------------------------------------------------------------*/
;*    ude-generate-makefile ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-generate-makefile (option)
  (interactive)
  (if (or (not (file-exists-p (concat ude-root-directory ude-makefile)))
	  (yes-or-no-p-dialog-box "Makefile already exists, overwrite it?"))
      (let ((main (buffer-file-name)))
	(message "Generating Makefile")
	(if (not (file-exists-p main))
	    (message "*** WARNING: Can't find entry file -- %s" main))
	(let ((ude-compile-command (format "%s %s %s -o %s; %s -f %s %s"
					   ude-makemake
					   option
					   (file-relative-name
					    main
					    ude-root-directory)
					   ude-makefile
					   ude-make
					   ude-makefile
					   ude-make-ude-entry)))
	  ;; we do produce the Makefile
	  (ude-compile)))))

;*---------------------------------------------------------------------*/
;*    ude-add-makefile-entry ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-add-makefile-entry (object source)
  (interactive)
  (if (and (stringp ude-makefile) (file-exists-p ude-makefile))
      (progn
	(message "Adding entries to makefile")
	(let* ((cmd (format "%s -f %s addentry OBJ_ENTRY=%s SRC_ENTRY=%s; %s -f %s %s"
			    ude-make
			    ude-makefile
			    (if (stringp object)
				(file-relative-name object
						    ude-root-directory)
			      "")
			    (if (stringp source)
				(file-relative-name source
						    ude-root-directory)
			      "")
			    ude-make
			    ude-makefile
			    ude-make-ude-entry))
	       (compilation-ask-about-save nil)
	       (ude-always-detach-compile-buffer nil)
	       (ude-compile-command cmd))
	  (ude-compile)))
    (ude-error "Can't find Makefile.")))

;*---------------------------------------------------------------------*/
;*    ude-add-user-makefile-entry ...                                  */
;*    -------------------------------------------------------------    */
;*    The interactive wrapper for UDE-ADD-MAKEFILE-ENTRY               */
;*---------------------------------------------------------------------*/
(defun ude-add-user-makefile-entry (source)
  (interactive "sSource entry: ")
  (let ((default-directory ude-root-directory))
    (if (string-match "\\(.*\\)\\([.][^.]+\\)$" source)
	(let ((base (substring source (match-beginning 1) (match-end 1))))
	  (ude-add-makefile-entry (concat base ".o") source))
      (ude-add-makefile-entry (read-string "object: ") source))))
  
;*---------------------------------------------------------------------*/
;*    ude-makefile-debug-mode ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-makefile-debug-mode ()
  "Sets the project Makefile in high debugging mode."
  (interactive)
  (let ((default-directory ude-root-directory))
    (if (and (stringp ude-makefile) (file-exists-p ude-makefile))
	(let ((buffer (current-buffer)))
	  (message "Switching to high debugging mode...")
	  (setq ude-compile-mode 'debug)
	  (if (bufferp ude-last-compile-buffer)
	      (progn
		(set-buffer ude-last-compile-buffer)
		(ude-set-compilation-modeline)
		(set-buffer buffer)))
	  (let* ((cmd (format "%s -o %s -debug"
			      ude-makemake
			      ude-makefile))
		 (compilation-ask-about-save nil)
		 (ude-always-detach-compile-buffer nil)
		 (ude-compile-command cmd))
	    (ude-compile)))
      (ude-error "Can't find Makefile."))))
  
;*---------------------------------------------------------------------*/
;*    ude-makefile-devel-mode ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-makefile-devel-mode ()
  "Sets the project Makefile in development mode."
  (interactive)
  (let ((default-directory ude-root-directory))
    (if (and (stringp ude-makefile) (file-exists-p ude-makefile))
	(let ((buffer (current-buffer)))
	  (message "Switching to development mode...")
	  (setq ude-compile-mode 'devel)
	  (if (bufferp ude-last-compile-buffer)
	      (progn
		(set-buffer ude-last-compile-buffer)
		(ude-set-compilation-modeline)
		(set-buffer buffer)))
	  (let* ((cmd (format "%s -o %s -devel"
			      ude-makemake
			      ude-makefile))
		 (compilation-ask-about-save nil)
		 (ude-always-detach-compile-buffer nil)
		 (ude-compile-command cmd))
	    (ude-compile)))
      (ude-error "Can't find Makefile."))))
  
;*---------------------------------------------------------------------*/
;*    ude-makefile-final-mode ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-makefile-final-mode ()
  "Sets the project Makefile in final mode."
  (interactive)
  (let ((default-directory ude-root-directory))
    (if (and (stringp ude-makefile) (file-exists-p ude-makefile))
	(let ((buffer (current-buffer)))
	  (message "Switching to final mode...")
	  (setq ude-compile-mode 'final)
	  (if (bufferp ude-last-compile-buffer)
	      (progn
		(set-buffer ude-last-compile-buffer)
		(ude-set-compilation-modeline)
		(set-buffer buffer)))
	  (let* ((cmd (format "%s -o %s -final"
			      ude-makemake
			      ude-makefile))
		 (compilation-ask-about-save nil)
		 (ude-always-detach-compile-buffer nil)
		 (ude-compile-command cmd))
	    (ude-compile)))
      (ude-error "Can't find Makefile."))))

;*---------------------------------------------------------------------*/
;*    ude-makefile-set-name ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-makefile-set-name (arg)
  "Sets the project name."
  (interactive "SProject name: ")
  (let ((default-directory ude-root-directory))
    (if (and (stringp ude-makefile) (file-exists-p ude-makefile))
	(let ((buffer (current-buffer)))
	  (message (format "Setting project name to %S..." arg))
	  (if (bufferp ude-last-compile-buffer)
	      (progn
		(set-buffer ude-last-compile-buffer)
		(ude-set-compilation-modeline)
		(set-buffer buffer)))
	  (let* ((cmd (format "%s -o %s -project %s"
			      ude-makemake
			      ude-makefile
			      arg))
		 (compilation-ask-about-save nil)
		 (ude-always-detach-compile-buffer nil)
		 (ude-compile-command cmd))
	    (ude-compile)))
      (ude-error "Can't find Makefile."))))
