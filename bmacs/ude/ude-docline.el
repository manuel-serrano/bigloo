;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-docline.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 12 21:29:10 1998                          */
;*    Last change :  Wed Oct 26 19:09:05 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Ude online documentation handling.                               */
;*    -------------------------------------------------------------    */
;*    Up to now two online documentation formats are accepted:         */
;*      - info format                                                  */
;*      - man format                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-docline)
(require 'ude-custom)
(require 'ude-config)
(require 'ude-autoload)
(require 'info)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-docline-initialized-p ...                                    */
;*    -------------------------------------------------------------    */
;*    Is ude-docline initialized for that buffer?                      */
;*---------------------------------------------------------------------*/
(defvar ude-docline-initialized-p nil)
(make-variable-buffer-local 'ude-docline-initialized-p)

(make-variable-buffer-local 'Info-directory-list)

;*---------------------------------------------------------------------*/
;*    ude-docline-init ...                                             */
;*    -------------------------------------------------------------    */
;*    We look if there is an existing                                  */
;*      ude-root-directory/info                                        */
;*    directory. If such exists, we add it to the search path for      */
;*    xinfo.                                                           */
;*---------------------------------------------------------------------*/
(defun ude-docline-init ()
  (if (not ude-docline-initialized-p)
      (progn
	(setq ude-docline-initialized-p t)
	(remove-hook 'Info-mode-hook 'xinfo-mode-hook)
	(add-hook 'Info-mode-hook (function ude-info-mode-hook))
	(if (stringp ude-root-directory)
	    (let ((info (concat ude-root-directory "/info")))
	      (if (and (file-exists-p info) (file-directory-p info))
		  (setq Info-directory-list
			(cons info Info-directory-list))))))))

;*---------------------------------------------------------------------*/
;*    bee-pop-to-info ...                                              */
;*    -------------------------------------------------------------    */
;*    Pop up a buffer displaying an info file.                         */
;*---------------------------------------------------------------------*/
(defun ude-pop-to-info ()
  (let ((pop-up-frames t))
    (pop-to-buffer "*info*")))

;*---------------------------------------------------------------------*/
;*    ude-info-ref-internal ...                                        */
;*    -------------------------------------------------------------    */
;*    Search in the info documentation for a function (or variable)    */
;*    definition.                                                      */
;*---------------------------------------------------------------------*/
(defun ude-info-ref-internal (var)
  (if (string= var "")
      (progn
	(save-window-excursion
	  (info)
	  (Info-find-node (car ude-info-file-list) "Top"))
	(ude-pop-to-info))
    (let ((found nil))
      (save-window-excursion
	(info)
	(let ((ifiles ude-info-file-list)
	      (ude-info-fontify nil))
	  (while (and (not found) (consp ifiles))
	    (condition-case nil
		(progn
		  (Info-find-node (car ifiles) "Top")
		  (let ((index (Info-index var)))
		    ;; this is a kind of awful hack that I don't know how
		    ;; to implement in a better way. Info-index raises an
		    ;; error if it finds no match for the request (this explain
		    ;; the nesting condition-case). Moreover, Info-index
		    ;; returns a string like `Index "substring", found'.
		    ;; Because we want exact matches when searching for the
		    ;; online documentation, we have to test that the result
		    ;; of Info-index exactly matches the requested string
		    ;; nested inside `"'. If the result format of Info-node
		    ;; changed, I'm dead. This has to be checked when Xemacs
		    ;; will evolved.
		    (if (string-match (if (featurep 'xemacs)
					  (regexp-quote (concat "\"" var "\""))
					(regexp-quote (concat "\`" var "\'")))
				      index)
			(setq found t)
		      (setq ifiles (cdr ifiles)))))
	      (error (setq ifiles (cdr ifiles)))))))
      (if (not found)
	  (ude-error "Can't find documentation for `%S'" var)
	(progn
	  (if ude-info-fontify
	      (let* ((cbuf (current-buffer))
		     (ibuf (get-buffer "*info*")))
		(if (bufferp ibuf)
		    (progn
		      (set-buffer ibuf)
		      (font-lock-fontify-buffer)
		      (set-buffer cbuf)))))
	  (ude-pop-to-info))))))

;*---------------------------------------------------------------------*/
;*    ude-info-section ...                                             */
;*    -------------------------------------------------------------    */
;*    Search in the info documentation for a manual section.           */
;*---------------------------------------------------------------------*/
(defun ude-info-section (section)
  "Look up a section in the manual in the Info system.
This command is designed to be used whether you are already in Info or not."
  (interactive "sSection: ")
  (let ((found nil))
    (save-window-excursion
      (info)
      (let ((ifiles ude-info-file-list)
	    (ude-info-fontify nil))
	(while (and (not found) (consp ifiles))
	  (condition-case nil
	      (progn
		(Info-find-node (car ifiles) "Top")
		(Info-goto-node (format "(%s)%s" (car ifiles) section))
		(setq found t))
	    (error (setq ifiles (cdr ifiles)))))))
    (if (not found)
	(ude-error (format "Can't find section `%S'" section))
      (progn
	(if ude-info-fontify
	    (let* ((cbuf (current-buffer))
		   (ibuf (get-buffer "*info*")))
	      (if (bufferp ibuf)
		  (progn
		    (set-buffer ibuf)
		    (font-lock-fontify-buffer)
		    (set-buffer cbuf)))))
	(ude-pop-to-info)))))

;*---------------------------------------------------------------------*/
;*    ude-info-docline-ident ...                                       */
;*    -------------------------------------------------------------    */
;*    This function calls an online documentation with respect         */
;*    to the active context.                                           */
;*    -------------------------------------------------------------    */
;*    If a region is active, this function prints the documentation    */
;*    for the s-expression.                                            */
;*---------------------------------------------------------------------*/
(defun ude-info-docline-ident (flock ident)
  "Popup an online documentation according to context."
  (interactive)
  (ude-docline-init)
  (ude-info-init flock)
  (ude-info-ref-internal ident))

;*---------------------------------------------------------------------*/
;*    ude-info-docline ...                                             */
;*    -------------------------------------------------------------    */
;*    This function calls an online documentation with respect         */
;*    to the active context.                                           */
;*    -------------------------------------------------------------    */
;*    If a region is active, this function print the documentation     */
;*    for the s-expression.                                            */
;*---------------------------------------------------------------------*/
(defun ude-info-docline (flock)
  "Popup an online documentation according to context."
  (interactive)
  (ude-docline-init)
  (ude-info-init flock)
  (if (and (region-active-p)
	   (region-exists-p)
	   (functionp ude-info-region))
      (funcall ude-info-region (region-beginning) (region-end))
    (ude-info-ref-internal
     (ude-fetch-then-request-identifier (point) "help for: "))))

;*---------------------------------------------------------------------*/
;*    ude-man-docline ...                                              */
;*    -------------------------------------------------------------    */
;*    This function calls an online documentation with respect         */
;*    to the active context.                                           */
;*---------------------------------------------------------------------*/
(defun ude-man-docline ()
  "Popup an online documentation according to context."
  (interactive)
  (let* ((ident (ude-fetch-then-request-identifier (point) "help for: "))
	 (Manual-buffer-view-mode 'other)
	 (pop-up-frames t))
    (setq Manual-buffer-view-mode Manual-buffer-view-mode)
    (manual-entry ident ude-manual-page-number)))

;*---------------------------------------------------------------------*/
;*    doc source keymap                                                */
;*---------------------------------------------------------------------*/
(defvar ude-path-mouse-map (make-sparse-keymap))
(define-key ude-path-mouse-map ude-mouse-2-binding
  (function ude-path-find))
(if ude-mouse-2-binding-disable
    (define-key ude-path-mouse-map ude-mouse-2-binding-disable
      #'(lambda (e) (interactive "e"))))
(defvar ude-ref-mouse-map (make-sparse-keymap))
(define-key ude-ref-mouse-map ude-mouse-2-binding
  (function ude-ref-find))
(if ude-mouse-2-binding-disable
    (define-key ude-ref-mouse-map ude-mouse-2-binding-disable
      #'(lambda (e) (interactive "e"))))
(defvar ude-deffn-mouse-map (make-sparse-keymap))
(define-key ude-deffn-mouse-map ude-mouse-2-binding
  (function ude-deffn-find))
(if ude-mouse-2-binding-disable
    (define-key ude-deffn-mouse-map ude-mouse-2-binding-disable
      #'(lambda (e) (interactive "e"))))
(defvar ude-node-mouse-map (make-sparse-keymap))
(define-key ude-node-mouse-map ude-mouse-2-binding
  (function ude-node-find))
(if ude-mouse-2-binding-disable
    (define-key ude-node-mouse-map ude-mouse-2-binding-disable
      #'(lambda (e) (interactive "e"))))

;*---------------------------------------------------------------------*/
;*    ude-fontify-doc-source ...                                       */
;*    -------------------------------------------------------------    */
;*    This function fontify a buffer according to doc sources          */
;*    annotations.                                                     */
;*---------------------------------------------------------------------*/
(defun ude-fontify-doc-source (buffer)
  (interactive "Bbuffer: ")
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (let* ((path-regexp   "@path[ \t]+\\([^@]+\\)@")
	   (anchor-regexp "@ref[ \t]+\\([^@:]+\\):\\([^@]+\\)@")
	   (node-regexp   "@node[ \t]+\\([^@]+\\)@")
	   (doc-regexp    "@deffn ?[^@]*@")
	   (eval-regexp   "@eval[ \t]+\\([^@]+\\)@")
	   (union-regexp  (concat path-regexp "\\|"
				  anchor-regexp "\\|"
				  node-regexp "\\|"
				  doc-regexp "\\|"
				  eval-regexp))
	   (path '()))
      (while (re-search-forward union-regexp (point-max) t)
	(goto-char (match-beginning 0))
	(let ((end (match-end 0)))
	  (cond
	   ((re-search-forward anchor-regexp end t)
	    ;; a reference
	    (let ((file (buffer-substring (match-beginning 1) (match-end 1)))
		  (ref  (buffer-substring (match-beginning 2) (match-end 2))))
	      (if (and (stringp file) (file-exists-p file))
		  (put-text-properties (match-beginning 0) (match-end 0)
				       'doc-source file
				       'mouse-face 'highlight
				       'keymap ude-ref-mouse-map
				       'doc-anchor ref))))
	   ((re-search-forward path-regexp end t)
	    ;; a path addition
	    (let ((file (buffer-substring (match-beginning 1) (match-end 1))))
	      
	      (if (and (stringp file) (file-exists-p file))
		  (progn
		    (put-text-properties (match-beginning 0) (match-end 0)
					 'doc-source file
					 'mouse-face 'highlight
					 'keymap ude-path-mouse-map)
		    (setq path (cons file path))))))
	   ((re-search-forward doc-regexp end t)
	    ;; a function definition
	    (let* ((start  (match-beginning 0))
		   (stop   (match-end 0)))
	      (put-text-properties start stop
				   'doc-source path
				   'mouse-face 'highlight
				   'doc-position (cons start stop)
				   'keymap ude-deffn-mouse-map)))
	   ((re-search-forward node-regexp end t)
	    ;; a node definition
	    (let ((node (buffer-substring (match-beginning 1) (match-end 1))))
	      (if (stringp node)
		  (put-text-properties (match-beginning 0) (match-end 0)
				       'doc-source (cons node path)
				       'mouse-face 'highlight
				       'keymap ude-node-mouse-map))))
	   ((re-search-forward eval-regexp end t)
	    (eval-region (match-beginning 1) (match-end 1)))
	   (t
	    (error (format "Illegal match %S" end))))
	  (goto-char end))))))

;*---------------------------------------------------------------------*/
;*    ude-path-find ...                                                */
;*---------------------------------------------------------------------*/
(defun ude-path-find  (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event)))
    (find-file-other-frame (find-text-property point 'doc-source buffer))))

;*---------------------------------------------------------------------*/
;*    ude-find-in-files ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-find-in-files (pat files)
  (let ((found nil))
    (while (and (not found) (consp files))
      (let* ((cmd (concat ude-egrep " \"" pat "\" " (car files)))
	     (res (shell-command-to-string cmd)))
	(if (string= res "")
	    (setq files (cdr files))
	  (setq found (car files)))))
    found))

;*---------------------------------------------------------------------*/
;*    ude-ref-find ...                                                 */
;*---------------------------------------------------------------------*/
(defun ude-ref-find (event)
  (interactive "e")
  (let* ((point      (event-closest-point event))
	 (buffer     (event-buffer event))
	 (file       (find-text-property point 'doc-source buffer))
	 (anchor     (find-text-property point 'doc-anchor buffer))
	 (doc-regexp (format "@label %s@" anchor)))
    (let ((buffer (find-file-noselect file)))
      (if (bufferp buffer)
	  (progn
	    (set-buffer buffer)
	    (goto-char (point-min))
	    (if (search-forward doc-regexp (point-max) t)
		(let ((pop-up-frames t))
		  (goto-char (match-beginning 0))
		  (pop-to-buffer buffer))
	      (ude-error "Can't find label `%S' in `%S'" anchor file)))
	(ude-error "Can't find file `%S'" file)))))

;*---------------------------------------------------------------------*/
;*    ude-deffn-find ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-deffn-find (event)
  (interactive "e")
  (let* ((buffer     (event-buffer event))
	 (files      (find-text-property 'doc-source buffer))
	 (pos        (find-text-property 'doc-position buffer))
	 (start      (car pos))
	 (stop       (cdr pos))
	 (doc-regexp "@deffn \\([^@]+\\)@")
	 (id         (save-excursion
		       (goto-char start)
		       (if (re-search-forward doc-regexp stop t)
			   (buffer-substring (match-beginning 1)
					     (match-end 1))
			 nil))))
    (if id
	(let* ((eregexp (concat "@deffn[x]?[ \t]*\\{[^}]+\\}[ \t]+"
				(regexp-quote id)
				"[\n \t]"))
	       (regexp (concat "@deffn[x]?[ \t]*{[^}]+}[ \t]+"
			       (regexp-quote id)
			       "[\n \t]"))
	       (file (ude-find-in-files eregexp files)))
	  (if (stringp file)
	      (let ((buffer (find-file-noselect file)))
		(if (bufferp buffer)
		    (progn
		      (set-buffer buffer)
		      (goto-char (point-min))
		      (if (re-search-forward regexp (point-max) t)
			  (let ((pop-up-frames t))
			    (goto-char (match-beginning 0))
			    (pop-to-buffer buffer))
			(ude-error "Can't find documentation for `%S' in `%S'"
				   id
				   file)))
		  (ude-error "Can't find documentation for `%S' in `%S'"
			     id
			     file)))
	    (ude-error "Can't find file `%S'" file)))
      (ude-error "Can't find variable name at `%S'"
		 (buffer-substring start stop)))))

;*---------------------------------------------------------------------*/
;*    ude-node-find ...                                                */
;*---------------------------------------------------------------------*/
(defun ude-node-find (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event))
	 (prop   (find-text-property point 'doc-source buffer))
	 (node   (car prop))
	 (files  (cdr prop)))
    (let* ((regexp (concat "@node[ \t]+" node))
	   (file   (ude-find-in-files regexp files)))
      (if (stringp file)
	  (let ((buffer (find-file-noselect file)))
	    (if (bufferp buffer)
		(progn
		  (set-buffer buffer)
		  (goto-char (point-min))
		  (if (re-search-forward regexp (point-max) t)
		      (let ((pop-up-frames t))
			(goto-char (match-beginning 0))
			(pop-to-buffer buffer))
		    (ude-error "Can't find documentation for node `%S' in `%S'"
			       node file)))
	      (ude-error "Can't find buffer for `%S'" file)))
	(ude-error "Can't find buffer for `%S'" file)))))

;*---------------------------------------------------------------------*/
;*    Reference positioning ...                                        */
;*---------------------------------------------------------------------*/
(defvar ude-ref-point nil)
(defvar ude-ref-buffer nil)
(defvar ude-ref-label nil)

;*---------------------------------------------------------------------*/
;*    ude-mouse-ref-armed-p ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-mouse-ref-armed-p ()
  (stringp ude-ref-label))

;*---------------------------------------------------------------------*/
;*    ude-mouse-ref-label ...                                          */
;*---------------------------------------------------------------------*/
(defun ude-mouse-ref-label ()
  ude-ref-label)

;*---------------------------------------------------------------------*/
;*    ude-mouse-make-label ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-mouse-make-label (str)
  (interactive "sReference label: ")
  (setq ude-ref-label str)
  (setq ude-ref-point (point))
  (setq ude-ref-buffer (current-buffer))
  (insert (format "@label %s@" ude-ref-label))
  (message "Label %S sets at %S:%d"
	   ude-ref-label
	   (buffer-name ude-ref-buffer)
	   ude-ref-point))

;*---------------------------------------------------------------------*/
;*    ude-mouse-make-ref ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-mouse-make-ref ()
  (interactive)
  (condition-case nil
      (progn
	(message "Setting reference %S from buffer %S to buffer %S"
		 ude-ref-label
		 (buffer-name (current-buffer))
		 (buffer-name ude-ref-buffer))
	(let* ((fname (buffer-file-name (current-buffer)))
	       (fname2 (file-relative-name
			(buffer-file-name ude-ref-buffer)
			(file-name-directory fname))))
	  (insert (format "@ref %s:%s@" fname2 ude-ref-label))))
    (error
     (ude-error "Can't set %S" ude-ref-label))))

;*---------------------------------------------------------------------*/
;*    ude-mouse-make-mutual-ref ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-mouse-make-mutual-ref ()
  (interactive)
  (condition-case nil
      (progn
	(message "Setting mutual reference %S from buffer %S to buffer %S"
		 ude-ref-label
		 (buffer-name (current-buffer))
		 (buffer-name ude-ref-buffer))
	(let ((fname (buffer-file-name (current-buffer))))
	  ;; set the new local label
	  (insert (format "@label %s@" ude-ref-label))
	  (save-excursion
	    (let ((fname2 (file-relative-name
			   fname
			   (file-name-directory
			    (buffer-file-name ude-ref-buffer))))
		  (fname3 (file-relative-name
			   (buffer-file-name ude-ref-buffer)
			   (file-name-directory fname))))
	      (insert (format "@ref %s:%s@" fname3 ude-ref-label))
	      (set-buffer ude-ref-buffer)
	      (save-excursion
		(goto-char ude-ref-point)
		(insert (format "@ref %s:%s@" fname2 ude-ref-label)))))))
    (error
     (ude-error "Can't set %S" ude-ref-label))))
    
  
