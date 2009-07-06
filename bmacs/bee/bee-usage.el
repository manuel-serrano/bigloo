;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-usage.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Nov 14 16:04:11 1998                          */
;*    Last change :  Tue Sep 20 08:36:02 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bee variable usage informations.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-usage)
(require 'bee-autoload)
(require 'ude-config)
(require 'ude-custom)
(require 'ude-icon)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    bee-find-local-definition ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-find-local-definition (var)
  "Find a local variable definition."
  nil)

;*---------------------------------------------------------------------*/
;*    make-define-entry ...                                            */
;*---------------------------------------------------------------------*/
(defun make-define-entry (pos buffer)
  (let ((define (buffer-substring-no-properties (save-excursion
						  (goto-char pos)
						  (beginning-of-line)
						  (point))
						(save-excursion
						  (end-of-line)
						  (point)))))
    (vector (concat "def   : " define)
	    `(let ((pop-up-frames t))
	       (pop-to-buffer ,buffer)
	       (goto-char ,pos))
	    t)))

;*---------------------------------------------------------------------*/
;*    make-module-entry ...                                            */
;*---------------------------------------------------------------------*/
(defun make-module-entry (module buffer)
  (if (consp module)
      (vector (concat "module: " (car module))
	      `(let ((pop-up-frames t))
		 (pop-to-buffer ,buffer)
		 (goto-char (point-min)))
	      t)
    (vector "????" '() t)))

;*---------------------------------------------------------------------*/
;*    make-decl-entry ...                                              */
;*---------------------------------------------------------------------*/
(defun make-decl-entry (module buffer)
  (if (consp (cdr module))
      (let* ((start (cdr (cdr module)))
	     (decl (id-select-sexp-start start)))
	(vector (concat "decl  : " (buffer-substring-no-properties (car decl)
								   (cdr decl)))
		`(let ((pop-up-frames t))
		   (pop-to-buffer ,buffer)
		   (goto-char ,(car decl)))
		t))
    (vector "decl  : implicit" '() t)))

;*---------------------------------------------------------------------*/
;*    make-use-entry ...                                               */
;*---------------------------------------------------------------------*/
(defun make-use-entry (ident module)
  (let ((static (or (not (consp (cdr module)))
		    (string= (car (cdr module)) "static"))))
    (vector "Usages..." `(bee-find-usage ,ident ,static) t)))
  
;*---------------------------------------------------------------------*/
;*    make-assert-entry ...                                            */
;*---------------------------------------------------------------------*/
(defun make-assert-entry (asserts)
  (if (null asserts)
      asserts
    (list (cons "Assertions..."
		(mapcar #'(lambda (assert)
			    (vector (format "%S" assert) '() t))
			asserts)))))

;*---------------------------------------------------------------------*/
;*    bee-usage-find ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is not exact because it does not open the include  */
;*    file in order to read the directives clauses.                    */
;*---------------------------------------------------------------------*/
(defun bee-usage-find (ident)		     
  "Find documentation."
  (interactive (ude-interactive-ident (point) "Usage: "))
  (if (bee-browser-ready-p)
      (bee-browser-find-usage ident)
    (let ((buffer (save-excursion (bee-tags-find-variable-noselect ident))))
      (if (bufferp buffer)
	  ;; we have found the definition for this variable
	  (progn
	    (set-buffer buffer)
	    ;; we fetch the s-expression that defines the variable
	    (let ((sexp (id-select-sexp-start (point))))
	      ;; if check for errors
	      (if (not (consp sexp))
		  (ude-error (format "Illegal definition -- %S" ident))
		(let* ((module (bee-module-declaration-find ident))
		       (static (or (not (consp (cdr module)))
				   (string= (car (cdr module)) "static"))))
		  (bee-find-usage ident static)))))))))

;*---------------------------------------------------------------------*/
;*    bee-usage-info ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is not exact because it does not open the include  */
;*    file in order to read the directives clauses.                    */
;*---------------------------------------------------------------------*/
(defun bee-usage-info (ident)		     
  "Find documentation."
  (interactive (ude-interactive-ident (point) "Usage: "))
  (if (bee-browser-ready-p)
      (bee-browser-find-usage ident)
    (let ((buffer (save-excursion (bee-tags-find-variable-noselect ident))))
      (if (bufferp buffer)
	  ;; we have found the definition for this variable
	  (progn
	    (set-buffer buffer)
	    ;; we fetch the s-expression that defines the variable
	    (let ((sexp (id-select-sexp-start (point))))
	      ;; if check for errors
	      (if (not (consp sexp))
		  (ude-error (format "Illegal definition -- %S" ident))
		(let* ((assert (save-excursion (bee-assert-find sexp)))
		       (define-point (point))
		       (module (bee-module-declaration-find ident))
		       (menu   (append
				(list (make-module-entry module buffer)
				      "-"
				      (make-decl-entry module buffer)
				      (make-define-entry define-point buffer)
				      "-"
				      (make-use-entry ident module))
				(make-assert-entry assert))))
		  (popup-menu (cons ident menu))))))))))

;*---------------------------------------------------------------------*/
;*    grep keymap                                                      */
;*---------------------------------------------------------------------*/
(defvar bee-grep-mouse-map (make-sparse-keymap))
(define-key bee-grep-mouse-map ude-mouse-2-binding
  (function bee-grep-visit))
(defvar bee-local-root-directory "./"
  "This variable IS NOT buffer local.
It is used to bypass the buffer local variable mechanism!")

;*---------------------------------------------------------------------*/
;*    bee-find-usage ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-find-usage (ident static)
  ;; we have to set the BEE-LOCAL-ROOT-DIRECTORY while we are
  ;; in the source buffer. It is not possible to use the
  ;; UDE-ROOT-DIRECTORY in the GREP buffer because that variable
  ;; will be incorrect over there.
  (setq bee-local-root-directory ude-root-directory)
  (let ((files (if static
		   (file-relative-name (buffer-file-name (current-buffer))
				       ude-root-directory)
		 (ude-compile-makefile-entry ude-makefile-getsources-entry))))
    (cond
     ((string= files "")
      (ude-error "Can't find source files (or can't find Makefile entry `getsources'"))
     ((string-match ude-makefile-getsources-entry files)
      (ude-error "Can't find source file in Makefile"))
     (t
      (let* ((cmd (format "cd %s; %s \"^[^;]+%s\" %s"
			  ude-root-directory
			  ude-egrep-n
			  (regexp-quote ident)
			  files))
	     (res (shell-command-to-string cmd)))
	(if (not (string= res ""))
	    (let ((src-file (file-relative-name
			     (buffer-file-name
			      (current-buffer))
			     ude-root-directory))
		  (buffer (get-buffer-create (format "*Calls-and-Refs-%s*"
						     ident))))
	      (set-buffer buffer)
	      (erase-buffer)
	      (insert res)
	      (let* ((height (save-excursion
			       (+ 2 (count-lines (point-min) (point-max)))))
		     (cur-height (frame-height (selected-frame)))
		     (new-height (if (> height cur-height)
				     cur-height
				   height))
		     (new-height (if (featurep 'xemacs)
				     new-height
				   (+ 6 new-height)))
		     (default-frame-alist (ude-default-frame-alist
					   'height new-height))
		     (pop-up-frames t)
		     (file-regexp "^\\([a-zA-Z]?:?[^:( \t\n]+\\)[:( \t]+\\([0-9]+\\)[:) \t].*$")
		     (nofile-regexp "^\\([0-9]+\\)[:) \t].*$"))
		;; the grep toolbar
		(bee-usage-init-toolbar (current-buffer))
		;; we seek `file:line: expression'
		(goto-char (point-min))
		(while (re-search-forward file-regexp (point-max) t)
		  (let* ((file (buffer-substring-no-properties
				(match-beginning 1)
				(match-end 1)))
			 (line (buffer-substring-no-properties
				(match-beginning 2)
				(match-end 2)))
			 (end  (match-end 0)))
		    (put-text-properties (match-beginning 0) end
					 'grep (cons file line)
					 'mouse-face 'highlight
					 'keymap bee-grep-mouse-map)
		    (put-text-properties (match-beginning 0) (match-end 1)
					 'face ude-font-lock-face-1)
		    (put-text-properties (match-beginning 2) (match-end 2)
					 'face ude-font-lock-face-4)
		    (goto-char end)))
		;; we seek `line: expression'
		(goto-char (point-min))
		(while (re-search-forward nofile-regexp (point-max) t)
		  (let* ((line (buffer-substring-no-properties
				(match-beginning 1)
				(match-end 1)))
			 (end  (match-end 0)))
		    (put-text-properties (match-beginning 0) end
					 'grep (cons src-file line)
					 'mouse-face 'highlight
					 'keymap bee-grep-mouse-map)
		    (put-text-properties (match-beginning 0) (match-end 1)
					 'face ude-font-lock-face-4)
		    (goto-char end)))
		;; we not highlight the matches
		(goto-char (point-min))
		(while (re-search-forward (regexp-quote ident) (point-max) t)
		  (put-text-properties (match-beginning 0) (match-end 0)
				       'face ude-font-lock-face-2)
		  (goto-char (match-end 0)))
		(pop-to-buffer buffer)))
	  (ude-error (format "Nothing found about %S" ident))))))))

;*---------------------------------------------------------------------*/
;*    bee-grep-visit ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-grep-visit (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event))
	 (prop   (find-text-property point 'grep buffer)))
    (if (consp prop)
	(let ((file (car prop))
	      (line (cdr prop)))
	  (if (stringp file)
	      (let* ((default-directory bee-local-root-directory)
		     (buffer (find-file-noselect file)))
		(if (bufferp buffer)
		    (progn
		      (let ((pop-up-frames t))
			(pop-to-buffer buffer))
		      (set-buffer buffer)
		      (goto-char (point-min))
		      (goto-line (string-to-number line)))
		  (ude-error "Can't find buffer for %S" file))))))))

;*---------------------------------------------------------------------*/
;*    bee-assert-find ...                                              */
;*    -------------------------------------------------------------    */
;*    This function search for all assertion in SEXP.                  */
;*---------------------------------------------------------------------*/
(defun bee-assert-find (sexp)
  (let ((start  (car sexp))
	(end    (cdr sexp))
	(assert '()))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "[(\\[]assert[ \n\t]" end t)
	(let ((sexp (id-select-sexp-start (match-beginning 0))))
	  (if (consp sexp)
	      (progn
		(setq assert (cons (buffer-substring-no-properties
				    (car sexp) (cdr sexp))
				   assert))
		(goto-char (cdr sexp))))))
      (nreverse assert))))

;*---------------------------------------------------------------------*/
;*    bee-usage-toolbar ...                                            */
;*---------------------------------------------------------------------*/
(defvar bee-usage-toolbar 
  `(;; the quit button
    (,ude-quit-icon ude-tool-bar-delete-frame "Close Usage Frame")
    --))

;*---------------------------------------------------------------------*/
;*    bee-usage-init-toolbar ...                                       */
;*---------------------------------------------------------------------*/
(defun bee-usage-init-toolbar (buffer)
  (ude-toolbar-set bee-usage-toolbar))

