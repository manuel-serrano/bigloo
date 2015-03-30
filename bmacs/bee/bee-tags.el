;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-tags.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 28 12:00:29 1998                          */
;*    Last change :  Sat Sep 15 08:58:53 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements the search functions:                     */
;*      - a module definition (using the afile)                        */
;*      - a local variable definition                                  */
;*      - a global imported variable (using the tags).                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-tags)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require bmacs-etags)
(require 'bee-autoload)
(require 'bee-config)
(require 'bee-mode)
(require 'ude-autoload)
(require 'ude-config)

;*---------------------------------------------------------------------*/
;*    bee-find-file-create-p ...                                       */
;*    -------------------------------------------------------------    */
;*    This function searches the file FNAME in the root                */
;*    directory. If that file does not exists it is created. If after  */
;*    that creation FNAME still does not exists, nil is returned.      */
;*---------------------------------------------------------------------*/
(defun bee-find-file-create-p (fname cmd)
  (if (file-exists-p fname)
      fname
    (progn
      (message "calling `%s'..." cmd)
      (let ((res (shell-command-to-string cmd)))
	(message "")
	(if (string-match ude-make-error-output res)
	    nil
	  fname)))))

;*---------------------------------------------------------------------*/
;*    bee-find-afile-create-p ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-find-afile-create-p ()
  (bee-find-file-create-p (concat ude-root-directory ude-afile-name)
			  bee-make-afile))

;*---------------------------------------------------------------------*/
;*    bee-find-tagsfile-create-p ...                                   */
;*    -------------------------------------------------------------    */
;*    This function searches the UDE-TAGSFILE-NAME file in the root    */
;*    directory. If that file does not exists it is created. If after  */
;*    that creation the tagsfile still does not exists, FIND-TAGSFILE  */
;*    returns nil.                                                     */
;*---------------------------------------------------------------------*/
(defun bee-find-tagsfile-create-p ()
  (bee-find-file-create-p (concat ude-root-directory ude-tagsfile-name)
			  bee-make-tags))

;*---------------------------------------------------------------------*/
;*    bee-safe-read ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-safe-read (port)
  (condition-case err
      (read port)
    (error port)))

;*---------------------------------------------------------------------*/
;*    bee-find-afile-module ...                                        */
;*    -------------------------------------------------------------    */
;*    Searches an afile file in order to find implementation file      */
;*    for MODULE.                                                      */
;*    -------------------------------------------------------------    */
;*    The afile must have been created or loaded before that function  */
;*    is entered (in order to let caller handle afile error).          */
;*---------------------------------------------------------------------*/
(defun bee-find-afile-module (module)
  (let* ((cmd (concat ude-uncase-grep " \"(" module " \" "
		      ude-root-directory ude-afile-name))
	 (res (shell-command-to-string cmd)))
    (if (> (length res) 0)
	(let ((cell (bee-safe-read res)))
	  (if (consp cell)
	      (car (cdr cell))
	    nil))
      nil)))
  
;*---------------------------------------------------------------------*/
;*    bee-find-module-nokeymap ...                                     */
;*    -------------------------------------------------------------    */
;*    Find a module implementation.                                    */
;*    This function is not bound to any key binding.                   */
;*---------------------------------------------------------------------*/
(defun bee-find-module-nokeymap (module)
  "Find a module implementation."
  (interactive (ude-interactive-ident (point) "Module: "))
  (if (not (bee-find-afile-create-p))
      (ude-error "Find module: no afile file loaded.")
    (let ((fname (bee-find-afile-module module)))
      (if fname
	  (let ((iname (bee-interface-builder-module-p module fname)))
	    ;; before opening the module, we look if this module
	    ;; is generated by an interface builder and if we should
	    ;; spawn a new interface builder instead of poping an
	    ;; emacs buffer
	    (if iname
		(if (bee-find-builder-editing iname)
		    (message "A builder is already editing %S" iname)
		  (bee-run-builder iname))
	      (let ((buffer (find-file-other-frame
			     (if (file-name-absolute-p fname)
				 fname
			       (concat ude-root-directory fname)))))
		(if (bufferp buffer)
		    (progn
		      (set-buffer buffer)
		      (goto-char (point-min))
		      (if (search-forward "(module" (point-max) t)
			  (beginning-of-line))))))
	    module)
	(ude-error "Find module: can't find module %S" module)))))

;*---------------------------------------------------------------------*/
;*    bee-find-module ...                                              */
;*    -------------------------------------------------------------    */
;*    Find a module implementation.                                    */
;*---------------------------------------------------------------------*/
(defun bee-find-module (module)
  "Find a module implementation."
  (interactive (ude-interactive-ident (point) "Module: "))
  (bee-find-module-nokeymap module))

;*---------------------------------------------------------------------*/
;*    bee-find-interface-builder-module ...                            */
;*---------------------------------------------------------------------*/
(defun bee-find-interface-builder-module (module)
  "Find a module implementation."
  (interactive (ude-interactive-ident (point) "Module: "))
  (if (not (bee-find-afile-create-p))
      (ude-error "Find module: no afile file loaded.")
    (let ((fname (bee-find-afile-module module)))
      (if fname
	  (let ((iname (bee-interface-builder-module-p module fname)))
	    ;; before opening the module, we look if this module
	    ;; is generated by an interface builder and if we should
	    ;; spawn a new interface builder instead of poping an
	    ;; emacs buffer
	    (if iname
		(if (bee-find-builder-editing iname)
		    (message "A builder is already editing %S" iname)
		  (bee-run-builder iname))
	      (ude-error "No such builder module %S" module)))))))
  
;*---------------------------------------------------------------------*/
;*    bee-find-module-list ...                                         */
;*    -------------------------------------------------------------    */
;*    If the afile exists, that function return the list of the        */
;*    defined modules. This function is used in bee-keymap.            */
;*---------------------------------------------------------------------*/
(defun bee-find-module-list ()
  (if (file-exists-p (concat ude-root-directory ude-afile-name))
      (let ((old-buffer (find-buffer-visiting (concat ude-root-directory
						      ude-afile-name))))
	(if (bufferp old-buffer)
	    (save-excursion
	      (set-buffer old-buffer)
	      (goto-char (point-min))
	      (sort (mapcar #'(lambda (x) (symbol-name (car x)))
			    (read old-buffer))
		    'string<))
	  (let ((buffer (find-file-noselect (concat ude-root-directory
						    ude-afile-name))))
	    (if (bufferp buffer)
		(let ((res (sort (mapcar #'(lambda (x) (symbol-name (car x)))
					 (condition-case err
					     (read buffer)
					   '()))
				 'string<)))
		  (kill-buffer buffer)
		  res)
	      '()))))
    '()))

;*---------------------------------------------------------------------*/
;*    bee-find-definition ...                                          */
;*    -------------------------------------------------------------    */
;*    Search for a variable definition. This function first searches   */
;*    for a local variable and then searches for a global one.         */
;*---------------------------------------------------------------------*/
(defun bee-find-definition (var)
  "Find a variable or class definition."
  (interactive (ude-interactive-ident (point) "Identifier: "))
  (if (not (bee-find-local-definition var))
      ;; there is no local variable called VAR in the context
      (if (not (bee-find-tagsfile-create-p))
	  (ude-error "Find variable: no tags file loaded.")
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (condition-case err
	      (find-tag-other-frame-if-new (bee-find-var-regexp var) nil t)
	    (error (apply 'ude-error "Can't find %S definition (%S)"
			  var
			  (cdr err))))))))

;*---------------------------------------------------------------------*/
;*    bee-tags-find-variable-noselect ...                              */
;*    -------------------------------------------------------------    */
;*    Find a buffer defining VAR in a buffer that is not selected.     */
;*---------------------------------------------------------------------*/
(defun bee-tags-find-variable-noselect (var)
  "Find a variable or class definition."
  (interactive "sVariable: ")
  (if (not (bee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (message "Seeking definition `%S'" var)
	  (find-tag-noselect (bee-find-var-regexp var) nil t))
      (error (apply 'ude-error "Can't find %S variable (%S)"
		    var
		    (cdr err))))))

;*---------------------------------------------------------------------*/
;*    bee-tags-entry-exists-p ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-tags-entry-exists-p (var)
  "Checks if the VAR exists in the tags table"
  (if (not (bee-find-tagsfile-create-p))
      nil
    (let ((buffer (current-buffer)))
      (condition-case nil
	  (let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
		(tags-add-tablens t))
	    (find-tag-noselect (bee-find-var-regexp var) nil t)
	    (set-buffer buffer)
	    var)
	(error
	 (progn
	   (set-buffer buffer)
	   nil))))))
  
;*---------------------------------------------------------------------*/
;*    bee-afile-entry-exists-p ...                                     */
;*---------------------------------------------------------------------*/
(defun bee-afile-entry-exists-p (module)
  "Checks if the VAR exists in the afile table"
  (if (not (bee-find-afile-create-p))
      nil
    (let ((buffer (current-buffer)))
      (condition-case nil
	  (let* ((cmd (concat ude-uncase-grep " \"" module " \" "
			      ude-root-directory ude-afile-name))
		 (out (shell-command-to-string cmd))
		 (res (if (> (length out) 0)
			  (consp (bee-safe-read out))
			nil)))
	    (set-buffer buffer)
	    res)
	(error
	 (progn
	   (set-buffer buffer)
	   nil))))))
  
;*---------------------------------------------------------------------*/
;*    bee-tags-find-variable ...                                       */
;*    -------------------------------------------------------------    */
;*    Find a variable or class definition.                             */
;*---------------------------------------------------------------------*/
(defun bee-tags-find-variable (var)
  "Find a variable or class definition."
  (interactive (ude-interactive-ident (point) "Variable: "))
  (if (not (bee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (find-tag-other-frame-if-new (bee-find-var-regexp var) nil t))
      (error (apply 'ude-error "Can't find %S variable (%S)"
		    var
		    (cdr err))))))

;*---------------------------------------------------------------------*/
;*    bee-tags-find-class ...                                          */
;*---------------------------------------------------------------------*/
(defun bee-tags-find-class (class)
  "Find a class definition."
  (interactive (ude-interactive-ident (point) "Class: "))
  (if (not (bee-find-tagsfile-create-p))
      (ude-error "Find class: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (find-tag-other-frame-if-new (concat "class " class))
	  class)
      (error (apply 'ude-error "Can't find %S class (%S)"
		    class (cdr err))))))

;*---------------------------------------------------------------------*/
;*    bee-get-symbol ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-get-symbol (point)
  (interactive)
  (save-excursion
    (goto-char point)
    (let* ((beg (with-syntax-table emacs-lisp-mode-syntax-table
		  (save-excursion
		    (backward-sexp 1)
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point))))
	   (end (progn (forward-sexp 1) (point))))
      (buffer-substring-no-properties beg end))))
	 
;*---------------------------------------------------------------------*/
;*    bee-tags-find ...                                                */
;*    -------------------------------------------------------------    */
;*    This function is a dispatcher. It checks (with the current       */
;*    position) if it should look for a module or a variable           */
;*    definition. It looks for a module if (point) is inside an        */
;*    `import' or `use' module clause.                                 */
;*---------------------------------------------------------------------*/
(defun bee-tags-find ()
  "Find a variable/class/module definition."
  (interactive)
  (let ((class (let ((symbol (bee-get-symbol (point))))
		 (if (consp symbol)
		     (if (save-excursion
			   (goto-char (car symbol))
			   (re-search-forward ".*::\\(.+\\)" (cdr symbol) t))
			 (buffer-substring (match-beginning 1)
					   (match-end 1))
		       nil)
		   nil))))
    (if (stringp class)
	(bee-tags-find-class class)
      (let* ((pos (point))
	     (ident (ude-fetch-identifier pos)))
	(if (stringp ident)
	    (if (bee-module-declaration-p pos "(\\(import\\|use\\)[ \n]")
		(bee-find-module ident)
	      (bee-tags-find-variable ident))
	  (let ((ident (read-string "Find definition: ")))
	    (if (not (string= ident ""))
		(bee-tags-find-variable ident))))))))

;*---------------------------------------------------------------------*/
;*    bee-tags-find/ident ...                                          */
;*    -------------------------------------------------------------    */
;*    This function is a dispatcher. It checks (with the current       */
;*    position) if it should look for a module or a variable           */
;*    definition. It looks for a module if (point) is inside an        */
;*    `import' or `use' module clause.                                 */
;*---------------------------------------------------------------------*/
(defun bee-tags-find/ident (pos ident)
  "Find an ident variable/class/module definition."
  (interactive)
  (let ((class (let ((symbol ident))
		 (if (consp symbol)
		     (if (save-excursion
			   (goto-char (car symbol))
			   (re-search-forward ".*::\\(.+\\)" (cdr symbol) t))
			 (buffer-substring (match-beginning 1)
					   (match-end 1))
		       nil)
		   nil))))
    (if (stringp class)
	(bee-tags-find-class class)
      (progn
	(if (stringp ident)
	    (if (bee-module-declaration-p pos "(\\(import\\|use\\)[ \n]")
		(bee-find-module ident)
	      (bee-tags-find-variable ident))
	  (let ((ident (read-string "Find definition: ")))
	    (if (not (string= ident ""))
		(bee-tags-find-variable ident))))))))

;*---------------------------------------------------------------------*/
;*    bee-tags-find-next ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-tags-find-next ()
  "Find a variable/class/module definition."
  (interactive)
  (if (not (bee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (find-tag nil t))
      (error (apply 'ude-error "No more definition %S" (cdr err))))))

;*---------------------------------------------------------------------*/
;*    bee-tag-find ...                                                 */
;*---------------------------------------------------------------------*/
(defun bee-tag-find ()
  "Find a variable or class definition."
  (interactive)
  (if (not (bee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (let* ((ident (ude-fetch-then-request-identifier (point) "Binding: ")))
      (condition-case err
	  (let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
		(tags-add-tables t))
	    (find-tag (bee-find-var-regexp ident) nil t))
	(error (apply 'ude-error "Can't find %S definition (%S)"
		      ident (cdr err)))))))

;*---------------------------------------------------------------------*/
;*    bee-in-comment-p ...                                             */
;*    -------------------------------------------------------------    */
;*    Is point located inside a comment. Instead of searching for      */
;*    a ";" we could possibly use the face property to check if        */
;*    at the current point location the text is highlighted for        */
;*    comment. For a language like C it would probably be a better     */
;*    way to proceed.                                                  */
;*---------------------------------------------------------------------*/
(defun bee-in-comment-p (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (search-forward ";" pos t)))
  
;*---------------------------------------------------------------------*/
;*    bee-tags-find-or-info ...                                        */
;*    -------------------------------------------------------------    */
;*    This function search for a function definition. If no definition */
;*    is found, then the documentation is searched.                    */
;*---------------------------------------------------------------------*/
(defun bee-tags-find-or-info (pos ident)
  "Find an ident variable/class/module definition."
  (interactive)
  (if (or (bee-tags-entry-exists-p ident)
	  (bee-afile-entry-exists-p ident))
      (bee-tags-find/ident pos ident)
    (bee-doc-ident ident)))

;*---------------------------------------------------------------------*/
;*    bee-tags-find-if-exists ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-tags-find-if-exists (pos ident)
  "Find a variable without error if the variable does not exist."
  (interactive)
  (if (or (bee-tags-entry-exists-p ident)
	  (bee-afile-entry-exists-p ident))
      (progn (bee-tags-find/ident pos ident) t)
    nil))

;*---------------------------------------------------------------------*/
;*    bee-find-var-regexp ...                                          */
;*---------------------------------------------------------------------*/
(defun bee-find-var-regexp (ident)
  (concat " [(]?" (regexp-quote ident) "\\(?: \\|[::][^]+\\)?[]"))
