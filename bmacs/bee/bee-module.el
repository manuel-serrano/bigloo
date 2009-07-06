;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-module.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 11 11:58:51 1998                          */
;*    Last change :  Tue Sep 20 08:35:23 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Module handling                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-module)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-config)
(require 'bee-autoload)
(require 'ude-autoload)

;*---------------------------------------------------------------------*/
;*    bee-module-declaration-p ...                                     */
;*    -------------------------------------------------------------    */
;*    Is POS inside a module declaration?                              */
;*    CLAUSE is the kind of clause we want to parse (for instance it   */
;*    could be "(\\(import\\|use\\)[ \n]")                             */
;*---------------------------------------------------------------------*/
(defun bee-module-declaration-p (pos clause)
  (save-excursion
    (let ((sexp (id-select-sexp pos)))
      (and (consp sexp)
	   (let ((start (car sexp)))
	     (goto-char start)
	     (if (looking-at clause)
		 (let ((sexp (id-select-sexp start)))
		   (and (consp sexp)
			(let ((start (car sexp)))
			  (goto-char start)
			  (looking-at "(module[ \n]"))))))))))

;*---------------------------------------------------------------------*/
;*    bee-find-domestic-definition ...                                 */
;*    -------------------------------------------------------------    */
;*    This function search for a binding definition.                   */
;*---------------------------------------------------------------------*/
(defun bee-find-domestic-definition (ident)
  (save-excursion
    (goto-char (point-min))
    (let ((reg-exp (concat
		    "^(\\(define\\|define-generic\\|define-inline\\)[ \t\n]+(?"
		    (regexp-quote ident)
		    "\\(::" (ude-ident-regexp) "\\|\\>\\)")))
      (re-search-forward reg-exp (point-max) t))))

;*---------------------------------------------------------------------*/
;*    bee-export-binding/class ...                                     */
;*---------------------------------------------------------------------*/
(defun bee-export-binding/class (class tagname)
  (save-excursion
    (if (not (bee-find-domestic-definition tagname))
	;; we have found no definition
	(error (format "Can't find binding [%s]" tagname))
      (progn
	(goto-char (match-beginning 0))
	(cond
	 ((and (memq class '(binding function))
	       (re-search-forward "^\\=(define-\\(generic\\|inline\\)[ \t\n]+("
				  (point-max) t))
	  ;; this is an inline or generic function definition
	  (let ((start (- (match-end 0) 1))
		(end   (condition-case ()
			   (progn
			     (backward-char 1)
			     (forward-sexp 1)
			     (point))
			 (error nil))))
	    (if end
		(bee-add-export (concat "("
					(buffer-substring (match-beginning 1)
							  (match-end 1))
					" "
					(buffer-substring (+ 1 start) end)))
	      (ude-error (format "Incorrect definition for %s" tagname)))))
	 ((and (memq class '(binding function))
	       (re-search-forward "^\\=(define[ \t\n]+(" (point-max) t))
	  ;; this is a function definition
	  (let ((start (- (match-end 0) 1))
		(end   (condition-case ()
			   (progn
			     (backward-char 1)
			     (forward-sexp 1)
			     (point))
			 (error nil))))
	    (if end
		(bee-add-export (buffer-substring start end))
	      (ude-error (format "Incorrect definition for %s" tagname)))))
	 ((and (memq class '(binding variable))
	       (re-search-forward "^\\=(define[ \t\n]+" (point-max) t))
	  ;; this is a variable definition
	  (let ((start (match-end 0))
		(end   (condition-case ()
			   (progn
			     (forward-sexp 1)
			     (point))
			 (error nil))))
	    (if end
		(bee-add-export (buffer-substring start end))
	      (ude-error (format "Incorrect definition for %s" tagname)))))
	 (t
	  (ude-error (format "Can't find binding [%s]" tagname))))))))

;*---------------------------------------------------------------------*/
;*    bee-export-binding ...                                           */
;*    -------------------------------------------------------------    */
;*    This function search for a global binding definition and it      */
;*    exports it as a function is the binding is a lambda form or      */
;*    it exports it as a variable if the definition looks like a       */
;*    variable definition.                                             */
;*---------------------------------------------------------------------*/
(defun bee-export-binding (tagname)
  (interactive (ude-interactive-ident (point) "Export binding: "))
  (bee-export-binding/class 'binding tagname))

;*---------------------------------------------------------------------*/
;*    bee-export-function ...                                          */
;*    -------------------------------------------------------------    */
;*    Looks for a function (or a generic function) and exports it.     */
;*---------------------------------------------------------------------*/
(defun bee-export-function (tagname)
  (interactive (ude-interactive-ident (point) "Export function: "))
  (bee-export-binding/class 'function tagname))

;*---------------------------------------------------------------------*/
;*    bee-export-variable ...                                          */
;*    -------------------------------------------------------------    */
;*    Looks for a variable definition and exports it.                  */
;*---------------------------------------------------------------------*/
(defun bee-export-variable (tagname)
  (interactive (ude-interactive-ident (point) "Export variable: "))
  (bee-export-binding/class 'variable tagname))

;*---------------------------------------------------------------------*/
;*    bee-insert-clause ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-insert-clause (clause pos create-p definition)
  (goto-char pos)
  (insert "\n")
  (if create-p (insert (format "(%s " clause)))
  (indent-for-tab-command)
  (insert definition)
  (if create-p (insert ")"))
  (message "%S added to %s clauses" definition clause))

;*---------------------------------------------------------------------*/
;*    bee-add-clause ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-add-clause (clause definition)
  (goto-char (point-min))
  (if (re-search-forward "(module[ \t\n]" (point-max) t)
      (progn
	(goto-char (match-beginning 0))
	(let ((end (condition-case ()
		       (save-excursion
			 (forward-sexp 1)
			 (point))
		     (error nil))))
	  (if end
	      ;; we search for wanted clause (export or import)
	      (if (let ((eval t)
			(extern t))
		    (while (and (or eval extern)
				(re-search-forward
				 (format "(%s[ \t\n]" (regexp-quote clause))
				 end
				 t))
		      (let ((match-0 (match-beginning 0)))
			(setq eval (bee-module-declaration-p
				    match-0
				    "(eval[ \n]"))
			(setq extern (bee-module-declaration-p
				      match-0
				      "(extern[ \n]"))))
		    (and (not eval) (not extern)))
		  (progn
		    (goto-char (match-beginning 0))
		    ;; it may be the export clause of an eval form...
		    (condition-case ()
			(progn
			  (forward-sexp 1)
			  (backward-char 1)
			  (bee-insert-clause clause (point) nil definition))
		       (ude-error (format "Illegal %s clause" clause))))
		(progn
		  (goto-char end)
		  (backward-char 1)
		  (bee-insert-clause clause (point) t definition)))
	    (ude-error "Illegal module clause"))))))

;*---------------------------------------------------------------------*/
;*    bee-add-export ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-add-export (definition)
  (bee-add-clause "export" definition))

;*---------------------------------------------------------------------*/
;*    bee-add-import ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-add-import (definition)
  (bee-add-clause "import" definition))

;*---------------------------------------------------------------------*/
;*    bee-export-definition ...                                        */
;*    -------------------------------------------------------------    */
;*    Export a definition.                                             */
;*---------------------------------------------------------------------*/
(defun bee-export-definition (ident)
  "Export a definition."
  (interactive (ude-interactive-ident (point) "Export: "))
  (if (and (stringp ident) (not (string= ident "")))
      (bee-export-binding/class 'binding ident)))
  
;*---------------------------------------------------------------------*/
;*    bee-import-c-file ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-import-c-file (file)
  (interactive "FImport C file: ")
  (if (stringp file)
      (let* ((cmd (concat bee-cigloo " -s -no-directives " file))
	     (res (shell-command-to-string cmd)))
	(if (string-match ")[ \n\t]*$" res)
	    ;; no error occured, we insert the result
	    (save-excursion
	      (goto-char (point-min))
	      (if (re-search-forward "(module[ \t\n]" (point-max) t)
		  (progn
		    (goto-char (match-end 0))
		    (condition-case ()
			(progn
			  (forward-sexp 1)
			  (insert "\n")
			  (save-excursion (insert res))
			  (indent-for-tab-command)
			  (bee-indent-sexp)
 			  (ude-add-makefile-entry
			   (concat (file-name-sans-extension file) ".o")
			   file))
		      (ude-error "Illegal module clause")))
		(ude-error "Can't find module clause")))
	  (progn
	    (ude-error (format "Error in C file %s" file))
	    (pop-to-buffer "*bee cigloo errors*")
	    (erase-buffer)
	    (insert res))))))

;*---------------------------------------------------------------------*/
;*    bee-module-declaration-find ...                                  */
;*    -------------------------------------------------------------    */
;*    This function search inside the module declaration a clause      */
;*    for VAR. If found, the clause and the kind (export or static)    */
;*    of the clause is returned.                                       */
;*---------------------------------------------------------------------*/
(defun bee-module-declaration-find (var)
  (save-excursion
    (goto-char (point-min))
    ;; first we search for the module declaration
    (if (re-search-forward (concat "(module[ \n\t]\\("
				   (ude-ident-regexp)
				   "+\\)")
			   (point-max) t)
	(let ((module (buffer-substring (match-beginning 1) (match-end 1)))
	      (sexp (id-select-sexp-start (match-beginning 0))))
	  (if (consp sexp)
	      ;; we have found the module clause
	      (let ((max-bound (cdr sexp))
		    (found     nil))
		(while (and (not found)
			    (re-search-forward "(\\(export\\|static\\)[ \n\t]"
					       max-bound
					       t))
		  ;; there is another defining clause
		  (let* ((start (match-beginning 0))
			 (kind (buffer-substring (match-beginning 1)
						 (match-end 1))))
		      (goto-char start)
		      (while (and (not found)
				  (re-search-forward 
				   (format "(\\(%s+[ \n\t]+\\)?%s\\(::\\|[ \n\t]\\)\\|%s\\|(export \\|(static "
					   (ude-ident-regexp)
					   (regexp-quote var)
					   (regexp-quote var))
				   max-bound t))
			(let ((s (match-string 0)))
			  (cond
			   ((string= s "(export ")
			    (setq kind "export")
			    (goto-char (match-end 0)))
			   ((string= s "(static ")
			    (setq kind "static")
			    (goto-char (match-end 0)))
			   (t
			    (setq found (cons kind (match-beginning 0)))))))))
		(cons module found))
	    (cons module nil)))
      nil)))

;*---------------------------------------------------------------------*/
;*    bee-binding-imported-p ...                                       */
;*    -------------------------------------------------------------    */
;*    This predicate is true iff variable VAR from module MODULE is    */
;*    imported in the current buffer.                                  */
;*---------------------------------------------------------------------*/
(defun bee-binding-imported-p (module var)
  (save-excursion
    (goto-char (point-min))
    ;; first we search for the module declaration
    (if (re-search-forward (concat "(module[ \n\t]\\("
				   (ude-ident-regexp)
				   "+\\)")
			   (point-max) t)
	(let ((module (buffer-substring (match-beginning 1) (match-end 1)))
	      (sexp (id-select-sexp-start (match-beginning 0)))
	      (module-id (intern module))
	      (var-id (intern var)))
	  (if (consp sexp)
	      ;; we have found the module clause
	      (let ((max-bound (cdr sexp))
		    (found     nil))
		(goto-char (car sexp))
		(while (and (not found)
			    (re-search-forward "(\\(import\\|use\\)[ \n\t]"
					       max-bound
					       t))
		  ;; there is another import clause
		  (let* ((start (match-beginning 0))
			 (clause (id-select-sexp-start start)))
		    (if (consp clause)
			(let* ((end (cdr clause))
			       (str  (buffer-substring (car clause) end))
			       (sexp (car (read-from-string str))))
			  (let ((clauses (cdr sexp)))
			    (while (and (not found) (consp clauses))
			      (let ((clause (car clauses)))
				(cond
				 ((eq clause module-id)
				  (setq found t))
				 ((and (consp clause)
				       (eq (car clause) var-id)
				       (consp (cdr clause))
				       (eq (car (cdr clause)) module-id))
				  (setq found t))
				 (t
				  (setq clauses (cdr clauses)))))))
			  (goto-char end))
		      (ude-error "Illegal import clause -- %S" module))))
		found)
	    (ude-error "Illegal module clause -- %S" module)))
      (ude-error "Illegal module clause -- %S" module))))

;*---------------------------------------------------------------------*/
;*    bee-get-module-name ...                                          */
;*---------------------------------------------------------------------*/
(defun bee-get-module-name (buffer)
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    ;; first we search for the module declaration
    (if (re-search-forward (concat "(module[ \n\t]\\(" (ude-ident-regexp)
				   "+\\)")
			   (point-max) t)
	(buffer-substring (match-beginning 1) (match-end 1))
      nil)))
  
;*---------------------------------------------------------------------*/
;*    bee-import-binding ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-import-binding (var)
  ;; first we have to find in which module this variable is defined
  "Find documentation."
  (interactive (ude-interactive-ident (point) "Import binding: "))
  (let ((current (current-buffer))
	(buffer (bee-tags-find-variable-noselect var)))
    (if (bufferp buffer)
	;; we have found the definition for this variable
	(let ((current-module (bee-get-module-name (current-buffer))))
	  (set-buffer buffer)
	  ;; then we have to find the module clause for that variable
	  (let* ((module (bee-module-declaration-find var))
		 (import-module (if (consp module)
				    (car module)
				  (ude-error
				   (format "Illegal module declaration -- %S"
					   var)))))
	    (cond
	     ((string= current-module import-module)
	      (ude-error (format "The variable is defined in the module -- %S"
				 var)))
	     ((or (null (cdr module))
		  (not (string= (car (cdr module)) "export")))
	      (ude-error (format "Variable %S is %S in %S"
				 var
				 (let ((export (car (cdr module))))
				   (if export export "static"))
				 import-module)))
	     ((bee-binding-imported-p import-module var)
	      (ude-error (format "Variable %S is already imported from %S"
				 var import-module)))
	     (t
	      (set-buffer current)
	      (let ((import (if (eq bee-import-mode 'all)
				import-module
			      (format "(%s %s)" var import-module))))
		(save-excursion
		  (bee-add-import import))))))))))
	     


