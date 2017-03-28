;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-mode.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:49:23 1998                          */
;*    Last change :  Mon Dec  3 16:04:40 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bee mode declaration.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-mode)
(require 'font-lock)
(require 'ude-custom)
(require 'ude-config)
(require 'ude-autoload)
(require 'bmacs-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-autoload)
(require 'bee-config)
(require 'bee-indent)
(require 'bee-toolbar)
(require 'bee-flock)
(require 'bee-keymap)
(require 'bee-doc)
(require 'bug)

;*---------------------------------------------------------------------*/
;*    bee-mode-syntax-table ...                                        */
;*---------------------------------------------------------------------*/
(defvar bee-mode-syntax-table (make-syntax-table) "")

;*---------------------------------------------------------------------*/
;*    bee-init-syntax-table ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-init-syntax-table ()
  (let ((i 0)
	(local-syntax-table (syntax-table)))
    ;; Default is atom-constituent.
    (while (< i 256)
      (modify-syntax-entry i "_   " local-syntax-table)
      (setq i (1+ i)))

    ;; Word components.
    (setq i ?0)
    (while (<= i ?9)
      (modify-syntax-entry i "w   " local-syntax-table)
      (setq i (1+ i)))
    (setq i ?A)
    (while (<= i ?Z)
      (modify-syntax-entry i "w   " local-syntax-table)
      (setq i (1+ i)))
    (setq i ?a)
    (while (<= i ?z)
      (modify-syntax-entry i "w   " local-syntax-table)
      (setq i (1+ i)))
    (modify-syntax-entry ?* "w   " local-syntax-table)
    (modify-syntax-entry ?@ "w   " local-syntax-table)
    (modify-syntax-entry ?! "w   " local-syntax-table)
    (modify-syntax-entry ?? "w   " local-syntax-table)
    (modify-syntax-entry ?= "w   " local-syntax-table)
    (modify-syntax-entry ?< "w   " local-syntax-table)
    (modify-syntax-entry ?> "w   " local-syntax-table)
    (modify-syntax-entry ?+ "w   " local-syntax-table)
    (modify-syntax-entry ?* "w   " local-syntax-table)
    (modify-syntax-entry ?~ "w   " local-syntax-table)
    (modify-syntax-entry ?$ "w   " local-syntax-table)
    (modify-syntax-entry ?% "w   " local-syntax-table)
    (modify-syntax-entry ?^ "w   " local-syntax-table)
    (modify-syntax-entry ?\\ "w   " local-syntax-table)
    (modify-syntax-entry ?. ".   " local-syntax-table)
    (if (eq bee-identifier-syntax 'bigloo)
	(modify-syntax-entry ?. "w   " local-syntax-table)
      (modify-syntax-entry ?. ".   " local-syntax-table))
    (modify-syntax-entry ?_ "w   " local-syntax-table)

    ;; Whitespace
    (modify-syntax-entry ?\t "    " local-syntax-table)
    (modify-syntax-entry ?\n ">   " local-syntax-table)
    (modify-syntax-entry ?\f "    " local-syntax-table)
    (modify-syntax-entry ?\r "    " local-syntax-table)
    (modify-syntax-entry ?  "    " local-syntax-table)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?[ "(]  " local-syntax-table)
    (modify-syntax-entry ?] ")[  " local-syntax-table)
    
    (modify-syntax-entry ?{ "(}  " local-syntax-table)
    (modify-syntax-entry ?} "){  " local-syntax-table)
    
    (modify-syntax-entry ?\| "    " local-syntax-table)

    ;; Other atom delimiters
    (modify-syntax-entry ?\( "()  " local-syntax-table)
    (modify-syntax-entry ?\) ")(  " local-syntax-table)
    (if (< bmacs-emacs-version 22)
	(modify-syntax-entry ?\; "<   " local-syntax-table)
      (modify-syntax-entry ?\; "< 2 " local-syntax-table))
    (modify-syntax-entry ?\" "\"    " local-syntax-table)
    (modify-syntax-entry ?' "'   " local-syntax-table)
    (modify-syntax-entry ?` "'   " local-syntax-table)
    (modify-syntax-entry ?\: "'   " local-syntax-table)

    ;; Special characters
    (modify-syntax-entry ?, "'   " local-syntax-table)
    (if (< bmacs-emacs-version 22)
	(modify-syntax-entry ?# "'   " local-syntax-table)
      (modify-syntax-entry ?# "' 14" local-syntax-table))
    (modify-syntax-entry ?\\ "\\   " local-syntax-table)

    ;; legal Bigloo identifier chars that are not recognized by the \w syntax
    (setq ude-extra-identifier-chars "[-_/]")))

;*---------------------------------------------------------------------*/
;*    bee-sexp-comment-syntax-table                                    */
;*---------------------------------------------------------------------*/
(defconst bee-sexp-comment-syntax-table
  (let ((st (make-syntax-table bee-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

;*---------------------------------------------------------------------*/
;*    bee-mode-abbrev-table ...                                        */
;*---------------------------------------------------------------------*/
(defvar bee-mode-abbrev-table nil "")
(define-abbrev-table 'bee-mode-abbrev-table ())

;*---------------------------------------------------------------------*/
;*    bee keymap ...                                                   */
;*    -------------------------------------------------------------    */
;*    For a reason that I don't know these variables cannot be         */
;*    defined inside BEE-KEYMAP otherwise emacs don't succeed at       */
;*    loading the present file!                                        */
;*---------------------------------------------------------------------*/
(defvar bee-mode-map (make-sparse-keymap))

;*---------------------------------------------------------------------*/
;*    bee-mode-variables ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-mode-variables ()
  (setq local-abbrev-table bee-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
;*   (make-local-variable 'fill-paragraph-function)                    */
;*   (setq fill-paragraph-function bee-fill-paragraph-function)        */
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'bee-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'bee-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t))

;*---------------------------------------------------------------------*/
;*    bee-compile ...                                                  */
;*---------------------------------------------------------------------*/
(defun bee-compile ()
  (interactive)
  (ude-mode-compile bee-compilation-error-regexp-alist
		    bee-compilation-font-lock-keywords))

;*---------------------------------------------------------------------*/
;*    bee-generate/update-makefile ...                                 */
;*---------------------------------------------------------------------*/
(defun bee-generate/update-makefile ()
  (interactive)
  (if (not (file-exists-p (concat ude-root-directory ude-makefile)))
      (ude-generate-makefile bee-bmake-application-option)
    (ude-update-makefile)))

;*---------------------------------------------------------------------*/
;*    bee-find-toplevel-sexp ...                                       */
;*    -------------------------------------------------------------    */
;*    Find the toplevel POS is in.                                     */
;*---------------------------------------------------------------------*/
(defun bee-find-toplevel-sexp (pos)
  (save-excursion
    (let* ((sexp nil)
	   (res  (id-select-sexp pos))
	   (old  pos))
      (while (and (consp res) (not (eq (car res) old)))
	(setq sexp res)
	(setq old (car res))
	(setq res (id-select-sexp (car res))))
      sexp)))

;*---------------------------------------------------------------------*/
;*    bee-mode ...                                                     */
;*---------------------------------------------------------------------*/
(defun bee-mode ()
  "Major mode for editing Bigloo code.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{bee-mode-map}
Entry to this mode calls the value of bee-mode-hook
if that value is non-nil."
  (interactive)
  (kill-all-local-variables)
  ;; mode declaration
  (setq major-mode 'bee-mode)
  (setq mode-name "Bee")
  (use-local-map bee-mode-map)
  ;; we setup the project root directory
  (ude-auto-set-root-directory bee-root)
  ;; syntax table
  (set-syntax-table bee-mode-syntax-table)
  (bee-init-syntax-table)
  ;; imenu
  (set (make-local-variable 'imenu-case-fold-search) t)
  (setq imenu-generic-expression bee-imenu-generic-expression)
  (set (make-local-variable 'imenu-syntax-alist)
       '(("+-*/.<>=?!$%_&~^:" . "w")))
  ;; global buffer local variables
  (bee-mode-variables)
  ;; online documentation initialization
  (bee-doc-initialize)
  ;; bmake initialization
  (setq ude-makemake bee-bmake)
  ;; compilation initialization
  (setq ude-mode-menu-compile 'bee-compile)
  ;; keymap bindings
  (bee-keymap-init)
  ;; parenthesis blinking init
  (ude-paren-init)
  ;; profile highlighting
  (setq ude-profile-success-hook 'bee-profile-success-hook)
  ;; starting font-lock
  (bee-set-font-lock)
  ;; the doc source fontification
  (ude-fontify-doc-source (current-buffer))
  ;; the toolbar
  (bee-toolbar-init)
  ;; bee jump-identifier
  (if bee-tags-balloon-p
      (ude-tags-balloon-start 'bee-ident
			      'bee-in-comment-p
			      'bee-tags-find-or-info
			      #'(lambda (point ident)
				  (popup-menu
				   (cons ident
					 (cdr (car (bee-find-menu))))))))
  ;; repl initialization
  (add-hook 'ude-repl-hooks
	    #'(lambda () (set-syntax-table bee-mode-syntax-table))
	    '()
	    t)
  ;; the bee hook
  (run-hooks 'bee-mode-hook)
  ;; turn on font-lock
  (if ude-font-lock-p
      (font-lock-mode t)
    (font-lock-mode nil)))

;*---------------------------------------------------------------------*/
;*    bee-set-font-lock ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-set-font-lock ()
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (if (< bmacs-emacs-version 22)
      (setq font-lock-defaults '(bee-font-lock-keywords))
    (setq font-lock-defaults '((bee-font-lock-keywords)
			       nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
			       beginning-of-defun
			       (font-lock-mark-block-function . mark-defun)
			       (font-lock-syntactic-face-function
				. bee-font-lock-syntactic-face-function)
			       (parse-sexp-lookup-properties . t)
			       (font-lock-extra-managed-props syntax-table)))))

;*---------------------------------------------------------------------*/
;*    bee-font-lock-syntactic-face-function ...                        */
;*    -------------------------------------------------------------    */
;*    This is a verbatim copy of what I have found in regular          */
;*    scheme.el mode. Don't ask me the meaning of all this. It is      */
;*    to make the #; operational.                                      */
;*---------------------------------------------------------------------*/
(defun bee-font-lock-syntactic-face-function (state)
  (when (and (null (nth 3 state))
             (eq (char-after (nth 8 state)) ?#)
             (eq (char-after (1+ (nth 8 state))) ?\;))
    ;; It's a sexp-comment. Tell parse-partial-sexp where it ends.
    (save-excursion
      (let ((pos (point))
            (end
             (condition-case err
                 (let ((parse-sexp-lookup-properties nil))
                   (goto-char (+ 2 (nth 8 state)))
                   ;; FIXME: this doesn't handle the case where the sexp
                   ;; itself contains a #; comment.
                   (forward-sexp 1)
                   (point))
               (scan-error (nth 2 err)))))
        (when (< pos (- end 2))
          (put-text-property pos (- end 2)
                             'syntax-table bee-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

;*---------------------------------------------------------------------*/
;*    bee-gdb ...                                                      */
;*---------------------------------------------------------------------*/
(defun bee-gdb (command-line)
  (interactive
   (list (let ((make-entry (ude-fetch-makefile-binary-entry)))
	   (if (stringp make-entry)
	       (concat "bdb " make-entry)
	     (read-from-minibuffer (concat "Run bdb" "(like this): ")
				   (if (consp bug-history)
				       (car bug-history)
				     "bdb a.out")
				   bug-minibuffer-local-map nil
				   '(bug-history . 1))))))
  (gdb command-line))

;*---------------------------------------------------------------------*/
;*    bee-debug ...                                                    */
;*---------------------------------------------------------------------*/
(defun bee-debug ()
  (interactive)
  (cond
   ((eq bee-debugger 'bugloo)
    (bugloo-other-frame))
   ((eq bee-debugger 'bdb)
    (call-interactively 'bee-gdb))
   (t
    (popup-menu
     (list "Debugger"
	   ["bdb" bee-gdb t]
	   ["bugloo" bugloo-other-frame t])))))
