;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/bmacs/bgl-mode.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:49:23 1998                          */
;*    Last change :  Sun Jun 21 08:31:23 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Emacs bgl-mode                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bgl-mode)
(require 'font-lock)
(require 'posframe)

;*---------------------------------------------------------------------*/
;*    Configuration                                                    */
;*---------------------------------------------------------------------*/
;; bgl version
(defconst bgl-version "0.0.1"
  "*The Bgl version.")

;; bgl group
(defgroup bgl nil
  "Bgl Emacs Environment."
  :tag "Bgl"
  :prefix "bgl-"
  :group 'processes)

(defcustom bgl-suffixes '("bgl" "bgh")
  "*Bigloo source suffixes."
  :group 'bgl
  :type '(repeat (string)))

(defcustom bgl-forced-indent-regexp ";;;"
  "*The regexp that mark a forced indentation"
  :group 'bgl
  :type 'string)

(defcustom bgl-indent-on-keyword-p t
  "*Force special indentation for keywords arguments"
  :group 'bgl
  :type 'boolean)

(defcustom bgl-indent-on-quote-p t
  "*Force special indentation for quoted lists and vectors"
  :group 'bgl
  :type 'boolean)

(defcustom bgl-indent-mode 'bgl
  "*Force special indentation for quoted lists and vectors"
  :group 'bgl
  :type '(choice (const scheme) (const bgl)))

(defcustom bgl-indent-style 'left
  "*Force special indentation for quoted lists and vectors"
  :group 'bgl
  :type '(choice (const (left) (const column))))

;; flycheck configuration
(defcustom bgl-flycheck-file-extensions '("bgl" "bgh")
  "*The file extensions that automatically start flycheck"
  :group 'bgl
  :type '(repeat (string)))

(defcustom bgl-flycheck-args '("-coerce" "-dump" "module")
  "*The compiler options passed to flycheck"
  :group 'bgl
  :type '(repeat (string))
  :safe #'string-listp)

(defcustom bgl-flycheck-size-limit 100000
  "*The maximum file size for automatic flycheck start"
  :group 'bgl
  :type 'number)

(defcustom bgl-flycheck-compiler "/home/serrano/prgm/project/bigloo/5.0.x/bin/bigloo"
  "*The bigloo compiler used by flycheck"
  :group 'bgl
  :type 'string)

(defcustom bgl-flydoc-p t
  "*Enable on the fly documentation"
  :group 'bgl
  :type 'string)

(defcustom bgl-flydoc-browser nil
  "*The web browser used for the documentation"
  :group 'bgl
  :type '(choice (const 'nil) string))

(defcustom bgl-doc-dir "/home/serrano/prgm/project/bigloo/5.0.x/doc"
  "*The Bigloo documentation directory."
  :group 'bgl
  :type 'string)

(defcustom bgl-cache-dir "/tmp/serrano/cache/bigloo/5.0.0/88211ec9+/C"
  "*The Bigloo cache module directory."
  :group 'bgl
  :type 'string)

(defcustom bgl-doc-index "index.sexp"
  "*The pre-computed index of the Bigloo documentation"
  :group 'bgl
  :type 'string)

(defcustom bgl-tooltip-visibility-duration 10
  "*The number of seconds tooltips are visible"
  :group 'bgl
  :type 'number)

;; bgl fontification
(defcustom bgl-font-lock-keywords
  (list
   (cons (concat "\\(?:define\\|define-inline"
		 "\\|define-struct\\|define-record-type\\|define-record"
		 "\\|define-macro\\|define-generic\\|define-method\\|define-walk-method"
		 "\\|define-syntax\\|define-expander"
		 "\\|define-class\\|define-abstract-class\\|define-final-class"
		 "\\|define-wide-class\\|define-parameter\\)[ ]")
	 'font-lock-function-name-face)
   (list (concat "^\(\\(?:define\\|define-inline"
		 "\\|define-struct\\|define-record-type\\|define-record"
		 "\\|define-macro\\|define-generic\\|define-method\\|define-walk-method"
		 "\\|define-syntax\\|define-expander"
		 "\\|define-class\\|define-abstract-class\\|define-final-class"
		 "\\|define-wide-class\\|define-parameter"
		 "\\)[ ]\(?\\([^ \t\n]+\\)")
	 1
	 'font-lock-function-name-face)
   (list "\(\\(\\(?:module\\|interface\\)[ ]+[^ \n]+\\)[ \t\n]"
         1
	 'bgl-font-lock-face-1)
   (list "\(\\(directives\\)"
         1
	 'bgl-font-lock-face-1)
   (list "[']\\([^ ),[(#]\\([^ \n\t[()#]\\|]\\)*\\)"
	 1
	 'font-lock-string-face)
   (cons "\\(?:::[^ \n)]+\\|#![a-zA-Z]+\\|:,@\\|[`,]\\)"
	 'bgl-font-lock-face-4)
   (cons "[ \n\t(]:[^ :\n\t]+\\|[^ :\n\t(]+:[ \n\t)]"
	 'bgl-font-lock-face-11)
   (list (concat "\(\\(type\\|export\\|import\\|option\\|eval\\|eval!\\|main\\|with"
		 "\\|from\\|static\\|use\\|library"
		 "\\|include\\|foreign\\|extern\\|java\\|wasm\\|Cforeign"
		 "\\|require:\\|provide:\\)[ \t\n]")
	 1
	 'font-lock-type-face)
   (list (concat "\(\\(class\\|wide-class\\|final-class\\|abstract-class"
		 "\\|generic\\|inline\\|macro\\|expander\\|syntax"
		 "\\|infix[ ]macro\\|cnst[ ]macro\\)[ \t\n]")
	 1
	 'bgl-font-lock-face-3)
   (cons "~\\|with-hop" 'bgl-font-lock-face-8)
   (cons "[$][^( \t\n]*" 'bgl-font-lock-face-10)
   (list (concat "\(\\(let\\|let[*]\\|letrec\\|letrec[*]\\|co-instantiate"
		 "\\|let-values\\|let*-values"
		 "\\|set[!]\\|with-access\\|instantiate\\|duplicate"
		 "\\|widen[!]\\|shrink!\\|lambda\\|service\\|labels"
		 "\\|let-syntax\\|letrec-syntax"
		 "\\|regular-grammar\\|lalr-grammar"
		 "\\|if\\|when\\|unless\\|begin\\|case\\|cond\\|else"
		 "\\|args-parse\\|multiple-value-bind\\|values"
		 "\\|match-case\\|match-lambda\\|event-case\\|on-event"
		 "\\|string-case\\|syntax-rules\\)[ :\n\t]")
	 1
	 'font-lock-keyword-face)
   (cons "\\[assert[^\] \n]*\\]\\|\(assert[^\) \n]*\)"
	 'bgl-font-lock-face-2)
   (list (concat "\(\\(error\\|error/location\\|error/source\\|warning\\|pragma\\|trace\\|"
		 "with-trace\\|trace-item\\|when-trace\\|"
		 "bind-exit\\|call/cc\\|try\\|unwind-protect\\|"
		 "with-exception-handler\\|with-handler\\|with-alarm\\|current-exception-handler\\|raise\\|"
		 "profile\\|profile/gc\\|delay\\|force\\)"
		 "[ \n\t:]")
	 1
	 'bgl-font-lock-face-8)
   (cons "cond-expand"
	 'bgl-font-lock-face-compile)
   (list (concat "\(\\(make-mutex\\>\\|mutex-lock!\\>\\|mutex-unlock!\\>"
		 "\\|make-thread\\>\\|thread-start!\\>\\|thread-yield!\\>"
		 "\\|thread-start-joinable!\\>"
		 "\\|make-condition-variable\\>"
		 "\\|condition-variable-wait!\\>"
		 "\\|condition-variable-signal!\\>"
		 "\\|condition-variable-broadcast!\\>"
		 "\\|thread-sleep!\\>\\|thread-join!\\>"
		 "\\|thread-terminate!\\>\\|thread-suspend!\\>"
		 "\\|thread-resume!\\>\\|thread-yield!\\>"
		 "\\|thread-await!\\>\\|thread-await[*]!\\>"
		 "\\|thread-await-values!\\>\\|thread-await-values[*]!\\>"
		 "\\|thread-get-values!\\>\\|thread-get-values[*]!\\>"
		 "\\|thread-specific-set!\\>\\|thread-specific\\>"
		 "\\|thread-cleanup-set!\\>\\|thread-cleanup\\>"
		 "\\|thread-name-set!\\>\\|thread-name\\>\\|thread[?]\\>"
		 "\\|scheduler-react!\\>\\|scheduler-start!\\>"
		 "\\|broadcast!\\>\\|scheduler-broadcast!\\>"
		 "\\|current-scheduler\\>\\|current-time\\>"
		 "\\|default-scheduler\\>\\|with-scheduler\\>"
		 "\\|scheduler-terminate!\\>\\|scheduler-instant\\>"
		 "\\|scheduler[?]\\>"
		 "\\|make-scheduler\\>\\|current-thread\\>"
		 "\\|make-asynchronous-signal\\>\\|with-lock\\(-uw\\)?\\|synchronize\\>\\)")
	 1
	 'bgl-font-lock-face-8)
   (list (concat "\(\\(sqlite-format\\>\\|sqlite-eval\\>\\|sqlite-exec\\>"
		 "\\|sqlite-map\\|sqlite-close\\>"
		 "\\|sqlite-table-informations\\>"
		 "\\|sqlite-table-number-of-rows\\>"
		 "\\|sqlite-table-name-of-columns\\>"
		 "\\|sqlite-name-of-tables\\>"
		 "\\|sqlite-dump\\>\\|sqlite-dump-table\\>"
		 "\\|sqlite-last-insert-rowid\\>\\)")
	 1
	 'bgl-font-lock-face-9)
   (list "\(\\([a-zA-Z0-9_!=%$@^*<>?/-]+&\\)[ \t\n)]"
	 1
	 'bgl-font-lock-face-7)
   (list "\(\\(<[^> \t\n]+>\\)[ \n\t)]"
	 1
	 'bgl-font-lock-face-12)
   (cons "#;"
	 'font-lock-comment-face))
  "The Bgl font-lock specification."
  :group 'bgl)

;; key bindings
(defcustom bgl-elisp-like-keymap-p 'nil
  "*Uses Elisp like REPL keys"
  :group 'bgl
  :type 'boolean)

;*---------------------------------------------------------------------*/
;*    bgl faces                                                        */
;*---------------------------------------------------------------------*/
(defface bgl-font-lock-face-1
  '((((class color) (background light)) (:foreground "slateblue3" :bold t))
    (((class color) (background dark)) (:foreground "Plum1" :bold t))
    (t (:bold t)))
  "Bgl face 1."
  :group 'bgl)
(defvar bgl-font-lock-face-1 'bgl-font-lock-face-1)

(defface bgl-font-lock-face-2
  '((((class color) (background light)) (:foreground "blue" :bold t))
    (((class color) (background dark)) (:foreground "tomato2" :bold t))
    (t (:bold t)))
  "Bgl face 2."
  :group 'bgl)
(defvar bgl-font-lock-face-2 'bgl-font-lock-face-2)

(defface bgl-font-lock-face-3
  '((((class color) (background light)) (:foreground "tomato2" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue" :bold t))
    (t (:bold t)))
  "Bgl face 3."
  :group 'bgl)
(defvar bgl-font-lock-face-3 'bgl-font-lock-face-3)

(defface bgl-font-lock-face-4
  '((((class color) (background light)) (:foreground "green3" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t))
    (t (:bold t)))
  "Bgl face 4."
  :group 'bgl)
(defvar bgl-font-lock-face-4 'bgl-font-lock-face-4)

(defface bgl-font-lock-face-5
  '((((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Bgl face 5."
  :group 'bgl)
(defvar bgl-font-lock-face-5 'bgl-font-lock-face-5)

(defface bgl-font-lock-face-6
  '((((class color) (background light)) (:foreground "BlueViolet" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Bgl face 6."
  :group 'bgl)
(defvar bgl-font-lock-face-6 'bgl-font-lock-face-6)

(defface bgl-font-lock-face-7
  '((((class color) (background light)) (:foreground "green4" :bold t))
    (((class color) (background dark)) (:foreground "green2" :bold t))
    (t (:bold t)))
  "Bgl face 7."
  :group 'bgl)
(defvar bgl-font-lock-face-7 'bgl-font-lock-face-7)

(defface bgl-font-lock-face-8
  '((((class color) (background light)) (:foreground "#ff2020" :bold t))
    (((class color) (background dark)) (:foreground "red2" :bold t))
    (t (:bold t)))
  "Bgl face 8."
  :group 'bgl)
(defvar bgl-font-lock-face-8 'bgl-font-lock-face-8)

(defface bgl-font-lock-face-9
  '((((class color) (background light)) (:foreground "#e72c9f" :bold t))
    (((class color) (background dark)) (:foreground "blue2" :bold t))
    (t (:bold t)))
  "Bgl face 9."
  :group 'bgl)
(defvar bgl-font-lock-face-9 'bgl-font-lock-face-9)

(defface bgl-font-lock-face-10
  '((((class color) (background light)) (:foreground "#492ead" :bold t))
    (((class color) (background dark)) (:foreground "#492ead" :bold t))
    (t (:bold t)))
  "Bgl face 10."
  :group 'bgl)
(defvar bgl-font-lock-face-10 'bgl-font-lock-face-10)

(defface bgl-font-lock-face-11
  '((((class color) (background light)) (:foreground "#492ead" :bold nil))
    (((class color) (background dark)) (:foreground "#492ead" :bold nil))
    (t (:bold t)))
  "Bgl face 11."
  :group 'bgl)
(defvar bgl-font-lock-face-11 'bgl-font-lock-face-11)

(defface bgl-font-lock-face-12
  '((((class color) (background light)) (:foreground "#87910F" :bold t))
    (((class color) (background dark)) (:foreground "#701680" :bold t))
    (t (:bold t)))
  "Bgl face 12."
  :group 'bgl)
(defvar bgl-font-lock-face-12 'bgl-font-lock-face-12)

(defface bgl-font-lock-face-compile
  '((((class color) (background light)) (:background "#ffa435" :foreground "#ffffff" :bold t))
    (t (:bold t)))
  "Bgl face for compilation keywords."
  :group 'bgl)
(defvar bgl-font-lock-face-compile 'bgl-font-lock-face-compile)


;; cf bgl-invisible-face
(if (featurep 'xemacs)
    (progn
      (defface bgl-modeline-root-face
	`((((class color)) (:foreground "red" :bold t))
	  (t (:bold t)))
	"Ude modeline Root face."
	:group 'bgl)
      (set-face-background 'bgl-modeline-root-face
			   (face-background 'modeline)))
  (defface bgl-modeline-root-face
    (list (list '((class color))
		(list ':foreground "red"
		      ':bold t))
	  '(t (:bold t)))
    "Ude modeline Root face."
    :group 'bgl))

(defvar bgl-modeline-root-face 'bgl-modeline-root-face)

;; cf bgl-invisible-face
(if (featurep 'xemacs)
    (progn
      (defface bgl-modeline-no-root-face
	'((((class color)) (:foreground "blue" :bold t))
	  (t (:bold t)))
	"Ude modeline Root face."
	:group 'bgl)
      (set-face-background 'bgl-modeline-no-root-face
			   (face-background 'modeline)))
  (defface bgl-modeline-no-root-face
    (list (list '((class color))
		(list ':foreground "blue"
		      ':bold t))
	  '(t (:bold t)))
    "Ude modeline No Root face."
    :group 'bgl))

(defvar bgl-modeline-no-root-face 'bgl-modeline-no-root-face)

(defface bgl-error-face
  '((((class color)) (:foreground "red" :bold t))
    (t (:bold t)))
  "Bgl error face."
  :group 'bgl)
(defvar bgl-error-face 'bgl-error-face)

(defface bgl-ok-face
  '((((class color) (background light)) (:foreground "green3" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t))
    (t (:bold t)))
  "Bgl ok face."
  :group 'bgl)
(defvar bgl-ok-face 'bgl-ok-face)

(defface bgl-italic-face
  '((((class color)) (:bold t :italic t))
    (t (:bold t)))
  "Bgl face to display italic text."
  :group 'bgl)

;*---------------------------------------------------------------------*/
;*    bgl-return ...                                                   */
;*---------------------------------------------------------------------*/
(defun bgl-return (&optional dummy)
  "Indent on [return]"
  (interactive)
  (if (= (point) 1)
      (newline)
    (newline-and-indent)))

;*---------------------------------------------------------------------*/
;*    bgl keymap ...                                                   */
;*---------------------------------------------------------------------*/
(defvar bgl-map-prefix ?\c)
(defvar bgl-prefixed-map (make-sparse-keymap))

;*---------------------------------------------------------------------*/
;*    bgl-keymap-init ...                                              */
;*---------------------------------------------------------------------*/
(defun bgl-keymap-init ()

  ;; bgl bindings
  (define-key bgl-mode-map "\t" 'bgl-indent-line)
  (define-key bgl-mode-map "\e\C-q" 'bgl-indent-sexp)
  (define-key bgl-mode-map "\C-m" 'bgl-return)
  (define-key bgl-mode-map "\e\C-m" 'newline)
  (define-key bgl-mode-map "\e\C- " 'mark-sexp)
  (define-key bgl-mode-map "\e." 'bgl-goto-def)
  (define-key bgl-mode-map (kbd "ESC C-.") 'bgl-goto-def-other-frame)
  (define-key bgl-mode-map "\e," 'bgl-pop-def)
  (define-key bgl-mode-map "\e>" 'bgl-load-decl-index)

  ;; C-' keymap
  (define-key bgl-mode-map [(control \c)] 'bgl-prefix)
  (fset 'bgl-prefix bgl-prefixed-map)

  ;; repl
  (define-key bgl-prefixed-map "\C-r\C-r" 'bgl-repl-other-frame)
  (define-key bgl-prefixed-map "\C-rb" 'bgl-repl-send-buffer)
  (define-key bgl-prefixed-map "\C-rd" 'bgl-repl-send-define)
  (define-key bgl-prefixed-map "\C-rl" 'bgl-repl-send-last-sexp)
  (define-key bgl-prefixed-map "\C-rt" 'bgl-repl-send-toplevel-sexp)
  (define-key bgl-prefixed-map "\C-rr" 'bgl-repl-send-region)

  ;; indent
  (define-key bgl-prefixed-map "\C-i\C-i" 'bgl-external-indent)
  (define-key bgl-prefixed-map "\C-i\C-d" 'bgl-indent-define)
  (define-key bgl-prefixed-map "\C-i\C-l" 'bgl-indent-last-sexp)
  (define-key bgl-prefixed-map "\C-i\C-t" 'bgl-indent-toplevel-sexp)

  (when bgl-elisp-like-keymap-p
    (define-key bgl-mode-map "\C-c;" 'comment-region)
    (define-key bgl-mode-map "\C-x\C-e" 'bgl-repl-send-last-sexp)
    (define-key bgl-mode-map "\C-\M-x" 'bgl-repl-send-define)))
  
;*---------------------------------------------------------------------*/
;*    bgl-mode-syntax-table ...                                        */
;*---------------------------------------------------------------------*/
(defvar bgl-mode-syntax-table (make-syntax-table) "")

;*---------------------------------------------------------------------*/
;*    bgl-init-syntax-table ...                                        */
;*---------------------------------------------------------------------*/
(defun bgl-init-syntax-table ()
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
    (modify-syntax-entry ?. "w   " local-syntax-table)
    (modify-syntax-entry ?_ "w   " local-syntax-table)

    ;; Whitespace
    (modify-syntax-entry ?\t "    " local-syntax-table)
    (modify-syntax-entry ?\n ">   " local-syntax-table)
    (modify-syntax-entry ?\f "    " local-syntax-table)
    (modify-syntax-entry ?\r "    " local-syntax-table)
    (modify-syntax-entry ?  "    " local-syntax-table)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " local-syntax-table)
    (modify-syntax-entry ?\] ")[  " local-syntax-table)
    
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
    (setq bgl-extra-identifier-chars "[-_/]")))

;*---------------------------------------------------------------------*/
;*    bgl-sexp-comment-syntax-table                                    */
;*---------------------------------------------------------------------*/
(defconst bgl-sexp-comment-syntax-table
  (let ((st (make-syntax-table bgl-mode-syntax-table)))
    (modify-syntax-entry ?\; "." st)
    (modify-syntax-entry ?\n " " st)
    (modify-syntax-entry ?#  "'" st)
    st))

;*---------------------------------------------------------------------*/
;*    bgl-mode-abbrev-table ...                                        */
;*---------------------------------------------------------------------*/
(defvar bgl-mode-abbrev-table nil "")
(define-abbrev-table 'bgl-mode-abbrev-table ())

;*---------------------------------------------------------------------*/
;*    bgl keymap ...                                                   */
;*    -------------------------------------------------------------    */
;*    For a reason that I don't know these variables cannot be         */
;*    defined inside BGL-KEYMAP otherwise emacs don't succeed at       */
;*    loading the present file!                                        */
;*---------------------------------------------------------------------*/
(defvar bgl-mode-map (make-sparse-keymap))

;*---------------------------------------------------------------------*/
;*    bgl-mode-variables ...                                           */
;*---------------------------------------------------------------------*/
(defun bgl-mode-variables ()
  (setq local-abbrev-table bgl-mode-abbrev-table)
  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "^$\\|" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'bgl-indent-line)
  (make-local-variable 'comment-start)
  (setq comment-start ";")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip ";+[ \t]*")
  (make-local-variable 'comment-column)
  (setq comment-column 40)
  (make-local-variable 'comment-indent-function)
  (setq comment-indent-function 'bgl-comment-indent)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)
  (setq-local syntax-propertize-function #'bgl-syntax-propertize))

;*---------------------------------------------------------------------*/
;*    bgl-syntax-propertize ...                                        */
;*---------------------------------------------------------------------*/
(defun bgl-syntax-propertize (beg end)
  (goto-char beg)
  (bgl-syntax-propertize-sexp-comment (point) end)
  (funcall
   (syntax-propertize-rules
    ("\\(#\\);" (1 (prog1 "< cn"
                     (bgl-syntax-propertize-sexp-comment (point) end)))))
   (point) end))

;*---------------------------------------------------------------------*/
;*    bgl-syntax-propertize-sexp-comment ...                           */
;*---------------------------------------------------------------------*/
(defun bgl-syntax-propertize-sexp-comment (_ end)
  (let ((state (syntax-ppss)))
    (when (eq 2 (nth 7 state))
      ;; It's a sexp-comment.  Tell parse-partial-sexp where it ends.
      (condition-case nil
          (progn
            (goto-char (+ 2 (nth 8 state)))
            ;; FIXME: this doesn't handle the case where the sexp
            ;; itself contains a #; comment.
            (forward-sexp 1)
            (put-text-property (1- (point)) (point)
                               'syntax-table (string-to-syntax "> cn")))
        (scan-error (goto-char end))))))

;*---------------------------------------------------------------------*/
;*    bgl-comment-indent ...                                           */
;*---------------------------------------------------------------------*/
(defun bgl-comment-indent (&optional pos)
  (save-excursion
    (if pos (goto-char pos))
    (cond
     ((looking-at ";;;")
      (current-column))
     ((looking-at ";\\*")
      0)
     ((looking-at "[ \t]*;;")
      (let ((tem (bgl-calculate-indent)))
	(if (listp tem) (car tem) tem)))
     (t
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column)))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-offset ...                                            */
;*---------------------------------------------------------------------*/
(defvar bgl-indent-offset nil "")

;*---------------------------------------------------------------------*/
;*    bgl-indent-hook ...                                              */
;*---------------------------------------------------------------------*/
(defvar bgl-indent-hook 'bgl-indent-hook "")

;*---------------------------------------------------------------------*/
;*    bgl-indent-line ...                                              */
;*---------------------------------------------------------------------*/
(defun bgl-indent-line (&optional whole-exp)
  "Indent current line as Bigloo code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (bgl-calculate-indent)) shift-amt beg end
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (or (looking-at ";;;")
            (looking-at ";\\*")
	    (looking-at ";[*]"))
	;; Don't alter indentation of a ;;; or a ;* comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
           (save-excursion
             (goto-char beg)
             (forward-sexp 1)
             (setq end (point))
             (goto-char beg)
             (forward-line 1)
             (setq beg (point))
             (> end beg))
           (indent-code-rigidly beg end shift-amt)))))

;*---------------------------------------------------------------------*/
;*    bgl-calculate-indent ...                                         */
;*---------------------------------------------------------------------*/
(defun bgl-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Bigloo code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (or (bgl-calculate-forced-indent)
      (bgl-calculate-unforced-indent parse-start)))

;*---------------------------------------------------------------------*/
;*    bgl-calculate-forced-indent ...                                  */
;*    -------------------------------------------------------------    */
;*    Returns a column number iff the line indentation is forced       */
;*    (i.e. the previous line starts with a "[ \t]*;;;"). Otherwise    */
;*    returns f.                                                       */
;*---------------------------------------------------------------------*/
(defun bgl-calculate-forced-indent ()
  (when (> (count-lines 1 (point)) 1)
    (save-excursion
      (previous-line 1)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((s (current-column)))
	(and (looking-at bgl-forced-indent-regexp) s)))))

;*---------------------------------------------------------------------*/
;*    bgl-calculate-unforced-indent ...                                */
;*---------------------------------------------------------------------*/
(defun bgl-calculate-unforced-indent (&optional parse-start)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point)) state paren-depth desired-indent (retry t)
          last-sexp containing-sexp first-sexp-list-p)
      (if parse-start
          (goto-char parse-start)
        (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
        (setq retry nil)
        (setq last-sexp (nth 2 state))
        (setq containing-sexp (car (cdr state)))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and last-sexp (> last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek))))
        (if (not retry)
            ;; Innermost containing sexp found
            (progn
              (goto-char (1+ containing-sexp))
              (if (not last-sexp)
                  ;; indent-point immediately follows open paren.
                  ;; Don't call hook.
                  (setq desired-indent (current-column))
                ;; Move to first sexp after containing open paren
                (parse-partial-sexp (point) last-sexp 0 t)
                (setq first-sexp-list-p (looking-at "\\s("))
                (cond
                 ((> (save-excursion (forward-line 1) (point)) last-sexp)
                  ;; Last sexp is on same line as containing sexp.
                  ;; It's almost certainly a function call.
                  (parse-partial-sexp (point) last-sexp 0 t)
		  (if (/= (point) last-sexp)
		      (if (eq bgl-indent-style 'TODO)
			  (setq desired-indent (- (+ (current-column) bgl-body-indent) 1))
			;; Indent beneath first argument or, if only one sexp
			;; on line, indent beneath that.
			(progn (forward-sexp 1)
			       (parse-partial-sexp (point) last-sexp 0 t))))
		  (backward-prefix-chars))
                 (t
                  ;; Indent beneath first sexp on same line as last-sexp.
                  ;; Again, it's almost certainly a function call.
                  (goto-char last-sexp)
                  (beginning-of-line)
                  (parse-partial-sexp (point) last-sexp 0 t)
                  (backward-prefix-chars)))))))
      ;; If looking at a list, don't call hook.
      (if first-sexp-list-p
          (setq desired-indent (current-column)))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by bgl-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond ((car (nthcdr 3 state))
             ;; Inside a string, don't change indentation.
             (goto-char indent-point)
             (skip-chars-forward " \t")
             (setq desired-indent (current-column)))
	    ((bgl-indent-quote-p state)
	     (save-excursion
	       (goto-char (+ (car (nthcdr 1 state)) 1))
	       (setq desired-indent (current-column))))
	    ((bgl-indent-brace-p state)
	     (setq desired-indent 0))
            ((and (integerp bgl-indent-offset) containing-sexp)
             ;; Indent by constant offset
             (goto-char containing-sexp)
             (setq desired-indent (- bgl-indent-offset (current-column))))
	    ((and bgl-indent-on-keyword-p
		  (looking-at ":[^:]+")
		  (let ((pos (point)))
		    (save-excursion
		      (beginning-of-line)
		      (skip-chars-forward " \t")
		      (not (= (point) pos)))))
	     ;; We are at a keyword position, we backward sexp until we are
	     ;; no longer located on a keyword
	     (while (looking-at ":[^:]+")
	       (backward-sexp 1))
	     (setq desired-indent (- (+ (current-column) bgl-body-indent) 1)))
	    ((in-condp state)
	     (save-excursion
	       (goto-char (cadr state))
	       (setq desired-indent (1+ (current-column)))))
            ((not (or desired-indent
                      (and (boundp 'bgl-indent-hook)
                           bgl-indent-hook
                           (not retry)
                           (setq desired-indent
                                 (funcall bgl-indent-hook
                                          indent-point state)))))
             ;; Use default indentation if not computed yet
             (setq desired-indent (current-column))))
      desired-indent)))

;*---------------------------------------------------------------------*/
;*    in-condp ...                                                     */
;*---------------------------------------------------------------------*/
(defun in-condp (state)
  (and (consp state)
       (>= (car state) 2)
       (let* ((conts (nth 9 state))
	      (pos (nth (- (length conts) 2) conts)))
	 (save-excursion
	   (goto-char (1+ pos))
	   (looking-at cond-regexp)))))

;*---------------------------------------------------------------------*/
;*    in-modulep ...                                                   */
;*---------------------------------------------------------------------*/
(defun in-modulep (state)
  (and (>= (car state) 2)
       (let* ((conts (nth 9 state))
	      (pos (nth (- (length conts) 2) conts)))
	 (save-excursion
	   (goto-char (1+ pos))
	   (looking-at "\\(module\\|directives\\)[\t\n ]")))))

;*---------------------------------------------------------------------*/
;*    normal-indent ...                                                */
;*---------------------------------------------------------------------*/
(defvar normal-indent 0)

;*---------------------------------------------------------------------*/
;*    bgl-indent-hook ...                                              */
;*---------------------------------------------------------------------*/
(defun bgl-indent-hook (indent-point state)
  (let ((normal-indent (current-column)))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (point) (car (cdr state)))
          (let ((function (buffer-substring (progn (forward-char -1) (point))
                                            (progn (forward-sexp 1) (point))))
                method)
            ;; Who cares about this, really?
	    ;; (if (not (string-match "\\\\\\||" function)))
            (setq function (downcase function))
            (setq method (get (intern-soft function) 'bgl-indent-hook))
            (cond
	     ((integerp method)
	      (if (< method 0)
		  '()
		(bgl-indent-specform method state indent-point)))
	     (method
	      (funcall method state indent-point))
	     ((and (> (length function) 3)
		   (string-equal (substring function 0 3) "def"))
	      (bgl-indent-defform state indent-point))
	     ((and (> (length function) 13)
		   (string-equal (substring function 0 13) "with-access::"))
	      (bgl-with-access-indent state indent-point))
	     ((and (> (length function) 13)
		   (string-equal (substring function 0 13) "instantiate::"))
	      (bgl-instantiate-indent state indent-point))
	     ((and (> (length function) 11)
		   (string-equal (substring function 0 11) "duplicate::"))
	      (bgl-duplicate-indent state indent-point))
	     ((and (> (length function) 11)
		   (string-equal (substring function 0 8) "widen!::"))
	      (bgl-duplicate-indent state indent-point))
	     (t
	      (bgl-indent-defform state indent-point))))))))

;*---------------------------------------------------------------------*/
;*    bgl-module-indent-hook ...                                       */
;*---------------------------------------------------------------------*/
(defun bgl-module-indent-hook (state point)
  (if (in-modulep state)
      (save-excursion
	(if (= (1+ (cadr state)) (cadr (cdr state)))
	    (progn
	      (goto-char (cadr state))
	      (+ (current-column) bgl-body-indent))
	  (progn
	    (goto-char (cadr (cdr state)))
	    (current-column))))
    (save-excursion
      (goto-char (cadr state))
      (+ (current-column) bgl-body-indent))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-brace-p ...                                           */
;*---------------------------------------------------------------------*/
(defun bgl-indent-brace-p (state)
  (or (and (integerp (car (nthcdr 1 state)))
	   (let ((c (char-after (car (nthcdr 1 state)))))
	     (or (eq c ?{) (and (eq c ?\[) (eq bgl-indent-mode 'hop)))))
      (let ((op (car (nthcdr 9 state))))
	(and (consp op)
	     (let ((po (reverse op))
		   (context 'unknown))
	       (save-excursion
		 (while (and (consp po) (eq context 'unknown))
		   (cond
		    ((eq (char-after (car po)) ?{)
		     (setq context 'brace))
		    ((eq (char-after (car po)) ?\()
		     (setq context 'scheme))
		    (t
		     (setq po (cdr po))))))
	       (eq context 'brace))))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-quote-p ...                                           */
;*---------------------------------------------------------------------*/
(defun bgl-indent-quote-p (state)
  (and bgl-indent-on-quote-p
       (integerp (car (nthcdr 1 state)))
       (> (car (nthcdr 1 state)) (point-min))
       (eq (char-after (car (nthcdr 1 state))) ?\()
       (or (eq (char-after (- (car (nthcdr 1 state)) 1)) ?')
	   (and (> (car (nthcdr 1 state)) (1+ (point-min)))
		(eq (char-after (- (car (nthcdr 1 state)) 1)) ?#)
		(eq (char-after (- (car (nthcdr 1 state)) 2)) ?')))))

;*---------------------------------------------------------------------*/
;*    bgl-body-indent ...                                              */
;*---------------------------------------------------------------------*/
(defvar bgl-body-indent 3 "")

;*---------------------------------------------------------------------*/
;*    bgl-indent-specform ...                                          */
;*---------------------------------------------------------------------*/
(defun bgl-indent-specform (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  bgl-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ bgl-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case nil
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
           ;; A distinguished form.  Use double bgl-body-indent.
           (list (+ containing-form-column (* 2 bgl-body-indent))
                 containing-form-start))
          ;; A non-distinguished form. Use body-indent if there are no
          ;; distinguished forms and this is the first undistinguished
          ;; form, or if this is the first undistinguished form and
          ;; the preceding distinguished form has indentation at least
          ;; as great as body-indent.
          ((and (= count 0)
                (or (= i 0)
                    (<= body-indent normal-indent)))
           body-indent)
          (t
           normal-indent))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-defform ...                                           */
;*---------------------------------------------------------------------*/
(defun bgl-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
        (goto-char (car (cdr state)))
        (+ bgl-body-indent (current-column)))))

;*---------------------------------------------------------------------*/
;*    bgl-with-access-indent ...                                       */
;*---------------------------------------------------------------------*/
(defun bgl-with-access-indent (state indent-point)
  (skip-chars-forward " \t")
  (bgl-indent-with-access-form 2 state indent-point))

;*---------------------------------------------------------------------*/
;*    bgl-duplicate-indent ...                                         */
;*---------------------------------------------------------------------*/
(defun bgl-duplicate-indent (state indent-point)
  (skip-chars-forward " \t")
  (bgl-indent-with-access-form 1 state indent-point))

;*---------------------------------------------------------------------*/
;*    bgl-instantiate-indent ...                                       */
;*---------------------------------------------------------------------*/
(defun bgl-instantiate-indent (state indent-point)
  (skip-chars-forward " \t")
  (bgl-indent-with-access-form 0 state indent-point))

;*---------------------------------------------------------------------*/
;*    bgl-indent-with-access-form ...                                  */
;*---------------------------------------------------------------------*/
(defun bgl-indent-with-access-form (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  bgl-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ bgl-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case nil
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
           ;; A distinguished form.  Use double bgl-body-indent.
           (list (+ containing-form-column (* 2 bgl-body-indent))
                 containing-form-start))
          ;; A non-distinguished form. Use body-indent if there are no
          ;; distinguished forms and this is the first undistinguished
          ;; form, or if this is the first undistinguished form and
          ;; the preceding distinguished form has indentation at least
          ;; as great as body-indent.
          ((and (= count 0)
                (or (= i 0)
                    (<= body-indent normal-indent)))
           body-indent)
          (t
           normal-indent))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-instantiate-form ...                                  */
;*---------------------------------------------------------------------*/
(defun bgl-indent-instantiate-form (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
	body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  bgl-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ bgl-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 2)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case nil
		    (progn
		      (setq count (1- count))
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
	   ;; A distinguished form.  Use double bgl-body-indent.
	   (list (+ containing-form-column (* 2 bgl-body-indent))
		 containing-form-start))
	  ;; A non-distinguished form. Use body-indent if there are no
	  ;; distinguished forms and this is the first undistinguished
	  ;; form, or if this is the first undistinguished form and
	  ;; the preceding distinguished form has indentation at least
	  ;; as great as body-indent.
	  ((and (= count 0)
		(or (= i 0)
		    (<= body-indent normal-indent)))
	   body-indent)
	  (t
	   normal-indent))))

;*---------------------------------------------------------------------*/
;*    bgl-let-indent ...                                               */
;*---------------------------------------------------------------------*/
(defun bgl-let-indent (state indent-point)
  (skip-chars-forward " \t")
  (if (looking-at "[a-zA-Z0-9+-*/?!@$%^&_:~]")
      (bgl-indent-specform 2 state indent-point)
    (bgl-indent-specform 1 state indent-point)))

;*---------------------------------------------------------------------*/
;*    bgl-indent-sexp ...                                              */
;*---------------------------------------------------------------------*/
(defun bgl-indent-sexp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) last-depth bol
        outer-loop-done inner-loop-done state this-indent)
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
        (setq last-depth next-depth
              inner-loop-done nil)
        (while (and (not inner-loop-done)
                    (not (setq outer-loop-done (eobp))))
          (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
                                          nil nil state))
          (setq next-depth (car state))
          (if (car (nthcdr 4 state))
              (progn (bgl-comment-indent)
                     (end-of-line)
                     (setcar (nthcdr 4 state) nil)))
          (if (car (nthcdr 3 state))
              (progn
                (forward-line 1)
                (setcar (nthcdr 5 state) nil))
            (setq inner-loop-done t)))
        (if (setq outer-loop-done (<= next-depth 0))
            nil
          (while (> last-depth next-depth)
            (setq indent-stack (cdr indent-stack)
                  last-depth (1- last-depth)))
          (while (< last-depth next-depth)
            (setq indent-stack (cons nil indent-stack)
                  last-depth (1+ last-depth)))
          (forward-line 1)
          (setq bol (point))
          (skip-chars-forward " \t")
          (if (or (eobp) (looking-at ";\\(;;\\|[*]\\)"))
              nil
            (if (and (car indent-stack)
                     (>= (car indent-stack) 0))
                (setq this-indent (car indent-stack))
              (let ((val (bgl-calculate-indent
                          (if (car indent-stack) (- (car indent-stack))))))
                (if (integerp val)
                    (setcar indent-stack
                            (setq this-indent val))
                  (setcar indent-stack (- (car (cdr val))))
                  (setq this-indent (car val)))))
            (if (/= (current-column) this-indent)
                (progn (delete-region bol (point))
                       (indent-to this-indent)))))))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-last-sexp ...                                         */
;*---------------------------------------------------------------------*/
(defun bgl-indent-last-sexp ()
  (interactive)
  (forward-sexp -1)
  (bgl-indent-sexp)
  (forward-sexp 1))

;*---------------------------------------------------------------------*/
;*    bgl-indent-define ...                                            */
;*---------------------------------------------------------------------*/
(defun bgl-indent-define ()
  (interactive)
  (condition-case ()
      (save-excursion
	(end-of-defun)
	(bgl-indent-sexp))))

;*---------------------------------------------------------------------*/
;*    bgl-indent-toplevel-sexp ...                                     */
;*---------------------------------------------------------------------*/
(defun bgl-indent-toplevel-sexp (pos)
  (interactive "dPos: ")
  (let ((sexp (bgl-find-toplevel-sexp pos)))
    (if (consp sexp)
	(save-excursion
	  (goto-char (car sexp))
	  (bgl-indent-sexp)) 
      (error "Corrupted toplevel sexp"))))

;*---------------------------------------------------------------------*/
;*    Bee indent forms                                                 */
;*---------------------------------------------------------------------*/
(defvar cond-regexp
  "\\(cond\\|case\\|match-case\\|args-parse\\|cond-expand\\|string-case\\|match-lambda\\|syntax-rule\\|regular-grammar\\)[\t\n ]")

;; basic forms
(put 'begin                     'bgl-indent-hook 0)
(put 'case                      'bgl-indent-hook 1)
(put 'delay                     'bgl-indent-hook 0)
(put 'do                        'bgl-indent-hook 0)
(put 'lambda                    'bgl-indent-hook 1)
(put 'cond                      'bgl-indent-hook 0)
(put 'when                      'bgl-indent-hook 1)
(put 'unless                    'bgl-indent-hook 1)
(put 'if                        'bgl-indent-hook -1)
(put 'or                        'bgl-indent-hook -1)
(put 'and                       'bgl-indent-hook -1)
(put 'else                      'bgl-indent-hook -1)

;; binding forms
(put 'let                       'bgl-indent-hook 'bgl-let-indent)
(put 'let*                      'bgl-indent-hook 1)
(put 'letrec                    'bgl-indent-hook 1)
(put 'letrec*                   'bgl-indent-hook 1)
(put 'labels                    'bgl-indent-hook 'bgl-let-indent)
(put 'let-syntax                'bgl-indent-hook 'bgl-let-indent)
(put 'letrec-syntax             'bgl-indent-hook 'bgl-let-indent)
(put 'co-instantiate            'bgl-indent-hook 'bgl-let-indent)

;; output/input command
(put 'call-with-input-file      'bgl-indent-hook 1)
(put 'call-with-input-string    'bgl-indent-hook 1)
(put 'with-input-from-file      'bgl-indent-hook 1)
(put 'with-input-from-port      'bgl-indent-hook 1)
(put 'with-input-from-string    'bgl-indent-hook 1)
(put 'with-input-from-procedure 'bgl-indent-hook 1)
(put 'call-with-output-file     'bgl-indent-hook 1)
(put 'call-with-output-string   'bgl-indent-hook 0)
(put 'with-output-to-file       'bgl-indent-hook 1)
(put 'with-output-to-port       'bgl-indent-hook 1)
(put 'with-output-to-string     'bgl-indent-hook 0)
(put 'with-error-to-port        'bgl-indent-hook 1)
(put 'with-error-to-file        'bgl-indent-hook 1)
(put 'with-error-to-string      'bgl-indent-hook 0)
(put 'with-scheduler            'bgl-indent-hook 1)
(put 'with-exception-handler    'bgl-indent-hook 0)
(put 'with-handler              'bgl-indent-hook 0)
(put 'with-alarm                'bgl-indent-hook 1)
(put 'with-lock                 'bgl-indent-hook 1)

;; define forms
(put 'define-macro              'bgl-indent-hook 1)
(put 'macro                     'bgl-indent-hook 1)
(put 'define-generic            'bgl-indent-hook 1)
(put 'define-method             'bgl-indent-hook 1)

;; exceptions
(put 'bind-exit                 'bgl-indent-hook 1)
(put 'unwind-protect            'bgl-indent-hook 0)
(put 'dynamic-wind              'bgl-indent-hook 0)

;; multiple values
(put 'multiple-value-bind       'bgl-indent-hook 1)
(put 'receive                   'bgl-indent-hook 1)
(put 'call-with-values          'bgl-indent-hook 0)

;; parsing
(put 'regular-grammar           'bgl-indent-hook 'bgl-let-indent)
(put 'lalr-grammar              'bgl-indent-hook 0)

;; module indentation
(put 'module                    'bgl-indent-hook 1)
(put 'interface                 'bgl-indent-hook 1)
(put 'directives                'bgl-indent-hook 0)
(put 'class                     'bgl-indent-hook 1)
(put 'abstract-class            'bgl-indent-hook 1)
(put 'wide-class                'bgl-indent-hook 1)
(put 'final-class               'bgl-indent-hook 1)

;; matching indentation
(put 'args-parse                'bgl-indent-hook 1)
(put 'match-case                'bgl-indent-hook 1)
(put 'cond-expand               'bgl-indent-hook 0)
(put 'string-case               'bgl-indent-hook 1)
(put 'match-lambda              'bgl-indent-hook 0)
(put 'syntax-rules              'bgl-indent-hook 1)
(put 'event-case                'bgl-indent-hook 1)
(put 'on-event                  'bgl-indent-hook 1)
(put 'with-trace                'bgl-indent-hook 2)

;; profiling
(put 'profile                   'bgl-indent-hook 1)
(put 'profile/gc                'bgl-indent-hook 1)

;; hop
(put 'service                   'bgl-indent-hook 1)
(put 'add-event-listener!       'bgl-indent-hook 2)
(put 'remove-event-listener!    'bgl-indent-hook 2)
(put 'timeout                   'bgl-indent-hook 1)
(put 'after                     'bgl-indent-hook 1)
(put 'with-hop                  'bgl-indent-hook 1)
(put 'with-url                  'bgl-indent-hook 1)
(put 'node-style-set!           'bgl-indent-hook 1)

;; sql
(put 'sqlite-exec               'bgl-indent-hook 1)
(put 'sqlite-map                'bgl-indent-hook 2)
(put 'sqlite-eval               'bgl-indent-hook 2)

;*---------------------------------------------------------------------*/
;*    bgl-mode ...                                                     */
;*---------------------------------------------------------------------*/
(defun bgl-mode ()
  "Major mode for editing Bigloo code.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{bgl-mode-map}
Entry to this mode calls the value of bgl-mode-hook
if that value is non-nil."
  (interactive)
  
  ;; mode declaration
  (kill-all-local-variables)
  (setq major-mode 'bgl-mode)
  (setq mode-name "Bgl")
  (use-local-map bgl-mode-map)
  
  ;; syntax table
  (set-syntax-table bgl-mode-syntax-table)
  (bgl-init-syntax-table)
  
  ;; global buffer local variables
  (bgl-mode-variables)
  
  ;; keymap bindings
  (bgl-keymap-init)
  
  ;; starting font-lock
  (bgl-set-font-lock)
  
  ;; repl initialization
;*   (add-hook 'bgl-repl-hooks                                         */
;* 	    #'(lambda () (set-syntax-table bgl-mode-syntax-table))     */
;* 	    '()                                                        */
;* 	    t)                                                         */

  ;; flycheck configuration
  (bgl-flycheck-init)

;*   ;; lsp configuration                                              */
;*   (when (package-installed-p 'lsp-mode)                             */
;*     (bgl-lsp-init))                                                 */

  ;; load the documentation index
  (when bgl-flydoc-p
    (bgl-load-doc-index)
    (bgl-load-decl-index)
    (run-with-idle-timer 0.2 t #'bgl-proto-at-point))
  
  ;; activate th emode
  (font-lock-mode t)

  ;; the bgl hook
  (run-hooks 'bgl-mode-hook))

;*---------------------------------------------------------------------*/
;*    bgl-set-font-lock ...                                            */
;*---------------------------------------------------------------------*/
(defun bgl-set-font-lock ()
  (setq font-lock-keywords-case-fold-search t)
  (make-local-variable 'font-lock-defaults)
  (if (< bmacs-emacs-version 22)
      (setq font-lock-defaults '(bgl-font-lock-keywords))
    (setq font-lock-defaults
	  '((bgl-font-lock-keywords)
	    nil t (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
	    beginning-of-defun
	    (font-lock-mark-block-function . mark-defun)
	    (font-lock-syntactic-face-function . bgl-font-lock-syntactic-face-function)
	    (parse-sexp-lookup-properties . t)
	    (font-lock-extra-managed-props syntax-table)))))

;*---------------------------------------------------------------------*/
;*    bgl-font-lock-syntactic-face-function ...                        */
;*    -------------------------------------------------------------    */
;*    This is a verbatim copy of what I have found in regular          */
;*    scheme.el mode. Don't ask me the meaning of all this. It is      */
;*    to make the #; operational.                                      */
;*---------------------------------------------------------------------*/
(defun bgl-font-lock-syntactic-face-function (state)
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
                             'syntax-table bgl-sexp-comment-syntax-table))
        (put-text-property (- end 1) end 'syntax-table '(12)))))
  ;; Choose the face to use.
  (lisp-font-lock-syntactic-face-function state))

;*---------------------------------------------------------------------*/
;*    bgl-flycheck-initializedp                                        */
;*---------------------------------------------------------------------*/
(defvar bgl-flycheck-initializedp nil)

;*---------------------------------------------------------------------*/
;*    bgl-flycheck-init ...                                            */
;*---------------------------------------------------------------------*/
(defun bgl-flycheck-init ()
  (unless bgl-flycheck-initializedp
    (setq bgl-flycheck-initializedp t)
    (when (package-installed-p 'flycheck)
      (require 'flycheck)
      (with-eval-after-load 'flycheck
	(flycheck-define-checker bgl
	  "A bgl syntax checker using the bgl compiler."
	  :command ("/home/serrano/prgm/project/bigloo/5.0.x/bin/bigloo"
		    (eval bgl-flycheck-args)
		    source-inplace)
	  :error-patterns
	  ((error line-start
		  "File \"" (file-name) "\", line " line ", character " column ":\n"
		  (zero-or-more anything)
		  "*** ERROR:" (one-or-more not-newline) "\n"
		  (message) line-end))
	  :modes bgl-mode)
	(add-to-list 'flycheck-checkers 'bgl))

      (add-hook 'flycheck-status-changed-functions
		#'(lambda (status)
		   (when (eq status 'finished)
		     (unless flycheck-current-errors
		       (bgl-load-decl-index)))))))

  (when (and (member (file-name-extension (buffer-file-name))
		     bgl-flycheck-file-extensions)
	     (< (buffer-size) bgl-flycheck-size-limit))
    (let ((be (bgl-guess-buffer-backend)))
      (cond
	((not be)
	 t)
	((string= be "C")
	 (setq bgl-flycheck-args '("-coerce" "-dump" "module")))
	((string= be "java")
	 (setq bgl-flycheck-args '("-coerce" "-dump" "module" "-jvm")))
	((string= be "wasm")
	 (setq bgl-flycheck-args '("-coerce" "-dump" "module" "-wasm")))))
    (flycheck-mode 1)))

;*---------------------------------------------------------------------*/
;*    bgl-guess-buffer-backend ...                                     */
;*---------------------------------------------------------------------*/
(defun bgl-guess-buffer-backend ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "extern[ \n\t]+\"\\([a-zA-Z]*\\)\"" nil t)
      (match-string 1))))

;*---------------------------------------------------------------------*/
;*    bgl-lsp-initializedp                                             */
;*---------------------------------------------------------------------*/
(defvar bgl-lsp-initializedp nil)

;*---------------------------------------------------------------------*/
;*    bgl-lsp-init ...                                                 */
;*---------------------------------------------------------------------*/
(defun bgl-lsp-init ()
  (when (> (length bgl-lsp-server) 0)
    (if bgl-lsp-initializedp
	(lsp)
	(progn
	  (setq bgl-lsp-initializedp t)
	  (require 'lsp-mode)
	  (with-eval-after-load 'lsp-mode
	    (lsp-register-client
	     (make-lsp-client
	      :new-connection (lsp-stdio-connection (list bgl-lsp-server))
	      :activation-fn (lsp-activate-on "bgl")
	      
	      :server-id 'bgl-lsp))
	    (add-to-list 'lsp-language-id-configuration '(bgl-mode . "bgl"))
	    (global-set-key (kbd "C-c l s") 'lsp-workspace-restart)
	    (lsp))))))

;*---------------------------------------------------------------------*/
;*    doc-table-index ...                                              */
;*---------------------------------------------------------------------*/
(defvar bgl-doc-table-index (make-hash-table :test 'equal))
(defvar bgl-decl-table-index (make-hash-table :test 'equal))
(defvar bgl-decl-table-loaded nil)
(make-variable-buffer-local 'bgl-decl-table-index)
(make-variable-buffer-local 'bgl-decl-table-loaded)

;*---------------------------------------------------------------------*/
;*    bgl-last-symbol ...                                              */
;*---------------------------------------------------------------------*/
(defvar bgl-last-decl-symbol nil)
(defvar bgl-last-decl-entry nil)
(defvar bgl-last-doc-symbol nil)
(defvar bgl-last-doc-entry nil)
(defvar bgl-popup-buffer "*bgl-mode-popup*")

(make-variable-buffer-local 'bgl-last-decl-symbol)
(make-variable-buffer-local 'bgl-last-decl-entry)
(make-variable-buffer-local 'bgl-last-doc-symbol)
(make-variable-buffer-local 'bgl-last-doc-entry)

;*---------------------------------------------------------------------*/
;*    bgl-load-doc-index ...                                           */
;*    -------------------------------------------------------------    */
;*    Load the documentation index                                     */
;*---------------------------------------------------------------------*/
(defun bgl-load-doc-index ()
  (let ((index (concat bgl-doc-dir "/" bgl-doc-index)))
    (when (file-exists-p index)
      (message "loading \"%s\" doc index..." index)
      (let ((l (save-excursion
		 (with-temp-buffer
		   (progn
		     (set-buffer-multibyte nil)
		     (insert-file-contents index)
		     (goto-char (point-min))
		     (while (search-forward "#" nil t)
		       (replace-match "\\x23" t t))
		     (goto-char (point-min))
		     (read (current-buffer)))))))
	(mapc #'(lambda (el)
		  (let* ((name (car el))
			 (i (string-match-p "::" name))
			 (k (if i (substring name 0 i) name)))
		    (puthash k (cdr el) bgl-doc-table-index)))
	      l)
	(setq bgl-last-doc-symbol nil)
	(setq bgl-decl-table-loaded t)))))

;*---------------------------------------------------------------------*/
;*    bgl-load-decl-index ...                                          */
;*---------------------------------------------------------------------*/
(defun bgl-load-decl-index ()
  (interactive)
  (unless bgl-decl-table-loaded
    (bgl-load-module-index (expand-file-name buffer-file-name))))

;*---------------------------------------------------------------------*/
;*    bgl-load-module-index ...                                        */
;*    -------------------------------------------------------------    */
;*    Load the pre-compiled module description generated by the        */
;*    compler in order to resolve global variable definition.          */
;*---------------------------------------------------------------------*/
(defun bgl-load-module-index (path)
  (let ((file (concat (file-name-directory path) "/flycheck_"
		      (file-name-sans-extension
		       (file-name-nondirectory path))
		      ".sexp")))
    (when (file-exists-p file)
      (message "loading \"%s\" module..." path)
      (let* ((mod (save-excursion
		    (with-temp-buffer
		      (progn
			(set-buffer-multibyte nil)
			(insert-file-contents file)
			(goto-char (point-min))
			(while (search-forward "#" nil t)
			  (replace-match "\\x23" nil t))
			(goto-char (point-min))
			(while (search-forward "?" nil t)
			  (replace-match "\\x3f" nil t))
			(goto-char (point-min))
			(read (current-buffer))))))
	     (imports (assq 'imports (cddr mod)))
	     (defs (assq 'defs (cddr mod))))
	(when (consp imports)
	  (mapc #'(lambda (e)
		    (message "put %s" e)
		    (puthash (car e) (cdr e) bgl-decl-table-index))
		(cdr imports)))
	(when (consp defs)
	  (mapc #'(lambda (e)
		    (puthash (car e) (cdr e) bgl-decl-table-index))
		(cdr defs)))
	(setq bgl-last-decl-symbol nil)))))

;*---------------------------------------------------------------------*/
;*    bgl-doc-entry-at-point ...                                       */
;*---------------------------------------------------------------------*/
(defun bgl-doc-entry-at-point (sym)
  (if (equal sym bgl-last-doc-symbol)
      bgl-last-doc-entry
      (progn
	(setq bgl-last-doc-symbol sym)
	(if sym
	    (setq bgl-last-doc-entry (gethash sym bgl-doc-table-index))
	    (setq bgl-last-doc-entry nil))))
  bgl-last-doc-entry)

;*---------------------------------------------------------------------*/
;*    bgl-decl-entry-at-point ...                                      */
;*---------------------------------------------------------------------*/
(defun bgl-decl-entry-at-point (sym)
  (if (equal sym bgl-last-decl-symbol)
      bgl-last-decl-entry
      (progn
	(setq bgl-last-decl-symbol sym)
	(if sym
	    (setq bgl-last-decl-entry (gethash sym bgl-decl-table-index))
	    (setq bgl-last-decl-entry nil))))
  bgl-last-decl-entry)

;*---------------------------------------------------------------------*/
;*    posframe-bottom-right-above-modeline ...                         */
;*---------------------------------------------------------------------*/
(defun posframe-bottom-right-above-modeline (info)
  (let* ((win (plist-get info :parent-window))
         (win-edges (window-inside-pixel-edges win))
         (x (- (nth 2 win-edges)
               (plist-get info :posframe-width)
	       4))
         (y (- (nth 3 win-edges)
               (plist-get info :posframe-height)
               4)))
    (cons x y)))

;*---------------------------------------------------------------------*/
;*    bgl-indent ...                                                   */
;*---------------------------------------------------------------------*/
(defun bgl-indent (str max-len)
  (let ((len (length str)))
    (if (> len max-len)
	(let ((i (string-match " " str 0)))
	  (if i
	      (let ((res (substring str 0 i)))
		(while (> (- len i) (- max-len 3))
		  (let ((j (string-match " " str (1+ i))))
		    (if j
			(let ((s (substring str i j)))
			  (setq res (concat res "\n   " s))
			  (setq i j))
			(let ((s (substring str i len)))
			  (setq res (concat res "\n   " s))
			  (setq i len)))))
		(concat res "\n   " (substring str i len)))
	      str))
	str)))

;*---------------------------------------------------------------------*/
;*    bgl-proto-at-point ...                                           */
;*---------------------------------------------------------------------*/
(defun bgl-proto-at-point ()
  "Show the prototype of the symbol at point."
  (interactive)
  (when (eq major-mode 'bgl-mode)
    (let* ((sym (thing-at-point 'symbol t))
	   (d (bgl-decl-entry-at-point sym))
	   (e (or d (bgl-doc-entry-at-point sym))))
      (if e
	  (let* ((s (replace-regexp-in-string "\\x23" "#" (format "%s" (car e))))
		 (s2 (replace-regexp-in-string "\\x3f" "?" (format "%s" s)))
		 (cs (replace-regexp-in-string
		      "::[^ )]+"
		      '(lambda (match) (propertize match 'face 'bgl-font-lock-face-4))
		      s2))
		 (ls (if (and (> (length s) 57) (string-match " " s))
			 (bgl-indent cs 57)
			 cs)))
	    (posframe-show
	     bgl-popup-buffer
	     :string ls
	     :background-color (if d "#ccffcc" "#ffffcc")
	     :foreground-color "black"
	     :border-width 1
	     :border-color "#cccccc"
	     :internal-border-width 4
	     :poshandler #'posframe-bottom-right-above-modeline)
	    (if d
		(unless (bgl-message-once
			 'jump-def
			 "[M-.] to jump to definition")
		  (message (bgl-jump-stack-message)))
		(bgl-message-once
		 'open-doc
		 "[M-.] to browse the definition")))
	  (posframe-hide bgl-popup-buffer)))))

;*---------------------------------------------------------------------*/
;*    bgl-once-messages ...                                            */
;*---------------------------------------------------------------------*/
(defvar bgl-once-messages '())

;*---------------------------------------------------------------------*/
;*    bgl-message-once ...                                             */
;*---------------------------------------------------------------------*/
(defun bgl-message-once (key msg)
  (unless (memq key bgl-once-messages)
    (setq bgl-once-messages (cons key bgl-once-messages))
    (message msg)
    t))
      
;*---------------------------------------------------------------------*/
;*    bgl-browse-doc-at-point ...                                      */
;*---------------------------------------------------------------------*/
(defun bgl-browse-doc-at-point (sym)
  (let* ((e (bgl-doc-entry-at-point sym))
	 (file (if e
		   (format "%s/%s#%s" bgl-doc-dir (caddr e) (cadr e))
		   (format "%s/index.html" bgl-doc-dir))))
    (if (stringp bgl-flydoc-browser)
	(let ((process-connection-type nil)
	      (url (concat "file://" file)))
	  (start-process "doc" nil bgl-flydoc-browser url))
	(progn
	  (select-frame (make-frame))
	  (eww-open-file file)))))

;*---------------------------------------------------------------------*/
;*    bgl-jump-stack ...                                               */
;*---------------------------------------------------------------------*/
(defvar bgl-jump-stack '())

;*---------------------------------------------------------------------*/
;*    bgl-jump-stack-message ...                                       */
;*---------------------------------------------------------------------*/
(defun bgl-jump-stack-message ()
  (let ((msg "")
	(stk bgl-jump-stack))
    (while (and (consp stk) (< (length msg) 50))
      (let ((e (car stk)))
	(setq stk (cdr stk))
	(setq msg (format "%s:%s>%s" (cadr e) (caddr e) msg))))
    msg))

;*---------------------------------------------------------------------*/
;*    bgl-visit-decl-at-point ...                                      */
;*---------------------------------------------------------------------*/
(defun bgl-visit-decl-at-point (sym other-frame)
  (let ((e (bgl-decl-entry-at-point sym)))
    (when e
      (let* ((loc (cadr e))
	     (file (cadr loc))
	     (pos (caddr loc)))
	(let ((buf (if other-frame
		       (progn
			 (setq bgl-jump-stack
			       (cons (list (file-name-directory (buffer-name))
					   (current-buffer)
					   (point))
				     bgl-jump-stack))
			 (find-file-other-frame file))
		       (find-file file))))
	  (switch-to-buffer buf)
	  (goto-char (+ 1 pos))
	  (unless (bgl-message-once 'pop-def "[M-,] pop definition")
	    (message (bgl-jump-stack-message))))))))

;*---------------------------------------------------------------------*/
;*    bgl-pop-def ...                                                  */
;*---------------------------------------------------------------------*/
(defun bgl-pop-def ()
  (interactive)
  (when (consp bgl-jump-stack)
    (let* ((e (car bgl-jump-stack)))
      (setq bgl-jump-stack (cdr bgl-jump-stack))
      (message "buffer=%s pos=%s" (cadr e) (caddr e))
      (switch-to-buffer (cadr e))
      (goto-char (caddr e)))))

;*---------------------------------------------------------------------*/
;*    bgl-goto-def ...                                                 */
;*---------------------------------------------------------------------*/
(defun bgl-goto-def ()
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (or (bgl-visit-decl-at-point sym nil)
	(bgl-browse-doc-at-point sym))))

;*---------------------------------------------------------------------*/
;*    bgl-goto-other-frame-def ...                                     */
;*---------------------------------------------------------------------*/
(defun bgl-goto-other-frame-def ()
  (interactive)
  (let ((sym (thing-at-point 'symbol t)))
    (or (bgl-visit-decl-at-point sym t)
	(bgl-browse-doc-at-point sym))))
