;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-config.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:28:09 1998                          */
;*    Last change :  Fri Mar  3 11:16:56 2017 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bee configuration file.                                      */
;*    -------------------------------------------------------------    */
;*    Bdb configuration is set into bee-bdb.el                         */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-config)
(require 'ude-custom)
(require 'ude-config)

;; bee version
(defconst bee-version "0.5"
  "*The Bee version.")

;; bee group
(defgroup bee nil
  "Bigloo Emacs Environment."
  :tag "Bee"
  :prefix "bee-"
  :group 'processes)

;; additional directories for online documentation
(defcustom bee-docdir '()
  "*Additional directories for online documentation."
  :group 'bee
  :type '(repeat (string)))

;; Bigloo suffix
(defcustom bee-suffixes '("scm" "bgl" "sch" "bee")
  "*Bigloo source suffixes."
  :group 'bee
  :type '(repeat (string)))

;; Bee root file
(defcustom bee-root ".bee"
  "*Bigloo root file name ."
  :group 'bee
  :type 'string)

;; online documentation preferred format
(defcustom bee-doc-preferred-format 'html
  "*The online documentation preferred format."
  :group 'bee
  :type '(choice (const html) (const info)))

;; online documentation info pages
(defcustom bee-info-file-list '("bigloo.info" "biglook.info" "r5rs.info")
  "*The info documentation pages."
  :group 'bee
  :type '(repeat (string)))

;; compiler and interpreter
(defcustom bee-bigloo "bigloo"
  "*The name of the Bigloo compiler."
  :group 'bee
  :type 'string)

;; C extractor
(defcustom bee-cigloo "cigloo"
  "*The name of the Cigloo binary file."
  :group 'bee
  :type 'string)

;; indentation
(defcustom bee-external-indent "bglpp"
  "*The external Bee indent program."
  :group 'bee
  :type 'string)

(defcustom bee-forced-indent-regexp ";;;"
  "*The regexp that mark a forced indentation"
  :group 'bee
  :type 'string)

(defcustom bee-indent-on-keyword-p t
  "*Force special indentation for keywords arguments"
  :group 'bee
  :type 'boolean)

(defcustom bee-indent-on-quote-p t
  "*Force special indentation for quoted lists and vectors"
  :group 'bee
  :type 'boolean)

(defcustom bee-indent-mode 'hop
  "*Force special indentation for quoted lists and vectors"
  :group 'bee
  :type '(choice (const scheme) (const hop)))

(defcustom bee-indent-style 'left
  "*Force special indentation for quoted lists and vectors"
  :group 'bee
  :type '(choice (const (left) (const column))))

;; bigloo make-make
(defcustom bee-bmake "bglmake"
  "*The name of the Bigloo Makefile generator.
Setting that variable must change the buffer local UDE-MAKEMAKE"
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq ude-makemake value)))

;; bee-bmake-application-option
(defcustom bee-bmake-application-option "-fapplication -main"
  "*The name of the bmake option to produce an application Makefile"
  :group 'bee
  :type 'string)

;; bee-bmake-library-option
(defcustom bee-bmake-library-option "-flibrary -heap"
  "*The name of the bmake option to produce a library Makefile"
  :group 'bee
  :type 'string)

;; bee-identifier-syntax
(defcustom bee-identifier-syntax 'bigloo
  "*The identifier syntax"
  :group 'bee
  :type '(choice (const bigloo) (const r5rs)))

;; bee fontification
(defcustom bee-font-lock-keywords
  (list
;*    (list (concat "^\(\\(\\(?:define[*]?\\|define-inline"            */
;* 		 "\\|define-struct\\|define-record-type\\|define-record" */
;* 		 "\\|define-macro\\|define-generic\\|define-method"    */
;* 		 "\\|define-syntax\\|define-expander"                  */
;* 		 "\\|define-class\\|define-abstract-class\\|define-final-class" */
;* 		 "\\|define-service\\|define-parameter\\|define-preferences" */
;* 		 "\\|define-tag"                                       */
;* 		 "\\|define-command\\)[ ]+[^ ]+\\)[ \n]")              */
;* 	 1                                                             */
;* 	 'font-lock-function-name-face)                                */
   (cons (concat "\\(?:define\\|define-inline"
		 "\\|define-struct\\|define-record-type\\|define-record"
		 "\\|define-macro\\|define-generic\\|define-method"
		 "\\|define-syntax\\|define-expander"
		 "\\|define-class\\|define-abstract-class\\|define-final-class"
		 "\\|define-service\\|define-parameter\\|define-preferences"
		 "\\|define-tag"
		 "\\|define-command\\)[ ]")
	 'font-lock-function-name-face)
   (list (concat "^\(\\(?:define\\|define-inline"
		 "\\|define-struct\\|define-record-type\\|define-record"
		 "\\|define-macro\\|define-generic\\|define-method"
		 "\\|define-syntax\\|define-expander"
		 "\\|define-class\\|define-abstract-class\\|define-final-class"
		 "\\|define-service\\|define-parameter\\|define-preferences"
		 "\\|define-tag"
		 "\\|define-command\\)[ ]\(?\\([^ \t\n]+\\)")
	 1
	 'font-lock-function-name-face)
   (list "\(\\(\\(?:module\\|interface\\)[ ]+[^ \n]+\\)[ \t\n]"
         1
	 'ude-font-lock-face-1)
   (list "\(\\(directives\\)"
         1
	 'ude-font-lock-face-1)
   (list "[']\\([^ ),[(#]\\([^ \n\t[()#]\\|]\\)*\\)"
	 1
	 'font-lock-string-face)
   (cons "\\(?:::[^ \n)]+\\|#![a-zA-Z]+\\|:,@\\|[`,]\\)"
	 'ude-font-lock-face-4)
   (cons "[ \n\t(]:[^ :\n\t]+\\|[^ :\n\t(]+:[ \n\t)]"
	 'ude-font-lock-face-11)
   (list (concat "\(\\(type\\|export\\|import\\|option\\|eval\\|eval!\\|main\\|with"
		 "\\|from\\|static\\|use\\|library"
		 "\\|include\\|foreign\\|extern\\|java\\|Cforeign"
		 "\\|require:\\|provide:\\)[ \t\n]")
	 1
	 'font-lock-type-face)
   (list (concat "\(\\(class\\|wide-class\\|final-class\\|abstract-class"
		 "\\|generic\\|inline\\|macro\\|expander\\|syntax"
		 "\\|infix[ ]macro\\)[ \t\n]")
	 1
	 'ude-font-lock-face-3)
   (cons "~\\|with-hop" 'ude-font-lock-face-8)
   (cons "[$][^( \t\n]*" 'ude-font-lock-face-10)
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
	 'ude-font-lock-face-2)
   (list (concat "\(\\(error\\|error/location\\|error/source\\|warning\\|pragma\\|trace\\|"
		 "with-trace\\|trace-item\\|"
		 "bind-exit\\|call/cc\\|try\\|unwind-protect\\|cond-expand\\|"
		 "with-exception-handler\\|with-handler\\|with-alarm\\|current-exception-handler\\|raise\\|"
		 "profile\\|profile/gc\\|delay\\|force\\)"
		 "[ \n\t:]")
	 1
	 'ude-font-lock-face-8)
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
	 'ude-font-lock-face-8)
   (list (concat "\(\\(sqlite-format\\>\\|sqlite-eval\\>\\|sqlite-exec\\>"
		 "\\|sqlite-map\\|sqlite-close\\>"
		 "\\|sqlite-table-informations\\>"
		 "\\|sqlite-table-number-of-rows\\>"
		 "\\|sqlite-table-name-of-columns\\>"
		 "\\|sqlite-name-of-tables\\>"
		 "\\|sqlite-dump\\>\\|sqlite-dump-table\\>"
		 "\\|sqlite-last-insert-rowid\\>\\)")
	 1
	 'ude-font-lock-face-9)
   (list "\(\\([a-zA-Z0-9_!=%$@^*<>?/-]+&\\)[ \t\n)]"
	 1
	 'ude-font-lock-face-7)
   (list "\(\\(<[^> \t\n]+>\\)[ \n\t)]"
	 1
	 'ude-font-lock-face-12))
  "The Bee font-lock specification."
  :group 'bee)

;; bee menu
(defcustom bee-imenu-generic-expression
  '((nil
     "^(define\\(\\|-\\(generic\\(\\|-procedure\\)\\|method\\)\\)*\\s-+(?\\(\\sw+\\)" 4)
    ("Types"
     "^(define-struct\\s-+(?\\(\\sw+\\)" 1)
    ("Macros"
     "^(\\(define-expander\\|define-macro\\|define-syntax\\)\\s-+(?\\(\\sw+\\)" 2))
  "Imenu generic expression for Scheme mode.  See `imenu-generic-expression'."
  :group 'bee)

;; compile configuration
(defcustom bee-compilation-font-lock-keywords
  '(("^cd.+$" . ude-modeline-root-face)
    ("^#.*$" . font-lock-comment-face)
    ("^File.+:$" . font-lock-function-name-face)
    ("^.+:$" . ude-ok-face)
    ("[0-9]+ error.? occured, ending.*$" . ude-error-face)
    ("[*][*][*] WARNING.*$" . font-lock-comment-face)
    ("Compilation finished.*$" . ude-ok-face))
  "The regular expressions for compilation font lock errors."
  :group 'bee)

(defcustom bee-error-regexp "^File \"\\([^\"]+\\)\", line \\([0-9]+\\)"
  "*The expression that matches Bigloo errors."
  :group 'bee
  :type 'string)

(defcustom bee-compilation-error-regexp-alist (list bee-error-regexp 1 2)
  "The expression for compilation errors."
  :group 'bee)

;; afile
(defcustom bee-afile-name ".afile"
  "*The name of the Bee afile file."
  :group 'bee
  :type 'string)

(defcustom bee-make-afile (concat ude-make " " bee-afile-name)
  "*The name of the command to build the afile."
  :group 'bee
  :type 'string)

;; tags
(defcustom bee-make-tags (concat ude-make " " ude-tagsfile-name)
  "*The name of the command to build the tagsfile."
  :group 'bee
  :type 'string)

;; profiling faces
(defcustom bee-ident-profile-face 'font-lock-function-name-face
  "The face to display Bigloo identifiers on profile."
  :group 'bee
  :type 'symbol)

(defcustom bee-module-profile-face 'ude-font-lock-face-1
  "The face to display Bigloo modules on profile."
  :group 'bee
  :type 'symbol)

(defface bee-c-profile-face
  '((((class color)) (:foreground "gray75" :bold nil))
    (t (:bold t)))
  "The face to display C identifiers on profile."
  :group 'bee)

;; user menu
(defcustom bee-user-menu nil
  "*Non-nil means a popup menu user part."
  :group 'bee
  :type 'boolean)

;; tmp dir (for macro expansion)
(defcustom bee-tmp-dir (let ((dir (getenv "TMPDIR")))
			 (if (stringp dir)
			     dir
			   "/tmp"))
  "*The tmp directory."
  :group 'bee
  :type 'string)

;; use highligh identifier to jump to definitions
(defcustom bee-tags-balloon-p t
  "*Non-nil means that identifier below the mouse are highlighed for
a short [find-definition] facility."
  :group 'bee
  :type 'boolean)

;; a name of a binary file that implements a Bigloo interface builder
(defcustom bee-interface-builder (file-installed-p "buildoo" exec-path)
  "*The name of an interace builder."
  :group 'bee
  :type '(choice (boolean) (string)))

;; the option to the interface builder when called from the bee
(defcustom bee-interface-builder-bee-options '("-no-event-loop" "--bee")
  "*The interface builder bee options"
  :group 'bee
  :type '(repeat (string)))

;; the file suffixes for interface file
(defcustom bee-interface-builder-suffix (list "bld")
  "*The suffix for interface builder files."
  :group 'bee
  :type '(repeat (string)))

;; a name of a binary file that implements a profiler
(defcustom bee-profiler (file-installed-p "kprof" exec-path)
  "*The name of a visual profiler."
  :group 'bee
  :type '(choice (boolean) (string)))

;; the option to the profiler when called from the bee
(defcustom bee-profiler-bee-options '("-no-event-loop" "--emacs")
  "*The profiler bee options"
  :group 'bee
  :type '(repeat (string)))

;; a name of a binary file that implements a bigloo browser
(defcustom bee-browser (file-installed-p "kbrowse" exec-path)
  "*The name of a visual browser."
  :group 'bee
  :type '(choice (boolean) (string)))

;; the kbrowse version we are configurated for
(defcustom bee-browser-version 2
  "*The version of kbrowse Bee is tuned for."
  :group 'bee
  :type '(choice (number)))

;; the option to the browser when called from the bee
(defcustom bee-browser-bee-options '("-no-event-loop" "--emacs")
  "*The browser bee options"
  :group 'bee
  :type '(repeat (string)))

;; the type of debugger to be used
(defcustom bee-debugger nil
  "*The debugger to be used"
  :group 'bee
  :type '(choice (boolean) (const bugloo) (const bdb)))

;; a name of a binary file that implements the Bigloo debugger
(defcustom bee-kbdb (file-installed-p "kbdb" exec-path)
  "*The name of the Bigloo debugger."
  :group 'bee
  :type '(choice (boolean) (string)))

;; the options send to kbdb when called from the bee
(defcustom bee-kbdb-bee-options '("-no-event-loop" "--bee" "-mode" "scheme")
  "*The kbdb bee options"
  :group 'bee
  :type '(repeat (string)))

;; select if when the import is clicked the whole module is import or
;; only the wanted binding
(defcustom bee-import-mode 'all
  "*The bee import mode"
  :group 'bee
  :type 'symbol)

;; key bindings
(defcustom bee-elisp-like-keymap-p 'nil
  "*Uses Elisp like REPL keys"
  :group 'bee
  :type 'boolean)

;* ;; the name of the begoo binary file                                */
;* (defcustom bee-begoo (file-installed-p "begoo" exec-path)           */
;*   "*The name of the Begoo tool."                                    */
;*   :group 'bee                                                       */
;*   :type '(choice (boolean) (string)))                               */
;*                                                                     */
;* ;; the name of the Makefile entry the delivers the name of the bmg file */
;* (defcustom bee-begoo-getbmg-entry "getbegoo"                        */
;*   "*The name of the Makefile entry that delivers the name of the bmg file." */
;*   :group 'bee                                                       */
;*   :type 'string)                                                    */
;*                                                                     */
;* ;; the option to begoo when called from the bee                     */
;* (defcustom bee-begoo-bee-options '("-no-event-loop" "--emacs")      */
;*   "*The Begoo bee options"                                          */
;*   :group 'bee                                                       */
;*   :type '(repeat (string)))                                         */

;*---------------------------------------------------------------------*/
;*    bee-customize ...                                                */
;*    -------------------------------------------------------------    */
;*    This function invoked BEE customization in a new frame.          */
;*---------------------------------------------------------------------*/
(defun bee-customize ()
  "Invokes Ude customization in a new frame."
  (interactive)
  (if (ude-empty-window-p)
      (customize-group 'bee)
    (let ((pop-up-frames t))
      (customize-group-other-window 'bee))))

