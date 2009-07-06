;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-config.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:28:09 1998                          */
;*    Last change :  Mon Feb 18 10:16:47 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Cee configuration file.                                      */
;*    -------------------------------------------------------------    */
;*    Bdb configuration is set into cee-bdb.el                         */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-config)
(require 'ude-custom)
(require 'dbg-config)

;; cee version
(defconst cee-version "0.2"
  "*The Cee version.")

;; cee group
(defgroup cee nil
  "Bigloo Emacs Environment."
  :tag "Cee"
  :prefix "cee-"
  :group 'processes)

;; online documentation preferred format
(defcustom cee-docline-preferred-format '((java-mode . html) (t . info))
  "*The docline preferred format."
  :group 'cee)

;; online documentation preferred format
(defcustom cee-docline-files '((java-mode . "file:///usr/java/doc/jdk1.3.1/api/java/lang/package-summary.html"))
  "*The docline external files."
  :group 'cee)

;; online documentation info pages
(defcustom cee-info-file-list '("libc.info" "ccp.info" "readline.info")
  "*The info docline pages."
  :group 'cee
  :type '(repeat (string)))

;; indentation
(defcustom cee-external-indent "indent"
  "*The external Cee indent program."
  :group 'cee
  :type 'string)

(defcustom cee-external-indent-opt "-st"
  "*The external Cee indent program options."
  :group 'cee
  :type 'string)

;; tags
(defcustom cee-make-tags (concat ude-make " " ude-tagsfile-name)
  "*The name of the command to build the tagsfile."
  :group 'cee
  :type 'string)

;; use highligh identifier to jump to definitions
(defcustom cee-tags-balloon-p t
  "*Non-nil means that identifier below the mouse are highlighed for
a short [find-definition] facility."
  :group 'cee
  :type 'boolean)

;; user menu
(defcustom cee-user-menu nil
  "*Non-nil means a popup menu user part."
  :group 'cee
  :type 'boolean)

;; tmp dir (for macro expansion)
(defcustom cee-tmp-dir (let ((dir (getenv "TMPDIR")))
		      (if (stringp dir)
			  dir
			"/tmp"))
  "*The tmp directory."
  :group 'cee
  :type 'string)

;; cee profiling face
(defcustom cee-profile-face 'font-lock-function-name-face
  "The face to display Bigloo identifiers on profile."
  :group 'cee
  :type 'symbol)

;; a name of a binary file that implements a profiler
(defcustom cee-profiler (file-installed-p "xprof" exec-path)
  "*The name of a visual profiler."
  :group 'cee
  :type '(choice (boolean) (string)))

;; the option to the profiler when called from the cee
(defcustom cee-profiler-cee-options '("-no-event-loop" "--emacs")
  "*The profiler cee options"
  :group 'cee
  :type '(repeat (string)))

;; a name of a binary file that implements the Bigloo debugger
(defcustom cee-kbdb (file-installed-p "kbdb" exec-path)
  "*The name of the Bigloo debugger."
  :group 'cee
  :type '(choice (boolean) (string)))

;; the options send to kbdb when called from the cee
(defcustom cee-kbdb-cee-options '("-no-event-loop"
				  "--bee"
				  "--no-heap-explorer"
				  "--no-demangling")
  "*The kbdb cee options"
  :group 'cee
  :type '(repeat (string)))

;; cee fontification
(defcustom cee-font-lock-keywords
  (list
   (cons "//.*$"
	 'font-lock-comment-face)
   (cons "^\\([*a-zA-Z0-9_:.]+[ ,\t]*\\)+"
	 'font-lock-function-name-face)
   (cons (concat "\\("
		 "[a-zA-Z_.0-9]+::"
		 "\\|"
		 (concat "^#[\t ]*\\(define\\|include\\|if\\|endif"
			 "\\|else\\|undef\\|ifdef\\|ifndef\\)\\>")
		 "\\)")
	 'ude-font-lock-face-4)
   (cons (concat "\\("
		 "[*&[]\\|\]"
		 "\\|"
		 "\\(\\<goto\\>[ \t]+[_a-zA-Z0-9]+\\|[a-zA-Z0-9_]+[ \t]*:\\)"
		 "\\)")
	 'ude-font-lock-face-2)
   (list "<\\([^ \t\n>]+\\)>"
	 1
	 'font-lock-string-face)
   (cons (concat "\\(\\<\\(for\\|case\\|do\\|while\\|switch\\|if\\|else"
		 "\\|return\\|continue\\|break\\|virtual"
		 "\\|new\\|malloc\\)\\>\\|[{}]\\)")
	 'font-lock-keyword-face))
  "The Cee font-lock specification."
  :group 'cee)

;*---------------------------------------------------------------------*/
;*    cee-customize ...                                                */
;*    -------------------------------------------------------------    */
;*    This function invoked CEE customization in a new frame.          */
;*---------------------------------------------------------------------*/
(defun cee-customize ()
  "Invokes Ude customization in a new frame."
  (interactive)
  (if (ude-empty-window-p)
      (customize-group 'cee)
    (let ((pop-up-frames t))
      (customize-group-other-window 'cee))))






