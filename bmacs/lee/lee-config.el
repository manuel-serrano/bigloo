;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-config.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:28:09 1998                          */
;*    Last change :  Wed Jan 30 06:08:58 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Lee configuration file.                                      */
;*    -------------------------------------------------------------    */
;*    Bdb configuration is set into lee-bdb.el                         */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-config)
(require 'ude-custom)

;; lee version
(defconst lee-version "0.0"
  "*The Lee version.")

;; lee group
(defgroup lee nil
  "Bigloo Emacs Environment."
  :tag "Lee"
  :prefix "lee-"
  :group 'processes)

;; online documentation info pages
(defcustom lee-info-file-list '("lispref.info")
  "*The info docline pages."
  :group 'lee
  :type '(repeat (string)))

;; tags
(defcustom lee-make-tags (concat ude-make " " ude-tagsfile-name)
  "*The name of the command to build the tagsfile."
  :group 'lee
  :type 'string)

;; user menu
(defcustom lee-user-menu nil
  "*Non-nil means a popup menu user part."
  :group 'lee
  :type 'boolean)

;; tmp dir (for macro expansion)
(defcustom lee-tmp-dir (let ((dir (getenv "TMPDIR")))
		      (if (stringp dir)
			  dir
			"/tmp"))
  "*The tmp directory."
  :group 'lee
  :type 'string)

;; fontification
(defvar lee-font-lock-keywords
  (list
   (list (concat "\(\\(\\(defun\\|defmacro\\|defvar\\|defgroup"
		 "\\|defcustom\\|defface\\|defconst\\)[ ]+[^ \n]+\\)[ \n]")
	 1
	 'font-lock-function-name-face)
   (list "\(\\(provide\\|require\\|autoload\\)[ \n]"
	 1
	 'ude-font-lock-face-1)
   (list (concat "\(\\(let\\|let[*]\\|labels"
		 "\\|setq\\|lambda\\|if\\|progn\\|cond\\)[ \n]")
	 1
	 'font-lock-keyword-face)
   (list "\(\\(condition-case\\|error\\|interactive\\)\\>"
	 1
	 'ude-font-lock-face-2)
   (list "\(\\(define-key\\|global-set-key\\|local-set-key\\)"
	 1
	 'ude-font-lock-face-4)
   (cons ":\\w+" 'ude-font-lock-face-5)
   (cons "\\(`\\|,\\|'\\|,@\\)"
	 'ude-font-lock-face-2)))

;*---------------------------------------------------------------------*/
;*    lee-customize ...                                                */
;*    -------------------------------------------------------------    */
;*    This function invoked LEE customization in a new frame.          */
;*---------------------------------------------------------------------*/
(defun lee-customize ()
  "Invokes Ude customization in a new frame."
  (interactive)
  (if (ude-empty-window-p)
      (customize-group 'lee)
    (let ((pop-up-frames t))
      (customize-group-other-window 'lee))))






