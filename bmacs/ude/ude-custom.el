;*=====================================================================*/
;*    serrano/emacs/site-lisp/bigloo/ude-custom.el                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  8 07:29:03 1998                          */
;*    Last change :  Wed Feb 22 14:42:08 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `Unix Development Environment' custom environment. This      */
;*    file contains all the Ude defcustom and Ude defface.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-custom)
(require 'ude-autoload)

;*---------------------------------------------------------------------*/
;*    The Ude group                                                    */
;*---------------------------------------------------------------------*/
(defgroup ude nil
  "Unix Development Environment."
  :tag "Ude"
  :prefix "ude-"
  :group 'processes)

;*---------------------------------------------------------------------*/
;*    URL browsers                                                     */
;*---------------------------------------------------------------------*/
(defcustom ude-url-browser "netscape"
  "*The binary file to run for browing URLs or nil for Emacs mode."
  :group 'ude
  :type '(choice string (const nil)))

;*---------------------------------------------------------------------*/
;*    Grep ...                                                         */
;*---------------------------------------------------------------------*/
(defcustom ude-grep "grep"
  "*The name of the GREP binary file."
  :group 'ude
  :type 'string)

(defcustom ude-grep-w "grep -w -E -e"
  "*The name of the EGREP -w (exact word) command."
  :group 'ude
  :type 'string)

(defcustom ude-egrep "grep -E -e"
  "*The name of the EGREP command."
  :group 'ude
  :type 'string)

(defcustom ude-egrep-n "grep -E -n -e"
  "*The name of the EGREP with number command."
  :group 'ude
  :type 'string)

(defcustom ude-uncase-grep "grep -i"
  "*The name of the case insensitive GREP command."
  :group 'ude
  :type 'string)

;*---------------------------------------------------------------------*/
;*    Root Directory ...                                               */
;*---------------------------------------------------------------------*/
(defcustom ude-root-search-depth 3
  "*The depth search for the Ude Root Directory."
  :group 'ude
  :type 'number)

;*---------------------------------------------------------------------*/
;*    Tags and association files ...                                   */
;*---------------------------------------------------------------------*/
(defcustom ude-tagsfile-name ".etags"
  "*The name for TAGS table file."
  :group 'ude
  :type 'string)

(defcustom ude-afile-name ".afile"
  "*The name of the Ude module association file."
  :group 'ude
  :type 'string)

;*---------------------------------------------------------------------*/
;*    Make and Makefiles ...                                           */
;*---------------------------------------------------------------------*/
(defcustom ude-make (if (stringp (getenv "UDEMAKE"))
				(getenv "UDEMAKE")
				"make")
  "*The name of the MAKE binary file."
  :group 'ude
  :type 'string)

(defcustom ude-make-error-output "[*][*][*]\\| Stop"
  "*A regular expression that matches make error (such as `no rule' error)."
  :group 'ude
  :type 'string)

(defcustom ude-makefile "Makefile"
  "*A string, the name of the Makefile file."
  :group 'ude
  :type 'string)

(defcustom ude-sort-makefile-entries nil
  "*True means Makefile entries are sorted for popup."
  :group 'ude
  :type 'boolean)

(defcustom ude-makefile-entry-regexp "^[^.:\t $][^ %$:]*[ \t]*"
  "*The regexp that matches Makefile entry declaration (without the :)."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-entry-exclude-regexp "[.]o[ \t]*$\\|^%."
  "*The regexp for excluding makefile entry matches."
  :group 'ude
  :type 'string)

(defcustom ude-make-ude-entry "ude"
  "*The name of the Makefile entry that compute Ude Makefile entries."
  :group 'ude
  :type 'string)
  
(defcustom ude-makefile-update-entry "updatemakefile"
  "*The name of the Makefile entry that udpates the Makefile."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-binary-entry "getbinary"
  "*The name of the binary file built by the Makefile."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-binary_p-entry "getbinary_p"
  "*The name of the profiled binary file built by the Makefile."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-mode-entry "getmode"
  "*The Makefile's mode."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-revision-entry "revision"
  "*The name of the Makefile entry that give the current revision."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-tar-gz-entry "tar.gz"
  "*The name of the Makefile entry that builds a tar.gz file."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-infofile-entry "infofile"
  "*The name of the Makefile entry that give file revision."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-infofile-args "FILE"
  "*The name of the Makefile file variable."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-checkout-entry "checkoutfile"
  "*The name of the Makefile entry that checks a file out."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-fileversion-args "FILEVERSION"
  "*The name of the Makefile file version variable."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-getsources-entry "getsources"
  "*The name of the Makefile entry that checks a file out."
  :group 'ude
  :type 'string)

;*---------------------------------------------------------------------*/
;*    Compilation ...                                                  */
;*---------------------------------------------------------------------*/
(defcustom ude-compilation-window-height window-min-height
  "*Number of lines in a compilation window. If nil, use Emacs default."
  :group 'ude
  :type '(choice number
		 (const nil)))

(defcustom ude-compilation-frame-height (let ((v (/ (frame-height) 3)))
					  (if (>= v 20)
					      v
					    20))
  "*Number of lines in a compilation frame."
  :group 'ude
  :type 'number)

(defcustom ude-compilation-frame-top-correct
  (let ((env (getenv "EMACS_CORRECT_WM_TOP")))
    (if (> (length env) 0)
	(string-to-number env)
      0))
  "*Number of pixel to correct the compilation frame top position"
  :group 'ude
  :type 'number)
  
(defcustom ude-delete-compilation-window-on-success t
  "*True means that compilation window are destroyed on success."
  :group 'ude
  :type 'boolean)

(defcustom ude-sound-compilation-on-error t
  "*True means play the error sound when compilation aborts."
  :group 'ude
  :type 'boolean)

(defcustom ude-pop-compilation-frame-on-error nil
  "*True means pop up a new frame printing compilation errors."
  :group 'ude
  :type 'boolean)

(defcustom ude-enlarge-compilation-window-on-error t
  "*True means enlarge the current compilation window on abortion."
  :group 'ude
  :type 'boolean)

(defcustom ude-always-detach-compile-buffer t
  "*True means that compilation buffer are always displayed detached."
  :group 'ude
  :type 'boolean)

;*---------------------------------------------------------------------*/
;*    Profiling ...                                                    */
;*---------------------------------------------------------------------*/
(defcustom ude-makefile-profile-entry "profile_comp"
  "*The name of the Makefile entry that compile for profiling."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-extra-profile-entry "extra_profile_comp"
  "*The name of the Makefile entry that compile for high profiling."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-clean-profile-entry "profile_clean"
  "*The name of the Makefile entry that compile for high profiling."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-run-profile-entry "profile_run"
  "*The name of the Makefile entry that compile for profiling."
  :group 'ude
  :type 'string)

(defcustom ude-makefile-run-profile-args "PROFILE_ARGS"
  "*The argument for running a profile."
  :group 'ude
  :type 'string)

(defcustom ude-profile-file-name "PROF"
  "*The name of a profile file."
  :group 'ude
  :type 'string)

;*---------------------------------------------------------------------*/
;*    Errors ...                                                       */
;*---------------------------------------------------------------------*/
(defcustom ude-error-sound-p nil
  "*True means Ude plays a sound on error"
  :group 'ude
  :type 'boolean)

(defcustom ude-error-sound-file "etc/bass-snap.au"
  "*The file name for the sound to be played on error"
  :group 'ude
  :type 'string)

(defcustom ude-error-sound-volume 80
  "*The volume of error sound"
  :group 'ude
  :type 'number)

(defface ude-error-face
  (list (list '((class color))
	      (list ':foreground "red"
		    ':bold nil))
	'(t (:bold t)))
  "Ude error face"
  :group 'ude)

;*---------------------------------------------------------------------*/
;*    Key bindings and Menus ...                                       */
;*---------------------------------------------------------------------*/
(defcustom ude-balloon-binding (if (featurep 'xemacs) [(button1)] [(down-mouse-1)])
  "*The mouse binding for Bee mouse pseudo-balloon event."
  :group 'ude
  :type 'vector)

(defcustom ude-mouse-binding (if (featurep 'xemacs) [(button3)] [(down-mouse-3)])
  "*The mouse binding for Bee mouse event."
  :group 'ude
  :type 'vector)

(defcustom ude-mouse-binding-disable (if (featurep 'xemacs) nil [(mouse-3)])
  "*The mouse binding to disable for Bee mouse event."
  :group 'ude
  :type 'vector)

(defcustom ude-mouse-2-binding (if (featurep 'xemacs) [(button2)] [(mouse-2)])
  "*The mouse binding for Bee mouse event."
  :group 'ude
  :type 'vector)

(defcustom ude-mouse-2-binding-disable (if (featurep 'xemacs) nil nil)
  "*The mouse binding to disable for Bee mouse event."
  :group 'ude
  :type 'vector)

(defcustom ude-menu-max-width 30
  "*The maximum width of bee menus."
  :group 'ude
  :type 'number)

(defcustom ude-menu-max-entry 30
  "*The maximum entry of bee menus."
  :group 'ude
  :type 'number)

;*---------------------------------------------------------------------*/
;*    Tool-bar                                                         */
;*---------------------------------------------------------------------*/
(defcustom ude-tool-bar-force-legend-p nil
  "*True means that legends are forced in the tool-bar."
  :group 'ude
  :type 'boolean)


(defcustom ude-toolbar-image-directory "images"
  "*Subdirectory of bmacs elisp directory containing toolbar images."
  :group 'ude
  :type 'string)
  
;*---------------------------------------------------------------------*/
;*    Font Lock                                                        */
;*---------------------------------------------------------------------*/
(defcustom ude-font-lock-p t
  "*True means uses font lock for Ude buffers."
  :group 'ude
  :type 'boolean)

(defcustom ude-paren-adapt-p t
  "*True means change parentheses highlighting."
  :group 'ude
  :type 'boolean)

(defface ude-font-lock-face-1
  '((((class color) (background light)) (:foreground "slateblue3" :bold t))
    (((class color) (background dark)) (:foreground "Plum1" :bold t))
    (t (:bold t)))
  "Bee face 1."
  :group 'ude)
(defvar ude-font-lock-face-1 'ude-font-lock-face-1)

(defface ude-font-lock-face-2
  '((((class color) (background light)) (:foreground "blue" :bold t))
    (((class color) (background dark)) (:foreground "tomato2" :bold t))
    (t (:bold t)))
  "Bee face 2."
  :group 'ude)
(defvar ude-font-lock-face-2 'ude-font-lock-face-2)

(defface ude-font-lock-face-3
  '((((class color) (background light)) (:foreground "tomato2" :bold t))
    (((class color) (background dark)) (:foreground "SkyBlue" :bold t))
    (t (:bold t)))
  "Bee face 3."
  :group 'ude)
(defvar ude-font-lock-face-3 'ude-font-lock-face-3)

(defface ude-font-lock-face-4
  '((((class color) (background light)) (:foreground "green3" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t))
    (t (:bold t)))
  "Bee face 4."
  :group 'ude)
(defvar ude-font-lock-face-4 'ude-font-lock-face-4)

(defface ude-font-lock-face-5
  '((((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Bee face 5."
  :group 'ude)
(defvar ude-font-lock-face-5 'ude-font-lock-face-5)

(defface ude-font-lock-face-6
  '((((class color) (background light)) (:foreground "BlueViolet" :bold t))
    (((class color) (background dark)) (:foreground "yellow" :bold t))
    (t (:bold t)))
  "Bee face 6."
  :group 'ude)
(defvar ude-font-lock-face-6 'ude-font-lock-face-6)

(defface ude-font-lock-face-7
  '((((class color) (background light)) (:foreground "green4" :bold t))
    (((class color) (background dark)) (:foreground "green2" :bold t))
    (t (:bold t)))
  "Bee face 7."
  :group 'ude)
(defvar ude-font-lock-face-7 'ude-font-lock-face-7)

(defface ude-font-lock-face-8
  '((((class color) (background light)) (:foreground "#ff2020" :bold t))
    (((class color) (background dark)) (:foreground "red2" :bold t))
    (t (:bold t)))
  "Bee face 8."
  :group 'ude)
(defvar ude-font-lock-face-8 'ude-font-lock-face-8)

(defface ude-font-lock-face-9
  '((((class color) (background light)) (:foreground "#e72c9f" :bold t))
    (((class color) (background dark)) (:foreground "blue2" :bold t))
    (t (:bold t)))
  "Bee face 9."
  :group 'ude)
(defvar ude-font-lock-face-9 'ude-font-lock-face-9)

(defface ude-font-lock-face-10
  '((((class color) (background light)) (:foreground "#492ead" :bold t))
    (((class color) (background dark)) (:foreground "#492ead" :bold t))
    (t (:bold t)))
  "Bee face 10."
  :group 'ude)
(defvar ude-font-lock-face-10 'ude-font-lock-face-10)

(defface ude-font-lock-face-11
  '((((class color) (background light)) (:foreground "#492ead" :bold nil))
    (((class color) (background dark)) (:foreground "#492ead" :bold nil))
    (t (:bold t)))
  "Bee face 11."
  :group 'ude)
(defvar ude-font-lock-face-11 'ude-font-lock-face-11)

(defface ude-font-lock-face-12
  '((((class color) (background light)) (:foreground "#87910F" :bold t))
    (((class color) (background dark)) (:foreground "#701680" :bold t))
    (t (:bold t)))
  "Bee face 12."
  :group 'ude)
(defvar ude-font-lock-face-12 'ude-font-lock-face-12)

;; cf ude-invisible-face
(if (featurep 'xemacs)
    (progn
      (defface ude-modeline-root-face
	`((((class color)) (:foreground "red" :bold t))
	  (t (:bold t)))
	"Ude modeline Root face."
	:group 'ude)
      (set-face-background 'ude-modeline-root-face
			   (face-background 'modeline)))
  (defface ude-modeline-root-face
    (list (list '((class color))
		(list ':foreground "red"
		      ':bold t))
	  '(t (:bold t)))
    "Ude modeline Root face."
    :group 'ude))

(defvar ude-modeline-root-face 'ude-modeline-root-face)

;; cf ude-invisible-face
(if (featurep 'xemacs)
    (progn
      (defface ude-modeline-no-root-face
	'((((class color)) (:foreground "blue" :bold t))
	  (t (:bold t)))
	"Ude modeline Root face."
	:group 'ude)
      (set-face-background 'ude-modeline-no-root-face
			   (face-background 'modeline)))
  (defface ude-modeline-no-root-face
    (list (list '((class color))
		(list ':foreground "blue"
		      ':bold t))
	  '(t (:bold t)))
    "Ude modeline No Root face."
    :group 'ude))

(defvar ude-modeline-no-root-face 'ude-modeline-no-root-face)

(defface ude-error-face
  '((((class color)) (:foreground "red" :bold t))
    (t (:bold t)))
  "Bee error face."
  :group 'ude)
(defvar ude-error-face 'ude-error-face)

(defface ude-ok-face
  '((((class color) (background light)) (:foreground "green3" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t))
    (t (:bold t)))
  "Bee ok face."
  :group 'ude)
(defvar ude-ok-face 'ude-ok-face)

(defface ude-italic-face
  '((((class color)) (:bold t :italic t))
    (t (:bold t)))
  "Bee face to display italic text."
  :group 'ude)

(condition-case ()
    ;; sometimes I hate emacs :-(
    ;; Why do I have to plut a condition-case here???
    (defface ude-invisible-face
      (list (list '((class color))
		  (list ':foreground (face-background 'ude-italic-face))))
      "Bee face to display italic text."
      :group 'ude)
  (error '()))

;*---------------------------------------------------------------------*/
;*    Repl                                                             */
;*---------------------------------------------------------------------*/
(defcustom ude-place-to-start-repl (list 'other-frame)
  "Whether to start the REPL buffer in another frame or in another window."
  :group 'ude
  :type '(list
          (radio-button-choice
           (const :tag "Other Window" other-window)
           (const :tag "Other Frame" other-frame))))

;*---------------------------------------------------------------------*/
;*    ude-customize ...                                                */
;*    -------------------------------------------------------------    */
;*    This function invoked UDE customization in a new frame.          */
;*---------------------------------------------------------------------*/
(defun ude-customize ()
  "Invokes Ude customization in a new frame."
  (interactive)
  (if (ude-empty-window-p)
      (customize-group 'ude)
    (let ((pop-up-frames t))
      (customize-group-other-window 'ude))))
