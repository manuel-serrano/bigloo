;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-config.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 30 13:30:57 1998                          */
;*    Last change :  Wed Jan 30 09:05:13 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The user configuration file.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-config)
(require 'dbg-autoload)
(require 'ude-custom)

;*---------------------------------------------------------------------*/
;*    dbg-version ...                                                  */
;*---------------------------------------------------------------------*/
(defconst dbg-version "0.0a"
  "*The Dbg version.")

;*---------------------------------------------------------------------*/
;*    dbg-significant-prefix ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-significant-prefix (cmd)
  (let* ((max-char 3)
	 (len (length cmd))
	 (i   1)
	 (max (if (> len max-char) max-char len))
	 (res (regexp-quote (if (< len 3) cmd (substring cmd 0 3)))))
    (while (< i max)
      (setq res (concat (regexp-quote (substring cmd 0 i)) " \\|" res))
      (setq i (+ 1 i)))
    (concat "\\(?:" res "\\)")))

;*---------------------------------------------------------------------*/
;*    Configuration variables (mode specific)                          */
;*---------------------------------------------------------------------*/
;; the binary files
(defvar dbg-binary "gdb"
  "The name of the binary file that is the debugger (e.g. bdb, gdb, dbx).")

;; the name of the info file for the debugger
(defvar dbg-info-file "gdb.info"
  "The name for the info file associated to the debugger.")
  
;; the debugger emacs option
(defvar dbg-emacs-option "--fullname"
  "The option to be sent to the debugger when running with emacs.")
  
;; prompt
(defvar dbg-prompt-regexp "(gdb) "
  "The dbg prompt.")

(defvar dbg-prompt-eol-regexp (concat dbg-prompt-regexp "$")
  "The regexp matching dbg prompt on a line.
That variable should be set by the means of :set property of customization
of the DBG-PROMPT-REGEXP.")

;; dbg marker regexp
(defvar dbg-marker-regexp
  (concat "\032\032\\([^" path-separator "\n]*\\)" path-separator
	  "\\([0-9]*\\)" path-separator ".*\n")
  "*A string that is a regexp that matches a debugger line printing.")

;; dbg debug io start and stop
(defvar dbg-dbg-io-start-regexp nil
  "Nil or the regexp that matches the beginning of the debugger io.")

(defvar dbg-dbg-io-stop-regexp nil
  "Nil or the regexp that matches the beginning of the debugger io.")

;; dbg command
(defvar dbg-run-command "run"
  "The run command.")

(defvar dbg-step-command "step"
  "The step command.")

(defvar dbg-cont-command "cont"
  "The cont command.")

(defvar dbg-call-command "call"
  "The call command.")

(defvar dbg-return-command "return"
  "The return command.")

(defvar dbg-next-command "next"
  "The next command.")

(defvar dbg-until-command "until"
  "The until command.")

(defvar dbg-finish-command "finish"
  "The finish command.")

(defvar dbg-break-command "break"
  "The break command.")

(defvar dbg-footprint-command "footprint"
  "The footprint command.")

(defvar dbg-info-break-command "info breakpoint"
  "The info break command.")

(defvar dbg-enable-break-command "enable"
  "The info break command.")

(defvar dbg-disable-break-command "disable"
  "The info break command.")

(defvar dbg-delete-break-command "delete"
  "The info break command.")

(defvar dbg-tbreak-command "tbreak"
  "The tbreak command.")

(defvar dbg-cond-command "cond"
  "The cond command.")

(defvar dbg-assert-command nil
  "The assert command.")

(defvar dbg-info-assert-command nil
  "The info assert command.")

(defvar dbg-unassert-command nil
  "The unassert command.")

(defvar dbg-complete-command "complete"
  "The complete command.")

(defvar dbg-info-args-command "info args"
  "The info argument command.")
  
(defvar dbg-info-locals-command "info locals"
  "The info argument command.")
  
(defvar dbg-display-command "display"
  "The display argument command.")
  
;; dbg regular expressions
(defvar dbg-breakpoint-regexp
  "\\([0-9]+\\)\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)\\s-*0x[0-9a-fA-F]+\\s-+in\\s-+[a-zA-Z0-9_]+\\s-+at\\s-+\\([^:]+\\)[:]\\([0-9]+\\)"
  "The regexp that matches a bdb breakpoint as printed by the
`info breakpoint' command. This regular expression matches the
first line of the breakpoint. It does not match the line
describing the condition nor the hit rate of the breakpoint.")
  
(defvar dbg-breakpoint-condition-regexp
  "^\\s-+stop only if [a-zA-Z0-9_]+[ ]*(\"\\([^\"]+\\)\""
  "The regular expression that match a breakpoint condition as
reported by `info breakpoint'.")

(defvar dbg-breakpoint-assert-regexp
  nil
  "Nil or the regexp that matches a bdb breakpoint as printed by the
`info breakpoint' command. This matches assert breakpoints.")

;*---------------------------------------------------------------------*/
;*    dbg-make-breakpoint-hook-regexp ...                              */
;*---------------------------------------------------------------------*/
(defun dbg-make-breakpoint-hook-regexp ()
  (concat "^\\(?:"
	  (dbg-significant-prefix dbg-delete-break-command)
	  "\\|"
	  (dbg-significant-prefix dbg-disable-break-command)
	  "\\|"
	  (dbg-significant-prefix dbg-enable-break-command)
	  "\\|"
	  (dbg-significant-prefix dbg-cond-command)
	  "\\|"
	  (dbg-significant-prefix dbg-break-command)
	  "\\|"
	  (dbg-significant-prefix dbg-tbreak-command)
	  (if (stringp dbg-assert-command)
	      (concat "\\|" (dbg-significant-prefix dbg-assert-command))
	    "")
	  (if (stringp dbg-unassert-command)
	      (concat "\\|" (dbg-significant-prefix dbg-unassert-command))
	    "")
	  "\\|"
	  (dbg-significant-prefix dbg-run-command)
	  ;; break and tbreak are always added to the regexp
	  "\\|bre\\|tbre\\|run"
	  "\\)"))

(defvar dbg-breakpoint-hook-regexp
  (dbg-make-breakpoint-hook-regexp)
  "Nil of the regexp that matches all the dbg command.
That value is used by the function DBG-BREAKPOINT-HOOK to check if
breakpoints must refreshed.")

;*---------------------------------------------------------------------*/
;*    dbg-make-display-hook-regexp ...                                 */
;*---------------------------------------------------------------------*/
(defun dbg-make-display-hook-regexp ()
  (concat "^\\(?:"
	  (dbg-significant-prefix dbg-next-command)
	  "\\|"
	  (dbg-significant-prefix dbg-step-command)
	  "\\|"
	  (dbg-significant-prefix dbg-cont-command)
	  "\\|"
	  (dbg-significant-prefix dbg-call-command)
	  "\\|"
	  (dbg-significant-prefix dbg-return-command)
	  "\\|"
	  (dbg-significant-prefix dbg-until-command)
	  "\\|"
	  (dbg-significant-prefix dbg-finish-command)
	  "\\|"
	  (dbg-significant-prefix dbg-run-command)
	  ;; break and tbreak are always added to the regexp
	  "\\|n\\|s\\|c\\|ru\\|re\\|unt\\|fin\\|fra"
	  "\\)"))

(defvar dbg-display-hook-regexp
  (dbg-make-display-hook-regexp)
  "Nil of the regexp that matches all the dbg command.
That value is used by the function DBG-DISPLAY-HOOK to check if
displays must refreshed.")

;; tags finders
(defvar dbg-mode-tags-find (lambda () nil)
  "A procedure that implement a mode specific tag search.")

(defvar dbg-mode-usage-find (lambda () nil)
  "A procedure that implement a mode specific usage search.")

;; stack window
(defvar dbg-stack-mode-menu nil
  "A procedure that build the stack menus or NIL.")

(defvar dbg-info-stack-hook-command "info stack"
  "A procedure that processes returns the command for the stack hook.")

;*---------------------------------------------------------------------*/
;*    Custom                                                           */
;*---------------------------------------------------------------------*/
;; dbg group
(defgroup dbg nil
  "Debuggin."
  :tag "Dbg"
  :prefix "dbg-"
  :group 'processes)

;; dbg hooking timeout
(defcustom dbg-wait-timeout 500
  "A number of loop iteration before timeout."
  :group 'dbg
  :type 'number)

;; window configuration (colors, margin size, ...)
(defcustom dbg-margin-color "bisque"
  "The color for the left margin."
  :group 'dbg
  :type 'string)

;; margin width
(defcustom dbg-margin-width 3
  "The widh of the left dbg margin."
  :group 'dbg
  :type 'number)

;; display line heigth
(defcustom dbg-stack-depth 10
  "The number of stack frame to be displayed (or nil for all stack)."
  :group 'dbg
  :type 'number)

(defcustom dbg-display-height 10
  "The display frame height."
  :group 'dbg
  :type 'number)

(defcustom dbg-args-height 10
  "The args display frame height."
  :group 'dbg
  :type 'number)

(defcustom dbg-locals-height 10
  "The locals frame height."
  :group 'dbg
  :type 'number)

;; raise frame displaying active source line
(defcustom dbg-raise-active-source-frame-p t
  "Raise the frame displaying active source line."
  :group 'dbg
  :type 'boolean)

;; key bindings
(defcustom dbg-mouse-binding ude-mouse-binding
  "*The mouse binding for Dbg mouse event."
  :group 'dbg
  :type 'vector)

;; balloon configuration
(defcustom dbg-balloon-timeout 1000
  "Display balloon after this many milliseconds of mouse inactivity."
  :group 'dbg
  :type 'number)

(defcustom dbg-balloon-width 80
  "The maximum number of chars displayed in a balloon window."
  :group 'dbg
  :type 'number)

;; dbg config
(defcustom dbg-wait-output-timeout 2
  "The number of seconds before timeout when waiting for dbg output."
  :group 'dbg
  :type 'number)

(defcustom dbg-verbose-remote t
  "*Non-nil means that remote call are printed and registered in the history."
  :group 'dbg
  :type 'boolean)

(defcustom dbg-balloon-enabled-modes '(scheme-mode c-mode)
  "The list of emacs mode enabled has source file mode."
  :group 'dbg
  :type '(repeat (symbol)))

(defcustom dbg-window-height 15
  "The number of lines of the dbg console window."
  :group 'dbg
  :type 'number)

(defcustom dbg-display-source-in-frame-p t
  "*Non-nil means that source file are displayed in a separate frame."
  :group 'dbg
  :type 'boolean)

;; dbg show
(defcustom dbg-show '(local args stack)
  "*A list of elements that may be LOCAL, ARGS, DISPLAY or STACK."
  :group 'dbg
  :type '(repeat (symbol)))

;; dbg faces
(defface dbg-output-color 
  (list (list '((class color))
	      (list ':foreground "blue4"
		    ':bold nil))
	'(t (:bold t)))
  "Dbg output face"
  :group 'dbg)

(defface dbg-prompt-face
  (list (list '((class color))
	      (list ':foreground "slateblue3"
		    ':bold t))
	'(t (:bold t)))
  "Dbg prompt face"
  :group 'dbg)
  
(defface dbg-source-io-face
  (list (list '((class color))
	      (list ':bold nil)))
  "Dbg debugger output face."
  :group 'dbg)

(defface dbg-dbg-io-face
  (list (list '((class color))
	      (list ':foreground "orange"
		    ':bold t))
	'(t (:bold t)))
  "Dbg debugger output face."
  :group 'dbg)

(defcustom dbg-hilit-io-p t
  "*Non-nil means to hilit non dbg output."
  :group 'dbg
  :type 'boolean)

;*---------------------------------------------------------------------*/
;*    dbg-customize ...                                                */
;*    -------------------------------------------------------------    */
;*    This function invoked DBG customization in a new frame.          */
;*---------------------------------------------------------------------*/
(defun dbg-customize ()
  "Invokes Dbg customization in a new frame."
  (interactive)
  (let ((pop-up-frames t))
    (customize-group-other-window 'dbg)))

;*---------------------------------------------------------------------*/
;*    Faces                                                            */
;*---------------------------------------------------------------------*/
(defcustom dbg-scheme-frame-face 'ude-font-lock-face-2
  "The face to display Scheme stack frames."
  :group 'dbg
  :type 'symbol)

(defcustom dbg-c-frame-face 'ude-font-lock-face-1
  "The face to display C stack frames."
  :group 'dbg
  :type 'symbol)


