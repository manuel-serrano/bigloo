;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-bdb.el                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov 15 06:58:27 1998                          */
;*    Last change :  Tue Jan 28 11:02:05 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connexion to bdb (via dbg).                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-bdb)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-config)
(require 'bee-autoload)
(require 'bee-keymap)
(require 'dbg-autoload)
(require 'dbg-config)
(require 'ude-autoload)
(require 'ude-config)
(require 'ude-balloon)
(require 'cee-gdb)
 
;*---------------------------------------------------------------------*/
;*    Bdb configuration                                                */
;*---------------------------------------------------------------------*/
(defcustom bee-dbg "bdb"
  "*The name of the Bigloo symbolic debugger."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-binary value)))

(defcustom bee-dbg-info-file "bdb.info"
  "*The name of the info file for the Bigloo symbolic debugger."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-info-file value)))

(defcustom bee-dbg-emacs-option "--emacs"
  "*The option to be sent to the debugger when running from emacs."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-emacs-option value)))

(defcustom bee-dbg-prompt-regexp "(bdb\\(:[^)]+\\)?) "
  "*The prompt of Bdb."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-prompt-regexp value)
	 (setq dbg-prompt-eol-regexp (concat dbg-prompt-regexp "$"))))

(defcustom bee-dbg-marker-regexp
  (concat "\032\032\\([^" path-separator "\n]*\\)" path-separator
	  "\\([0-9]*\\)" path-separator ".*\n")
  "*The Bdb source line regexp."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-marker-regexp value)))

;; bdb command
(defcustom bee-dbg-run-command "run"
  "*The run command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-run-command value)
	 (setq dbg-breakpoint-hook-regexp (dbg-make-breakpoint-hook-regexp))
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-step-command "step"
  "*The step command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-step-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-cont-command "cont"
  "*The cont command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-cont-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-call-command "call"
  "*The call command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-call-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-return-command "return"
  "*The return command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-return-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-next-command "next"
  "*The next command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-next-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-until-command "until"
  "*The until command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-until-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-finish-command "finish"
  "*The finish command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-finish-command value)
	 (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))))

(defcustom bee-dbg-break-command "break"
  "*The break command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-break-command value)))

(defcustom bee-dbg-info-break-command "info breakpoint"
  "*The info break command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-info-break-command value)))

(defcustom bee-dbg-info-assert-command "info assert"
  "*The info assert command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-info-assert-command value)))

(defcustom bee-dbg-enable-break-command "enable"
  "*The enable break command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-enable-break-command value)
	 (setq dbg-breakpoint-hook-regexp
	       (dbg-make-breakpoint-hook-regexp))))

(defcustom bee-dbg-disable-break-command "disable"
  "*The disable break command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-disable-break-command value)
	 (setq dbg-breakpoint-hook-regexp
	       (dbg-make-breakpoint-hook-regexp))))

(defcustom bee-dbg-delete-break-command "delete"
  "*The delete break command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-delete-break-command value)
	 (setq dbg-breakpoint-hook-regexp
	       (dbg-make-breakpoint-hook-regexp))))

(defcustom bee-dbg-tbreak-command "tbreak"
  "*The tbreak command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-tbreak-command value)
	 (setq dbg-breakpoint-hook-regexp
	       (dbg-make-breakpoint-hook-regexp))))

(defcustom bee-dbg-cond-command "cond"
  "*The cond command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-cond-command value)
	 (setq dbg-breakpoint-hook-regexp
	       (dbg-make-breakpoint-hook-regexp))))

(defcustom bee-dbg-complete-command "complete"
  "*The complete command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-complete-command value)))

(defcustom bee-dbg-info-args-command "info args"
  "*The info args command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-info-args-command value)))

(defcustom bee-dbg-info-locals-command "info locals"
  "*The info locals command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-info-locals-command value)))

(defcustom bee-dbg-display-command "display"
  "*The info locals command."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-display-command value)))

;; bdb regular expression
(defcustom bee-dbg-breakpoint-regexp
"\\([0-9]+\\)\\s-*\\S-*\\s-*\\S-*\\s-*\\(.\\)\\s-*0x[0-9a-fA-F]+\\s-+in\\s-+[a-zA-Z0-9_?><:!@$%^&*+=\\|~-]+\\s-+at\\s-+\\([^:]+\\)[:]\\([0-9]+\\)"
  "The regexp that matches a bdb breakpoint as printed by the
`info breakpoint' command. This regular expression matches the
first line of the breakpoint. It does not match the line
describing the condition nor the hit rate of the breakpoint."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-breakpoint-regexp value)))

(defcustom bee-dbg-breakpoint-condition-regexp
  "^\\s-+stop only if [a-zA-Z0-9_]+[ ]*(\"\\([^\"]+\\)\""
  "*The regular expression that match a Bdb breakpoint condition as
reported by `info breakpoint'."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-breakpoint-condition-regexp value)))

(defcustom bee-dbg-breakpoint-assert-regexp
  "^\\s-+stop only if bdb_assert[ ]*(\"\\([^\"]+\\)\""
  "Nil or the regexp that matches a bdb breakpoint as printed by the
`info breakpoint' command. This matches assert breakpoints."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-breakpoint-assert-regexp value)))

(defcustom bee-dbg-dbg-io-start-regexp
  "^gdb-io-start[\n]"
  "Nil or the regexp that matches the beginning of Bdb output."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-dbg-io-start-regexp value)))

(defcustom bee-dbg-dbg-io-stop-regexp
  "^gdb-io-stop[\n]"
  "Nil or the regexp that matches the beginning of Bdb output."
  :group 'bee
  :type 'string
  :set (lambda (symbol value)
	 (set-default symbol value)
	 (setq dbg-dbg-io-stop-regexp value)))

;*---------------------------------------------------------------------*/
;*    bdb-eval-region ...                                              */
;*---------------------------------------------------------------------*/
(defun bdb-eval-region (region)
  (if (and (> (length region) 0) (not (eq (aref region 0) ?\()))
      (dbg-remote-call (format "(begin %s)" region))
    (dbg-remote-call region)))

;*---------------------------------------------------------------------*/
;*    bdb-bee-source-menu-entries ...                                  */
;*---------------------------------------------------------------------*/
(defun bdb-bee-source-menu-entries (event)
  (let* ((region (buffer-substring (region-beginning) (region-end)))
	 (buffer (event-buffer event))
	 (file   (file-name-nondirectory (buffer-file-name buffer)))
	 (line   (count-lines 1 (+ (region-beginning) 1))))
    (list (vector "(bdb) eval"
		  (list 'bdb-eval-region region)
		  t)
	  (vector "(bdb) print"
		  (list 'dbg-command-region "print " region)
		  t)
	  "-"
	  (vector "(bdb) whatis"
		  (list 'dbg-command-region "whatis " region)
		  t)
	  "-"
	  (vector "(bdb) break"
		  (list 'bdb-remote-set-breakpoint
			(concat bee-dbg-break-command " ")
			file
			line)
		  t)
	  (vector "(bdb) temporary break"
		  (list 'bdb-remote-set-breakpoint
			(concat bee-dbg-tbreak-command " ")
			file
			line)
		  t)
	  "--:shadowDoubleEtchedOut")))

;*---------------------------------------------------------------------*/
;*    bdb-bee-balloon-start ...                                        */
;*---------------------------------------------------------------------*/
(defun bdb-bee-balloon-start ()
  ;; we start balloon
  (ude-balloon-start)
  ;; the action on breakpoint (margin balloon)
  '(ude-add-balloon-action
   'bee-bdb
   #'(lambda (x win)
       (and (windowp win)
	    (numberp x)
	    (> (window-left-margin-pixel-width win) x)))
   #'(lambda ()
      (let* ((buffer (ude-balloon-get-buffer))
	     (pos    (ude-balloon-get-point)))
	(if (and (bufferp buffer) (numberp pos))
	    (let* ((ex  (find-text-property pos 'dbg-breakpoint buffer))
		   (msg (dbg-breakpoint-balloon-message ex)))
	      (if (stringp msg)
		  (display-message 'no-log msg))))))))

;*---------------------------------------------------------------------*/
;*    bdb-bee-balloon-stop ...                                         */
;*---------------------------------------------------------------------*/
(defun bdb-bee-balloon-stop ()
  (ude-remove-balloon-action 'bee-bdb))

;*---------------------------------------------------------------------*/
;*    bee-connect-buffer-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-connect-buffer-hook ()
  ;; we set dbg mouse bindings
  '(ude-add-menu #'(lambda (event)
		   (and (> (window-left-margin-pixel-width
			    (event-window event))
			   (event-x-pixel event))))
		'dbg-breakpoint-menu)
  ;; we actived Scheme source balloon help
  (bdb-bee-balloon-start)
  ;; the s-expression menu when clicking on the active region
  (bee-add-region-popup-entry 'bdb-bee-source-menu-entries)
  ;; we have to refresh the breakpoints
  (dbg-breakpoint-command))

;*---------------------------------------------------------------------*/
;*    bee-disconnect-buffer-hook ...                                   */
;*---------------------------------------------------------------------*/
(defun bee-disconnect-buffer-hook ()
  ;; we first remove the region popup
  (bee-remove-region-popup-entry 'bdb-bee-source-menu-entries)
  ;; then, we remove the balloon help
  (bdb-bee-balloon-stop))
  
;*---------------------------------------------------------------------*/
;*    bee-dbg-stack ...                                                */
;*    -------------------------------------------------------------    */
;*    Which stack to display.                                          */
;*---------------------------------------------------------------------*/
(defvar bee-dbg-stack 'SCHEME
  "The Symbol SCHEME or C.")

;*---------------------------------------------------------------------*/
;*    bee-dbg-stack-mode-menu ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-dbg-stack-mode-menu ()
  (list "dbg stack"
	(vector "Close stack frame display"
		'(dbg-stack-quit)
		t)))

;*---------------------------------------------------------------------*/
;*    bee-bdb-init ...                                                 */
;*---------------------------------------------------------------------*/
(defun bee-bdb-init ()
  ;; binary name 
  (setq dbg-binary bee-dbg)
  ;; debugger online doc
  (setq dbg-info-file bee-dbg-info-file)
  ;; emacs option
  (setq dbg-emacs-option bee-dbg-emacs-option)
  ;; prompt setting
  (setq dbg-prompt-regexp bee-dbg-prompt-regexp)
  ;; dbg command
  (setq dbg-run-command bee-dbg-run-command)
  (setq dbg-step-command bee-dbg-step-command)
  (setq dbg-cont-command bee-dbg-cont-command)
  (setq dbg-call-command bee-dbg-call-command)
  (setq dbg-return-command bee-dbg-call-command)
  (setq dbg-next-command bee-dbg-next-command)
  (setq dbg-until-command bee-dbg-until-command)
  (setq dbg-finish-command bee-dbg-finish-command)
  (setq dbg-break-command bee-dbg-break-command)
  (setq dbg-info-break-command bee-dbg-info-break-command)
  (setq dbg-tbreak-command bee-dbg-tbreak-command)
  (setq dbg-cond-command bee-dbg-cond-command)
  (setq dbg-info-assert-command bee-dbg-info-assert-command)
  (setq dbg-complete-command bee-dbg-complete-command)
  (setq dbg-info-args-command bee-dbg-info-args-command)
  (setq dbg-info-locals-command bee-dbg-info-locals-command)
  (setq dbg-display-command bee-dbg-display-command)
  ;; tags search
  (setq dbg-mode-tags-find 'bee-tags-find)
  (setq dbg-mode-usage-find 'bee-usage-find)
  ;; dbg source line
  (setq dbg-marker-regexp bee-dbg-marker-regexp)
  ;; bdb output parsing
  (setq dbg-breakpoint-regexp bee-dbg-breakpoint-regexp)
  (setq dbg-breakpoint-condition-regexp bee-dbg-breakpoint-condition-regexp)
  (setq dbg-breakpoint-assert-regexp bee-dbg-breakpoint-assert-regexp)
  (setq dbg-dbg-io-start-regexp bee-dbg-dbg-io-start-regexp)
  (setq dbg-dbg-io-stop-regexp bee-dbg-dbg-io-stop-regexp)
  ;; we prepare the dbg hooking
  (setq dbg-breakpoint-hook-regexp (dbg-make-breakpoint-hook-regexp))
  (setq dbg-display-hook-regexp (dbg-make-display-hook-regexp))
  ;; stack printing
  (setq dbg-stack-mode-menu 'bee-dbg-stack-mode-menu)
  (setq dbg-info-stack-hook-command "info stack")
  ;; bee bdb connexion
  (dbg-add-connect-hook 'bee-mode 'bee-connect-buffer-hook)
  (dbg-add-disconnect-hook 'bee-mode 'bee-disconnect-buffer-hook)
  (dbg-add-connect-hook 'c-mode 'cee-connect-buffer-hook)
  (dbg-add-disconnect-hook 'c-mode 'cee-disconnect-buffer-hook))

;*---------------------------------------------------------------------*/
;*    bdb-other-frame ...                                              */
;*---------------------------------------------------------------------*/
(defun bdb-other-frame ()
  (interactive)
  (bee-bdb-init)
  (let ((default-directory ude-root-directory))
    (dbg-other-frame)))


