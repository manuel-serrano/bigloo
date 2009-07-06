;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg.el                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 19 15:16:46 1998                          */
;*    Last change :  Sat Mar  2 12:07:37 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The main DBG function.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'comint)
(require 'dbg-config)
(require 'dbg-autoload)
(require 'dbg-process)
(require 'dbg-filter)
(require 'dbg-complete)
(require 'dbg-mode)
(require 'dbg-breakpoint)
(require 'ude-autoload)
(require 'ude-custom)

;*---------------------------------------------------------------------*/
;*    Comint global control.                                           */
;*---------------------------------------------------------------------*/
(defvar dbg-comint-buffer nil)

;*---------------------------------------------------------------------*/
;*    dbg-history ...                                                  */
;*---------------------------------------------------------------------*/
(defvar dbg-history '())

;*---------------------------------------------------------------------*/
;*    dbg-started-p ...                                                */
;*---------------------------------------------------------------------*/
(defvar dbg-started-p nil)

;*---------------------------------------------------------------------*/
;*    dbg-wrapper-caller                                               */
;*    -------------------------------------------------------------    */
;*    A possible function that is in charge to call the debugger.      */
;*---------------------------------------------------------------------*/
(defvar dbg-wrapper-caller nil)

;*---------------------------------------------------------------------*/
;*    dbg ...                                                          */
;*---------------------------------------------------------------------*/
(defun dbg (command-line)
  "Run a debugger dbg on program FILE in buffer *dbg-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (let ((make-entry (ude-fetch-makefile-binary-entry)))
	   (if (stringp make-entry)
	       (concat dbg-binary " " make-entry)
	     (read-from-minibuffer (concat "Run " dbg-binary "(like this): ")
				   (if (consp dbg-history)
				       (car dbg-history)
				     (concat dbg-binary " "))
				   dbg-minibuffer-local-map nil
				   '(dbg-history . 1))))))
  (setq dbg-history (cons command-line dbg-history))
  ;; we split the command line in list of words
  (let ((word-list (dbg-string->list command-line)))
    (if (= (length word-list) 2)
	(let* ((binary  (car word-list))
	       (a.out   (cadr word-list))
	       (dbgname (concat dbg-binary "-" a.out))
	       (bufname (concat "*" dbgname "*")))
	  (setq dbg-binary binary)
	  ;; we create a *dbg* buffer
	  (switch-to-buffer-other-frame bufname)
	  ;; we set this buffer as the comint buffer
	  (setq dbg-comint-buffer (current-buffer))
	  ;; we start comint
	  (setq dbg-marker-filter 'dbg-default-marker-filter)
	  (make-comint dbgname binary nil dbg-emacs-option a.out)
	  ;; the process filter and sentinel
	  (process-kill-without-query (get-buffer-process dbg-comint-buffer))
	  (set-process-filter (get-buffer-process dbg-comint-buffer)
			      'dbg-filter)
	  (set-process-sentinel (get-buffer-process dbg-comint-buffer)
				'dbg-sentinel)
	  ;; we prepare breakpoint tracing
	  (dbg-add-send-input-hook 'dbg-breakpoint-hook)
	  ;; we start the dbg-mode
	  (dbg-mode)
	  ;; dbg hooking
	  (dbg-wait-for-comint "Debugger spawning" nil)
	  (run-hooks 'dbg-spawn-hook)
	  ;; we are now ready
	  (setq dbg-started-p t))
      (ude-error "Illegal %s invokation %S" dbg-binary command-line))))

;*---------------------------------------------------------------------*/
;*    dbg-other-frame ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-other-frame ()
  (interactive)
  (message "Spawning %s..." dbg-binary)
  (if (bufferp dbg-comint-buffer)
      (let ((pop-up-frames t))
	(display-buffer dbg-comint-buffer))
    (call-interactively 'dbg)))

;*---------------------------------------------------------------------*/
;*    dbg-string->list ...                                             */
;*    -------------------------------------------------------------    */
;*    This function split a string into a list of string. Space or     */
;*    tab is used as a word delimiter.                                 */
;*---------------------------------------------------------------------*/
(defun dbg-string->list (string)
  (let ((i 0) (beg 0)
	(len (length string))
	(words nil))
    (while (< i len)
      (if (memq (aref string i) '(?\t ? ))
	  (progn
	    (setq words (cons (substring string beg i) words))
	    (setq beg (1+ i))
	    (while (and (< beg len) (memq (aref string beg) '(?\t ? )))
	      (setq beg (1+ beg)))
	    (setq i (1+ beg)))
	(setq i (1+ i))))
    (if (< beg len)
	(setq words (cons (substring string beg) words)))
    (nreverse words)))

;*---------------------------------------------------------------------*/
;*    dbg-send-input-hooks ...                                         */
;*---------------------------------------------------------------------*/
(defvar dbg-send-input-hooks '())

;*---------------------------------------------------------------------*/
;*    dbg-add-send-input-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun dbg-add-send-input-hook (hook)
  (setq dbg-send-input-hooks (cons hook dbg-send-input-hooks)))

;*---------------------------------------------------------------------*/
;*    dbg-remove-send-input-hook ...                                   */
;*---------------------------------------------------------------------*/
(defun dbg-remove-send-input-hook (hook)
  (setq dbg-send-input-hooks (delq hook dbg-send-input-hooks)))

;*---------------------------------------------------------------------*/
;*    dbg-installed-hook-p ...                                         */
;*    -------------------------------------------------------------    */
;*    Is a hook all set?                                               */
;*---------------------------------------------------------------------*/
(defun dbg-installed-hook-p (hook)
  (memq hook dbg-send-input-hooks))

;*---------------------------------------------------------------------*/
;*    dbg-remote-call ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-remote-call (command)
  (if (functionp dbg-wrapper-caller)
      (funcall dbg-wrapper-caller command)
    (if dbg-verbose-remote
	(dbg-verbose-remote-call command)
      (dbg-silent-remote-call command))))

;*---------------------------------------------------------------------*/
;*    dbg-verbose-remote-call ...                                      */
;*    -------------------------------------------------------------    */
;*    This function is used for sending a dbg call from a remote       */
;*    buffer.                                                          */
;*---------------------------------------------------------------------*/
(defun dbg-verbose-remote-call (command)
  (if (not (bufferp dbg-comint-buffer))
      (ude-error "No debugger running...")
    (progn
      (set-buffer dbg-comint-buffer)
      (let ((proc (get-buffer-process dbg-comint-buffer)))
	(goto-char (process-mark proc))
	(delete-region (point) (point-max))
	(insert command)
	(dbg-send-input)
	(goto-char (point-max))))))

;*---------------------------------------------------------------------*/
;*    dbg-silent-remote-call ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-silent-remote-call (command)
  (message "Command: %s" command)
  (sit-for 0)
  (dbg-basic-call command))

;*---------------------------------------------------------------------*/
;*    dbg-basic-call ...                                               */
;*    -------------------------------------------------------------    */
;*    Sending a dbg command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-basic-call (command)
  (interactive)
  (dbg-console-log "dbg-basic-call" command 'font-lock-function-name-face)
  (let ((command (concat command "\n"))
	(proc (get-buffer-process dbg-comint-buffer)))
    (or proc (ude-error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer dbg-comint-buffer)
      (goto-char (process-mark proc))
      (beginning-of-line)
      (if (looking-at comint-prompt-regexp)
	  (set-marker dbg-delete-prompt-marker (point))))
    (process-send-string proc command)))

;*---------------------------------------------------------------------*/
;*    dbg-send-input ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-send-input ()
  (interactive)
  (let* ((proc  (get-buffer-process dbg-comint-buffer))
	 (pmark (process-mark proc))
	 (intxt (if (>= (point) (marker-position pmark))
		    (progn (if comint-eol-on-send (end-of-line))
			   (buffer-substring pmark (point)))
		  nil)))
    (if (stringp intxt)
	(dbg-console-log "dbg-send-input" intxt 'font-lock-type-face)
      (dbg-console-log "dbg-send-input" 'nil 'font-lock-string-face))
    (comint-send-input)
    (mapcar (lambda (hook) (funcall hook intxt)) dbg-send-input-hooks)
    (redisplay-frame)))

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-no-output-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-no-output-string)

;; this variable holds a state: are we currently inquirying for no-outputs
(defvar dbg-no-output-command-armed-p nil)

;*---------------------------------------------------------------------*/
;*    dbg-no-output-call ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-no-output-call (string)
  "Perform completion on the DBG command preceding point.
This is implemented using the DBG `no-output' command." 
  (interactive)
  (dbg-console-log "dbg-no-output-call" string 'font-lock-doc-string-face)
  (if (not dbg-no-output-command-armed-p)
      (progn
	;; we wait for comint to be ready
	(dbg-wait-for-comint "dbg-no-output-call" nil)
	;; we mark that no-output is armed not to re-enter this hook
	(setq dbg-no-output-command-armed-p t)
	;; Temporarily install our filter function.
	(let ((dbg-marker-filter 'dbg-no-output-filter))
	  ;; Issue the command to DBG.
	  (dbg-info-no-output-call string)
	  (setq dbg-no-output-in-progress t)
	  (setq dbg-no-output-string "")
	  ;; Slurp the output.
	  (while dbg-no-output-in-progress
	    (accept-process-output (get-buffer-process dbg-comint-buffer))))
	;; at this point dbg-no-output-string is the string of all
	;; the no-output, we have to parse in order to find which
	;; no-output are still actives.
	(setq dbg-no-output-command-armed-p nil)
	dbg-no-output-string)
    ""))

;*---------------------------------------------------------------------*/
;*    dbg-info-no-output-call ...                                      */
;*    -------------------------------------------------------------    */
;*    Sending a dbg command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-no-output-call (string)
  (interactive)
  (let ((command (concat string "\n"))
	(proc (get-buffer-process dbg-comint-buffer)))
    (or proc (ude-error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer dbg-comint-buffer)
      (goto-char (process-mark proc))
      (while (not (looking-at comint-prompt-regexp))
	(goto-char (process-mark proc))
	(beginning-of-line))
      (process-send-string proc command))))

;*---------------------------------------------------------------------*/
;*    dbg-no-output-filter ...                                         */
;*    -------------------------------------------------------------    */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of DBG up to the next prompt and build the completion */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(defun dbg-no-output-filter (string)
  (setq dbg-marker-acc "")
  (setq dbg-no-output-string (concat dbg-no-output-string string))
  (if (string-match comint-prompt-regexp string)
      (setq dbg-no-output-in-progress nil))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-find-file ...                                                */
;*    -------------------------------------------------------------    */
;*    Find a file for dbg. Here we can add special keymap if we        */
;*    whish to.                                                        */
;*---------------------------------------------------------------------*/
(defun dbg-find-file (f)
  (interactive "Ffile: ")
  (if (file-exists-p f)
      (save-excursion
	(let ((buffer (find-file-noselect f)))
	  (if (bufferp buffer)
	      (dbg-connect-buffer buffer))
	  buffer))
    (let ((lst   (buffer-list))
	  (match (concat f "$"))
	  (buf   '()))
      (while (consp lst)
	(let* ((b     (car lst))
	       (bname (buffer-file-name b)))
	  (if (and (stringp bname) (string-match match bname))
	      ;; we have found the buffer
	      (progn
		(dbg-connect-buffer buf)
		(setq buf b)
		(setq lst '()))
	    (setq lst (cdr lst)))))
      buf)))

;*---------------------------------------------------------------------*/
;*    dbg-refresh ...                                                  */
;*---------------------------------------------------------------------*/
(defun dbg-refresh (&optional following)
  (interactive (list current-prefix-arg))
  (let ((dbg-mode nil))
    (setq dbg-mode dbg-mode)
    (recenter following)
    (mapcar (lambda (hook) (funcall hook nil)) dbg-send-input-hooks)
    (redisplay-frame)))

;*---------------------------------------------------------------------*/
;*    dbg-comint-started-p ...                                         */
;*---------------------------------------------------------------------*/
(defun dbg-comint-started-p ()
  (and (bufferp dbg-comint-buffer)
       dbg-started-p))
       
;*---------------------------------------------------------------------*/
;*    dbg-comint-ready-p ...                                           */
;*    -------------------------------------------------------------    */
;*    This predicate returns T if and only if comint is ready for      */
;*    a new command.                                                   */
;*---------------------------------------------------------------------*/
(defun dbg-comint-ready-p ()
  (if dbg-waiting
      nil
    (save-excursion
      (save-restriction
	(widen)
	(progn
	  (set-buffer dbg-comint-buffer)
	  (goto-char (point-max))
	  (let ((bound (point)))
	    (beginning-of-line)
	    (re-search-forward (concat comint-prompt-regexp "[ \t\n]*")
			       bound t)))))))

;*---------------------------------------------------------------------*/
;*    dbg-waiting ...                                                  */
;*---------------------------------------------------------------------*/
(defvar dbg-waiting nil)

;*---------------------------------------------------------------------*/
;*    dbg-wait-for-comint ...                                          */
;*    -------------------------------------------------------------    */
;*    We sit for the prompt is printed. This function is called        */
;*    by the send hooks.                                               */
;*---------------------------------------------------------------------*/
(defun dbg-wait-for-comint (from timeout)
  (if (not dbg-comint-buffer)
      (ude-error "No debugger running")
    (save-excursion
      (save-restriction
	(widen)
	(let ((count 0)
	      (str   "/-\\|"))
	  (setq str str)
	  (set-buffer dbg-comint-buffer)
	  ;; we mark dbg waiting
	  (setq dbg-waiting t)
	  (accept-process-output)
	  (while (progn
		   (goto-char (point-max))
		   (let ((bound (point)))
		     (beginning-of-line)
		     (and (not (re-search-forward (concat
						   comint-prompt-regexp
						   "[ \t\n]*")
						  bound t))
			  (or (not (numberp timeout))
			      (< count timeout)))))
	    (setq count (+ 1 count))
	    '(display-message 'no-log
	      (format "waiting for `%s' [%c] (timeout %d/%S)"
		      from
		      (aref str (% count 4))
		      count
		      timeout))
	    (sit-for 0.1)
	    (redisplay-frame (selected-frame) t)
	    (set-buffer dbg-comint-buffer))
	  ;; we remove the last wait message
	  (display-message 'no-log " ")
	  ;; dbg is now ready
	  (setq dbg-waiting nil))))))

;*---------------------------------------------------------------------*/
;*    dbg-clear-window ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-clear-window ()
  (interactive)
  (set-buffer dbg-comint-buffer)
  (let ((proc (get-buffer-process dbg-comint-buffer)))
    (goto-char (process-mark proc))
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))))

;*---------------------------------------------------------------------*/
;*    dbg-clear-line ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-clear-line ()
  (interactive)
  (set-buffer dbg-comint-buffer)
  (let ((proc (get-buffer-process dbg-comint-buffer)))
    (goto-char (process-mark proc))
    (kill-line 1)
    (goto-char (point-max))))

;*---------------------------------------------------------------------*/
;*    dbg-line-number ...                                              */
;*    -------------------------------------------------------------    */
;*    This function returns the point line number.                     */
;*---------------------------------------------------------------------*/
(defun dbg-line-number (buffer pos)
  (save-restriction
    (set-buffer buffer)
    (let (start)
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (widen)
	  (beginning-of-line)
	  (setq start (point))
	  (goto-char pos)
	  (beginning-of-line)
	  (if (/= start 1)
	      (1+ (count-lines 1 (point)))
	    (1+ (count-lines 1 (point)))))))))

;*---------------------------------------------------------------------*/
;*    dbg-delete-window-or-frame ...                                   */
;*    -------------------------------------------------------------    */
;*    If a buffer is the only one displayed in a frame, this function  */
;*    will delete the frame. Otherwise, the window displaying the      */
;*    buffer will be deleted.                                          */
;*---------------------------------------------------------------------*/
(defun dbg-delete-window-or-frame (buffer)
  (let ((win (get-buffer-window buffer t)))
    (if (windowp win)
	(let ((frame (window-frame win)))
	  (if (framep frame)
	      (save-excursion
		(select-window win)
		(if (one-window-p t)
		    (delete-frame frame)
		  (delete-window win)))
	    (delete-window win))))))

;*---------------------------------------------------------------------*/
;*    dbg-show ...                                                     */
;*---------------------------------------------------------------------*/
(defun dbg-show ()
  (interactive)
  (if (memq 'local dbg-show)
      (if (not (dbg-installed-hook-p 'dbg-locals-hook))
	  (dbg-locals-start nil)
	(dbg-locals-quit)))
  (if (memq 'args dbg-show)
      (if (not (dbg-installed-hook-p 'dbg-args-hook))
	  (dbg-args-start nil)
	(dbg-args-quit)))
  (if (memq 'display dbg-show)
      (if (not (dbg-installed-hook-p 'dbg-display-hook))
	  (dbg-display-start nil)
	(dbg-display-quit)))
  (if (memq 'stack dbg-show)
      (if (not (dbg-installed-hook-p 'dbg-stack-hook))
	  (dbg-stack-start nil)
	(dbg-stack-quit)))
  (let ((window (get-buffer-window dbg-comint-buffer)))
    (if (windowp window)
	(select-window window))))
