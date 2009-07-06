;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-stack.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 30 10:24:19 1998                          */
;*    Last change :  Tue Sep 20 06:06:45 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implement the dbg stack printing.                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-stack)
(require 'dbg-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'dbg)
(require 'font-lock)

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-stack-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-stack-string)

;; this variable holds a state: are we currently inquirying for stacks
(defvar dbg-stack-command-armed-p nil)

;; the buffer for C stack
(defvar dbg-stack-buffer nil)

;; which stack to display (Scheme or C)?
(defvar dbg-stack 'scheme)

;; display in frame or window
(defvar dbg-stack-in-frame-p t)

;*---------------------------------------------------------------------*/
;*    dbg-stack-hook ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-stack-hook (input)
  (if (and (not dbg-stack-command-armed-p)
	   (or (not (stringp input))
	       ;; we invoke this hook on:
	       ;; next, step, continue, until, call, finish, run,
	       ;; return and breturn.
	       ;; it is important not to re-call the hook on `frame...'
	       (string-match dbg-display-hook-regexp input)))
      (progn
	(if (not (bufferp dbg-stack-buffer))
	    (setq dbg-stack-buffer (dbg-make-stack-buffer "*Dbg stack*"))
	  (dbg-popup-stack dbg-stack-buffer))
	(dbg-stack-command dbg-info-stack-hook-command dbg-stack-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-stack-command ...                                            */
;*    -------------------------------------------------------------    */
;*    This command refresh the stack tracing.                          */
;*---------------------------------------------------------------------*/
(defun dbg-stack-command (info buffer)
  "Perform completion on the DBG command preceding point.
This is implemented using the DBG `stack' command." 
  (interactive)
  (if (not dbg-stack-command-armed-p)
      (progn
	;; we mark that stack is armed not to re-enter this hook
	(setq dbg-stack-command-armed-p t)
	;; we wait for comint to be ready
	(dbg-wait-for-comint "dbg-stack-command" dbg-wait-timeout)
	;; Temporarily install our filter function.
	(let ((dbg-marker-filter 'dbg-stack-filter))
	  ;; Issue the command to DBG.
	  (dbg-info-stack-call info)
	  (setq dbg-stack-in-progress t)
	  (setq dbg-stack-string "")
	  ;; Slurp the output.
	  (while dbg-stack-in-progress
	    (if (not (accept-process-output
		      (get-buffer-process dbg-comint-buffer)
		      dbg-wait-output-timeout))
		;; we have exhausted wait time because we have
		;; raised the timeout
		(progn
		  (message "Stack timeout...")
		  (setq dbg-stack-in-progress nil)
		  (setq dbg-stack-string nil)))))
	;; at this point dbg-stack-string is the string of all
	;; the stack, we have to parse in order to find which
	;; stack are still actives.
	(if (stringp dbg-stack-string)
	    (dbg-display-stack buffer dbg-stack-string))
	(setq dbg-stack-command-armed-p nil)))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-info-stack-call ...                                          */
;*    -------------------------------------------------------------    */
;*    Sending a dbg command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-stack-call (info)
  (interactive)
  (let ((command (format "%s %s\n"
			 info 
			 (if (numberp dbg-stack-depth)
			     (int-to-string dbg-stack-depth)
			   "")))
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
;*    dbg-stack-filter ...                                             */
;*    -------------------------------------------------------------    */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of DBG up to the next prompt and build the completion */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(defun dbg-stack-filter (string)
  (setq dbg-marker-acc "")
  (setq dbg-stack-string (concat dbg-stack-string string))
  (if (string-match comint-prompt-regexp string)
      (setq dbg-stack-in-progress nil))
  "")

;*---------------------------------------------------------------------*/
;*    The stack mode map                                               */
;*---------------------------------------------------------------------*/
(defvar dbg-stack-mode-map (make-sparse-keymap))
(define-key dbg-stack-mode-map dbg-mouse-binding
  'dbg-stack-mode-menu)
(defvar dbg-stack-mouse-map (make-sparse-keymap))
(define-key dbg-stack-mouse-map ude-mouse-2-binding
  'dbg-frames-select-by-mouse)
(define-key dbg-stack-mode-map "\C-x\C-c" 'dbg-stack-quit)

;*---------------------------------------------------------------------*/
;*    dbg-display-stack ...                                            */
;*    -------------------------------------------------------------    */
;*    Display a new stack in a separate buffer.                        */
;*---------------------------------------------------------------------*/
(defun dbg-display-stack (buffer stack)
  (save-excursion
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert stack)
      (save-excursion
	(goto-char (point-max))
	(re-search-backward comint-prompt-regexp))
      ;; we remove the dbg prompt
      (delete-region (match-beginning 0) (match-end 0))
      ;; we insert property in all text
      (save-excursion
	(goto-char (point-min))
	(let ((keep t))
	  (while keep
	    (beginning-of-line)
	    (let ((start (point)))
	      (if (looking-at "^#[0-9]+")
		  ;; this line is a frame line
		  (let ((end (progn (end-of-line) (point))))
		    (put-text-properties start end
					 'mouse-face 'highlight
					 'keymap dbg-stack-mouse-map)
		    (if (< end (point-max))
			(next-line 1)
		      (setq keep nil)))
		(progn
		  (end-of-line)
		  (if (< (point) (point-max))
		      (next-line 1)
		    (setq keep nil)))))))))))
		
;*---------------------------------------------------------------------*/
;*    dbg-make-stack-buffer ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-make-stack-buffer (name)
  (if dbg-stack-in-frame-p
      (dbg-make-stack-frame-buffer name)
    (dbg-make-stack-window-buffer name)))

;*---------------------------------------------------------------------*/
;*    dbg-make-stack-frame-buffer ...                                  */
;*---------------------------------------------------------------------*/
(defun dbg-make-stack-frame-buffer (name)
  (let ((height (if (numberp dbg-stack-depth)
		    (+ dbg-stack-depth 3)
		  10)))
    (let* ((default-frame-alist (ude-default-frame-alist 'height height))
	   (buffer (switch-to-buffer-other-frame name)))
      (set-buffer buffer)
      (dbg-stack-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-make-stack-window-buffer ...                                 */
;*---------------------------------------------------------------------*/
(defun dbg-make-stack-window-buffer (name)
  (set-buffer dbg-comint-buffer)
  (let* ((window (selected-window))
	 (height (window-height window)))
    (split-window window)
    (let ((buffer (switch-to-buffer-other-window name)))
      (set-buffer buffer)
      (dbg-stack-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-popup-stack ...                                              */
;*    -------------------------------------------------------------    */
;*    If the frame/window displaying a stack buffer has been killed,   */
;*    we popup a fresh one.                                            */
;*---------------------------------------------------------------------*/
(defun dbg-popup-stack (buffer)
  (let ((window (get-buffer-window buffer 0)))
    (if (not (windowp window))
	(dbg-make-stack-buffer (buffer-name buffer)))))
	
;*---------------------------------------------------------------------*/
;*    dbg-stack-mode ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-stack-mode ()
  "Major mode for dbg frames.

\\{dbg-stack-mode-map}"
  (setq major-mode 'dbg-stack-mode)
  (setq mode-name "Dbg Stack")
  (setq buffer-read-only t)
  (setq comint-prompt-regexp dbg-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (use-local-map dbg-stack-mode-map)
  (suppress-keymap dbg-stack-mode-map)
  (if (one-window-p)
      (frame-getrid-toolbar (selected-frame)))
  (font-lock-mode t)
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-keywords (list
			    (list "#[0-9]+\\s-+\\([^0(]\\S-*\\)\\s-+\""
				  1
				  dbg-scheme-frame-face)
			    (list "#[0-9]+\\s-+\\([(][@][^)]+[)]\\)\\s-+\""
				  1
				  dbg-scheme-frame-face)
			    (list "in\\s-+\\([^0]\\S-*\\)\\s-+\""
				  1
				  dbg-scheme-frame-face)
			    (list "in\\s-+\\([(][@][^)]+[)]\\)\\s-+"
				  1
				  dbg-scheme-frame-face)
			    (list "#[0-9]+\\s-+\\([^0(]\\S-*\\)\\s-+[(]"
				  1
				  dbg-c-frame-face)
			    (list "in\\s-+\\([^0(]\\S-*\\)\\s-+[(]"
				  1
				  dbg-c-frame-face)
			    (list "#[0-9]+\\s-+\\([(][@][^)]+[)]\\)\\s-+[(]"
				  1
				  dbg-c-frame-face)
			    (list "in\\s-+\\([^0(]\\S-*\\)"
				  1
				  dbg-c-frame-face))))
  
;*---------------------------------------------------------------------*/
;*    dbg-frames-select-by-mouse ...                                   */
;*---------------------------------------------------------------------*/
(defun dbg-frames-select-by-mouse (event)
  (interactive "e")
  (let (selection)
    (save-excursion
      (set-buffer (event-buffer event))
      (save-excursion
	(goto-char (event-closest-point event))
	(setq selection (dbg-get-frame-number))))
    (select-window (get-buffer-window (event-buffer event)))
    (dbg-remote-call (format "frame %d" selection))))

;*---------------------------------------------------------------------*/
;*    dbg-get-frame-number ...                                         */
;*---------------------------------------------------------------------*/
(defun dbg-get-frame-number ()
  (save-excursion
    (let ((pos (re-search-backward "^#\\([0-9]\\)" nil t)))
      (or (and pos
	       (string-to-number
		(buffer-substring (match-beginning 1) (match-end 1))))
	  0))))

;*---------------------------------------------------------------------*/
;*    dbg-stack-mode-menu ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-stack-mode-menu (event)
  (interactive "e")
  (if (functionp dbg-stack-mode-menu)
      (popup-menu (funcall dbg-stack-mode-menu))
    (popup-menu (list "dbg stack"
		      "--:shadowEtchedOut"
		      (vector "Close stack frame display"
			      '(dbg-stack-quit)
			      t)))))

;*---------------------------------------------------------------------*/
;*    dbg-stack-start ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-stack-start (framep)
  (interactive)
  (setq dbg-stack-in-frame-p framep)
  (if (not (dbg-installed-hook-p 'dbg-stack-hook))
      (progn
	(dbg-add-send-input-hook 'dbg-stack-hook)
	(dbg-stack-hook t))))

;*---------------------------------------------------------------------*/
;*    dbg-stack-quit ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-stack-quit ()
  (interactive)
  (dbg-delete-window-or-frame dbg-stack-buffer)
  (dbg-remove-send-input-hook 'dbg-stack-hook))

;*---------------------------------------------------------------------*/
;*    dbg-stack-toggle ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-stack-toggle ()
  (interactive)
  (if (not (dbg-installed-hook-p 'dbg-stack-hook))
      (dbg-stack-start t)
    (dbg-stack-quit)))
