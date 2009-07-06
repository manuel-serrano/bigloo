;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-args.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 30 10:24:19 1998                          */
;*    Last change :  Fri Feb  8 08:17:17 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implement the dbg args printing.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-args)
(require 'dbg-config)
(require 'dbg)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'font-lock)

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-args-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-args-string)

;; this variable holds a state: are we currently inquirying for argss
(defvar dbg-args-command-armed-p nil)

;; the buffer for C args
(defvar dbg-args-buffer nil)

;; display in frame or window
(defvar dbg-args-in-frame-p t)

;*---------------------------------------------------------------------*/
;*    dbg-args-hook ...                                                */
;*---------------------------------------------------------------------*/
(defun dbg-args-hook (input)
  (if (and (not dbg-args-command-armed-p)
	   (or (not (stringp input))
	       ;; we invoke this hook on:
	       ;; next, step, continue, until, call, finish, run,
	       ;; return and breturn.
	       ;; it is important not to re-call the hook on `frame...'
	       (string-match dbg-display-hook-regexp input)))
      (progn
	(if (not (bufferp dbg-args-buffer))
	    (setq dbg-args-buffer (dbg-make-args-buffer "*Dbg args*"))
	  (dbg-popup-args dbg-args-buffer))
	(dbg-args-command dbg-args-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-args-command ...                                             */
;*    -------------------------------------------------------------    */
;*    This command refresh the args tracing.                           */
;*---------------------------------------------------------------------*/
(defun dbg-args-command (buffer)
  "Perform completion on the DBG command preceding point.
This is implemented using the DBG `args' command." 
  (interactive)
  (if (not dbg-args-command-armed-p)
      (progn 
	;; we mark that args is armed not to re-enter this hook
	(setq dbg-args-command-armed-p t)
	;; we wait for comint to be ready
	(dbg-wait-for-comint "dbg-args-command" dbg-wait-timeout)
	;; Temporarily install our filter function.
	(let ((dbg-marker-filter 'dbg-args-filter))
	  ;; Issue the command to DBG.
	  (dbg-info-args-call)
	  (setq dbg-args-in-progress t)
	  (setq dbg-args-string "")
	  ;; Slurp the output.
	  (while dbg-args-in-progress
	    (if (not (accept-process-output
		      (get-buffer-process dbg-comint-buffer)
		      dbg-wait-output-timeout))
		(progn
		  (message "Args timeout...")
		  (setq dbg-args-in-progress nil)
		  (setq dbg-args-string nil)))))
	;; at this point dbg-args-string is the string of all
	;; the args, we have to parse in order to find which
	;; args are still actives.
	(if (stringp dbg-args-string)
	    (dbg-args-args buffer dbg-args-string))
	(setq dbg-args-command-armed-p nil)))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-info-args-call ...                                           */
;*    -------------------------------------------------------------    */
;*    Sending a dbg command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-args-call ()
  (interactive)
  (let ((proc (get-buffer-process dbg-comint-buffer)))
    (or proc (ude-error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer dbg-comint-buffer)
      (goto-char (process-mark proc))
      (while (not (looking-at comint-prompt-regexp))
	(goto-char (process-mark proc))
	(beginning-of-line))
      (process-send-string proc dbg-info-args-command)
      (process-send-string proc "\n"))))

;*---------------------------------------------------------------------*/
;*    dbg-args-filter ...                                              */
;*    -------------------------------------------------------------    */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of DBG up to the next prompt and build the completion */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(defun dbg-args-filter (string)
  (setq dbg-marker-acc "")
  (setq dbg-args-string (concat dbg-args-string string))
  (if (string-match comint-prompt-regexp string)
      (setq dbg-args-in-progress nil))
  "")

;*---------------------------------------------------------------------*/
;*    The args mode map                                                */
;*---------------------------------------------------------------------*/
(defvar dbg-args-mode-map (make-sparse-keymap))
(define-key dbg-args-mode-map dbg-mouse-binding
  'dbg-args-mode-menu)
(defvar dbg-args-mouse-map (make-sparse-keymap))
(define-key dbg-args-mouse-map ude-mouse-2-binding
  'dbg-args-menu)
(define-key dbg-args-mode-map "\C-x\C-c" 'dbg-args-quit)

;*---------------------------------------------------------------------*/
;*    dbg-args-args ...                                                */
;*    -------------------------------------------------------------    */
;*    Args a new args in a separate buffer.                            */
;*---------------------------------------------------------------------*/
(defun dbg-args-args (buffer args)
  (save-excursion
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert args)
      (save-excursion
	(goto-char (point-max))
	(re-search-backward comint-prompt-regexp))
      ;; we remove the dbg prompt
      (delete-region (match-beginning 0) (match-end 0))
      ;; we insert a property in all text
      (save-excursion
	(goto-char (point-min))
	(let ((keep t))
	  (while keep
	    (beginning-of-line)
	    (let ((start (point)))
	      (if (looking-at "^[0-9]+")
		  ;; this line is a frame line
		  (let ((end (progn (end-of-line) (point))))
		    (put-text-properties start end
					 'mouse-face 'highlight
					 'keymap dbg-args-mouse-map)
		    (if (< end (point-max))
			(next-line 1)
		      (setq keep nil)))
		(progn
		  (end-of-line)
		  (if (< (point) (point-max))
		      (next-line 1)
		    (setq keep nil)))))))))))
		
;*---------------------------------------------------------------------*/
;*    dbg-make-args-buffer ...                                         */
;*---------------------------------------------------------------------*/
(defun dbg-make-args-buffer (name)
  (if dbg-args-in-frame-p
      (dbg-make-args-frame-buffer name)
    (dbg-make-args-window-buffer name)))

;*---------------------------------------------------------------------*/
;*    dbg-make-args-frame-buffer ...                                   */
;*---------------------------------------------------------------------*/
(defun dbg-make-args-frame-buffer (name)
  (let ((height dbg-args-height))
    (let* ((default-frame-alist (ude-default-frame-alist 'height height))
	   (buffer (switch-to-buffer-other-frame name)))
      (set-buffer buffer)
      (dbg-args-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-make-args-window-buffer ...                                  */
;*---------------------------------------------------------------------*/
(defun dbg-make-args-window-buffer (name)
  (set-buffer dbg-comint-buffer)
  (let* ((window (selected-window))
	 (height (window-height window)))
    (setq height height)
    (split-window window)
    (let ((buffer (switch-to-buffer-other-window name)))
      (set-buffer buffer)
      (dbg-args-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-popup-args ...                                               */
;*    -------------------------------------------------------------    */
;*    If the frame argsing a args buffer has been killed, we           */
;*    popup a fresh one.                                               */
;*---------------------------------------------------------------------*/
(defun dbg-popup-args (buffer)
  (let ((window (get-buffer-window buffer 0)))
    (if (not (windowp window))
	(dbg-make-args-buffer (buffer-name buffer)))))
	
;*---------------------------------------------------------------------*/
;*    dbg-args-mode ...                                                */
;*---------------------------------------------------------------------*/
(defun dbg-args-mode ()
  "Major mode for dbg args.

\\{dbg-args-mode-map}"
  (setq major-mode 'dbg-args-mode)
  (setq mode-name "Dbg Args")
  (setq buffer-read-only t)
  (setq comint-prompt-regexp dbg-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (use-local-map dbg-args-mode-map)
  (suppress-keymap dbg-args-mode-map)
  (font-lock-mode t)
  (if (one-window-p)
      (frame-getrid-toolbar (selected-frame)))
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-keywords (list
			    (list "^\\(\\S-+\\)\\s-+=" 1 dbg-c-frame-face))))
  
;*---------------------------------------------------------------------*/
;*    dbg-args-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun dbg-args-menu (event)
  (interactive "e")
  (let (selection)
    (save-excursion
      (set-buffer (event-buffer event))
      (save-excursion
	(goto-char (event-closest-point event))
	(message "point: %S" (point))
	(setq selection (dbg-get-args-number))))
    (popup-menu (list (concat "args " selection)
		      (vector "disable"
			      (list 'dbg-remote-call
				    (concat "disable args " selection))
			      t)
		      (vector "unargs"
			      (list 'dbg-remote-call
				    (concat "unargs " selection))
			      t)))))

;*---------------------------------------------------------------------*/
;*    dbg-get-args-number ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-get-args-number ()
  (save-excursion
    (let ((pos (re-search-backward "^[0-9]+" nil t)))
      (or (and pos (buffer-substring (match-beginning 0) (match-end 0)))
	  "0"))))

;*---------------------------------------------------------------------*/
;*    dbg-args-mode-menu ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-args-mode-menu (event)
  (interactive "e")
  (popup-menu
   (list "dbg args" ["Close args frame args" dbg-args-quit t])))

;*---------------------------------------------------------------------*/
;*    dbg-args-set-height ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-args-set-height (height)
  (interactive "nHeight: ")
  (if (> height 0)
      (progn
	(setq dbg-args-height height)
	(set-frame-height (selected-frame) dbg-args-height))))

;*---------------------------------------------------------------------*/
;*    dbg-args-start ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-args-start (framep)
  (interactive)
  (setq dbg-args-in-frame-p framep)
  (if (not (dbg-installed-hook-p 'dbg-args-hook))
      (progn
	(dbg-add-send-input-hook 'dbg-args-hook)
	(dbg-args-hook t))))

;*---------------------------------------------------------------------*/
;*    dbg-args-quit ...                                                */
;*---------------------------------------------------------------------*/
(defun dbg-args-quit ()
  (interactive)
  (dbg-delete-window-or-frame dbg-args-buffer)
  (dbg-remove-send-input-hook 'dbg-args-hook))

;*---------------------------------------------------------------------*/
;*    dbg-args-toggle ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-args-toggle ()
  (interactive)
  (if (not (dbg-installed-hook-p 'dbg-args-hook))
      (dbg-args-start t)
    (dbg-args-quit)))
