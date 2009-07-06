;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-display.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 30 10:24:19 1998                          */
;*    Last change :  Fri Feb  8 08:17:32 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implement the dgb display printing.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-display)
(require 'dbg-config)
(require 'dbg)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'font-lock)

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-display-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-display-string)

;; this variable holds a state: are we currently inquirying for displays
(defvar dbg-display-command-armed-p nil)

;; the buffer for C display
(defvar dbg-display-buffer nil)

;; display in frame or window
(defvar dbg-display-in-frame-p t)

;*---------------------------------------------------------------------*/
;*    dbg-display-hook ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-display-hook (input)
  (if (and (not dbg-display-command-armed-p)
	   (or (not (stringp input))
	       ;; we invoke this hook on:
	       ;; next, step, continue, until, call, finish, run,
	       ;; return and breturn.
	       ;; it is important not to re-call the hook on `frame...'
	       (string-match dbg-display-hook-regexp input)))
      (progn
	(if (not (bufferp dbg-display-buffer))
	    (setq dbg-display-buffer (dbg-make-display-buffer "*Dbg display*"))
	  (dbg-popup-display dbg-display-buffer))
	(dbg-display-command dbg-display-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-display-command ...                                          */
;*    -------------------------------------------------------------    */
;*    This command refresh the display tracing.                        */
;*---------------------------------------------------------------------*/
(defun dbg-display-command (buffer)
  "Perform completion on the DBG command preceding point.
This is implemented using the DBG `display' command." 
  (interactive)
  (if (not dbg-display-command-armed-p)
      (progn
	;; we mark that display is armed not to re-enter this hook
	(setq dbg-display-command-armed-p t)
	;; we wait for comint to be ready
	(dbg-wait-for-comint "dbg-display-command" dbg-wait-timeout)
	;; Temporarily install our filter function.
	(let ((dbg-marker-filter 'dbg-display-filter))
	  ;; Issue the command to DBG.
	  (dbg-info-display-call)
	  (setq dbg-display-in-progress t)
	  (setq dbg-display-string "")
	  ;; Slurp the output.
	  (while dbg-display-in-progress
	    (if (not (accept-process-output
		      (get-buffer-process dbg-comint-buffer)
		      dbg-wait-output-timeout ))
		(progn
		  (message "Display timeout...")
		  (setq dbg-display-in-progress nil)
		  (setq dbg-display-string nil)))))
	;; at this point dbg-display-string is the string of all
	;; the display, we have to parse in order to find which
	;; display are still actives.
	(if (stringp dbg-display-string)
	    (dbg-display-display buffer dbg-display-string))
	(setq dbg-display-command-armed-p nil)))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-info-display-call ...                                        */
;*    -------------------------------------------------------------    */
;*    Sending a dbg command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-display-call ()
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
      (process-send-string proc dbg-display-command)
      (process-send-string proc "\n"))))

;*---------------------------------------------------------------------*/
;*    dbg-display-filter ...                                           */
;*    -------------------------------------------------------------    */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of DBG up to the next prompt and build the completion */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(defun dbg-display-filter (string)
  (setq dbg-marker-acc "")
  (setq dbg-display-string (concat dbg-display-string string))
  (if (string-match comint-prompt-regexp string)
      (setq dbg-display-in-progress nil))
  "")

;*---------------------------------------------------------------------*/
;*    The display mode map                                             */
;*---------------------------------------------------------------------*/
(defvar dbg-display-mode-map (make-sparse-keymap))
(define-key dbg-display-mode-map dbg-mouse-binding
  'dbg-display-mode-menu)
(defvar dbg-display-mouse-map (make-sparse-keymap))
(define-key dbg-display-mouse-map ude-mouse-2-binding
  'dbg-display-menu)
(define-key dbg-display-mode-map "\C-x\C-c" 'dbg-display-quit)

;*---------------------------------------------------------------------*/
;*    dbg-display-display ...                                          */
;*    -------------------------------------------------------------    */
;*    Display a new display in a separate buffer.                      */
;*---------------------------------------------------------------------*/
(defun dbg-display-display (buffer display)
  (save-excursion
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert display)
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
	      (if (looking-at "^[0-9]+")
		  ;; this line is a frame line
		  (let ((end (progn (end-of-line) (point))))
		    (put-text-properties start end
					 'mouse-face 'highlight
					 'keymap dbg-display-mouse-map)
		    (if (< end (point-max))
			(next-line 1)
		      (setq keep nil)))
		(progn
		  (end-of-line)
		  (if (< (point) (point-max))
		      (next-line 1)
		    (setq keep nil)))))))))))
		
;*---------------------------------------------------------------------*/
;*    dbg-make-display-buffer ...                                      */
;*---------------------------------------------------------------------*/
(defun dbg-make-display-buffer (name)
  (if dbg-display-in-frame-p
      (dbg-make-display-frame-buffer name)
    (dbg-make-display-window-buffer name)))

;*---------------------------------------------------------------------*/
;*    dbg-make-display-frame-buffer ...                                */
;*---------------------------------------------------------------------*/
(defun dbg-make-display-frame-buffer (name)
  (let ((height dbg-display-height))
    (let* ((default-frame-alist (ude-default-frame-alist 'height height))
	   (buffer (switch-to-buffer-other-frame name)))
      (set-buffer buffer)
      (dbg-display-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-make-display-window-buffer ...                               */
;*---------------------------------------------------------------------*/
(defun dbg-make-display-window-buffer (name)
  (set-buffer dbg-comint-buffer)
  (let* ((window (selected-window))
	 (height (window-height window)))
    (setq height height)
    (split-window window)
    (let ((buffer (switch-to-buffer-other-window name)))
      (set-buffer buffer)
      (dbg-display-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-popup-display ...                                            */
;*    -------------------------------------------------------------    */
;*    If the frame displaying a display buffer has been killed, we     */
;*    popup a fresh one.                                               */
;*---------------------------------------------------------------------*/
(defun dbg-popup-display (buffer)
  (let ((window (get-buffer-window buffer 0)))
    (if (not (windowp window))
	(dbg-make-display-buffer (buffer-name buffer)))))
	
;*---------------------------------------------------------------------*/
;*    dbg-display-mode ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-display-mode ()
  "Major mode for dbg display.

\\{dbg-display-mode-map}"
  (setq major-mode 'dbg-display-mode)
  (setq mode-name "Dbg Display")
  (setq buffer-read-only t)
  (setq comint-prompt-regexp dbg-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (use-local-map dbg-display-mode-map)
  (suppress-keymap dbg-display-mode-map)
  (if (one-window-p)
      (frame-getrid-toolbar (selected-frame)))
  (font-lock-mode t)
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-keywords (list
			    (list "^[0-9]+:\\s-+\\(\\S-+\\).+(!)"
				  1
				  dbg-c-frame-face)
			    (list "^[0-9]+:\\s-+\\(\\S-+\\).+(-)"
				  1
				  dbg-scheme-frame-face))))
  
;*---------------------------------------------------------------------*/
;*    dbg-display-menu ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-display-menu (event)
  (interactive "e")
  (let (selection)
    (save-excursion
      (set-buffer (event-buffer event))
      (save-excursion
	(goto-char (event-closest-point event))
	(message "point: %S" (point))
	(setq selection (dbg-get-display-number))))
    (popup-menu (list (concat "display " selection)
		      (vector "disable"
			      (list 'dbg-remote-call
				    (concat "disable display " selection))
			      t)
		      (vector "undisplay"
			      (list 'dbg-remote-call
				    (concat "undisplay " selection))
			      t)))))

;*---------------------------------------------------------------------*/
;*    dbg-get-display-number ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-get-display-number ()
  (save-excursion
    (let ((pos (re-search-backward "^[0-9]+" nil t)))
      (or (and pos (buffer-substring (match-beginning 0) (match-end 0)))
	  "0"))))

;*---------------------------------------------------------------------*/
;*    dbg-display-mode-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-display-mode-menu (event)
  (interactive "e")
  (popup-menu
   (list "dbg display" ["Close display frame display" dbg-display-quit t])))

;*---------------------------------------------------------------------*/
;*    dbg-display-set-height ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-display-set-height (height)
  (interactive "nHeight: ")
  (if (> height 0)
      (progn
	(setq dbg-display-height height)
	(set-frame-height (selected-frame) dbg-display-height))))

;*---------------------------------------------------------------------*/
;*    dbg-display-start ...                                            */
;*---------------------------------------------------------------------*/
(defun dbg-display-start (framep)
  (interactive)
  (setq dbg-display-in-frame-p framep)
  (if (not (dbg-installed-hook-p 'dbg-display-hook))
      (progn
	(dbg-add-send-input-hook 'dbg-display-hook)
	(dbg-display-hook t))))

;*---------------------------------------------------------------------*/
;*    dbg-display-quit ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-display-quit ()
  (interactive)
  (dbg-delete-window-or-frame dbg-display-buffer)
  (dbg-remove-send-input-hook 'dbg-display-hook))

;*---------------------------------------------------------------------*/
;*    dbg-display-toggle ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-display-toggle ()
  (interactive)
  (if (not (dbg-installed-hook-p 'dbg-display-hook))
      (dbg-display-start t)
    (dbg-display-quit)))
