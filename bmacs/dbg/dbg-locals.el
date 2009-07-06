;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-locals.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 30 10:24:19 1998                          */
;*    Last change :  Fri Feb  8 08:17:44 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implement the dbg locals printing.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-locals)
(require 'dbg-config)
(require 'dbg)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'font-lock)

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-locals-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-locals-string)

;; this variable holds a state: are we currently inquirying for localss
(defvar dbg-locals-command-armed-p nil)

;; the buffer for C locals
(defvar dbg-locals-buffer nil)

;; display in frame or window
(defvar dbg-locals-in-frame-p t)

;*---------------------------------------------------------------------*/
;*    dbg-locals-hook ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-locals-hook (input)
  (if (and (not dbg-locals-command-armed-p)
	   (or (not (stringp input))
	       ;; we invoke this hook on:
	       ;; next, step, continue, until, call, finish, run,
	       ;; return and breturn.
	       ;; it is important not to re-call the hook on `frame...'
	       (string-match dbg-display-hook-regexp input)))
      (progn
	(if (not (bufferp dbg-locals-buffer))
	    (setq dbg-locals-buffer (dbg-make-locals-buffer "*Dbg locals*"))
	  (dbg-popup-locals dbg-locals-buffer))
	(dbg-locals-command dbg-locals-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-locals-command ...                                           */
;*    -------------------------------------------------------------    */
;*    This command refresh the locals tracing.                         */
;*---------------------------------------------------------------------*/
(defun dbg-locals-command (buffer)
  "Perform completion on the DBG command preceding point.
This is implemented using the DBG `locals' command." 
  (interactive)
  (if (not dbg-locals-command-armed-p)
      (progn
	;; we mark that locals is armed not to re-enter this hook
	(setq dbg-locals-command-armed-p t)
	;; we wait for comint to be ready
	(dbg-wait-for-comint "dbg-locals-command" dbg-wait-timeout)
	;; Temporarily install our filter function.
	(let ((dbg-marker-filter 'dbg-locals-filter))
	  ;; Issue the command to DBG.
	  (dbg-info-locals-call)
	  (setq dbg-locals-in-progress t)
	  (setq dbg-locals-string "")
	  ;; Slurp the output.
	  (while dbg-locals-in-progress
	    (if (not (accept-process-output
		      (get-buffer-process dbg-comint-buffer)
		      dbg-wait-output-timeout))
		(progn
		  (message "Locals timeout...")
		  (setq dbg-locals-in-progress nil)
		  (setq dbg-locals-string nil)))))
	;; at this point dbg-locals-string is the string of all
	;; the locals, we have to parse in order to find which
	;; locals are still actives.
	(if (stringp dbg-locals-string)
	    (dbg-locals-locals buffer dbg-locals-string))
	(setq dbg-locals-command-armed-p nil)))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-info-locals-call ...                                         */
;*    -------------------------------------------------------------    */
;*    Sending a dbg command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-locals-call ()
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
      (process-send-string proc dbg-info-locals-command)
      (process-send-string proc "\n"))))

;*---------------------------------------------------------------------*/
;*    dbg-locals-filter ...                                            */
;*    -------------------------------------------------------------    */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of DBG up to the next prompt and build the completion */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(defun dbg-locals-filter (string)
  (setq dbg-marker-acc "")
  (setq dbg-locals-string (concat dbg-locals-string string))
  (if (string-match comint-prompt-regexp string)
      (setq dbg-locals-in-progress nil))
  "")

;*---------------------------------------------------------------------*/
;*    The locals mode map                                              */
;*---------------------------------------------------------------------*/
(defvar dbg-locals-mode-map (make-sparse-keymap))
(define-key dbg-locals-mode-map dbg-mouse-binding
  'dbg-locals-mode-menu)
(defvar dbg-locals-mouse-map (make-sparse-keymap))
(define-key dbg-locals-mouse-map ude-mouse-2-binding
  'dbg-locals-menu)
(define-key dbg-locals-mode-map "\C-x\C-c" 'dbg-locals-quit)

;*---------------------------------------------------------------------*/
;*    dbg-locals-locals ...                                            */
;*    -------------------------------------------------------------    */
;*    Locals a new locals in a separate buffer.                        */
;*---------------------------------------------------------------------*/
(defun dbg-locals-locals (buffer locals)
  (save-excursion
    (set-buffer buffer)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert locals)
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
					 'keymap dbg-locals-mouse-map)
		    (if (< end (point-max))
			(next-line 1)
		      (setq keep nil)))
		(progn
		  (end-of-line)
		  (if (< (point) (point-max))
		      (next-line 1)
		    (setq keep nil)))))))))))
		
;*---------------------------------------------------------------------*/
;*    dbg-make-locals-buffer ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-make-locals-buffer (name)
  (if dbg-locals-in-frame-p
      (dbg-make-locals-frame-buffer name)
    (dbg-make-locals-window-buffer name)))

;*---------------------------------------------------------------------*/
;*    dbg-make-locals-frame-buffer ...                                 */
;*---------------------------------------------------------------------*/
(defun dbg-make-locals-frame-buffer (name)
  (let ((height dbg-locals-height))
    (let* ((default-frame-alist (ude-default-frame-alist 'height height))
	   (buffer (switch-to-buffer-other-frame name)))
      (set-buffer buffer)
      (dbg-locals-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-make-locals-window-buffer ...                                */
;*---------------------------------------------------------------------*/
(defun dbg-make-locals-window-buffer (name)
  (set-buffer dbg-comint-buffer)
  (let* ((window (selected-window))
	 (height (window-height window)))
    (split-window window)
    (let ((buffer (switch-to-buffer-other-window name)))
      (set-buffer buffer)
      (dbg-locals-mode)
      (current-buffer))))

;*---------------------------------------------------------------------*/
;*    dbg-popup-locals ...                                             */
;*    -------------------------------------------------------------    */
;*    If the frame localsing a locals buffer has been killed, we       */
;*    popup a fresh one.                                               */
;*---------------------------------------------------------------------*/
(defun dbg-popup-locals (buffer)
  (let ((window (get-buffer-window buffer 0)))
    (if (not (windowp window))
	(dbg-make-locals-buffer (buffer-name buffer)))))
	
;*---------------------------------------------------------------------*/
;*    dbg-locals-mode ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-locals-mode ()
  "Major mode for dbg locals.

\\{dbg-locals-mode-map}"
  (setq major-mode 'dbg-locals-mode)
  (setq mode-name "Dbg Locals")
  (setq buffer-read-only t)
  (setq comint-prompt-regexp dbg-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (use-local-map dbg-locals-mode-map)
  (suppress-keymap dbg-locals-mode-map)
  (if (one-window-p)
      (frame-getrid-toolbar (selected-frame)))
  (font-lock-mode t)
  (setq font-lock-keywords-case-fold-search t)
  (setq font-lock-keywords (list
			    (list "^\\(\\S-+\\)\\s-+=" 1 dbg-c-frame-face))))
  
;*---------------------------------------------------------------------*/
;*    dbg-locals-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-locals-menu (event)
  (interactive "e")
  (let (selection)
    (save-excursion
      (set-buffer (event-buffer event))
      (save-excursion
	(goto-char (event-closest-point event))
	(message "point: %S" (point))
	(setq selection (dbg-get-locals-number))))
    (popup-menu (list (concat "locals " selection)
		      (vector "disable"
			      (list 'dbg-remote-call
				    (concat "disable locals " selection))
			      t)
		      (vector "unlocals"
			      (list 'dbg-remote-call
				    (concat "unlocals " selection))
			      t)))))

;*---------------------------------------------------------------------*/
;*    dbg-get-locals-number ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-get-locals-number ()
  (save-excursion
    (let ((pos (re-search-backward "^[0-9]+" nil t)))
      (or (and pos (buffer-substring (match-beginning 0) (match-end 0)))
	  "0"))))

;*---------------------------------------------------------------------*/
;*    dbg-locals-mode-menu ...                                         */
;*---------------------------------------------------------------------*/
(defun dbg-locals-mode-menu (event)
  (interactive "e")
  (popup-menu
   (list "dbg locals" ["Close locals frame locals" (dbg-locals-quit) t])))

;*---------------------------------------------------------------------*/
;*    dbg-locals-set-height ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-locals-set-height (height)
  (interactive "nHeight: ")
  (if (> height 0)
      (progn
	(setq dbg-locals-height height)
	(set-frame-height (selected-frame) dbg-locals-height))))

;*---------------------------------------------------------------------*/
;*    dbg-locals-start ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-locals-start (framep)
  (interactive)
  (setq dbg-locals-in-frame-p framep)
  (if (not (dbg-installed-hook-p 'dbg-locals-hook))
      (progn
	(dbg-add-send-input-hook 'dbg-locals-hook)
	(dbg-locals-hook t))))

;*---------------------------------------------------------------------*/
;*    dbg-locals-quit ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-locals-quit ()
  (interactive)
  (dbg-delete-window-or-frame dbg-locals-buffer)
  (dbg-remove-send-input-hook 'dbg-locals-hook))

;*---------------------------------------------------------------------*/
;*    dbg-locals-toggle ...                                            */
;*---------------------------------------------------------------------*/
(defun dbg-locals-toggle ()
  (interactive)
  (if (not (dbg-installed-hook-p 'dbg-local-hook))
      (dbg-locals-start t)
    (dbg-locals-quit)))
