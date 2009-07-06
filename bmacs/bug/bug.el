;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug.el                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:49:29 2002                          */
;*    Last change :  Tue Sep 20 08:50:03 2005 (serrano)                */
;*    Copyright   :  2002-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo debugger emacs lisp mode entry point                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug)
(require 'comint)
(require 'bug-custom)
(require 'bug-config)
(require 'bug-autoload)
(require 'bug-process)
(require 'ude-autoload)
(require 'ude-custom)
(require 'ude-config)

;*---------------------------------------------------------------------*/
;*    Top level initialization                                         */
;*---------------------------------------------------------------------*/
(if (not bug-minibuffer-local-map)
    (progn
      (setq bug-minibuffer-local-map (copy-keymap minibuffer-local-map))
      (define-key
	bug-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename)))

;*---------------------------------------------------------------------*/
;*    bug-history ...                                                  */
;*---------------------------------------------------------------------*/
(defvar bug-history '())

;*---------------------------------------------------------------------*/
;*    bugloo ...                                                       */
;*---------------------------------------------------------------------*/
(defun bugloo (command-line)
  "Run a debugger BUGLOO on program FILE in buffer *bugloo-FILE*.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (let ((make-entry (ude-fetch-makefile-binary-entry)))
	   (if (stringp make-entry)
	       (concat bug-binary " " make-entry)
	     (read-from-minibuffer (concat "Run " bug-binary "(like this): ")
				   (if (consp bug-history)
				       (car bug-history)
				     (concat bug-binary " a.out"))
				   bug-minibuffer-local-map nil
				   '(bug-history . 1))))))
  (setq bug-history (cons command-line bug-history))
  (let ((default-directory ude-root-directory))
    ;; we split the command line in list of words
    (let* ((word-list (bug-string->list command-line))
	   (binary  (car word-list))
	   (a.out   (cadr word-list))
	   (bugname (concat bug-binary "-" a.out))
	   (bufname (concat "*" bugname "*")))
      (setq bug-binary binary)
      ;; we create a *bug* buffer
      (switch-to-buffer-other-frame bufname)
      ;; we set this buffer as the comint buffer
      (setq bug-comint-buffer (current-buffer))
      ;; we start comint
      (apply 'make-comint
	     `(,bugname ,binary ,nil ,@(bug-string->list bug-emacs-option)
			,a.out))
      ;; the process filter and sentinel
      (process-kill-without-query (get-buffer-process bug-comint-buffer))
      (set-process-filter (get-buffer-process bug-comint-buffer)
			  'bug-filter))
    ;; we start the bug-mode
    (bug-mode)
    ;; bug hooking
    (run-hooks 'bug-spawn-hook)))

;*---------------------------------------------------------------------*/
;*    bugloo-other-window ...                                          */
;*---------------------------------------------------------------------*/
(defun bugloo-other-window ()
  (interactive)
  (message "Spawning %s..." bug-binary)
  (if (bufferp bug-comint-buffer)
      (let ((pop-up-frames t))
	(display-buffer bug-comint-buffer))
    (call-interactively 'bugloo)))

;*---------------------------------------------------------------------*/
;*    bugloo-other-frame ...                                           */
;*---------------------------------------------------------------------*/
(defun bugloo-other-frame ()
  (interactive)
  (message "Spawning %s..." bug-binary)
  (if (bufferp bug-comint-buffer)
      (let ((pop-up-frames t))
	(display-buffer bug-comint-buffer))
    (call-interactively 'bugloo)))

;*---------------------------------------------------------------------*/
;*    bug-string->list ...                                             */
;*    -------------------------------------------------------------    */
;*    This function split a string into a list of string. Space or     */
;*    tab is used as a word delimiter.                                 */
;*---------------------------------------------------------------------*/
(defun bug-string->list (string)
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
;*    bugloo-quit ...                                                  */
;*---------------------------------------------------------------------*/
(defun bugloo-quit ()
  (interactive)
  (if (bufferp bug-comint-buffer)
      (save-excursion
	(set-buffer bug-comint-buffer)
	(condition-case ()
	    (comint-kill-subjob)
	  (error
	   '_))
	(bug-stack-quit)
	(bug-disconnect-all-buffers)
	(ude-delete-buffer-window-frame bug-comint-buffer)
	(kill-buffer bug-comint-buffer)
	(setq bug-comint-buffer nil))))

;*---------------------------------------------------------------------*/
;*    bug-delete-window-or-frame ...                                   */
;*    -------------------------------------------------------------    */
;*    If a buffer is the only one displayed in a frame, this function  */
;*    will delete the frame. Otherwise, the window displaying the      */
;*    buffer will be deleted.                                          */
;*---------------------------------------------------------------------*/
(defun bug-delete-window-or-frame (buffer)
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
;*    bugloo-show ...                                                  */
;*---------------------------------------------------------------------*/
(defun bugloo-show ()
  (interactive)
  (if (memq 'args bug-show)
      (if (not (bug-installed-hook-p 'bug-args-hook))
	  (bug-args-start nil)
	(bug-args-quit)))
  (if (memq 'stack bug-show)
      (if (not (bug-installed-hook-p 'bug-stack-hook))
	  (bug-stack-start nil)
	(bug-stack-quit)))
  (let ((window (get-buffer-window bug-comint-buffer)))
    (if (windowp window)
	(select-window window))))
