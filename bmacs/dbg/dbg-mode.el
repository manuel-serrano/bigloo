;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-mode.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 19 15:12:44 1998                          */
;*    Last change :  Tue Sep 20 08:52:25 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bdb mode (not to be confused with the bdb function).         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-mode)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'comint)
(require 'dbg-config)
(require 'dbg-filter)
(require 'dbg-complete)
(require 'dbg-toolbar)
(require 'dbg-about-icon)
(require 'dbg-autoload)
(require 'ude-autoload)
(require 'ude-config)

;*---------------------------------------------------------------------*/
;*    minibuffer local map                                             */
;*---------------------------------------------------------------------*/
(defvar dbg-minibuffer-local-map nil
  "Keymap for minibuffer prompting of bdb startup command.")

(if (not dbg-minibuffer-local-map)
    (progn
      (setq dbg-minibuffer-local-map (copy-keymap minibuffer-local-map))
      (define-key
	dbg-minibuffer-local-map "\C-i" 'comint-dynamic-complete-filename)))

;*---------------------------------------------------------------------*/
;*    dbg-release ...                                                  */
;*---------------------------------------------------------------------*/
(defun dbg-release ()
  (interactive)
  (ude-about (format "Ude release: %s\nDbg release: %s\n\n%s\n\n%s"
		     ude-version
		     dbg-version
		     ude-author
		     ude-url)
	     dbg-about-icon))
;  
;*---------------------------------------------------------------------*/
;*    dbg-make-customize-menu ...                                      */
;*---------------------------------------------------------------------*/
(defun dbg-make-customize-menu ()
  '(["Release..." dbg-release t]
     "--:shadowEtchedOut"
     ["Ude..." ude-customize t]
     ["Dbg..." dbg-customize t]))

;*---------------------------------------------------------------------*/
;*    dbg-output-buffer ...                                            */
;*    -------------------------------------------------------------    */
;*    The buffer for process output.                                   */
;*---------------------------------------------------------------------*/
(defvar dbg-output-buffer nil)

;*---------------------------------------------------------------------*/
;*    dbg-mode ...                                                     */
;*---------------------------------------------------------------------*/
(defun dbg-mode ()
  "Major mode for running Bdb withing XEmacs.
The following command are available:

\\{comint-mode-map}."
  (interactive)
  (comint-mode)
  (setq major-mode 'dbg-mode)
  (setq mode-name dbg-binary)
  (setq mode-line-process '(":%s"))
  (use-local-map (copy-keymap comint-mode-map))
  ;; local bindings
  (local-set-key "\C-i" 'dbg-complete-command)
  (local-set-key "\C-m" 'dbg-send-input)
  (local-set-key "\C-l" 'dbg-refresh)
  (local-set-key dbg-mouse-binding 'ude-predicate-mouse-event)
  ;; the popup menu
  (ude-add-menu #'(lambda (event) t)
		'dbg-popup-menu)
  ;; the menu bar
  (if (featurep 'menubar)
      (progn
	(ude-menubar-set "config" (dbg-make-customize-menu))))
  ;; the bdb prompt
  (setq comint-prompt-regexp dbg-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'paragraph-start)
  ;; the prompt marker is defined in dbg-filter file.
  (make-local-variable 'dbg-delete-prompt-marker)
  (setq dbg-delete-prompt-marker (make-marker))
  ;; same for dbg-last-frame
  (make-variable-buffer-local 'dbg-last-frame)
  (setq dbg-last-frame nil)
  ;; and so it is for dbg-marker-acc
  (make-variable-buffer-local 'dbg-marker-acc)
  ;; the mouse shape when flying over non text
  (ude-set-nontext-pointer "left_ptr")
  ;; we set up kill buffer hook
  (add-hook 'kill-buffer-hook 'dbg-quit '() t)
  ;; the toolbar
  (dbg-toolbar-init)
  ;; the modeline
  (ude-set-root-modeline)
  ;; we are done thus we run bdb hooks
  (run-hooks 'dbg-mode-hook))

;*---------------------------------------------------------------------*/
;*    dbg-quit ...                                                     */
;*---------------------------------------------------------------------*/
(defun dbg-quit ()
  (if (eq (current-buffer) dbg-comint-buffer)
      (progn
	;; we have to disconnect all connected buffer
	(dbg-disconnect-all-buffers)
	;; we now close all additional frames
	(if (dbg-installed-hook-p 'dbg-stack-hook)
	    (dbg-stack-quit))
	(if (dbg-installed-hook-p 'dbg-args-hook)
	    (dbg-args-quit))
	(if (dbg-installed-hook-p 'dbg-locals-hook)
	    (dbg-locals-quit))
	(if (dbg-installed-hook-p 'dbg-display-hook)
	    (dbg-display-quit))
	;; if bdb was the only one in its frame, we close the frame
	(let* ((frame (selected-frame))
	       (wins  (get-buffer-window-list dbg-comint-buffer)))
	  (while (consp wins)
	    (if (null (cdr wins))
		;; we have to delete the whole frame
		(progn
		  (setq wins '())
		  (delete-frame frame))
	      (progn
		(delete-window (car wins))
		(setq wins (cdr wins)))))))))

;*---------------------------------------------------------------------*/
;*    dbg-popup-menu ...                                               */
;*    ------------------------------------------------------------     */
;*    The main window bdb popup menu.                                  */
;*---------------------------------------------------------------------*/
(defun dbg-popup-menu (event)
  (interactive "e")
  (popup-menu
   (list "bdb"
	 ;; clearance
	 ["Clear line" dbg-clear-line t]
	 ["Clear window" dbg-clear-window t]
	 "--:shadowEtchedOut"
	 ;; buffer connection
	 ["Connect buffer" dbg-connect-buffer t]
	 ["Disconnect buffer" dbg-disconnect-buffer t]
	 ["Connect file" dbg-connect-file t]
	 "--:shadowEtchedOut"
	 ;; gdb hooking
	 ["Show stack"
	 dbg-stack-toggle
	 :style toggle
	 :selected (dbg-installed-hook-p 'dbg-stack-hook)]
	 ["Show args"
	 dbg-args-toggle
	 :style toggle
	 :selected (dbg-installed-hook-p 'dbg-args-hook)]
	 ["Show locals"
	 dbg-locals-toggle
	 :style toggle
	 :selected (dbg-installed-hook-p 'dbg-locals-hook)]
	 ["Show display"
	 dbg-display-toggle
	 :style toggle
	 :selected (dbg-installed-hook-p 'dbg-display-hook)]
	 ;; console logs
	 "--:shadowEtchedOut"
	 ["Console log" dbg-pop-console t]
	 ["Erase console log" dbg-erase-console t]
	 ["Recenter console log" dbg-recenter-console t])))

  

