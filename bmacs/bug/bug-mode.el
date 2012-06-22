;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-mode.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:36:12 2002                          */
;*    Last change :  Fri Jun 15 18:32:25 2012 (serrano)                */
;*    Copyright   :  2002-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bigloo debugger mode.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-mode)
(require 'comint)
(require 'ude-custom)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require (if (featurep 'xemacs) 'bug-xemacs 'bug-gnu-emacs))
(require 'bug-config)
(require 'bug-custom)
(require 'bug-toolbar)

;*---------------------------------------------------------------------*/
;*    bug-mode ...                                                     */
;*---------------------------------------------------------------------*/
(defun bug-mode ()
  "Major mode for running Bigloo debugger inside Emacs."
  (interactive)
  (comint-mode)
  (setq major-mode 'bug-mode)
  (setq mode-name bug-binary)
  (setq mode-line-process '(":%s"))
  (use-local-map (copy-keymap comint-mode-map))
  ;; local bindings
  (local-set-key "\C-i" 'bug-complete-command)
  (local-set-key "\C-m" 'bug-send-input)
  (local-set-key "\C-l" 'bug-refresh)
  ;; the popup menu
  (local-set-key ude-mouse-binding 'ude-predicate-mouse-event)
  (ude-add-menu #'(lambda (event) t) 'bug-popup-menu)
  ;; the bdb prompt
  (setq comint-prompt-regexp bug-prompt-regexp)
  (setq paragraph-start comint-prompt-regexp)
  (make-local-variable 'comint-prompt-regexp)
  (make-local-variable 'paragraph-start)
  ;; and so it is for bug-marker-acc
  (make-variable-buffer-local 'bug-marker-acc)
  ;; the mouse shape when flying over non text
  (ude-set-nontext-pointer "left_ptr")
  ;; we set up kill buffer hook
;*   (add-hook 'kill-buffer-hook 'bug-quit)                            */
  ;; the modeline
  (ude-set-root-modeline)
  ;; we are done thus we run bdb hooks
  (run-hooks 'bug-mode-hook)
  ;; the toolbar
  (bug-toolbar-init))
  
;*---------------------------------------------------------------------*/
;*    bug-clear-window ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-clear-window ()
  (interactive)
  (set-buffer bug-comint-buffer)
  (let ((proc (get-buffer-process bug-comint-buffer)))
    (goto-char (process-mark proc))
    (beginning-of-line)
    (delete-region (point-min) (point))
    (goto-char (point-max))))

;*---------------------------------------------------------------------*/
;*    bug-clear-line ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-clear-line ()
  (interactive)
  (set-buffer bug-comint-buffer)
  (let ((proc (get-buffer-process bug-comint-buffer)))
    (goto-char (process-mark proc))
    (kill-line 1)
    (goto-char (point-max))))

;*---------------------------------------------------------------------*/
;*    bug-popup-menu ...                                               */
;*    ------------------------------------------------------------     */
;*    The main window bdb popup menu.                                  */
;*---------------------------------------------------------------------*/
(defun bug-popup-menu (event)
  (interactive "e")
  (popup-menu
   (list "bdb"
	 ;; clearance
	 ["Clear line" bug-clear-line t]
	 ["Clear window" bug-clear-window t]
	 "--:shadowEtchedOut"
	 ;; buffer connection
	 ["Connect buffer" bug-connect-buffer t]
	 ["Disconnect buffer" bug-disconnect-buffer t]
	 ["Connect file" bug-connect-file t]
	 "--:shadowEtchedOut"
	 ;; hooking
	 ["Show stack"
	 bug-stack-toggle
	 :style toggle
	 :selected (bug-installed-hook-p 'bug-stack-hook)]
	 ["Show args"
	 bug-args-toggle
	 :style toggle
	 :selected (bug-installed-hook-p 'bug-args-hook)])))

 
