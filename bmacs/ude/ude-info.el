;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-info.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul 31 09:29:56 1998                          */
;*    Last change :  Sun Apr 10 10:00:41 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements an Xemacs configuration for the Info      */
;*    mode. To initialize ude-info, just call UDE-INFO-INIT.           */
;*                                                                     */
;*    This mode check the shell variable UDE-INFOPATH for extra        */
;*    directories containing info files. These directory must          */
;*    contain dir files.                                               */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-info)
(require 'info)
(require 'ude-icon)
(require 'ude-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-info-initialized-p ...                                       */
;*    -------------------------------------------------------------    */
;*    Is ude-info initialized for that buffer?                         */
;*---------------------------------------------------------------------*/
(defvar ude-info-initialized-p nil)
(make-variable-buffer-local 'ude-info-initialized-p)

;*---------------------------------------------------------------------*/
;*    ude-info-font-lock-keywords ...                                  */
;*---------------------------------------------------------------------*/
(defvar ude-info-font-lock-keywords nil)

;*---------------------------------------------------------------------*/
;*    ude-info-init ...                                                */
;*---------------------------------------------------------------------*/
(defun ude-info-init (flock)
  (setq ude-info-font-lock-keywords flock)
  (if (not ude-info-initialized-p)
      (progn
	(setq ude-info-initialized-p t)
	(add-hook 'Info-mode-hook (function ude-info-mode-hook)))))

;*---------------------------------------------------------------------*/
;*    ude-info toolbar ...                                             */
;*---------------------------------------------------------------------*/
(defvar ude-info-toolbar
  `(;; the quit button
    (,ude-quit-icon ude-tool-bar-delete-frame "Close Info Frame")
    --
    ;; back action
    (,ude-back-icon ude-info-back "Return to previous page in History list")
    ;; forward action
    (,ude-forward-icon ude-info-forward "Go to the next page in History list")
    ;; the up action
    (,ude-up-icon ude-info-up "Go to the superior node")
    ;; the next action
    (,ude-next-icon ude-info-next "Go to the next node")
    --
    ;; home action
    (,ude-home-icon ude-info-home "Go to the home page")
    ;; hotlist action
    (,ude-hotlist-icon ude-info-hotlist "Select an ude-info page")
    ;; the open action
    (,ude-open-icon ude-info-open "Open an ude-info page")
    --
    ;; the print action
    (,ude-print-icon ude-info-print "Print this ude-info page")
    ;; the search action
    (,ude-search-icon ude-info-search "Search for a string")
    --
    ;; flushing right
    -->
    --
    ;; the help action
    (,ude-help-icon ude-info-help "The help for ude-info")
    ;; the info button
    (,ude-info-icon ude-info-info "The online documentation for Info")))

(fset 'ude-info-back 'Info-last)

(defun ude-info-forward ()
  (Info-last))

(fset 'ude-info-up 'Info-up)
(fset 'ude-info-next 'Info-next)

(defun ude-info-home ()
  (let ((up (Info-extract-pointer "up" t)))
    (while (and (stringp up) (not (string-equal up "(dir)")))
      (progn
	(Info-up)
	(setq up (Info-extract-pointer "up" t))))))

(defun ude-info-hotlist ()
  (interactive)
  (Info-goto-node "(dir)"))

(fset 'ude-info-print 'print-buffer)
(fset 'ude-info-open 'Info-visit-file)
(fset 'ude-info-search 'Info-search)
(fset 'ude-info-help 'describe-mode)

(defun ude-info-info ()
  (interactive)
  (Info-find-node "info" "Top"))

(defvar Info-font-lock-keywords nil)

;*---------------------------------------------------------------------*/
;*    ude-info-mode-hook ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-info-mode-hook ()
  ;; additional files
  (let ((path (getenv "INFOPATH")))
    (if path
	(setq Info-directory-list
	      (append (split-string path path-separator)
		      Info-directory-list))))
  ;; ude-info faces
  (custom-set-faces
   '(info-xref ((t (:bold t :underline t :foreground "#0000ee"))))
   '(info-node ((t (:bold t :underline t :foreground "goldenrod")))))
  ;; the toolbar
  (ude-toolbar-set ude-info-toolbar)
  ;; the fontification
  (add-hook 'Info-select-hook
	    #'(lambda ()
	       (if ude-info-fontify
		   (font-lock-fontify-buffer))))
  (add-hook 'Info-startup-hook
	    #'(lambda ()
	       (let ((kwd ude-info-font-lock-keywords))
		 (if (consp kwd)
		     (setq Info-font-lock-keywords kwd)))))
  ;; the ude-info-hook
  (run-hooks 'ude-info-mode-hook))

