;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-doc.el                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 22 08:33:14 2002                          */
;*    Last change :  Fri Nov 12 15:00:09 2004 (serrano)                */
;*    Copyright   :  2002-04 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bee documentation handling                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bee-doc)
(require 'ude-custom)
(require 'ude-config)
(require 'ude-autoload)
(require 'bmacs-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-config)
(require 'bee-autoload)

;*---------------------------------------------------------------------*/
;*    bee-doc-installed-manuals ...                                    */
;*    -------------------------------------------------------------    */
;*    Returns the list of installed Bigloo manuals.                    */
;*---------------------------------------------------------------------*/
(defun bee-doc-installed-manuals ()
  (let ((sui (apply 'append (mapcar #'(lambda (d)
					(condition-case ()
					    (directory-files
					     (expand-file-name d)
					     t
					     "sui$")
					  (error
					   '())))
				    (cons bmacs-docdir bee-docdir)))))
    (let ((res '()))
      (while (consp sui)
	(let* ((f (concat (file-name-sans-extension (car sui)))))
	  (setq sui (cdr sui))
	  (if (file-exists-p (concat f ".html"))
	      (setq res (cons f res)))))
      (reverse res))))

;*---------------------------------------------------------------------*/
;*    bee-doc-visit ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-doc-visit (&optional manual)
  (interactive "sManual: ")
  (if (not manual)
      (setq manual bmacs-docdir))
  (if (or (eq bee-doc-preferred-format 'info)
	  (not (file-exists-p (concat manual ".html")))
	  (not (bee-doc-visit-html manual)))
      (bee-doc-visit-info)))

;*---------------------------------------------------------------------*/
;*    bee-doc-visit-bigloo ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-doc-visit-bigloo ()
  (interactive)
  (bee-doc-visit (concat bmacs-docdir "/bigloo")))

;*---------------------------------------------------------------------*/
;*    bee-doc-visit-info ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-doc-visit-info ()
  (setq ude-info-file-list bee-info-file-list)
  (ude-info-docline (bee-font-lock-get-info-keywords)))

;*---------------------------------------------------------------------*/
;*    bee-doc-visit-html ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-doc-visit-html (manual)
  (let ((url (concat "file:" manual ".html")))
    (if (stringp ude-url-browser)
	(ude-system ude-url-browser url)
      (browse-url url))))
  
;*---------------------------------------------------------------------*/
;*    bee-doc-ident ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-doc-ident (ident)
  (interactive (ude-interactive-ident (point) "Identifier: "))
  (if (or (eq bee-doc-preferred-format 'info)
	  (not (file-exists-p bmacs-docdir))
	  (not (bee-doc-ident-html ident)))
      (bee-doc-ident-info ident)))

;*---------------------------------------------------------------------*/
;*    bee-doc-ident-info ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-doc-ident-info (ident)
  (setq ude-info-file-list bee-info-file-list)
  (ude-info-docline-ident (bee-font-lock-get-info-keywords) ident))

;*---------------------------------------------------------------------*/
;*    bee-doc-ident-html ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-doc-ident-html (ident)
  (let ((html-ref (ude-sui-find-ref ident bmacs-docdir)))
    (if (stringp html-ref)
	(let ((url (concat "file:" bmacs-docdir "/" html-ref)))
	(if (stringp ude-url-browser)
	    (ude-system ude-url-browser url)
	  (browse-url url))))))

;*---------------------------------------------------------------------*/
;*    bee-doc-info-sexp ...                                            */
;*    -------------------------------------------------------------    */
;*    This function checks its string argument in order to             */
;*    discover what kind of documentation is required. For instance,   */
;*    this function may decide to print the number, string or          */
;*    application documentation instead of the documentation of a      */
;*    particular pre-defined function.                                 */
;*---------------------------------------------------------------------*/
(defun bee-doc-info-sexp (beg end)
  "Popup an online documentation according to the active region."
  (interactive "r")
  (let ((string (buffer-substring beg end)))
    (cond 
     ((string-match "^[ \t]*;" string)
      ;; this is a comment
      (ude-info-section "Comments"))
     ((string-match "^[0-9.]+$" string)
      ;; this is a number
      (ude-info-section "Numbers"))
     ((string-match "^\"[^\"]*\"" string)
      ;; this is a string
      (ude-info-section "Strings"))
     ((string-match "^((" string)
      ;; this is a computed call
      (ude-info-section "procedure call"))
     ((string-match "['`,]" string)
      ;; this is a quotation
      (ude-info-section "quotation"))
     ((string-match "#[ftFT]$" string)
      ;; booleans
      (ude-info-section "Booleans"))
     ((string-match "#[\\]" string)
      ;; chars
      (ude-info-section "Characters"))
     ((string-match "(\\(\\w+\\)" string)
      ;; an application
      (ude-info-ref-internal (substring string
					(match-beginning 1)
					(match-end 1))))
     (t
      (ude-info-ref-internal string)))))

;*---------------------------------------------------------------------*/
;*    bee-doc-initialize ...                                           */
;*    -------------------------------------------------------------    */
;*    Initialize the online documentation system, that is, tell Ude to */
;*    use info with the Bigloo page.                                   */
;*---------------------------------------------------------------------*/
(defun bee-doc-initialize ()
  (setq ude-info-region (function bee-doc-info-sexp))
  (setq ude-info-file-list bee-info-file-list))
  
