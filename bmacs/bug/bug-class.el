;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-class.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 15 08:29:50 2002                          */
;*    Last change :  Thu May 23 16:33:59 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Conversion from file name to class name.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-class)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require (if (featurep 'xemacs) 'bug-xemacs 'bug-gnu-emacs))
(require 'bee-autoload)
(require 'bee-config)
(require 'bug-autoload)

;*---------------------------------------------------------------------*/
;*    Current buffer class                                             */
;*---------------------------------------------------------------------*/
(defvar bug-buffer-class nil)
(make-variable-buffer-local 'bug-buffer-class)

;*---------------------------------------------------------------------*/
;*    bug-buffer-class-name ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-buffer-class-name ()
  (if (string-match "\\(.*\\)\.scm$" (buffer-name))
      (substring (buffer-name) (match-beginning 1) (match-end 1))))

;*---------------------------------------------------------------------*/
;*    bug-file-to-string ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-file-to-string (file)
  (save-excursion
    (let ((buffer (bug-find-file file)))
      (if buffer
	  (progn
	    (set-buffer buffer)
	    (let ((s (buffer-string)))
	      (kill-buffer buffer)
	      s))
	""))))

;*---------------------------------------------------------------------*/
;*    bug-jfile-module-name ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-jfile-module-name (jfile module)    
  (let* ((contents (read-from-string (bug-file-to-string jfile)))
	 (imodule (intern module))
	 (name (assq imodule (car contents))))
    (if name
	(cadr name))))
  
;*---------------------------------------------------------------------*/
;*    bug-find-jfile ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-find-jfile (file)
  (let* ((dir (or (ude-auto-find-root-directory file) "."))
	 (jfile (concat dir ".jfile")))
    (if (file-exists-p jfile)
	jfile
      nil)))
    
;*---------------------------------------------------------------------*/
;*    bug-find-current-buffer-class ...                                */
;*---------------------------------------------------------------------*/
(defun bug-find-current-buffer-class (file buffer)
  (let* ((jfile (bug-find-jfile file))
	 (classname (if jfile
			(let ((mod (bee-get-module-name buffer)))
			  (bug-jfile-module-name jfile mod))
		      nil)))
    (if classname
	classname
      (bug-buffer-class-name))))

;*---------------------------------------------------------------------*/
;*    bug-find-class-file ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-find-class-file (class)
  (let ((p (replace-regexp-in-string "[.]" "/" class))
	(s bee-suffixes)
	(f nil))
    (while (and (consp s) (not f))
      (let ((n (concat p "." (car s))))
	(if (file-exists-p n)
	    (setq f n)
	  (setq s (cdr s)))))
    f))

;*---------------------------------------------------------------------*/
;*    bug-class-files ...                                              */
;*---------------------------------------------------------------------*/
(defvar bug-class-files '())

;*---------------------------------------------------------------------*/
;*    bug-file-to-class ...                                            */
;*---------------------------------------------------------------------*/
(defun bug-file-to-class (file)
  (interactive "Ffile: ")
  (let ((buffer (get-buffer file)))
    (save-excursion
      (set-buffer buffer)
      (if bug-buffer-class
	  bug-buffer-class
	(progn
	  (setq bug-buffer-class (bug-find-current-buffer-class file buffer))
	  (setq bug-class-files (cons (cons bug-buffer-class file)
				      bug-class-files))
	  bug-buffer-class)))))

;*---------------------------------------------------------------------*/
;*    bug-class-to-file ...                                            */
;*---------------------------------------------------------------------*/
(defun bug-class-to-file (class)
  (interactive "sclass: ")
  (let ((c (assq class bug-class-files)))
    (if (consp c)
	(cdr c)
      (let ((f (bug-find-class-file class)))
	(if (stringp f)
	    (setq bug-class-files (cons (cons c f) bug-class-files)))
	f))))
