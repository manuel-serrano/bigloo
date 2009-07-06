;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-tags.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 08:32:07 1998                          */
;*    Last change :  Tue Sep 30 16:11:30 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    C tags handling.                                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'c-tags)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require bmacs-etags)
(require 'cee-autoload)
(require 'cee-config)
(require 'ude-autoload)
(require 'ude-config)

;*---------------------------------------------------------------------*/
;*    cee-find-file-create-p ...                                       */
;*    -------------------------------------------------------------    */
;*    This function searches the file FNAME in the root                */
;*    directory. If that file does not exists it is created. If after  */
;*    that creation FNAME still does not exists, nil is returned.      */
;*---------------------------------------------------------------------*/
(defun cee-find-file-create-p (fname cmd)
  (if (file-exists-p fname)
      fname
    (progn
      (message "calling `%s'..." cmd)
      (let ((res (shell-command-to-string cmd)))
	(message "")
	(if (string-match ude-make-error-output res)
	    nil
	  fname)))))

;*---------------------------------------------------------------------*/
;*    cee-find-tagsfile-create-p ...                                   */
;*    -------------------------------------------------------------    */
;*    This function searches the UDE-TAGSFILE-NAME file in the root    */
;*    directory. If that file does not exists it is created. If after  */
;*    that creation the tagsfile still does not exists, FIND-TAGSFILE  */
;*    returns nil.                                                     */
;*---------------------------------------------------------------------*/
(defun cee-find-tagsfile-create-p ()
  (cee-find-file-create-p (concat ude-root-directory ude-tagsfile-name)
			  cee-make-tags))
  
;*---------------------------------------------------------------------*/
;*    cee-tags-find-variable-noselect ...                              */
;*    -------------------------------------------------------------    */
;*    Find a buffer defining VAR in a buffer that is not selected.     */
;*---------------------------------------------------------------------*/
(defun cee-tags-find-variable-noselect (var)
  "Find a variable or class definition."
  (interactive "sVariable: ")
  (if (not (cee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (message "Seeking definition `%S'" var)
	  (find-tag-noselect var))
      (error (apply 'ude-error (cdr err))))))

;*---------------------------------------------------------------------*/
;*    cee-tags-find-variable ...                                       */
;*    -------------------------------------------------------------    */
;*    Find a variable or class definition.                             */
;*---------------------------------------------------------------------*/
(defun cee-tags-find-variable (var)
  "Find a variable or class definition."
  (interactive "sVariable: ")
  (if (not (cee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (find-tag-other-frame var))
      (error (apply 'ude-error (cdr err))))))

;*---------------------------------------------------------------------*/
;*    cee-tags-find ...                                                */
;*---------------------------------------------------------------------*/
(defun cee-tags-find ()
  "Find a C definition."
  (interactive)
  (let ((ident (ude-fetch-identifier (point))))
    (if (stringp ident)
	(cee-tags-find-variable ident)
      (let ((ident (read-string "Find C definition: ")))
	(if (not (string= ident ""))
	    (cee-tags-find-variable ident))))))
  
;*---------------------------------------------------------------------*/
;*    cee-tags-find-next ...                                           */
;*---------------------------------------------------------------------*/
(defun cee-tags-find-next ()
  "Find a variable/class/module definition."
  (interactive)
  (if (not (cee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (condition-case err
	(let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
	      (tags-add-tables t))
	  (find-tag nil t))
      (error (apply 'ude-error "No more definition %S" (cdr err))))))

;*---------------------------------------------------------------------*/
;*    cee-tag-find ...                                                 */
;*---------------------------------------------------------------------*/
(defun cee-tag-find ()
  "Find a variable or class definition."
  (interactive)
  (if (not (cee-find-tagsfile-create-p))
      (ude-error "Find variable: no tags file loaded.")
    (let* ((ident (ude-fetch-then-request-identifier (point) "Binding: ")))
      (condition-case err
	  (let ((tags-file-name (concat ude-root-directory ude-tagsfile-name))
		(tags-add-tables t))
	    (find-tag ident))
	(error (apply 'ude-error (cdr err)))))))

