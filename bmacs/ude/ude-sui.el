;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-sui.el                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 21 09:57:59 2002                          */
;*    Last change :  Mon Nov 24 10:14:16 2003 (serrano)                */
;*    Copyright   :  2002-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Interface to SUI files (see Scribe).                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'ude-sui)

;*---------------------------------------------------------------------*/
;*    ude-sui-find-ref ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-sui-find-ref (ident dir)
  (and (file-directory-p dir)
       (let ((files (directory-files dir t ".*[.]sui"))
	     (res '()))
	 (while (and (consp files) (not res))
	   (setq res (ude-sui-find-ref-file (car files) ident 'marks))
	   (setq files (cdr files)))
	 res)))

;*---------------------------------------------------------------------*/
;*    ude-sui-find-ref-file ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-sui-find-ref-file (file ident section)
  (let ((buffer (find-file-noselect file)))
    (let* ((sui (read buffer))
	   (marks (assq section (cdr (cdr (cdr (cdr sui)))))))
      (kill-buffer buffer)
      (if (consp marks)
	  (ude-sui-find-ref-section (cdr marks) ident)
	'()))))

;*---------------------------------------------------------------------*/
;*    ude-sui-find-ref-section ...                                     */
;*---------------------------------------------------------------------*/
(defun ude-sui-find-ref-section (entries ident)
  (let ((res nil))
    (while (and (consp entries) (not (stringp res)))
      (let ((entry (car entries)))
	(if (and (consp entry) (string= (car entry) ident))
	    (let ((f (memq ':file entry))
		  (m (memq ':mark entry)))
	      (if (and (consp f) (consp m))
		  (setq res (format "%s#%s" (car (cdr f)) (car (cdr m))))
		(setq entries (cdr entries))))
	  (setq entries (cdr entries)))))
    res))
