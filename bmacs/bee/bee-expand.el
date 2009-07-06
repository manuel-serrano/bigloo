;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-expand.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 27 08:33:49 1998                          */
;*    Last change :  Tue Sep 20 08:39:45 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implement the Bee macro expansion part.                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-expand)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-config)
(require 'bee-mode)

;*---------------------------------------------------------------------*/
;*    bee-expand-temporary-buffer ...                                  */
;*---------------------------------------------------------------------*/
(defvar bee-expand-temporary-buffer nil
  "The temporary buffer for expansion.")

;*---------------------------------------------------------------------*/
;*    bee-external-expand-process ...                                  */
;*---------------------------------------------------------------------*/
(defvar bee-external-expand-process '()
  "The external Bee expand process")

;*---------------------------------------------------------------------*/
;*    bee-external-expand-completed ...                                */
;*---------------------------------------------------------------------*/
(defvar bee-external-expand-completed nil)

;*---------------------------------------------------------------------*/
;*    bee-external-expand-sentinel ...                                 */
;*---------------------------------------------------------------------*/
(defun bee-external-expand-sentinel (proc msg)
  (setq bee-external-expand-completed t)
  (if (equal (substring msg 0 8) "finished")
      (message "Expand done ...")
    (progn
      (beep)
      (message "Abnormal end: %s" msg ))))

;*---------------------------------------------------------------------*/
;*    bee-external-expand ...                                          */
;*---------------------------------------------------------------------*/
(defun bee-external-expand ()
  "Call an external global expand on current buffer"
   (interactive)
   (setq bee-external-expand-completed nil)
   (cond
     ((buffer-modified-p)
         (progn
            (beep)
            (message "Can't Expand modified buffers ...")))
     (t
      (progn
	(goto-char (point-max))
	(kill-region 1 (point))
	(message "Expanding Bigloo file...")
	(setq bee-external-expand-process
	      (start-process "expand"
			     (buffer-name)
			     bee-bigloo
			     "-expand"
			     "--to-stdout"
			     "-suffix"
			     "scme"
			     (buffer-file-name)))
	(set-process-sentinel bee-external-expand-process
			      'bee-external-expand-sentinel)))))

;*---------------------------------------------------------------------*/
;*    bee-set-expand-temporary-buffer ...                              */
;*---------------------------------------------------------------------*/
(defun bee-set-expand-temporary-buffer ()
  (if (and (bufferp bee-expand-temporary-buffer)
	   (buffer-live-p bee-expand-temporary-buffer))
      (save-excursion
	(set-buffer bee-expand-temporary-buffer)
	(toggle-read-only nil)
	(erase-buffer))
    (setq bee-expand-temporary-buffer (get-buffer-create "*bee tmp expand*"))))

;*---------------------------------------------------------------------*/
;*    bee-get-temp-name ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-get-temp-name ()
  (concat bee-tmp-dir "/" (make-temp-name "expand") ".scme"))

;*---------------------------------------------------------------------*/
;*    bee-expand-buffer-internal ...                                   */
;*---------------------------------------------------------------------*/
(defun bee-expand-buffer-internal (&optional completion-hook)
  ;; we now expand the buffer
  (set-buffer bee-expand-temporary-buffer)
  (bee-external-expand)
  ;; we wait for completion
  (while (not bee-external-expand-completed)
    (sit-for 0.1))
  ;; expansion is completed, we can keep going
  (if completion-hook
      (funcall completion-hook bee-expand-temporary-buffer))
  (delete-file (buffer-file-name bee-expand-temporary-buffer))
  (set-buffer-modified-p nil)
  (toggle-read-only t)
  (let* ((pop-up-frames t)
	 (lines      (+ 3 (count-lines (point-min) (point-max))))
	 (cur-height (frame-height (selected-frame)))
	 (default-frame-alist
	   (if (< lines cur-height)
	       (cons 'height
		     (cons lines
			   default-frame-alist))
	     (cons 'minibuffer
		   (cons nil
			 default-frame-alist)))))
    (bee-mode)
    (display-buffer (current-buffer))))

;*---------------------------------------------------------------------*/
;*    bee-expand-buffer ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-expand-buffer ()
  (interactive)
  (let ((buf   (current-buffer))
	(fname (let ((bufname (buffer-file-name)))
		 (if (stringp bufname)
		     (concat bufname "e")
		   (bee-get-temp-name)))))
    (bee-set-expand-temporary-buffer)
    (set-buffer bee-expand-temporary-buffer)
    (insert-buffer buf)
    (write-file fname nil)
    (bee-expand-buffer-internal)))

;*---------------------------------------------------------------------*/
;*    bee-expand-region ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-expand-region (beg end)
  "Expand each line of the list starting just after point."
  (interactive "r")
  (let ((buf   (current-buffer))
	(fname (bee-get-temp-name)))
    (bee-set-expand-temporary-buffer)
    ;; we now have to insert all macro definitions that are found
    ;; fefore beg in the source buffer.
    (let ((regexp "(define-\\(expander\\|macro\\)"))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward regexp beg t)
	  (let* ((beg (match-beginning 0))
		 (tl  (bee-find-toplevel-sexp (+ 1 beg))))
	    (if (and (consp tl) (eq (car tl) beg))
		(progn
		  ;; we got one
		  (set-buffer bee-expand-temporary-buffer)
		  (insert-buffer-substring buf (car tl) (cdr tl))
		  (set-buffer buf)
		  (goto-char (cdr tl)))
	      (match-end 0))))))
    ;; we now send the region that has to be expanded
    (set-buffer bee-expand-temporary-buffer)
    (insert-buffer-substring buf beg end)
    (goto-char (point-min))
    (insert "(module foo)\n")
    (write-file fname nil)
    (bee-expand-buffer-internal 'bee-remove-module-hook)))

;*---------------------------------------------------------------------*/
;*    bee-expand-last-sexp ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-expand-last-sexp ()
  (interactive)
  (save-excursion
    (forward-sexp -1)
    (let ((start (point)))
      (forward-sexp 1)
      (bee-expand-region start (point)))))

;*---------------------------------------------------------------------*/
;*    bee-expand-toplevel-sexp ...                                     */
;*---------------------------------------------------------------------*/
(defun bee-expand-toplevel-sexp (pos)
  (interactive "dPos: ")
  (save-excursion
    (let ((sexp (bee-find-toplevel-sexp pos)))
      (if (consp sexp)
	  (save-excursion (bee-expand-region (car sexp) (cdr sexp)))
	(error "Corrupted toplevel sexp")))))

;*---------------------------------------------------------------------*/
;*    bee-expand-define ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-expand-define ()
  (interactive)
  (condition-case ()
      (save-excursion
	(end-of-defun)
	(let ((end (point)))
	  (beginning-of-defun)
	  (bee-expand-region (point) end)))))

;*---------------------------------------------------------------------*/
;*    bee-remove-module-hook ...                                       */
;*    -------------------------------------------------------------    */
;*    This function search for a module clause. If found, the clause   */
;*    is removed.                                                      */
;*---------------------------------------------------------------------*/
(defun bee-remove-module-hook (buffer)
  (set-buffer buffer)
  (goto-char (point-min))
  (if (search-forward "(module" (point-max) t)
      (progn
	(goto-char (match-beginning 0))
	(if (condition-case ()
		(progn (forward-sexp 1) t)
	      (error nil))
	    (progn
	      (skip-chars-forward " \t\n")
	      (delete-region (point-min) (point)))))))
