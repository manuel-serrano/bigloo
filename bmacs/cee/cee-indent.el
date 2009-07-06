;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-indent.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:27:11 1998                          */
;*    Last change :  Tue Sep 20 08:39:56 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Cee indent.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-indent)
(require 'ude-autoload)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'cee-config)
(require 'cc-mode)

;*---------------------------------------------------------------------*/
;*    cee-indent-exp ...                                               */
;*---------------------------------------------------------------------*/
(defun cee-indent-exp ()
  (interactive)
  (condition-case err
      (c-indent-exp)
    (error
     (if (and (consp (cdr err)) (stringp (car (cdr err))))
	 (ude-error "Illegal expression")
       nil))))
      
;*---------------------------------------------------------------------*/
;*    cee-external-indent-process ...                                  */
;*---------------------------------------------------------------------*/
(defvar cee-external-indent-process '()
  "The external Cee indent process")

;*---------------------------------------------------------------------*/
;*    cee-external-indent-sentinel ...                                 */
;*---------------------------------------------------------------------*/
(defun cee-external-indent-sentinel (proc msg)
  (if (equal (substring msg 0 8) "finished")
      (progn
	(message "Indent done ...")
	(goto-char (point-min)))
    (progn
      (beep)
      (message "Abnormal end: %s" msg ))))

;*---------------------------------------------------------------------*/
;*    cee-external-indent ...                                          */
;*---------------------------------------------------------------------*/
(defun cee-external-indent ()
  "Call an external global indent on current buffer"
  (interactive)
  (cond
   ((buffer-modified-p)
    (ude-error "Can't Indent modified buffers ..."))
   (t
    (goto-char (point-max))
    (kill-region 1 (point))
    (message "Indenting C file")
    (setq cee-external-indent-process
	  (start-process "indent"
			 (buffer-name)
			 cee-external-indent
			 cee-external-indent-opt
			 (buffer-file-name)))
    (set-process-sentinel cee-external-indent-process
			  'cee-external-indent-sentinel))))


;*---------------------------------------------------------------------*/
;*    cee-indent-hook ...                                              */
;*---------------------------------------------------------------------*/
(defun cee-indent-hook ()
  (if (boundp 'c-version)
      (progn
	(cond
	 ((not (boundp 'c-style-alist))
	  (error "cee-indent-init:cannot find c-style-alist"))
	 ((consp (assoc "k&r" c-style-alist))
	  (c-set-style "k&r"))
	 ((consp (assoc "K&R" c-style-alist))
	  (c-set-style "K&R"))
	 (t
	  (error "cee-indent-init:cannot find k&r style")))
	(setq c-echo-syntactic-information-p t)
	(setq c-basic-offset 3)
	(c-set-offset 'access-label '0)
	(c-set-offset 'case-label '+))
    (progn
      (make-local-variable 'tab-width)
      (setq tab-width 8)
      ;; tab indent la ligne courante
      (setq c-tab-always-indent t)
      ;; la taille des indentations
      ;; le auto-newline
      (setq c-auto-newline nil))))


