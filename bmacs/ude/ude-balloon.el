;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-balloon.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  6 16:00:36 1998                          */
;*    Last change :  Sat Oct  3 09:51:38 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    My own brewed balloon implementation. This implementation is     */
;*    much less ambition than the official balloon but it is less      */
;*    instrusive because much less resource consuming...               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-balloon)
(require 'ude-config)
(require 'ude-custom)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-balloon-started-p ...                                        */
;*    -------------------------------------------------------------    */
;*    A buffer local variable that tells if balloon is already all     */
;*    set on the requesting buffer.                                    */
;*---------------------------------------------------------------------*/
(defvar ude-balloon-started-p nil)
(make-variable-buffer-local 'ude-balloon-started-p)
(defvar ude-balloon-old-binding nil)
(make-variable-buffer-local 'ude-balloon-old-binding)
(defvar ude-balloon-old-global-binding nil)

;*---------------------------------------------------------------------*/
;*    ude-balloon-start ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-balloon-start ()
  (if (not ude-balloon-started-p)
      (progn
	(setq ude-balloon-started-p t)
;* 	(if (featurep 'xemacs)                                         */
;* 	    (progn                                                     */
	      (setq ude-balloon-old-binding
		    (global-key-binding ude-balloon-binding))
	      (if (not ude-balloon-old-global-binding)
		  (setq ude-balloon-old-global-binding 
			ude-balloon-old-binding))
;* 	      ))                                                       */
	(local-set-key ude-balloon-binding
		       #'(lambda (event)
			   (interactive "e")
			   (ude-balloon-mouse-hook event)
			   (cond
			    (ude-balloon-old-binding
			     (funcall ude-balloon-old-binding event))
			    (ude-balloon-old-global-binding
			     (funcall ude-balloon-old-global-binding event))))))))

;*---------------------------------------------------------------------*/
;*    ude-balloon-stop ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-balloon-stop ()
  (local-unset-key ude-balloon-binding)
  (if ude-balloon-old-binding
      (local-set-key ude-balloon-binding ude-balloon-old-binding)))

;*---------------------------------------------------------------------*/
;*    ude-balloon-actions ...                                          */
;*---------------------------------------------------------------------*/
(defvar ude-balloon-actions '())

;*---------------------------------------------------------------------*/
;*    ude-add-balloon-action ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-add-balloon-action (label pred action)
  (setq ude-balloon-actions (cons (cons label (cons pred action))
				  ude-balloon-actions)))

;*---------------------------------------------------------------------*/
;*    ude-remove-balloon-action ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-remove-balloon-action (label)
  (let ((cell (assq label ude-balloon-actions)))
    (if (consp cell)
	(setq ude-balloon-actions (delq cell ude-balloon-actions)))
    (if (null ude-balloon-actions)
	(ude-balloon-stop))))

;*---------------------------------------------------------------------*/
;*    ude-balloon-mouse-point ...                                      */
;*---------------------------------------------------------------------*/
(defvar ude-balloon-mouse-point nil)
(defvar ude-balloon-mouse-buffer nil)

;*---------------------------------------------------------------------*/
;*    ude-balloon-get-buffer ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-balloon-get-buffer ()
  ude-balloon-mouse-buffer)

;*---------------------------------------------------------------------*/
;*    ude-balloon-get-point ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-balloon-get-point ()
  ude-balloon-mouse-point)

;*---------------------------------------------------------------------*/
;*    ude-balloon-mouse-hook ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-balloon-mouse-hook (event)
  (if (eventp event)
      (let ((point (event-closest-point event))
	    (win (event-window event))
	    (l ude-balloon-actions)
	    (found nil))
	(setq ude-balloon-mouse-buffer (event-buffer event))
	(setq ude-balloon-mouse-point point)
	(while (and (not found) (consp l))
	  (if (funcall (car (cdr (car l))) point win)
	      (setq found (car l))
	    (setq l (cdr l))))
	(if found (funcall (cdr (cdr found)))))))

