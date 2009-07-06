;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-about.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 11 18:12:37 1998                          */
;*    Last change :  Wed Jan 23 11:55:20 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ude about implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-about)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-about-insert-xpm ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-about-insert-xpm (buffer xpm)
  (buffer-insert-image buffer xpm))

;*---------------------------------------------------------------------*/
;*    ude-about-make-junk-frame ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-about-make-junk-frame ()
  (if (featurep 'xemacs)
      (let ((window-min-height 1)
	    (window-min-width 1))
	(save-excursion
	  (set-buffer (generate-new-buffer "*junk-frame-buffer*"))
	  (prog1
	      (make-frame '(minibuffer t initially-unmapped t width 1 height 1))
	    (rename-buffer " *junk-frame-buffer*" t))))))

;*---------------------------------------------------------------------*/
;*    ude-about-make-frame ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-about-make-frame (x y msg xpm)
  (save-excursion
    (let ((window-min-height 100)
	  (window-min-width 100)
	  (bg-color (or (x-get-global-resource "backgroundToolBarColor"
					       "BackgroundToolBarColor")
			"grey75"))
	  (buffer (get-buffer-create " *ude-about-buffer*"))
	  (frame nil))
      (set-buffer buffer)
      (ude-about-insert-xpm (current-buffer) xpm)
      (insert "\n")
      (insert msg)
      (setq frame (make-frame (list
			       '(initially-unmapped . t)
			       ;; try to evade frame decorations
			       (cons 'name "ude about")
			       '(border-width . 2)
			       (cons 'border-color bg-color)
			       (cons 'top y)
			       (cons 'left x)
			       (cons 'popup (ude-about-make-junk-frame))
			       '(minibuffer . nil)
			       '(width . 50)
			       '(height . 12))))
      (frame-getrid-modeline frame)
      (frame-getrid-toolbar frame)
      (frame-getrid-text-cursor frame)
      (frame-getrid-menubar frame)
      (frame-getrid-scrollbar frame)
      (set-face-background 'default bg-color frame)
      (set-face-background 'modeline bg-color frame)
      (set-face-background-pixmap 'default "" frame)
      (set-window-buffer (frame-selected-window frame) buffer)
      frame)))

;*---------------------------------------------------------------------*/
;*    ude-about ...                                                    */
;*---------------------------------------------------------------------*/
(defun ude-about (msg xpm)
  (let* ((echo-keystrokes 0)
	 (params          (frame-parameters))
	 (left            (cdr (assq 'left params)))
	 (top             (cdr (assq 'top params))))
    (if (and (numberp left) (numberp top))
	(let ((frame (ude-about-make-frame (+ left 20) (+ top 150) msg xpm)))
	  (make-frame-visible frame)
	  (sit-for 10)
	  (make-frame-invisible frame)
	  (delete-frame frame)
	  (kill-buffer " *ude-about-buffer*")))))
