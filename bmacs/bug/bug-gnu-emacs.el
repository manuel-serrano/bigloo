;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-gnu-emacs.el           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 29 11:13:53 2002                          */
;*    Last change :  Tue Jan 28 11:12:19 2003 (serrano)                */
;*    Copyright   :  2002-03 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The GNU Emacs specific part of the debugger.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-gnu-emacs)
(require 'bug-custom)

;*---------------------------------------------------------------------*/
;*    GNU emacs settings...                                            */
;*---------------------------------------------------------------------*/
;; margin width
(defconst bug-gnu-emacs-margin-width 3)
;; margin color
(defconst bug-margin-color "none")

;*---------------------------------------------------------------------*/
;*    bug-toggle-margin-on ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-toggle-margin-on (buffer)
  (mapcar #'(lambda (win)
	      (set-window-margins win bug-gnu-emacs-margin-width))
	  (get-buffer-window-list buffer nil t)))

;*---------------------------------------------------------------------*/
;*    bug-toggle-margin-off ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-toggle-margin-off (buffer)
  (mapcar #'(lambda (win)
	      (set-window-margins win 0))
	  (get-buffer-window-list buffer nil t)))

;*---------------------------------------------------------------------*/
;*    bug-margin-off-p ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-margin-off-p ()
  (= (current-left-margin) 0))

;*---------------------------------------------------------------------*/
;*    bug-add-margin-image-overlay ...                                 */
;*---------------------------------------------------------------------*/
(defun bug-add-margin-image-overlay (buffer line image)
  (save-excursion
    (set-buffer buffer)
    (goto-line line)
    (let ((o (make-overlay (line-beginning-position)
			   (1+ (line-beginning-position))))
	  (s (string ? )))
      (insert-text-property 0 1
			 'display
			 (list '(margin left-margin) image)
			 s)
      (overlay-put o 'window nil)
      (overlay-put o 'before-string s)
      (overlay-put o 'evaporate t)
      (overlay-put o 'mouse-face 'highlight)
      o)))

;*---------------------------------------------------------------------*/
;*    bug-add-image-overlay ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-add-image-overlay (buffer char image)
  (save-excursion
    (set-buffer buffer)
    (goto-char char)
    (let ((o (make-overlay (line-beginning-position)
			   (1+ (line-beginning-position))))
	  (s (string ? )))
      (insert-text-property 0 1
			 'display
			 (list image)
			 s)
      (overlay-put o 'window nil)
      (overlay-put o 'before-string s)
      (overlay-put o 'evaporate t)
      (overlay-put o 'mouse-face 'highlight)
      o)))

;*---------------------------------------------------------------------*/
;*    bug-overlay-put ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-overlay-put (ov map)
  ;; don't use the LOCAL-MAP property otherwise, for a reason that I'm not
  ;; not able to understand, emacs disable the toolbar.
  (overlay-put ov 'keymap map))

;*---------------------------------------------------------------------*/
;*    bug-overlayp ...                                                 */
;*---------------------------------------------------------------------*/
(defun bug-overlayp (ov)
  (overlayp ov))

;*---------------------------------------------------------------------*/
;*    bug-delete-overlay ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-delete-overlay (ov)
  (delete-overlay ov))

;*---------------------------------------------------------------------*/
;*    bug-overlay-help ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-overlay-help (ov msg)
  (overlay-put ov 'help-echo msg))

;*---------------------------------------------------------------------*/
;*    bug-gnu-emacs-source-line-overlay ...                            */
;*---------------------------------------------------------------------*/
(defvar bug-gnu-emacs-source-line-overlay nil)

;*---------------------------------------------------------------------*/
;*    bug-unshow-line ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-unshow-line ()
  (if (overlayp bug-gnu-emacs-source-line-overlay)
      (delete-overlay bug-gnu-emacs-source-line-overlay))
  (setq overlay-arrow-position nil))

;*---------------------------------------------------------------------*/
;*    bug-show-line ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-show-line (window buffer line)
  (if (not bug-gnu-emacs-source-line-overlay)
      (progn
	(setq bug-gnu-emacs-source-line-overlay (make-overlay 1 1))
	(overlay-put bug-gnu-emacs-source-line-overlay 'face 'bug-line-face)))
  (save-excursion
    (set-buffer buffer)
    (let ((pos))
      (save-restriction
	(widen)
	(goto-line line)
	(setq pos (point))
	(move-overlay bug-gnu-emacs-source-line-overlay
		      (line-beginning-position)
		      (line-end-position)
		      buffer)
	(setq overlay-arrow-string "=>")
	(or overlay-arrow-position
	    (setq overlay-arrow-position (make-marker)))
	(set-marker overlay-arrow-position (point) (current-buffer)))
      (cond ((or (< pos (point-min)) (> pos (point-max)))
	     (widen)
	     (goto-char pos)))))
  (set-window-point window overlay-arrow-position))

;*---------------------------------------------------------------------*/
;*    bug-margin-pixel-width ...                                       */
;*---------------------------------------------------------------------*/
(defun bug-margin-pixel-width ()
  7)

;*---------------------------------------------------------------------*/
;*    bug-mouse-event-margin-p ...                                     */
;*---------------------------------------------------------------------*/
(defun bug-mouse-event-margin-p (event)
  (let ((m (window-margins)))
    (and (consp m)
	 (numberp (car m))
	 (<= (car (posn-x-y (event-start event)))
	     (* (car m) (bug-margin-pixel-width))))))
		   


    
  
