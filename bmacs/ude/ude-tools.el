;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-tools.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  8 11:52:51 1998                          */
;*    Last change :  Sun Apr 10 09:59:00 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Various tools for Ude.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-tools)
(require 'ude-custom)

;*---------------------------------------------------------------------*/
;*    ude-string-xpm-p ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-string-xpm-p (string)
  (string-match "/[*] *[Xx][Pp][Mm] *[*]/" string))

;*---------------------------------------------------------------------*/
;*    ude-string-suffix ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-string-suffix (string)
  (if (string-match ".+[.]\\([^.]*\\)" string)
      (substring string (match-beginning 1) (match-end 1))
    string))

;*---------------------------------------------------------------------*/
;*    ude-string-prefix ...                                            */
;*---------------------------------------------------------------------*/
(defun ude-string-prefix (string)
  (if (string-match "\\(.+\\)[.][^.]*" string)
      (substring string (match-beginning 1) (match-end 1))
    string))

;*---------------------------------------------------------------------*/
;*    ude-one-window-p ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-one-window-p (win)
  (let* ((frame (window-frame win))
	 (wins  (window-list frame nil win))
	 (res   t))
    (while (and res (consp wins))
      (cond
       ((eq (car wins) win)
	(setq wins (cdr wins)))
       ((window-minibuffer-p (car wins))
	(setq wins (cdr wins)))
       (t
	(setq res nil))))
    res))

;*---------------------------------------------------------------------*/
;*    ude-delete-buffer-window-frame ...                               */
;*    -------------------------------------------------------------    */
;*    Delete a buffer, its window and its frame if it is a one         */
;*    window frame.                                                    */
;*---------------------------------------------------------------------*/
(defun ude-delete-buffer-window-frame (buffer)
  "Delete a buffer, its window and its frame."
  (if (bufferp buffer)
      (let ((window (get-buffer-window buffer t)))
	(if (windowp window)
	    (if (ude-one-window-p window)
		(let ((frame (window-frame window)))
		  (if (framep frame)
		      (delete-frame frame)))
	      (delete-window window))))))

;*---------------------------------------------------------------------*/
;*    ude-one-frame-buffer-p ...                                       */
;*    -------------------------------------------------------------    */
;*    Is FRAME containing only one buffer, BUFFER.                     */
;*---------------------------------------------------------------------*/
(defun ude-one-frame-buffer-p (buffer)
  (if (bufferp buffer)
      (let ((window (get-buffer-window buffer 0)))
	(if (windowp window)
	    (ude-one-window-p window)
	  nil))
    (error "Argument not a buffer %S" buffer)))

;*---------------------------------------------------------------------*/
;*    ude-empty-window-p ...                                           */
;*    -------------------------------------------------------------    */
;*    Returns true if the selected window is the only one in its frame */
;*    and if it is empty.                                              */
;*---------------------------------------------------------------------*/
(defun ude-empty-window-p ()
  (and (ude-one-window-p (selected-window)) (= (point-min) (point-max))))

;*---------------------------------------------------------------------*/
;*    ude-region-excerpt ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-region-excerpt (min max)
  (if (< max min)
      (let ((old min))
	(setq min max)
	(setq max old)))
  (if (> (- max min) ude-menu-max-width)
      (let ((half (- (/ ude-menu-max-width 4) 1)))
	(concat (buffer-substring min (+ min half))
		" .. "
		(buffer-substring max (- max half))))
    (buffer-substring min max)))

;*---------------------------------------------------------------------*/
;*    ude-string-excerpt ...                                           */
;*---------------------------------------------------------------------*/
(defun ude-string-excerpt (string)
  (let ((len (length string)))
    (if (> len ude-menu-max-width)
	(let ((half (- (/ ude-menu-max-width 2) 1)))
	  (concat (substring string 0 half)
		  " .. "
		  (substring string (- len half) len)))
      string)))

;*---------------------------------------------------------------------*/
;*    ude-quit-frame-or-emacs ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-quit-frame-or-emacs ()
  (interactive)
  (if (eq (next-frame) (selected-frame))
      (save-buffers-kill-emacs)
    (condition-case nil
        (delete-frame (selected-frame) t)
      (error (save-buffers-kill-emacs)))))

;*---------------------------------------------------------------------*/
;*    ude-first-entries ...                                            */
;*    -------------------------------------------------------------    */
;*    Returns a cons of the first N entries and the NUM-N entries.     */
;*---------------------------------------------------------------------*/
(defun ude-first-entries (n entries)
  (let ((i    1)
	(tail entries))
    (while (and (< i n) (consp tail))
      (setq i (+ i 1))
      (setq tail (cdr tail)))
    (if (consp tail)
	(let ((res (cons entries (cdr tail))))
	  (rplacd tail '())
	  res)
      (cons entries nil))))

;*---------------------------------------------------------------------*/
;*    ude-split-menu-entries ...                                       */
;*    -------------------------------------------------------------    */
;*    Split a popup menu into several submenus if it has too much      */
;*    entries.                                                         */
;*---------------------------------------------------------------------*/
(defun ude-split-menu-entries (menu)
  (let ((len (length menu)))
    (if (<= len ude-menu-max-entry)
	menu
      (let ((new-menu '())
	    (old-menu  menu))
	(while (consp old-menu)
	  (let* ((split      (ude-first-entries ude-menu-max-entry old-menu))
		 (subentries (car split))
		 (submenu    (cons (concat (aref (car subentries) 0) "...")
				   subentries)))
	    (setq new-menu (cons submenu new-menu))
	    (setq old-menu (cdr split))))
	(nreverse new-menu)))))

;*---------------------------------------------------------------------*/
;*    ude-system ...                                                   */
;*    -------------------------------------------------------------    */
;*    Execute a command asynchronously with no output.                 */
;*---------------------------------------------------------------------*/
(defun ude-system (cmd &rest args)
  (interactive "sCommand: ")
  (apply 'start-process "system" nil cmd args))

;*---------------------------------------------------------------------*/
;*    ude-tool-bar-delete-frame ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-tool-bar-delete-frame ()
  (interactive)
  (let* ((frame (selected-frame))
	 (lst (window-list frame)))
    (if (and (consp lst) (null (cdr lst)))
	(delete-frame frame))))

