;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-breakpoint.el          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 16 11:06:07 2002                          */
;*    Last change :  Tue Dec  3 15:42:19 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Breakpoints handling                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-breakpoint)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-tools)
(require 'bug-config)
(require 'bug-autoload)
(require 'bug-images)

;*---------------------------------------------------------------------*/
;*    bug-breakpoints ...                                              */
;*---------------------------------------------------------------------*/
(defvar bug-breakpoints nil)

;*---------------------------------------------------------------------*/
;*    bug-find-breakpoint ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-find-breakpoint (bnum)
  (let ((cell (assq bnum bug-breakpoints)))
    (if (consp cell)
	(cdr cell)
      nil)))

;*---------------------------------------------------------------------*/
;*    bug-add-breakpoint ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-add-breakpoint (bnum bp)
  (setq bug-breakpoints (cons (cons bnum bp) bug-breakpoints))
  bp)

;*---------------------------------------------------------------------*/
;*    bug-del-breakpoint ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-del-breakpoint (bnum)
  (let ((cell (assq bnum bug-breakpoints)))
    (if (consp cell)
	(setq bug-breakpoints (delq cell bug-breakpoints)))))

;*---------------------------------------------------------------------*/
;*    bug-make-breakpoint ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-make-breakpoint (bnum bfile bclass bline bstatus)
  (bug-add-breakpoint bnum (vector bfile bclass bline bstatus '_ nil)))

;*---------------------------------------------------------------------*/
;*    bug-undisplay-breakpoint ...                                     */
;*---------------------------------------------------------------------*/
(defun bug-undisplay-breakpoint (o)
  (let ((ov (aref o 5)))
    (if (bug-overlayp ov)
	(bug-delete-overlay ov))))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint-image ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-breakpoint-image (k s)
  (cond
   ((eq k 'bp)
    (cond
     ((eq s 'on) bug-breakpoint-red-image)
     ((eq s 'off) bug-breakpoint-green-image)
     ((eq s 'tmp) bug-breakpoint-orange-image)
     (t bug-warning-image)))
   ((eq k 'fp)
    (cond
     ((eq s 'on) bug-footprint-enable-image)
     ((eq s 'off) bug-footprint-disable-image)
     (t bug-warning-image)))))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint-popup-menu ...                                    */
;*---------------------------------------------------------------------*/
(defun bug-breakpoint-popup-menu (f l n s)
  (popup-menu
   (list (if (and (stringp f) (numberp l))
	     (format "Breakpoint %d (%s, line %d)" n f l)
	   (format "Breakpoint %d" n))
	 "-"
	 (vector "Remove breakpoint..."
		 `(bug-remote-call (bug-delete-break-command ,n))
		 t)
	 "-"
	 (vector "Disable breakpoint..."
		 `(bug-remote-call (bug-disable-break-command ,n))
		 (or (eq s 'all) (eq s 'on) (eq s 'tmp)))
	 (vector "Enable breakpoint..."
		 `(bug-remote-call (bug-enable-break-command ,n))
		 (or (eq s 'all) (eq s 'off))))))

;*---------------------------------------------------------------------*/
;*    bug-footprint-popup-menu ...                                     */
;*---------------------------------------------------------------------*/
(defun bug-footprint-popup-menu (f l n s)
  (popup-menu
   (list (if (and (stringp f) (numberp l))
	     (format "Footprint %d (%s, line %d)" n f l)
	   (format "Footprint %d" n))
	 "-"
	 (vector "Remove footprint..."
		 `(bug-remote-call (bug-delete-footprint-command ,n))
		 t)
	 "-"
	 (vector "Disable footprint..."
		 `(bug-remote-call (bug-disable-footprint-command ,n))
		 (or (eq s 'all) (eq s 'on)))
	 (vector "Enable footprint..."
		 `(bug-remote-call (bug-enable-footprint-command ,n))
		 (or (eq s 'all) (eq s 'off))))))
  
;*---------------------------------------------------------------------*/
;*    bug-breakpoint-keymap ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-breakpoint-keymap (ov k f l n s)
  (let ((map (make-sparse-keymap)))
    (bug-overlay-put ov map)
    (cond
     ((eq k 'bp)
      (define-key map ude-mouse-binding
	`(lambda (event)
	   (interactive "e")
	   (bug-breakpoint-popup-menu ,f ,l ,n ',s)))
      (bug-overlay-help ov (format "Breakpoint %d" n)))
     ((eq k 'fp)
      (define-key map ude-mouse-binding
	`(lambda (event)
	   (interactive "e")
	   (bug-footprint-popup-menu ,f ,l ,n ',s)))
      (bug-overlay-help ov (format "Footprint %d" n))))))

;*---------------------------------------------------------------------*/
;*    bug-redisplay-breakpoint ...                                     */
;*---------------------------------------------------------------------*/
(defun bug-redisplay-breakpoint (o k n)
  (let ((s (aref o 3))
	(os (aref o 4)))
    (if (not (eq s os))
	;; the status has changed
	(let ((ov (aref o 5))
	      (l (aref o 2))
	      (f (aref o 0))
	      (i (bug-breakpoint-image k s)))
	  ;; we store the new status
	  (aset o 4 s)
	  (if (bug-overlayp ov) (bug-delete-overlay ov))
	  (let ((buf (bug-find-file f)))
	    (if (bufferp buf)
		(let ((ov (bug-add-margin-image-overlay buf l i)))
		  (bug-breakpoint-keymap ov k f l n s)
		  (aset o 5 ov))))))))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint ...                                               */
;*---------------------------------------------------------------------*/
(defun bug-breakpoint (kind bnum bfile bclass bline bstatus)
  ;; first, we try to retreive the breakpoint
  (let ((o (bug-find-breakpoint bnum)))
    (if (eq bstatus 'del)
	(if o
	    (progn
	      (bug-undisplay-breakpoint o)
	      (bug-del-breakpoint bnum)))
      (let ((b (or o (bug-make-breakpoint bnum bfile bclass bline bstatus))))
	(aset b 3 bstatus)
	(bug-redisplay-breakpoint b kind bnum)))))
