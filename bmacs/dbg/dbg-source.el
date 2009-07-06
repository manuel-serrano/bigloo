;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-source.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 19 15:15:26 1998                          */
;*    Last change :  Thu Feb  7 09:21:14 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module takes in charge the hilit of the current source      */
;*    code line.                                                       */
;*    -------------------------------------------------------------    */
;*    This module implements the function `gdb-display-source-line'.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-source)
(require 'dbg)
(require 'dbg-config)
(require 'dbg-autoload)

;*---------------------------------------------------------------------*/
;*    The arrow glyph, extent and face.                                */
;*---------------------------------------------------------------------*/
(defvar dbg-arrow-glyph
  (ude-make-glyph
   "/* XPM */
static char *arrow[] = {
/* width height num_colors chars_per_pixel */
\"    14    12        2            1\",
/* colors */
\". c None s None\",
\"# c #32cd32\",
/* pixels */
\"..............\",
\".........#....\",
\".........##...\",
\"#.##.#######..\",
\"#.##.########.\",
\"#.##.##########\",
\"#.##.########.\",
\"#.##.#######..\",
\".........##...\",
\".........#....\",
\"..............\",
\"..............\",
};")
  "The glyph to be use for arrow display.")

(defface dbg-arrow-face
  (list (list '((class color))
	      (list ':background dbg-margin-color
		    ':bold nil))
	'(t (:bold nil)))
  "Dbg arrow face"
  :group 'dbg)
;* (make-face 'dbg-arrow-face)                                         */
;* (set-face-background 'dbg-arrow-face dbg-margin-color)              */

(defvar dbg-stop-glyph
  (ude-make-glyph
   "/* XPM */
static char *arrow[] = {
/* width height num_colors chars_per_pixel */
\"    14    12        2            1\",
/* colors */
\". c None s None\",
\"# c #ff2222\",
/* pixels */
\"..............\",
\".........#....\",
\".........##...\",
\"#.##.#######..\",
\"#.##.########.\",
\"#.##.##########\",
\"#.##.########.\",
\"#.##.#######..\",
\".........##...\",
\".........#....\",
\"..............\",
\"..............\",
};")
  "The glyph to be use when the arrow reaches a breakpoint.")

(defface dbg-stop-face
  (list (list '((class color))
	      (list ':background dbg-margin-color
		    ':bold nil))
	'(t (:bold nil)))
  "Dbg arrow face"
  :group 'dbg)

;* (make-face 'dbg-stop-face)                                          */
;* (set-face-background 'dbg-stop-face dbg-margin-color)               */

(defvar dbg-source-line-extent nil)

;*---------------------------------------------------------------------*/
;*    arrow keymap ...                                                 */
;*    -------------------------------------------------------------    */
;*    This keymap is somekind of hack. What we want to do is that      */
;*    when the arrow is in front of a breakpoint the arrow inherit     */
;*    from the breakpoint popup menu. What we do is that, when         */
;*    displaying an arrow we check if there is a previous extent.      */
;*    If we found one, we set the variable dbg-bp-extent to that       */
;*    extent and we set the extent with a keymap that will call        */
;*    dbg-arrow-mouse-map. This last function simply call the          */
;*    breakpoint popup with the dbg-bp-extent value as parameter.      */
;*---------------------------------------------------------------------*/
(defvar dbg-arrow-mouse-map (make-sparse-keymap))
(defvar dbg-bp-extent nil)
(define-key dbg-arrow-mouse-map ude-mouse-2-binding
  (function dbg-breakpoint-update-menu))

;*---------------------------------------------------------------------*/
;*    dbg-undisplay-source-line ...                                    */
;*    -------------------------------------------------------------    */
;*    This function must be call when quitting dbg in order            */
;*    to get a clean buffers redisplay.                                */
;*---------------------------------------------------------------------*/
(defun dbg-undisplay-source-line ()
  (if (extentp dbg-source-line-extent)
      (delete-extent dbg-source-line-extent)))

;*---------------------------------------------------------------------*/
;*    dbg-highlight-source-line ...                                    */
;*---------------------------------------------------------------------*/
(defun dbg-highlight-source-line (true-file line)
  ;; true-file is the file name to display
  ;; and line is number to highlight and make visible
  (let* ((buffer (save-excursion
		   (dbg-find-file true-file)))
	 (window (and buffer
		      (if dbg-display-source-in-frame-p
			  (let ((pop-up-frames t))
			    (display-buffer buffer))
			(display-buffer buffer))))
	 (extent dbg-source-line-extent)
	 (pos))
    (if buffer
	(progn
	  ;; we set the left margin for the dbg source buffer
	  (dbg-connect-buffer buffer)
	  (if (and extent (extent-live-p extent))
	      (progn
		(not (eq (extent-object extent) buffer))
		(setq extent (delete-extent extent))))
	  (if (or (not extent) (not (extent-live-p extent)))
	      (progn
		(setq extent (make-extent 1 1 buffer))
		(set-extent-face extent 'dbg-arrow-face)
		(set-extent-begin-glyph extent dbg-arrow-glyph)
		(set-extent-begin-glyph-layout extent 'outside-margin)
		(setq dbg-source-line-extent extent)))
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (set-window-point window (point))
	      (setq pos (progn
			  (beginning-of-line)
			  (point)))
	      ;; we raise the frame displaying the source line
	      (if dbg-raise-active-source-frame-p
		  (let ((frame (window-frame window)))
		    (if (framep frame)
			(raise-frame frame))))
	      (if (and (consp (extent-list (current-buffer) (point) (point)))
		       (extent-live-p (car (extent-list (current-buffer)
							(point)
							(point)))))
		  (progn
		    ;; the arrow is shadowing a breakpoint
		    (setq dbg-bp-extent
			  (car (extent-list (current-buffer) (point) (point))))
		    (set-extent-face extent 'dbg-stop-face)
		    (set-extent-begin-glyph extent dbg-stop-glyph)
		    (set-extent-begin-glyph-layout extent 'outside-margin)
		    (set-extent-property extent 'mouse-face 'highlight)
		    (set-extent-property extent
					 'dbg-breakpoint
					 (extent-property dbg-bp-extent
							  'dbg-breakpoint))
		    (set-extent-property extent 'keymap dbg-arrow-mouse-map)))
	      (end-of-line)
	      (set-extent-endpoints extent pos (point)))
	    (if (or (< pos (point-min)) (> pos (point-max)))
		(progn
		  (widen)
		  (goto-char pos)
		  (set-window-point window pos))))
	  (goto-char pos)
	  (set-window-point window pos))
      nil)))

;*---------------------------------------------------------------------*/
;*    dbg-display-source-line ...                                      */
;*    -------------------------------------------------------------    */
;*    This function display the current source line.                   */
;*---------------------------------------------------------------------*/
(defun dbg-display-source-line (true-file line)
  ;; true-file is the file name to display
  ;; and line is number to highlight and make visible
  (let* ((buffer (save-excursion
		   (or (eq (current-buffer) dbg-comint-buffer)
		       (set-buffer dbg-comint-buffer))
		   (dbg-find-file true-file))))
    (if buffer
	(progn
	  (dbg-highlight-source-line true-file line)
	  (set-buffer dbg-comint-buffer)
	  (let ((height (window-height)))
	    (if (and (not dbg-display-source-in-frame-p)
		     (> height dbg-window-height)
		     (consp (window-list))
		     (consp (cdr (window-list))))
		(shrink-window (- height dbg-window-height))))
	  (set-buffer buffer)))))

;*---------------------------------------------------------------------*/
;*    dbg-command-region ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-command-region (command region)
  (dbg-remote-call (concat command region)))

;*---------------------------------------------------------------------*/
;*    dbg-set-buffer-margin ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-set-buffer-margin (buffer)
  ;; we set the correct left margin width
  (add-spec-to-specifier left-margin-width (dbg-get-margin-width) buffer)
  ;; we set the margin so that pixmap are always displayed
  ;; even if they are larger that the current buffer font
  (add-spec-to-specifier use-left-overflow t buffer)
  ;; and the correct color
  (set-face-background 'left-margin dbg-margin-color))

;*---------------------------------------------------------------------*/
;*    dbg-unset-buffer-margin ...                                      */
;*---------------------------------------------------------------------*/
(defun dbg-unset-buffer-margin (buffer)
  ;; we set the correct left margin width
  (remove-specifier left-margin-width buffer)
  (sit-for 0))

;*---------------------------------------------------------------------*/
;*    dbg-get-buffer-margin ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-get-buffer-margin (buffer)
  (specifier-instance left-margin-width))

;*---------------------------------------------------------------------*/
;*    dbg-get-margin-width ...                                         */
;*    -------------------------------------------------------------    */
;*    This function computes the size in character of the margin       */
;*    width. Smaller the characters are, bigger the margin width       */
;*    is. A tool small margin will prevent pixmap to appear when       */
;*    inserted. The use of this function is not totally satisfactory   */
;*    because it does not constitute a global solution to the          */
;*    margin width problem. If user resize emacs during a session,     */
;*    it may happens that margin icons appear or disappear.            */
;*---------------------------------------------------------------------*/
(defun dbg-get-margin-width ()
  ;; I reckon this code is not portable. I'm not sure it is legal
  ;; to use font-default-size-for-device. If it appears that this
  ;; function does not exist on some system I will try best later on
  (let ((size (font-default-size-for-device)))
    (if (numberp size)
	;; we make a kind of translation from font size to
	;; margin size. The general idea is that for a font
	;; size of 12 pt, the margin is 3 chars, for a font
	;; size of 10 pt, it is 4 chars
	(cond
	 ((<= size 8) 5)
	 ((<= size 10) 4)
	 ((<= size 14) 3)
	 ((<= size 20) 2)
	 (t 1))
      dbg-margin-width)))

