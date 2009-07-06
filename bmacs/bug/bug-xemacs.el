;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-xemacs.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  3 07:34:10 2002                          */
;*    Last change :  Thu May 23 16:35:49 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The XEmacs specific part of the debugger                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-xemacs)
(require 'bug-custom)

;*---------------------------------------------------------------------*/
;*    Xemacs settings                                                  */
;*---------------------------------------------------------------------*/
;; margin width
(defcustom bug-xemacs-margin-width 3
  "The widh of the left bug margin."
  :group 'bug
  :type 'number)

;*---------------------------------------------------------------------*/
;*    bug-xemacs-get-margin-width ...                                  */
;*    -------------------------------------------------------------    */
;*    This function computes the size in character of the margin       */
;*    width. Smaller the characters are, bigger the margin width       */
;*    is. A tool small margin will prevent pixmap to appear when       */
;*    inserted. The use of this function is not totally satisfactory   */
;*    because it does not constitute a global solution to the          */
;*    margin width problem. If user resize emacs during a session,     */
;*    it may happens that margin icons appear or disappear.            */
;*---------------------------------------------------------------------*/
(defun bug-xemacs-get-margin-width ()
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
      bug-xemacs-margin-width)))

;*---------------------------------------------------------------------*/
;*    bug-toggle-margin-on ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-toggle-margin-on (buffer)
  ;; we set the correct left margin width
  (add-spec-to-specifier left-margin-width
			 (bug-xemacs-get-margin-width)
			 buffer)
  ;; we set the margin so that pixmap are always displayed
  ;; even if they are larger that the current bufferfer font
  (add-spec-to-specifier use-left-overflow t buffer)
  ;; and the correct color
  (set-face-background 'left-margin bug-margin-color))

;*---------------------------------------------------------------------*/
;*    bug-toggle-margin-off ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-toggle-margin-off (buffer)
  (remove-specifier left-margin-width buffer)
  (sit-for 0))

;*---------------------------------------------------------------------*/
;*    bug-margin-off-p ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-margin-off-p ()
  (= (specifier-instance left-margin-width) 0))

;*---------------------------------------------------------------------*/
;*    bug-xemacs-source-line-extent ...                                */
;*---------------------------------------------------------------------*/
(defvar bug-xemacs-source-line-extent nil)

;*---------------------------------------------------------------------*/
;*    The arrow glyph, extent and face.                                */
;*---------------------------------------------------------------------*/
(defvar bug-xemacs-arrow-glyph
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
  "The glyph to be use for arrow display.")

;*---------------------------------------------------------------------*/
;*    bug-unshow-line ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-unshow-line ()
  (if (extentp bug-xemacs-source-line-extent)
      (delete-extent bug-xemacs-source-line-extent)))

;*---------------------------------------------------------------------*/
;*    bug-show-line ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-show-line (window buffer line)
  (set-buffer buffer)
  (let ((extent bug-xemacs-source-line-extent))
    ;; unshow the currently shown line
    (if (and extent (extent-live-p extent))
	(progn
	  (not (eq (extent-object extent) buffer))
	  (setq extent (delete-extent extent))))
    ;; show the new line
    (if (or (not extent) (not (extent-live-p extent)))
	(progn
	  (setq extent (make-extent 1 1 buffer))
	  (set-extent-face extent 'bug-line-face)
	  (set-extent-begin-glyph extent bug-xemacs-arrow-glyph)
	  (set-extent-begin-glyph-layout extent 'outside-margin)
	  (setq bug-xemacs-source-line-extent extent)))
    (let (pos)
      (save-excursion
	(save-restriction
	  (widen)
	  (goto-line line)
	  (setq pos (point))))
      (goto-char pos)
      (set-window-point window pos)
      (set-extent-endpoints extent
			    (line-beginning-position)
			    (line-end-position)))))
    
;*---------------------------------------------------------------------*/
;*    bug-mouse-event-margin-p ...                                     */
;*---------------------------------------------------------------------*/
(defun bug-mouse-event-margin-p (event)
  (and (> (window-left-margin-pixel-width (event-window event))
	  (event-x-pixel event))))

;*---------------------------------------------------------------------*/
;*    bug-add-margin-image-overlay ...                                 */
;*---------------------------------------------------------------------*/
(defun bug-add-margin-image-overlay (buffer line image)
  (save-excursion
    (set-buffer buffer)
    (goto-line line)
    (let ((o (make-extent (line-beginning-position)
			  (1+ (line-beginning-position))
			  buffer)))
      (set-extent-begin-glyph o image)
      (set-extent-begin-glyph-layout o 'outside-margin)
      (set-extent-property o 'mouse-face 'highlight)
      o)))

;*---------------------------------------------------------------------*/
;*    bug-add-image-overlay ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-add-image-overlay (buffer char image)
  (save-excursion
    (set-buffer buffer)
    (goto-char char)
    (let ((o (make-extent (line-beginning-position)
			  (1+ (line-beginning-position))
			  buffer)))
      (set-extent-begin-glyph o image)
      (set-extent-property o 'mouse-face 'highlight)
      o)))

;*---------------------------------------------------------------------*/
;*    bug-overlay-put ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-overlay-put (ov map)
  (set-extent-property ov 'keymap map))

;*---------------------------------------------------------------------*/
;*    bug-overlayp ...                                                 */
;*---------------------------------------------------------------------*/
(defun bug-overlayp (ov)
  (extentp ov))

;*---------------------------------------------------------------------*/
;*    bug-delete-overlay ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-delete-overlay (ov)
  (delete-extent ov))

;*---------------------------------------------------------------------*/
;*    bug-overlay-help ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-overlay-help (ov msg)
  '())

;*---------------------------------------------------------------------*/
;*    replace-regexp-in-string ...                                     */
;*---------------------------------------------------------------------*/
(defun replace-regexp-in-string (regexp rep string &optional fixedcase literal subexp start)
  (replace-in-string string regexp rep))

;*---------------------------------------------------------------------*/
;*    match-string-no-properties ...                                   */
;*---------------------------------------------------------------------*/
(defun match-string-no-properties (num &optional string)
  (match-string num string))
