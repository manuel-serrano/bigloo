;;; etags-add.el --- additional functionality for etags

;; Copyright (C) 1985, 1986, 1988, 1989, 1992, 1993, 1994, 1995
;;	Free Software Foundation, Inc.

;; Author: Roland McGrath <roland@gnu.ai.mit.edu>
;; Keywords: tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:
(provide 'etags-add)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require bmacs-etags)

;;;###autoload
(defun find-tag-other-frame-if-new (tagname &optional next-p regexp-p)
  "Find tag (in current tags table) whose name contains TAGNAME.
Select the buffer containing the tag's definition in another window, and
move point there.  The default for TAGNAME is the expression in the buffer
around or before point.

If second arg NEXT-P is t (interactively, with prefix arg), search for
another tag that matches the last tagname or regexp used.  When there are
multiple matches for a tag, more exact matches are found first.  If NEXT-P
is negative (interactively, with prefix arg that is a negative number or
just \\[negative-argument]), pop back to the previous tag gone to.

See documentation of variable `tags-file-name'."
  (interactive (find-tag-interactive "Find tag other window: "))
  ;; This hair is to deal with the case where the tag is found in the
  ;; selected window's buffer; without the hair, point is moved in both
  ;; windows.  To prevent this, we save the selected window's point before
  ;; doing find-tag-noselect, and restore it after.
  (let* ((window-point (window-point (selected-window)))
	 (tagbuf (find-tag-noselect tagname next-p regexp-p))
	 (tagpoint (progn (set-buffer tagbuf) (point))))
    (set-window-point (prog1
			  (selected-window)
			(if (not (let ((win (get-buffer-window tagbuf t)))
				   (and win
					(let ((frame (window-frame win)))
					  (raise-frame frame)
					  (select-frame frame)
					  (select-window win)
					  (set-buffer tagbuf)
					  (goto-char tagpoint)
					  (set-window-point win tagpoint)
					  t))))
			    (let ((pop-up-frames t))
			      (switch-to-buffer-other-window tagbuf)))
			;; We have to set this new window's point; it
			;; might already have been displaying a
			;; different portion of tagbuf, in which case
			;; switch-to-buffer-other-window doesn't set
			;; the window's point from the buffer.
			(set-window-point (selected-window) tagpoint))
		      window-point)))
