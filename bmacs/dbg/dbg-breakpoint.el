;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-breakpoint.el          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 22 06:12:31 1998                          */
;*    Last change :  Tue Sep 30 16:11:40 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bdb breakpoint handling.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-breakpoint)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'comint)
(require bmacs-etags)
(require 'dbg-autoload)
(require 'dbg)
(require 'dbg-mode)
(require 'dbg-filter)
(require 'font-lock)

;*---------------------------------------------------------------------*/
;*    The breakpoint glyphs                                            */
;*---------------------------------------------------------------------*/
(defvar dbg-red-bp-glyph
  (ude-make-glyph
   (concat
    "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       13            1\",
/* colors */
\". c #000000\",
\"# c #202020\",
\"a c #303060\",
\"b c #6064c8\",
\"c c #800000\",
\"d c #808080\",
\"e c #9898f8\",
\"f c " dbg-margin-color "\",
\"g c #f80000\",
\"h c #f86430\",
\"i c #f864c8\",
\"j c #f8fc00\",
\"k c #f8fcf8\",
/* pixels */
\"ffffbbbbbbbbabdffffff\",
\"ffabfkfa.afekbaa.afff\",
\"ffabkfcgccdekaee.afff\",
\"fffbkfighg.ekbfcgffff\",
\"fffbkcfggc#ekbbbcffff\",
\"ffabkeccc#eekaaaaafff\",
\"ffabkea...aekbee.afff\",
\"fffbkhddaaaekeb.aafff\",
\"fffbkbddabbekbaaaffff\",
\"ffabkedaaaeekbaaa.fff\",
\"ffdbkb#a.#aekee.aafff\",
\"fffbkbaaaa.ekdaaabdff\",
\"fffbeeeaa.dekbaabbfff\",
\"fffffaaebbbbababbdfff\",
};"))
  "The glyph to be use for red breakpoint display.")

(defvar dbg-orange-bp-glyph
  (ude-make-glyph
   (concat
    "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       13            1\",
/* colors */
\". c #000000\",
\"# c #202020\",
\"a c #303060\",
\"b c #6064c8\",
\"c c #800000\",
\"d c #808080\",
\"e c #9898f8\",
\"f c " dbg-margin-color "\",
\"g c #f80000\",
\"h c #f86430\",
\"i c #f864c8\",
\"j c #f8fc00\",
\"k c #f8fcf8\",
/* pixels */
\"ffffbbbbbbbbabdffffff\",
\"ffabkea..#eekaaaaafff\",
\"fffbkdddaaaekeb.aafff\",
\"fffbkbddabbekbaaaffff\",
\"ffabfkaa.afekbaa.afff\",
\"ffabkfhhccdekaee.afff\",
\"fffbkfghhh.ekbe.h.fff\",
\"fffbkfgghh.ekbfhhffff\",
\"fffbkchhhc#ekbbbhffff\",
\"ffabkedaaaeekbaaa.fff\",
\"ffdbkb#a.#aekee.aafff\",
\"fffbkbaaa#.ekddaadbff\",
\"fffbeeeaa.dekbaabbfff\",
\"fffffaaebbbbababbdfff\",
};"))
  "The glyph to be use for red breakpoint display.")

(defvar dbg-green-bp-glyph
  (ude-make-glyph
   (concat 
    "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       13            1\",
/* colors */
\". c #000000\",
\"# c #202020\",
\"a c #303060\",
\"b c #6064c8\",
\"c c #008000\",
\"d c #808080\",
\"e c #9898f8\",
\"f c " dbg-margin-color "\",
\"g c #00f800\",
\"h c #64f830\",
\"i c #f864c8\",
\"j c #00fc00\",
\"k c #f8fcf8\",
/* pixels */
\"ffffbbbbbbbbabdffffff\",
\"ffabkea..#eekaaaaafff\",
\"ffabkea...aekbee.afff\",
\"fffbkdddaaaekeb.aafff\",
\"fffbkbdbbaaekefaaffff\",
\"ffabkedaaaeekbaaa.fff\",
\"ffdbkb#a.#aekee.aafff\",
\"fffbkbaaaa.ekdaaabdff\",
\"ffabfafa.afekbaa.afff\",
\"ffabkfhhccdekaee.afff\",
\"fffbkhjhhh.ekbe.c.fff\",
\"fffbkhgghh.ekbfchffff\",
\"fffbkcfghc#ekbbbcffff\",
\"fffffaaebbbbababfffff\",
};"))
  "The glyph to be use for green breakpoint display.")

(defvar dbg-warning-glyph
  (ude-make-glyph
   (concat
    "/* XPM */
static char *mini-redlight[] = {
/* width height num_colors chars_per_pixel */
\"    21    14       4            1\",
/* colors */
\"  c " dbg-margin-color "\",
\"x c #ff2020\",
\"o c black\",
\". c white\",
/* pixels */
\"         xxx         \",
\"         xxx         \",
\"        xx.xx        \",
\"        xx.xx        \",
\"       xx...xx       \",
\"       xx.o.xx       \",
\"      xx..o..xx      \",
\"      xx..o..xx      \",
\"     xx...o...xx     \",
\"     xx...o...xx     \",
\"    xx....o....xx    \",
\"    xx.........xx    \",
\"   xxxxxxxxxxxxxxx   \",
\"    xxxxxxxxxxxxx    \",
};"))
  "The glyph to be used for assertions.")

(defvar dbg-footprint-glyph
  (ude-make-glyph
   (concat 
   "/* XPM */
static char *footprint[] = {
/* width height num_colors chars_per_pixel */
\"    19    12        2            1\",
/* colors */
\"_ c " dbg-margin-color "\",
\"d c red\",
/* pixels */
\"___________dd______\",
\"___________dd_dd___\",
\"______________dd___\",
\"__________ddd______\",
\"__________dddd_dd__\",
\"__dd______dddd_dd__\",
\"__dd_dd____ddd_____\",
\"_____dd____________\",
\"_ddd_______________\",
\"_dddd_dd___________\",
\"_dddd_dd___________\",
\"__ddd______________\",
};"))
  "The glyph to be used for footprints.")

(defvar dbg-footprint-disable-glyph
  (ude-make-glyph
   (concat
    "/* XPM */
static char *footprint[] = {
/* width height num_colors chars_per_pixel */
\"    19    14        2            1\",
/* colors */
\"_ c " dbg-margin-color "\",
\"d c #00fc00\",
/* pixels */
\"___________________\",
\"___________dd______\",
\"___________dd_dd___\",
\"______________dd___\",
\"__________ddd______\",
\"__________dddd_dd__\",
\"__dd______dddd_dd__\",
\"__dd_dd____ddd_____\",
\"_____dd____________\",
\"_ddd_______________\",
\"_dddd_dd___________\",
\"_dddd_dd___________\",
\"__ddd______________\",
\"___________________\",
};"))
  "The glyph to be used for disabled footprints.")

(defface dbg-breakpoint-face
  (list (list '((class color))
	      (list ':background dbg-margin-color
		    ':bold nil))
	'(t (:bold nil)))
  "Dbg breakpoint face"
  :group 'dbg)
;*                                                                     */
;*                                                                     */
;* (make-face 'dbg-breakpoint-face)                                    */
;* (set-face-background 'dbg-breakpoint-face dbg-margin-color)         */

;*---------------------------------------------------------------------*/
;*    breakpoint keymap                                                */
;*---------------------------------------------------------------------*/
(defvar dbg-breakpoint-mouse-map (make-sparse-keymap))
(define-key dbg-breakpoint-mouse-map ude-mouse-2-binding
  (function dbg-breakpoint-update-menu))

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-breakpoint-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-breakpoint-string)

;; this variable holds a state: are we currently inquirying for breakpoints
(defvar dbg-breakpoint-command-armed-p nil)

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-hook ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-hook (input)
  (if (and (not dbg-breakpoint-command-armed-p)
	   (or (not (stringp input))
	       (string-match dbg-breakpoint-hook-regexp input)))
      (dbg-breakpoint-command)))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-command ...                                       */
;*    -------------------------------------------------------------    */
;*    This command refresh the breakpoint tracing.                     */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-command ()
  (interactive)
  (if (not dbg-breakpoint-command-armed-p)
      (progn
	;; we wait for comint to be ready
	(dbg-wait-for-comint "dbg-breakpoint-command" dbg-wait-timeout)
	;; we mark that breakpoint is armed not to re-enter this hook
	(setq dbg-breakpoint-command-armed-p t)
	;; Temporarily install our filter function.
	(let ((dbg-marker-filter 'dbg-breakpoint-filter))
	  ;; Issue the command to BDB.
	  (dbg-info-breakpoint-call)
	  (setq dbg-breakpoint-in-progress t)
	  (setq dbg-breakpoint-string "")
	  ;; Slurp the output.
	  (while dbg-breakpoint-in-progress
	    (accept-process-output (get-buffer-process dbg-comint-buffer))))
	;; at this point dbg-breakpoint-string is the string of all
	;; the breakpoint, we have to parse in order to find which
	;; breakpoint are still actives.
	(dbg-update-breakpoints dbg-breakpoint-string)
	(setq dbg-breakpoint-command-armed-p nil)))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-info-breakpoint-call ...                                     */
;*    -------------------------------------------------------------    */
;*    Sending a bdb command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-breakpoint-call ()
  (interactive)
  (let ((proc (get-buffer-process dbg-comint-buffer)))
    (or proc (ude-error "Current buffer has no process"))
    ;; Arrange for the current prompt to get deleted.
    (save-excursion
      (set-buffer dbg-comint-buffer)
      (goto-char (process-mark proc))
      (process-send-string proc dbg-info-break-command)
      (process-send-string proc "\n"))))

;*---------------------------------------------------------------------*/
;*    dbg-info-assert-call ...                                         */
;*    -------------------------------------------------------------    */
;*    Sending a bdb command.                                           */
;*---------------------------------------------------------------------*/
(defun dbg-info-assert-call (num)
  (interactive)
  (if (not (stringp dbg-info-assert-command))
      (ude-error "Info on assertion not supported.")
    (let ((command (format "%s %S\n" dbg-info-assert-command num))
	  (proc (get-buffer-process dbg-comint-buffer)))
      (or proc (ude-error "Current buffer has no process"))
      ;; Arrange for the current prompt to get deleted.
      (save-excursion
	(set-buffer dbg-comint-buffer)
	(goto-char (process-mark proc))
	(process-send-string proc command)))))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-filter ...                                        */
;*    -------------------------------------------------------------    */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of BDB up to the next prompt and build the completion */
;*    list.                                                            */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-filter (string)
  (setq dbg-marker-acc "")
  (setq dbg-breakpoint-string (concat dbg-breakpoint-string string))
  (if (string-match comint-prompt-regexp string)
      (setq dbg-breakpoint-in-progress nil))
  "")

;*---------------------------------------------------------------------*/
;*    dbg-assert-filter ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is not a gdb filter. It is used to remove          */
;*    the leading and ending marker of the info assert command. This   */
;*    function is used after dbg-breakpoint-filter has been used to    */
;*    filter out the result of comint.                                 */
;*---------------------------------------------------------------------*/
(defun dbg-assert-filter (string)
  (if (dbg-gdb-io-start-p string)
      (let ((start (match-end 0)))
	(if (dbg-gdb-io-stop-p string)
	    (let ((end (- (match-beginning 0) 1)))
	      (substring string start end))
	  string))
    string))
  
;*---------------------------------------------------------------------*/
;*    dbg-registered-breakpoint-list ...                               */
;*    -------------------------------------------------------------    */
;*    This variable holds the list of the bdb registered breakpoint.   */
;*    This list is sorted. That is the breakpoint of this listed are   */
;*    sorted from the older to the newer.                              */
;*---------------------------------------------------------------------*/
(defvar dbg-registered-breakpoint-list '())

;*---------------------------------------------------------------------*/
;*    dbg-update-breakpoints ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-update-breakpoints (string)
  (save-excursion
    (let ((old dbg-registered-breakpoint-list)
	  (new '()))
      ;; as long as they are breakpoint to be matched we loop
      (while (string-match dbg-breakpoint-regexp string)
	(let ((num (string-to-number
		    (substring string (match-beginning 1) (match-end 1))))
	      (enablep (eq (aref
			    (substring string
				       (match-beginning 2)
				       (match-end 2))
			    0)
			   ?y))
	      (file (substring string (match-beginning 3) (match-end 3)))
	      (line (string-to-number
		     (substring string (match-beginning 4) (match-end 4)))))
	  ;; we are done with this breakpoint
	  (setq string (substring string (match-end 0)))
	  (let* ((condp (eq (string-match
			     dbg-breakpoint-condition-regexp string)
			    0))
		 (assertp (and condp
			       (eq 
				(string-match
				 dbg-breakpoint-assert-regexp string)
				0)))
		 (fpp     nil)
		 (msg     (cond
			   (assertp
			    ;; we wait for comint to be ready
			    (dbg-wait-for-comint "dbg-update-breakpoints(2)"
						 dbg-wait-timeout)
			    ;; we mark that breakpoint is armed not
			    ;; to re-enter this hook
			    (setq dbg-breakpoint-command-armed-p t)
			    ;; Temporarily install our filter function.
			    (let ((dbg-marker-filter 'dbg-breakpoint-filter))
			      ;; Issue the command to BDB.
			      (dbg-info-assert-call num)
			      (setq dbg-breakpoint-in-progress t)
			      (setq dbg-breakpoint-string "")
			      ;; Slurp the output.
			      (while dbg-breakpoint-in-progress
				(accept-process-output
				 (get-buffer-process dbg-comint-buffer)))
			      (dbg-assert-filter dbg-breakpoint-string)))
			   (condp
			    (substring string
				       (match-beginning 1)
				       (match-end 1)))
			   (t
			    nil)))
		 (kind    (cond
			   (fpp 'footprint)
			   (assertp 'assert)
			   (condp 'cond)
			   (t nil))))
	    ;; we search in the old list the breakpoint number num. in this
	    ;; search we unregister all unreferenced breakpoint (breakpoints
	    ;; that have been deleted since the last dbg-update-breakpoints).
	    (while (and (consp old)
			(dbg-breakpoint-p (car old))
			(< (dbg-breakpoint-num (car old)) num))
	      ;; this breakpoint is not used anymore
	      (dbg-delete-breakpoint (car old))
	      (setq old (cdr old)))
	    (if (and (consp old) (dbg-breakpoint-p (car old)))
		(let ((cur (car old)))
		  (if (= (dbg-breakpoint-num cur) num)
		      (progn
			(setq old (cdr old))
			;; we test if we have to refresh the breakpoint
			(if (or (not (eq (dbg-breakpoint-enablep cur) enablep))
				(not (eq (dbg-breakpoint-kind cur) kind)))
			    ;; this is the same breakpoint but is has changed
			    (progn
			      (dbg-delete-breakpoint cur)
			      (let ((nbp (dbg-make-breakpoint num
							      enablep
							      file
							      line
							      fpp
							      condp
							      assertp
							      msg)))
				(setq new (cons nbp new))))
			  ;; the breakpoint is still used
			  ;; and has not been changed
			  (setq new (cons cur new))))))
	      (let ((nbp (dbg-make-breakpoint num
					      enablep
					      file
					      line
					      fpp
					      condp
					      assertp
					      msg)))
		(setq new (cons nbp new)))))))
      ;; we now have to remove all the remaining breakpoints
      (while (consp old)
	(dbg-delete-breakpoint (car old))
	(setq old (cdr old)))
      ;; we are now done we just have to register the new breakpoints
      (setq dbg-registered-breakpoint-list (reverse new)))))
      
;*---------------------------------------------------------------------*/
;*    dbg-delete-breakpoint ...                                        */
;*    -------------------------------------------------------------    */
;*    We make a breakpoint to disapear from the screen.                */
;*---------------------------------------------------------------------*/
(defun dbg-delete-breakpoint (bp)
  (if bp (delete-extent (dbg-breakpoint-extent bp))))

;*---------------------------------------------------------------------*/
;*    dbg-make-breakpoint ...                                          */
;*    -------------------------------------------------------------    */
;*    a bdb breakpoint is a vector that holds the following values:    */
;*      - num     : an integer    : the breakpoint number              */
;*      - enablep : a bool        : is the breakpoint enabled          */
;*      - file    : a string      : the file name of this breakpoint   */
;*      - line    : an integer    : the line number of this breakpoint */
;*      - fpp     : a bool        : is the breakpoint a footprint      */
;*      - condp   : a bool        : is the breakpoint conditional      */
;*      - assertp : a bool        : is the breakpoint an assertion     */
;*      - msg     : a string      : the expr of the breakpoint         */
;*---------------------------------------------------------------------*/
(defun dbg-make-breakpoint (num enablep file line fpp condp assertp msg)
  (save-excursion
    (let ((buffer (dbg-find-file file)))
      (if (bufferp buffer)
	  (let* ((extent (make-extent 1 1 buffer))
		 (bp (vector 'breakpoint
			     num
			     enablep
			     file
			     line
			     (cond
			      (fpp 'footprint)
			      (assertp 'assert)
			      (condp 'cond)
			      (t nil))
			     extent
			     msg)))
	    ;; we set the left margin for the bdb source buffer
	    (dbg-connect-buffer buffer)
	    (set-extent-face extent 'dbg-breakpoint-face)
	    (set-extent-begin-glyph extent (cond
					    (fpp
					     (if enablep
						 dbg-footprint-glyph
					       dbg-footprint-disable-glyph))
					    (assertp dbg-warning-glyph)
					    ((not enablep) dbg-green-bp-glyph)
					    (condp dbg-orange-bp-glyph)
					    (t dbg-red-bp-glyph)))
	    (set-extent-begin-glyph-layout extent 'outside-margin)
	    (set-extent-priority extent 1000)
	    (set-extent-property extent 'keymap dbg-breakpoint-mouse-map)
	    (set-extent-property extent 'mouse-face 'highlight)
	    (set-extent-property extent 'dbg-breakpoint bp)
	    (set-buffer buffer)
	    (goto-line line)
	    (beginning-of-line)
	    (set-extent-endpoints extent (point) (point))
	    bp)))))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-balloon-message ...                               */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-balloon-message (object)
  (if (extentp object)
      (let ((bp (extent-property object 'dbg-breakpoint)))
	(if (dbg-breakpoint-p bp)
	    (let ((str (concat (cond
				((eq (dbg-breakpoint-kind bp) 'assert)
				 "assertion: ")
				((eq (dbg-breakpoint-kind bp) 'footprint)
				 "footprint: ")
				(t
				 "breakpoint: "))
			       (number-to-string (dbg-breakpoint-num bp)))))
	      (insert-text-property 0 10
				 'face
				 'dbg-prompt-face str)
	      str)
	  nil))
    nil))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-p ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-p (bp)
  (and (vectorp bp) (= (length bp) 8) (eq (aref bp 0) 'breakpoint)))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-extent ...                                        */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-extent (bp)
  (aref bp 6))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-kind ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-kind (bp)
  (aref bp 5))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-line ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-line (bp)
  (aref bp 4))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-file ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-file (bp)
  (aref bp 3))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-enablep ...                                       */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-enablep (bp)
  (aref bp 2))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-num ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-num (bp)
  (aref bp 1))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-msg ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-msg (bp)
  (aref bp 7))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-update-menu ...                                   */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-update-menu (event &rest arg)
  (interactive "e")
  (let ((extent (event-glyph-extent event)))
    (dbg-breakpoint-update-menu/extent extent)))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-update-menu/extent ...                            */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-update-menu/extent (extent)
  (if (extentp extent)
      (let ((bp (extent-property extent 'dbg-breakpoint)))
	(if (dbg-breakpoint-p bp)
	    (save-excursion
	      (popup-menu
	       (dbg-breakpoint-update-popup-menu bp
						 (extent-end-position extent)
						 (current-buffer))))))))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-update-popup-menu ...                             */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-update-popup-menu (breakpoint pos buffer)
  "The popup menu to print when the cursor hit a breakpoint glyph."
  (let* ((msg (dbg-breakpoint-msg breakpoint))
	 (kind (cond
		((eq (dbg-breakpoint-kind breakpoint) 'assert)
		 "assertion")
		((eq (dbg-breakpoint-kind breakpoint) 'footprint)
		 "footprint")
		(t
		 "breakpoint")))
	 (rest (if (eq (dbg-breakpoint-kind breakpoint) 'assert)
		   (list (vector "Unassert"
				 (list 'dbg-remote-unassert-breakpoint
				       breakpoint)
				 t))
		 (cons (if (dbg-breakpoint-enablep breakpoint)
			   (vector (concat "Disable " kind)
				   (list 'dbg-remote-disable-breakpoint
					 breakpoint)
				   t)
			 (vector (concat "Enable " kind)
				 (list 'dbg-remote-enable-breakpoint
				       breakpoint)
				 t))
		       (list (vector (concat "Delete " kind)
				     (list 'dbg-remote-delete-breakpoint
					   breakpoint)
				     t))))))
    (if (stringp msg)
	(cons (format "%s:%d (%s %d)"
		      (dbg-breakpoint-file breakpoint)
		      (dbg-breakpoint-line breakpoint)
		      kind
		      (dbg-breakpoint-num breakpoint))
	      (append (dbg-unnewline msg) (cons "--:shadowEtchedIn" rest)))
      (cons (format "%s:%d (%s %d)"
		    (dbg-breakpoint-file breakpoint)
		    (dbg-breakpoint-line breakpoint)
		    kind
		    (dbg-breakpoint-num breakpoint))
	    rest))))

;*---------------------------------------------------------------------*/
;*    dbg-unnewline ...                                                */
;*    -------------------------------------------------------------    */
;*    This function takes a string that contains newline characters.   */
;*    It returns a list of string splitted on each newline.            */
;*---------------------------------------------------------------------*/
(defun dbg-unnewline (string)
  (let ((res '())
	(i    0)
	(len  (length string)))
    (while (string-match "\n" string i)
      (setq res (cons (substring string i (- (match-end 0) 1)) res))
      (setq i (match-end 0)))
    (reverse (cons (substring string i len) res))))

;*---------------------------------------------------------------------*/
;*    dbg-remote-disable-breakpoint ...                                */
;*---------------------------------------------------------------------*/
(defun dbg-remote-disable-breakpoint (bp)
  (interactive)
  (save-excursion
    (dbg-remote-call (format "%s %d"
			     dbg-disable-break-command
			     (dbg-breakpoint-num bp)))))

;*---------------------------------------------------------------------*/
;*    dbg-remote-enable-breakpoint ...                                 */
;*---------------------------------------------------------------------*/
(defun dbg-remote-enable-breakpoint (bp)
  (interactive)
  (save-excursion
    (dbg-remote-call (format "%s %d"
			     dbg-enable-break-command
			     (dbg-breakpoint-num bp)))))

;*---------------------------------------------------------------------*/
;*    dbg-remote-delete-breakpoint ...                                 */
;*---------------------------------------------------------------------*/
(defun dbg-remote-delete-breakpoint (bp)
  (interactive)
  (save-excursion
    (dbg-remote-call (format "%s %d"
			     dbg-delete-break-command
			     (dbg-breakpoint-num bp)))))

;*---------------------------------------------------------------------*/
;*    dbg-remote-unassert-breakpoint ...                               */
;*---------------------------------------------------------------------*/
(defun dbg-remote-unassert-breakpoint (bp)
  (interactive)
  (if (not (stringp dbg-unassert-command))
      (ude-error "Command `unassert' not supported.")
    (save-excursion
      (dbg-remote-call (format "%s %d"
			       dbg-unassert-command
			       (dbg-breakpoint-num bp))))))

;*---------------------------------------------------------------------*/
;*    dbg-breakpoint-menu ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-breakpoint-menu (event)
  (save-excursion
    (let* ((buffer (event-buffer event))
	   (file   (buffer-file-name buffer))
	   (fname  (file-name-nondirectory file))
	   (pos    (event-closest-point event))
	   (line   (dbg-line-number buffer pos)))
      (popup-menu
       (list (format "%s, line %s" fname line)
	     (vector "Set breakpoint"
		     (list 'dbg-remote-set-breakpoint
			   dbg-break-command
			   fname line)
		     t)
	     (vector "Set temporary breakpoint"
		     (list 'dbg-remote-set-breakpoint
			   dbg-tbreak-command
			   fname
			   line)
		     t)
	     "-"
	     (vector "Set footprint"
		     (list 'dbg-remote-set-breakpoint
			   dbg-footprint-command
			   fname line)
		     t)
	     "-"
	     (vector "Continue until here"
		     (list 'dbg-remote-continue fname line)
		     t))))))

;*---------------------------------------------------------------------*/
;*    dbg-remote-set-breakpoint ...                                    */
;*---------------------------------------------------------------------*/
(defun dbg-remote-set-breakpoint (break fname line)
  (let ((cmd (concat break " " fname ":" (int-to-string line))))
    (dbg-remote-call cmd)))
  
;*---------------------------------------------------------------------*/
;*    dbg-remote-continue ...                                          */
;*---------------------------------------------------------------------*/
(defun dbg-remote-continue (fname line)
  (let ((cmd (concat dbg-tbreak-command " " fname ":" (int-to-string line))))
    (dbg-remote-call cmd)
    (dbg-remote-call dbg-cont-command)))

    
  
