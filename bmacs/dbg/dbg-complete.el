;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-complete.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Rick Sladkey <jrs@world.std.com>                  */
;*    Creation    :  Sun Apr 19 07:19:00 1998                          */
;*    Last change :  Tue Sep 30 16:11:50 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bdb completion.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-complete)
(require 'comint)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require bmacs-etags)
(require 'dbg)
(require 'dbg-mode)
(require 'dbg-filter)

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
;; The completion process filter indicates when it is finished.
(defvar dbg-complete-in-progress)

;; Since output may arrive in fragments we accumulate partials strings here.
(defvar dbg-complete-string)

;; We need to know how much of the completion to chop off.
(defvar dbg-complete-break)

;; The completion list is constructed by the process filter.
(defvar dbg-complete-list)

;*---------------------------------------------------------------------*/
;*    dbg-complete-command ...                                         */
;*    -------------------------------------------------------------    */
;*    This command is invoked when TAB is pressed. We here use the     */
;*    dbg completion to find what to do.                               */
;*---------------------------------------------------------------------*/
(defun dbg-complete-command ()
  "Perform completion on the debugger command preceding point.
This is implemented using the debugger `complete' command." 
  (interactive)
  (let* ((end (point))
	 (command (save-excursion
		    (beginning-of-line)
		    (and (looking-at comint-prompt-regexp)
			 (goto-char (match-end 0)))
		    (buffer-substring (point) end)))
	 command-word)
    ;; Find the word break.  This match will always succeed.
    (string-match "\\(\\`\\| \\)\\([^ ]*\\)\\'" command)
    (setq dbg-complete-break (match-beginning 2))
    (setq command-word (substring command dbg-complete-break))
    (message (format "completing %s..." command-word))
    ;; Temporarily install our filter function.
    (let ((dbg-marker-filter 'dbg-complete-filter))
      ;; Issue the command to DBG.
      (dbg-basic-call (concat dbg-complete-command " " command))
      (setq dbg-complete-in-progress t)
      (setq dbg-complete-string nil)
      (setq dbg-complete-list nil)
      ;; Slurp the output.
      (while dbg-complete-in-progress
	(accept-process-output (get-buffer-process dbg-comint-buffer))))
    ;; Sort the list like readline.
    (setq dbg-complete-list (sort dbg-complete-list (function string-lessp)))
    ;; Remove duplicates.
    (let ((first dbg-complete-list)
	  (second (cdr dbg-complete-list)))
      (while second
	(if (string-equal (car first) (car second))
	    (setcdr first (setq second (cdr second)))
	  (setq first second
		second (cdr second)))))
    ;; Add a trailing single quote if there is a unique completion
    ;; and it contains an odd number of unquoted single quotes.
    (and (= (length dbg-complete-list) 1)
	 (let ((str (car dbg-complete-list))
	       (pos 0)
	       (count 0))
	   (while (string-match "\\([^'\\]\\|\\\\'\\)*'" str pos)
	     (setq count (1+ count)
		   pos (match-end 0)))
	   (and (= (mod count 2) 1)
		(setq dbg-complete-list (list (concat str "'"))))))
    (let ((complete-list (mapcar 'downcase dbg-complete-list)))
      ;; Let comint handle the rest.
      (comint-dynamic-simple-complete command-word complete-list))))

;*---------------------------------------------------------------------*/
;*    dbg-complete-filter ...                                          */
;*    -------------------------------------------------------------    */
;*    The output filter for the completion.                            */
;*    The completion process filter is installed temporarily to slurp  */
;*    the output of debugger up to the next prompt and build the       */
;*    completion list.                                                 */
;*---------------------------------------------------------------------*/
(defun dbg-complete-filter (string)
  (setq string (concat dbg-complete-string string))
  (while (string-match "\n" string)
    (setq dbg-complete-list
	  (cons (substring string dbg-complete-break (match-beginning 0))
		dbg-complete-list))
    (setq string (substring string (match-end 0))))
  (if (string-match comint-prompt-regexp string)
      (progn
	(setq dbg-complete-in-progress nil)
	string)
    (progn
      (setq dbg-complete-string string)
      "")))

