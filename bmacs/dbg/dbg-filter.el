;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-filter.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 20 16:21:44 1998                          */
;*    Last change :  Tue Sep 20 06:06:22 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The dbg output filter.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-filter)
(require 'dbg-source)
(require 'dbg-breakpoint)
(require 'dbg-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    Global control variables.                                        */
;*---------------------------------------------------------------------*/
(defvar dbg-filter-defer-flag nil
  "*Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")

(defvar dbg-filter-pending-text nil
  "Non-nil means this is text that has been saved for later in `dbg-filter'.")

(defvar dbg-delete-prompt-marker nil)

(defvar dbg-last-frame nil)

(defvar dbg-last-last-frame nil)

(defvar dbg-marker-acc "")

(defvar dbg-marker-filter nil)

(defconst dbg-file:linespec-regexp "\\(\\S-+\\):\\([0-9]+\\)$")
(defconst dbg-file:linespec2-regexp "file \\(\\S-+\\) line \\([0-9]+\\).")
(defconst dbg-@-regexp "[(][@][^)]+[)]")
(defconst dbg-procedure-regexp "[#][<]procedure:\\([^.]+\\)[.][-]?[0-9]+[>]")

;*---------------------------------------------------------------------*/
;*    keymaps ...                                                      */
;*---------------------------------------------------------------------*/
(defvar dbg-linespec-mouse-map (make-sparse-keymap))
(define-key dbg-linespec-mouse-map ude-mouse-2-binding 'dbg-extent-display-linespec)

(defvar dbg-@-expr-mouse-map (make-sparse-keymap))
(define-key dbg-@-expr-mouse-map ude-mouse-2-binding 'dbg-extent-display-@-expr)

(defvar dbg-procedure-mouse-map (make-sparse-keymap))
(define-key dbg-procedure-mouse-map ude-mouse-2-binding 'dbg-extent-display-procedure)

;*---------------------------------------------------------------------*/
;*    dbg-toggle-hilit-output ...                                      */
;*---------------------------------------------------------------------*/
(defun dbg-toggle-hilit-output ()
  (interactive)
  (setq dbg-hilit-io-p (not dbg-hilit-io-p)))

(defvar *string* '())
(defvar *buffer* '())
(defvar *filter* '())
(defvar *internal* '())
(defvar *filter2* '())

;*---------------------------------------------------------------------*/
;*    dbg-split-buffer ...                                             */
;*---------------------------------------------------------------------*/
(defvar dbg-split-buffer "")

;*---------------------------------------------------------------------*/
;*    dbg-split-output ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-split-output (string)
  (setq *string* (cons string *string*))
  (setq dbg-split-buffer (concat dbg-split-buffer string))
  (if (string-match dbg-prompt-eol-regexp dbg-split-buffer)
      (let ((buffer dbg-split-buffer))
	(setq *buffer* (cons dbg-split-buffer *buffer*))
	(setq dbg-split-buffer "")
	(dbg-split-output-internal buffer))
    ""))

;*---------------------------------------------------------------------*/
;*    dbg-io-state ...                                                 */
;*---------------------------------------------------------------------*/
(defvar dbg-io-state 'source)

;*---------------------------------------------------------------------*/
;*    dbg-gdb-io-start-p ...                                           */
;*    -------------------------------------------------------------    */
;*    Is a string a gdb IO starter?                                    */
;*---------------------------------------------------------------------*/
(defun dbg-gdb-io-start-p (string)
  (and (stringp dbg-dbg-io-start-regexp)
       (string-match dbg-dbg-io-start-regexp string)))

;*---------------------------------------------------------------------*/
;*    dbg-gdb-io-stop-p ...                                            */
;*---------------------------------------------------------------------*/
(defun dbg-gdb-io-stop-p (string)
  (and (stringp dbg-dbg-io-stop-regexp)
       (string-match dbg-dbg-io-stop-regexp string)))

;*---------------------------------------------------------------------*/
;*    dbg-source-io-text ...                                           */
;*---------------------------------------------------------------------*/
(defun dbg-source-io-text (string start end)
  (if dbg-hilit-io-p
      (insert-text-property start end 'face 'dbg-source-io-face string))
  (if (and dbg-hilit-io-p (string-match comint-prompt-regexp string))
      (insert-text-property (match-beginning 0) (match-end 0)
			 'face 'dbg-prompt-face
			 string)
    ;; we will walk thru the entire output string
    (let ((len    (length string))
	  (i      0))
      (while (< i len)
	(cond
	 ;; we search for a <procedure:????.--> expression
	 ((and dbg-hilit-io-p (string-match dbg-procedure-regexp string i))
	  (let ((beg (match-beginning 0))
		(end (match-end 0)))
	    ;; prepare the extent in the dbg buffer
	    (insert-text-property beg end 'mouse-face 'highlight string)
	    (insert-text-property beg end 'keymap dbg-procedure-mouse-map string)
	    (insert-text-property beg end
			       'procedure
			       (substring string
					  (match-beginning 1)
					  (match-end 1))
			       string)
	    (setq i end)))
	 (t
	  (setq i len))))))
  string)

;*---------------------------------------------------------------------*/
;*    dbg-dbg-io-text ...                                              */
;*---------------------------------------------------------------------*/
(defun dbg-dbg-io-text (string start end)
  (if dbg-hilit-io-p
      (insert-text-property start end 'face 'dbg-dbg-io-face string))
  ;; we will walk thru the entire output string
  (let ((len    (length string))
	(i      0))
    (while (< i len)
      (cond
       ;; we search for LINE-SPEC (...:...)
       ((and dbg-hilit-io-p (string-match dbg-file:linespec-regexp string i))
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  ;; prepare the extent in the dbg buffer
	  (insert-text-property beg end 'mouse-face 'highlight string)
	  (insert-text-property beg end 'keymap dbg-linespec-mouse-map string)
	  (insert-text-property beg end
			     'filename
			     (substring string
					(match-beginning 1)
					(match-end 1))
			     string)
	  (insert-text-property beg end
			     'linespec
			     (string-to-number (substring string
						       (match-beginning 2)
						       (match-end 2)))
			     string)
	  (setq i end)))
       ;; we search for LINE-SPEC (file ..., line ...)
       ((and dbg-hilit-io-p (string-match dbg-file:linespec2-regexp string i))
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  ;; prepare the extent in the dbg buffer
	  (insert-text-property beg end 'mouse-face 'highlight string)
	  (insert-text-property beg end 'keymap dbg-linespec-mouse-map string)
	  (insert-text-property beg end
			     'filename
			     (substring string
					(match-beginning 1)
					(- (match-end 1) 1))
			     string)
	  (insert-text-property beg end
			     'linespec
			     (string-to-number (substring string
						       (match-beginning 2)
						       (match-end 2)))
			     string)
	  (setq i end)))
       ;; we search for a (@ id module) expression
       ((and dbg-hilit-io-p (string-match dbg-@-regexp string i))
	(let ((beg (match-beginning 0))
	      (end (match-end 0)))
	  ;; prepare the extent in the dbg buffer
	  (insert-text-property beg end 'mouse-face 'highlight string)
	  (insert-text-property beg end 'keymap dbg-@-expr-mouse-map string)
	  (insert-text-property beg end
			     '@-expr
			     (substring string beg end)
			     string)
	  (setq i end)))
       (t
	(setq i len)))))
  string)
  
;*---------------------------------------------------------------------*/
;*    dbg-split-output-internal ...                                    */
;*---------------------------------------------------------------------*/
(defun dbg-split-output-internal (string)
  (setq *internal* (cons string *internal*))
  (if (= (length string) 0)
      ""
    (let ((start-beg nil)
	  (start-end nil)
	  (stop-beg  nil)
	  (stop-end  nil))
      (if (dbg-gdb-io-start-p string)
	  (progn
	    (setq start-beg (match-beginning 0))
	    (setq start-end (match-end 0))))
      (if (dbg-gdb-io-stop-p string)
	  (progn
	    (setq stop-beg (match-beginning 0))
	    (setq stop-end (match-end 0))))
      (cond
       ((not (or start-beg stop-beg))
	;; this is a regular string, no marker at all
	(if (eq dbg-io-state 'source)
	    (dbg-source-io-text string 0 (length string))
	  (dbg-dbg-io-text string 0 (length string))))
       ((or (not start-beg)
	    (and stop-beg (< stop-beg start-beg)))
	;; we have found a stop marker, we mark until the mark
	(let ((pre-str (substring string 0 stop-beg))
	      (ante-str (substring string stop-end (length string))))
	  (setq dbg-io-state 'source)
	  (concat (dbg-dbg-io-text pre-str 0 stop-beg)
		  (dbg-split-output-internal ante-str))))
       (t
	;; we have found a start marker, we mark until the mark
	(setq dbg-io-state 'io)
	(let ((pre-str (substring string 0 start-beg))
	      (ante-str (substring string start-end (length string))))
	  (concat (dbg-source-io-text pre-str 0 start-beg)
		  (dbg-split-output-internal ante-str))))))))

;*---------------------------------------------------------------------*/
;*    dbg-default-marker-filter ...                                    */
;*---------------------------------------------------------------------*/
(defun dbg-default-marker-filter (string)
  (setq *filter* (cons string *filter*))
  (setq dbg-marker-acc (concat dbg-marker-acc string))
  (let ((output ""))
    ;; Process all the complete markers in this chunk.
    (while (string-match dbg-marker-regexp dbg-marker-acc)
      ;; Extract the frame position from the marker.
      (setq dbg-last-frame
	    (cons (substring dbg-marker-acc (match-beginning 1) (match-end 1))
		  (string-to-number (substring dbg-marker-acc
					    (match-beginning 2)
					    (match-end 2)))))
      ;; Append any text before the marker to the output we're going
      ;; to return - we don't include the marker in this text.
      (setq output (concat output
			   (substring dbg-marker-acc 0 (match-beginning 0))))
      ;; Set the accumulator to the remaining text.
      (setq dbg-marker-acc (substring dbg-marker-acc (match-end 0))))
    ;; Does the remaining text look like it might end with the
    ;; beginning of another marker?  If it does, then keep it in
    ;; dbg-marker-acc until we receive the rest of it.  Since we
    ;; know the full marker regexp above failed, it's pretty simple to
    ;; test for marker starts.
    (cond
     ((string-match "\032.*\\'" dbg-marker-acc)
      ;; Everything before the potential marker start can be output.
      (setq output (concat output (substring dbg-marker-acc
					     0 (match-beginning 0))))
      ;; Everything after, we save, to combine with later input.
      (setq dbg-marker-acc (substring dbg-marker-acc (match-beginning 0))))
     (t
      (setq output (concat output dbg-marker-acc))
      (setq dbg-marker-acc "")))
    (dbg-split-output output)))

;*---------------------------------------------------------------------*/
;*    dbg-filter ...                                                   */
;*---------------------------------------------------------------------*/
(defvar *debug-proc* nil)
(defvar *debug-output* nil)
(defun dbg-filter (proc string)
  (setq *filter2* (cons string *filter2*))
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if dbg-filter-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later.
	    (setq dbg-filter-pending-text
		  (concat (or dbg-filter-pending-text "") string))
	  ;; If we have to ask a question during the processing,
	  ;; defer any additional text that comes from the debugger
	  ;; during that time.
	  (let ((dbg-filter-defer-flag t))
	    ;; Process now any text we previously saved up.
	    (if dbg-filter-pending-text
		(progn
		  (setq string (concat dbg-filter-pending-text string))
		  (setq dbg-filter-pending-text nil)))
	    (save-excursion
	      (set-buffer (process-buffer proc))
	      ;; If we have been so requested, delete the debugger prompt.
	      (if (marker-buffer dbg-delete-prompt-marker)
		  (progn
		    (delete-region (process-mark proc)
				   dbg-delete-prompt-marker)
		    (set-marker dbg-delete-prompt-marker nil)))
	      ;; Save the process output, checking for source file markers.
	      (setq output (funcall dbg-marker-filter string))
;* 	      (message-box (format "marker-filter: %S -> %S" string output)) */
	      ;; Check for a filename-and-line number.
	      ;; Don't display the specified file
	      ;; unless (1) point is at or after the
	      ;; position where output appears
	      ;; and (2) this buffer is on the screen.
	      (setq process-window
		    (and dbg-last-frame
			 (>= (point) (process-mark proc))
			 (get-buffer-window (current-buffer))))
	      (setq *debug-proc* proc)
	      (setq *debug-output* output)
	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
;* 	      (message (format "dbg-filter: %S %S" proc output))       */
	      (comint-output-filter proc output)))
	  ;; Put the arrow on the source line.
	  ;; This must be outside of the save-excursion
	  ;; in case the source file is our current buffer.
	  (if process-window
	      (save-selected-window
		(select-window process-window)
		(dbg-display-frame))
	    ;; We have to be in the proper buffer, (process-buffer proc),
	    ;; but not in a save-excursion, because that would restore point.
	    (let ((old-buf (current-buffer)))
	      (set-buffer (process-buffer proc))
	      (unwind-protect
		  (dbg-display-frame)
		(set-buffer old-buf))))
	  ;; If we deferred text that arrived during this processing,
	  ;; handle it now.
	  (if dbg-filter-pending-text
	      (dbg-filter proc ""))))))

;*---------------------------------------------------------------------*/
;*    dbg-display-frame ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is called when a new source line is hitted and     */
;*    as to be prompted.                                               */
;*---------------------------------------------------------------------*/
(defun dbg-display-frame ()
  (interactive)
  (if (and dbg-last-frame (not (equal dbg-last-frame dbg-last-last-frame)))
      (progn
	(setq dbg-last-last-frame dbg-last-frame)
	(dbg-display-source-line (car dbg-last-frame) (cdr dbg-last-frame))
	(setq dbg-last-frame nil))))

;*---------------------------------------------------------------------*/
;*    dbg-extent-display-linespec ...                                  */
;*---------------------------------------------------------------------*/
(defun dbg-extent-display-linespec (event)
  (interactive "e")
  (let ((point  (event-closest-point event))
	(buffer (event-buffer event)))
    (if (numberp point)
	(save-excursion
	  (let ((file (find-text-property point 'filename buffer))
		(linespec (find-text-property point 'linespec buffer)))
	    (if (and (stringp file) (numberp linespec))
		(dbg-display-source-line file linespec)))))))

;*---------------------------------------------------------------------*/
;*    dbg-extent-display-@-expr ...                                    */
;*---------------------------------------------------------------------*/
(defun dbg-extent-display-@-expr (event)
  (interactive "e")
  (let ((point  (event-closest-point event))
	(buffer (event-buffer event)))
    (if (numberp point)
	(save-excursion
	  (let ((@-expr (find-text-property point '@-expr buffer)))
	    (if (stringp @-expr)
		(dbg-silent-remote-call (concat "binfo line " @-expr))))))))

;*---------------------------------------------------------------------*/
;*    dbg-extent-display-procedure ...                                 */
;*---------------------------------------------------------------------*/
(defun dbg-extent-display-procedure (event)
  (interactive "e")
  (let ((point  (event-closest-point event))
	(buffer (event-buffer event)))
    (if (numberp point)
	(save-excursion
	  (let ((proc (find-text-property point 'procedure buffer)))
	    (if (stringp proc)
		(dbg-silent-remote-call (concat "info line *0x" proc))))))))
	    
	    
  
