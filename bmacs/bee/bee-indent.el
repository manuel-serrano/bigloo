;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-indent.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:27:11 1998                          */
;*    Last change :  Wed Dec 23 08:14:04 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bee indent (this file is adapted from the Scheme mode by     */
;*    Bill Rozas).                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-indent)
(require 'ude-autoload)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-config)
(require 'bee-mode)

;*---------------------------------------------------------------------*/
;*    bee-external-indent-process ...                                  */
;*---------------------------------------------------------------------*/
(defvar bee-external-indent-process '()
  "The external Bee indent process")

;*---------------------------------------------------------------------*/
;*    bee-external-indent-sentinel ...                                 */
;*---------------------------------------------------------------------*/
(defun bee-external-indent-sentinel (proc msg)
  (if (equal (substring msg 0 8) "finished")
      (progn
	(message "Indent done ...")
	(goto-char (point-min)))
    (progn
      (beep)
      (message "Abnormal end: %s" msg ))))

;*---------------------------------------------------------------------*/
;*    bee-external-indent ...                                          */
;*---------------------------------------------------------------------*/
(defun bee-external-indent ()
  "Call an external global indent on current buffer"
  (interactive)
  (cond
   ((buffer-modified-p)
    (ude-error "Can't Indent modified buffers ..."))
   (t
    (goto-char (point-max))
    (kill-region 1 (point))
    (message "Indenting Scheme file")
    (setq bee-external-indent-process
	  (start-process "indent"
			 (buffer-name)
			 bee-external-indent
			 (format "-w%d" (- (frame-width) 1))
			 (buffer-file-name)))
    (set-process-sentinel bee-external-indent-process
			  'bee-external-indent-sentinel))))

;* {*---------------------------------------------------------------------*} */
;* {*    bee-fill-paragraph-function ...                                  *} */
;* {*---------------------------------------------------------------------*} */
;* (defun bee-fill-paragraph-function (justify)                        */
;*   'todo)                                                            */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    bee-comment-indent ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-comment-indent (&optional pos)
  (save-excursion
    (if pos (goto-char pos))
    (cond
     ((looking-at ";;;")
      (current-column))
     ((looking-at ";\\*")
      0)
     ((looking-at "[ \t]*;;")
      (let ((tem (bee-calculate-indent)))
	(if (listp tem) (car tem) tem)))
     (t
      (skip-chars-backward " \t")
      (max (if (bolp) 0 (1+ (current-column)))
	   comment-column)))))

;*---------------------------------------------------------------------*/
;*    bee-indent-offset ...                                            */
;*---------------------------------------------------------------------*/
(defvar bee-indent-offset nil "")

;*---------------------------------------------------------------------*/
;*    bee-indent-hook ...                                              */
;*---------------------------------------------------------------------*/
(defvar bee-indent-hook 'bee-indent-hook "")

;*---------------------------------------------------------------------*/
;*    bee-indent-line ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-indent-line (&optional whole-exp)
  "Indent current line as Bigloo code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (bee-calculate-indent)) shift-amt beg end
        (pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (or (looking-at ";;;")
            (looking-at ";\\*")
	    (looking-at ";[*]"))
	;; Don't alter indentation of a ;;; or a ;* comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
          nil
        (delete-region beg (point))
        (indent-to indent))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
           (save-excursion
             (goto-char beg)
             (forward-sexp 1)
             (setq end (point))
             (goto-char beg)
             (forward-line 1)
             (setq beg (point))
             (> end beg))
           (indent-code-rigidly beg end shift-amt)))))

;*---------------------------------------------------------------------*/
;*    bee-calculate-indent ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-calculate-indent (&optional parse-start)
  "Return appropriate indentation for current line as Bigloo code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (or (bee-calculate-forced-indent)
      (bee-calculate-unforced-indent parse-start)))

;*---------------------------------------------------------------------*/
;*    bee-calculate-forced-indent ...                                  */
;*    -------------------------------------------------------------    */
;*    Returns a column number iff the line indentation is forced       */
;*    (i.e. the previous line starts with a "[ \t]*;;;"). Otherwise    */
;*    returns f.                                                       */
;*---------------------------------------------------------------------*/
(defun bee-calculate-forced-indent ()
  (when (> (count-lines 1 (point)) 1)
    (save-excursion
      (previous-line 1)
      (beginning-of-line)
      (skip-chars-forward " \t")
      (let ((s (current-column)))
	(and (looking-at bee-forced-indent-regexp) s)))))

;*---------------------------------------------------------------------*/
;*    bee-calculate-unforced-indent ...                                */
;*---------------------------------------------------------------------*/
(defun bee-calculate-unforced-indent (&optional parse-start)
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point)) state paren-depth desired-indent (retry t)
          last-sexp containing-sexp first-sexp-list-p)
      (if parse-start
          (goto-char parse-start)
        (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
        (setq retry nil)
        (setq last-sexp (nth 2 state))
        (setq containing-sexp (car (cdr state)))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and last-sexp (> last-sexp (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
              (if (setq retry (car (cdr peek))) (setq state peek))))
        (if (not retry)
            ;; Innermost containing sexp found
            (progn
              (goto-char (1+ containing-sexp))
              (if (not last-sexp)
                  ;; indent-point immediately follows open paren.
                  ;; Don't call hook.
                  (setq desired-indent (current-column))
                ;; Move to first sexp after containing open paren
                (parse-partial-sexp (point) last-sexp 0 t)
                (setq first-sexp-list-p (looking-at "\\s("))
                (cond
                 ((> (save-excursion (forward-line 1) (point)) last-sexp)
                  ;; Last sexp is on same line as containing sexp.
                  ;; It's almost certainly a function call.
                  (parse-partial-sexp (point) last-sexp 0 t)
		  (if (/= (point) last-sexp)
		      (if (eq bee-indent-style 'TODO)
			  (setq desired-indent (- (+ (current-column) bee-body-indent) 1))
			;; Indent beneath first argument or, if only one sexp
			;; on line, indent beneath that.
			(progn (forward-sexp 1)
			       (parse-partial-sexp (point) last-sexp 0 t))))
		  (backward-prefix-chars))
                 (t
                  ;; Indent beneath first sexp on same line as last-sexp.
                  ;; Again, it's almost certainly a function call.
                  (goto-char last-sexp)
                  (beginning-of-line)
                  (parse-partial-sexp (point) last-sexp 0 t)
                  (backward-prefix-chars)))))))
      ;; If looking at a list, don't call hook.
      (if first-sexp-list-p
          (setq desired-indent (current-column)))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by bee-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond ((car (nthcdr 3 state))
             ;; Inside a string, don't change indentation.
             (goto-char indent-point)
             (skip-chars-forward " \t")
             (setq desired-indent (current-column)))
	    ((bee-indent-quote-p state)
	     (save-excursion
	       (goto-char (+ (car (nthcdr 1 state)) 1))
	       (setq desired-indent (current-column))))
	    ((bee-indent-brace-p state)
	     (setq desired-indent 0))
            ((and (integerp bee-indent-offset) containing-sexp)
             ;; Indent by constant offset
             (goto-char containing-sexp)
             (setq desired-indent (- bee-indent-offset (current-column))))
	    ((and bee-indent-on-keyword-p
		  (looking-at ":[^:]+")
		  (let ((pos (point)))
		    (save-excursion
		      (beginning-of-line)
		      (skip-chars-forward " \t")
		      (not (= (point) pos)))))
	     ;; We are at a keyword position, we backward sexp until we are
	     ;; no longer located on a keyword
	     (while (looking-at ":[^:]+")
	       (backward-sexp 1))
	     (setq desired-indent (- (+ (current-column) bee-body-indent) 1)))
	    ((in-condp state)
	     (save-excursion
	       (goto-char (cadr state))
	       (setq desired-indent (1+ (current-column)))))
            ((not (or desired-indent
                      (and (boundp 'bee-indent-hook)
                           bee-indent-hook
                           (not retry)
                           (setq desired-indent
                                 (funcall bee-indent-hook
                                          indent-point state)))))
             ;; Use default indentation if not computed yet
             (setq desired-indent (current-column))))
      desired-indent)))

;*---------------------------------------------------------------------*/
;*    in-condp ...                                                     */
;*---------------------------------------------------------------------*/
(defun in-condp (state)
  (and (consp state)
       (>= (car state) 2)
       (let* ((conts (nth 9 state))
	      (pos (nth (- (length conts) 2) conts)))
	 (save-excursion
	   (goto-char (1+ pos))
	   (looking-at cond-regexp)))))

;*---------------------------------------------------------------------*/
;*    in-modulep ...                                                   */
;*---------------------------------------------------------------------*/
(defun in-modulep (state)
  (and (>= (car state) 2)
       (let* ((conts (nth 9 state))
	      (pos (nth (- (length conts) 2) conts)))
	 (save-excursion
	   (goto-char (1+ pos))
	   (looking-at "\\(module\\|directives\\)[\t\n ]")))))

;*---------------------------------------------------------------------*/
;*    normal-indent ...                                                */
;*---------------------------------------------------------------------*/
(defvar normal-indent 0)

;*---------------------------------------------------------------------*/
;*    bee-indent-hook ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-indent-hook (indent-point state)
  (let ((normal-indent (current-column)))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (point) (car (cdr state)))
          (let ((function (buffer-substring (progn (forward-char -1) (point))
                                            (progn (forward-sexp 1) (point))))
                method)
            ;; Who cares about this, really?
	    ;; (if (not (string-match "\\\\\\||" function)))
            (setq function (downcase function))
            (setq method (get (intern-soft function) 'bee-indent-hook))
            (cond
	     ((integerp method)
	      (if (< method 0)
		  '()
		(bee-indent-specform method state indent-point)))
	     (method
	      (funcall method state indent-point))
	     ((and (> (length function) 3)
		   (string-equal (substring function 0 3) "def"))
	      (bee-indent-defform state indent-point))
	     ((and (> (length function) 13)
		   (string-equal (substring function 0 13) "with-access::"))
	      (bee-with-access-indent state indent-point))
	     ((and (> (length function) 13)
		   (string-equal (substring function 0 13) "instantiate::"))
	      (bee-instantiate-indent state indent-point))
	     ((and (> (length function) 11)
		   (string-equal (substring function 0 11) "duplicate::"))
	      (bee-duplicate-indent state indent-point))
	     ((and (> (length function) 11)
		   (string-equal (substring function 0 8) "widen!::"))
	      (bee-duplicate-indent state indent-point))
	     (t
	      (bee-indent-defform state indent-point))))))))

;*---------------------------------------------------------------------*/
;*    bee-module-indent-hook ...                                       */
;*---------------------------------------------------------------------*/
(defun bee-module-indent-hook (state point)
  (if (in-modulep state)
      (save-excursion
	(if (= (1+ (cadr state)) (cadr (cdr state)))
	    (progn
	      (goto-char (cadr state))
	      (+ (current-column) bee-body-indent))
	  (progn
	    (goto-char (cadr (cdr state)))
	    (current-column))))
    (save-excursion
      (goto-char (cadr state))
      (+ (current-column) bee-body-indent))))

;*---------------------------------------------------------------------*/
;*    bee-indent-brace-p ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-indent-brace-p (state)
  (or (and (integerp (car (nthcdr 1 state)))
	   (let ((c (char-after (car (nthcdr 1 state)))))
	     (or (eq c ?{) (and (eq c ?[) (eq bee-indent-mode 'hop)))))
      (let ((op (car (nthcdr 9 state))))
	(and (consp op)
	     (let ((po (reverse op))
		   (context 'unknown))
	       (save-excursion
		 (while (and (consp po) (eq context 'unknown))
		   (cond
		    ((eq (char-after (car po)) ?{)
		     (setq context 'brace))
		    ((eq (char-after (car po)) ?\()
		     (setq context 'scheme))
		    (t
		     (setq po (cdr po))))))
	       (eq context 'brace))))))

;*---------------------------------------------------------------------*/
;*    bee-indent-quote-p ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-indent-quote-p (state)
  (and bee-indent-on-quote-p
       (integerp (car (nthcdr 1 state)))
       (> (car (nthcdr 1 state)) (point-min))
       (eq (char-after (car (nthcdr 1 state))) ?\()
       (or (eq (char-after (- (car (nthcdr 1 state)) 1)) ?')
	   (and (> (car (nthcdr 1 state)) (1+ (point-min)))
		(eq (char-after (- (car (nthcdr 1 state)) 1)) ?#)
		(eq (char-after (- (car (nthcdr 1 state)) 2)) ?')))))

;*---------------------------------------------------------------------*/
;*    bee-body-indent ...                                              */
;*---------------------------------------------------------------------*/
(defvar bee-body-indent 3 "")

;*---------------------------------------------------------------------*/
;*    bee-indent-specform ...                                          */
;*---------------------------------------------------------------------*/
(defun bee-indent-specform (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  bee-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ bee-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case nil
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
           ;; A distinguished form.  Use double bee-body-indent.
           (list (+ containing-form-column (* 2 bee-body-indent))
                 containing-form-start))
          ;; A non-distinguished form. Use body-indent if there are no
          ;; distinguished forms and this is the first undistinguished
          ;; form, or if this is the first undistinguished form and
          ;; the preceding distinguished form has indentation at least
          ;; as great as body-indent.
          ((and (= count 0)
                (or (= i 0)
                    (<= body-indent normal-indent)))
           body-indent)
          (t
           normal-indent))))

;*---------------------------------------------------------------------*/
;*    bee-indent-defform ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-indent-defform (state indent-point)
  (goto-char (car (cdr state)))
  (forward-line 1)
  (if (> (point) (car (cdr (cdr state))))
      (progn
        (goto-char (car (cdr state)))
        (+ bee-body-indent (current-column)))))

;*---------------------------------------------------------------------*/
;*    bee-with-access-indent ...                                       */
;*---------------------------------------------------------------------*/
(defun bee-with-access-indent (state indent-point)
  (skip-chars-forward " \t")
  (bee-indent-with-access-form 2 state indent-point))

;*---------------------------------------------------------------------*/
;*    bee-duplicate-indent ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-duplicate-indent (state indent-point)
  (skip-chars-forward " \t")
  (bee-indent-with-access-form 1 state indent-point))

;*---------------------------------------------------------------------*/
;*    bee-instantiate-indent ...                                       */
;*---------------------------------------------------------------------*/
(defun bee-instantiate-indent (state indent-point)
  (skip-chars-forward " \t")
  (bee-indent-with-access-form 0 state indent-point))

;*---------------------------------------------------------------------*/
;*    bee-indent-with-access-form ...                                  */
;*---------------------------------------------------------------------*/
(defun bee-indent-with-access-form (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
        body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  bee-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ bee-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 1)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
                (condition-case nil
                    (progn
                      (setq count (1- count))
                      (forward-sexp 1)
                      (parse-partial-sexp (point) indent-point 1 t))
                  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
           ;; A distinguished form.  Use double bee-body-indent.
           (list (+ containing-form-column (* 2 bee-body-indent))
                 containing-form-start))
          ;; A non-distinguished form. Use body-indent if there are no
          ;; distinguished forms and this is the first undistinguished
          ;; form, or if this is the first undistinguished form and
          ;; the preceding distinguished form has indentation at least
          ;; as great as body-indent.
          ((and (= count 0)
                (or (= i 0)
                    (<= body-indent normal-indent)))
           body-indent)
          (t
           normal-indent))))

;*---------------------------------------------------------------------*/
;*    bee-indent-instantiate-form ...                                  */
;*---------------------------------------------------------------------*/
(defun bee-indent-instantiate-form (count state indent-point)
  (let ((containing-form-start (car (cdr state))) (i count)
	body-indent containing-form-column)
    ;; Move to the start of containing form, calculate indentation
    ;; to use for non-distinguished forms (> count), and move past the
    ;; function symbol.  bee-indent-hook guarantees that there is at
    ;; least one word or symbol character following open paren of containing
    ;; form.
    (goto-char containing-form-start)
    (setq containing-form-column (current-column))
    (setq body-indent (+ bee-body-indent containing-form-column))
    (forward-char 1)
    (forward-sexp 2)
    ;; Now find the start of the last form.
    (parse-partial-sexp (point) indent-point 1 t)
    (while (and (< (point) indent-point)
		(condition-case nil
		    (progn
		      (setq count (1- count))
		      (forward-sexp 1)
		      (parse-partial-sexp (point) indent-point 1 t))
		  (error nil))))
    ;; Point is sitting on first character of last (or count) sexp.
    (cond ((> count 0)
	   ;; A distinguished form.  Use double bee-body-indent.
	   (list (+ containing-form-column (* 2 bee-body-indent))
		 containing-form-start))
	  ;; A non-distinguished form. Use body-indent if there are no
	  ;; distinguished forms and this is the first undistinguished
	  ;; form, or if this is the first undistinguished form and
	  ;; the preceding distinguished form has indentation at least
	  ;; as great as body-indent.
	  ((and (= count 0)
		(or (= i 0)
		    (<= body-indent normal-indent)))
	   body-indent)
	  (t
	   normal-indent))))

;*---------------------------------------------------------------------*/
;*    bee-let-indent ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-let-indent (state indent-point)
  (skip-chars-forward " \t")
  (if (looking-at "[a-zA-Z0-9+-*/?!@$%^&_:~]")
      (bee-indent-specform 2 state indent-point)
    (bee-indent-specform 1 state indent-point)))

;*---------------------------------------------------------------------*/
;*    bee-indent-sexp ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-indent-sexp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) last-depth bol
        outer-loop-done inner-loop-done state this-indent)
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
        (setq last-depth next-depth
              inner-loop-done nil)
        (while (and (not inner-loop-done)
                    (not (setq outer-loop-done (eobp))))
          (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
                                          nil nil state))
          (setq next-depth (car state))
          (if (car (nthcdr 4 state))
              (progn (bee-comment-indent)
                     (end-of-line)
                     (setcar (nthcdr 4 state) nil)))
          (if (car (nthcdr 3 state))
              (progn
                (forward-line 1)
                (setcar (nthcdr 5 state) nil))
            (setq inner-loop-done t)))
        (if (setq outer-loop-done (<= next-depth 0))
            nil
          (while (> last-depth next-depth)
            (setq indent-stack (cdr indent-stack)
                  last-depth (1- last-depth)))
          (while (< last-depth next-depth)
            (setq indent-stack (cons nil indent-stack)
                  last-depth (1+ last-depth)))
          (forward-line 1)
          (setq bol (point))
          (skip-chars-forward " \t")
          (if (or (eobp) (looking-at ";\\(;;\\|[*]\\)"))
              nil
            (if (and (car indent-stack)
                     (>= (car indent-stack) 0))
                (setq this-indent (car indent-stack))
              (let ((val (bee-calculate-indent
                          (if (car indent-stack) (- (car indent-stack))))))
                (if (integerp val)
                    (setcar indent-stack
                            (setq this-indent val))
                  (setcar indent-stack (- (car (cdr val))))
                  (setq this-indent (car val)))))
            (if (/= (current-column) this-indent)
                (progn (delete-region bol (point))
                       (indent-to this-indent)))))))))

;*---------------------------------------------------------------------*/
;*    bee-indent-last-sexp ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-indent-last-sexp ()
  (interactive)
  (forward-sexp -1)
  (bee-indent-sexp)
  (forward-sexp 1))

;*---------------------------------------------------------------------*/
;*    bee-indent-define ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-indent-define ()
  (interactive)
  (condition-case ()
      (save-excursion
	(end-of-defun)
	(bee-indent-sexp))))

;*---------------------------------------------------------------------*/
;*    bee-indent-toplevel-sexp ...                                     */
;*---------------------------------------------------------------------*/
(defun bee-indent-toplevel-sexp (pos)
  (interactive "dPos: ")
  (let ((sexp (bee-find-toplevel-sexp pos)))
    (if (consp sexp)
	(save-excursion
	  (goto-char (car sexp))
	  (bee-indent-sexp)) 
      (error "Corrupted toplevel sexp"))))

;*---------------------------------------------------------------------*/
;*    Bee indent forms                                                 */
;*---------------------------------------------------------------------*/
(defvar cond-regexp
  "\\(cond\\|case\\|match-case\\|args-parse\\|cond-expand\\|string-case\\|match-lambda\\|syntax-rule\\|regular-grammar\\)[\t\n ]")

;; basic forms
(put 'begin                     'bee-indent-hook 0)
(put 'case                      'bee-indent-hook 1)
(put 'delay                     'bee-indent-hook 0)
(put 'do                        'bee-indent-hook 0)
(put 'lambda                    'bee-indent-hook 1)
(put 'cond                      'bee-indent-hook 0)
(put 'when                      'bee-indent-hook 1)
(put 'unless                    'bee-indent-hook 1)
(put 'if                        'bee-indent-hook -1)
(put 'or                        'bee-indent-hook -1)
(put 'and                       'bee-indent-hook -1)
(put 'else                      'bee-indent-hook -1)

;; module
(put 'static                    'bee-indent-hook 'bee-module-indent-hook)
(put 'import                    'bee-indent-hook 'bee-module-indent-hook)
(put 'extern                    'bee-indent-hook 'bee-module-indent-hook)
(put 'export                    'bee-indent-hook 'bee-module-indent-hook)
(put 'include                   'bee-indent-hook 'bee-module-indent-hook)
(put 'library                   'bee-indent-hook 'bee-module-indent-hook)
(put 'use                       'bee-indent-hook 'bee-module-indent-hook)
(put 'from                      'bee-indent-hook 'bee-module-indent-hook)
(put 'pragma                    'bee-indent-hook 'bee-module-indent-hook)

;; binding forms
(put 'let                       'bee-indent-hook 'bee-let-indent)
(put 'let*                      'bee-indent-hook 1)
(put 'letrec                    'bee-indent-hook 1)
(put 'letrec*                   'bee-indent-hook 1)
(put 'labels                    'bee-indent-hook 'bee-let-indent)
(put 'let-syntax                'bee-indent-hook 'bee-let-indent)
(put 'letrec-syntax             'bee-indent-hook 'bee-let-indent)
(put 'co-instantiate            'bee-indent-hook 'bee-let-indent)

;; output/input command
(put 'call-with-input-file      'bee-indent-hook 1)
(put 'call-with-input-string    'bee-indent-hook 1)
(put 'with-input-from-file      'bee-indent-hook 1)
(put 'with-input-from-port      'bee-indent-hook 1)
(put 'with-input-from-string    'bee-indent-hook 1)
(put 'with-input-from-procedure 'bee-indent-hook 1)
(put 'call-with-output-file     'bee-indent-hook 1)
(put 'call-with-output-string   'bee-indent-hook 0)
(put 'with-output-to-file       'bee-indent-hook 1)
(put 'with-output-to-port       'bee-indent-hook 1)
(put 'with-output-to-string     'bee-indent-hook 0)
(put 'with-error-to-port        'bee-indent-hook 1)
(put 'with-error-to-file        'bee-indent-hook 1)
(put 'with-error-to-string      'bee-indent-hook 0)
(put 'with-scheduler            'bee-indent-hook 1)
(put 'with-exception-handler    'bee-indent-hook 0)
(put 'with-handler              'bee-indent-hook 0)
(put 'with-alarm                'bee-indent-hook 1)
(put 'with-lock                 'bee-indent-hook 1)

;; define forms
(put 'define-macro              'bee-indent-hook 1)
(put 'macro                     'bee-indent-hook 1)
(put 'define-generic            'bee-indent-hook 1)
(put 'define-method             'bee-indent-hook 1)

;; exceptions
(put 'bind-exit                 'bee-indent-hook 1)
(put 'unwind-protect            'bee-indent-hook 0)
(put 'dynamic-wind              'bee-indent-hook 0)

;; multiple values
(put 'multiple-value-bind       'bee-indent-hook 1)
(put 'receive                   'bee-indent-hook 1)
(put 'call-with-values          'bee-indent-hook 0)

;; parsing
(put 'regular-grammar           'bee-indent-hook 'bee-let-indent)
(put 'lalr-grammar              'bee-indent-hook 0)

;; module indentation
(put 'module                    'bee-indent-hook 1)
(put 'interface                 'bee-indent-hook 1)
(put 'directives                'bee-indent-hook 0)
(put 'class                     'bee-indent-hook 1)
(put 'abstract-class            'bee-indent-hook 1)
(put 'wide-class                'bee-indent-hook 1)
(put 'final-class               'bee-indent-hook 1)

;; matching indentation
(put 'args-parse                'bee-indent-hook 1)
(put 'match-case                'bee-indent-hook 1)
(put 'cond-expand               'bee-indent-hook 0)
(put 'string-case               'bee-indent-hook 1)
(put 'match-lambda              'bee-indent-hook 0)
(put 'syntax-rules              'bee-indent-hook 1)
(put 'event-case                'bee-indent-hook 1)
(put 'on-event                  'bee-indent-hook 1)
(put 'with-trace                'bee-indent-hook 2)

;; profiling
(put 'profile                   'bee-indent-hook 1)
(put 'profile/gc                'bee-indent-hook 1)

;; hop
(put 'service                   'bee-indent-hook 1)
(put 'add-event-listener!       'bee-indent-hook 2)
(put 'remove-event-listener!    'bee-indent-hook 2)
(put 'timeout                   'bee-indent-hook 1)
(put 'after                     'bee-indent-hook 1)
(put 'with-hop                  'bee-indent-hook 1)
(put 'with-url                  'bee-indent-hook 1)
(put 'node-style-set!           'bee-indent-hook 1)

;; sql
(put 'sqlite-exec               'bee-indent-hook 1)
(put 'sqlite-map                'bee-indent-hook 2)
(put 'sqlite-eval               'bee-indent-hook 2)
