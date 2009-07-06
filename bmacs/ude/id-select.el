;;!emacs
;;
;; LCD-ENTRY:    id-select.el|InfoDock Associates|elisp@infodock.com|Syntactical region selecting|02/28/97|1.4.5|
;;
;; FILE:         id-select.el
;; SUMMARY:      Select larger and larger syntax-driven regions in a buffer.
;; USAGE:        XEmacs and Emacs Lisp Library
;; KEYWORDS:     matching, mouse
;;
;; AUTHOR:       Bob Weiner
;;
;; ORG:          InfoDock Associates.  We sell corporate support and
;;               development contracts for InfoDock, Emacs and XEmacs.
;;               E-mail: <info@infodock.com>  Web: http://www.infodock.com
;;               Tel: +1 408-243-3300
;;
;;
;; ORIG-DATE:    19-Oct-96 at 02:25:27
;; LAST-MOD:     28-Feb-97 at 15:36:39 by Bob Weiner
;;
;; Copyright (C) 1996, 1997  InfoDock Associates
;;
;; This file is part of InfoDock.
;; It is available for use and distribution under the terms of the GNU Public
;; License.
;;
;; DESCRIPTION:  
;;
;;   This is a radically cool, drop in mouse and keyboard-based library for
;;   selecting successively bigger syntactical regions within a buffer.
;;   Simply load this library and you are ready to try it out by
;;   double-clicking on various kinds of characters in different buffer major
;;   modes.  You'll quickly get the hang of it.  (It also provides a command
;;   to jump between beginning and end tags within HTML and SGML buffers.) 
;;   
;;   A great deal of smarts are built-in so that it does the right thing
;;   almost all of the time; many other attempts at similar behavior such as
;;   thing.el fail to deal with many file format complexities.
;;   
;;   Double clicks of the Selection Key (left mouse key) at the same point
;;   will select bigger and bigger regions with each successive use.  The
;;   first double click selects a region based upon the character at the
;;   point of the click.  For example, with the point over an opening or
;;   closing grouping character, such as { or }, the whole grouping is
;;   selected, e.g. a C function.  When on an _ or - within a programming
;;   language variable name, the whole name is selected.  The type of
;;   selection is displayed in the minibuffer as feedback.  When using a
;;   language based mainly on indenting, like Bourne shell, a double click on
;;   the first alpha character of a line, such as an if statement, selects
;;   the whole statement.
;;
;;   ---------------
;;
;;   This whole package is driven by a single function, available in mouse
;;   and keyboard forms, that first marks a region based on the syntax
;;   category of the character following point.  Successive invocations mark
;;   larger and larger regions until the whole buffer is marked.  See the
;;   documentation for the function, id-select-syntactical-region, for the
;;   kinds of syntax categories handled.
;;
;;   Loading this package automatically installs its functionalty on
;;   double-clicks (or higher) of the left mouse key.  (See the documentation
;;   for the variable, mouse-track-click-hook, for how this is done.)  A
;;   single click of the left button will remove the region and reset point.
;;
;;   The function, id-select-thing, may be bound to a key to provide the same
;;   syntax-driven region selection functionality. {C-c C-m} is a
;;   reasonable site-wide choice since this key is seldom used and it
;;   mnemonically indicates marking something.  {C-c s} may be preferred as a
;;   personal binding.
;;
;;   Use {C-g} to unmark the region when done.  Use,
;;   id-select-thing-with-mouse, if you want to bind this to a mouse key and
;;   thereby use single clicks instead of double clicks.
;;
;;   Three other commands are also provided:
;;    id-select-and-copy-thing - mark and copy the syntactical unit to the
;;      kill ring 
;;    id-select-and-kill-thing - kill the syntactical unit at point
;;    id-select-goto-matching-tag - In HTML and SGML modes (actually any
;;      listed in the variable, `id-select-markup-modes'), moves point to the
;;      start of the tag paired with the closest tag that point is within or
;;      which it precedes, so you can quickly jump back and forth between
;;      open and close tags.
;;
;;   ---------------
;;   SETUP:
;;
;;   To autoload this package under XEmacs or InfoDock via mouse usage, add
;;   the following line to one of your initialization files.  (Don't do this
;;   for GNU Emacs.)
;;
;;      (add-hook 'mouse-track-click-hook 'id-select-double-click-hook)
;;
;;   For any version of Emacs you should add the following autoload entries
;;   at your site:
;;
;;      (autoload 'id-select-and-kill-thing
;;         "id-select" "Kill syntactical region selection" t)
;;      (autoload 'id-select-and-copy-thing
;;         "id-select" "Select and copy syntactical region" t)
;;      (autoload 'id-select-double-click-hook
;;         "id-select" "Double mouse click syntactical region selection" nil)
;;      (autoload 'id-select-thing
;;         "id-select" "Keyboard-driven syntactical region selection" t)
;;      (autoload 'id-select-thing-with-mouse
;;         "id-select" "Single mouse click syntactical region selection" t)
;;
;;   If you want to be able to select C++ and Java methods and classes by
;;   double-clicking on the first character of a definition or on its opening
;;   or closing brace, you may need the following setting (all
;;   because Sun programmers can't put their opening braces in the first
;;   column):
;;
;;      (add-hook 'java-mode-hook
;;         (function
;;           (lambda ()
;;	      (setq defun-prompt-regexp
;;		    "^[ \t]*\\(\\(\\(public\\|protected\\|private\\|const\\|abstract\\|synchronized\\|final\\|static\\|threadsafe\\|transient\\|native\\|volatile\\)\\s-+\\)*\\(\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*[][_$.a-zA-Z0-9]+\\|[[a-zA-Z]\\)\\s-*\\)\\s-+\\)\\)?\\(\\([[a-zA-Z][][_$.a-zA-Z0-9]*\\s-+\\)\\s-*\\)?\\([_a-zA-Z][^][ \t:;.,{}()=]*\\|\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)\\)\\s-*\\(([^);{}]*)\\)?\\([] \t]*\\)\\(\\s-*\\<throws\\>\\s-*\\(\\([_$a-zA-Z][_$.a-zA-Z0-9]*\\)[, \t\n\r\f]*\\)+\\)?\\s-*"))))
;;
;;      (add-hook 'c++-mode-hook
;;	   (function
;;	    (lambda ()
;;	      (setq defun-prompt-regexp
;;		   "^[ \t]*\\(template[ \t\n\r]*<[^>;.{}]+>[ \t\n\r]*\\)?\\(\\(\\(auto\\|const\\|explicit\\|extern[ \t\n\r]+\"[^\"]+\"\\|extern\\|friend\\|inline\\|mutable\\|overload\\|register\\|static\\|typedef\\|virtual\\)[ \t\n\r]+\\)*\\(\\([[<a-zA-Z][]_a-zA-Z0-9]*\\(::[]_a-zA-Z0-9]+\\)?[ \t\n\r]*<[_<>a-zA-Z0-9 ,]+>[ \t\n\r]*[*&]*\\|[[<a-zA-Z][]_<>a-zA-Z0-9]*\\(::[[<a-zA-Z][]_<>a-zA-Z0-9]+\\)?[ \t\n\r]*[*&]*\\)[*& \t\n\r]+\\)\\)?\\(\\(::\\|[[<a-zA-Z][]_a-zA-Z0-9]*[ \t\n\r]*<[^>;{}]+>[ \t\n\r]*[*&]*::\\|[[<a-zA-Z][]_~<>a-zA-Z0-9]*[ \t\n\r]*[*&]*::\\)[ \t\n\r]*\\)?\\(operator[ \t\n\r]*[^ \t\n\r:;.,?~{}]+\\([ \t\n\r]*\\[\\]\\)?\\|[_~<a-zA-Z][^][ \t:;.,~{}()]*\\|[*&]?\\([_~<a-zA-Z][_a-zA-Z0-9]*[ \t\n\r]*<[^>;{}]+[ \t\n\r>]*>\\|[_~<a-zA-Z][_~<>a-zA-Z0-9]*\\)\\)[ \t\n\r]*\\(([^{;]*)\\(\\([ \t\n\r]+const\\|[ \t\n\r]+mutable\\)?\\([ \t\n\r]*[=:][^;{]+\\)?\\)?\\)\\s-*"))))
;;
;;   If you want tags, comments, sentences and text blocks to be selectable
;;   in HTML mode, you need to add the following to your personal
;;   initializations  (You would do something similar for SGML mode.):
;;
;;      ;; Make tag begin and end delimiters act like grouping characters,
;;      ;; for easy syntactical selection of tags.
;;      (add-hook 'html-mode-hook
;;    	  (function
;;	   (lambda ()
;;	     (modify-syntax-entry ?<  "(>" html-mode-syntax-table)
;;	     (modify-syntax-entry ?>  ")<" html-mode-syntax-table)
;;	     (modify-syntax-entry ?\" "\"" html-mode-syntax-table)
;;	     (modify-syntax-entry ?=  "."  html-mode-syntax-table)
;;	     (make-local-variable 'comment-start)
;;	     (make-local-variable 'comment-end)
;;	     (setq comment-start "<!--" comment-end "-->")
;;	     (make-local-variable 'sentence-end)
;;	     (setq sentence-end "\\([^ \t\n\r>]<\\|>\\(<[^>]*>\\)*\\|[.?!][]\"')}]*\\($\\| $\\|\t\\|  \\)\\)[ \t\n]*")
;;
;;           (define-key html-mode-map "\C-c." 'id-select-goto-matching-tag)
;;           )))
;;
;;   If you are incredibly academic and you use the Miranda programming
;;   language with a literate programming style (where code is preceded by a
;;   > character in the first column, you'll want to change the line in
;;   mira.el that reads:
;;           (modify-syntax-entry ?> ".")
;;   to:
;;           (modify-syntax-entry ?> " ")
;;
;;   in order to make this package recognize the indented expressions of the
;;   language.  If you don't use the literate style, no changes should be
;;   necessary.
;;
;; DESCRIP-END.

;;; ************************************************************************
;;; Public variables
;;; ************************************************************************

(defgroup id-select nil
  "Select larger and larger syntax-driven regions in a buffer"
  :group 'matching
  :group 'mouse)


(defcustom id-select-brace-modes
  '(c++-mode c-mode java-mode objc-mode perl-mode tcl-mode)
  "*List of language major modes which define things with brace delimiters."
  :type '(repeat (symbol :tag "Mode" symbol))
  :group 'id-select)

(defcustom id-select-markup-modes
  '(html-mode sgml-mode)
  "*List of markup language modes that use SGML-style <tag> </tag> pairs."
  :type '(repeat (symbol :tag "Mode"))
  :group 'id-select)

(defcustom id-select-text-modes
  '(fundamental-mode kotl-mode indented-text-mode Info-mode outline-mode text-mode)
  "*List of textual modes where paragraphs may be outdented or indented."
  :type '(repeat (symbol :tag "Mode"))
  :group 'id-select)

(defcustom id-select-indent-modes
  (append '(asm-mode csh-mode eiffel-mode ksh-mode miranda-mode python-mode
	    pascal-mode sather-mode)
	  id-select-text-modes)
  "*List of language major modes which use mostly indentation to define syntactic structure."
  :type '(repeat (symbol :tag "Mode"))
  :group 'id-select)

(defvar id-select-indent-non-end-regexp-alist
  '((csh-mode    "\\(\\|then\\|elsif\\|else\\)[ \t]*$")
    (eiffel-mode "\\(\\|then\\|else if\\|else\\)[ \t]*$")
    (ksh-mode    "\\(\\|then\\|elif\\|else\\)[ \t]*$")
    (miranda-mode "[ \t>]*$")
    (pascal-mode "\\(\\|then\\|else\\)[ \t]*$")
    (python-mode "[ \t]*$")
    (sather-mode "\\(\\|then\\|else if\\|else\\)[ \t]*$")
    ;;
    (fundamental-mode "[^ \t\n*]")
    (kotl-mode "[^ \t\n*]")
    (indented-text-mode "[^ \t\n*]")
    (Info-mode "[^ \t\n]")
    (outline-mode "[^*]")
    (text-mode  "[^ \t\n*]")
    )
  "List of (major-mode . non-terminator-line-regexp) elements used to avoid early dropoff when marking indented code.")

(defvar id-select-indent-end-regexp-alist
  '((csh-mode "end\\|while")
    (eiffel-mode "end")
    (ksh-mode "\\(fi\\|esac\\|until\\|done\\)[ \t\n]")
    (pascal-mode "end")
    (sather-mode "end")
    ;;
    (fundamental-mode "[ \t]*$")
    (indented-text-mode "[ \t]*$")
    (Info-mode "[ \t]*$")
    (text-mode  "[ \t]*$")
    )
  "List of (major-mode . terminator-line-regexp) elements used to include a final line when marking indented code.")

(defcustom id-select-char-p t
  "*If t, return single character boundaries when all else fails."
  :type 'boolean
  :group 'id-select)

(defcustom id-select-display-type t
  "*If t, display the thing selected with each mouse click."
  :type 'boolean
  :group 'id-select)

(defcustom id-select-whitespace t
  "*If t, groups of whitespace are considered as things."
  :type 'boolean
  :group 'id-select)

(defvar id-select-previous 'nil)
(defvar id-select-prior-point 'nil)
(defvar id-select-prior-buffer 'nil)

;;; ************************************************************************
;;; Public functions
;;; ************************************************************************

;;
;; Commands
;;

;;;###autoload
(defun id-select-install ()
  "Install the id-select mode as the default mode of operation."
  (interactive)
  (if (string-match "XEmacs" emacs-version)
      (add-hook 'mouse-track-click-hook 'id-select-double-click-hook)
    (if (string-match "^19\\." emacs-version)
	(progn (transient-mark-mode 1)
	       (global-set-key [mouse-1] 'mouse-set-point)
	       (global-set-key [double-mouse-1] 'id-select-thing-with-mouse)
	       (global-set-key [triple-mouse-1] 'id-select-thing-with-mouse)))))

;;;###autoload
(defun id-select-thing ()
  "Mark the region selected by the syntax of the thing at point.
If invoked repeatedly, selects bigger and bigger things.
If `id-select-display-type' is non-nil, the type of selection is displayed in
the minibuffer."
  (interactive
   (cond ((and (fboundp 'region-active-p) (region-active-p))
	  nil)
	 ((and (boundp 'transient-mark-mode) transient-mark-mode mark-active)
	  nil)
	 (t
	  ;; Reset selection based on the syntax of character at point.
	  (id-select-reset)
	  nil)))
  (let ((region (id-select-boundaries (point))))
    (if region
	(progn (goto-char (car region))
	       (set-mark (cdr region))
	       (if (fboundp 'activate-region) (activate-region))
	       (if (and (boundp 'transient-mark-mode)
			transient-mark-mode)
		   (setq mark-active t))
	       (and (interactive-p) id-select-display-type
		    (message "%s" id-select-previous))
	       (run-hooks 'id-select-thing-hook)
	       t))))

;;;###autoload
(defun id-select-thing-with-mouse (event)
  "Select a region based on the syntax of the character from a mouse click.
If the click occurs at the same point as the last click, select
the next larger syntactic structure.  If `id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer."
  (interactive "@e")
  (cond ((and (eq id-select-prior-point (point))
	      (eq id-select-prior-buffer (current-buffer)))
	 ;; Prior click was at the same point as before, so enlarge
	 ;; selection to the next bigger item.
	 (if (and (id-select-bigger-thing) id-select-display-type)
 	     (progn
	       ;; Conditionally, save selected region for pasting.
 	       (cond
		;; XEmacs
		((fboundp 'x-store-cutbuffer)
		 (x-store-cutbuffer (buffer-substring (point) (mark))))
		;; Emacs 19
		((and (boundp 'interprogram-cut-function)
		      interprogram-cut-function)
		 (x-set-selection 'PRIMARY (buffer-substring (point) (mark)))))
	       (message "%s" id-select-previous)))
	 t)
	(t (setq this-command 'mouse-start-selection)
	   (id-select-reset)
	   (id-select-thing-with-mouse event))))

;;;###autoload
(defun id-select-goto-matching-tag ()
  "If in a major mode listed in `id-select-markup-modes,' moves point to the start of the tag paired with the closest tag that point is within or precedes.
Returns t if point is moved, else nil.
Signals an error if no tag is found following point or if the closing tag
does not have a `>' terminator character."
  (interactive)
  (if (not (memq major-mode id-select-markup-modes))
      nil
    (let ((result)
	  ;; Assume case of tag names is irrelevant.
	  (case-fold-search t)
	  (opoint (point))
	  (tag)
	  end-point
	  start-regexp
	  end-regexp)

      ;; Leave point at the start of the tag that point is within or that
      ;; follows point.
      (cond
       ;; Point is at the start of a tag.
       ((looking-at "<[^<> \t\n\r]"))
       ;; Point was within a tag.
       ((and (re-search-backward "[<>]" nil t)
	     (looking-at "<[^<> \t\n\r]")))
       ;; Move to following tag.
       ((and (re-search-forward "<" nil t)
	     (progn (backward-char 1)
		    (looking-at "<[^<> \t\n\r]"))))
       ;; No tag follows point.
       (t (error "(id-select-goto-matching-tag): No tag found after point.")))

      (if (catch 'done
	    (cond
	     ;; Beginning of a tag pair
	     ((looking-at "<[^/][^<> \t\n\r]*")
	      (setq tag (buffer-substring (match-beginning 0) (match-end 0))
		    start-regexp (regexp-quote tag)
		    end-regexp   (concat "</" (substring start-regexp 1)))
	      ;; Skip over nested tags.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp))
		    match-point)
		(while (and (>= count 0)
			    (re-search-forward regexp nil t))
		  (setq match-point (match-beginning 0))
		  (if (/= (char-after (1+ (match-beginning 0))) ?/)
		      ;; Start tag
		      (setq count (1+ count))
		    ;; End tag
		    (setq end-point (point))
		    (if (or (not (re-search-forward "[<>]" nil t))
			    (= (preceding-char) ?<))
			;; No terminator character `>' for end tag
			(progn (setq result end-point)
			       (throw 'done nil)))
		    (setq count (1- count))
		    (if (= count 0)
			(progn
			  (goto-char match-point)
			  (setq result t)
			  (throw 'done result)))))))
	     ;;
	     ;; End of a tag pair
	     ((or (looking-at "</[^> \t\n\r]+")
		  (and (skip-chars-backward "<")
		       (looking-at "</[^> \t\n\r]+")))
	      (goto-char (match-end 0))
	      (setq tag (buffer-substring (match-beginning 0) (match-end 0))
		    end-regexp (regexp-quote tag)
		    start-regexp   (concat "<" (substring end-regexp 2)))
	      (setq end-point (point))
	      (if (or (not (re-search-forward "[<>]" nil t))
		      (= (preceding-char) ?<))
		  ;; No terminator character `>' for end tag
		  (progn (setq result end-point)
			 (throw 'done nil)))
	      ;; Skip over nested tags.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp)))
		(while (and (>= count 0)
			    (re-search-backward regexp nil t))
		  (if (= (char-after (1+ (point))) ?/)
		      ;; End tag
		      (setq count (1+ count))
		    ;; Start tag
		    (setq count (1- count))
		    (if (= count 0)
			(progn
			  (setq result t)
			  (throw 'done t)))))))))
	  nil
	;; Didn't find matching tag.
	(goto-char opoint))

      (cond ((integerp result)
	     (goto-char result)
	     (error "(id-select-goto-matching-tag): Add a terminator character for this end <tag>"))
	    ((null tag)
	     (error "(id-select-goto-matching-tag): No <tag> following point"))
	    ((null result)
	     (if (interactive-p)
		 (progn
		   (beep)
		   (message "(id-select-goto-matching-tag): No matching tag for %s>"
			    tag)
		   result)))
	    (t result)))))

;;;###autoload
(defun id-select-and-copy-thing ()
  "Copy the region surrounding the syntactical unit at point."
  (interactive)
  (let ((bounds (id-select-boundaries (point))))
    (if bounds (copy-region-as-kill (car bounds) (cdr bounds)))))

;;;###autoload
(defun id-select-and-kill-thing ()
  "Kill the region surrounding the syntactical unit at point."
  (interactive "*")
  (let ((bounds (id-select-boundaries (point))))
    (if bounds (kill-region (car bounds) (cdr bounds)))))


;;
;; Functions
;;

(defun id-select-boundaries (pos)
  "Return the (start . end) of a syntactically defined region based upon the last region selected or on position POS.
The character at POS is selected if no other thing is matched."
  (interactive)
  (setq zmacs-region-stays t)
  (setcar id-select-old-region (car id-select-region))
  (setcdr id-select-old-region (cdr id-select-region))
  (let ((prior-type id-select-previous))
    (cond
     ((eq id-select-previous 'char)
      (id-select-syntactical-region pos))
     ((and (car id-select-old-region)
	   (memq id-select-previous
		 '(sexp sexp-start sexp-end sexp-up))
	   (id-select-sexp-up pos)
	   (id-select-region-bigger-p id-select-old-region id-select-region))
      id-select-region)
     ;;
     ;; In the general case, we can't know ahead of time what the next
     ;; biggest type of thing to select is, so we test them all and choose
     ;; the best fit.  This means that dynamically, the order of type
     ;; selection will change based on the buffer context.
     (t (let ((min-region (1+ (- (point-max) (point-min))))
	      (result)
	      region region-size)
	  (mapcar
	   (function
	    (lambda (sym-func)
	      (setq region
		    (if (car (cdr sym-func))
			(funcall (car (cdr sym-func)) pos)))
	      (if (and region (car region)
		       (id-select-region-bigger-p
			id-select-old-region region)
		       (setq region-size
			     (- (cdr region) (car region)))
		       (< region-size min-region))
		  (setq min-region region-size
			result 
			(list;; The actual selection type is
			 ;; sometimes different than the one we
			 ;; originally tried, so recompute it here.
			 (car (assq id-select-previous
				    id-select-bigger-alist))
			 (car region) (cdr region))))))
	   id-select-bigger-alist)
	  (if result
	      ;; Returns id-select-region
	      (progn (setq id-select-previous (car result))
		     (id-select-set-region (nth 1 result) (nth 2 result)))
	    ;;
	    ;; Restore prior selection type since we failed to find a
	    ;; new one.
	    (setq id-select-previous prior-type)
	    (beep)
	    (message
	     "(id-select-boundaries): `%s' is the largest selectable region"
	     id-select-previous)
	    nil))))))

;;;###autoload
(defun id-select-double-click-hook (event click-count)
  "Select a region based on the syntax of the character wherever the mouse is double-clicked.
If the double-click occurs at the same point as the last double-click, select
the next larger syntactic structure.  If `id-select-display-type' is non-nil,
the type of selection is displayed in the minibuffer."
  (cond ((/= click-count 2)
	 ;; Return nil so any other hooks are performed.
	 nil)
	(t (id-select-thing-with-mouse event))))

(defun id-select-syntactical-region (pos)
  "Return the (start . end) of a syntactically defined region based upon the buffer position POS.
Uses `id-select-syntax-alist' and the current buffer's syntax table to
determine syntax groups.

Typically:
 Open or close grouping character syntax marks an s-expression.
 Double quotes mark strings.
 The end of a line marks the line, including its trailing newline.
 Word syntax marks the current word.
 Symbol syntax (such as _) marks a symbol.
 Whitespace marks a span of whitespace.
 Comment start or end syntax marks the comment.
 Punctuation syntax marks the words on both sides of the punctuation.
 The fallback default is to mark the character at POS.

If an error occurs during syntax scanning, it returns nil."
  (interactive "d")
  (setq id-select-previous 'char)
  (if (save-excursion (goto-char pos) (eolp))
      (id-select-line pos)
    (let* ((syntax (char-syntax (if (eobp) (preceding-char) (char-after pos))))
           (pair (assq syntax id-select-syntax-alist)))
      (cond ((and pair
		  (or id-select-whitespace
		      (not (eq (car (cdr pair)) 'thing-whitespace))))
             (funcall (car (cdr pair)) pos))
            (id-select-char-p
             (setq id-select-previous 'char)
             (id-select-set-region pos (1+ pos)))
            (t
             nil)))))

;;; ************************************************************************
;;; Private functions
;;; ************************************************************************

(defun id-select-at-blank-line-or-comment ()
  "Return non-nil if on a blank line or a comment start or end line.
Assumes point is befor any non-whitespace character on the line."
  (let ((comment-end-p (and (stringp comment-end)
			    (not (string-equal comment-end "")))))
    (if (looking-at
	 (concat "\\s-*$\\|\\s-*\\(//\\|/\\*\\|.*\\*/"
		 (if comment-start
		     (concat
		      "\\|" (regexp-quote comment-start)))
		 (if comment-end-p
		     (concat
		      "\\|.*" (regexp-quote comment-end)))
		 "\\)"))
	(or (not (and comment-start comment-end-p))
	    ;; Ignore start and end of comments that
	    ;; follow non-commented text.
	    (not (looking-at
		  (format ".*\\S-.*%s.*%s"
			  (regexp-quote comment-start)
			  (regexp-quote comment-end))))))))

(defun id-select-back-to-indentation ()
  "Move point to the first non-whitespace character on this line and return point.
This respects the current syntax table definition of whitespace, whereas
`back-to-indentation' does not.  This is relevant in literate programming and
mail and news reply modes."
  (goto-char (min (progn (end-of-line) (point))
		  (progn (beginning-of-line)
			 (skip-syntax-forward " ")
			 (point)))))

(defun id-select-bigger-thing ()
  "Select a bigger object where point is."
  (prog1
      (id-select-thing)
    (setq this-command 'select-thing)))

(defun id-select-region-bigger-p (old-region new-region)
  "Return t if OLD-REGION is smaller than NEW-REGION and NEW-REGION partially overlaps OLD-REGION, or if OLD-REGION is uninitialized."
  (if (null (car old-region))
      t
    (and (> (abs (- (cdr new-region) (car new-region)))
       (abs (- (cdr old-region) (car old-region))))
	 ;; Ensure the two regions intersect.
	 (or (and (<= (min (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region)))
		  (>  (max (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region))))
	     (and (>  (min (cdr new-region) (car new-region))
		      (min (cdr old-region) (car old-region)))
		  (<= (min (cdr new-region) (car new-region))
		      (max (cdr old-region) (car old-region))))))))

(defun id-select-reset ()
  ;; Reset syntactic selection.
  (setq id-select-prior-point (point)
	id-select-prior-buffer (current-buffer)
	id-select-previous 'char)
  (id-select-set-region nil nil))

(defun id-select-set-region (beginning end)
  "Set the cons cell held by the variable `id-select-region' to (BEGINNING . END).
Return the updated cons cell."
  (setcar id-select-region beginning)
  (setcdr id-select-region end)
  (if (and (null beginning) (null end))
      (progn (setcar id-select-old-region nil)
	     (setcdr id-select-old-region nil)))
  (if (and (not (eq id-select-previous 'buffer))
	   (integerp beginning) (integerp end)
	   (= beginning (point-min)) (= end (point-max)))
      ;; If we selected the whole buffer, make sure that 'thing' type is 'buffer'.
      nil
    id-select-region))

(defun id-select-string-p (&optional start-delim end-delim)
  "Returns (start . end) of string whose first line point is within or immediately before.
Positions include delimiters.  String is delimited by double quotes unless
optional START-DELIM and END-DELIM (strings) are given.
Returns nil if not within a string."
  (let ((opoint (point))
	(count 0)
	bol start delim-regexp start-regexp end-regexp)
    (or start-delim (setq start-delim "\""))
    (or end-delim (setq end-delim "\""))
    ;; Special case for the empty string.
    (if (looking-at (concat (regexp-quote start-delim)
			    (regexp-quote end-delim)))
	(id-select-set-region (point) (match-end 0))
      (setq start-regexp (concat "\\(^\\|[^\\]\\)\\("
				 (regexp-quote start-delim) "\\)")
	    end-regexp   (concat "[^\\]\\(" (regexp-quote end-delim) "\\)")
	    delim-regexp (concat start-regexp "\\|" end-regexp))
      (save-excursion
	(beginning-of-line)
	(setq bol (point))
	(while (re-search-forward delim-regexp opoint t)
	  (setq count (1+ count))
	  ;; This is so we don't miss the closing delimiter of an empty
	  ;; string.
	  (if (and (= (point) (1+ bol))
		   (looking-at (regexp-quote end-delim)))
	      (setq count (1+ count))
	    (if (bobp) nil (backward-char 1))))
	(goto-char opoint)
	;; If found an even # of starting and ending delimiters before
	;; opoint, then opoint is at the start of a string, where we want it.
	(if (zerop (mod count 2))
	    (if (bobp) nil (backward-char 1))
	  (re-search-backward start-regexp nil t))
	;; Point is now before the start of the string.
	(if (re-search-forward start-regexp nil t)
	    (progn
	      (setq start (match-beginning 2))
	      (if (re-search-forward end-regexp nil t)
		  (id-select-set-region start (point)))))))))

;;;
;;; Code selections
;;;

(defun id-select-brace-def-or-declaration (pos)
  "If POS is at the first character, opening brace or closing brace of a brace delimited language definition, return (start . end) region, else nil.
The major mode for each supported brace language must be included in the
list, id-select-brace-modes."
  (interactive)
  (if (not (and (featurep 'cc-mode) (memq major-mode id-select-brace-modes)))
      nil
    (save-excursion
      (goto-char pos)
      (let ((at-def-brace
	     (or (looking-at "^{") (looking-at "^}")
		 ;; Handle stupid old C-style and new Java
		 ;; style of putting braces at the end of
		 ;; lines.
		 (and (= (following-char) ?{)
		      (stringp defun-prompt-regexp)
		      (save-excursion
			(beginning-of-line)
			(looking-at defun-prompt-regexp)))
		 (and (= (following-char) ?})
		      (stringp defun-prompt-regexp)
		      (condition-case ()
			  (progn
			    ;; Leave point at opening brace.
			    (goto-char
			     (scan-sexps (1+ (point)) -1))
			    ;; Test if these are defun braces.
			    (save-excursion
			      (beginning-of-line)
			      (looking-at defun-prompt-regexp)))
			(error nil)))))
	    eod)
	(if (or at-def-brace
		;; At the start of a definition:
		;; Must be at the first non-whitespace character in the line.
		(and (= (point) (save-excursion (id-select-back-to-indentation)))
		     ;; Must be on an alpha or symbol-constituent character.
		     ;; Also allow ~ for C++ destructors. 
		     (looking-at "[a-zA-z~]\\|\\s_")
		     ;; Previous line, if any,  must be blank or a comment
		     ;; start or end or `defun-prompt-regexp' must be defined
		     ;; for this mode.
		     (or (stringp defun-prompt-regexp)
			 (save-excursion
			   (if (/= (forward-line -1) 0)
			       t
			     (id-select-at-blank-line-or-comment))))))
	    (progn
	      (setq id-select-previous 'brace-def-or-declaration)
	      ;; Handle declarations and definitions embedded within classes.
	      (if (and (= (following-char) ?{)
		       (/= (point) (save-excursion
				     (id-select-back-to-indentation))))
		  (setq at-def-brace nil))
	      ;;
	      (if at-def-brace nil (beginning-of-line))
	      (if (and (not at-def-brace)
		       (stringp defun-prompt-regexp)
		       (looking-at defun-prompt-regexp))
		  ;; Mark the declaration or definition
		  (id-select-set-region
		   (point)
		   (progn (goto-char (match-end 0))
			  (if (= (following-char) ?{)
			      (forward-list 1)
			    (search-forward ";" nil t))
			  (skip-chars-forward " \t")
			  (skip-chars-forward "\n")
			  (if (looking-at "^\\s-*$")
			      (forward-line 1))
			  (point)))
		;; Mark function definitions only
		(setq eod (save-excursion
			    (condition-case ()
				(progn
				  (end-of-defun)
				  (if (looking-at "^\\s-*$")
				      (forward-line 1))
				  (point))
			      (error (point-max)))))
		(if (= (following-char) ?})
		    ;; Leave point at opening brace.
		    (goto-char (scan-sexps (1+ (point)) -1)))
		(if (= (following-char) ?{)
		    (progn
		      (while (and (zerop (forward-line -1))
				  (not (id-select-at-blank-line-or-comment))))
		      (if (id-select-at-blank-line-or-comment)
			  (forward-line 1))))
		;; Mark the whole definition
		(setq id-select-previous 'brace-def-or-declaration)
		(id-select-set-region (point) eod))))))))

(defun id-select-indent-def (pos)
  "If POS is at the first alpha character on a line, return (start . end) region,

The major mode for each supported indented language must be included in the
list, id-select-indent-modes."
  (interactive)
  (if (not (memq major-mode id-select-indent-modes))
      nil
    (save-excursion
      (goto-char pos)
      (if (and 
	   ;; Use this function only if point is on the first non-blank
	   ;; character of a block, whatever a block is for the current
	   ;; mode.
	   (cond ((eq major-mode 'kotl-mode)
		  (and (looking-at "[1-9*]") (not (kview:valid-position-p))))
		 ((or (eq major-mode 'outline-mode) selective-display)
		  (save-excursion (beginning-of-line)
				  (looking-at outline-regexp)))
		 ;; After indent in any other mode, must be on an alpha 
		 ;; or symbol-constituent character.
		 (t (looking-at "[a-zA-z]\\|\\s_")))
	   ;; Must be at the first non-whitespace character in the line.
	   (= (point) (save-excursion (id-select-back-to-indentation))))
	  (let* ((start-col (current-column))
		 (opoint (if (eq major-mode 'kotl-mode)
			     (progn (kotl-mode:to-valid-position) (point))
			   (beginning-of-line) (point))))
	    (while
		(and (zerop (forward-line 1))
		     (bolp)
		     (or (progn (id-select-back-to-indentation)
				(> (current-column) start-col))
			 ;; If in a text mode, allow outdenting, otherwise
			 ;; only include special lines here indented to the
			 ;; same point as the original line.
			 (and (or (memq major-mode id-select-text-modes)
				  (= (current-column) start-col))
			      (looking-at
			       (or (car (cdr
					 (assq
					  major-mode
					  id-select-indent-non-end-regexp-alist)))
				   "\\'"))))))
	    (if (and (looking-at
		      (or (car (cdr (assq major-mode
					  id-select-indent-end-regexp-alist)))
			  "\\'"))
		     (or (memq major-mode id-select-text-modes)
			 (= (current-column) start-col)))
		(forward-line 1))
	    (beginning-of-line)
	    ;; Mark the whole definition
	    (setq id-select-previous 'indent-def)
	    (id-select-set-region opoint (point)))))))

(defun id-select-symbol (pos)
  "Return (start . end) of a symbol at POS."
  (or (id-select-markup-pair pos)
      ;; Test for indented def here since might be on an '*' representing
      ;; an outline entry, in which case we mark entries as indented blocks.
      (id-select-indent-def pos)
      (save-excursion
	(if (memq (char-syntax (if (eobp) (preceding-char) (char-after pos)))
		  '(?w ?_))
	    (progn (setq id-select-previous 'symbol)
		   (condition-case ()
		       (let ((end (scan-sexps pos 1)))
			 (id-select-set-region
			  (min pos (scan-sexps end -1)) end))
		     (error nil)))))))

(defun id-select-sexp-start (pos)
  "Return (start . end) of sexp starting at POS."
  (or (id-select-markup-pair pos)
      (id-select-brace-def-or-declaration pos)
      (save-excursion
	(setq id-select-previous 'sexp-start)
	(condition-case ()
	    (id-select-set-region pos (scan-sexps pos 1))
	  (error nil)))))

(defun id-select-sexp-end (pos)
  "Return (start . end) of sexp ending at POS."
  (or (id-select-brace-def-or-declaration pos)
      (save-excursion
	(setq id-select-previous 'sexp-end)
	(condition-case ()
	    (id-select-set-region (scan-sexps (1+ pos) -1) (1+ pos))
	  (error nil)))))

(defun id-select-sexp (pos)
  "Return (start . end) of the sexp that POS is within."
  (setq id-select-previous 'sexp)
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(id-select-set-region (progn (backward-up-list 1) (point))
			      (progn (forward-list 1) (point)))
      (error nil))))

(defun id-select-sexp-up (pos)
  "Return (start . end) of the sexp enclosing the selected area or nil."
  (setq id-select-previous 'sexp-up)
  ;; Keep going up and backward in sexps.  This means that id-select-sexp-up
  ;; can only be called after id-select-sexp or after itself.
  (setq pos (or (car id-select-region) pos))
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(id-select-set-region (progn (backward-up-list 1) (point))
			      (progn (forward-list 1) (point)))
      (error nil))))

(defun id-select-preprocessor-def (pos)
  "Return (start . end) of a preprocessor #definition starting at POS, if any.
The major mode for each language that uses # preprocessor notation must be
included in the list, id-select-brace-modes."
  ;; Only applies in brace modes (strictly, this should apply in a subset
  ;; of brace modes, but doing it this way permits for configurability.  In
  ;; other modes, one doesn't have to use the function on a # symbol.
  (if (not (memq major-mode id-select-brace-modes))
      nil
    (setq id-select-previous 'preprocessor-def)
    (save-excursion
      (goto-char pos)
      (if (and (= (following-char) ?#)
	       ;; Must be at the first non-whitespace character in the line.
	       (= (point) (save-excursion (id-select-back-to-indentation))))
	  (progn
	    ;; Skip past continuation lines that end with a backslash.
	    (while (and (looking-at ".*\\\\\\s-*$")
			(zerop (forward-line 1))))
	    (forward-line 1)
	    ;; Include one trailing blank line, if any.
	    (if (looking-at "^[ \t\n\r]*$") (forward-line 1))
	    (id-select-set-region pos (point)))))))

;; Allow punctuation marks not followed by white-space to include
;; the previous and subsequent sexpression.  Useful in contexts such as
;; 'foo.bar'.
(defun id-select-punctuation (pos)
  "Return (start . end) region including sexpressions before and after POS, when at a punctuation character."
  (or (id-select-comment pos)
      (id-select-preprocessor-def pos)
      (id-select-brace-def-or-declaration pos) ;; Might be on a C++ ;; destructor ~.
      (save-excursion
	(setq id-select-previous 'punctuation)
	(goto-char (min (1+ pos) (point-max)))
	(if (= (char-syntax (if (eobp) (preceding-char) (char-after (point))))
	       ?\ )
	    (id-select-set-region pos (1+ pos))
	  (goto-char pos)
	  (condition-case ()
	      (id-select-set-region
	       (save-excursion (backward-sexp) (point))
	       (progn (forward-sexp) (point)))
	    (error nil))))))

(defun id-select-comment (pos)
  "Return rest of line from POS to newline."
  (setq id-select-previous 'comment)
  (save-excursion
    (goto-char pos)
    (let ((start-regexp  (if (stringp comment-start)
			     (regexp-quote comment-start)))
	  (end-regexp    (if (stringp comment-end)
			     (regexp-quote comment-end)))
	  bolp)
      (cond
       ;; Beginning of a comment
       ((and (stringp comment-start)
	     (or (looking-at start-regexp)
		 (and (skip-chars-backward comment-start)
		      (looking-at start-regexp))))
	(skip-chars-backward " \t")
	(setq bolp (bolp)
	      pos (point))
	(if (equal comment-end "")
	    (progn (end-of-line)
		   (id-select-set-region pos (point)))
	  (if (stringp comment-end)
	      ;; Skip over nested comments.
	      (let ((count 0)
		    (regexp (concat start-regexp "\\|" end-regexp)))
		(catch 'done
		  (while (re-search-forward regexp nil t)
		    (if (string-equal
			 (buffer-substring (match-beginning 0) (match-end 0))
			 comment-start)
			(setq count (1+ count))
		      ;; End comment
		      (setq count (1- count))
		      (if (= count 0)
			  (progn
			    (if (looking-at "[ \t]*[\n\r]")
				;; Don't include final newline unless the
				;; comment is first thing on its line.
				(goto-char (if bolp (match-end 0)
					     (1- (match-end 0)))))
			    (throw 'done (id-select-set-region
					  pos (point))))))))))))
       ;; End of a comment
       ((and (stringp comment-end)
	     (not (string-equal comment-end ""))
	     (or (looking-at end-regexp)
		 (and (skip-chars-backward comment-end)
		      (looking-at end-regexp))))
	(goto-char (match-end 0))
	(if (looking-at "[ \t]*[\n\r]")
	    (goto-char (match-end 0)))
	(setq pos (point))
	(skip-chars-forward " \t")
	;; Skip over nested comments.
	(let ((count 0)
	      (regexp (concat start-regexp "\\|" end-regexp)))
	  (catch 'done
	    (while (re-search-backward regexp nil t)
	      (if (string-equal
		   (buffer-substring (match-beginning 0) (match-end 0))
		   comment-end)
		  (setq count (1+ count))
		;; Begin comment
		(setq count (1- count))
		(if (= count 0)
		    (progn
		      (skip-chars-backward " \t")
		      ;; Don't include final newline unless the comment is
		      ;; first thing on its line.
		      (if (bolp) nil (setq pos (1- pos)))
		      (throw 'done (id-select-set-region
				    (point) pos)))))))))))))

;;;
;;; Textual selections
;;;

(defun id-select-word (pos)
  "Return (start . end) of word at POS."
  (or (id-select-brace-def-or-declaration pos)
      (id-select-indent-def pos)
      (progn (setq id-select-previous 'word)
	     (save-excursion
	       (goto-char pos)
	       (forward-word 1)
	       (let ((end (point)))
		 (forward-word -1)
		 (id-select-set-region (point) end))))))

(defun id-select-string (pos)
  "Returns (start . end) of string at POS or nil.  Pos include delimiters.
Delimiters may be single, double or open and close quotes."
  (setq id-select-previous 'string)
  (save-excursion
    (goto-char pos)
    (if (and (memq major-mode id-select-markup-modes)
	     (/= (following-char) ?\")
	     (save-excursion
	       (and (re-search-backward "[<>]" nil t)
		    (= (following-char) ?>))))
	(progn (setq id-select-previous 'text)
	       (search-backward ">" nil t)
	       (id-select-set-region
		(1+ (point))
		(progn (if (search-forward "<" nil 'end)
			   (1- (point))
			 (point)))))
      (or (id-select-string-p) (id-select-string-p "'" "'")
	  (id-select-string-p "`" "'")))))

(defun id-select-sentence (pos)
  "Return (start . end) of the sentence at POS."
  (setq id-select-previous 'sentence)
  (save-excursion 
    (goto-char pos)
    (condition-case ()
	(id-select-set-region (progn (backward-sentence) (point))
			      (progn (forward-sentence) (point)))
      (error nil))))

(defun id-select-whitespace (pos)
  "Return (start . end) of all but one char of whitespace POS, unless 
there is only one character of whitespace or this is leading whitespace on
the line.  Then return all of it."
  (setq id-select-previous 'whitespace)
  (save-excursion
    (goto-char pos)
    (if (= (following-char) ?\^L)
	(id-select-page pos)
      (let ((end (progn (skip-chars-forward " \t") (point)))
	    (start (progn (skip-chars-backward " \t") (point))))
	(if (looking-at "[ \t]")
	    (if (or (bolp) (= (1+ start) end))
		(id-select-set-region start end)
	      (id-select-set-region (1+ start) end)))))))

(defun id-select-markup-pair (pos)
  "Return (start . end) of region between the opening and closing of an HTML or SGML tag pair, one of which is at POS.
The major mode for each language that uses such tags must be included in the
list, id-select-markup-modes."
  (if (not (memq major-mode id-select-markup-modes))
      nil
    (setq id-select-previous 'markup-pair)
    (let ((pos-with-space)
	  ;; Assume case of tag names is irrelevant.
	  (case-fold-search t)
	  (result)
	  start-regexp
	  end-regexp
	  bolp
	  opoint)
      (save-excursion
	(catch 'done
	  (goto-char pos)
	  (cond
	   ;; Beginning of a tag pair
	   ((looking-at "<[^/][^<> \t\n\r]*")
	    (setq start-regexp (regexp-quote (buffer-substring
					      (match-beginning 0) (match-end 0)))
		  end-regexp   (concat "</" (substring start-regexp 1)))
	    (setq pos (point))
	    (skip-chars-backward " \t")
	    (setq bolp (bolp)
		  pos-with-space (point))
	    ;; Skip over nested tags.
	    (let ((count 0)
		  (regexp (concat start-regexp "\\|" end-regexp)))
	      (while (and (>= count 0)
			  (re-search-forward regexp nil t))
		(if (/= (char-after (1+ (match-beginning 0))) ?/)
		    ;; Start tag
		    (setq count (1+ count))
		  ;; Move past end tag terminator
		  (setq opoint (point))
		  (if (or (not (re-search-forward "[<>]" nil t))
			  (= (preceding-char) ?<))
		      (progn (setq result opoint)
			     (throw 'done nil)))
		  (setq count (1- count))
		  (if (= count 0)
		      (progn
			(if (looking-at "[ \t]*[\n\r]")
			    ;; Don't include final newline unless the
			    ;; start tag was the first thing on its line.
			    (if bolp
				(progn (goto-char (match-end 0))
				       ;; Include leading space since the
				       ;; start and end tags begin and end
				       ;; lines.
				       (setq pos pos-with-space))
			      (goto-char (1- (match-end 0)))))
			(setq result (id-select-set-region pos (point)))
			(throw 'done nil)))))))
	   ;;
	   ;; End of a tag pair
	   ((or (looking-at "</[^> \t\n\r]+")
		(and (skip-chars-backward "<")
		     (looking-at "</[^> \t\n\r]+")))
	    (goto-char (match-end 0))
	    (setq end-regexp (regexp-quote (buffer-substring
					    (match-beginning 0) (match-end 0)))
		  start-regexp   (concat "<" (substring end-regexp 2)))
	    (setq opoint (point))
	    (if (or (not (re-search-forward "[<>]" nil t))
		    (= (preceding-char) ?<))
		(progn (setq result opoint)
		       (throw 'done nil)))
	    (setq pos (point))
	    (if (looking-at "[ \t]*[\n\r]")
		(setq pos-with-space (match-end 0)))
	    ;; Skip over nested tags.
	    (let ((count 0)
		  (regexp (concat start-regexp "\\|" end-regexp)))
	      (while (and (>= count 0)
			  (re-search-backward regexp nil t))
		(if (= (char-after (1+ (point))) ?/)
		    ;; End tag
		    (setq count (1+ count))
		  ;; Start tag
		  (setq count (1- count))
		  (if (= count 0)
		      (progn
			(if pos-with-space
			    ;; Newline found after original end tag.
			    (progn 
			      (skip-chars-backward " \t")
			      (if (bolp)
				  ;; Don't include final newline unless the
				  ;; start tag is the first thing on its line.
				  (setq pos pos-with-space)
				(setq pos (1- pos-with-space))
				;; Don't include non-leading space.
				(skip-chars-forward " \t"))))
			(setq result (id-select-set-region (point) pos))
			(throw 'done nil))))))))))
      (if (integerp result)
	  (progn (goto-char result)
		 (error "(id-select-markup-pair): Add a terminator character for this end tag"))
	result))))

;;;
;;; Document selections
;;;

(defun id-select-line (pos)
  "Return (start . end) of the whole line POS is in, with newline unless at end of buffer."
  (setq id-select-previous 'line)
  (save-excursion
    (goto-char pos)
    (let* ((start (progn (beginning-of-line 1) (point)))
	   (end (progn (forward-line 1) (point))))
      (id-select-set-region start end))))

(defun id-select-paragraph (pos)
  "Return (start . end) of the paragraph at POS."
  (setq id-select-previous 'paragraph)
  (save-excursion 
    (goto-char pos)
    (id-select-set-region (progn (backward-paragraph) (point))
			  (progn (forward-paragraph) (point)))))

(defun id-select-page (pos)
  "Return (start . end) of the page preceding POS."
  (setq id-select-previous 'page)
  (save-excursion 
    (goto-char pos)
    (id-select-set-region (progn (backward-page) (point))
			  (progn (forward-page) (point)))))

(defun id-select-buffer (pos)
  "Return (start . end) of the buffer at POS."
  (setq id-select-previous 'buffer)
  (id-select-set-region (point-min) (point-max)))

;;; ************************************************************************
;;; Private variables
;;; ************************************************************************

(defvar id-select-bigger-alist
  '((char nil)
    (whitespace id-select-whitespace)
    (word id-select-word)
    (symbol id-select-symbol)
    (punctuation nil)
    (string id-select-string)
    (text nil)
    (comment id-select-comment)
    (markup-pair nil)
    (preprocessor-def nil)
    (sexp id-select-sexp)
    (sexp-start nil)
    (sexp-end nil)
    (sexp-up id-select-sexp-up)
    (line id-select-line)
    (sentence id-select-sentence)
    (brace-def-or-declaration id-select-brace-def-or-declaration)
    (indent-def id-select-indent-def)
    (paragraph id-select-paragraph)
    (page id-select-page)
    (buffer id-select-buffer)
    )
  "List of (REGION-TYPE-SYMBOL REGION-SELECTION-FUNCTION) pairs.
Used to go from one thing to a bigger thing.  See id-select-bigger-thing.
Nil value for REGION-SELECTION-FUNCTION means that region type is skipped
over when trying to grow the region and is only used when a selection is made
with point on a character that triggers that type of selection.  Ordering of
entries is largely irrelevant to any code that uses this list.")


(defvar id-select-prior-buffer nil)
(defvar id-select-prior-point nil)

(defvar id-select-previous 'char
  "Most recent type of selection.  Must be set by all id-select functions.")

(defvar id-select-region (cons 'nil 'nil)
  "Cons cell that contains a region (<beginning> . <end>).
The function `id-select-set-region' updates and returns it.")

(defvar id-select-old-region (cons 'nil 'nil)
  "Cons cell that contains a region (<beginning> . <end>).")

(defcustom id-select-syntax-alist
  '((?w  id-select-word)
    (?_  id-select-symbol)
    (?\" id-select-string)
    (?\( id-select-sexp-start)
    (?\$ id-select-sexp-start)
    (?'  id-select-sexp-start)
    (?\) id-select-sexp-end)
    (?   id-select-whitespace)
    (?<  id-select-comment)
    (?.  id-select-punctuation))
  "*List of pairs of the form (SYNTAX-CHAR FUNCTION) used by the function `id-select-syntactical-region'.
Each FUNCTION takes a single position argument and returns a region
(start . end) delineating the boundaries of the thing at that position.
Ordering of entries is largely irrelevant to any code that uses this list."
  :type '(repeat (list (sexp :tag "Syntax-Char" function)))
  :group 'id-select)


(provide 'id-select)
