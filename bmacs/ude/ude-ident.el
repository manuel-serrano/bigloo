;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-ident.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 12 21:43:15 1998                          */
;*    Last change :  Tue Jan 28 11:03:20 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Ude identifier handling.                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-ident)
(require 'ude-config)
(require 'ude-custom)
(require 'ude-balloon)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-ident-regexp ...                                             */
;*---------------------------------------------------------------------*/
(defun ude-ident-regexp ()
  (if (stringp ude-extra-identifier-chars)
      (concat "\\(?:\\w\\|" ude-extra-identifier-chars "\\)")
    "\\w"))

;*---------------------------------------------------------------------*/
;*    ude-fetch-identifier-region ...                                  */
;*    -------------------------------------------------------------    */
;*    Fetch the region of the identifier at POS.                       */
;*---------------------------------------------------------------------*/
(defun ude-fetch-identifier-region (pos)
  "Fetch the region identifier at POS."
  (interactive "dPosition: ")
  (save-excursion
    (goto-char pos)
    ;; we start skipping left until we found a separator
    (let* ((min (save-excursion (beginning-of-line) (point)))
	   (max (save-excursion (end-of-line) (point))))
      (if (looking-at (ude-ident-regexp))
	  ;; we could be located on a identifier we go left until we found
	  ;; a non identifier character
	  (progn
	    (or (= (point) (point-min))
		(save-excursion
		  (backward-char 1)
		  (and (looking-at "\\W")
		       (or (not (stringp ude-extra-identifier-chars))
			   (not (looking-at ude-extra-identifier-chars)))))
		(ude-backward-word))
	    (if (or (= (point) min)
		    (progn
		      (backward-char 1)
		      (looking-at "\\s-\\|(\\|:\\|'\\|,")))
		(let ((start (if (looking-at "\\s-\\|(\\|:\\|'\\|,")
				 (+ 1 (point))
			       (point))))
		  ;; we have to look right
		  (goto-char pos)
		  (ude-forward-word)
		  (if (or (= (point) max)
			  (looking-at "\\s-\\|\\s(\\|\\s)\\|::"))
		      ;; we are not sure yet we have to check that it
		      ;; is not a number
		      (let ((end (point)))
			(goto-char start)
			(skip-chars-forward "0-9")
			(if (= (point) end)
			    ;; this is a number
			    nil
			  ;; yes, this is correct
			  (progn
			    (goto-char start)
			    (if (and (> (- end 2) (point))
				     (search-forward "::" (- end 2) t))
				(cons start (match-beginning 0))
			      (cons start end)))))
		    nil))
	      nil))
	nil))))
  
;*---------------------------------------------------------------------*/
;*    ude-fetch-identifier ...                                         */
;*    -------------------------------------------------------------    */
;*    Fetch the identifier at POS.                                     */
;*---------------------------------------------------------------------*/
(defun ude-fetch-identifier (pos)
  "Fetch the Bigloo identifier at POS."
  (interactive "dPosition: ")
  (let ((region (ude-fetch-identifier-region pos)))
    (if (consp region)
	(buffer-substring-no-properties (car region) (cdr region))
      nil)))

;*---------------------------------------------------------------------*/
;*    ude-forward-word ...                                             */
;*    -------------------------------------------------------------    */
;*    Forward one word using additional identifier chars               */
;*---------------------------------------------------------------------*/
(defun ude-forward-word ()
  (forward-word 1)
  (if (stringp ude-extra-identifier-chars)
      (while (looking-at ude-extra-identifier-chars)
	(forward-word 1))))
  
;*---------------------------------------------------------------------*/
;*    ude-backward-word ...                                            */
;*    -------------------------------------------------------------    */
;*    Forward one word using additional identifier chars               */
;*---------------------------------------------------------------------*/
(defun ude-backward-word ()
  (backward-word 1)
  (if (stringp ude-extra-identifier-chars)
      (while (and (> (point) (point-min))
		  (save-excursion
		    (backward-char 1)
		    (looking-at ude-extra-identifier-chars)))
	(backward-word 1))))

;*---------------------------------------------------------------------*/
;*    ude-interactive-ident ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-interactive-ident (pos prompt)
  (list (ude-fetch-then-request-identifier pos prompt)))
				  
;*---------------------------------------------------------------------*/
;*    ude-fetch-then-request-identifier ...                            */
;*    -------------------------------------------------------------    */
;*    This function try to fetch an identifier. If it fails, the       */
;*    function ask the user for an identifier.                         */
;*---------------------------------------------------------------------*/
(defun ude-fetch-then-request-identifier (pos &optional prompt) 
  "Fetch the identifier at POS and when failure, ask for an identifier."
  (interactive "dPosition: ")
  (let ((ident (ude-fetch-identifier pos)))
    (if (stringp ident)
	ident
      (if (not prompt)
	  (read-string "Find: ")
	(read-string prompt)))))

;*---------------------------------------------------------------------*/
;*    ude-request-identifier-at ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-request-identifier-at (&optional prompt)
  (if (not prompt)
      (setq prompt "Find: "))
  (let ((res (ude-fetch-identifier (point))))
    (if res
	res
      (read-string prompt))))

;*---------------------------------------------------------------------*/
;*    ude-tags-balloon-region ...                                      */
;*---------------------------------------------------------------------*/
(defvar ude-tags-balloon-region nil)
(make-variable-buffer-local 'ude-tags-balloon-region)

;*---------------------------------------------------------------------*/
;*    ude-make-ident-balloon-extent ...                                */
;*---------------------------------------------------------------------*/
(defun ude-make-ident-balloon-extent (pos region)
  ;; we enable all balloon positions
  (setq ude-tags-disabled nil)
  ;; remove the last ballon
  (ude-tags-balloon-delete)
  ;; we construct the new balloon
  (let ((mod (buffer-modified-p)))
    (put-text-properties (car region) (cdr region)
			 'ude-balloon-ident t
			 'mouse-face 'highlight
			 'keymap ude-tags-balloon-mouse-map
			 'help-echo "mouse-2: Find\nmouse-3: Menu...")
    (set-buffer-modified-p mod))
  (setq ude-tags-ident
	(buffer-substring-no-properties (car region) (cdr region)))
  ;; store the new position
  (setq ude-tags-balloon-region (cons (current-buffer) region)))

;*---------------------------------------------------------------------*/
;*    doc source keymap                                                */
;*---------------------------------------------------------------------*/
(defvar ude-tags-balloon-find nil)
(make-variable-buffer-local 'ude-tags-balloon-find)

(defvar ude-tags-balloon-menu nil)
(make-variable-buffer-local 'ude-tags-balloon-menu)

(defvar ude-tags-in-comment-p nil)
(make-variable-buffer-local 'ude-tags-in-comment-p)

(defvar ude-tags-ident nil)
(make-variable-buffer-local 'ude-tags-ident)

(defvar ude-tags-disabled nil)
(make-variable-buffer-local 'ude-tags-disabled)

(defvar ude-tags-balloon-mouse-map (make-sparse-keymap))
;; mouse-2
(define-key ude-tags-balloon-mouse-map ude-mouse-2-binding
  (function ude-tags-balloon-find))
(if ude-mouse-2-binding-disable
    (define-key ude-tags-balloon-mouse-map ude-mouse-2-binding-disable
      #'(lambda (e) (interactive "e"))))
;; mouse-3
(define-key ude-tags-balloon-mouse-map ude-mouse-binding
  (function ude-tags-balloon-menu))
(if ude-mouse-binding-disable
    (define-key ude-tags-balloon-mouse-map ude-mouse-binding-disable
      #'(lambda (e) (interactive "e"))))

;*---------------------------------------------------------------------*/
;*    ude-tags-balloon-remove ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-tags-balloon-remove (event)
  (interactive "e")
  (let* ((point   (event-closest-point event))
	 (buffer  (event-buffer event))
	 (current (current-buffer)))
    (set-buffer buffer)
    ;; we store the disabled position
    (setq ude-tags-disabled (cons buffer point))
    (set-buffer current)
    (ude-tags-balloon-delete)))

;*---------------------------------------------------------------------*/
;*    ude-tags-disabled-p ...                                          */
;*    -------------------------------------------------------------    */
;*    Is the balloon disabled at the requested position?               */
;*    Actually the balloon is disabled around the position where       */
;*    mouse-3 has been clicked. Around is defined by the click         */
;*    position +/- 1.                                                  */
;*---------------------------------------------------------------------*/
(defun ude-tags-disabled-p (buffer pos)
  (and (consp ude-tags-disabled)
       (eq (car ude-tags-disabled) buffer)
       (let ((point (cdr ude-tags-disabled)))
	 (or (eq point pos)
	     (eq (+ point 1) pos)
	     (eq (- point 1) pos)))))

;*---------------------------------------------------------------------*/
;*    ude-tags-balloon-find ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-tags-balloon-find (event)
  (interactive "e")
  (let ((point (event-closest-point event)))
    (funcall ude-tags-balloon-find point ude-tags-ident)))

;*---------------------------------------------------------------------*/
;*    ude-tags-balloon-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-tags-balloon-menu (event)
  (interactive "e")
  (let ((point (event-closest-point event)))
    (funcall ude-tags-balloon-menu point ude-tags-ident)))

;*---------------------------------------------------------------------*/
;*    ude-tags-balloon-delete ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-tags-balloon-delete (&optional frame)
  (if (consp ude-tags-balloon-region)
      (let ((buffer (car ude-tags-balloon-region))
	    (region (cdr ude-tags-balloon-region))
	    (cbuf (current-buffer)))
	(set-buffer buffer)
	(let ((mod (buffer-modified-p))
	      (l '(help-echo ude-balloon-ident mouse-face keymap)))
	  (while (consp l)
	    (remove-text-property (car region) (cdr region) (car l))
	    (setq l (cdr l)))
	  (set-buffer-modified-p mod))
	(set-buffer cbuf)
	(setq ude-tags-balloon-region nil))))

;*---------------------------------------------------------------------*/
;*    ude-tags-balloon-start ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-tags-balloon-start (mode-ident comment-p find-def menu-def)
  (setq ude-tags-balloon-find find-def)
  (setq ude-tags-balloon-menu menu-def)
  (setq ude-tags-in-comment-p (if comment-p comment-p #'(lambda (_) nil)))
  (ude-balloon-start)
  (ude-add-balloon-action
   mode-ident
   #'(lambda (x win)
       (and (functionp ude-tags-in-comment-p)
	    (eq (ude-balloon-get-buffer) (current-buffer))))
   #'(lambda ()
       (let* ((buffer (ude-balloon-get-buffer))
	      (pos    (ude-balloon-get-point)))
	 (if (not (ude-tags-disabled-p buffer pos))
	     (if (and (numberp pos)
		      (not (funcall ude-tags-in-comment-p pos)))
		 (if (and (bufferp buffer) (numberp pos))
		     (let ((region (ude-fetch-identifier-region pos)))
		       (if (and (consp region)
				(numberp (car region))
				(numberp (cdr region)))
			   (ude-make-ident-balloon-extent pos region))))))))))

  
