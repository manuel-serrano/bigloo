;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-filter.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May  9 15:30:30 2002                          */
;*    Last change :  Tue Sep 20 06:06:05 2005 (serrano)                */
;*    Copyright   :  2002-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Bugloo output filter.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-filter)
(require 'ude-custom)
(require 'bug-custom)
(require 'bug-autoload)
(require 'bug-config)
(require 'bug-images)
(require 'bee-autoload)

;*---------------------------------------------------------------------*/
;*    Global filtering control variables                               */
;*---------------------------------------------------------------------*/
;; the bug filtering (parsing) state
(defvar bug-filter-states '())

;; the pending outputs, that is, a string containing all
;; the character previously issued that are not already processed.
(defvar bug-filter-pending "")
(defvar bug-filter-pending-filtering 0)

;; the regexp that matches all the bugloo markups
(defvar bug-filter-regexp nil)

(defvar bug-debug nil)
(defun bug-message-box (fmt &rest r)
  (if bug-debug (apply 'message-box (cons fmt r))))

;; default output function
(defvar bug-filter-output (function comint-output-filter))

;; default prompt output function
(defvar bug-prompt-output (function comint-output-filter))

;; the hook to be applied when a prompt is raised
(defvar bug-prompt-hook (function bug-run-command-hooks))

;*---------------------------------------------------------------------*/
;*    bug-buffer-text-properties-at ...                                */
;*---------------------------------------------------------------------*/
(defun bug-buffer-text-properties-at (buf p)
  (save-excursion
    (set-buffer buf)
    (text-properties-at p)))

;*---------------------------------------------------------------------*/
;*    bug-filter-init ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-filter-init ()
  (if (not bug-filter-regexp)
      ;; the filter has not been initialized yet
      (setq bug-filter-regexp
	    (regexp-opt (append (mapcar (lambda (x)
					  (concat "<" (car x) ">"))
					bug-markups)
				(mapcar (lambda (x)
					  (concat "</" (car x) ">"))
					bug-markups))))))

;*---------------------------------------------------------------------*/
;*    bug-filter-open-markup ...                                       */
;*---------------------------------------------------------------------*/
(defun bug-filter-open-markup (string begin)
  (if (eq (string-match "<\\([^/>]+\\)>" string begin) begin)
      (substring string (match-beginning 1) (match-end 1))
    nil))

;*---------------------------------------------------------------------*/
;*    bug-filter-close-markup ...                                      */
;*---------------------------------------------------------------------*/
(defun bug-filter-close-markup (string begin)
  (if (eq (string-match "</\\([^/>]+\\)>" string begin) begin)
      (substring string (match-beginning 1) (match-end 1))
    nil))

;*---------------------------------------------------------------------*/
;*    bug-filter-push-state ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-filter-push-state (state beg)
  (bug-message-box (format "PUSHING state=%S b=%S" state beg))
  (if (not (consp (assoc state bug-markups)))
      (ude-error (format "Illegal bugloo command `<%s>'" state))
    (setq bug-filter-states (cons (cons state (+ 2 (length state) beg))
				  bug-filter-states))))

;*---------------------------------------------------------------------*/
;*    bug-filter-pop-state ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-filter-pop-state (state proc string be)
  (cond
   ((null bug-filter-states)
    (ude-error (format "Illegal Bugloo command `</%s>' (empty state)" state)))
   ((not (string-equal state (car (car bug-filter-states))))
    (ude-error (format "Illegal Bugloo command `</%s>' (expecting `</%s>')"
		       state (car (car bug-filter-states)))))
   (t
    (let* ((f (cdr (assoc state bug-markups)))
	   (b (cdr (car bug-filter-states)))
	   (o (substring string b be)))
      (bug-message-box (format "POPING state=%S o=%S" state o))
      (setq bug-filter-states (cdr bug-filter-states))
      (save-excursion
	(funcall f proc o))
      b))))

;*---------------------------------------------------------------------*/
;*    bug-filter-flush ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-filter-flush (proc string)
  (bug-message-box
   (format "FLUSHING state=%S str=%S" (car (car bug-filter-states)) string))
  (if (consp bug-filter-states)
      (let ((f (cdr (assoc (car (car bug-filter-states)) bug-markups))))
	(save-excursion (funcall f proc string))
	(length string))))

;*---------------------------------------------------------------------*/
;*    bug-filter ...                                                   */
;*---------------------------------------------------------------------*/
(defun bug-filter (proc string)
  (if (functionp bug-custom-filter)
      (save-excursion
	(funcall bug-custom-filter proc string))
    (progn
      (bug-message-box
       (format "bug-filter [string=%S] [pending=%S] [offset=%S]"
	       string
	       bug-filter-pending
	       bug-filter-pending-filtering))
      ;; initialize the filtering
      (bug-filter-init)
      ;; try to see if we have found a markup
      (let ((o (concat bug-filter-pending string)))
	(let ((i bug-filter-pending-filtering))
	  (while (string-match bug-filter-regexp o i)
	    ;; there is a match
	    (let ((b (match-beginning 0))
		  (e (match-end 0)))
	      (let ((state (bug-filter-open-markup o b)))
		(if state
		    ;; this is an open markup, we push the state
		    (progn
		      (if (> b i)
			  (progn
			    (bug-message-box (format "flush: len=%S i=%S b=%S"
						     (length o) i b))
			    (bug-filter-flush proc (substring o i b))))
		      (bug-filter-push-state state b)
		      (setq i e))
		  (let ((state (bug-filter-close-markup o b)))
		    (bug-filter-pop-state state proc o b)
		    (if (consp bug-filter-states)
			(let* ((hd (car bug-filter-states))
			       (state (car hd)))
			  (setq bug-filter-states
				(cons (cons state e)
				      (cdr bug-filter-states)))))
		    (setq i e))))))
	  ;; there is no more match, we have to wait further outputs
	  (if (null bug-filter-states)
	      (progn
		(setq bug-filter-pending (substring o i (length o)))
		(setq bug-filter-pending-filtering 0))
	    (progn
	      (setq bug-filter-pending o)
	      (setq bug-filter-pending-filtering i)))
	  (accept-process-output proc bug-sec-timeout bug-msec-timeout))))))

;*---------------------------------------------------------------------*/
;*    bug-filter-default ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-filter-default (proc output)
  (funcall bug-filter-output proc output))

;*---------------------------------------------------------------------*/
;*    bug-filter-command ...                                           */
;*---------------------------------------------------------------------*/
(defun bug-filter-command (proc output)
  (bug-message-box (format "*** comint-filter-command: [%S]" output))
  (funcall bug-filter-output proc output))

;*---------------------------------------------------------------------*/
;*    bug-filter-prompt ...                                            */
;*---------------------------------------------------------------------*/
(defun bug-filter-prompt (proc output)
  (bug-message-box (format "*** comint-filter-prompt: [%S]" output))
  (funcall bug-prompt-output proc output)
  (funcall bug-prompt-hook))

;*---------------------------------------------------------------------*/
;*    bug-filter-error ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-filter-error (proc output)
  (insert-text-property 0 (length output) 'face 'bug-error-face output)
  (funcall bug-filter-output proc output))

;*---------------------------------------------------------------------*/
;*    bug-filter-output ...                                            */
;*---------------------------------------------------------------------*/
(defun bug-filter-output (proc output)
  (insert-text-property 0 (length output) 'face 'bug-output-face output)
  (funcall bug-filter-output proc output))

;*---------------------------------------------------------------------*/
;*    bug-filter-location ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-filter-location (proc output)
  (if (string-match bug-linespec-regexp output)
      (let ((f (substring output (match-beginning 1) (match-end 1)))
	    (l (substring output (match-beginning 2) (match-end 2))))
	(bug-prompt-file-line f (string-to-number l)))))

;*---------------------------------------------------------------------*/
;*    bug-filter-bp ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-filter-bp (proc output)
  (if (string-match bug-breakpoint-regexp output)
      (let ((n (substring output (match-beginning 1) (match-end 1)))
	    (c (substring output (match-beginning 2) (match-end 2)))
	    (f (substring output (match-beginning 3) (match-end 3)))
	    (l (substring output (match-beginning 4) (match-end 4)))
	    (s (substring output (match-beginning 5) (match-end 5))))
	(bug-breakpoint 'bp (string-to-number n) f c (string-to-number l) (intern s)))))

;*---------------------------------------------------------------------*/
;*    bug-filter-fp ...                                                */
;*---------------------------------------------------------------------*/
(defun bug-filter-fp (proc output)
  (if (string-match bug-breakpoint-regexp output)
      (let ((n (substring output (match-beginning 1) (match-end 1)))
	    (c (substring output (match-beginning 2) (match-end 2)))
	    (f (substring output (match-beginning 3) (match-end 3)))
	    (l (substring output (match-beginning 4) (match-end 4)))
	    (s (substring output (match-beginning 5) (match-end 5))))
	(bug-breakpoint 'fp (string-to-number n) f c (string-to-number l) (intern s)))))

;*---------------------------------------------------------------------*/
;*    bug-filter-footprint-map ...                                     */
;*---------------------------------------------------------------------*/
(defvar bug-filter-footprint-map (make-sparse-keymap))
(define-key bug-filter-footprint-map ude-mouse-binding
  #'(lambda (e)
      (interactive "e")
      (let* ((p (event-closest-point e))
	     (l (memq 'bug-footprint-info
		      (bug-buffer-text-properties-at (event-buffer e) p))))
	(if (consp l)
	    (let ((bp (car (cdr l))))
	      (if (string-match bug-fpspec-regexp bp)
		  (let ((n (string-to-number (match-string-no-properties 1 bp))))
		    (bug-footprint-popup-menu nil nil n 'all))))))))
		  
;*---------------------------------------------------------------------*/
;*    bug-filter-footprint ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-filter-footprint (proc output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'face 'bug-footprint-face output)
  (insert-text-property 0 (length output) 'bug-footprint-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-footprint-map output)
  (insert-text-property 0 (length output) 'help-echo "mouse-3: Edit properties" output)
  (funcall bug-filter-output proc output)
  (save-excursion
    (set-buffer bug-comint-buffer)
    (bug-add-image-overlay bug-comint-buffer (1- (point-max))
			   bug-footprint-enable-image)))
  
;*---------------------------------------------------------------------*/
;*    bug-filter-breakpoint-map ...                                    */
;*---------------------------------------------------------------------*/
(defvar bug-filter-breakpoint-map (make-sparse-keymap))
(define-key bug-filter-breakpoint-map ude-mouse-binding
  #'(lambda (e)
      (interactive "e")
      (let* ((p (event-closest-point e))
	     (l (memq 'bug-breakpoint-info
		      (bug-buffer-text-properties-at (event-buffer e) p))))
	(if (consp l)
	    (let ((bp (car (cdr l))))
	      (if (string-match bug-bpspec-regexp bp)
		  (let ((n (string-to-number (match-string-no-properties 1 bp))))
		    (bug-breakpoint-popup-menu nil nil n 'all))))))))
		  
;*---------------------------------------------------------------------*/
;*    bug-filter-breakpoint ...                                        */
;*---------------------------------------------------------------------*/
(defun bug-filter-breakpoint (proc output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'face 'bold output)
  (insert-text-property 0 (length output) 'bug-breakpoint-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-breakpoint-map output)
  (insert-text-property 0 (length output) 'help-echo "mouse-3: Edit properties" output)
  (funcall bug-filter-output proc output)
  (save-excursion
    (set-buffer bug-comint-buffer)
    (bug-add-image-overlay bug-comint-buffer (1- (point-max))
			   bug-breakpoint-red-image)))

;*---------------------------------------------------------------------*/
;*    bug-filter-ident-map ...                                         */
;*---------------------------------------------------------------------*/
(defvar bug-filter-ident-map (make-sparse-keymap))
(define-key bug-filter-ident-map ude-mouse-2-binding
  #'(lambda (e)
      (interactive "e")
      (let* ((p (event-closest-point e))
	     (l (memq 'bug-ident-info
		      (bug-buffer-text-properties-at (event-buffer e) p))))
	(if (consp l)
	    (bee-find-definition (car (cdr l)))))))
		  
;*---------------------------------------------------------------------*/
;*    bug-filter-ident ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-filter-ident (proc output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'face 'font-lock-function-name-face output)
  (insert-text-property 0 (length output) 'help-echo "mouse-2: Edit definition" output)
  (insert-text-property 0 (length output) 'bug-ident-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-ident-map output)
  (funcall bug-filter-output proc output))
  
;*---------------------------------------------------------------------*/
;*    bug-filter-type-map ...                                          */
;*---------------------------------------------------------------------*/
(defvar bug-filter-type-map (make-sparse-keymap))
(define-key bug-filter-type-map ude-mouse-2-binding
  #'(lambda (e)
      (interactive "e")
      (let* ((p (event-closest-point e))
	     (l (memq 'bug-type-info
		      (bug-buffer-text-properties-at (event-buffer e) p))))
	(if (consp l)
	    (bee-tags-find-class (car (cdr l)))))))
		  
;*---------------------------------------------------------------------*/
;*    bug-filter-type ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-filter-type (proc output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'face 'ude-font-lock-face-4 output)
  (insert-text-property 0 (length output) 'help-echo "mouse-2: Edit class" output)
  (insert-text-property 0 (length output) 'bug-type-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-type-map output)
  (funcall bug-filter-output proc output))

;*---------------------------------------------------------------------*/
;*    bug-filter-line-map ...                                          */
;*---------------------------------------------------------------------*/
(defvar bug-filter-line-map (make-sparse-keymap))
(define-key bug-filter-line-map ude-mouse-2-binding
  #'(lambda (e)
      (interactive "e")
      (let* ((p (event-closest-point e))
	     (l (memq 'bug-line-info
		      (bug-buffer-text-properties-at (event-buffer e) p))))
	(if (consp l)
	    (let ((line (car (cdr l))))
	      (if (string-match bug-linespec-regexp line)
		  (let ((c (match-string-no-properties 1 line))
			(l (match-string-no-properties 2 line)))
		    (bug-display-file-line (bug-class-to-file c)
					   (string-to-number l)))))))))
		  
;*---------------------------------------------------------------------*/
;*    bug-filter-line ...                                              */
;*---------------------------------------------------------------------*/
(defun bug-filter-line (proc output)
  (insert-text-property 0 (length output) 'face 'underline output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'help-echo "mouse-2: Edit line" output)
  (insert-text-property 0 (length output) 'bug-line-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-line-map output)
  (funcall bug-filter-output proc output))
  
;*---------------------------------------------------------------------*/
;*    bug-filter-frame-map ...                                         */
;*---------------------------------------------------------------------*/
(defvar bug-filter-frame-map (make-sparse-keymap))
(define-key bug-filter-frame-map ude-mouse-2-binding
  #'(lambda (e)
      (interactive "e")
      (let* ((p (event-closest-point e))
	     (l (memq 'bug-frame-info
		      (bug-buffer-text-properties-at (event-buffer e) p))))
	(if (consp l)
	    (let ((f (car (cdr l))))
	      (if (string-match bug-framespec-regexp f)
		  (let ((n (string-to-number (match-string-no-properties 1 f))))
		    (bug-remote-call (bug-frame-command n)))))))))
		  
;*---------------------------------------------------------------------*/
;*    bug-filter-frame ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-filter-frame (proc output)
  (insert-text-property 0 (length output) 'face 'underline output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'help-echo "mouse-2: Inspect stack frame" output)
  (insert-text-property 0 (length output) 'bug-frame-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-frame-map output)
  (funcall bug-filter-output proc output))

;*---------------------------------------------------------------------*/
;*    bug-filter-framecur ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-filter-framecur (proc output)
  (insert-text-property 0 (length output) 'face 'bug-current-frame-face output)
  (insert-text-property 0 (length output) 'mouse-face 'highlight output)
  (insert-text-property 0 (length output) 'help-echo "mouse-2: Inspect stack frame" output)
  (insert-text-property 0 (length output) 'bug-frame-info output output)
  (insert-text-property 0 (length output) 'keymap bug-filter-frame-map output)
  (funcall bug-filter-output proc output))
