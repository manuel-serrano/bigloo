;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-connect.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  6 11:25:49 1998                          */
;*    Last change :  Fri Nov 29 17:58:52 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between bug and the source buffers.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bug-connect)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require (if (featurep 'xemacs) 'bug-xemacs 'bug-gnu-emacs))
(require 'bug-config)
(require 'bug-autoload)
(require 'bug)
(require 'bee-autoload)
(require 'ude-autoload)

;*---------------------------------------------------------------------*/
;*    bug-connected-buffers ...                                        */
;*    -------------------------------------------------------------    */
;*    The list of connected buffers.                                   */
;*---------------------------------------------------------------------*/
(defvar bug-connected-buffers '())

;*---------------------------------------------------------------------*/
;*    bug-connected-buffer ...                                         */
;*    -------------------------------------------------------------    */
;*    A buffer local variable that help the implementation of the      */
;*    bug-connected-buffer-p predicate.                                */
;*---------------------------------------------------------------------*/
(defvar bug-connected-buffer nil)
(make-variable-buffer-local 'bug-connected-buffer)

;*---------------------------------------------------------------------*/
;*    bug-connected-buffer-p ...                                       */
;*    -------------------------------------------------------------    */
;*    Is the current buffer connected?                                 */
;*---------------------------------------------------------------------*/
(defun bug-connected-buffer-p ()
  bug-connected-buffer)

;*---------------------------------------------------------------------*/
;*    bug-connect-hooks ...                                            */
;*---------------------------------------------------------------------*/
(defvar bug-connect-hooks ()
  "The list of hooks to run when connecting a buffer.")

;*---------------------------------------------------------------------*/
;*    bug-disconnect-hooks ...                                         */
;*---------------------------------------------------------------------*/
(defvar bug-disconnect-hooks ()
  "The list of hooks to run when disconnecting a buffer.")

;*---------------------------------------------------------------------*/
;*    bug-add-connect-hook ...                                         */
;*---------------------------------------------------------------------*/
(defun bug-add-connect-hook (mode hook)
  (setq bug-connect-hooks (cons (cons mode hook) bug-connect-hooks)))

;*---------------------------------------------------------------------*/
;*    bug-add-disconnect-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun bug-add-disconnect-hook (mode hook)
  (setq bug-disconnect-hooks (cons (cons mode hook) bug-disconnect-hooks)))

;*---------------------------------------------------------------------*/
;*    bug-line-number ...                                              */
;*    -------------------------------------------------------------    */
;*    This function returns the point line number.                     */
;*---------------------------------------------------------------------*/
(defun bug-line-number (buffer pos)
  (save-restriction
    (set-buffer buffer)
    (let (start)
      (save-excursion
	(save-restriction
	  (goto-char (point-min))
	  (widen)
	  (beginning-of-line)
	  (setq start (point))
	  (goto-char pos)
	  (beginning-of-line)
	  (if (/= start 1) (1+ (count-lines 1 (point)))
	    (1+ (count-lines 1 (point)))))))))

;*---------------------------------------------------------------------*/
;*    bug-breakpoint-menu ...                                          */
;*---------------------------------------------------------------------*/
(defun bug-breakpoint-menu (event)
  (save-excursion
    (let* ((buffer (event-buffer event))
	   (file   (buffer-file-name buffer))
	   (fname  (file-name-nondirectory file))
	   (class  (bug-file-to-class fname))
	   (pos    (event-closest-point event))
	   (line   (bug-line-number buffer pos)))
      (popup-menu
       (list (format "%s, line %s" fname line)
	     (vector "Set breakpoint"
		     `(bug-remote-call (bug-break-command ,class ,line ,fname))
		     t)
	     (vector "Set temporary breakpoint"
		     `(bug-remote-call (bug-tbreak-command ,class ,line ,fname))
		     t)
	     "-"
	     (vector "Set footprint"
		     `(bug-remote-call (bug-footprint-command ,class ,line ,fname))
		     t)
	     "-"
	     (vector "Continue until here"
		     `(bug-remote-call (bug-until-command ,class ,line ,fname))
		     t))))))

;*---------------------------------------------------------------------*/
;*    bug-connect-add-bindings ...                                     */
;*    -------------------------------------------------------------    */
;*    Add the key binding for the connected buffer.                    */
;*---------------------------------------------------------------------*/
(defun bug-connect-add-bindings ()
  (ude-add-menu #'(lambda (event) (bug-mouse-event-margin-p event))
		#'bug-breakpoint-menu))

;*---------------------------------------------------------------------*/
;*    bug-toggle-margin ...                                            */
;*---------------------------------------------------------------------*/
(defun bug-toggle-margin (buffer &optional val)
  "Toggle the left margin on and off."
  (cond
   ((not val)
    (if (bug-margin-off-p)
	(bug-toggle-margin-on buffer)
      (bug-toggle-margin-off buffer)))
   ((and (numberp val) (= val 0))
    (bug-toggle-margin-off buffer))
   (t
    (if (bug-margin-off-p)
	(bug-toggle-margin-on buffer)))))

;*---------------------------------------------------------------------*/
;*    bug-connect-buffer ...                                           */
;*    -------------------------------------------------------------    */
;*    This function connect a buffer, that is, some buffer bindings    */
;*    are changed and the margin is setup.                             */
;*---------------------------------------------------------------------*/
(defun bug-connect-buffer (buffer)
  (interactive "BBuffer: ")
  ;; because it may happens that latency make emacs got confused,
  ;; we check that it is not the comint buffer that we are trying
  ;; to connect. If so, we simply ignore the request
  (cond
   ((not (eq buffer bug-comint-buffer))
    (if (stringp buffer)
	(setq buffer (get-buffer buffer)))
    (if (buffer-live-p buffer)
	(progn
	  (set-buffer buffer)
	  (if (not bug-connected-buffer)
	      (progn
		;; we register this buffer as connected to bug
		(setq bug-connected-buffers
		      (cons buffer bug-connected-buffers))
		;; we mark this buffer as connected to bug
		(setq bug-connected-buffer t)
		;; we draw the margin
		(bug-toggle-margin buffer t)
		;; add the connected bindings
		(bug-connect-add-bindings)
		;; mode specific initialization
		(let ((cell (assq major-mode bug-connect-hooks)))
		  (if (consp cell)
		      (funcall (cdr cell))))
		t)
	    (bug-toggle-margin buffer 1)))
      nil))
   (t
    nil)))

;*---------------------------------------------------------------------*/
;*    bug-connect-file ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-connect-file (file)
  (interactive "fConnect to file: ")
  (let ((buffer (find-file-noselect file)))
    (if (bufferp buffer)
	(let ((pop-up-frames t))
	  (pop-to-buffer (buffer-name buffer))
	  (bug-connect-buffer buffer)))))
  
;*---------------------------------------------------------------------*/
;*    bug-disconnect-buffer ...                                        */
;*    -------------------------------------------------------------    */
;*    This function connect a buffer, that is, some buffer bindings    */
;*    are changed and the margin is setup.                             */
;*---------------------------------------------------------------------*/
(defun bug-disconnect-buffer (buffer)
  (interactive "BBuffer: ")
  (if (buffer-live-p buffer)
      (progn
	(set-buffer buffer)
	(if bug-connected-buffer
	    (progn
	      ;; we remove the bug region popup entry
	      (let ((cell (assq major-mode bug-disconnect-hooks)))
		(if (consp cell)
		    (funcall (cdr cell))))
	      ;; we mark this buffer as connected to bug
	      (setq bug-connected-buffer nil)
	      ;; don't show the active line anymore
	      (bug-unshow-line)
	      ;; we draw the margin
	      (bug-toggle-margin-off buffer)
	      ;; we set bug mouse bindings
	      (local-unset-key ude-mouse-2-binding)
	      ;; remove the buffer from the connecte list
	      (setq bug-connected-buffers (delq buffer bug-connected-buffers))
	      ;; we redisplay the buffer
	      (sit-for 0))))))
	    
;*---------------------------------------------------------------------*/
;*    bug-toggle-connect-buffer ...                                    */
;*---------------------------------------------------------------------*/
(defun bug-toggle-connect-buffer ()
  (interactive)
  (let ((buffer (current-buffer)))
    (if (bug-connected-buffer-p)
	(bug-disconnect-buffer buffer)
      (bug-connect-buffer buffer))))

;*---------------------------------------------------------------------*/
;*    bug-disconnect-all-buffers ...                                   */
;*    -------------------------------------------------------------    */
;*    Disconnect all currently connected buffers.                      */
;*---------------------------------------------------------------------*/
(defun bug-disconnect-all-buffers ()
  (mapcar 'bug-disconnect-buffer bug-connected-buffers))
  
