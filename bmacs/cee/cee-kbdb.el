;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-kbdb.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  1 11:57:44 1999                          */
;*    Last change :  Tue Sep 20 06:06:15 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between the CEE and a visual debugger.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-kbdb)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'cee-autoload)
(require 'cee-config)
(require 'cee-gdb)
(require 'ude-autoload)
(require 'ude-config)
(require 'dbg-autoload)
(require 'plugin)
(require 'ude-balloon)

;*---------------------------------------------------------------------*/
;*    cee-kbdb-ready-p ...                                             */
;*    -------------------------------------------------------------    */
;*    This true predicate returns true iff an interface builder        */
;*    is ready. That is if the binary file for the interface           */
;*    builder exists and may be executed.                              */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-ready-p ()
  (and (stringp cee-kbdb)
       (file-exists-p cee-kbdb)
       (file-executable-p cee-kbdb)))

;*---------------------------------------------------------------------*/
;*    cee-find-kbdb ...                                                */
;*    -------------------------------------------------------------    */
;*    Finds a kbdb associated to the project. If such a kbdb           */
;*    does not exist, returns '()                                      */
;*---------------------------------------------------------------------*/
(defun cee-find-kbdb (project)
  (plugin-find-process-from-key cee-kbdb project))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-start ...                                               */
;*    -------------------------------------------------------------    */
;*    This function start a new interface kbdb. It first creates       */
;*    a module name and a file name and launch a new process.          */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-start ()
  (interactive)
  (cee-run-kbdb))

;*---------------------------------------------------------------------*/
;*    cee-run-kbdb ...                                                 */
;*    -------------------------------------------------------------    */
;*    This functions spawn a new kbdb process.                         */
;*---------------------------------------------------------------------*/
(defun cee-run-kbdb ()
  (if (cee-kbdb-ready-p)
      (cee-run-external-kbdb)
    (cee-run-internal-gdb)))

;*---------------------------------------------------------------------*/
;*    cee-run-internal-gdb ...                                         */
;*---------------------------------------------------------------------*/
(defun cee-run-internal-gdb ()
  (setq dbg-wrapper-caller 'nil)
  (cee-gdb-other-frame))

;*---------------------------------------------------------------------*/
;*    kbdb-call ...                                                    */
;*---------------------------------------------------------------------*/
(defun kbdb-call (cmd)
  (let ((plugin (plugin-find (intern cee-kbdb))))
    (if (consp plugin)
	(plugin-send-string (car (car plugin)) (format "(BDB %S)" cmd))
      (ude-error "Can't find kbdb plugin..."))))
  
;*---------------------------------------------------------------------*/
;*    kbdb-breakpoint-menu ...                                         */
;*---------------------------------------------------------------------*/
(defun kbdb-breakpoint-menu (event)
  (save-excursion
    (let* ((buffer (event-buffer event))
	   (file   (buffer-file-name buffer))
	   (fname  (file-name-nondirectory file))
	   (pos    (event-closest-point event))
	   (line   (dbg-line-number buffer pos)))
      (popup-menu
       (list (format "%s, line %s" fname line)
	     (vector "Set breakpoint"
		     (list 'kbdb-call
			   (format "%s %s:%s" dbg-break-command fname line))
		     t)
	     (vector "Set temporary breakpoint"
		     (list 'kbdb-call
			   (format "%s %s:%s" dbg-tbreak-command fname line))
		     t)
	     "-"
	     (vector "Set footprint"
		     (list 'kbdb-call
			   (format "%s %s:%s" dbg-footprint-command fname line))
		     t)
	     "-"
	     (vector "Continue until here"
		     (list 'kbdb-call
			   (format "until %s:%s" fname line))
		     t))))))

;*---------------------------------------------------------------------*/
;*    kbdb-cee-source-menu-entries ...                                 */
;*---------------------------------------------------------------------*/
(defun kbdb-cee-source-menu-entries (event)
  (let ((region (upcase (buffer-substring (region-beginning) (region-end)))))
    (list (vector "(bdb) print"
		  (list 'kbdb-call (concat "print " region))
		  t)
	  (vector "(bdb) display"
		  (list 'kbdb-call (concat "display " region))
		  t)
	  "--:shadowDoubleEtchedOut")))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-connect-buffer-hook ...                                 */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-connect-buffer-hook ()
  ;; we set dbg mouse bindings
  '(ude-add-menu #'(lambda (event)
		   (and (> (window-left-margin-pixel-width
			    (event-window event))
			   (event-x-pixel event))))
		'kbdb-breakpoint-menu)
  ;; the s-expression menu when clicking on the active region
  (cee-add-region-popup-entry 'kbdb-cee-source-menu-entries))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-disconnect-buffer-hook ...                              */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-disconnect-buffer-hook ()
  ;; we first remove the region popup
  (cee-remove-region-popup-entry 'kbdb-cee-source-menu-entries))
  
;*---------------------------------------------------------------------*/
;*    cee-run-external-kbdb ...                                        */
;*---------------------------------------------------------------------*/
(defun cee-run-external-kbdb ()
  ;; cee bdb connexion
  (dbg-add-connect-hook 'c-mode 'cee-kbdb-connect-buffer-hook)
  (dbg-add-disconnect-hook 'c-mode 'cee-kbdb-disconnect-buffer-hook)
  (dbg-add-connect-hook 'c-mode 'cee-connect-buffer-hook)
  (dbg-add-disconnect-hook 'c-mode 'cee-disconnect-buffer-hook)
  ;; the caller wrapper
  (setq dbg-wrapper-caller 'kbdb-call)
  ;; the argument parsing
  (let* ((frame     (selected-frame))
	 (emacs-opt '()))
    ;; background color
    (let ((bg (face-foreground-name 'text-cursor)))
      (if (stringp bg)
	  (setq emacs-opt
		(cons "-bg" (cons bg emacs-opt)))))
    ;; toolbar color
    (let ((tl (frame-toolbar-background frame)))
      (if (stringp tl)
	  (setq emacs-opt
		(cons "-tl" (cons tl emacs-opt)))))
    ;; active background
    (let ((abg (face-background-name 'highlight)))
      (if (stringp abg)
	  (setq emacs-opt
		(cons "-ac" (cons abg emacs-opt)))))
    ;; modeline background
    (let ((mbg (face-background-name 'modeline)))
      (if (stringp mbg)
	  (setq emacs-opt
		(cons "-ml" (cons mbg emacs-opt)))))
    ;; modeline foreground
    (let ((mfg (face-foreground-name 'modeline-buffer-id)))
      (if (stringp mfg)
	  (setq emacs-opt
		(cons "-mf" (cons mfg emacs-opt)))))
    ;; modeline buffer id
    (let ((mid (face-foreground-name 'modeline-mousable)))
      (if (stringp mid)
	  (setq emacs-opt
		(cons "-mi" (cons mid
				  (cons "-pc"
					(cons mid emacs-opt)))))))
    ;; modeline project
    (let ((mid (face-foreground-name 'ude-modeline-root-face)))
      (if (stringp mid)
	  (setq emacs-opt
		(cons "-mp" (cons mid emacs-opt)))))
    ;; font
    (let ((font (face-font-name 'text-cursor)))
      (if (stringp font)
	  (setq emacs-opt
		(cons "-fn" (cons font emacs-opt)))))
    ;; modeline font
    (let ((font (face-font-name 'modeline)))
      (if (stringp font)
	  (setq emacs-opt
		(cons "-fmn" (cons font emacs-opt)))))
    ;; width
    (let ((width (frame-pixel-width (selected-frame))))
      (if (numberp width)
	  (setq emacs-opt (cons "-width"
				(cons (number-to-string width)
				      emacs-opt)))))
    ;; height
    (let ((height (frame-pixel-height (selected-frame))))
      (if (numberp height)
	  (setq emacs-opt (cons "-height"
				(cons (number-to-string height)
				      emacs-opt)))))
    ;; regular arguments
    (let* ((a.out   (let ((name (ude-fetch-makefile-binary-entry)))
		      (if (stringp name)
			  name
			"a.out")))
	   (opts    (cons a.out
			  (cons "-root"
				(cons ude-root-directory
				      (append cee-kbdb-cee-options
					      emacs-opt))))))
      (condition-case err
	  (if (cee-find-kbdb ude-root-directory)
	      ;; simply connect the buffer
	      (dbg-connect-buffer (current-buffer))
	    (progn
	      ;; we start the kbdb
	      (make-plugin cee-kbdb
			   opts
			   (function cee-kbdb-callback)
			   nil
			   ude-root-directory
			   'cee-kbdb-terminate-hook)
	      ;; and we connect the current buffer
	      (dbg-connect-buffer (current-buffer))))
	(error
	 (if (stringp (car (cdr err)))
	     (ude-error (car (cdr err)))
	   (ude-error "Can't start kbdb")))))))

;*---------------------------------------------------------------------*/
;*    kbdb-breakpoints ...                                             */
;*---------------------------------------------------------------------*/
(defvar kbdb-breakpoints '())

;*---------------------------------------------------------------------*/
;*    cee-kbdb-terminate-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-terminate-hook ()
  ;; disconnect all buffers
  (dbg-disconnect-all-buffers)
  ;; remove the current source line display
  (dbg-undisplay-source-line)
  ;; remove all breakpoints
  (while (consp kbdb-breakpoints)
    (let ((l kbdb-breakpoints))
      (dbg-delete-breakpoint (cdr (car l)))
      (setq l (cdr l))
      (setq kbdb-breakpoints l))))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-requesting-process ...                                  */
;*---------------------------------------------------------------------*/
(defvar cee-kbdb-requesting-process nil)

;*---------------------------------------------------------------------*/
;*    kbdb-find-breakpoint ...                                         */
;*---------------------------------------------------------------------*/
(defun kbdb-find-breakpoint (num)
  (let ((cell (assq num kbdb-breakpoints)))
    (if (consp cell)
	(cdr cell)
      '())))

;*---------------------------------------------------------------------*/
;*    kbdb-unregister-breakpoint ...                                   */
;*---------------------------------------------------------------------*/
(defun kbdb-unregister-breakpoint (num)
  (cond
   ((null kbdb-breakpoints)
    'done)
   ((eq (car (car kbdb-breakpoints)) num)
    (setq kbdb-breakpoints (cdr kbdb-breakpoints)))
   (t
    (let ((l    (cdr kbdb-breakpoints))
	  (last kbdb-breakpoints))
      (if (consp l)
	  (progn
	    (if (eq (car l) num)
		(rplacd last (cdr l))
	      (progn
		(setq last l)
		(setq l (cdr l))))))))))

;*---------------------------------------------------------------------*/
;*    kbdb-register-breakpoint ...                                     */
;*---------------------------------------------------------------------*/
(defun kbdb-register-breakpoint (num bp)
  (setq kbdb-breakpoints (cons (cons num bp) kbdb-breakpoints)))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-callback ...                                            */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-callback (proc command)
  ;; we parse the command
  (cond
   ((not (consp command))
    (message-box (format "%S" command))
    t)
   ((eq (car command) 'SOURCE-LINE)
    (let ((line (car (cdr command))))
      (if (string-match "\032\032\\([^:]+\\):\\([0-9]+\\):" line)
	  (let* ((file (substring line (match-beginning 1) (match-end 1)))
		 (lnum (string-to-number
			(substring line (match-beginning 2) (match-end 2)))))
	    (progn
	      (dbg-highlight-source-line file lnum)
	      (plugin-send-string proc "#t")))
	(plugin-send-string proc "#f"))
      t))
   ((eq (car command) 'BREAKPOINT-REMOVE)
    (let* ((num (car (cdr command)))
	   (bp  (kbdb-find-breakpoint num)))
      (kbdb-unregister-breakpoint num)
      (dbg-delete-breakpoint bp)
      t))
   ((eq (car command) 'BREAKPOINT-ADD)
    (let* ((num (car (cdr command)))
	   (en  (string= (car (cdr (cdr command))) "t"))
	   (pos (car (cdr (cdr (cdr command)))))
	   (c   (car (cdr (cdr (cdr (cdr command))))))
	   (con (if (stringp c) c '())))
      (string-match "\\([^:]+\\):\\([0-9]+\\)" pos)
      (let* ((file (substring pos (match-beginning 1) (match-end 1)))
	     (line (string-to-number (substring pos
					     (match-beginning 2)
					     (match-end 2))))
	     (bp (dbg-make-breakpoint num en file line (stringp c) '() con "breakpoint add")))
	(kbdb-register-breakpoint num bp)
	t)))
   ((eq (car command) 'BREAKPOINT-CHANGE)
    (let* ((num (car (cdr command)))
	   (en  (string= (car (cdr (cdr command))) "t"))
	   (pos (car (cdr (cdr (cdr command)))))
	   (c   (car (cdr (cdr (cdr (cdr command))))))
	   (con (if (stringp c) c '())))
      (string-match "\\([^:]+\\):\\([0-9]+\\)" pos)
      (let* ((file (substring pos (match-beginning 1) (match-end 1)))
	     (line (string-to-number (substring pos
					     (match-beginning 2)
					     (match-end 2))))
	     (obp (kbdb-find-breakpoint num))
	     (bp  (dbg-make-breakpoint num en file line (stringp c) '() con "breakpoint add")))
	(kbdb-unregister-breakpoint num)
	(dbg-delete-breakpoint obp)
	(kbdb-register-breakpoint num bp)
	t)))
   ((eq (car command) 'EDIT)
    (let ((root (car (cdr command)))
	  (def  (car (cdr (cdr command)))))
      (cond
       ((string-match "\\(?:[^:]+:\\)?\\([^:]+\\):\\([0-9]+\\)" def)
	;; this is a lambda
	(let* ((fname (concat root
			      "/"
			      (substring def
					 (match-beginning 1)
					 (match-end 1))))
	       (pos   (substring def (match-beginning 2) (match-end 2)))
	       (npos  (string-to-number pos))
	       (buf   (find-buffer-visiting fname))
	       (default-directory root))
	  (if (bufferp buf)
	      (let ((win (get-buffer-window buf t))
		    (bname (buffer-name buf)))
		(if (not (windowp win))
		    (setq buf (switch-to-buffer-other-frame bname))
		  (let ((frame (window-frame win)))
		    (if (not (framep frame))
			(setq buf (switch-to-buffer-other-frame bname))
		      (raise-frame frame)
		      (select-frame frame)
		      (select-window win)
		      (set-buffer buf)
		      (goto-line npos)
		      (beginning-of-line)
		      (set-window-point win npos)
		      t))))
	    (setq buf (progn
			(find-file-other-frame fname)
			(current-buffer))))
	  (if (bufferp buf)
	      (progn
		(set-buffer buf)
		(goto-line npos)
		(beginning-of-line)
		t))))
       (t
	(let ((default-directory root))
	  (if (not (cee-tags-find (point-min) def))
	      (plugin-error proc
			    ":Can't find global definition for variable -- "
			    def)))))))
   (t '())))

;*---------------------------------------------------------------------*/
;*    cee-external-kbdb-success ...                                    */
;*---------------------------------------------------------------------*/
(defun cee-external-kbdb-success (buffer msg)
  (if (processp cee-kbdb-requesting-process)
      (plugin-send-string cee-kbdb-requesting-process
			   (format "(record-completed %S)" msg))))
  
;*---------------------------------------------------------------------*/
;*    doc source keymap                                                */
;*---------------------------------------------------------------------*/
(defvar cee-kbdb-mouse-map (make-sparse-keymap))
(define-key cee-kbdb-mouse-map ude-mouse-2-binding
  (function cee-kbdb-find))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-find ...                                                */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-find (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event))
	 (prop   (find-text-property point 'prof buffer)))
    (cond
     ((and (consp prop) (eq (car prop) 'var))
      (cee-find-definition (cdr prop))))))

;*---------------------------------------------------------------------*/
;*    cee-external-kbdb-inspect ...                                    */
;*---------------------------------------------------------------------*/
(defun cee-external-kbdb-inspect (ident)
  (let ((proc (cee-find-kbdb ude-root-directory)))
    (if (and (processp proc) (eq (process-status proc) 'run))
	(plugin-send-string proc (format "(inspect %S)" (upcase ident)))
      (message "No running kbdb."))))

;*---------------------------------------------------------------------*/
;*    cee-kbdb-inspect ...                                             */
;*---------------------------------------------------------------------*/
(defun cee-kbdb-inspect ()
  "Inspect function"
  (interactive)
  (let* ((pos (point))
	 (ident (ude-fetch-identifier pos)))
    (if (stringp ident)
	(cee-external-kbdb-inspect ident)
      (let ((ident (read-string "Inspect function: ")))
	(if (not (string= ident ""))
	    (cee-external-kbdb-inspect ident))))))
