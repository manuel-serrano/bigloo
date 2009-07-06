;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-kbdb.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  1 11:57:44 1999                          */
;*    Last change :  Tue Sep 20 06:05:47 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between the BEE and a visual debugger.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-kbdb)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-autoload)
(require 'bee-config)
(require 'bee-bdb)
(require 'ude-autoload)
(require 'ude-config)
(require 'dbg-autoload)
(require 'dbg)
(require 'cee-gdb)
(require 'plugin)

;*---------------------------------------------------------------------*/
;*    bee-kbdb-ready-p ...                                             */
;*    -------------------------------------------------------------    */
;*    This true predicate returns true iff a debugger                  */
;*    is ready. That is if the binary file for the interface           */
;*    builder exists and may be executed.                              */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-ready-p ()
  (and (stringp bee-kbdb)
       (file-exists-p bee-kbdb)
       (file-executable-p bee-kbdb)))

;*---------------------------------------------------------------------*/
;*    bee-find-kbdb ...                                                */
;*    -------------------------------------------------------------    */
;*    Finds a kbdb associated to the project. If such a kbdb           */
;*    does not exist, returns '()                                      */
;*---------------------------------------------------------------------*/
(defun bee-find-kbdb (project)
  (plugin-find-process-from-key bee-kbdb project))

;*---------------------------------------------------------------------*/
;*    bee-kbdb-start ...                                               */
;*    -------------------------------------------------------------    */
;*    This function start a new interface kbdb. It first creates       */
;*    a module name and a file name and launch a new process.          */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-start ()
  (interactive)
  (bee-run-kbdb))

;*---------------------------------------------------------------------*/
;*    bee-run-kbdb ...                                                 */
;*    -------------------------------------------------------------    */
;*    This functions spawn a new kbdb process.                         */
;*---------------------------------------------------------------------*/
(defun bee-run-kbdb ()
  (if (bee-kbdb-ready-p)
      (bee-run-external-kbdb)
    (bee-run-internal-kbdb)))

;*---------------------------------------------------------------------*/
;*    bee-run-internal-kbdb ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-run-internal-kbdb ()
  (setq dbg-wrapper-caller 'nil)
  (bdb-other-frame))

;*---------------------------------------------------------------------*/
;*    kbdb-call ...                                                    */
;*---------------------------------------------------------------------*/
(defun kbdb-call (cmd)
  (let ((plugin (plugin-find (intern bee-kbdb))))
    (if (consp plugin)
	(plugin-send-string (car (car plugin)) (format "(bdb %S)" cmd))
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
;*    kbdb-bee-source-menu-entries ...                                 */
;*---------------------------------------------------------------------*/
(defun kbdb-bee-source-menu-entries (event)
  (let ((region (upcase (buffer-substring (region-beginning) (region-end)))))
    (list (vector "(bdb) print"
		  (list 'kbdb-call (concat "print " region))
		  t)
	  (vector "(bdb) display"
		  (list 'kbdb-call (concat "display " region))
		  t)
	  "--:shadowDoubleEtchedOut")))

;*---------------------------------------------------------------------*/
;*    bee-kbdb-connect-buffer-hook ...                                 */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-connect-buffer-hook ()
  ;; we set dbg mouse bindings
  '(ude-add-menu #'(lambda (event)
		   (and (> (window-left-margin-pixel-width
			    (event-window event))
			   (event-x-pixel event))))
		'kbdb-breakpoint-menu)
  ;; we actived Scheme source balloon help
  (bdb-bee-balloon-start)
  ;; the s-expression menu when clicking on the active region
  (bee-add-region-popup-entry 'kbdb-bee-source-menu-entries))

;*---------------------------------------------------------------------*/
;*    bee-kbdb-disconnect-buffer-hook ...                              */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-disconnect-buffer-hook ()
  ;; we first remove the region popup
  (bee-remove-region-popup-entry 'kbdb-bee-source-menu-entries)
  ;; then, we remove the balloon help
  (bdb-bee-balloon-stop))
  
;*---------------------------------------------------------------------*/
;*    bee-run-external-kbdb ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-run-external-kbdb ()
  ;; bee bdb connexion
  (dbg-add-connect-hook 'bee-mode 'bee-kbdb-connect-buffer-hook)
  (dbg-add-disconnect-hook 'bee-mode 'bee-kbdb-disconnect-buffer-hook)
  (dbg-add-connect-hook 'c-mode 'cee-connect-buffer-hook)
  (dbg-add-disconnect-hook 'c-mode 'cee-disconnect-buffer-hook)
  ;; the caller wrapper
  (setq dbg-wrapper-caller 'kbdb-call)
  ;; the argument parsing
  (let* ((frame     (selected-frame))
	 (emacs-opt (plugin-color-configuration)))
    ;; width
    (let ((width (frame-pixel-width frame)))
      (if (numberp width)
	  (setq emacs-opt (cons "-width"
				(cons (number-to-string width)
				      emacs-opt)))))
    ;; height
    (let ((height (frame-pixel-height frame)))
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
				      (append bee-kbdb-bee-options
					      emacs-opt))))))
      (condition-case err
	  (if (bee-find-kbdb ude-root-directory)
	      ;; simply connect the buffer
	      (dbg-connect-buffer (current-buffer))
	    (let ((default-directory ude-root-directory))
	      ;; we start the kbdb
	      (make-plugin bee-kbdb
			   opts
			   (function bee-kbdb-callback)
			   nil
			   ude-root-directory
			   'bee-kbdb-terminate-hook)
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
;*    bee-kbdb-terminate-hook ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-terminate-hook ()
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
;*    bee-kbdb-requesting-process ...                                  */
;*---------------------------------------------------------------------*/
(defvar bee-kbdb-requesting-process nil)

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
;*    kbdb-edit-file-line ...                                          */
;*---------------------------------------------------------------------*/
(defun kbdb-edit-file-line (root fname npos)
  (let ((buf   (find-buffer-visiting fname))
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

;*---------------------------------------------------------------------*/
;*    bee-kbdb-callback ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-callback (proc command)
  ;; we parse the command
  (cond
   ((not (consp command))
    (message-box (format "Illegal Kbdb callback: %S" command))
    (ude-error (format "Illegal Kbdb callback: %S" command))
    t)
   ((memq (car command) '(PROGN progn))
    (let ((cmd (cdr command)))
      (while (consp cmd)
	(progn
	  (bee-kbdb-callback proc (car cmd))
	  (setq cmd (cdr cmd)))))
    t)
   ((memq (car command) '(SOURCE-LINE source-line))
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
   ((memq (car command) '(BREAKPOINT-REMOVE breakpoint-remove))
    (let* ((num (car (cdr command)))
	   (bp  (kbdb-find-breakpoint num)))
      (kbdb-unregister-breakpoint num)
      (dbg-delete-breakpoint bp)
      (plugin-send-string proc "#t");;new
      t))
   ((memq (car command) '(BREAKPOINT-ADD breakpoint-add))
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
	     (bp (dbg-make-breakpoint num en file line '() (stringp c) '() con)))
	(kbdb-register-breakpoint num bp)
	(plugin-send-string proc "#t");;new
	t)))
   ((memq (car command) '(BREAKPOINT-CHANGE breakpoint-change))
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
	     (bp  (dbg-make-breakpoint num en file line '() (stringp c) '() con)))
	(kbdb-unregister-breakpoint num)
	(dbg-delete-breakpoint obp)
	(kbdb-register-breakpoint num bp)
	(plugin-send-string proc "#t");;new
	t)))
   ((memq (car command) '(FOOTPRINT-REMOVE footprint-remove))
    (let* ((num (car (cdr command)))
	   (bp  (kbdb-find-breakpoint num)))
      (kbdb-unregister-breakpoint num)
      (dbg-delete-breakpoint bp)
      (plugin-send-string proc "#t");;new
      t))
   ((memq (car command) '(FOOTPRINT-ADD footprint-add))
    (let* ((num (car (cdr command)))
	   (en  (string= (car (cdr (cdr command))) "t"))
	   (pos (car (cdr (cdr (cdr command)))))
	   (c   (car (cdr (cdr (cdr (cdr command))))))
	   (msg (if (stringp c) c '())))
      (string-match "\\([^:]+\\):\\([0-9]+\\)" pos)
      (let* ((file (substring pos (match-beginning 1) (match-end 1)))
	     (line (string-to-number (substring pos
					     (match-beginning 2)
					     (match-end 2))))
	     (bp (dbg-make-breakpoint num en file line t (stringp c) '() msg)))
	(kbdb-register-breakpoint num bp)
	(plugin-send-string proc "#t");;new
	t)))
   ((memq (car command) '(FOOTPRINT-CHANGE footprint-change))
    (let* ((num (car (cdr command)))
	   (en  (string= (car (cdr (cdr command))) "t"))
	   (pos (car (cdr (cdr (cdr command)))))
	   (c   (car (cdr (cdr (cdr (cdr command))))))
	   (msg (if (stringp c) c '())))
      (string-match "\\([^:]+\\):\\([0-9]+\\)" pos)
      (let* ((file (substring pos (match-beginning 1) (match-end 1)))
	     (line (string-to-number (substring pos
					     (match-beginning 2)
					     (match-end 2))))
	     (obp (kbdb-find-breakpoint num))
	     (bp  (dbg-make-breakpoint num en file line t (stringp c) '() msg)))
	(kbdb-unregister-breakpoint num)
	(dbg-delete-breakpoint obp)
	(kbdb-register-breakpoint num bp)
	(plugin-send-string proc "#t");;new
	t)))
   ((memq (car command) '(EDIT edit))
    (let ((root (car (cdr command)))
	  (def  (car (cdr (cdr command)))))
      ;; send the acknoledge
      (plugin-send-string proc "#t")
      (cond
       ((eq (aref def 0) ?@)
	;; this is a module we want to edit
	(let ((str (substring def 1 (length def)))
	      (default-directory root))
	  (cond
	   ((string-match "(toplevel)\\(.+\\)" str)
	    (bee-find-module (substring str
					(match-beginning 1)
					(match-end 1))))
	   (t
	    (bee-find-module str)))))
       ((string-match " at \\([^:]+\\):\\([0-9]+\\)" def)
	;; this a direct lined position in the source code
	(let* ((fname (concat root
			      "/"
			      (substring def
					 (match-beginning 1)
					 (match-end 1))))
	       (pos   (substring def (match-beginning 2) (match-end 2)))
	       (npos  (string-to-number pos)))
	  (kbdb-edit-file-line root fname npos)))
       ((string-match "<anonymous:[0-9]+:\\([^:]+\\):\\([0-9]+\\)" def)
	;; this is a lambda (current format)
	(let* ((fname (concat root
			      "/"
			      (substring def
					 (match-beginning 1)
					 (match-end 1))))
	       (pos   (substring def (match-beginning 2) (match-end 2)))
	       (npos  (string-to-number pos)))
	  (kbdb-edit-file-line root fname npos)))
       ((string-match "\\(?:[^:]+:\\)?\\([^:]+\\):\\([0-9]+\\)" def)
	;; this is a lambda (former format)
	(let* ((fname (concat root
			      "/"
			      (substring def
					 (match-beginning 1)
					 (match-end 1))))
	       (pos   (substring def (match-beginning 2) (match-end 2)))
	       (npos  (string-to-number pos)))
	  (kbdb-edit-file-line root fname npos)))
       (t
	;; this is a plain identifier
	(let ((default-directory root))
	  (if (not (bee-tags-find-if-exists (point-min) def))
	      (plugin-error proc
			    ":Can't find global definition for variable -- "
			    def)
	    t))))))
   (t '())))

;*---------------------------------------------------------------------*/
;*    bee-external-kbdb-success ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-external-kbdb-success (buffer msg)
  (if (processp bee-kbdb-requesting-process)
      (plugin-send-string bee-kbdb-requesting-process
			   (format "(record-completed %S)" msg))))
  
;*---------------------------------------------------------------------*/
;*    doc source keymap                                                */
;*---------------------------------------------------------------------*/
(defvar bee-kbdb-mouse-map (make-sparse-keymap))
(define-key bee-kbdb-mouse-map ude-mouse-2-binding
  (function bee-kbdb-find))

;*---------------------------------------------------------------------*/
;*    bee-kbdb-find ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-find (event)
  (interactive "e")
  (let* ((point  (event-closest-point event))
	 (buffer (event-buffer event))
	 (prop   (find-text-property point 'prof buffer)))
    (cond
     ((and (consp prop) (eq (car prop) 'var))
      (bee-find-definition (cdr prop)))
     ((and (consp prop) (eq (car prop) 'module))
      (bee-find-module (cdr prop))))))

;*---------------------------------------------------------------------*/
;*    bee-external-kbdb-inspect ...                                    */
;*---------------------------------------------------------------------*/
(defun bee-external-kbdb-inspect (ident)
  (let ((proc (bee-find-kbdb ude-root-directory)))
    (if (and (processp proc) (eq (process-status proc) 'run))
	(plugin-send-string proc (format "(inspect %S)" (upcase ident)))
      (message "No running kbdb."))))

;*---------------------------------------------------------------------*/
;*    bee-kbdb-inspect ...                                             */
;*---------------------------------------------------------------------*/
(defun bee-kbdb-inspect ()
  "Inspect function"
  (interactive)
  (let* ((pos (point))
	 (ident (ude-fetch-identifier pos)))
    (if (stringp ident)
	(bee-external-kbdb-inspect ident)
      (let ((ident (read-string "Inspect function: ")))
	(if (not (string= ident ""))
	    (bee-external-kbdb-inspect ident))))))
