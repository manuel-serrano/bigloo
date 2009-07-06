;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/plugin.el                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jun  6 10:31:27 1999                          */
;*    Last change :  Wed Jan 23 09:22:57 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This package provides facilities to implement emacs plugins.     */
;*    An emacs plugin is a binary file that is spawned by emacs and    */
;*    that establishes a communication channel with its emacs parent.  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'plugin)
(require 'info)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    make-plugin ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function declares a plugin, runs it and returns a           */
;*    process structure hosting the plugin.                            */
;*        CMD     : the name of the plugin (the binary file that       */
;*                  implements the plugin                              */
;*        ARGS    : the arguments of the plugin                        */
;*        CALLBACK: is a procedure of two arguments: PROC and LIST.    */
;*                  CALLBACK is called with the plugin process         */
;*                  requesting a command and the command list. LIST is */
;*                  not a string, it is a lisp list.                   */
;*                  If the CALLBACK calls returns '(), then the        */
;*                  default plugin callback is invoked. See the        */
;*                  PLUGIN-DEFAULT-CALLBACK documentation for the list */
;*                  of the recognized command and there emacs          */
;*                  equivalent commands.                               */
;*        UNIQUE  : is the plugin may have several instance?           */
;*        KEY     : if the plugin may have several instance, the key   */
;*                  identifies the instance. If the key is not nil,    */
;*                  it is illegal to start several instance of the     */
;*                  plugin with a unique key.                          */
;*---------------------------------------------------------------------*/
(defun make-plugin (cmd args callback &optional unique key sentinel)
  "This function declares a plugin, runs it and returns a
process structure hosting the plugin.
    CMD     : the name of the plugin (the binary file that
              implements the plugin
    ARGS    : the arguments of the plugin
    CALLBACK: is a procedure of two arguments: PROC and LIST.
              CALLBACK is called with the plugin process
              requesting a command and the command list. LIST is
              not a string, it is a lisp list.
              If the CALLBACK calls returns '(), then the
              default plugin callback is invoked. See the
              PLUGIN-DEFAULT-CALLBACK documentation for the list
              of the recognized command and there emacs
              equivalent commands.
    UNIQUE  : is the plugin may have several instance?
    KEY     : if the plugin may have several instance, the key
              identifies the instance. If the key is not nil,
              it is illegal to start several instance of the
              plugin with a unique key."
  (let ((cmd-intern (intern cmd)))
    ;; first, we check for unique plugin if we are not already running
    ;; one plugin
    (cond
     ((and unique (plugin-find cmd-intern))
      (error "A plugin %S is already runnning, exiting..."
	     (file-name-nondirectory cmd))
      nil)
     ((and key (plugin-find-key cmd-intern key))
      (error "A plugin %S for %S is already runnning, exiting..."
	     (file-name-nondirectory cmd)
	     key)
      nil)
     (t
      (let* ((bufname (concat cmd "-" (get-new-name)))
	     (process (apply 'start-process cmd bufname cmd args)))
	;; we print a message while waiting for the plugin
	(message "Waiting for %S..." (file-name-nondirectory cmd))
	;; we setup a filter for that process in order to get a means
	;; to communicate with the process
	(set-process-buffer process nil)
	(set-process-filter process (function plugin-filter))
	;; we setup a sentinel that will tell use when a process
	;; is completed. Our sentinel will simply removes the process
	;; from the builder process list
	(set-process-sentinel process (function plugin-sentinel))
	;; we store the process into the builder process list.
	(plugin-register-process cmd-intern
				 process
				 key
				 callback
				 sentinel
				 bufname))))))

;*---------------------------------------------------------------------*/
;*    plugin-processes ...                                             */
;*    -------------------------------------------------------------    */
;*    The list of running plugins.                                     */
;*---------------------------------------------------------------------*/
(defvar plugin-processes '())

;*---------------------------------------------------------------------*/
;*    plugin-sentinel ...                                              */
;*    -------------------------------------------------------------    */
;*    If PROC is not found in the register list, we don't emit         */
;*    error messages.                                                  */
;*---------------------------------------------------------------------*/
(defun plugin-sentinel (proc msg)
  ;; We search in the process list for proc.
  ;; When found we remove it from the list
  (let ((p plugin-processes))
    (while (consp p)
      (let* ((raw   (car p))
	     (pname (car raw))
	     (procs (cdr raw))
	     (prev  raw))
	(while (consp procs)
	  (if (eq (car (car procs)) proc)
	      ;; we have found the process the terminates
	      (progn
		;; we try to invoke the user sentinel and we remove the
		;; associated buffer
		(let* ((sentinel (car (cdr (cdr (cdr (car procs))))))
		       (bname    (car (cdr (cdr (cdr (cdr (car procs)))))))
		       (buffer   (get-buffer bname)))
		  (if (bufferp buffer)
		      (kill-buffer buffer))
		  (if (functionp sentinel)
		      (funcall sentinel)))
		(rplacd prev (cdr procs))
		;; we message corresponding to the termination
		(if (eq (process-exit-status proc) 0)
		    (message "%S completed." pname)
		  (message "%S aborded." pname))
		;; we abort the scanning
		(setq p '())
		(setq procs '()))
	    (progn
	      (setq prev procs)
	      (setq procs (cdr procs)))))
	(setq p (cdr p))))))

;*---------------------------------------------------------------------*/
;*    plugin-find ...                                                  */
;*---------------------------------------------------------------------*/
(defun plugin-find (proc-key)
  (let ((cell (assq proc-key plugin-processes)))
    (if (consp cell)
	(cdr cell)
      '())))

;*---------------------------------------------------------------------*/
;*    plugin-find-key ...                                              */
;*---------------------------------------------------------------------*/
(defun plugin-find-key (proc-key key)
  (let ((raw (assq proc-key plugin-processes)))
    (if (consp raw)
	(let ((l   (cdr raw))
	      (res '()))
	  (while (and (not res) (consp l))
	    (if (equal key (car (cdr (car l))))
		(setq res (car l))
	      (setq l (cdr l))))
	  res)
      '())))

;*---------------------------------------------------------------------*/
;*    plugin-find-process-key ...                                      */
;*    -------------------------------------------------------------    */
;*    Return the process raw key associated to PROC.                   */
;*---------------------------------------------------------------------*/
(defun plugin-find-process-key (proc)
  (let ((p   plugin-processes)
	(res '()))
    (while (and (not res) (consp p))
      (let* ((raw   (car p))
	     (pname (car raw))
	     (procs (cdr raw)))
	(while (and (not res) (consp procs))
	  (if (eq (car (car procs)) proc)
	      (setq res pname)
	    (setq procs (cdr procs))))
	(setq p (cdr p))))
    res))

;*---------------------------------------------------------------------*/
;*    plugin-find-process-callback ...                                 */
;*    -------------------------------------------------------------    */
;*    Return the process callback associated to PROC.                  */
;*---------------------------------------------------------------------*/
(defun plugin-find-process-callback (proc)
  (let ((p   plugin-processes)
	(res '()))
    (while (and (not res) (consp p))
      (let* ((raw   (car p))
	     (procs (cdr raw)))
	(while (and (not res) (consp procs))
	  (if (eq (car (car procs)) proc)
	      (setq res (car (cdr (cdr (car procs)))))
	    (setq procs (cdr procs))))
	(setq p (cdr p))))
    res))

;*---------------------------------------------------------------------*/
;*    plugin-find-process-from-key ...                                 */
;*    -------------------------------------------------------------    */
;*    Given a process key, returns the associated process.             */
;*---------------------------------------------------------------------*/
(defun plugin-find-process-from-key (plugin key)
  (let ((p   plugin-processes)
	(res '()))
    (while (and (not res) (consp p))
      (let* ((raw   (car p))
	     (procs (cdr raw)))
	(while (and (not res) (consp procs))
	  (let ((k (car (cdr (car procs))))
		(i (car (car procs))))
	    (if (and (processp i)
		     (equal plugin (process-name i))
		     (equal k key))
		(setq res (car (car procs)))
	      (setq procs (cdr procs)))))
	(setq p (cdr p))))
    res))

;*---------------------------------------------------------------------*/
;*    plugin-filter ...                                                */
;*    -------------------------------------------------------------    */
;*    This function parsers the output from the plugin process.        */
;*    There is a collaboration between emacs and the plugin. In        */
;*    particular, when a plugin starts talking to emacs, emacs will    */
;*    wait until the plugin talk is completed.                         */
;*---------------------------------------------------------------------*/
(defun plugin-filter (proc string)
  (let ((pending (plugin-pending-string-p proc)))
    (if pending
	;; yes there is a pending string for that process. We have
	;; to concatanete the pending string and the current string
	(setq string (concat pending string))))
  (if (plugin-talk-completed-p proc string)
      ;; ok, the command is completed, we may now execute it
      (progn
	;; there is no more pending string for PROC
	(plugin-remove-pending proc)
	(plugin-execute-command proc string))
    ;; we store the string as a pending string
    (plugin-add-pending-string string proc)))

;*---------------------------------------------------------------------*/
;*    plugin-pending-string ...                                        */
;*---------------------------------------------------------------------*/
(defvar plugin-pending-string '())

;*---------------------------------------------------------------------*/
;*    plugin-pending-string-p ...                                      */
;*---------------------------------------------------------------------*/
(defun plugin-pending-string-p (proc)
  (let ((cell (assoc proc plugin-pending-string)))
    (if (consp cell)
	(cdr cell)
      '())))

;*---------------------------------------------------------------------*/
;*    plugin-add-pending-string ...                                    */
;*---------------------------------------------------------------------*/
(defun plugin-add-pending-string (string proc)
  (let ((cell (assoc proc plugin-pending-string)))
    (if (consp cell)
	(rplacd cell string)
      (setq plugin-pending-string
	    (cons (cons proc string ) plugin-pending-string)))))

;*---------------------------------------------------------------------*/
;*    plugin-remove-pending ...                                        */
;*---------------------------------------------------------------------*/
(defun plugin-remove-pending (proc)
  (if (and (consp plugin-pending-string)
	   (eq (car (car plugin-pending-string)) proc))
      (setq plugin-pending-string (cdr plugin-pending-string))
    (let ((l plugin-pending-string))
      (while (consp l)
	(if (eq (car (cdr l)) proc)
	    (progn
	      (rplacd l (cdr (cdr l)))
	      (setq l '()))
	  (setq l (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    plugin-talk-completed-p ...                                      */
;*    This function returns true iff the STRING argument is a complete */
;*    plugin command. In order to find out if STRING is a complete     */
;*    command, we check out a kind of inlined command dictionary.      */
;*    If STRING is in the dictionary the command is completed.         */
;*---------------------------------------------------------------------*/
(defun plugin-talk-completed-p (proc string)
  ;; first, we match against (file-name variable-name)
  (if (string-match "([^)]+)[ \n]*" string)
      t
    '()))

;*---------------------------------------------------------------------*/
;*    plugin-register-process ...                                      */
;*    -------------------------------------------------------------    */
;*    The plugin-processes list is a list of the following form:       */
;*       ((PROC-KEY . ((PROCESS INSTANCE-KEY CALLBACK SENTINEL BNAME)  */
;*     	               (PROCESS INSTANCE-KEY CALLBACK SENTINEL BNAME)  */
;*    	               ...))                                           */
;*       ((PROC-KEY . ((PROCESS INSTANCE-KEY CALLBACK SENTINEL BNAME)  */
;*     	               (PROCESS INSTANCE-KEY CALLBACK SENTINEL BNAME)  */
;*    	               ...))                                           */
;*        ...)                                                         */
;*---------------------------------------------------------------------*/
(defun plugin-register-process (proc-key process instance-key callback
					 sentinel bname)
  (let ((cell (list process instance-key callback sentinel bname))
	(raw  (assq proc-key plugin-processes)))
    (if (consp raw)
	(rplacd raw (cons cell (cdr raw)))
      (setq plugin-processes (cons (list proc-key cell) plugin-processes))))
  process)

;*---------------------------------------------------------------------*/
;*    plugin-execute-command ...                                       */
;*---------------------------------------------------------------------*/
(defun plugin-execute-command (proc command)
  (let* ((cmd (condition-case ()
		  (read command)
		(error '(error))))
	 (cb  (plugin-find-process-callback proc)))
    (if (and (consp cmd) (eq (car cmd) 'progn))
	(let ((l (cdr cmd)))
	  (while (consp l)
	    (if (or (not (functionp cb)) (not (funcall cb proc l)))
		(plugin-default-callback proc l))
	    (setq l (cdr l))))
      (if (or (not (functionp cb)) (not (funcall cb proc cmd)))
	  (plugin-default-callback proc cmd)))))
    
;*---------------------------------------------------------------------*/
;*    plugin-default-callback ...                                      */
;*---------------------------------------------------------------------*/
(defun plugin-default-callback (proc command)
  (let* ((pkey (plugin-find-process-key proc))
	 (key  (cond
		((stringp pkey)
		 (file-name-nondirectory pkey))
		((symbolp pkey)
		 (file-name-nondirectory (symbol-name pkey)))
		(t
		 pkey))))
    (cond
     ((or (equal command '(ACKNOWLEDGE))
	  (equal command '(acknowledge)))
      (message "%S ready." key))
     ((or (equal command '(INITIALIZATION))
	  (equal command '(initialization)))
      (message "%S initializing..." key))
     ((and (consp command)
	   (memq (car command) '(EDIT-FILE-LINE edit-file-line)))
      (let ((file (cadr command))
	    (line (car (cdr (cdr command)))))
	(let* ((buffer (get-buffer-create file))
	       (win    (get-buffer-window buffer t)))
	  (if (not (if (windowp win)
		       (let ((frame (window-frame win)))
			 (if (framep frame)
			     (progn
			       (raise-frame frame)
			       (select-frame frame)
			       (select-window win)
			       (set-buffer buffer)
			       (goto-line line)
			       (recenter)
			       t)
			   nil))
		     nil))
	      (let ((pop-up-frames t))
		(pop-to-buffer buffer)
		(goto-line line)
		(recenter))))))
     ((and (consp command) (memq (car command) '(HELP help)))
      (let ((prgm (let ((p (cadr command)))
		    (if (string-match "info^" p)
			p
		      (concat p ".info")))))
	(save-window-excursion
	  (info)
	  (if (consp (cddr command))
	      (Info-find-node prgm (car (cddr command)))
	    (Info-find-node prgm "Top")))	    
	(let ((pop-up-frames t))
	  (pop-to-buffer "*info*"))))
     ((and (consp command) (eq (car command) 'ERROR))
      (apply (function error) (cdr command)))
     (t
      (message-box (format "Illegal ude command [%S:%S]" key command))
      (message "%S illegal command %S" key command)))))

;*---------------------------------------------------------------------*/
;*    plugin-send-string ...                                           */
;*---------------------------------------------------------------------*/
(defun plugin-send-string (proc str)
  (process-send-string proc (concat str "\n")))

;*---------------------------------------------------------------------*/
;*    plugin-error ...                                                 */
;*---------------------------------------------------------------------*/
(defun plugin-error (proc obj msg)
  (plugin-send-string proc (format "(error plugin %S %S)" obj msg)))

;*---------------------------------------------------------------------*/
;*    plugin-color-configuration ...                                   */
;*---------------------------------------------------------------------*/
(defun plugin-color-configuration ()
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
		(cons "-tl" (cons tl emacs-opt))))
      ;; menu background
      (let ((mbg (x-get-global-resource "menubar.background"
					"menubar.Background")))
	(if (stringp mbg)
	    (setq emacs-opt
		  (cons "-mb" (cons mbg emacs-opt)))
	  (setq emacs-opt
		(cons "-mb" (cons tl emacs-opt))))))
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
		(cons "-mi" (cons mid emacs-opt)))))
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
    emacs-opt))

;*---------------------------------------------------------------------*/
;*    get-new-name-counter ...                                         */
;*---------------------------------------------------------------------*/
(defvar get-new-name-counter 17051966)

;*---------------------------------------------------------------------*/
;*    get-new-name ...                                                 */
;*---------------------------------------------------------------------*/
(defun get-new-name ()
  (setq get-new-name-counter (1+ get-new-name-counter))
  (format "g%d" get-new-name-counter))
