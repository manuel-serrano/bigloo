;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-ibuilder.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  1 11:57:44 1999                          */
;*    Last change :  Wed Jan 23 16:40:51 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The connection between the BEE and one Bigloo interface builder. */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-ibuilder)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-autoload)
(require 'bee-config)
(require 'ude-autoload)
(require 'ude-config)

;*---------------------------------------------------------------------*/
;*    bee-interface-builder-ready-p ...                                */
;*    -------------------------------------------------------------    */
;*    This true predicate returns true iff an interface builder        */
;*    is ready. That is if the binary file for the interface           */
;*    builder exists and may be executed.                              */
;*---------------------------------------------------------------------*/
(defun bee-interface-builder-ready-p ()
  (and (stringp bee-interface-builder)
       (file-exists-p bee-interface-builder)
       (file-executable-p bee-interface-builder)))

;*---------------------------------------------------------------------*/
;*    bee-interface-builder-module-p ...                               */
;*    -------------------------------------------------------------    */
;*    This semi-predicate returns true iff the module passed as        */
;*    argument (MODULE-NAME and MODULE-FILE-NAME) is a module that is  */
;*    maintained by an interface builder.                              */
;*    -------------------------------------------------------------    */
;*    A module is maintained by an interface builder if:               */
;*      1- an interface builder exists.                                */
;*      2- a file name made of MODULE-FILE-NAME where the suffix is    */
;*         the suffix af an interface buildler.                        */
;*---------------------------------------------------------------------*/
(defun bee-interface-builder-module-p (module-name module-file-name)
  (and (bee-interface-builder-ready-p)
       (let ((basename (ude-string-prefix module-file-name)))
	 (let ((l bee-interface-builder-suffix)
	       (res '()))
	   (while (consp l)
	     (let ((name (concat basename "." (car l))))
	       (if (file-exists-p name)
		   (progn
		     (setq l '())
		     (setq res name))
		 (setq l (cdr l)))))
	   res))))

;*---------------------------------------------------------------------*/
;*    bee-interface-builder-process ...                                */
;*---------------------------------------------------------------------*/
(defvar bee-interface-builder-processes '()
  "The list of the external Bigloo interface builders currently running")
  
;*---------------------------------------------------------------------*/
;*    bee-find-builder-editing ...                                     */
;*    -------------------------------------------------------------    */
;*    This function searches the list of the current interface         */
;*    builder in order to find one that is currently editing           */
;*    FILE-NAME. If no such builder is found BEE-FIND-BUILDER-EDITING  */
;*    returns '().                                                     */
;*---------------------------------------------------------------------*/
(defun bee-find-builder-editing (file-name)
  (let ((cell (assoc file-name bee-interface-builder-processes)))
    (if (consp cell)
	(cdr cell)
      '())))

;*---------------------------------------------------------------------*/
;*    bee-interface-builder-start ...                                  */
;*    -------------------------------------------------------------    */
;*    This function start a new interface builder. It first creates    */
;*    a module name and a file name and launch a new process.          */
;*---------------------------------------------------------------------*/
(defun bee-interface-builder-start ()
    (bee-run-builder nil))

;*---------------------------------------------------------------------*/
;*    bee-run-builder ...                                              */
;*    -------------------------------------------------------------    */
;*    This functions spawn a new builder process for editing a new     */
;*    interface. The output file of the builder is be name FILE-NAME   */
;*---------------------------------------------------------------------*/
(defun bee-run-builder (file-name)
  (let ((name (concat "builder-" file-name)))
    ;; we enforce a connection that makes use of pipes instead of ptys
    (progn ;; let ((process-connection-type nil))
      (let ((process (apply 'start-process
			    name
			    "*bee-interface-builder-process*"
			    bee-interface-builder
			    (if (stringp file-name)
				(append bee-interface-builder-bee-options
					(list file-name))
			      bee-interface-builder-bee-options))))
	;; we print a message for the builder starting
	(message "Waiting for %S..." bee-interface-builder)
	;; we setup a filter for that process in order to get a means
	;; to communicate with the process
	(set-process-buffer process nil)
	(set-process-filter process (function bee-builder-filter))
	;; we setup a sentinel that will tell use when a process
	;; is completed. Our sentinel will simply removes the process
	;; from the builder process list
	(set-process-sentinel process (function bee-builder-sentinel))
	;; we store the process into the builder process list.
	(setq bee-interface-builder-processes
	      (cons (cons file-name process)
		    bee-interface-builder-processes))))))

;*---------------------------------------------------------------------*/
;*    bee-builder-sentinel ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-builder-sentinel (proc msg)
  ;; we search in the process list for proc. When found we remove it
  ;; from the list
  (message "Interface builder completed.")
  (if (and (consp bee-interface-builder-processes)
	   (eq (cdr (car bee-interface-builder-processes)) proc))
      (setq bee-interface-builder-processes
	    (cdr bee-interface-builder-processes))
    (let ((l bee-interface-builder-processes))
      (while (consp l)
	(if (eq (cdr (car (cdr l))) proc)
	    (progn
	      (rplacd l (cdr (cdr l)))
	      (setq l '()))
	  (setq l (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    bee-builder-filter ...                                           */
;*    -------------------------------------------------------------    */
;*    This function parsers the output from the builder process.       */
;*    There is a collaboration between emacs and the builder. In       */
;*    particular, when a builder starts talking to emacs, emacs will   */
;*    wait until the builder talk is completed.                        */
;*---------------------------------------------------------------------*/
(defun bee-builder-filter (proc string)
  (let ((pending (bee-pending-string-p proc)))
    (if pending
	;; yes there is a pending string for that process. We have
	;; to concatanete the pending string and the current string
	(setq string (concat pending string))))
  (if (bee-builder-talk-completed-p proc string)
      ;; ok, the command is completed, we may now execute it
      (progn
	;; there is no more pending string for PROC
	(bee-remove-pending proc)
	(bee-builder-execute-builder-command proc string))
    ;; we store the string as a pending string
    (bee-builder-add-pending-string string proc)))

;*---------------------------------------------------------------------*/
;*    bee-send-error ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-send-error (proc obj msg)
  (process-send-string proc (format "(error \"bee\" %S %S)\n" obj msg)))
  
;*---------------------------------------------------------------------*/
;*    bee-builder-execute-builder-command ...                          */
;*---------------------------------------------------------------------*/
(defun bee-builder-execute-builder-command (proc string)
  (cond
   ((and (string-match "(acknowledge \\([^)]+\\))[ \n]*" string)
	 (= (match-end 0) (length string)))
    (let ((buffer (substring string (match-beginning 1) (match-end 1))))
      (message "Interface builder for %S ready." buffer)))
   ((and (string-match "(load \\([^)]+\\))[ \n]*" string)
	 (= (match-end 0) (length string)))
    (if (not (bee-find-afile-create-p))
	(bee-send-error proc
			"No afile file loaded."
			string)
      (let* ((mname (substring string (match-beginning 1) (match-end 1)))
	     (fname (bee-find-afile-module mname)))
	(if (not fname)
	    (bee-send-error proc
			    "Can't find module"
			    mname)
	  (let ((iname (bee-interface-builder-module-p mname fname)))
	    (if (not iname)
		(bee-send-error proc "Can't find builder module" mname)
	      (if (bee-find-builder-editing iname)
		  (bee-send-error proc "Module already in use" mname)
		(progn
		  (message "Loading interface file %S" iname)
		  (process-send-string proc
				       (format "(load %S)\n" iname))))))))))
   (t
    (bee-send-error proc
		    "Illegal command"
		    string))))

;*---------------------------------------------------------------------*/
;*    bee-pending-string ...                                           */
;*---------------------------------------------------------------------*/
(defvar bee-pending-string '())

;*---------------------------------------------------------------------*/
;*    bee-pending-string-p ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-pending-string-p (proc)
  (let ((cell (assoc proc bee-pending-string)))
    (if (consp cell)
	(cdr cell)
      '())))

;*---------------------------------------------------------------------*/
;*    bee-builder-add-pending-string ...                               */
;*---------------------------------------------------------------------*/
(defun bee-builder-add-pending-string (string proc)
  (let ((cell (assoc proc bee-pending-string)))
    (if (consp cell)
	(rplacd cell string)
      (setq bee-pending-string
	    (cons (cons proc string ) bee-pending-string)))))

;*---------------------------------------------------------------------*/
;*    bee-remove-pending ...                                           */
;*---------------------------------------------------------------------*/
(defun bee-remove-pending (proc)
  (if (and (consp bee-pending-string)
	   (eq (car (car bee-pending-string)) proc))
      (setq bee-pending-string (cdr bee-pending-string))
    (let ((l bee-pending-string))
      (while (consp l)
	(if (eq (car (cdr l)) proc)
	    (progn
	      (rplacd l (cdr (cdr l)))
	      (setq l '()))
	  (setq l (cdr l)))))))
  
;*---------------------------------------------------------------------*/
;*    bee-builder-talk-completed-p ...                                 */
;*    This function returns true iff the STRING argument is a complete */
;*    ibuilder command. In order to find out if STRING is a complete   */
;*    command, we check out a kind of inlined command dictionary.      */
;*    If STRING is in the dictionary the command is completed.         */
;*---------------------------------------------------------------------*/
(defun bee-builder-talk-completed-p (proc string)
  ;; first, we match against (file-name variable-name)
  (if (string-match "([^ ]+ [^ ]+)[ \n]*" string)
      t
    '()))
		    

