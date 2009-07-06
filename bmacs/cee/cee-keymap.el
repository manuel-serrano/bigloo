;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-keymap.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 08:55:20 1998                          */
;*    Last change :  Wed Jan 30 10:34:39 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The CEE key bindings.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-keymap)
(require 'cee-indent)
(require 'ude-autoload)
(require 'ude-config)

;*---------------------------------------------------------------------*/
;*    cee-return ...                                                   */
;*---------------------------------------------------------------------*/
(defun cee-return (&optional dummy)
   "On indent automatiquement sur un RET.
usage: (c-return)  -- [RET]"
   (interactive)
   (if (= (point) 1)
       (newline)
     (newline-and-indent)))

;*---------------------------------------------------------------------*/
;*    cee-release ...                                                  */
;*---------------------------------------------------------------------*/
(defun cee-release ()
  (interactive)
  (message-box
   (format "Ude release: %s\nCee release: %s\n"
	   ude-version
	   cee-version)))
  
;*---------------------------------------------------------------------*/
;*    cee-find-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun cee-find-menu ()
  (list (list "Find..."
	      ["manual entry" cee-docline-ident t]
	      ["manual" cee-docline t]
	      "-"
	      ["definition" cee-find-definition t])))

;*---------------------------------------------------------------------*/
;*    cee-makefile-menu ...                                            */
;*---------------------------------------------------------------------*/
(defun cee-makefile-menu ()
  (list (list (concat (file-name-nondirectory ude-makefile) "...")
	      ["Edit Makefile"
	      ude-edit-makefile
	      (file-exists-p (concat ude-root-directory ude-makefile))])))

;*---------------------------------------------------------------------*/
;*    cee-gdb-menu ...                                                 */
;*---------------------------------------------------------------------*/
(defun cee-gdb-menu ()
  (list (list "Gdb..."
	      ["Gdb other frame" gdb-other-frame t]
	      (if (and (fboundp 'dbg-connected-buffer-p)
		       (dbg-connected-buffer-p))
		  (vector "Disconnect buffer" 'dbg-toggle-connect-buffer t)
		(vector "Connect buffer" 'dbg-toggle-connect-buffer t)))))

;*---------------------------------------------------------------------*/
;*    cee-indent-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun cee-indent-menu ()
  (list (list "Indent..."
	      ["buffer" cee-external-indent t]
	      ["define" c-indent-defun t]
	      ["exp" cee-indent-exp t])))

;*---------------------------------------------------------------------*/
;*    cee-profile-menu ...                                             */
;*---------------------------------------------------------------------*/
(defun cee-profile-menu ()
  '(("Profile..."
     ["Compile for profile" ude-compile-for-profile t]
     ["Run for profile" ude-run-for-profile t])))

;*---------------------------------------------------------------------*/
;*    cee-user-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun cee-user-menu ()
  (if (listp cee-user-menu)
      cee-user-menu
    nil))

;*---------------------------------------------------------------------*/
;*    cee-make-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun cee-make-menu ()
  (let* ((separator (list "--:shadowEtchedOut"))
	 (user-menu (cee-user-menu))
	 (menu      (append (cee-find-menu)
			    (ude-compile-menu)
			    (cee-makefile-menu)
			    (cee-gdb-menu)
			    (cee-indent-menu)
			    (cee-profile-menu)
			    (if user-menu
				(append separator user-menu)
			      '()))))
    menu))

;*---------------------------------------------------------------------*/
;*    cee-make-version-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun cee-make-version-menu ()
  '(["Checkout file version" ude-checkout-file-version t]
     ["Compare to file version" ude-diff-file-version t]
     "--:shadowEtchedIn"
     ["Checkin project version" ude-checkin-project t]
     "--:shadowEtchedIn"
     ["Produce tar.gz" ude-tar-gz-project t]))

;*---------------------------------------------------------------------*/
;*    cee-make-customize-menu ...                                      */
;*---------------------------------------------------------------------*/
(defun cee-make-customize-menu ()
  '(["Release..." cee-release t]
     "--:shadowEtchedOut"
     ["Ude..." ude-customize t]
     ["Cee..." cee-customize t]))

;*---------------------------------------------------------------------*/
;*    cee-popup-menu ...                                               */
;*---------------------------------------------------------------------*/
(defun cee-popup-menu (event)
  (interactive "e")
  (popup-menu (cons "Cee" (cee-make-menu))))

;*---------------------------------------------------------------------*/
;*    cee-region-popup-entries ...                                     */
;*---------------------------------------------------------------------*/
(defvar cee-region-popup-entries
  (list ["kill region" kill-region t])
  "*The entry for the region menu.")
(make-variable-buffer-local 'cee-region-popup-entry)

;*---------------------------------------------------------------------*/
;*    cee-add-region-popup-entry ...                                   */
;*---------------------------------------------------------------------*/
(defun cee-add-region-popup-entry (entries)
  "Add ENTRIES (a list) to the region popup menu."
  (setq cee-region-popup-entries (cons entries cee-region-popup-entries)))

;*---------------------------------------------------------------------*/
;*    cee-remove-region-popup-entry ...                                */
;*---------------------------------------------------------------------*/
(defun cee-remove-region-popup-entry (entries)
  "Add ENTRIES (a list) to the region popup menu."
  (setq cee-region-popup-entries (delq entries cee-region-popup-entries)))

;*---------------------------------------------------------------------*/
;*    cee-region-popup-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun cee-region-popup-menu (event)
  (interactive "e")
  ;; we have to rebuild the cache first
  (let ((l cee-region-popup-entries)
	(menu '()))
    (while (consp l)
      (cond
       ((functionp (car l))
	(setq menu (append (reverse (funcall (car l) event)) menu)))
       ((consp (car l))
	(setq menu (append (car l) menu)))
       (t
	(setq menu (cons (car l) menu))))
      (setq l (cdr l)))
    (popup-menu (cons (concat
		       "`"
		       (ude-region-excerpt (region-beginning) (region-end))
		       "'")
		      (nreverse menu)))))

;*---------------------------------------------------------------------*/
;*    cee keymap ...                                                   */
;*---------------------------------------------------------------------*/
(defvar cee-map-prefix ?\c)
(defvar cee-prefixed-map (make-sparse-keymap))

;*---------------------------------------------------------------------*/
;*    cee-keymap-init ...                                              */
;*---------------------------------------------------------------------*/
(defun cee-keymap-init ()
  (let ((cee-mode-map (current-local-map)))

    ;; cee bindings
    (local-unset-key "\ee")
    (define-key cee-mode-map "\C-m"   'cee-return)
    (define-key cee-mode-map "\e\C-m" 'newline)
    (define-key cee-mode-map "\e\C-q" 'cee-indent-exp)
    (define-key cee-mode-map "\e\C-a" 'unix-c-indent)
  
    ;; C-' keymap
    (define-key cee-mode-map [(control \c)]    'cee-prefix)
    (fset 'cee-prefix cee-prefixed-map)

    ;; finders
    (define-key cee-prefixed-map "\C-di"       'cee-docline)
    (define-key cee-prefixed-map "\C-ds"       'cee-docline-ident)
    (define-key cee-prefixed-map "\C-du"       'cee-usage-find)
    (define-key cee-prefixed-map "\C-df"       'cee-tags-find-variable)
    (define-key cee-mode-map "\C-x5."          'cee-find-definition)
    (define-key cee-mode-map "\e."             'cee-tag-find)
    (define-key cee-mode-map "\e,"             'cee-tags-find-next)
  
    ;; compilation
    (define-key cee-prefixed-map "\C-c\C-c"    'ude-mode-compile-from-menu)
    (define-key cee-prefixed-map "\C-ck"       'kill-compilation)
    (define-key cee-prefixed-map "\C-cs"       'ude-set-compile-command)
    (define-key cee-prefixed-map "\C-cm"       'ude-view-last-compile-messages)

    ;; makefile
    (define-key cee-prefixed-map "\C-ce"       'ude-edit-makefile)
    (define-key cee-prefixed-map "\C-c\C-d"    'ude-makefile-debug-mode)
    (define-key cee-prefixed-map "\C-c\C-v"    'ude-makefile-devel-mode)
    (define-key cee-prefixed-map "\C-c\C-f"    'ude-makefile-final-mode)

    ;; gdb
    (define-key cee-prefixed-map "\C-b\C-b"    'gdb-other-frame)
    (define-key cee-prefixed-map "\C-b\c"      'cee-toggle-connect-buffer)

    ;; indent
    (define-key cee-prefixed-map "\C-i\C-i"    'cee-external-indent)
    (define-key cee-prefixed-map "\C-i\C-d"    'c-indent-define)

    ;; version
    (define-key cee-prefixed-map "\C-vi"       'ude-checkin-project)
    (define-key cee-prefixed-map "\C-vo"       'ude-checkout-file-version)
    (define-key cee-prefixed-map "\C-vd"       'ude-diff-file-version)

    ;; profile
    (define-key cee-prefixed-map "\C-pc"       'ude-compile-for-profile)
    (define-key cee-prefixed-map "\C-pr"       'ude-run-for-profile)
    
    ;; mouse bindings
    (define-key cee-mode-map ude-mouse-binding 'ude-predicate-mouse-event))

  ;; the popup menus
  (ude-add-menu #'(lambda (event) t)
		'cee-popup-menu)
  (ude-add-menu #'(lambda (event) (and (region-active-p) (region-exists-p)))
		'cee-region-popup-menu)

  ;; the menu bar
  (ude-menubar-set "Cee" (cee-make-menu))
  (ude-menubar-set "Version" (cee-make-version-menu))
  (ude-menubar-set "Config" (cee-make-customize-menu)))
