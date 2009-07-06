;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-keymap.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 08:55:20 1998                          */
;*    Last change :  Wed Jan 23 17:32:45 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The LEE key bindings.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-keymap)
(require 'lee-indent)
(require 'ude-autoload)
(require 'ude-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    lee-return ...                                                   */
;*---------------------------------------------------------------------*/
(defun lee-return (&optional dummy)
   "On indent automatiquement sur un RET.
usage: (c-return)  -- [RET]"
   (interactive)
   (if (= (point) 1)
       (newline)
     (newline-and-indent)))

;*---------------------------------------------------------------------*/
;*    lee-indent-buffer ...                                            */
;*---------------------------------------------------------------------*/
(defun lee-indent-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (call-interactively 'indent-region)))

;*---------------------------------------------------------------------*/
;*    lee-release ...                                                  */
;*---------------------------------------------------------------------*/
(defun lee-release ()
  (interactive)
  (message-box
   (format "Ude release: %s\nLee release: %s\n"
	   ude-version
	   lee-version)))
  
;*---------------------------------------------------------------------*/
;*    lee-find-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun lee-find-menu ()
  (list (list "Find..."
	      ["manual" lee-docline t]
	      ["definition" lee-find-definition t])))

;*---------------------------------------------------------------------*/
;*    lee-makefile-menu ...                                            */
;*---------------------------------------------------------------------*/
(defun lee-makefile-menu ()
  (list (list (concat (file-name-nondirectory ude-makefile) "...")
	      ["Edit Makefile"
	      ude-edit-makefile
	      (file-exists-p (concat ude-root-directory ude-makefile))])))

;*---------------------------------------------------------------------*/
;*    lee-indent-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun lee-indent-menu ()
  (list (list "Indent..."
	      ["define" c-indent-defun t]
	      ["exp" lee-indent-exp t])))

;*---------------------------------------------------------------------*/
;*    lee-user-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun lee-user-menu ()
  (if (listp lee-user-menu)
      lee-user-menu
    nil))

;*---------------------------------------------------------------------*/
;*    lee-make-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun lee-make-menu ()
  (let* ((separator (list "--:shadowEtchedOut"))
	 (user-menu (lee-user-menu))
	 (menu      (append (lee-find-menu)
			    (ude-compile-menu)
			    (lee-makefile-menu)
			    (lee-indent-menu)
			    (if user-menu
				(append separator user-menu)
			      '()))))
    menu))

;*---------------------------------------------------------------------*/
;*    lee-make-version-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun lee-make-version-menu ()
  '(["Checkout file version" ude-checkout-file-version t]
     ["Compare to file version" ude-diff-file-version t]
     "--:shadowEtchedIn"
     ["Checkin project version" ude-checkin-project t]
     "--:shadowEtchedIn"
     ["Produce tar.gz" ude-tar-gz-project t]))

;*---------------------------------------------------------------------*/
;*    lee-make-customize-menu ...                                      */
;*---------------------------------------------------------------------*/
(defun lee-make-customize-menu ()
  '(["Release..." lee-release t]
     "--:shadowEtchedOut"
     ["Ude..." ude-customize t]
     ["Lee..." lee-customize t]))

;*---------------------------------------------------------------------*/
;*    lee-popup-menu ...                                               */
;*---------------------------------------------------------------------*/
(defun lee-popup-menu (event)
  (interactive "e")
  (popup-menu (cons "Lee" (lee-make-menu))))

;*---------------------------------------------------------------------*/
;*    lee-region-popup-entries ...                                     */
;*---------------------------------------------------------------------*/
(defvar lee-region-popup-entries
  (list ["kill region" kill-region t])
  "*The entry for the region menu.")
(make-variable-buffer-local 'lee-region-popup-entry)

;*---------------------------------------------------------------------*/
;*    lee-add-region-popup-entry ...                                   */
;*---------------------------------------------------------------------*/
(defun lee-add-region-popup-entry (entries)
  "Add ENTRIES (a list) to the region popup menu."
  (setq lee-region-popup-entries (cons entries lee-region-popup-entries)))

;*---------------------------------------------------------------------*/
;*    lee-remove-region-popup-entry ...                                */
;*---------------------------------------------------------------------*/
(defun lee-remove-region-popup-entry (entries)
  "Add ENTRIES (a list) to the region popup menu."
  (setq lee-region-popup-entries (delq entries lee-region-popup-entries)))

;*---------------------------------------------------------------------*/
;*    lee-region-popup-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun lee-region-popup-menu (event)
  (interactive "e")
  ;; we have to rebuild the cache first
  (let ((l lee-region-popup-entries)
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
;*    lee keymap ...                                                   */
;*---------------------------------------------------------------------*/
(defvar lee-map-prefix ?\c)
(defvar lee-prefixed-map (make-sparse-keymap))

;*---------------------------------------------------------------------*/
;*    lee-keymap-init ...                                              */
;*---------------------------------------------------------------------*/
(defun lee-keymap-init ()
  (let ((lee-mode-map (current-local-map)))

    ;; lee bindings
    (local-unset-key "\ee")
    (define-key lee-mode-map "\C-m"   'lee-return)
    (define-key lee-mode-map "\e\C-m" 'newline)
    (define-key lee-mode-map "\e\C-q" 'indent-sexp)
    (define-key lee-mode-map "\e\C-a" 'lee-indent-buffer)
  
    ;; C-' keymap
    (define-key lee-mode-map [(control \c)]    'lee-prefix)
    (fset 'lee-prefix lee-prefixed-map)

    ;; finders
    (define-key lee-prefixed-map "\C-di"       'lee-docline)
    (define-key lee-prefixed-map "\C-du"       'lee-usage-find)
    (define-key lee-prefixed-map "\C-df"       'lee-tags-find-variable)
    (define-key lee-mode-map "\C-x5."          'lee-find-definition)
    (define-key lee-mode-map "\e."             'lee-tag-find)
    (define-key lee-mode-map "\e,"             'lee-tags-find-next)
  
    ;; compilation
    (define-key lee-prefixed-map "\C-c\C-c"    'ude-mode-compile-from-menu)
    (define-key lee-prefixed-map "\C-ck"       'kill-compilation)
    (define-key lee-prefixed-map "\C-cs"       'ude-set-compile-command)
    (define-key lee-prefixed-map "\C-cm"       'ude-view-last-compile-messages)

    ;; makefile
    (define-key lee-prefixed-map "\C-ce"       'ude-edit-makefile)

    ;; indent
    (define-key lee-prefixed-map "\C-i\C-d"    'c-indent-define)

    ;; version
    (define-key lee-prefixed-map "\C-vi"       'ude-checkin-project)
    (define-key lee-prefixed-map "\C-vo"       'ude-checkout-file-version)
    (define-key lee-prefixed-map "\C-vd"       'ude-diff-file-version)

    ;; mouse bindings
    (define-key lee-mode-map ude-mouse-binding 'ude-predicate-mouse-event))

  ;; the popup menus
  (ude-add-menu #'(lambda (event) t)
		'lee-popup-menu)
  (ude-add-menu #'(lambda (event) (and (region-active-p) (region-exists-p)))
		'lee-region-popup-menu)

  ;; the menu bar
  (ude-menubar-set "Lee" (lee-make-menu))
  (ude-menubar-set "Version" (lee-make-version-menu))
  (ude-menubar-set "Config" (lee-make-customize-menu)))
