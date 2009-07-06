;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-keymap.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May 26 07:34:07 1998                          */
;*    Last change :  Wed Dec 24 06:25:34 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bee menus                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-keymap)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'bee-config)
(require 'bee-autoload)
(require 'bee-mode)
(require 'bee-about-icon)
(require 'ude-config)
(require 'ude-autoload)
(require 'bug-autoload)

;*---------------------------------------------------------------------*/
;*    bee-return ...                                                   */
;*---------------------------------------------------------------------*/
(defun bee-return (&optional dummy)
  "Indent on [return]"
  (interactive)
  (if (= (point) 1)
      (newline)
    (newline-and-indent)))

;*---------------------------------------------------------------------*/
;*    bee-release ...                                                  */
;*---------------------------------------------------------------------*/
(defun bee-release ()
  (interactive)
  (ude-about (format "Ude release: %s\nBee release: %s\n\n%s\n\n%s"
		     ude-version
		     bee-version
		     ude-author
		     ude-url)
	     bee-about-icon))
  
;*---------------------------------------------------------------------*/
;*    bee-make-find-module-menu ...                                    */
;*    -------------------------------------------------------------    */
;*    For that menu, we look if the afile already exists. If it does   */
;*    exists, we create a sub menu with the module list. Otherwise,    */
;*    we ask the user for one specific menu. We do not create the      */
;*    afile file if it does not exists because it is a too long        */
;*    computation.                                                     */
;*---------------------------------------------------------------------*/
(defun bee-make-find-module-menu (module-list)
  (if module-list
      (let ((menu-list (ude-split-menu-entries
			(mapcar #'(lambda (x)
				   (vector x
					   (list 'bee-find-module-nokeymap x)
					   t))
				module-list)))
	    (ident (ude-fetch-identifier (point))))
	(if (stringp ident)
	    (append (list "Visit module..."
			  (vector ident
				  (list 'bee-find-module-nokeymap ident)
				  t)
			  "-")
		    menu-list)
	  (cons "Visit module..."
		menu-list)))
    ["module" bee-find-module t]))

;*---------------------------------------------------------------------*/
;*    bee-doc-menu ...                                                 */
;*---------------------------------------------------------------------*/
(defun bee-doc-menu ()
  (let ((manuals (bee-doc-installed-manuals)))
    (list (cons "Manuals..."
		(mapcar #'(lambda (x)
			    (vector x (list 'bee-doc-visit x) t))
			manuals)))))
  
;*---------------------------------------------------------------------*/
;*    bee-project-menu ...                                             */
;*---------------------------------------------------------------------*/
(defun bee-project-menu ()
  (list (list "Project..."
	      ["Root" ude-user-set-root-directory t]
	      ["Set name" ude-makefile-set-name t])))
  
;*---------------------------------------------------------------------*/
;*    bee-find-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-find-menu ()
  (list (list "Find..."
	      ["Manual entry" bee-doc-ident t]
	      ["Definition" bee-tags-find t]
	      (list "Definition as..."
		    ["Variable" bee-tags-find-variable t]
		    ["Class" bee-tags-find-class t]
		    ["Module" bee-find-module t])
	      ["Next definition" bee-tags-find-next t]
	      ["Info" bee-usage-info t]
	      ["Usage" bee-usage-find t])))

;*---------------------------------------------------------------------*/
;*    bee-find-interface-module-menu ...                               */
;*---------------------------------------------------------------------*/
(defun bee-find-interface-module-menu ()
  (if (not (bee-find-afile-create-p))
      '()
    (let ((old (bee-find-module-list))
	  (res '()))
      (while (consp old)
	(let ((fname (bee-find-afile-module (car old))))
	  (if fname
	      (if (bee-interface-builder-module-p (car old) fname)
		  (setq res (cons (car old) res))))
	  (setq old (cdr old))))
      (list (bee-make-find-module-menu (reverse res))))))

;*---------------------------------------------------------------------*/
;*    bee-builder-menu ...                                             */
;*---------------------------------------------------------------------*/
(defun bee-builder-menu ()
  (list (append (list "Builder..."
		      ["Start Builder" bee-interface-builder-start t]
		      ["Edit interface" bee-find-interface-builder-module t]))))
;*---------------------------------------------------------------------*/
;*    bee-makefile-menu ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-makefile-menu ()
  (list (list (concat (file-name-nondirectory ude-makefile) "...")
	      ["Edit Makefile"
	      ude-edit-makefile
	      (file-exists-p (concat ude-root-directory ude-makefile))]
	      ["Remove Makefile"
	      ude-remove-makefile
	      (file-exists-p (concat ude-root-directory ude-makefile))]
	      ["Add entry"
	      ude-add-user-makefile-entry
	      (file-exists-p (concat ude-root-directory ude-makefile))]
	      (if (file-exists-p (concat ude-root-directory ude-makefile))
		  (vector "Update" 'ude-update-makefile t)
		(list "Generate Makefile"
		      (vector "application"
			      (list 'ude-generate-makefile
				    bee-bmake-application-option)
			      t)
		      (vector "library"
			      (list 'ude-generate-makefile
				    bee-bmake-library-option)
			      t))))))

;*---------------------------------------------------------------------*/
;*    bee-bug-menu ...                                                 */
;*---------------------------------------------------------------------*/
(defun bee-bug-menu ()
  (list (list "Bug..."
	      ["Debugger" bee-debug t]
	      (if (and (fboundp 'bug-connected-buffer-p)
		       (bug-connected-buffer-p))
		  (vector "Disconnect buffer" 'bug-toggle-connect-buffer t)
		(vector "Connect buffer" 'bug-toggle-connect-buffer t)))))

;*---------------------------------------------------------------------*/
;*    bee-indent-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-indent-menu ()
  (list (list "Indent..."
	      ["Buffer" bee-external-indent t]
	      ["Define"
	      bee-indent-define
	      (condition-case ()
		  (save-excursion
		    (end-of-defun)
		    (beginning-of-defun))
		(error nil))]
	      ["Last sexp" bee-indent-last-sexp t]
	      ["Toplevel sexp" bee-indent-toplevel-sexp t])))

;*---------------------------------------------------------------------*/
;*    bee-repl-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-repl-menu ()
  (list (list "Repl..."
	      ["Other frame" ude-repl-other-frame t]
	      ["Buffer" ude-repl-send-buffer t]
	      ["Define"
	      bee-repl-send-define
	      (condition-case ()
		  (save-excursion
		    (end-of-defun)
		    (beginning-of-defun))
		(error nil))
	      ]
	      ["Last sexp" bee-repl-send-last-sexp t]
	      ["Toplevel sexp" bee-repl-send-toplevel-sexp t]
	      ["Region"
	      ude-repl-send-region
	      (and (region-active-p) (region-exists-p))])))

;*---------------------------------------------------------------------*/
;*    bee-expand-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-expand-menu ()
  (list (list "Expand..."
	      ["Buffer" bee-expand-buffer t]
	      ["Define"
	      bee-expand-define
	      (condition-case ()
		  (save-excursion
		    (end-of-defun)
		    (beginning-of-defun))
		(error nil))
	      ]
	      ["Last sexp" bee-expand-last-sexp t]
	      ["Toplevel sexp" bee-expand-toplevel-sexp t]
	      ["Region"
	      bee-expand-region
	      (and (region-active-p) (region-exists-p))])))

;*---------------------------------------------------------------------*/
;*    bee-profile-menu ...                                             */
;*---------------------------------------------------------------------*/
(defun bee-profile-menu ()
  '(("Profile..."
     ["Start profiler" bee-profiler-start t]
     ["Inspect function" bee-profiler-inspect t])))

;*---------------------------------------------------------------------*/
;*    bee-jinsight-menu ...                                            */
;*---------------------------------------------------------------------*/
(defun bee-jinsight-menu ()
  '(("Jinsight..."
     ["Start jinsight" bee-jinsight-start t]
     ("Module"
      ["Select" bee-jinsight-select-module t]
      ["Cpu time" bee-jinsight-module-cpu t]
      ["Allocations" bee-jinsight-module-instances t])
     ["Class Instances" bee-jinsight-class-instances t]
     ["Select Function" bee-jinsight-select-function t])))

;*---------------------------------------------------------------------*/
;*    bee-html-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-html-menu ()
  '(("Html..."
     ["Generate Html buffer" ude->html t]
     ["Write Html file" ude->html-file t])))
  
;*---------------------------------------------------------------------*/
;*    bee-ref-menu ...                                                 */
;*---------------------------------------------------------------------*/
(defun bee-ref-menu ()
  (list (list "@label..."
	      ["Make New label" ude-mouse-make-label t]
	      (vector (if (ude-mouse-ref-armed-p)
			  (concat "Make Ref (" (ude-mouse-ref-label) ")")
			"Make Ref")
		      'ude-mouse-make-ref '(ude-mouse-ref-armed-p))
	      (vector (if (ude-mouse-ref-armed-p)
			  (concat "Make Mutual Ref (" (ude-mouse-ref-label) ")")
			"Make Mutual Ref")
		      'ude-mouse-make-mutual-ref '(ude-mouse-ref-armed-p)))))
  
;*---------------------------------------------------------------------*/
;*    bee-module-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-module-menu ()
  `(("Module..."
     ["Import binding" bee-import-binding t]
     ["Import C file" bee-import-c-file t]
     "-"
     ["Export binding" bee-export-binding t]
     ["Export function" bee-export-function t]
     ["Export variable" bee-export-variable t]
     "-"
     ,(bee-make-find-module-menu (bee-find-module-list)))))

;*---------------------------------------------------------------------*/
;*    bee-user-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-user-menu ()
  (if (listp bee-user-menu)
      bee-user-menu
    nil))

;*---------------------------------------------------------------------*/
;*    bee-make-menu ...                                                */
;*---------------------------------------------------------------------*/
(defun bee-make-menu ()
  (let* ((separator (list "--:shadowEtchedOut"))
	 (dseparator (list "--:shadowEtchedOut"))
	 (user-menu (bee-user-menu))
	 (menu      (append (bee-doc-menu)
			    (bee-project-menu)
			    (bee-find-menu)
			    (if (bee-interface-builder-ready-p)
				(bee-builder-menu)
			      '())
			    (bee-module-menu)
			    separator
			    (ude-compile-menu)
			    (bee-makefile-menu)
			    (bee-repl-menu)
			    separator
			    (bee-bug-menu)
			    (bee-indent-menu)
			    (bee-expand-menu)
			    (bee-profile-menu)
;* 			    (bee-jinsight-menu)                        */
;* 			    (bee-html-menu)                            */
			    (bee-ref-menu)
			    (if user-menu
				(append dseparator user-menu)
			      '()))))
    menu))

;*---------------------------------------------------------------------*/
;*    bee-make-version-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-make-version-menu ()
  '(["Checkout file version" ude-checkout-file-version t]
     ["Compare to file version" ude-diff-file-version t]
     "--:shadowEtchedIn"
     ["Checkin project version" ude-checkin-project t]
     "--:shadowEtchedIn"
     ["Produce tar.gz" ude-tar-gz-project t]))

;*---------------------------------------------------------------------*/
;*    bee-make-customize-menu ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-make-customize-menu ()
  '(["Release..." bee-release t]
     "--:shadowEtchedOut"
     ["Ude..." ude-customize t]
     ["Bee..." bee-customize t]))

;*---------------------------------------------------------------------*/
;*    bee-popup-menu ...                                               */
;*---------------------------------------------------------------------*/
(defun bee-popup-menu (event)
  (interactive "e")
  (popup-menu (cons "Bee" (bee-make-menu))))

;*---------------------------------------------------------------------*/
;*    bee-region-popup-entries ...                                     */
;*---------------------------------------------------------------------*/
(defvar bee-region-popup-entries
  (list ["manual" bee-doc-visit t]
	["eval" ude-repl-send-region t]
	["expand" bee-expand-region t]
	"--:shadowDoubleEtchedOut"
	["kill region" kill-region t])
  "*The entry for the region menu.")
(make-variable-buffer-local 'bee-region-popup-entry)

;*---------------------------------------------------------------------*/
;*    bee-add-region-popup-entry ...                                   */
;*---------------------------------------------------------------------*/
(defun bee-add-region-popup-entry (entries)
  "Add ENTRIES (a list) to the region popup menu."
  (setq bee-region-popup-entries (cons entries bee-region-popup-entries)))

;*---------------------------------------------------------------------*/
;*    bee-remove-region-popup-entry ...                                */
;*---------------------------------------------------------------------*/
(defun bee-remove-region-popup-entry (entries)
  "Add ENTRIES (a list) to the region popup menu."
  (setq bee-region-popup-entries (delq entries bee-region-popup-entries)))

;*---------------------------------------------------------------------*/
;*    bee-region-popup-menu ...                                        */
;*---------------------------------------------------------------------*/
(defun bee-region-popup-menu (event)
  (interactive "e")
  ;; we have to rebuild the cache first
  (let ((l bee-region-popup-entries)
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
;*    bee keymap ...                                                   */
;*---------------------------------------------------------------------*/
(defvar bee-map-prefix ?\c)
(defvar bee-prefixed-map (make-sparse-keymap))

;*---------------------------------------------------------------------*/
;*    bee-keymap-init ...                                              */
;*---------------------------------------------------------------------*/
(defun bee-keymap-init ()

  ;; bee bindings
  (define-key bee-mode-map "\t"               'bee-indent-line)
  (define-key bee-mode-map "\e\C-q"           'bee-indent-sexp)
  (define-key bee-mode-map "\C-m"             'bee-return)
  (define-key bee-mode-map "\e\C-m"           'newline)
  (define-key bee-mode-map "\e\C- "           'mark-sexp)

  ;; C-' keymap
  (define-key bee-mode-map [(control \c)]     'bee-prefix)
  (fset 'bee-prefix bee-prefixed-map)

  ;; finders
  (define-key bee-prefixed-map "\C-di"        'bee-doc-visit)
  (define-key bee-prefixed-map "\C-ds"        'bee-doc-ident)
  (define-key bee-prefixed-map "\C-du"        'bee-usage-find)
  (define-key bee-prefixed-map "\C-df"        'bee-tags-find-variable)
  (define-key bee-prefixed-map "\C-dc"        'bee-tags-find-class)
  (define-key bee-prefixed-map "\C-dm"        'bee-find-module)
  (define-key bee-mode-map "\C-x5."           'bee-tags-find)
  (define-key bee-mode-map "\e."              'bee-tag-find)
  (define-key bee-mode-map "\e,"              'bee-tags-find-next)

  ;; interface builder
  (define-key bee-prefixed-map "\C-g\C-s"     'bee-interface-builder-start)
  (define-key bee-prefixed-map "\C-g\C-f"     'bee-find-interface-builder-module)

  ;; project
  (define-key bee-prefixed-map "\C-p\C-r"     'ude-user-set-root-directory)
  (define-key bee-prefixed-map "\C-p\C-n"     'ude-makefile-set-name)
    
  ;; compilation
  (define-key bee-prefixed-map "\C-c\C-c"     'ude-mode-compile-from-menu)
  (define-key bee-prefixed-map "\C-c\C-x"     'ude-mode-jcompile-from-menu)
  (define-key bee-prefixed-map "\C-c\C-r"     'ude-execute)
  (define-key bee-prefixed-map "\C-ck"        'kill-compilation)
  (define-key bee-prefixed-map "\C-cs"        'ude-set-compile-command)
  (define-key bee-prefixed-map "\C-cm"        'ude-view-last-compile-messages)

  ;; makefile
  (define-key bee-prefixed-map "\C-ce"        'ude-edit-makefile)
  (define-key bee-prefixed-map "\C-cn"        'ude-add-user-makefile-entry)
  (define-key bee-prefixed-map "\C-cr"        'ude-remove-makefile)
  (define-key bee-prefixed-map "\C-ca"        'ude-generate-makefile)
  (define-key bee-prefixed-map "\C-cu"        'ude-update-makefile)
  (define-key bee-prefixed-map "\C-c\C-d"     'ude-makefile-debug-mode)
  (define-key bee-prefixed-map "\C-c\C-v"     'ude-makefile-devel-mode)
  (define-key bee-prefixed-map "\C-c\C-f"     'ude-makefile-final-mode)

  ;; repl
  (define-key bee-prefixed-map "\C-r\C-r"     'ude-repl-other-frame)
  (define-key bee-prefixed-map "\C-rb"        'ude-repl-send-buffer)
  (define-key bee-prefixed-map "\C-rd"        'bee-repl-send-define)
  (define-key bee-prefixed-map "\C-rl"        'bee-repl-send-last-sexp)
  (define-key bee-prefixed-map "\C-rt"        'bee-repl-send-toplevel-sexp)
  (define-key bee-prefixed-map "\C-rr"        'ude-repl-send-region)

  ;; bugloo
  (define-key bee-prefixed-map "\C-b\C-b"     'bee-debug)
  (define-key bee-prefixed-map "\C-b\c"       'bee-toggle-connect-buffer)

  ;; indent
  (define-key bee-prefixed-map "\C-i\C-i"     'bee-external-indent)
  (define-key bee-prefixed-map "\C-i\C-d"     'bee-indent-define)
  (define-key bee-prefixed-map "\C-i\C-l"     'bee-indent-last-sexp)
  (define-key bee-prefixed-map "\C-i\C-t"     'bee-indent-toplevel-sexp)

  ;; expand
  (define-key bee-prefixed-map "\C-e\C-e"     'bee-expand-buffer)
  (define-key bee-prefixed-map "\C-e\C-d"     'bee-expand-define)
  (define-key bee-prefixed-map "\C-e\C-l"     'bee-expand-last-sexp)
  (define-key bee-prefixed-map "\C-e\C-t"     'bee-expand-toplevel-sexp)
  (define-key bee-prefixed-map "\C-e\C-r"     'bee-expand-region)

  ;; module
  (define-key bee-prefixed-map "\C-mi"        'bee-import-binding)
  (define-key bee-prefixed-map "\C-mc"        'bee-import-c-file)
  (define-key bee-prefixed-map "\C-mb"        'bee-export-binding)
  (define-key bee-prefixed-map "\C-mf"        'bee-export-function)
  (define-key bee-prefixed-map "\C-mv"        'bee-export-variable)

  ;; version
  (define-key bee-prefixed-map "\C-vi"        'ude-checkin-project)
  (define-key bee-prefixed-map "\C-vo"        'ude-checkout-file-version)
  (define-key bee-prefixed-map "\C-vd"        'ude-diff-file-version)

  ;; profile
  (define-key bee-prefixed-map "\C-pc"        'bee-profiler-start)
  (define-key bee-prefixed-map "\C-pi"        'bee-profiler-inspect)

  ;; jinsight
  (define-key bee-prefixed-map "\C-jc"        'bee-jinsight-start)
  (define-key bee-prefixed-map "\C-js"        'bee-jinsight-select-module)
  (define-key bee-prefixed-map "\C-jf"        'bee-jinsight-select-function)
  (define-key bee-prefixed-map "\C-jm"        'bee-jinsight-module-cpu)
  (define-key bee-prefixed-map "\C-jM"        'bee-jinsight-module-instances)
  (define-key bee-prefixed-map "\C-jk"        'bee-jinsight-class-instances)

  ;; mouse bindings
  (define-key bee-mode-map ude-mouse-binding  'ude-predicate-mouse-event)

  ;; ref bindings
  (define-key bee-prefixed-map "\C-ml"        'ude-mouse-make-label)
  (define-key bee-prefixed-map "\C-ms"        'ude-mouse-make-ref)
  (define-key bee-prefixed-map "\C-mm"        'ude-mouse-make-mutual-ref)

  (if bee-elisp-like-keymap-p
      (progn
	(define-key bee-mode-map "\C-c;"      'comment-region)
	(define-key bee-mode-map "\C-x\C-e"   'bee-repl-send-last-sexp)
	(define-key bee-mode-map "\C-\M-x"    'bee-repl-send-define)))
  
  ;; the popup menus
  (ude-add-menu #'(lambda (event) t)
		'bee-popup-menu)
  (ude-add-menu #'(lambda (event) (and (region-active-p) (region-exists-p)))
		'bee-region-popup-menu)

  ;; the menu bar
  (ude-menubar-set "Bee" (bee-make-menu))
  (ude-menubar-set "Version" (bee-make-version-menu))
  (ude-menubar-set "Config" (bee-make-customize-menu)))
