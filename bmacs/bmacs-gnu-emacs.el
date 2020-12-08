;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bmacs-gnu-emacs.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan 20 11:48:39 2002                          */
;*    Last change :  Mon Oct  8 14:04:01 2012 (serrano)                */
;*    Copyright   :  2002-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Gnu-Emacs (hum, hum, Emacs) specific file.                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bmacs-gnu-emacs)
(require 'ude-autoload)

;*---------------------------------------------------------------------*/
;*    bmacs-emacs-version ...                                          */
;*---------------------------------------------------------------------*/
(defvar bmacs-emacs-version
  (cond
   ((string-match "GNU Emacs 27" (emacs-version)) 27)
   ((string-match "GNU Emacs 26" (emacs-version)) 26)
   ((string-match "GNU Emacs 25" (emacs-version)) 25)
   ((string-match "GNU Emacs 24" (emacs-version)) 24)
   ((string-match "GNU Emacs 23" (emacs-version)) 23)
   ((string-match "GNU Emacs 22" (emacs-version)) 22)
   ((string-match "GNU Emacs 21" (emacs-version)) 21)
   ((string-match "GNU Emacs 20" (emacs-version)) 20)
   (t 19)))

;*---------------------------------------------------------------------*/
;*    bmacs-etags ...                                                  */
;*---------------------------------------------------------------------*/
(defvar bmacs-etags 'etags)

;*---------------------------------------------------------------------*/
;*    device-sound-enabled-p ...                                       */
;*---------------------------------------------------------------------*/
(defun device-sound-enabled-p ()
  t)

;*---------------------------------------------------------------------*/
;*    display-message ...                                              */
;*---------------------------------------------------------------------*/
(defun display-message (log msg)
  (message msg))

;*---------------------------------------------------------------------*/
;*    ude-modeline-root-text ...                                       */
;*---------------------------------------------------------------------*/
(defvar ude-modeline-root-text nil)
(make-variable-buffer-local 'ude-modeline-root-text)

;*---------------------------------------------------------------------*/
;*    toolbar settings ...                                             */
;*---------------------------------------------------------------------*/
(setq tool-bar-button-relief 1)
(if (= 22 bmacs-emacs-version)
    (setq tool-bar-button-margin 1)
    (setq tool-bar-button-margin 0))


;*---------------------------------------------------------------------*/
;*    Faces for plugins                                                */
;*---------------------------------------------------------------------*/
(copy-face 'cursor 'text-cursor)
(copy-face 'mode-line 'modeline-buffer-id)
(copy-face 'mode-line 'modeline-mousable)

;*---------------------------------------------------------------------*/
;*    ude-set-root-modeline ...                                        */
;*    -------------------------------------------------------------    */
;*    This function sets the modeline according to the root directory. */
;*---------------------------------------------------------------------*/
(defun ude-set-root-modeline (&optional txt)
  (let ((text (if txt
		  txt
		(propertize (ude-root-modeline-text)
			    'help-echo "mouse-1 to set"
			    'local-map (make-mode-line-mouse-map
					'mouse-1
					'ude-user-set-root-directory)
			    'face (if (stringp ude-root-directory)
				      'ude-modeline-root-face
				    'ude-modeline-no-root-face)))))
    (ude-root-modeline-add text)))

;*---------------------------------------------------------------------*/
;*    ude-root-modeline-add ...                                        */
;*---------------------------------------------------------------------*/
(defun ude-root-modeline-add (text)
  (make-variable-buffer-local 'mode-line-format)
  (if (not ude-modeline-root-text)
      ;; add a new root
      (progn
	(setq ude-modeline-root-text text)
	(setq mode-line-format
	      (cons (car mode-line-format)
		    (cons (car (cdr mode-line-format))
			  (cons "-"
				(cons text
				      (cdr (cdr mode-line-format))))))))
    ;; replace a root
    (let ((lst mode-line-format))
      (while (and (consp lst) (not (eq (car lst) ude-modeline-root-text)))
	(setq lst (cdr lst)))
      (if (eq (car lst) ude-modeline-root-text)
	  (progn
	    (setq ude-modeline-root-text text)
	    (rplaca lst text))
	(progn
	  (setq ude-modeline-root-text nil)
	  (ude-root-modeline-add text))))))

;*---------------------------------------------------------------------*/
;*    ude-make-image ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-make-image (id &optional ascent)
  (let ((descr (if (symbolp id)
		   (if (or t ude-tool-bar-force-legend-p)
		       (eval id)
		     (let ((i (intern
			       (concat (symbol-name id) "-sans-legend"))))
		       (if (boundp i)
			   (eval i)
			 (eval id))))
		 id)))
    (find-image (if (ude-string-xpm-p descr)
		    (if ascent
			`((:type xpm :ascent ,ascent :data ,descr))
		      `((:type xpm :data ,descr)))
		  (if ascent
		      `((:type xpm :ascent ,ascent :file ,descr))
		    `((:type xpm :file ,descr)))))))

;*---------------------------------------------------------------------*/
;*    ude-make-glyph ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-make-glyph (descr &optional ascent)
  (ude-make-image descr ascent))

;*---------------------------------------------------------------------*/
;*    tool-bar-separator-num ...                                       */
;*---------------------------------------------------------------------*/
(defvar tool-bar-separator-num 0)

;*---------------------------------------------------------------------*/
;*    tool-bar-separator ...                                           */
;*---------------------------------------------------------------------*/
(defun tool-bar-separator (map num)
  (let ((str (format "--%S" num)))
    (define-key map (vector 'tool-bar (intern str))
      `(menu-item ,str ,(lambda () (interactive) 0)
		  :image ,tool-bar-empty-image
		  :enable nil))))

;*---------------------------------------------------------------------*/
;*    tool-bar-empty-image ...                                         */
;*---------------------------------------------------------------------*/
(defvar tool-bar-empty-image
  (find-image
   '((:type xpm :file "tool-bar-empty-image.xpm"))))

;*---------------------------------------------------------------------*/
;*    ude-gensym-counter ...                                           */
;*---------------------------------------------------------------------*/
(defvar ude-gensym-counter 17051966)

;*---------------------------------------------------------------------*/
;*    ude-gensym ...                                                   */
;*---------------------------------------------------------------------*/
(defun ude-gensym (&optional arg)
  (setq ude-gensym-counter (1+ ude-gensym-counter))
  (let ((pref (cond
               ((not arg) 'G)
               ((symbolp arg) (symbol-name arg))
               ((stringp arg) arg)
               (t 'G))))
    (intern (format "%s%d" pref ude-gensym-counter))))

;*---------------------------------------------------------------------*/
;*    handle-names ...                                                 */
;*---------------------------------------------------------------------*/
(defvar handle-names '())

;*---------------------------------------------------------------------*/
;*    ude-toolbar-set ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-toolbar-set (lst)
  (when (= bmacs-emacs-version 22)
    (setq tool-bar-images-pixel-height 20))
  (let ((map (or (current-local-map) (make-sparse-keymap)))
	(num 0))
    (if (not (current-local-map))
	(use-local-map map))
    (set (make-local-variable 'tool-bar-map) map)
    ;; invert the list
    (setq lst (reverse lst))
    ;; add each entry to the menu
    (while (consp lst)
      (cond
       ((eq (car lst) '--)
	;; separator
	(setq num (1+ num))
	(unless (>= 22 bmacs-emacs-version)
	  (tool-bar-separator map num))
	(setq lst (cdr lst)))
       ((eq (car lst) '-->)
	;; flush right
	(setq lst (cdr lst)))
       ((= (length (car lst)) 3)
	;; plain entry
	(let* ((l (car lst))
	       (img (car l))
	       (cmd (car (cdr l)))
	       (help (car (cdr (cdr l))))
               (key (format "%s" cmd)))
	  (define-key map (vector 'tool-bar (intern key))
	    `(menu-item ,key ,(if (consp cmd)
				  `(lambda () (interactive) ,cmd)
				cmd)
			:image ,(ude-make-image img)
			:help ,help))
	  (setq lst (cdr lst))))
       (t
	;; illegal entry
	(error "ude-toolbar-set:illegal entry format `%S'" (car lst)))))
    (unless (>= 22 bmacs-emacs-version)
      (let ((handle-name (ude-gensym 'handle)))
	(setq handle-names (cons handle-name handle-names))
	(tool-bar-separator map 0)
	(if (null (cdr handle-names))
	    (define-key map (vector 'tool-bar handle-name)
	      `(menu-item "handle"
			  (lambda () (interactive) (tool-bar-mode))
			  :image ,(ude-make-image ude-close-toolbar-icon))))))))

;*---------------------------------------------------------------------*/
;*    ude-toolbar-add ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-toolbar-add (lst)
  (let ((map (current-local-map))
	(num 0))
    ;; invert the list
    (setq lst (reverse lst))
    ;; add each entry to the menu
    (while (consp lst)
      (cond
       ((eq (car lst) '--)
	;; separator
	(setq num (1+ num))
	(unless (>= 22 bmacs-emacs-version)
	  (tool-bar-separator map num))
	(setq lst (cdr lst)))
       ((eq (car lst) '-->)
	;; flush right
	(setq lst (cdr lst)))
       ((= (length (car lst)) 3)
	;; plain entry
	(let* ((l (car lst))
	       (img (car l))
	       (cmd (car (cdr l)))
	       (help (car (cdr (cdr l))))
	       (key (format "%s" cmd)))
	  (define-key map (vector 'tool-bar (intern key))
	    `(menu-item ,key ,(if (consp cmd)
				  `(lambda () (interactive) ,cmd)
				cmd)
			:image ,(ude-make-image img)
			:help ,help))
	  (setq lst (cdr lst))))
       (t
	;; illegal entry
	(error "ude-toolbar-add:illegal entry format `%S'" (car lst)))))
    (let ((omap (current-local-map)))
      (use-local-map map)
      (mapc #'(lambda (n)
		(local-unset-key (vector 'tool-bar n)))
	    handle-names)
      (use-local-map omap))
    (unless (>= 22 bmacs-emacs-version)
      (let ((handle-name (gensym 'handle)))
	(setq handle-names (cons handle-name handle-names))
	(tool-bar-separator map 0)
	(define-key map (vector 'tool-bar handle-name)
	  `(menu-item "handle"
		      (lambda () (interactive) (tool-bar-mode))
		      :image ,(ude-make-image ude-close-toolbar-icon)))))))

;*---------------------------------------------------------------------*/
;*    ude-menubar-set ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-menubar-set (name entries)
  (let* ((id (intern name))
	 (mid (vector 'menu-bar id))
	 (map (current-local-map)))
    (ude-menubar-submenu-set map name mid entries)))

;*---------------------------------------------------------------------*/
;*    ude-menubar-submenu-set ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-menubar-submenu-set (map name menu-id entries)
  (let* ((menu-map (make-sparse-keymap name))
	 (id (intern name)))
    (define-key map menu-id
      `(menu-item ,name ,menu-map))
    (setq entries (reverse entries))
    (while (consp entries)
      (let ((p (car entries)))
	(setq entries (cdr entries))
	(cond
	 ((vectorp p)
	  ;; a plain entry
	  (ude-menubar-vector-entry-set menu-map p))
	 ((consp p)
	  ;; a submenu
	  (let* ((name (car p))
		 (id (intern name))
		 (entries (cdr p)))
	    (ude-menubar-submenu-set menu-map name (vector id) entries)))
	 ((equal p "-")
	  (define-key menu-map [-] '("--")))
	 ((equal p "--:shadowEtchedOut")
	  (define-key menu-map [etchedout]
	    '("--shadow-etched-out")))
	 ((equal p "--:shadowEtchedIn")
	  (define-key menu-map [etchedin]
	    '("--shadow-etched-in")))
	 ((equal p "--:shadowDoubleEtchedIn")
	  (define-key menu-map [double-etched-in]
	    '("--shadow-double-etched-in")))
	 ((equal p "--:shadowDoubleEtchedOut")
	  (define-key menu-map [double-etched-out]
	    '("--shadow-double-etched-out")))
	 (t
	  (error "ude-menubar-set:illegal entry format `%S'" p)))))))

;*---------------------------------------------------------------------*/
;*    ude-menubar-vector-entry-set ...                                 */
;*---------------------------------------------------------------------*/
(defun ude-menubar-vector-entry-set (map entry)
  (let ((id (aref entry 0))
	(cmd (aref entry 1))
	(tst (aref entry 2)))
    (define-key map (vector (intern id))
      (if (consp cmd)
	  `(menu-item ,id (lambda () (interactive) ,cmd)
		      :enable ,tst)
	`(menu-item ,id ,cmd :enable ,tst)))))

(defun ude-menubar-vector-entry-set.old (map entry)
  (let ((id (aref entry 0))
	(cmd (aref entry 1))
	(tst (aref entry 2)))
    (define-key map (vector (intern id))
      `(menu-item ,id ,cmd :enable ,tst))))

;*---------------------------------------------------------------------*/
;*    delete-frame-or-quit ...                                         */
;*---------------------------------------------------------------------*/
(defun delete-frame-or-quit ()
  (interactive)
  (if (eq (next-frame) (selected-frame))
      (save-buffers-kill-emacs)
    (condition-case nil
	(delete-frame (selected-frame))
      (error (save-buffers-kill-emacs)))))

;*---------------------------------------------------------------------*/
;*    delete-window-or-quit ...                                        */
;*---------------------------------------------------------------------*/
(defun delete-window-or-quit ()
  (interactive)
  (if (eq (next-frame) (selected-frame))
      (save-buffers-kill-emacs)
    (condition-case nil
	(delete-window)
      (error (save-buffers-kill-emacs)))))

;*---------------------------------------------------------------------*/
;*    file-installed-p ...                                             */
;*---------------------------------------------------------------------*/
(defun file-installed-p (file &optional paths)
  "Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `load-path' is used."
  (if (null paths) (setq paths load-path))
  (catch 'tag
    (let (path)
      (while paths
	(setq path (expand-file-name file (car paths)))
	(if (file-exists-p path)
	    (throw 'tag path))
	(setq paths (cdr paths))))))

;*---------------------------------------------------------------------*/
;*    region-active-p ...                                              */
;*---------------------------------------------------------------------*/
(defun region-active-p ()
  mark-active)

;*---------------------------------------------------------------------*/
;*    region-exists-p ...                                              */
;*---------------------------------------------------------------------*/
(defun region-exists-p ()
  t)

;*---------------------------------------------------------------------*/
;*    yes-or-no-p-dialog-box ...                                       */
;*---------------------------------------------------------------------*/
(defun yes-or-no-p-dialog-box (arg)
  (let ((use-dialog-box t)
	(last-nonmenu-event nil))
    (yes-or-no-p arg)))

;*---------------------------------------------------------------------*/
;*    face-foreground-name ...                                         */
;*---------------------------------------------------------------------*/
(defun face-foreground-name (face)
  (face-foreground face))

;*---------------------------------------------------------------------*/
;*    face-background-name ...                                         */
;*---------------------------------------------------------------------*/
(defun face-background-name (face)
  (face-background face))

;*---------------------------------------------------------------------*/
;*    face-font-name ...                                               */
;*---------------------------------------------------------------------*/
(defun face-font-name (face)
  (face-font face))

;*---------------------------------------------------------------------*/
;*    frame-toolbar-background ...                                     */
;*---------------------------------------------------------------------*/
(defun frame-toolbar-background (frame)
  (face-background 'tool-bar))

;*---------------------------------------------------------------------*/
;*    frame-getrid-modeline ...                                        */
;*---------------------------------------------------------------------*/
(defun frame-getrid-modeline (frame)
  (setq mode-line-format '()))

;*---------------------------------------------------------------------*/
;*    frame-getrid-toolbar ...                                         */
;*---------------------------------------------------------------------*/
(defun frame-getrid-toolbar (frame)
  (modify-frame-parameters frame '((tool-bar-lines . 0))))

;*---------------------------------------------------------------------*/
;*    frame-getrid-text-cursor ...                                     */
;*---------------------------------------------------------------------*/
(defun frame-getrid-text-cursor (frame)
  nil)

;*---------------------------------------------------------------------*/
;*    frame-getrid-menubar ...                                         */
;*---------------------------------------------------------------------*/
(defun frame-getrid-menubar (frame)
  (modify-frame-parameters frame '((menu-bar-lines . 0))))

;*---------------------------------------------------------------------*/
;*    frame-getrid-scrollbar ...                                       */
;*---------------------------------------------------------------------*/
(defun frame-getrid-scrollbar (frame)
  (modify-frame-parameters frame '((vertical-scroll-bars . nil))))

;*---------------------------------------------------------------------*/
;*    motion-event-p ...                                               */
;*---------------------------------------------------------------------*/
(defun motion-event-p (event)
  nil)

;*---------------------------------------------------------------------*/
;*    buffer-insert-image ...                                          */
;*---------------------------------------------------------------------*/
(defun buffer-insert-image (buffer img)
  (let ((buf (current-buffer)))
    (set-buffer buffer)
    (insert-image (ude-make-image img))
    (set-buffer buf)))

;*---------------------------------------------------------------------*/
;*    x-get-global-resource ...                                        */
;*---------------------------------------------------------------------*/
(defun x-get-global-resource (id1 id2)
  (x-get-resource "backgroundToolBarColor"
		  "BackgroundToolBarColor"
		  "string"
		  "global"))

;*---------------------------------------------------------------------*/
;*    insert-text-property ...                                         */
;*---------------------------------------------------------------------*/
(defun insert-text-property (start end prop value &optional object)
  (if (stringp object)
      (put-text-property start end prop value object)
    (let ((buffer (current-buffer)))
      (if (bufferp object) (set-buffer object))
      (let ((ov (make-overlay start end nil t nil))
	    (mod (buffer-modified-p)))
	(overlay-put ov prop value)
	(set-buffer-modified-p mod)
	(set-buffer buffer)))))

;*---------------------------------------------------------------------*/
;*    put-text-properties ...                                          */
;*---------------------------------------------------------------------*/
(defun put-text-properties (start end &rest props)
  (let ((ov (make-overlay start end nil t nil))
	(mod (buffer-modified-p)))
    (while (consp props)
      (overlay-put ov (car props) (cadr props))
      (setq props (cddr props)))
    (set-buffer-modified-p mod)))

;*---------------------------------------------------------------------*/
;*    remove-text-property ...                                         */
;*---------------------------------------------------------------------*/
(defun remove-text-property (start end prop &optional object)
  (let ((l (overlays-in start end)))
    (while (consp l)
      (let ((o (car l)))
	(setq l (cdr l))
	(if (overlayp o)
	    (if (overlay-get o prop)
		(delete-overlay o)))))))

;*---------------------------------------------------------------------*/
;*    find-text-property ...                                           */
;*---------------------------------------------------------------------*/
(defun find-text-property (position prop &optional object)
  (let ((l (overlays-at position))
	(r nil)
	(buffer (current-buffer)))
    (if (bufferp object)
	(set-buffer buffer))
    (while (and (consp l) (not r))
      (let ((o (car l)))
	(if (overlayp o) (setq r (overlay-get o prop)))
	(setq l (cdr l))))
    (set-buffer buffer)
    r))

;*---------------------------------------------------------------------*/
;*    event-closest-point ...                                          */
;*---------------------------------------------------------------------*/
(defun event-closest-point (event)
  (car (cdr (event-start event))))

;*---------------------------------------------------------------------*/
;*    event-window ...                                                 */
;*---------------------------------------------------------------------*/
(defun event-window (event)
  (car (event-start event)))

;*---------------------------------------------------------------------*/
;*    event-buffer ...                                                 */
;*---------------------------------------------------------------------*/
(defun event-buffer (event)
  (window-buffer (car (event-start event))))

;*---------------------------------------------------------------------*/
;*    set-default-toolbar-visible ...                                  */
;*---------------------------------------------------------------------*/
(defun set-default-toolbar-visible (b)
  (if (not b)
      (modify-frame-parameters (selected-frame) '((tool-bar-lines . 0)))
    (modify-frame-parameters (selected-frame) '((tool-bar-lines . 1)))))

;*---------------------------------------------------------------------*/
;*    ude-set-nontext-pointer ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-set-nontext-pointer (ptr)
  nil)

;*---------------------------------------------------------------------*/
;*    redisplay-frame ...                                              */
;*---------------------------------------------------------------------*/
(defun redisplay-frame (&rest _)
  (sit-for 0))

;*---------------------------------------------------------------------*/
;*    ude-default-frame-alist ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-default-frame-alist (key val)
  (cons (cons key val) default-frame-alist))

;*---------------------------------------------------------------------*/
;*    compile-error-at-point ...                                       */
;*---------------------------------------------------------------------*/
(if (not (fboundp 'compile-error-at-point))
    (defun compile-error-at-point ()
      (compile-goto-error)))
