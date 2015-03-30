;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bmacs-xemacs.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan 20 11:51:19 2002                          */
;*    Last change :  Thu Oct  6 07:01:54 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The XEmacs specific file.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bmacs-xemacs)
(require 'xemacs-etags)

;*---------------------------------------------------------------------*/
;*    bmacs-etags ...                                                  */
;*---------------------------------------------------------------------*/
(defvar bmacs-etags 'xemacs-etags)

;*---------------------------------------------------------------------*/
;*    bmacs-emacs-version ...                                          */
;*---------------------------------------------------------------------*/
(defvar bmacs-emacs-version -21)

;*---------------------------------------------------------------------*/
;*    ude-set-root-modeline ...                                        */
;*    -------------------------------------------------------------    */
;*    This function sets the modeline according to the root directory. */
;*---------------------------------------------------------------------*/
(defun ude-set-root-modeline (&optional txt)
  (let* ((text (if txt txt (ude-root-modeline-text)))
	 (id (if (stringp text)
		 (let ((glyph (make-glyph text)))
		   (set-glyph-face glyph 'ude-modeline-root-face)
		   glyph)
	       (let ((glyph (make-glyph text)))
		 (set-glyph-face glyph 'ude-modeline-no-root-face)
		 glyph))))
    (if (consp ude-modeline-id)
	(rplacd ude-modeline-id id)
      (progn
	(setq ude-modeline-id
	      (cons (copy-extent modeline-buffer-id-left-extent) id))
	(setq mode-line-buffer-identification
	      (cons ude-modeline-id
		    (mapcar #'(lambda (prop)
			       (let ((name (cdr prop)))
				 (if (string= "XEmacs%N:" name)
				     (cons (car prop) "%N:")
				   prop)))
			    mode-line-buffer-identification)))))))

;*---------------------------------------------------------------------*/
;*    ude-make-image ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-make-image (descr &optional ascent)
  (if (ude-string-xpm-p descr)
      descr
    (file-installed-p descr load-path)))
      
;*---------------------------------------------------------------------*/
;*    ude-make-glyph ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-make-glyph (descr &optional ascent)
  (make-glyph descr))
      
;*---------------------------------------------------------------------*/
;*    ude-toolbar-set ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-toolbar-set (lst)
  ;; define a locate map for the tool-bar
  (let ((tb '()))
    ;; invert the list
    (setq lst (reverse lst))
    ;; add each entry to the menu
    (while (consp lst)
      (cond
       ((eq (car lst) '--)
	;; separator
	(setq lst (cdr lst))
	(setq tb (cons [:style 2d :size 1] tb)))
       ((eq (car lst) '-->)
	;; flush right
	(setq lst (cdr lst))
	(setq tb (cons nil tb)))
       ((= (length (car lst)) 3)
	;; plain entry
	(let* ((l (car lst))
	       (img (car l))
	       (cmd (car (cdr l)))
	       (help (car (cdr (cdr l)))))
	  (setq tb (cons (vector (toolbar-make-button-list
				  (ude-make-image img))
				 cmd t help)
			 tb))
	  (setq lst (cdr lst))))
       (t
	;; illegal entry
	(error "tool-bar-set:illegal entry format `%S'" (car lst)))))
    (if (not (eq (console-type) 'mswindows))
	(progn
	  (setq tb (cons [:style 2d :size 1] tb))
	  (setq tb (cons (vector (toolbar-make-button-list
				  (ude-make-image ude-close-toolbar-icon))
				 (lambda () (interactive) t)
				 t
				 "Close toolbar")
			 tb))))
    ;; make the menu local not to change the other buffers
    (set-specifier default-toolbar-visible-p t (current-buffer))
    (set-specifier default-toolbar tb (current-buffer))))

;*---------------------------------------------------------------------*/
;*    ude-toolbar-add ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-toolbar-add (lst)
  ;; define a locate map for the tool-bar
  (let ((tb '()))
    ;; invert the list
    (setq lst (reverse lst))
    ;; add each entry to the menu
    (while (consp lst)
      (cond
       ((eq (car lst) '--)
	;; separator
	(setq lst (cdr lst))
	(setq tb (cons [:style 2d :size 1] tb)))
       ((eq (car lst) '-->)
	;; flush right
	(setq lst (cdr lst))
	(setq tb (cons nil tb)))
       ((= (length (car lst)) 3)
	;; plain entry
	(let* ((l (car lst))
	       (img (car l))
	       (cmd (car (cdr l)))
	       (help (car (cdr (cdr l)))))
	  (setq tb (cons (vector (toolbar-make-button-list
				  (ude-make-image img))
				 cmd t help)
			 tb))
	  (setq lst (cdr lst))))
       (t
	;; illegal entry
	(error "tool-bar-set:illegal entry format `%S'" (car lst)))))))

;*---------------------------------------------------------------------*/
;*    ude-menubar-set ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-menubar-set (name entries)
  (if (featurep 'menubar)
      (progn
	(set-buffer-menubar current-menubar)
	(add-submenu nil (cons name entries)))))
 
;*---------------------------------------------------------------------*/
;*    delete-frame-or-quit ...                                         */
;*---------------------------------------------------------------------*/
(defun delete-frame-or-quit ()
  (interactive)
  (let ((lst (device-frame-list)))
    (if (and (consp lst) (null (cdr lst)))
	(kill-emacs)
      (delete-frame))))

;*---------------------------------------------------------------------*/
;*    delete-window-or-quit ...                                        */
;*---------------------------------------------------------------------*/
(defun delete-window-or-quit ()
  (interactive)
  (let ((lst (device-frame-list)))
    (if (and (consp lst) (null (cdr lst)))
	(kill-emacs)
      (delete-window))))

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
;*    frame-parameter ...                                              */
;*---------------------------------------------------------------------*/
(defun frame-parameter (frame key)
  (let* ((p (frame-parameters frame))
	 (c (assq key p)))
    (if (consp c) (cdr c) nil)))
    
;*---------------------------------------------------------------------*/
;*    frame-toolbar-background ...                                     */
;*---------------------------------------------------------------------*/
(defun frame-toolbar-background (frame)
  (frame-property frame 'background-toolbar-color))

;*---------------------------------------------------------------------*/
;*    frame-getrid-modeline ...                                        */
;*---------------------------------------------------------------------*/
(defun frame-getrid-modeline (frame)
  (set-specifier modeline-shadow-thickness (cons frame 1))
  (set-specifier has-modeline-p (cons frame nil)))

;*---------------------------------------------------------------------*/
;*    frame-getrid-toolbar ...                                         */
;*---------------------------------------------------------------------*/
(defun frame-getrid-toolbar (frame)
  (set-specifier top-toolbar-height (cons frame 0))
  (set-specifier left-toolbar-width (cons frame 0))
  (set-specifier right-toolbar-width (cons frame 0))
  (set-specifier bottom-toolbar-height (cons frame 0))
  (set-specifier top-toolbar (cons frame nil))
  (set-specifier left-toolbar (cons frame nil))
  (set-specifier right-toolbar (cons frame nil))
  (set-specifier bottom-toolbar (cons frame nil))
  (set-specifier scrollbar-width (cons frame 0))
  (set-specifier scrollbar-height (cons frame 0)))

;*---------------------------------------------------------------------*/
;*    frame-getrid-text-cursor ...                                     */
;*---------------------------------------------------------------------*/
(defun frame-getrid-text-cursor (frame)
  (let ((f (selected-frame)))
    (select-frame frame)
    (set-specifier text-cursor-visible-p (cons frame nil))
    (select-frame f)))

;*---------------------------------------------------------------------*/
;*    frame-getrid-menubar ...                                         */
;*---------------------------------------------------------------------*/
(defun frame-getrid-menubar (frame)
  (let ((f (selected-frame)))
    (select-frame frame)
    (set-buffer-menubar nil)
    (select-frame f)))

;*---------------------------------------------------------------------*/
;*    frame-getrid-scrollbar ...                                       */
;*---------------------------------------------------------------------*/
(defun frame-getrid-scrollbar (frame)
  nil)

;*---------------------------------------------------------------------*/
;*    buffer-insert-image ...                                          */
;*---------------------------------------------------------------------*/
(defun buffer-insert-image (buffer img)
  (set-extent-begin-glyph (make-extent (point) (point)) (make-glyph xpm)))

;*---------------------------------------------------------------------*/
;*    x-get-global-resource ...                                        */
;*---------------------------------------------------------------------*/
(defun x-get-global-resource (id1 id2)
  (x-get-resource "backgroundToolBarColor"
		  "BackgroundToolBarColor"
		  'string
		  'global))

;*---------------------------------------------------------------------*/
;*    insert-text-property ...                                         */
;*---------------------------------------------------------------------*/
(defun insert-text-property (start end prop value &optional object)
  (put-text-property start end prop value (if object object (current-buffer))))

;*---------------------------------------------------------------------*/
;*    put-text-properties ...                                          */
;*---------------------------------------------------------------------*/
(defun put-text-properties (start end &rest props)
  (let ((extent (make-extent start end)))
    (while (consp props)
      (cond
       ((eq (car props) 'face)
	(set-extent-face extent (cadr props)))
       ((eq (car props) 'local-map)
	(set-extent-property extent 'keymap (cadr props)))
       (t
	(set-extent-property extent (car props) (cadr props))))
      (setq props (cddr props)))))
  
;*---------------------------------------------------------------------*/
;*    find-text-property ...                                           */
;*---------------------------------------------------------------------*/
(defun find-text-property (position prop &optional object)
  (let ((extent (extent-at position (if object object (current-buffer)) prop)))
    (if (extentp extent)
	(extent-property extent prop)
      nil)))
  
;*---------------------------------------------------------------------*/
;*    remove-text-property ...                                         */
;*---------------------------------------------------------------------*/
(defun remove-text-property (start end prop &optional object)
  (let* ((buffer (if object object (current-buffer)))
	 (extent (extent-at start buffer prop)))
    (if (extentp extent)
	(delete-extent extent))))

;*---------------------------------------------------------------------*/
;*    set-default-toolbar-visible ...                                  */
;*---------------------------------------------------------------------*/
(defun set-default-toolbar-visible (b)
  (set-specifier default-toolbar-visible-p b (selected-frame)))

;*---------------------------------------------------------------------*/
;*    ude-set-nontext-pointer ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-set-nontext-pointer (ptr)
  (set-glyph-image nontext-pointer-glyph ptr))

;*---------------------------------------------------------------------*/
;*    ude-default-frame-alist ...                                      */
;*---------------------------------------------------------------------*/
(defun ude-default-frame-alist (key val)
  (cons key (cons val default-frame-alist)))

;*---------------------------------------------------------------------*/
;*    gdb ...                                                          */
;*---------------------------------------------------------------------*/
(if (not (fboundp 'gdb))
    (defun gdb (cmd)
      (tgdb cmd)))
