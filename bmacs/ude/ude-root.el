;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-root.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  8 07:27:43 1998                          */
;*    Last change :  Tue Oct 28 10:57:19 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `Unix Development Environment' root settings.                */
;*    -------------------------------------------------------------    */
;*    The root directory is the closest directory that contains a      */
;*    Makefile that contains the name of the file.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-root)
(require 'ude-custom)
(require 'ude-autoload)
(require 'ude-config)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-root-directory-set ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-root-directory-set (path)
  (run-hook-with-args 'ude-root-directory-hook path)
  (setq ude-root-directory path))

;*---------------------------------------------------------------------*/
;*    ude-auto-find-root-directory ...                                 */
;*    -------------------------------------------------------------    */
;*    This function search the root directory for the file named       */
;*    FNAME. It returns NIL if no root directory is found.             */
;*    -------------------------------------------------------------    */
;*    To seek the root directory, we search for a Makefile that        */
;*    contains the prefix of FNAME.                                    */
;*    -------------------------------------------------------------    */
;*    The directory returned by this function _contains_ an ending     */
;*    / character.                                                     */
;*---------------------------------------------------------------------*/
(defun ude-auto-find-root-directory (fname &optional rfname)
  "Automatically find a root directory for file FNAME."
  (if (not (stringp fname))
    nil
    (let* ((fname  (expand-file-name fname))
	   (dirold nil)
	   (dir    (file-name-directory fname))
	   (file   (file-name-nondirectory fname))
	   (prefix (ude-string-prefix file))
	   (depth  ude-root-search-depth)
	   (res    nil))
      (while (and (> depth 0)
		  (> (length dir) 0)
		  (not (string= dir "//"))
		  (not (equal dirold dir))
		  (not res))
	(let ((makefile (concat dir ude-makefile))
	      (rootfile (if (stringp rfname)
			    (concat dir rfname)
			  '())))
	  (setq dirold dir)
	  (cond
	   ((and (stringp rootfile) (file-exists-p rootfile))
	    (let* ((s (concat (file-name-directory rootfile)
			      (shell-command-to-string (concat "cat " rootfile))))
		   (l (length s)))
	      (if (and (> l 0) (eq (aref s (- l 1)) ?\n))
		  (setq res (substring s 0 (- l 1)))
		(setq res s))
	      (let ((l (length res)))
		(if (and (> l 0) (not (eq (aref res (- l 1)) ?/)))
		    (setq res (concat res "/"))))
	      res))
	   ((file-exists-p makefile)
	    (let* ((cmd  (concat ude-grep-w " '"
				 (regexp-quote prefix)
				 "[.]?' " makefile))
		   (grep (shell-command-to-string cmd)))
	      (if (string-match prefix grep)
		  (setq res dir)
		(let ((dirname (substring dir 0 (- (length dir)))))
		  (setq depth (+ 1 depth))
		  (setq dir (file-name-directory dirname))))))
	   (t
	    (let ((dirname (substring dir 0 (- (length dir) 1))))
	      (setq depth (+ 1 depth))
	      (setq dir (file-name-directory dirname)))))))
      res)))

;*---------------------------------------------------------------------*/
;*    ude-auto-set-root-directory ...                                  */
;*    -------------------------------------------------------------    */
;*    This sets the ude-root-directory each time Ude mode is entered.  */
;*---------------------------------------------------------------------*/
(defun ude-auto-set-root-directory (&optional file)
  (let ((d (ude-auto-find-root-directory (buffer-file-name (current-buffer))
					 file)))
    (if (stringp d)
	(ude-root-directory-set d)
      (ude-root-directory-set (ude-root-trailing-slash default-directory))))
  (ude-set-root-modeline))

;*---------------------------------------------------------------------*/
;*    ude-user-set-root-directory ...                                  */
;*    -------------------------------------------------------------    */
;*    This sets the ude-root-directory with the user choice.           */
;*---------------------------------------------------------------------*/
(defun ude-user-set-root-directory (dir)
  (interactive "DRoot directory: ")
  (if (and (stringp dir) (file-exists-p dir))
      (progn
	(ude-root-directory-set
	 (ude-root-trailing-slash (expand-file-name dir)))
	(ude-set-root-modeline))
    (ude-error "Can't find root directory %S" dir)))

;*---------------------------------------------------------------------*/
;*    ude-root-trailing-slash ...                                      */
;*    -------------------------------------------------------------    */
;*    Add a / at the end of the root path if that character is         */
;*    not present yet.                                                 */
;*---------------------------------------------------------------------*/
(defun ude-root-trailing-slash (dir)
  (if (not (eq (aref dir (- (length dir) 1)) ?/))
      (concat dir "/")
    dir))

;*---------------------------------------------------------------------*/
;*    ude-modeline-id ...                                              */
;*---------------------------------------------------------------------*/
(defvar ude-modeline-id nil)
(make-variable-buffer-local 'ude-modeline-id)

;*---------------------------------------------------------------------*/
;*    ude-root-modeline-text ...                                       */
;*---------------------------------------------------------------------*/
(defun ude-root-modeline-text ()
  (if (stringp ude-root-directory)
      (file-name-nondirectory (substring ude-root-directory
					 0
					 (- (length ude-root-directory) 1)))
    "no root"))
