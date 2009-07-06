;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-hook.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 08:49:24 1998                          */
;*    Last change :  Sat Mar  2 12:02:04 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The CEE hook that sets up all the CEE configuration.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-hook)
(require 'font-lock)
(require 'cee-indent)
(require 'cee-keymap)
(require 'cee-toolbar)
(require 'cee-flock)
(require 'ude-autoload)
(require 'ude-config)
(require 'ude-custom)
(require 'dbg-autoload)

;*---------------------------------------------------------------------*/
;*    cee-docline-preferred-format ...                                 */
;*---------------------------------------------------------------------*/
(defun cee-docline-preferred-format ()
  (let* ((cell (assq major-mode cee-docline-preferred-format))
	 (cell (if (consp cell)
		   cell
		 (assq t cee-docline-preferred-format))))
    (if (consp cell)
	(cdr cell)
      'info)))
  
;*---------------------------------------------------------------------*/
;*    cee-docline ...                                                  */
;*---------------------------------------------------------------------*/
(defun cee-docline ()
  (interactive)
  (if (not (eq (cee-docline-preferred-format) 'html))
      (cee-docline-info)
    (let ((cell (assq major-mode cee-docline-files)))
      (if (and (consp cell) (stringp ude-url-browser))
	  (ude-system ude-url-browser (cdr cell))
	(cee-docline-info)))))

;*---------------------------------------------------------------------*/
;*    cee-docline-info ...                                             */
;*---------------------------------------------------------------------*/
(defun cee-docline-info ()
  (setq ude-info-file-list cee-info-file-list)
  (ude-info-docline (cee-font-lock-get-info-keywords)))

;*---------------------------------------------------------------------*/
;*    cee-docline-ident ...                                            */
;*---------------------------------------------------------------------*/
(defun cee-docline-ident (ident)
  (interactive (ude-interactive-ident (point) "Identifier: "))
  (if (eq (cee-docline-preferred-format) 'html)
      (cee-docline)
    (cee-docline-ident-info ident)))

;*---------------------------------------------------------------------*/
;*    cee-ident ...                                                    */
;*---------------------------------------------------------------------*/
(defun cee-ident (ident)
  (interactive (ude-interactive-ident (point) "Identifier: "))
  ident)

;*---------------------------------------------------------------------*/
;*    cee-docline-ident-info ...                                       */
;*---------------------------------------------------------------------*/
(defun cee-docline-ident-info (ident)
  (setq ude-info-file-list cee-info-file-list)
  (ude-info-docline-ident (cee-font-lock-get-info-keywords) ident))

;*---------------------------------------------------------------------*/
;*    cee-docline-init ...                                             */
;*    -------------------------------------------------------------    */
;*    Initialize the docline system, that is, tell Ude to use          */
;*    info with the Bigloo page.                                       */
;*---------------------------------------------------------------------*/
(defun cee-docline-init ()
  (setq ude-docline (function ude-info-docline)))
  
;*---------------------------------------------------------------------*/
;*    cee-hook ...                                                     */
;*---------------------------------------------------------------------*/
(defun cee-hook ()
  ;; we setup the project root directory
  (ude-auto-set-root-directory)
  ;; docline initialization
  (cee-docline-init)
  ;; the key binding
  (cee-keymap-init)
  ;; parenthesis blinking init
  (ude-paren-init)
  ;; starting font-lock
  (if ude-font-lock-p
      (font-lock-mode t))
  ;; the toolbar
  (cee-toolbar-init)
  ;; balloon
  (if cee-tags-balloon-p
      (ude-tags-balloon-start 'cee-ident
			      nil
			      'bee-tags-find
			      #'(lambda (point ident)
				  (popup-menu
				   (cons ident
					 (cdr (car (cee-find-menu))))))))
  ;; the cee hook
  (run-hooks 'cee-hook))
 
