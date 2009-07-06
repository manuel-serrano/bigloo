;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-toolbar.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  5 21:03:27 1998                          */
;*    Last change :  Fri Nov 17 15:31:28 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Ude C toolbar system.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-toolbar)
(require 'lee-autoload)
(require 'ude-icon)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-autoload)
(require 'dbg-autoload)

;*---------------------------------------------------------------------*/
;*    lee-repl-buffer ...                                              */
;*---------------------------------------------------------------------*/
(defvar lee-repl-buffer nil)

;*---------------------------------------------------------------------*/
;*    lee-repl-other-frame ...                                         */
;*---------------------------------------------------------------------*/
(defun lee-repl-other-frame ()
  (interactive)
  (if (bufferp lee-repl-buffer)
      (let ((pop-up-frames t))
	(display-buffer lee-repl-buffer))
    (progn
      (switch-to-buffer-other-frame "*elisp-repl*")
      (lisp-interaction-mode))))

;*---------------------------------------------------------------------*/
;*    c opened toolbar ...                                             */
;*---------------------------------------------------------------------*/
(defvar lee-toolbar
  `(;; the tag button
    (,ude-tag-icon lee-tags-find "Find definition")
    --

    ;; the compile button
    (,ude-compile-icon ude-mode-compile-from-menu "Compile")
    ;; the root button
    (,ude-root-icon ude-user-set-root-directory "Set new Lisp root directory")
    ;; the make ude button
    (,ude-make-make-icon ude-make-ude "Generate/update Ude Makefile entries")
    --

    ;; the repl button
    (,ude-repl-icon lee-repl-other-frame "Start a read-eval-print loop")
    --

    -->
    ;; the help button
    --
    (,ude-help-icon describe-function-at-point "Describe function at point")
    ;; the info button
    (,ude-info-icon lee-docline "The online documentation for Lisp mode")))

;*---------------------------------------------------------------------*/
;*    lee-toolbar-init ...                                             */
;*---------------------------------------------------------------------*/
(defun lee-toolbar-init ()
  (ude-toolbar-set lee-toolbar))



