;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-toolbar.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  5 21:03:27 1998                          */
;*    Last change :  Tue Jul 29 11:37:07 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Ude C toolbar system.                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-toolbar)
(require 'cee-autoload)
(require 'ude-icon)
(require 'ude-autoload)
(require 'dbg-autoload)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    c toolbar ...                                                    */
;*---------------------------------------------------------------------*/
(defvar cee-toolbar
  `(;; the tag button
    (,ude-edit-icon cee-tags-find "Find definition")

    --
    ;; the indent button
    (,ude-indent-icon cee-external-indent "Indent whole buffer")
    --

    ;; the compile button
    (,ude-compile-icon ude-mode-compile-from-menu "Compile")
    ;; the root button
    (,ude-root-icon ude-user-set-root-directory "Set new C root directory")
    ;; the make ude button
    (,ude-make-make-icon ude-make-ude "Generate/update Ude Makefile entries")
    --

    ;; the profiler
    (,ude-profile-icon cee-profiler-start "Profiling")
    --

    ;; the gdb button
    (,ude-dbg-icon cee-kbdb-start "Debugging")
    ;; the gdb connect button
    (,ude-dbg-connect-icon dbg-toggle-connect-buffer "Debugger connection")
    --

    -->
    ;; the help button
    --
    (,ude-help-icon describe-mode "Cc mode help")
    ;; the info button
    (,ude-info-icon cee-docline "The online documentation")))

;*---------------------------------------------------------------------*/
;*    cee-toolbar-init ...                                             */
;*---------------------------------------------------------------------*/
(defun cee-toolbar-init ()
  (ude-toolbar-set cee-toolbar))


