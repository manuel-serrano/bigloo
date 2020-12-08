;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-toolbar.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  5 21:03:27 1998                          */
;*    Last change :  Fri Nov 17 15:30:10 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bee toolbar system.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-toolbar)
(require 'bee-autoload)
(require 'ude-icon)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-autoload)
(require 'bug-autoload)

;*---------------------------------------------------------------------*/
;*    bee opened toolbar ...                                           */
;*---------------------------------------------------------------------*/
(defvar bee-toolbar
  `(;; begoo
    ,@(if nil ;; (bee-begoo-ready-p)
	  `((,ude-graph-icon bee-begoo-start "Module graph") --)
	'())

      ;; either the usage button or the browser button
      ;; either the find or the ibuilder button
      ,@(if (bee-interface-builder-ready-p)
	    ;; the ibuilder button
	    `((,ude-ibuilder-icon bee-interface-builder-start "Interface builder"))
	  ;; the find button
	  `((,ude-edit-icon bee-tags-find "Find definition")))

      ;; browser when available
      ,@(if (bee-browser-ready-p)
	    ;; the browser button
	    `((,ude-browse-icon bee-browser-start "Browser"))
	  ;; the doc buttom
	  `((,ude-footprint-icon bee-usage-find "Definition documentation")))
      ;; the indent button
      (,ude-indent-icon bee-external-indent "Indent whole buffer")
      --
      ;; the import button
      (,ude-import-icon bee-import-binding "Import definition")
      ;; the export button
      (,ude-export-icon bee-export-definition "Export definition")
      ;; the C importation button
      (,ude-c-import-icon bee-import-c-file "Import C definition")
      --

      ;; the compile button
      (,ude-compile-icon ude-mode-compile-from-menu "Compile")
      ;; the root button
      (,ude-root-icon ude-user-set-root-directory "Set new Bee root directory")
      ;; the bmake button
      (,ude-make-make-icon bee-generate/update-makefile "Generate/update Makefile")
      ;; the bmake entry button
      (,ude-make-entry-icon ude-add-user-makefile-entry "Add Makefile entry")
      --

      ;; the repl button
      (,ude-repl-icon ude-repl-other-frame "Start a read-eval-print loop")
      --
      ;; the profile button
      (,ude-profile-icon bee-profiler-start "Profiling")
      ;; the bdb button
      (,ude-dbg-icon bee-debug "Debugging")
      ;; the gdb connect button
      (,ude-dbg-connect-icon bug-toggle-connect-buffer "Debugger connection")
      -->

      --
      ;; online documentation
      (,ude-info-icon bee-doc-visit-bigloo "The online documentation for Bee")))

;*---------------------------------------------------------------------*/
;*    bee-toolbar-init ...                                             */
;*---------------------------------------------------------------------*/
(defun bee-toolbar-init ()
  (ude-toolbar-set bee-toolbar))


