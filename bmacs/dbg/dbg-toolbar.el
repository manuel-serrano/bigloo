;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-toolbar.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun  5 21:03:27 1998                          */
;*    Last change :  Thu Feb  7 09:13:18 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The dbg toolbar system.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-toolbar)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'dbg-config)
(require 'ude-icon)
(require 'ude-autoload)
(require 'comint)

;*---------------------------------------------------------------------*/
;*    dbg-tags-find ...                                                */
;*---------------------------------------------------------------------*/
(defun dbg-tags-find ()
  (interactive)
  (call-interactively dbg-mode-tags-find))

;*---------------------------------------------------------------------*/
;*    dbg-usage-find ...                                               */
;*---------------------------------------------------------------------*/
(defun dbg-usage-find ()
  (interactive)
  (call-interactively dbg-mode-usage-find))

;*---------------------------------------------------------------------*/
;*    dbg-open-file ...                                                */
;*---------------------------------------------------------------------*/
(defun dbg-open-file (file)
  (interactive "fbinary file: ")
  (dbg-remote-call (concat "file " file)))

;*---------------------------------------------------------------------*/
;*    dbg toolbars ...                                                 */
;*---------------------------------------------------------------------*/
(defvar dbg-toolbar
  `(;; the find buttom
    (,ude-tag-icon dbg-tags-find "Find definition")
    ;; the doc buttom
    (,ude-declaration-icon dbg-usage-find "Definition documentation")
    --
    
    ;; the open button
    (,ude-open-icon dbg-open-file "Open binary file")
    ;; the file button
    (,ude-dbg-file-icon (dbg-remote-call "file") "Reload file")
    --
    
    ;; the stop button
    (,ude-stop-icon comint-interrupt-subjob "Stop execution")
    --
    
    ;; the run button
    (,ude-dbg-run-icon (dbg-remote-call dbg-run-command) "Run")
    ;; the step button
    (,ude-dbg-step-icon (dbg-remote-call dbg-step-command) "Step")
    ;; the continue button
    (,ude-dbg-continue-icon (dbg-remote-call dbg-cont-command) "Continue")
    ;; the next button
    (,ude-dbg-next-icon (dbg-remote-call dbg-next-command) "Next")
    ;; the until button
    (,ude-dbg-until-icon (dbg-remote-call dbg-until-command) "until")
    ;; the finish button
    (,ude-dbg-finish-icon (dbg-remote-call dbg-finish-command) "Finish")
    --

    ;; the show button
    (,ude-dbg-show-icon dbg-show "Show value")
    --
    
    ;; the dbg connect button
    (,ude-dbg-connect-icon dbg-connect-buffer "Dbg connect buffer")
    --

    -->
    ;; the info button
    (,ude-info-icon dbg-docline "The online documentation for Dbg")))
 
;*---------------------------------------------------------------------*/
;*    dbg-toolbar-init ...                                             */
;*---------------------------------------------------------------------*/
(defun dbg-toolbar-init ()
  (ude-toolbar-set dbg-toolbar))

