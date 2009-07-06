;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-toolbar.el             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:21:36 2002                          */
;*    Last change :  Thu May 23 14:06:12 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The debugger toolbar                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-toolbar)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-icon)
(require 'bee-autoload)
(require 'bug-config)
(require 'bug-autoload)

;*---------------------------------------------------------------------*/
;*    bug toolbars ...                                                 */
;*---------------------------------------------------------------------*/
(defvar bug-toolbar
  `(;; the find buttom
    (,ude-quit-icon bugloo-quit "Quit debugger")
    ;; the usage button
    (,ude-footprint-icon bee-usage-find "Usage")
    --
    
    ;; the open button
    (,ude-open-icon dbg-open-file "Open binary file")
    ;; the file button
    (,ude-dbg-file-icon (bug-remote-call "file") "Reload file")
    --
    
    ;; the stop button
    (,ude-stop-icon comint-interrupt-subjob "Stop execution")
    --
    
    ;; the run button
    (,ude-dbg-run-icon (bug-remote-call (bug-run-command)) "Run")
    ;; the step button
    (,ude-dbg-step-icon (bug-remote-call (bug-step-command)) "Step")
    ;; the continue button
    (,ude-dbg-continue-icon (bug-remote-call (bug-cont-command)) "Continue")
    ;; the next button
    (,ude-dbg-next-icon (bug-remote-call (bug-next-command)) "Next")
    ;; the until button
    (,ude-dbg-until-icon (bug-remote-call (bug-until-command)) "until")
    ;; the finish button
    (,ude-dbg-finish-icon (bug-remote-call (bug-finish-command)) "Finish")
    --

    ;; the show button
    (,ude-dbg-show-icon bugloo-show "Show values")
    --
    
    ;; the dbg connect button
    (,ude-dbg-connect-icon bug-connect-buffer "Connect buffer")
    --

    -->
    ;; the info button
    (,ude-info-icon bee-doc-visit-bigloo "Online documentation")))
 
;*---------------------------------------------------------------------*/
;*    bug-toolbar-init ...                                             */
;*---------------------------------------------------------------------*/
(defun bug-toolbar-init ()
  (ude-toolbar-set bug-toolbar))
