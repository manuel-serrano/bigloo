;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/dbg/dbg-autoload.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug  1 10:27:42 1998                          */
;*    Last change :  Wed Jan 23 17:36:01 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    All the autoload declarations for bdb.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'dbg-autoload)

;*---------------------------------------------------------------------*/
;*    Autoload declarations ...                                        */
;*---------------------------------------------------------------------*/
;; dbg
(autoload 'dbg-other-frame "dbg" "The Debugging emacs package." t)
(autoload 'dbg-installed-hook-p "dbg" "The Debugging emacs package." t)
(autoload 'dbg-remote-call "dbg" "The Debugging emacs package." t)
(autoload 'dbg-show "dbg" "The Debugging emacs package." t)
(autoload 'dbg-comint-ready-p "dbg" "The Debugging emacs package." t)
(autoload 'dbg-comint-started-p "dbg" "The Debugging emacs package." t)
;; dbg mode
(autoload 'dbg-verbose-remove-call "The Debugging emacs mode." t)
(autoload 'dbg-silent-remove-call "The Debugging emacs mode." t)
;; dbg-connect
(autoload 'dbg-toggle-connect-buffer "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-disconnect-all-buffers "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-connect-buffer "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-connect-file "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-disconnect-buffer "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-add-connect-hook "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-add-disconnect-hook "dbg-connect" "The Bdb Emacs package." t)
(autoload 'dbg-connected-buffer-p "dbg-connect" "The Bdb Emacs package." t)
;; bee-bmake
(autoload 'bee-makefile-fetch-binary-entry "bee-bmake" "Makefile handling." t)
;; dbg-breakpoint
(autoload 'dbg-breakpoint-menu "dbg-breakpoint" "Breakpoint handling." t)
(autoload 'dbg-breakpoint-update-menu "dbg-breakpoint" "Bp handling." t)
(autoload 'dbg-breakpoint-balloon-message "dbg-breakpoint" "Bp handling." t)
(autoload 'dbg-breakpoint-command "dbg-breakpoint" "Bp handling." t)
;; dbg-source
(autoload 'dbg-set-buffer-margin "dbg-source" "Source handling." t)
(autoload 'dbg-unset-buffer-margin "dbg-source" "Source handling." t)
(autoload 'dbg-get-buffer-margin "dbg-source" "Source handling." t)
(autoload 'dbg-command-region "dbg-source" "Source handling." t)
(autoload 'dbg-undisplay-source-line "dbg-source" "Source handling." t)
(autoload 'dbg-highlight-source-line "dbg-source" "Source handling." t)
;; dbg-stack
(autoload 'dbg-stack-start "dbg-stack" "Runtime stack display." t)
(autoload 'dbg-stack-quit "dbg-stack" "Runtime stack display." t)
(autoload 'dbg-stack-hook "dbg-stack" "Runtime stack display." t)
(autoload 'dbg-stack-toggle "dbg-stack" "Runtime locals display." t)
;; dbg-locals
(autoload 'dbg-locals-start "dbg-locals" "Runtime locals display." t)
(autoload 'dbg-locals-quit "dbg-locals" "Runtime locals display." t)
(autoload 'dbg-locals-toggle "dbg-locals" "Runtime locals display." t)
;; dbg-args
(autoload 'dbg-args-start "dbg-args" "Runtime args display." t)
(autoload 'dbg-args-quit "dbg-args" "Runtime args display." t)
(autoload 'dbg-args-toggle "dbg-args" "Runtime args display." t)
;; dbg-display
(autoload 'dbg-display-start "dbg-display" "Runtime display display." t)
(autoload 'dbg-display-quit "dbg-display" "Runtime display display." t)
(autoload 'dbg-display-toggle "dbg-display" "Runtime display display." t)
;; dbg-console
(autoload 'dbg-pop-console "dbg-console" "Bdb console login." t)
(autoload 'dbg-console-log "dbg-console" "Bdb console login." t)
(autoload 'dbg-erase-console "dbg-console" "Bdb console login." t)
(autoload 'dbg-recenter-console "dbg-console" "Bdb console login." t)
;; dbg-docline
(autoload 'dbg-docline "dbg-docline" "Bdb online documentation." t)
