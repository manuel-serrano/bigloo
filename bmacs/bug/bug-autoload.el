;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-autoload.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 29 11:14:10 2002                          */
;*    Last change :  Thu May 23 15:54:15 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The debugger autoload package                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package ...                                                  */
;*---------------------------------------------------------------------*/
(provide 'bug-autoload)

;; bugloo
(autoload 'bugloo "bug" "Bugloo Emacs Lisp." t)
(autoload 'bugloo-other-window "bug" "Bugloo Emacs Lisp." t)
(autoload 'bugloo-other-frame "bug" "Bugloo Emacs Lisp mode." t)
(autoload 'bugloo-quit "bug" "Bugloo Emacs Lisp mode." t)
(autoload 'bugloo-show "bug" "Bugloo Emacs Lisp mode." t)
(autoload 'bug-delete-window-or-frame "bug" "Bugloo Emacs Lisp mode." t)

;; bug-mode
(autoload 'bug-mode "bug-mode" "Bug Emacs Lisp mode." t)

;; bug-process
(autoload 'bug-process-call "bug-process" "Bug process manager." t)
(autoload 'bug-wait-process "bug-process" "Bug process manager." t)
(autoload 'bug-send-input "bug-process" "Bug process manager." t)
(autoload 'bug-remote-call "bug-process" "Bug process manager." t)
(autoload 'bug-silent-remote-call "bug-process" "Bug process manager." t)

;; bug-filter
(autoload 'bug-filter "bug-filter" "Bugloo output filter." t)
(autoload 'bug-filter-command "bug-filter" "Bugloo output filter." t)

;; bug-breakpoint
(autoload 'bug-breakpoint "bug-breakpoint" "Breakpoint manager." t)
(autoload 'bug-breakpoint-popup-menu "bug-breakpoint" "Breakpoint manager." t)
(autoload 'bug-footprint-popup-menu "bug-breakpoint" "Breakpoint manager." t)

;; bug-connect
(autoload 'bug-connect-buffer "bug-connect" "Bug connect buffer." t)
(autoload 'bug-connected-buffer-p "bug-connect" "Bug connect buffer." t)
(autoload 'bug-connect-file "bug-connect" "Bug connect buffer." t)
(autoload 'bug-toggle-connect-buffer "bug-connect" "Bug connect buffer." t)
(autoload 'bug-disconnect-buffer "bug-connect" "Bug connect buffer." t)
(autoload 'bug-disconnect-all-buffers "bug-connect" "Bug connect buffer." t)

;; bug-source
(autoload 'bug-find-file "bug-source" "Bug source line display." t)
(autoload 'bug-prompt-file-line "bug-source" "Bug source line display." t)
(autoload 'bug-display-file-line "bug-source" "Bug source line display." t)

;; bug-class
(autoload 'bug-file-to-class "bug-class" "Bug file/class conversion." t)
(autoload 'bug-class-to-file "bug-class" "Bug file/class conversion." t)

;; bug-hooking
(autoload 'bug-hook-command "bug-hooking" "Bug command hooks." t)
(autoload 'bug-add-command-hook "bug-hook" "Bug hook manager." t)
(autoload 'bug-remove-command-hook "bug-hook" "Bug hook manager." t)
(autoload 'bug-run-command-hooks "bug-hook" "Bug hook manager." t)
(autoload 'bug-installed-hook-p "bug-hook" "Bug hook manager." t)
(autoload 'bug-make-hook-buffer "bug-hook" "Bug hook manager." t)

;; bug-stack
(autoload 'bug-stack-start "bug-stack" "Bug stack frame inspector." t)
(autoload 'bug-stack-quit "bug-stack" "Bug stack frame inspector." t)
(autoload 'bug-stack-toggle "bug-stack" "Bug stack frame inspector." t)

;; bug-args
(autoload 'bug-args-start "bug-args" "Bug args frame inspector." t)
(autoload 'bug-args-quit "bug-args" "Bug args frame inspector." t)
(autoload 'bug-args-toggle "bug-args" "Bug args frame inspector." t)

