;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-autoload.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug  1 10:27:42 1998                          */
;*    Last change :  Sun Apr 10 09:59:41 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    All the autoload delcarations.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-autoload)

;; customization
(autoload 'ude-customize "ude-custom" "Ude customization." t)
;; config
(autoload 'bmacs-docdir "bmacs-config" "Bmacs configuration." t)
;; parent
(autoload 'ude-paren-init "ude-paren" "Ude paren initialization." t)
;; root
(autoload 'ude-auto-find-root-directory "ude-root" "Root directory." t)
(autoload 'ude-auto-set-root-directory "ude-root" "Root directory." t)
(autoload 'ude-user-set-root-directory "ude-root" "Root directory." t)
;; tools
(autoload 'ude-string-suffix "ude-tools" "Ude various tools." t)
(autoload 'ude-string-prefix "ude-tools" "Ude various tools." t)
(autoload 'ude-delete-buffer-window-frame "ude-tools" "Ude various tools." t)
(autoload 'ude-empty-window-p "ude-tools" "Ude various tools." t)
(autoload 'ude-region-excerpt "ude-tools" "Ude various tools." t)
(autoload 'ude-string-excerpt "ude-tools" "Ude various tools." t)
(autoload 'ude-quit-frame-or-emacs "ude-tools" "Ude various tools." t)
(autoload 'ude-split-menu-entries "ude-tools" "Ude various tools." t)
(autoload 'ude-one-frame-buffer-p "ude-tools" "Ude various tools." t)
(autoload 'ude-system "ude-tools" "Ude various tools." t)
(autoload 'ude-string-xpm-p "ude-tools" "Ude various tools." t)
(autoload 'ude-too-bar-delete-frame "ude-tools" "Ude various tools." t)
;; error
(autoload 'ude-error "ude-error" "Ude error management." t)
(autoload 'ude-set-error-sound-volume "ude-error" "Ude error management." t)
;; toolbar
(autoload 'ude-close-toolbar "ude-toolbar" "Ude command toolbar handling." t)
(autoload 'ude-open-toolbar "ude-toolbar" "Ude command toolbar handling." t)
;; compilation
(autoload 'ude-mode-compile "ude-compile" "Compile a project." t)
(autoload 'ude-view-last-compile-messages "ude-compile" "View compile msg." t)
(autoload 'ude-compile-messages-available-p "ude-compile" "Available msg?" t)
(autoload 'ude-success-hook "ude-compile" "Compilation success hooking." t)
(autoload 'ude-get-compile-command "ude-compile" "Compilation failure." t)
(autoload 'ude-compile-makefile-entry "ude-compile" "Entry compilation." t)
(autoload 'ude-compile-menu "ude-compile" "Entry compilation." t)
(autoload 'ude-mode-compile-from-menu "ude-compile" "Entry compilation." t)
(autoload 'ude-mode-jcompile-from-menu "ude-compile" "Entry compilation." t)
(autoload 'ude-last-compile-buffer "ude-compile" "Entry compilation." t)
;; makefile
(autoload 'ude-fetch-makefile-entries "ude-makefile" "Makefile handling." t)
(autoload 'ude-fetch-makefile-one-entry "ude-makefile" "Makefile handling." t)
(autoload 'ude-fetch-makefile-binary-entry "ude-makefile" "Makefile fetch." t)
(autoload 'ude-fetch-makefile-binary_p-entry "ude-makefile" "Makefile fetch" t)
(autoload 'ude-fetch-makefile-mode-entry "ude-makefile" "Makefile fetch" t)
(autoload 'ude-edit-makefile "ude-makefile" "Makefile handling." t)
(autoload 'ude-remove-makefile "ude-makefile" "Makefile handling." t)
(autoload 'ude-update-makefile "ude-makefile" "Makefile handling." t)
(autoload 'ude-generate-makefile "ude-makefile" "Makefile handling." t)
(autoload 'ude-make-ude "ude-makefile" "Makefile handling." t)
(autoload 'ude-add-makefile-entry "ude-makefile" "Entry compilation." t)
(autoload 'ude-add-user-makefile-entry "ude-makefile" "Entry compilation." t)
(autoload 'ude-makefile-debug-mode "ude-makefile" "Compilation." t)
(autoload 'ude-makefile-devel-mode "ude-makefile" "Compilation." t)
(autoload 'ude-makefile-final-mode "ude-makefile" "Compilation." t)
(autoload 'ude-makefile-set-name "ude-makefile" "Compilation." t)
;; info
(autoload 'ude-info-mode-hook "ude-info" "info mode hook." t)
(autoload 'ude-info-init "ude-info" "info mode hook." t)
;; repl
(autoload 'ude-repl-other-frame "ude-repl" "Read-eval-print loop." t)
(autoload 'ude-repl-send-buffer "ude-repl" "Read-eval-print loop." t)
(autoload 'ude-repl-send-define "ude-repl" "Read-eval-print loop." t)
(autoload 'ude-repl-send-last-sexp "ude-repl" "Read-eval-print loop." t)
(autoload 'ude-repl-send-toplevel-sexp "ude-repl" "Read-eval-print loop." t)
(autoload 'ude-repl-send-region "ude-repl" "Read-eval-print loop." t)
;; expand
(autoload 'ude-expand-buffer "ude-expand" "Macro expansion." t)
(autoload 'ude-expand-define "ude-expand" "Macro expansion." t)
(autoload 'ude-expand-toplevel-sexp "ude-expand" "Macro expansion." t)
(autoload 'ude-expand-last-sexp "ude-expand" "Macro expansion." t)
(autoload 'ude-expand-region "ude-expand" "Macro expansion." t)
;; ident
(autoload 'ude-interactive-ident "ude-ident" "Fetch identifiers." t)
(autoload 'ude-request-identifier-at "ude-ident" "Fetch identifiers." t)
(autoload 'ude-fetch-then-request-identifier "ude-ident" "Fetch identifiers." t)
(autoload 'ude-fetch-identifier "ude-ident" "Fetch identifiers." t)
(autoload 'ude-fetch-identifier-region "ude-ident" "Fetch identifiers." t)
(autoload 'ude-ident-regexp "ude-ident" "Ude identifiers regexp."t t)
(autoload 'ude-tags-balloon-start "ude-ident" "Ude tags balloon."t t)
;; docline
(autoload 'ude-info-docline "ude-docline" "Ude online documentation." t)
(autoload 'ude-info-docline-ident "ude-docline" "Ude online documentation." t)
(autoload 'ude-info-ref-internal "ude-docline" "Ude online documentation." t)
(autoload 'ude-info-section "ude-docline" "Ude online documentation." t)
(autoload 'ude-man-docline "ude-docline" "Ude online documentation." t)
(autoload 'ude-fontify-doc-source "ude-docline" "Ude online documentation." t)
(autoload 'ude-mouse-ref-armed-p "ude-docline" "Ude online documentation." t)
(autoload 'ude-mouse-ref-label "ude-docline" "Ude online documentation." t)
(autoload 'ude-mouse-make-label "ude-docline" "Ude online documentation." t)
(autoload 'ude-mouse-make-ref "ude-docline" "Ude online documentation." t)
(autoload 'ude-mouse-make-mutual-ref "ude-docline" "Ude online documentation." t)

;; ude-mouse
(autoload 'ude-add-menu "ude-mouse" "Ude mouse handling." t)
(autoload 'ude-remove-menu "ude-mouse" "Ude mouse handling." t)
(autoload 'ude-predicate-mouse-event "ude-mouse" "Ude mouse handling." t)
;; ude-prof
(autoload 'ude-compile-for-profile "ude-profile" "Profiling." t)
(autoload 'ude-compile-for-extra-profile "ude-profile" "Profiling." t)
(autoload 'ude-compile-for-clean-profile "ude-profile" "Profiling." t)
(autoload 'ude-load-profile-file "ude-profile" "The profiling tools." t)
(autoload 'ude-create-profile-buffer "ude-profile" "The profiling tools." t)
(autoload 'ude-run-for-profile "ude-profile" "The profiling tools." t)
;; ude-version
(autoload 'ude-checkout-file-version "ude-version" "Version manager." t)
(autoload 'ude-diff-file-version "ude-version" "Version manager." t)
(autoload 'ude-checkin-project "ude-version" "Version manager." t)
(autoload 'ude-tar-gz-project "ude-version" "Version manager." t)
;; ude-balloon
(autoload 'ude-add-ballon-action "ude-balloon" "Bdb balloon system." t)
(autoload 'ude-balloon-start "ude-balloon" "Bdb balloon system." t)
(autoload 'ude-balloon-stop "ude-balloon" "Bdb balloon system." t)
(autoload 'ude-balloon-get-buffer "ude-balloon" "Bdb balloon system." t)
(autoload 'ude-balloon-get-point "ude-balloon" "Bdb balloon system." t)
(autoload 'ude-remove-balloon-action "ude-balloon" "Bdb balloon system." t)
;; id-select
(autoload 'id-select-sexp "id-select" "Id select tools." t)
(autoload 'id-select-sexp-start "id-select" "Id select tools." t)
(autoload 'id-select-symbol "id-select" "Id select tools." t)
;; ude-about
(autoload 'ude-about "ude-about" "About ude." t)
;; ude-html
(autoload 'ude->html "ude-html" "Souce code to HTML converter." t)
(autoload 'ude->html-file "ude-html" "Souce code to HTML converter." t)
(autoload 'ude-html-fontify-html-buffer "ude-html" "Souce to HTML." t)
(autoload 'ude-html-fontify-html-region "ude-html" "Souce to HTML." t)
;; ude-sui
(autoload 'ude-sui-find-ref "ude-sui" "Scribe SUI file handling." t)
;; etags-add
(autoload 'find-tag-other-frame-if-new "etags-add" "Etags." t)
