;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-autoload.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug  1 10:27:42 1998                          */
;*    Last change :  Wed Jan 29 11:04:31 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    All the autoload delcarations.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-autoload)

;; bee-mode
(autoload 'bee-debug                   "bee-mode" "Bee emacs mode." t)
;; bee-repl
(autoload 'bee-repl-send-region        "bee-repl" "Read-eval-print loop." t)
(autoload 'bee-repl-send-buffer        "bee-repl" "Read-eval-print loop." t)
(autoload 'bee-repl-send-define        "bee-repl" "Read-eval-print loop." t)
(autoload 'bee-repl-send-last-sexp     "bee-repl" "Read-eval-print loop." t)
(autoload 'bee-repl-send-toplevel-sexp "bee-repl" "Read-eval-print loop." t)
;; bee-indent
(autoload 'bee-external-indent         "bee-indent" "Indent source code." t)
(autoload 'bee-indent-define           "bee-indent" "Indent source code." t)
(autoload 'bee-indent-toplevel-sexp    "bee-indent" "Indent source code." t)
(autoload 'bee-indent-last-sexp        "bee-indent" "Indent source code." t)
(autoload 'bee-indent-sexp             "bee-indent" "Indent source code." t)
(autoload 'bee-indent-region           "bee-indent" "Indent source code." t)
;; bee-module
(autoload 'bee-export-definition       "bee-module" "Bee module handling." t)
(autoload 'bee-import-binding          "bee-module" "Bee module handling." t)
(autoload 'bee-import-c-file           "bee-module" "Bee module handling." t)
(autoload 'bee-export-binding          "bee-module" "Bee module handling." t)
(autoload 'bee-export-function         "bee-module" "Bee module handling." t)
(autoload 'bee-export-variable         "bee-module" "Bee module handling." t)
(autoload 'bee-module-declaration-p    "bee-module" "Bee module handling." t)
(autoload 'bee-module-declaration-find "bee-module" "Bee module handling." t)
(autoload 'bee-get-module-name         "bee-module" "Bee module handling." t)
;; bee-expand
(autoload 'bee-expand-buffer           "bee-expand" "Macro expansion." t)
(autoload 'bee-expand-define           "bee-expand" "Macro expansion." t)
(autoload 'bee-expand-toplevel-sexp    "bee-expand" "Macro expansion." t)
(autoload 'bee-expand-last-sexp        "bee-expand" "Macro expansion." t)
(autoload 'bee-expand-region           "bee-expand" "Macro expansion." t)
;; bee-mouse
(autoload 'bee-add-menu                "bee-mouse" "Bee mouse handling." t)
;; bee-keymap
(autoload 'bee-add-region-popup-entry  "bee-keymap" "Bee keymap definition." t)
;; bee-tags
(autoload 'bee-find-module             "bee-tags" "Bee tags finder." t)
(autoload 'bee-find-module-nokeymap    "bee-tags" "Bee tags finder." t)
(autoload 'bee-find-module-list        "bee-tags" "Bee search engine." t)
(autoload 'bee-find-definition         "bee-tags" "Bee search engine." t)
(autoload 'bee-tags-find               "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find/ident         "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find-next          "bee-tags" "Bee tags finder." t)
(autoload 'bee-tag-find                "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find-variable      "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find-class         "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find-variable-noselect "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-exists-p           "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find-if-exists     "bee-tags" "Bee tags finder." t)
(autoload 'bee-tags-find-or-info       "bee-tags" "Bee tags finder." t)
(autoload 'bee-find-afile-create-p     "bee-tags" "Bee tags finder." t)
(autoload 'bee-find-afile-module       "bee-tags" "Bee tags finder." t)
;; bee-profile
(autoload 'bee-profile-highlight-buffer "bee-profile" "Bee profiler." t)
(autoload 'bee-profiler-start           "bee-profile" "Bee profiler." t)
(autoload 'bee-profiler-inspect         "bee-profile" "Bee profiler." t)
;; bee-jinsight
(autoload 'bee-jinsight-start          "bee-jinsight" "Jinsight viewer." t)
(autoload 'bee-jinsight-module-cpu     "bee-jinsight" "Jinsight viewer." t)
(autoload 'bee-jinsight-module-instances "bee-jinsight" "Jinsight viewer." t)
(autoload 'bee-jinsight-class-instances "bee-jinsight" "Jinsight viewer." t)
(autoload 'bee-jinsight-select-module  "bee-jinsight" "Jinsight viewer." t)
(autoload 'bee-jinsight-select-function  "bee-jinsight" "Jinsight viewer." t)
;; bee-browse
(autoload 'bee-browse-highlight-buffer "bee-browse" "Bee browser." t)
(autoload 'bee-browser-start           "bee-browse" "Bee browser." t)
(autoload 'bee-browser-ready-p         "bee-browse" "Bee ibuilder" t)
(autoload 'bee-browser-find-usage      "bee-browse" "Bee ibuilder" t)
;; bee-usage
(autoload 'bee-find-local-definition   "bee-usage" "Bee variable usage." t)
(autoload 'bee-usage-find              "bee-usage" "Bee usage search." t)
(autoload 'bee-usage-info              "bee-usage" "Bee usage search." t)
;; bee-config
(autoload 'bee-customize               "bee-config" "Bee configuration." t)
;; bee-flock
(autoload 'bee-font-lock-get-info-keywords "bee-flock" "Bee font lock." t)
;; bee-ibuilder
(autoload 'bee-interface-builder-start      "bee-ibuilder" "Bee ibuilder" t)
(autoload 'bee-interface-builder-module-p   "bee-ibuilder" "Bee ibuilder" t)
(autoload 'bee-interface-builder-module     "bee-ibuilder" "Bee ibuilder" t)
(autoload 'bee-interface-builder-ready-p    "bee-ibuilder" "Bee ibuilder" t)
(autoload 'bee-find-interface-builder-module "bee-ibuilder" "Bee ibuilder" t)
;; bee-doc
(autoload 'bee-doc-initialize        "bee-doc" "Bee documentation manuals." t)
(autoload 'bee-doc-visit             "bee-doc" "Bee documentation manuals." t)
(autoload 'bee-doc-visit-bigloo      "bee-doc" "Bee documentation manuals." t)
(autoload 'bee-doc-ident             "bee-doc" "Bee documentation manuals." t)
(autoload 'bee-doc-installed-manuals "bee-doc" "Bee documentation manuals." t)
;* ;; bee-begoo                                                        */
;* (autoload 'bee-begoo-ready-p           "bee-begoo" "The Begoo connection." t) */
