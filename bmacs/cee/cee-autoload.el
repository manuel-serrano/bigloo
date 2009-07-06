;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/cee/cee-autoload.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug  1 10:27:42 1998                          */
;*    Last change :  Tue Aug 24 07:45:21 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    All the autoload delcarations.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'cee-autoload)

;; cee-indent
(autoload 'cee-external-indent         "cee-indent" "Indent source code." t)
(autoload 'cee-indent-exp              "cee-indent" "Indent source code." t)
(autoload 'cee-indent-hook             "cee-indent" "Indent source code." t)
;; cee-mouse
(autoload 'cee-add-menu                "cee-mouse" "Cee mouse handling." t)
;; cee-keymap
(autoload 'cee-add-region-popup-entry  "cee-keymap" "Cee keymap definition." t)
;; cee-tags
(autoload 'cee-find-definition         "cee-tags" "Cee search engine." t)
(autoload 'cee-tags-find               "cee-tags" "Cee tags finder." t)
(autoload 'cee-tags-find-next          "cee-tags" "Cee tags finder." t)
(autoload 'cee-tags-find-variable      "cee-tags" "Cee tags finder." t)
(autoload 'cee-tags-find-variable-noselect "cee-tags" "Cee tags finder." t)
(autoload 'cee-tag-find                "cee-tags" "Cee tags finder." t)
;; cee-profile
(autoload 'cee-profile-highlight-buffer "cee-profile" "Cee profiler." t)
;; cee-config
(autoload 'cee-customize               "cee-config" "Cee configuration." t)
;; cee-gdb
(autoload 'cee-gdb-init                "cee-gdb" "The Gdb connexion." t)
(autoload 'cee-gdb-other-frame         "cee-gdb" "The Gdb connexion." t)
;; cee-flock
(autoload 'cee-font-lock-get-info-keywords "cee-flock" "Cee font lock." t)
;; bee-profile
(autoload 'cee-profiler-start          "cee-profile" "Cee profiler." t)
;; bee-bdb
(autoload 'cee-kbdb-start              "cee-kbdb" "The Kbdb connection." t)
