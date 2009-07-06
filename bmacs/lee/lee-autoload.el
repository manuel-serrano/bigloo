;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-autoload.el            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug  1 10:27:42 1998                          */
;*    Last change :  Sun Nov 22 08:23:59 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    All the autoload delcarations.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-autoload)

;; lee-mouse
(autoload 'lee-add-menu                "lee-mouse" "Lee mouse handling." t)
;; lee-keymap
(autoload 'lee-add-region-popup-entry  "lee-keymap" "Lee keymap definition." t)
;; lee-tags
(autoload 'lee-find-definition         "lee-tags" "Lee search engine." t)
(autoload 'lee-tags-find               "lee-tags" "Lee tags finder." t)
(autoload 'lee-tags-find-next          "lee-tags" "Lee tags finder." t)
(autoload 'lee-tags-find-variable      "lee-tags" "Lee tags finder." t)
(autoload 'lee-tags-find-variable-noselect "lee-tags" "Lee tags finder." t)
(autoload 'lee-tag-find                "lee-tags" "Lee tags finder." t)
;; lee-config
(autoload 'lee-customize               "lee-config" "Lee configuration." t)
;; lee-flock
(autoload 'lee-font-lock-get-info-keywords "lee-flock" "Lee font lock." t)
;; lee-indent
(autoload 'lee-indent-exp              "lee-indent" "Lee indenter." t)
