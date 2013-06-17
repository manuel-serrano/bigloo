;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-icon.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  8 11:25:01 1998                          */
;*    Last change :  Wed Apr 27 12:15:23 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Ude icon collection                                          */
;*    -------------------------------------------------------------    */
;*    This module contains only picture. It does not contains button   */
;*    in the sens of xemacs button.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-icon)
(require 'bmacs-config)
(require 'ude-custom)

;;; add to emacs image-load-path the directory containing the toolbar icons
(add-to-list 'image-load-path (concat bmacs-lispdir 
				      "/" 
				      ude-toolbar-image-directory))


(defvar ude-close-toolbar-icon "ude-close-toolbar-icon.xpm")

(defvar ude-open-toolbar-icon "ude-open-toolbar-icon.xpm")

(defvar ude-tag-icon "ude-tag-icon.xpm")
 
(defvar ude-tag-icon-sans-legend "ude-tag-icon-sans-legend.xpm")

(defvar ude-declaration-icon  "ude-declaration-icon.xpm")
 
(defvar ude-declaration-icon-sans-legend
 "ude-declaration-icon-sans-legend.xpm")

(defvar ude-footprint-icon
  "ude-footprint-icon.xpm")

(defvar ude-footprint-icon-sans-legend
 "ude-footprint-icon-sans-legend.xpm")

(defvar ude-ibuilder-icon
 "ude-ibuilder-icon.xpm")

(defvar ude-export-icon
  "ude-export-icon.xpm")

(defvar ude-export-icon-sans-legend
 "ude-export-icon-sans-legend.xpm")

(defvar ude-import-icon
 "ude-import-icon.xpm")

(defvar ude-import-icon-sans-legend
  "ude-import-icon-sans-legend.xpm")

(defvar ude-c-import-icon
  "ude-c-import-icon.xpm")

(defvar ude-c-import-icon-sans-legend
 "ude-c-import-icon-sans-legend.xpm")

(defvar ude-indent-icon 
  "ude-indent-icon.xpm")

(defvar ude-indent-icon-sans-legend 
  "ude-indent-icon-sans-legend.xpm")

(defvar ude-print-icon
  "ude-print-icon.xpm")

(defvar ude-print-icon-sans-legend
  "ude-print-icon-sans-legend.xpm")

(defvar ude-compile-icon "ude-compile-icon.xpm")

(defvar ude-compile-icon-sans-legend "ude-compile-icon-sans-legend.xpm")

(defvar ude-profile-compile-icon "ude-profile-compile-icon.xpm")

(defvar ude-profile-compile-icon-sans-legend "ude-profile-compile-icon-sans-legend.xpm")

(defvar ude-profile-extra-compile-icon "ude-profile-extra-compile-icon.xpm")

(defvar ude-profile-extra-compile-icon-sans-legend "ude-profile-extra-compile-icon-sans-legend.xpm")

(defvar ude-compile-detach-icon "ude-compile-detach-icon.xpm")

(defvar ude-compile-detach-icon-sans-legend "ude-compile-detach-icon-sans-legend.xpm")

(defvar ude-clean-icon "ude-clean-icon.xpm")

(defvar ude-clean-icon-sans-legend "ude-clean-icon-sans-legend.xpm")

(defvar ude-record-icon "ude-record-icon.xpm")

(defvar ude-record-icon-sans-legend "ude-record-icon-sans-legend.xpm")

(defvar ude-make-make-icon "ude-make-make-icon.xpm")

(defvar ude-make-make-icon-sans-legend "ude-make-make-icon-sans-legend.xpm")

(defvar ude-make-entry-icon "ude-make-entry-icon.xpm")

(defvar ude-make-entry-icon-sans-legend "ude-make-entry-icon-sans-legend.xpm")

(defvar ude-root-icon "ude-root-icon.xpm")

(defvar ude-root-icon-sans-legend "ude-root-icon-sans-legend.xpm")

(defvar ude-repl-icon "ude-repl-icon.xpm")

(defvar ude-repl-icon-sans-legend "ude-repl-icon-sans-legend.xpm")

(defvar ude-dbg-icon "ude-dbg-icon.xpm")

(defvar ude-dbg-icon-sans-legend "ude-dbg-icon-sans-legend.xpm")

(defvar ude-profile-icon.old "ude-profile-icon.old.xpm")

(defvar ude-profile-icon.old "ude-profile-icon.old.xpm")

(defvar ude-profile-icon "ude-profile-icon.xpm")

(defvar ude-profile-icon-sans-legend "ude-profile-icon-sans-legend.xpm")

(defvar ude-execute-icon "ude-execute-icon.xpm")

(defvar ude-execute-icon-sans-legend "ude-execute-icon-sans-legend.xpm")

(defvar ude-dbg-connect-icon "ude-dbg-connect-icon.xpm")

(defvar ude-dbg-connect-icon-sans-legend "ude-dbg-connect-icon-sans-legend.xpm")

(defvar ude-info-icon "ude-info-icon.xpm")

(defvar ude-info-icon-sans-legend "ude-info-icon-sans-legend.xpm")

(defvar ude-help-icon.old "ude-help-icon.old.xpm")

(defvar ude-help-icon "ude-help-icon.xpm")

(defvar ude-help-icon-sans-legend "ude-help-icon-sans-legend.xpm")

(defvar ude-pref-icon "ude-pref-icon.xpm")

(defvar ude-pref-icon-sans-legend "ude-pref-icon-sans-legend.xpm")

(defvar ude-dbg-file-icon "ude-dbg-file-icon.xpm")

(defvar ude-dbg-run-icon "ude-dbg-run-icon.xpm")

(defvar ude-dbg-continue-icon "ude-dbg-continue-icon.xpm")

(defvar ude-dbg-step-icon "ude-dbg-step-icon.xpm")

(defvar ude-dbg-next-icon "ude-dbg-next-icon.xpm")

(defvar ude-dbg-until-icon "ude-dbg-until-icon.xpm")

(defvar ude-dbg-finish-icon "ude-dbg-finish-icon.xpm")

(defvar ude-dbg-show-icon "ude-dbg-show-icon.xpm")

(defvar ude-stop-icon "ude-stop-icon.xpm")

(defvar ude-error-icon "ude-error-icon.xpm")

(defvar ude-back-icon "ude-back-icon.xpm")

(defvar ude-up-icon "ude-up-icon.xpm")

(defvar ude-next-icon "ude-next-icon.xpm")

(defvar ude-forward-icon "ude-forward-icon.xpm")

(defvar ude-home-icon "ude-home-icon.xpm")

(defvar ude-search-icon "ude-search-icon.xpm")

(defvar ude-hotlist-icon "ude-hotlist-icon.xpm")

(defvar ude-open-icon "ude-open-icon.xpm")

(defvar ude-print-icon "ude-print-icon.xpm")

(defvar ude-repl-next-icon "ude-repl-next-icon.xpm")

(defvar ude-repl-prev-icon "ude-repl-prev-icon.xpm")

(defvar ude-quit-icon "ude-quit-icon.xpm")

(defvar ude-open-icon "ude-open-icon.xpm")

(defvar ude-edit-icon "ude-edit-icon.xpm")

(defvar ude-edit-icon-sans-legend "ude-edit-icon-sans-legend.xpm")

(defvar ude-edit-makefile-icon "ude-edit-makefile-icon.xpm")

(defvar ude-browse-icon "ude-browse-icon.xpm")

(defvar ude-graph-icon "ude-graph-icon.xpm")
