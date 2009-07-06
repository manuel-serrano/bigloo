;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bmacs.el                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan 20 11:04:07 2002                          */
;*    Last change :  Wed Mar 23 16:18:39 2005 (serrano)                */
;*    Copyright   :  2002-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The emacs lisp file to be loaded at emacs run-time in order      */
;*    to install the whole bmacs package.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bmacs)

;*---------------------------------------------------------------------*/
;*    bmacs custom                                                     */
;*---------------------------------------------------------------------*/
(defcustom bmacs-modes '(bee)
  "Bmacs Emacs Lisp supported modes."
  :tag "Bmacs"
  :group 'bmacs
  :type '(repeat (symbol)))

;*---------------------------------------------------------------------*/
;*    Specific initialization                                          */
;*---------------------------------------------------------------------*/
(if (featurep 'xemacs)
    (require 'bmacs-xemacs)
  (require 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    Autoloads                                                        */
;*---------------------------------------------------------------------*/
(autoload 'ude-compile "ude-compile" "" t)
(autoload 'bee-mode "bee-mode" "bee mode" t)
(autoload 'cee-hook "cee-hook" "cee mode hooking" t)

;*---------------------------------------------------------------------*/
;*    Cee mode                                                         */
;*---------------------------------------------------------------------*/
(if (and (consp (memq 'cee bmacs-modes))
	 (locate-library "cee-hook"))
    (progn
      (add-hook 'c-mode-common-hook (function cee-hook))
      (add-hook 'c-mode-hook (function cee-hook))
      (autoload 'cee-indent-hook "cee-indent" "Cee indent configuration" t)
      (add-hook 'cee-hook (function cee-indent-hook))
      (autoload 'cee-font-lock-init "cee-flock" "cee mode hooking" t)
      (add-hook 'c-mode-hook (function cee-font-lock-init))))

;*---------------------------------------------------------------------*/
;*    Lee mode                                                         */
;*---------------------------------------------------------------------*/
(if (and (consp (memq 'lee bmacs-modes))
	 (locate-library "lee-hook"))
    (progn
      (autoload 'lee-hook "lee-hook" "lee mode hooking" t)
      (add-hook 'lisp-mode-hook (function lee-hook))
      (autoload 'lee-font-lock-init "lee-flock" "lee mode hooking" t)
      (add-hook 'lisp-mode-hook (function lee-font-lock-init))))

;*---------------------------------------------------------------------*/
;*    Auto-mode-alist                                                  */
;*---------------------------------------------------------------------*/
(setq auto-mode-alist
      (cons '("\\.\\(?:scm\\|sch\\|bgl\\|scme\\|bee\\|ast\\)$" . bee-mode)
	    auto-mode-alist))

