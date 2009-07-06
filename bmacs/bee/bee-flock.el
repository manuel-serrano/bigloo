;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-flock.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 08:00:32 1998                          */
;*    Last change :  Tue Jan 28 11:28:54 2003 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Bee font-lockification                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-flock)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-config)
(require 'ude-custom)
(require 'ude-autoload)
(require 'bee-config)

;*---------------------------------------------------------------------*/
;*    bee-font-lock-get-info-keywords ...                              */
;*    -------------------------------------------------------------    */
;*    The Bee font lock keyword list for xinfo.                        */
;*---------------------------------------------------------------------*/
(defun bee-font-lock-get-info-keywords ()
  (append
   (list (cons "^[ ]+- \\(?:essential \\|optional \\|library \\|bigloo \\|[CR][^ ]+ \\)?\\(?:\\(?:rgc \\)?procedure\\|generic\\|variable\\|Name\\|Class\\): .*$"
	       'font-lock-function-name-face)
	 (cons "^ - \\(?:essential \\|library \\|bigloo \\)?syntax: .*$"
	       'ude-font-lock-face-2)
	 (cons "^ - bigloo module clause: .*$"
	       'ude-font-lock-face-3)
	 (cons ":^[*] [^:]+:" 'font-lock-keyword-face)
	 (cons "\\([^=]=>\\|-|\\) .+$" 'ude-italic-face)
	 (cons "^ - bigloo class: .*$"
	       'ude-font-lock-face-4)
	 (cons "`\\([^' ]+\\)'" 'ude-invisible-face)
	 (list "`\\([^' ]+\\)'" 1 'ude-italic-face t)
	 (cons ";;.*$" 'font-lock-comment-face)
	 (cons "\"[^\"]+\"" 'font-lock-string-face))
   bee-font-lock-keywords))
  
  
