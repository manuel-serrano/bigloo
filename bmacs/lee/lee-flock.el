;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-flock.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 08:52:09 1998                          */
;*    Last change :  Wed Jan 30 06:09:16 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The LEE fontification.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-flock)
(require 'font-lock)

;*---------------------------------------------------------------------*/
;*    lee-font-lock-init ...                                           */
;*---------------------------------------------------------------------*/
(defun lee-font-lock-init ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(lee-font-lock-keywords)))

;*---------------------------------------------------------------------*/
;*    lee-font-lock-get-info-keywords ...                              */
;*    -------------------------------------------------------------    */
;*    The Lee font lock keyword list for xinfo.                        */
;*---------------------------------------------------------------------*/
(defun lee-font-lock-get-info-keywords ()
  (list (cons "^ - \\(?:Function\\|Variable\\|Command\\|Constant\\): .*$"
	      'font-lock-function-name-face)
	(cons "^ - \\(?:Special Form\\|User Option\\|Specifier\\): .*$"
	      'ude-font-lock-face-2)
	(cons "^[*] [^:]+:" 'font-lock-keyword-face)
	(cons "\\([^=]=>\\|-|\\) .+$" 'ude-italic-face)
	(cons ";;.*$" 'font-lock-comment-face)
	(cons "\"[^\"]+\"" 'font-lock-string-face)))

