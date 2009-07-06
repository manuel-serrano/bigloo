;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-paren.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 12 21:15:52 1998                          */
;*    Last change :  Wed Sep 14 13:48:50 2005 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Ude paren handling                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-paren)
(require 'ude-custom)

;*---------------------------------------------------------------------*/
;*    ude-paren-initialized-p ...                                      */
;*---------------------------------------------------------------------*/
(defvar ude-paren-initialized-p nil)

;*---------------------------------------------------------------------*/
;*    ude-paren-init ...                                               */
;*---------------------------------------------------------------------*/
(defun ude-paren-init ()
  (unless ude-paren-initialized-p
    (setq ude-paren-initialized-p t)
    (when ude-paren-adapt-p
      (custom-set-variables
       '(paren-mode (quote paren) nil (paren))
       '(show-paren-delay 0)
       '(show-paren-mode (quote paren) nil (paren)))
      (custom-set-faces
       '(paren-face-match ((((class color)) (:background "green"))))
       '(paren-face-mismatch ((((class color)) (:foreground "white" :background "red"))))
       '(show-paren-match-face ((((class color)) (:background "green"))))
       '(show-paren-mismatch-face ((((class color)) (:background "red"))))
       '(paren-match ((t (:background "green"))))
       '(paren-mismatch ((t (:background "red"))))))))


	 
