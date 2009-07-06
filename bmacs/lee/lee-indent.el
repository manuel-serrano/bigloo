;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-indent.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 07:27:11 1998                          */
;*    Last change :  Wed Jan 23 17:34:24 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The Lee indent (this file is adapted from the Scheme mode by     */
;*    Bill Rozas).                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-indent)
(require 'ude-autoload)
(require 'lee-config)

;*---------------------------------------------------------------------*/
;*    lee-indent-exp ...                                               */
;*---------------------------------------------------------------------*/
(defun lee-indent-exp ()
  (interactive)
  (condition-case err
      (indent-sexp)
    (error
     (if (and (consp (cdr err)) (stringp (car (cdr err))))
	 (ude-error "Illegal expression")
       nil))))



