;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bee/bee-repl.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 16:31:35 1998                          */
;*    Last change :  Wed Jan 23 16:23:06 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implements a Bigloo repl process embedded in emacs.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'bee-repl)
(require 'ude-config)
(require 'ude-autoload)
(require 'bee-mode)

;*---------------------------------------------------------------------*/
;*    bee-repl-send-define ...                                         */
;*---------------------------------------------------------------------*/
(defun bee-repl-send-define ()
  "Send the current definition to the inferior Bigloo process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (if (beginning-of-defun)
	  (ude-repl-send-region (point) end)))))

;*---------------------------------------------------------------------*/
;*    bee-repl-send-last-sexp ...                                      */
;*---------------------------------------------------------------------*/
(defun bee-repl-send-last-sexp ()
  "Send the last s-expression to the inferior Bigloo process."
  (interactive)
  (save-excursion
    (forward-sexp -1)
    (let ((start (point)))
      (forward-sexp 1)
      (ude-repl-send-region start (point)))))

;*---------------------------------------------------------------------*/
;*    bee-repl-send-toplevel-sexp ...                                  */
;*---------------------------------------------------------------------*/
(defun bee-repl-send-toplevel-sexp (pos)
  "Send the current at POS s-expression to the inferiror Bigloo process."
  (interactive "dPosition: ")
  (let ((sexp (bee-find-toplevel-sexp pos)))
    (if (consp sexp)
	(ude-repl-send-region (car sexp) (cdr sexp))
      (error "Corrupted toplevel sexp"))))

