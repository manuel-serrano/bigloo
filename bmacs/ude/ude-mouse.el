;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-mouse.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug  6 11:16:17 1998                          */
;*    Last change :  Wed May 15 07:57:52 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The mouse binding.                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-mouse)

;*---------------------------------------------------------------------*/
;*    ude-predicate-menus ...                                          */
;*---------------------------------------------------------------------*/
(defvar ude-predicate-menus '()
  "*The list of predicate menus.
A predicate menu is a pair (PRED . MENU).
PRED is a predicate with of one argument, an EVENT.
MENU is a function that popups a menu.")
(make-variable-buffer-local 'ude-predicate-menus)

;*---------------------------------------------------------------------*/
;*    ude-add-menu ...                                                 */
;*---------------------------------------------------------------------*/
(defun ude-add-menu (pred menu)
  (setq ude-predicate-menus (cons (cons pred menu) ude-predicate-menus)))

;*---------------------------------------------------------------------*/
;*    ude-remove-menu ...                                              */
;*---------------------------------------------------------------------*/
(defun ude-remove-menu (pred menu)
  (let ((cell (assq pred ude-predicate-menus)))
    (if (consp cell)
	(setq ude-predicate-menus (delq cell ude-predicate-menus)))))

;*---------------------------------------------------------------------*/
;*    ude-predicate-mouse-event ...                                    */
;*---------------------------------------------------------------------*/
(defun ude-predicate-mouse-event (event)
  (interactive "e")
  (let ((l ude-predicate-menus))
    (while (consp l)
      (if (funcall (car (car l)) event)
	  (progn
	    (funcall (cdr (car l)) event)
	    (setq l nil))
	(setq l (cdr l))))))



