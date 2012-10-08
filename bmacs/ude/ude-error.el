;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-error.el               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 11 08:19:19 1998                          */
;*    Last change :  Mon Oct  8 14:01:59 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Ude error management                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-error)
(require 'ude-custom)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))

;*---------------------------------------------------------------------*/
;*    ude-error ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function display the error message and, when customized,    */
;*    play the error sound.                                            */
;*---------------------------------------------------------------------*/
(defun ude-error (msg &rest rest)
  ;; play a sound
  (if ude-error-sound-p
      (if (device-sound-enabled-p)
	  (let ((synchronous-sounds nil))
	    (condition-case ()
		(play-sound-file
		 (concat bmacs-lispdir
			 "/"
			 ude-error-sound-file ude-error-sound-volume))
	      (error (beep))))
	(beep)))
  (let ((msg (ude-unformat (apply 'format (cons msg rest)))))
    ;; display the error message
    (insert-text-property 0 (length msg) 'face 'ude-error-face msg)
    (display-message 'no-log msg)
    (sit-for 2 nil))
  nil)

;*---------------------------------------------------------------------*/
;*    ude-unformat ...                                                 */
;*---------------------------------------------------------------------*/
(defun ude-unformat (str)
  (let ((l (length str))
	(nl 0))
    (let ((i 0))
      (while (< i l)
	(if (eq (aref str i) ?%)
	    (setq nl (+ 2 nl))
	  (setq nl (1+ nl)))
	(setq i (1+ i))))
    (let ((nstr (make-string nl ? ))
	  (i 0)
	  (j 0))
      (while (< i l)
	(let ((c (aref str i)))
	  (aset nstr j c)
	  (setq i (1+ i))
	  (if (eq c ?%)
	      (progn
		(aset nstr (1+ j) ?%)
		(setq j (+ 2 j)))
	    (setq j (1+ j)))))
      nstr)))
