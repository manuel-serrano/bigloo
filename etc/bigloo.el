;*=====================================================================*/
;*    serrano/prgm/utils/emacs/local/bigloo.el                         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  8 14:06:11 1994                          */
;*    Last change :  Fri Jan 24 15:37:00 1997 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The bigloo emacs file                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The provide                                                      */
;*---------------------------------------------------------------------*/
(provide 'bigloo)
(require 'comint)

;*---------------------------------------------------------------------*/
;*    bigloo-name ...                                                  */
;*---------------------------------------------------------------------*/
(defvar bigloo-name "bigloo"
  "The name of the Bigloo system")
 
;*---------------------------------------------------------------------*/
;*    bigloo ...                                                       */
;*---------------------------------------------------------------------*/
(defun bigloo ()
  "Invoke Bigloo without any argument."
  (interactive)
  ;; we skip the regular Bigloo prompt. You may ajust this variable
  ;; when using specific prompt.
  (setq comint-prompt-regexp "^[0-9]+:=> ")
  (comint-run bigloo-name)) 

;*---------------------------------------------------------------------*/
;*    comint-bigloo-hook                                               */
;*---------------------------------------------------------------------*/
(add-hook 'comint-bigloo-hook
	  '(lambda ()
	     nil))

;*---------------------------------------------------------------------*/
;*    start-bigloo-process-other-frame ...                             */
;*---------------------------------------------------------------------*/
(defun start-bigloo-process-other-frame ()
   (interactive)
   (if (get-bigloo-frame "*Bigloo*")
       (bigloo)))
 
;*---------------------------------------------------------------------*/
;*    bigloo-kill-interpreter ...                                      */
;*---------------------------------------------------------------------*/
(defun bigloo-kill-interpreter ()
   (interactive)
   (if (yes-or-no-p "Do you really want to kill the Bigloo process ?")
	 (progn
	    (comint-send-string bigloo-name "(exit 0)")
	    (comint-send-string bigloo-name "\n")
	    (let ((buffer (get-buffer "*Bigloo*")))
	       (kill-buffer buffer)))))

(put 'bigloo-kill-interpreter 'menu-enable
     '(bufferp (get-buffer "*Bigloo*")))

;*---------------------------------------------------------------------*/
;*    bigloo-send-definition ...                                       */
;*---------------------------------------------------------------------*/
(defun bigloo-send-definition ()
  "Send the current definition to the inferior Bigloo process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (comint-send-region bigloo-name (point) end)
      (comint-send-string bigloo-name "\n"))))

(put 'bigloo-send-definition 'menu-enable
     '(fboundp 'bigloo-send-definition))

;*---------------------------------------------------------------------*/
;*    bigloo-send-region ...                                           */
;*---------------------------------------------------------------------*/
(defun bigloo-send-region (beg end)
   (interactive "r")
   (comint-send-region bigloo-name beg end)
   (comint-send-string bigloo-name "\n"))

(put 'bigloo-send-region 'menu-enable
     '(and (fboundp 'bigloo-send-region)
	   mark-active))

;*---------------------------------------------------------------------*/
;*    bigloo-send-buffer ...                                           */
;*---------------------------------------------------------------------*/
(defun bigloo-send-buffer ()
   (interactive)
   (comint-send-region bigloo-name (point-min) (point-max))
   (comint-send-string bigloo-name "\n"))

(put 'bigloo-send-buffer 'menu-enable
     '(fboundp 'bigloo-send-region))

;*---------------------------------------------------------------------*/
;*    get-bigloo-frame ...                                             */
;*    -------------------------------------------------------------    */
;*    On recherche la frame qui contient `buffer'. Si elle n'existe    */
;*    pas, on la creer.                                                */
;*---------------------------------------------------------------------*/
(defun get-bigloo-frame (buffer &optional frame-parameters)
  "On recherche la frame qui contient BUFFER. Si elle n'existe pas on la creer. Cette fonction retourne nil si une frame existait deja, t dans le cas
contraire"
  (interactive "b")
  (let ((buf (get-buffer buffer))
	(fp  (if (eq frame-parameters ())
		 default-frame-alist
		frame-parameters)))
    (if (bufferp buf)
	(progn
	  (let ((window (get-buffer-window buf t)))
	    (if (windowp window)
		(let ((frame (window-frame window)))
		  (if (framep frame)
		      (select-frame frame)
		    (let ((frame (make-frame
				  (cons '(window window)
					frame-parameters))))
		      (select-frame frame))))
	      (let ((old default-frame-alist))
		(setq default-frame-alist fp)
		(switch-to-buffer-other-frame buf)
		(setq default-frame-alist old))))
	  nil)
      (if (and (not (buffer-modified-p))
	       (string-equal (buffer-name (current-buffer)) "*scratch*"))
	  (progn
	    (selected-frame)
	    t)
	(let ((frame (make-frame fp)))
	  (select-frame frame)
	  (get-buffer-create buffer)
	  (switch-to-buffer buffer)
	  t)))))
