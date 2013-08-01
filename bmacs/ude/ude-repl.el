;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-repl.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon May 25 16:31:35 1998                          */
;*    Last change :  Sat Jan 26 11:05:37 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This file implements a REPL process embedded in emacs.           */
;*    To fetch region in the code it uses buffer local variables that  */
;*    are defined inside UDE-CONFIG.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-repl)
(require 'ude-config)
(require 'ude-icon)
(require (if (featurep 'xemacs) 'bmacs-xemacs 'bmacs-gnu-emacs))
(require 'ude-autoload)
(require 'comint)

;*---------------------------------------------------------------------*/
;*    ude-repl-buffer ...                                              */
;*---------------------------------------------------------------------*/
(defvar ude-repl-buffer nil
  "The buffer interacting with `ude-repl-comint-process'.")
(defvar ude-repl-comint-process nil
  "The comint process running a Bigloo interpreter.")
(defcustom ude-repl-args "-i"
  "Additional arguments to the Bigloo interpreter."
  :group 'bee
  :type 'string)

;*---------------------------------------------------------------------*/
;*    ude-repl ...                                                     */
;*---------------------------------------------------------------------*/
(defun ude-repl ()
  (setq ude-repl-buffer (make-comint ude-repl ude-repl nil ude-repl-args))
  (setq ude-repl-comint-process (get-buffer-process ude-repl-buffer))

  (set-process-sentinel ude-repl-comint-process
			'ude-repl-sentinel)
  (set-process-filter ude-repl-comint-process
		      (function ude-repl-output-from-process))
  (save-excursion
    (set-buffer ude-repl-buffer)
    (setq comint-prompt-regexp ude-repl-prompt-regexp)
    (setq comint-scroll-show-maximum-output 0.4)
    (setq comint-scroll-to-bottom-on-output t)
    (setq comint-delimiter-argument-list '(? )))

  (if (eq (car ude-place-to-start-repl) 'other-window)
      (switch-to-buffer-other-window ude-repl-buffer)
    (switch-to-buffer-other-frame ude-repl-buffer))

  (ude-repl-init-toolbar)
  (run-hooks 'ude-repl-hooks))

;*---------------------------------------------------------------------*/
;*    ude-repl-output-from-process ...                                 */
;*---------------------------------------------------------------------*/
(defun ude-repl-output-from-process (process string)
  "This function is invoked each time the Bigloo process generates
output. It places the output in the buffer, and makes sure it is
displayed."
  (comint-output-filter process string)
  (if (eq (car ude-place-to-start-repl) 'other-window)
      (display-buffer ude-repl-buffer)))
 
;* (defun ude-repl.old ()                                              */
;*   (let ((bufname (concat "*" ude-repl-buffer-name "*")))            */
;*     (switch-to-buffer-other-frame bufname)                          */
;*     (comint-run ude-repl)                                           */
;*     (setq ude-repl-buffer (current-buffer))                         */
;*     (make-variable-buffer-local 'comint-prompt-regexp)              */
;*     (setq comint-prompt-regexp ude-repl-prompt)                     */
;*     (process-kill-without-query (get-buffer-process ude-repl-buffer)) */
;*     (set-process-sentinel (get-buffer-process ude-repl-buffer)      */
;* 			  'ude-repl-sentinel)                          */
;*     (ude-repl-init-toolbar)                                         */
;*     (run-hooks 'ude-repl-hooks)))                                   */

;*---------------------------------------------------------------------*/
;*    ude-repl-other-frame ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-repl-other-frame ()
  (interactive)
  (if (and (bufferp ude-repl-buffer) (buffer-name ude-repl-buffer))
      (display-buffer ude-repl-buffer))
  (ude-repl))

;*---------------------------------------------------------------------*/
;*    ude-repl-sentinel ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is called when the ude running process changes     */
;*    of state. This means that the process has been killed or         */
;*    stopped or anything else like that.                              */
;*---------------------------------------------------------------------*/
(defun ude-repl-sentinel (proc msg)
  (cond
   ((null (buffer-name (process-buffer proc)))
    ;; the buffer has uden killed. We stop displaying
    ;; arrow in the source files.
    (set-process-buffer proc nil))
   ((memq (process-status proc) '(signal exit))
    (let ((window (get-buffer-window ude-repl-buffer t)))
      (if (one-window-p window)
	  (let ((frame (window-frame window)))
	    (delete-frame frame))))
    (kill-buffer ude-repl-buffer)))
  (setq ude-repl-buffer nil))

;*---------------------------------------------------------------------*/
;*    ude-repl-send-region ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-repl-send-region (beg end)
  (interactive "r")
  (save-excursion
    (if (not (bufferp ude-repl-buffer))
        (ude-repl)))
  (let ((command (buffer-substring beg end))
	(proc    (get-buffer-process ude-repl-buffer)))
    (comint-simple-send proc command)))

;*---------------------------------------------------------------------*/
;*    ude-repl-find ...                                                */
;*---------------------------------------------------------------------*/
(defun ude-repl-find (var)
  "Find a variable definition."
  (interactive (ude-interactive-ident (point) "variable: "))
  (let ((search-str (format "(define[ \t\n]+[(]?%s" (regexp-quote var))))
    (if (not (re-search-backward search-str (point-min) t))
	(ude-error "Can't find REPL variable `%s'" var))))

;*---------------------------------------------------------------------*/
;*    ude-repl-quit ...                                                */
;*---------------------------------------------------------------------*/
(defun ude-repl-quit ()
  "Kill the comint subjob and repl buffer"
  (interactive)
  (condition-case ()
      (comint-kill-subjob)
    (error
     (if (bufferp ude-repl-buffer)
	 (ude-delete-buffer-window-frame ude-repl-buffer)))))

;*---------------------------------------------------------------------*/
;*    ude-repl-send-buffer ...                                         */
;*---------------------------------------------------------------------*/
(defun ude-repl-send-buffer ()
  "Send the whole buffer to the inferior Bigloo process."
  (interactive)
  (ude-repl-send-region (point-min) (point-max)))

;*---------------------------------------------------------------------*/
;*    ude-repl-toolbar ...                                             */
;*---------------------------------------------------------------------*/
(defvar ude-repl-toolbar 
  `(;; the quit button
    (,ude-quit-icon ude-repl-quit "Quit Repl")
    --
    
    ;; the tag button
    (,ude-tag-icon ude-repl-find "Find definition")
    --
    
    ;; the next button
    (,ude-repl-next-icon comint-next-input "Next Input")
    --
    
    ;; prev error
    (,ude-repl-prev-icon comint-previous-input "Previous Input")
    --
    
    ;; flushing right
    -->
    --
    ;; the help action
    (,ude-help-icon describe-mode "Help")
    ;; the info button
    (,ude-info-icon ude-docline "The online documentation for Bee")))

;*---------------------------------------------------------------------*/
;*    ude-repl-init-toolbar ...                                        */
;*    -------------------------------------------------------------    */
;*    This hook simply set the UDE repl toolbar for the buffer         */
;*---------------------------------------------------------------------*/
(defun ude-repl-init-toolbar ()
  (ude-toolbar-set ude-repl-toolbar))
  
