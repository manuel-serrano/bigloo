;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/lee/lee-hook.el                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 17 08:49:24 1998                          */
;*    Last change :  Wed Jan 23 17:33:46 2002 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The LEE hook that sets up all the LEE configuration.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'lee-hook)
(require 'font-lock)
(require 'lee-keymap)
(require 'lee-toolbar)
(require 'lee-flock)
(require 'ude-autoload)
(require 'ude-config)
(require 'ude-custom)

;*---------------------------------------------------------------------*/
;*    lee-docline ...                                                  */
;*---------------------------------------------------------------------*/
(defun lee-docline ()
  (interactive)
  (setq ude-info-file-list lee-info-file-list)
  (ude-info-docline (lee-font-lock-get-info-keywords)))

;*---------------------------------------------------------------------*/
;*    lee-eval-print-last-sexp ...                                     */
;*---------------------------------------------------------------------*/
(defun lee-eval-print-last-sexp ()
  (interactive)
  (let* ((value (eval-last-sexp nil))
	 (txt   (format "%S" value))
	 (standard-output (current-buffer)))
    (terpri)
    (let ((pos (point)))
      (insert txt)
      (put-text-properties pos (+ pos (length txt))
			   'face 'ude-font-lock-face-1))
    (terpri)
    (insert ":=> ")))

;*---------------------------------------------------------------------*/
;*    lee-interaction-mode-hook ...                                    */
;*---------------------------------------------------------------------*/
(defun lee-interaction-mode-hook ()
  (lee-font-lock-init)
  (local-unset-key "\C-j")
  (define-key (current-local-map) "\C-j" 'lee-eval-print-last-sexp)
  (let ((standard-output (current-buffer)))
    (goto-char (point-max))
    (if (not (and (>= (point) (+ (point-min) 4))
		  (save-excursion
		    (backward-char 4)
		    (search-forward ":=> " (point-max) t))))
	(insert ":=> "))))

;*---------------------------------------------------------------------*/
;*    lee-hook ...                                                     */
;*---------------------------------------------------------------------*/
(defun lee-hook ()
  ;; we setup the project root directory
  (ude-auto-set-root-directory)
  ;; extra Lisp identifier chars
  (setq ude-extra-identifier-chars "[-_]")
  ;; the key binding
  (lee-keymap-init)
  ;; parenthesis blinking init
  (ude-paren-init)
  ;; starting font-lock
  (if ude-font-lock-p
      (font-lock-mode t))
  ;; the toolbar
  (lee-toolbar-init)
  ;; the lisp interaction mode hook
  (add-hook 'lisp-interaction-mode-hook 'lee-interaction-mode-hook)
  ;; the lee hook
  (run-hooks 'lee-hook))
 
