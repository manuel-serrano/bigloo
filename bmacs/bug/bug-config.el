;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-config.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:40:26 2002                          */
;*    Last change :  Fri May 24 14:18:26 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The Bigloo debugger Emacs mode configuration.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-config)
(require 'bug-custom)

;*---------------------------------------------------------------------*/
;*    bug-version                                                      */
;*---------------------------------------------------------------------*/
(defconst bug-version "0.0a"
  "*The bug Emacs mode version.")

;*---------------------------------------------------------------------*/
;*    Regular expressions                                              */
;*---------------------------------------------------------------------*/
;; prompt
(defvar bug-prompt-regexp "(bugloo) "
  "The bug prompt.")

(defvar bug-prompt-eol-regexp (concat bug-prompt-regexp "$")
  "The regexp matching bug prompt on a line.
That variable should be set by the means of :set property of customization
of the BUG-PROMPT-REGEXP.")

;; line information
(defconst bug-linespec-regexp "\\(\\S-+\\):\\([0-9]+\\)$")

;; breakpoint information
(defconst bug-breakpoint-regexp "\\([0-9]+\\):\\(\\S-+\\):\\(\\S-+\\):\\([0-9]+\\):\\(\\S-+\\)$")

;; breakpoint reached information
(defconst bug-bpspec-regexp "breakpoint \\([0-9]+\\)$")

;; footprint reached information
(defconst bug-fpspec-regexp "footprint \\([0-9]+\\)$")

;; stack frame number
(defconst bug-framespec-regexp "#\\([0-9]+\\)")

;*---------------------------------------------------------------------*/
;*    Bugloo control                                                   */
;*---------------------------------------------------------------------*/
;; debugger binary file
(defvar bug-binary "bugloo"
  "*The name of the binary file that is the debugger.")

;; the debugger emacs option
(defvar bug-emacs-option "--emacs --scheme"
  "The option to be sent to the debugger when running with emacs.")

;; the connection timeout
(defvar bug-sec-timeout 0
  "The number of seconds to timeout when talking to the debugger.")
(defvar bug-msec-timeout 10
  "The number of milli-second to timeout when talking to the debugger.")
(defvar bug-hook-timeout 100
  "The number of seconds to timeout when waiting for a stack.")

;*---------------------------------------------------------------------*/
;*    bug-markups ...                                                  */
;*---------------------------------------------------------------------*/
(defvar bug-markups
  '(("command" . bug-filter-command)
    ("prompt" . bug-filter-prompt)
    ("error" . bug-filter-error)
    ("output" . bug-filter-output)
    ("location" . bug-filter-location)
    ("bp" . bug-filter-bp)
    ("fp" . bug-filter-fp)
    ("breakpoint" . bug-filter-breakpoint)
    ("footprint" . bug-filter-footprint)
    ("ident" . bug-filter-ident)
    ("type" . bug-filter-type)
    ("line" . bug-filter-line)
    ("framenum" . bug-filter-frame)
    ("framecur" . bug-filter-framecur)
    ("step" . bug-filter-default)))

;*---------------------------------------------------------------------*/
;*    Comint global control.                                           */
;*---------------------------------------------------------------------*/
(defvar bug-comint-buffer nil)

;*---------------------------------------------------------------------*/
;*    bug-minibuffer-local-map ...                                     */
;*---------------------------------------------------------------------*/
(defvar bug-minibuffer-local-map nil
  "Keymap for minibuffer prompting of bugloo startup command.")

;*---------------------------------------------------------------------*/
;*    command                                                          */
;*---------------------------------------------------------------------*/
(defun bug-run-command () "(begin (rerun) (resume))")
(defun bug-step-command () "(step)")
(defun bug-cont-command () "(resume)")
(defun bug-next-command () "(step over)")
(defun bug-finish-command () "(step out)")
(defun bug-break-command (c l f) (format "(bp add %s %S %S)" c l f))
(defun bug-delete-break-command (n) (format "(bp del %d)" n))
(defun bug-disable-break-command (n) (format "(bp disable %d)" n))
(defun bug-enable-break-command (n) (format "(bp enable %d)" n))
(defun bug-tbreak-command (c l f) (format "(bp add %s %S %S 1)" c l f))
(defun bug-footprint-command (c l f) (format "(fp add %s %S %S)" c l f))
(defun bug-delete-footprint-command (n) (format "(fp del %d)" n))
(defun bug-disable-footprint-command (n) (format "(bp disable %d)" n))
(defun bug-enable-footprint-command (n) (format "(bp enable %d)" n))
(defun bug-until-command (c l f) (format "(begin (bp add %s %S %S 1) (resume))" c l f))
(defun bug-frame-command (n) (format "(begin (history freeze all) (frame %d) (info line %d) (history thaw))" n n))
(defun bug-stack-hook-command (n) (format "(info stack %d)" n))
(defun bug-args-hook-command () "(info args)")
(defun bug-info-line-command (n) (format "(info line %d)" n))

;*---------------------------------------------------------------------*/
;*    bug-wrapper-caller                                               */
;*    -------------------------------------------------------------    */
;*    A possible function that is in charge to call the debugger.      */
;*---------------------------------------------------------------------*/
(defvar bug-wrapper-caller nil)

;*---------------------------------------------------------------------*/
;*    bug-custom-filter ...                                            */
;*    -------------------------------------------------------------    */
;*    A possible function in charge of filtering process outputs.      */
;*---------------------------------------------------------------------*/
(defvar bug-custom-filter nil)

