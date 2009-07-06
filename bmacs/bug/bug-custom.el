;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/bug/bug-custom.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat May  4 14:42:59 2002                          */
;*    Last change :  Thu May 23 12:20:45 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    User customization                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The package                                                      */
;*---------------------------------------------------------------------*/
(provide 'bug-custom)

;; bug group
(defgroup bug nil
  "Debugging."
  :tag "Bug"
  :prefix "bug-"
  :group 'processes)

(defcustom bug-verbose-remote t
  "*Non-nil means that remote call are printed and registered in the history."
  :group 'bug
  :type 'boolean)

(defcustom bug-window-height 15
  "The number of lines of the bug console window."
  :group 'bug
  :type 'number)

(defcustom bug-stack-depth 10
  "The number of stack frame to be automatically displayed."
  :group 'bug
  :type 'number)

(defcustom bug-args-height 16
  "The default height of the args window."
  :group 'bug
  :type '(choice number nil))

(defcustom bug-display-source-in-frame-p t
  "*Non-nil means that source file are displayed in a separate frame."
  :group 'bug
  :type 'boolean)

;; raise frame displaying active source line
(defcustom bug-raise-active-source-frame-p t
  "Raise the frame displaying active source line."
  :group 'bug
  :type 'boolean)

;; bug show
(defcustom bug-show '(args stack)
  "*A list of elements that may be ARGS or STACK."
  :group 'bug
  :type '(repeat (choice 'args 'stack)))

;; window configuration (colors, margin size, ...)
(defcustom bug-margin-color "bisque"
  "The color for the left margin."
  :group 'bug
  :type 'string)

;; the face to display current source line
(defface bug-line-face
  (list (list '((class color))
	      (list ':bold t ':underline t))
	'(t (:bold nil)))
  "Bug arrow face"
  :group 'bug)

(defface bug-error-face
  (list (list '((class color))
	      (list ':foreground "red" ':bold t))
	'(t (:bold t)))
  "Bug error output face."
  :group 'bug)

(defface bug-current-frame-face
  (list (list '((class color))
	      (list ':foreground "red" ':bold t ':underline t))
	'(t (:underline t :bold t)))
  "Bug error output face."
  :group 'bug)

(defface bug-output-face
  (list (list '((class color))
	      (list ':foreground "orange" ':bold t))
	'(t (:bold t)))
  "Bug standard output face."
  :group 'bug)

(defface bug-footprint-face
  (list (list '((class color))
	      (list ':foreground "#5848b8" ':bold t))
	'(t (:bold t)))
  "Bug standard footprint face."
  :group 'bug)

