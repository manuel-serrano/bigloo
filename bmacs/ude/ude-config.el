;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bmacs/ude/ude-config.el              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  8 07:29:03 1998                          */
;*    Last change :  Fri Nov  5 17:51:13 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `Unix Development Environment' configuration. This file      */
;*    contains nearly all variables that are mode local.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(provide 'ude-config)
(require 'ude-autoload)

;; ude version
(defconst ude-version "0.4"
  "*The Ude version.")

;; ude author
(defconst ude-author "Manuel Serrano (c)")
(defconst ude-url "Manuel.Serrano@inria.fr\nhttp://www-sop.inria.fr/members/Manuel.Serrano")

;; root directory
(defvar ude-root-directory default-directory
  "*A string, that is the project root directory.")
(make-variable-buffer-local 'ude-root-directory)

(defvar ude-root-directory-hook '()
  "The functions to be called when the ude-root-directory changes.")
(make-variable-buffer-local 'ude-root-directory-hook)

;; compile command
(defvar ude-compile-command "make")
(make-variable-buffer-local 'ude-compile-command)

;; jcompile-command
(defvar ude-jcompile-command (if (stringp (getenv "UDEMAKE"))
				(getenv "UDEMAKE")
				"make -j2"))
(make-variable-buffer-local 'ude-jcompile-command)

;; compilation mode
(defvar ude-compile-mode 'devel
  "A symbol that is either devel or final.")
(make-variable-buffer-local 'ude-compile-mode)

;; profile processing
(defvar ude-prof "gprof")
(make-variable-buffer-local 'ude-prof)

(defvar ude-profile-success-hook #'(lambda (proc buffer) buffer)
  "A function to be called if a profile execution succeeds.")
(make-variable-buffer-local 'ude-profile-success-hook)

;; mode menu compile
(defvar ude-mode-menu-compile nil
  "Nil or a procedure that processes a mode specific compilation.")
(make-variable-buffer-local 'ude-mode-menu-compile)

;; the tools that builds a Makefile
(defvar ude-makemake "makemake"
  "*A string, the name of the binary file that generates Makefile.")
(make-variable-buffer-local 'ude-makemake)

;; identifiers
(defvar ude-extra-identifier-chars nil
  "*A string, a regexp that matches legal identifier chars that are in \W.")
(make-variable-buffer-local 'ude-extra-identifier-chars)

;; repl
(defvar ude-repl "bigloo"
  "*A string, the name of the binary file that implements REPL.")
(make-variable-buffer-local 'ude-repl)
  
(defvar ude-repl-buffer-name "bee"
  "*A string, the name of the REPL buffer.")
(make-variable-buffer-local 'ude-repl-buffer-name)

(defvar ude-repl-prompt-regexp "^[0-9]*:=>"
  "*A string, the prompt for REPL.")

(defcustom ude-place-to-start-repl (list 'other-frame)
  "Whether to start the REPL buffer in another frame or in another window."
  :group 'ude
  :type '(list
          (radio-button-choice
           (const :tag "Other Window" other-window)
           (const :tag "Other Frame" other-frame))))

;; docline
(defvar ude-docline (lambda () (interactive) '())
  "*A function that is called to fetch an online documentation (e.g. repl).")
(make-variable-buffer-local 'ude-docline)

(defvar ude-info-region (lambda (min start) '())
  "*A function that is called when docline is request on a region.")
(make-variable-buffer-local 'ude-info-region)

(defvar ude-info-fontify t
  "*This variable implement a kind of local lock to prevent fontification
will looking for a string in a list of info files.")

(defvar ude-info-file-list '()
  "List of info files scanned when searching a function definition.")

(defvar ude-manual-page-number 1
  "The man section number.")
(make-variable-buffer-local 'ude-manual-page-number)

;; balloon configuration
(defcustom ude-balloon-timeout 1000
  "Display balloon after this many milliseconds of mouse inactivity."
  :group 'ude
  :type 'number)

(defcustom ude-balloon-width 80
  "The maximum number of chars displayed in a balloon window."
  :group 'ude
  :type 'number)

