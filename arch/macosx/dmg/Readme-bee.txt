Bee: the Bigloo mode for Emacs and other development tools.

The emacs mode is installed to /usr/local/share/emacs/bigloo.
In order to use it add the following lines to your ~/.emacs file and restart emacs.
;;-- Bee support --------------------------------------------------------------------
(setq load-path (cons (expand-file-name "/usr/local/share/emacs/bigloo/") load-path))
(if (locate-library "bmacs") (require 'bmacs))
(setq auto-mode-alist (cons '("\\.scm\\'" . bee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sch\\'" . bee-mode) auto-mode-alist))
;;-----------------------------------------------------------------------------------

