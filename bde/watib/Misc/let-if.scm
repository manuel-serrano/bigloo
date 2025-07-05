;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; A small macro allowing binding and testing at the same time.

(module misc_letif
   (export (macro let-if bind . body)))

(define-macro (let-if bind . body)
   `(let ((,(car bind) ,(cadr bind)))
       (when ,(car bind)
         ,@body)))
