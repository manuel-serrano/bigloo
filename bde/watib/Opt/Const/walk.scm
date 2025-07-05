;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Constant folding.

(module opt_const
   (from (ast_node "Ast/node.scm"))
   (export (const! f::func)))

(define (const! f::func)
   #f)

(define-generic (const-fold! i::instruction))
