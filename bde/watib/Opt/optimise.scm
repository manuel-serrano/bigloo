;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Gluing of all the optimisation phases from internal representation to
;; internal representation.

(module opt_optimise
   (from (ast_node "Ast/node.scm"))
   (import (opt_testbr "Opt/TestBr/walk.scm")
           (opt_uncast "Opt/UnCast/walk.scm")
           (opt_unreachable "Opt/Unreachable/walk.scm")
           (opt_const "Opt/Const/walk.scm"))
   (import (misc_letif "Misc/let-if.scm"))
   (export (opt-file! p::prog nthreads::bint)))

(define (opt-file! p::prog nthreads::bint)
   (with-access::env (-> p env) (nfunc)
      (do ((i 0 (+fx i 1)))
          ((>=fx i nfunc))
         (let-if (f (vector-ref (-> p funcs) i))
            (testbr! f)
            (uncast! (-> p env) f)
            (unreachable! f)
            (const! f)))))
