;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Gluing of all the optimisation phases from internal representation to
;; internal representation.

(module opt_optimise
   (from (ast_node "Ast/node.scm"))
   (cond-expand
      ((and multijob (library pthread)) (library pthread)))
   (import (opt_testbr "Opt/TestBr/walk.scm")
           (opt_uncast "Opt/UnCast/walk.scm")
           (opt_unreachable "Opt/Unreachable/walk.scm")
           (opt_const "Opt/Const/walk.scm")
           (opt_puredrop "Opt/PureDrop/walk.scm")
           (opt_copyprop "Opt/CopyProp/walk.scm"))
   (import (misc_letif "Misc/let-if.scm"))
   (export (opt-file! p::prog nthreads::bint)))

(define (opt-func! f::func p::prog)
   (testbr! f)
   (copyprop! f)
   (uncast! (-> p env) f)
   (unreachable! f)
   (const! f)
   (puredrop! f))

(cond-expand
 ((and multijob (library pthread))
  (define (multijob p::prog nthreads::bint)
    (define (opt! a::long b::long)
       (with-access::env (-> p env) (nfunc)
          (do ((i b (+fx i a)))
              ((>=fx i nfunc))
             (let-if (f (vector-ref (-> p funcs) i))
                (opt-func! f p)))))

    (let ((ts (list-tabulate
               nthreads
               (lambda (i)
                 (instantiate::pthread
                  (body (lambda () (opt! nthreads i))))))))
       (map thread-start-joinable! ts)
       (map thread-join! ts)))))

(define (opt-file! p::prog nthreads::bint)
   (opt-func! (vector-ref (-> p funcs) 690) p)
   ;; (cond-expand
   ;;  ((and multijob (library pthread))
   ;;   (multijob prog nthreads))
   ;;  (else (with-access::env (-> p env) (nfunc)
   ;;    (do ((i 0 (+fx i 1)))
   ;;        ((>=fx i nfunc))
   ;;       (let-if (f (vector-ref (-> p funcs) i))
   ;;          (opt-func! f p))))))



   )
