;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Computation of dominance trees for CFGs.
;;
;; We follow the algorithm described in Cooper, K. D., Harvey, T. J., & Kennedy,
;; K. (2001). A simple, fast dominance algorithm. Software Practice &
;; Experience, 4(1-10), 1-8.

(module cfg_dominance
   (library srfi1)
   (from (cfg_node "Opt/CFG/node.scm"))
   (import (cfg_order "Opt/CFG/order.scm"))

   (export (dominance entry::cfg-node order::pair-nil n::long)))

(define (dominance entry::cfg-node order::pair-nil n::long)
   (let ((doms (make-vector n #f)))
      (vector-set! doms (-fx 0 (-> entry idx)) entry)

      (define (intersect n1::cfg-node n2::cfg-node)
        (cond
         ((=fx (-> n1 idx) (-> n2 idx)) (-> n1 idx))
         ((<fx (-> n1 idx) (-> n2 idx)) (intersect (dom n1) n2))
         (else (intersect n1 (dom n2)))))

      (define (dom n::cfg-node)
         (vector-ref doms (-fx 0 (-> n idx))))

      (define (step n::cfg-node)
         (fold (lambda (new_idom p)
                 (if (dom p)
                     (if new_idom
                         (intersect new_idom p)
                         p)
                     new_idom)) #f (-> n preds)))

      (define (loop)
         (let ((l (map step order))
               (changed #f))
            (for-each (lambda (n::cfg-node p::cfg-node)
                        (unless (=fx (-> p idx)
                                     (with-access::cfg-node (dom n) (idx) idx))
                           (set! changed #t)
                           (vector-set! doms (-fx 0 (-> entry idx)) p)))
                      order l)
            (if changed (loop))))
      (loop)
      doms))
