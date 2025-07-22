;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Conversion from Wasm/CFG to CFG/Wasm.

(module cfg_walk
   (library srfi1)

   (from (cfg_node "Opt/CFG/node.scm"))
   (from (ast_node "Ast/node.scm"))

   (import (cfg_order "Opt/CFG/order.scm"))

   (export (func->cfg::cfg f::func)))

;; We implement the algorithm from Ramsey, N. (2022). Beyond Relooper: recursive
;; translation of unstructured control flow to structured control flow
;; (functional pearl). Proceedings of the ACM on Programming Languages, 6(ICFP),
;; 1-22.

(define (back-inedge? src::cfg-node dst::cfg-node)
   (>=fx (-> src idx) (-> dst idx)))

(define (loop-header? n::cfg-node)
   (any (lambda (src::cfg-node) (back-inedge? src n)) (-> n preds)))

(define (merge-node? n::cfg-node)
   (let* ((forward-pred? (lambda (src::cfg-node) (back-inedge? n src)))
          (tl (find-tail forward-pred? (-> n preds))))
      (and tl (any forward-pred? (cdr tl)))))

(define (do-tree entry::cfg-node doms::vector order::pair-nil ctx::pair-nil)
   #f)

(define (do-branch src dst ctx::pair-nil)
   #f)

(define (node-within n::cfg-node trees::pair-nil ctx::pair-nil)
   #f)

(define (cfg->wasm entry::cfg-node doms::vector order::pair-nil n::long)
   (define tree-vect (make-vector n '()))
   (for-each (lambda (n::cfg-node)
               (let* ((p::cfg-node (vector-ref doms (-fx 0 (-> n idx)))))
                  (vector-set! tree-vect (-> p idx)
                               (cons n (vector-ref tree-vect
                                                   (-fx 0 (-> p idx)))))))
             order)

   (define (build-tree n::cfg-node)
      (cons n (map build-tree (vector-ref tree-vect (-fx 0 (-> n idx))))))

   (do-tree doms order (build-tree entry) '()))

(define (func->cfg f::func)
   (define (build-node l::pair-nil seq-intype::pair-nil st::pair-nil
                       body::pair-nil next::cfg-node labs::pair-nil)
      (define (end-current-block end::jump)
         (instantiate::cfg-node
             (intype seq-intype)
             (outtype st)
             (body (reverse body))
             (end end)))
      (cond
       ((null? l)
        (if (null? body) next
            (end-current-block (instantiate::unconditional (dst next)))))

       ((isa? (car l) if-then)
        (with-access::if-then (car l) (then intype outtype)
           (let* ((new-st (append (reverse outtype) (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs)))
              (end-current-block
               (instantiate::conditional
                (dst-true
                 (build-node (with-access::sequence then (body) body)
                             intype intype '() n (cons n labs)))
                (dst-false
                 (if (isa? (car l) if-else)
                     (with-access::if-else (car l) (else)
                        (build-node (with-access::sequence else (body) body)
                                    intype intype '() n (cons n labs)))
                     n)))))))

       ((isa? (car l) block)
        (with-access::block (car l) (intype outtype (block-body body))
           (let* ((new-st (append (reverse outtype) (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs)))
              (end-current-block
               (instantiate::unconditional
                (dst (build-node block-body intype intype '() n
                                 (cons n labs))))))))

       ((isa? (car l) loop)
        (with-access::loop (car l) (intype outtype (loop-body body))
           (let* ((new-st (append (reverse outtype) (drop st (length intype))))
                  (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                           labs))
                  ;; created to break cyclicity
                  (dummy-node::cfg-node (instantiate::cfg-node
                                         (body '())
                                         (intype '())
                                         (outtype '())
                                         (end (instantiate::unconditional
                                               (dst n)))))
                  (loop-head::cfg-node (build-node loop-body intype intype '() n
                                                   (cons dummy-node labs))))

              (set! (-> dummy-node body) (-> loop-head body))
              (set! (-> dummy-node intype) (-> loop-head intype))
              (set! (-> dummy-node outtype) (-> loop-head outtype))
              (set! (-> dummy-node end) (-> loop-head end))

              (end-current-block
                (instantiate::unconditional (dst dummy-node))))))

       ((isa? (car l) try_table)
        (error "watib" "try_table not supported yet in CFG conversion" (car l)))

       (else
        (with-access::instruction (car l) (opcode intype outtype)
           (match-case opcode
              (br
               (with-access::one-arg (car l) (x)
                  (with-access::labelidxp x (type idx)
                     (instantiate::cfg-node
                      (intype seq-intype)
                      (outtype type)
                      (body (reverse body))
                      (end (instantiate::unconditional
                            (dst (list-ref labs idx))))))))
              (else
               (let ((new-st (append (reverse outtype)
                                     (drop st (length intype)))))
                  (build-node (cdr l) new-st new-st (cons (car l) body)
                              next labs))))))))

   (let* ((ret-node (instantiate::cfg-node
                     (intype (cadr (-> f type)))
                     (outtype '())
                     (body '())
                     (end (instantiate::terminal
                           (i (instantiate::instruction
                               (opcode 'return)
                               (outtype '())
                               (intype (cadr (-> f type)))
                               (parent f)))))))
          (entry (build-node
                 (with-access::sequence (-> f body) (body) body)
                 '()
                 '()
                 '()
                 ret-node
                 (list ret-node))))
     (multiple-value-bind (-size rpostorder) (reverse-postorder! entry)
        (instantiate::cfg
         (entry entry)
         (size (-fx 0 -size))
         (rpostorder rpostorder)))))
