;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Conversion from Wasm/CFG to CFG/Wasm.

(module cfg_walk
   (library srfi1)

   (from (cfg_node "Opt/CFG/node.scm"))
   (from (ast_node "Ast/node.scm"))

   (import (cfg_order     "Opt/CFG/order.scm")
           (cfg_dominance "Opt/CFG/dominance.scm"))

   (export (func->cfg::cfg f::func)
           (cfg->wasm g::cfg))

   (static (wide-class node-tree::cfg-node
              tree::pair)))

;; We implement the algorithm from Ramsey, N. (2022). Beyond Relooper: recursive
;; translation of unstructured control flow to structured control flow
;; (functional pearl). Proceedings of the ACM on Programming Languages, 6(ICFP),
;; 1-22.

(define (back-inedge? src::node-tree dst::node-tree)
   (>=fx (-> src idx) (-> dst idx)))

(define (loop-header? n::node-tree)
   (any (lambda (src::node-tree) (back-inedge? src n)) (-> n preds)))

(define (merge-node? n::node-tree)
   (let* ((forward-pred? (lambda (src::node-tree) (not (back-inedge? src n))))
          (tl (find-tail forward-pred? (-> n preds))))
      (and tl (any forward-pred? (cdr tl)))))

(define (merge-head? t::pair)
   (merge-node? (car t)))

;; tail-rec later
(define (label-index::long l::long frame::pair)
   (match-case (car frame)
      ((block-followed-by ?l1) (if (=fx l1 l)
                                   0
                                   (+fx 1 (label-index l (cdr frame)))))
      ((loop-headed-by ?l1) (if (=fx l1 l)
                                   0
                                   (+fx 1 (label-index l (cdr frame)))))
      (else (+fx 1 (label-index l (cdr frame))))))

(define-generic (children-to-access j::jump tree::pair)
   (filter merge-head? (cdr tree)))

(define-method (children-to-access j::on-cast tree::pair)
   (filter (lambda (n) (or (eq? (-> j dst-cast) (car n)) (merge-head? n)))
           (cdr tree)))

(define-method (children-to-access j::switch tree::pair)
   (cdr tree))

(define (do-tree::sequence tree::pair ctx::pair-nil outtype::pair-nil)
   (with-access::node-tree (car tree) (idx intype end)
      (if (loop-header? (car tree))
          (let ((body (node-within (car tree) (filter merge-head? (cdr tree))
                                   (cons `(loop-headed-by ,idx) ctx) intype)))
             (instruction->sequence (instantiate::loop
                                     (opcode 'loop)
                                     (intype intype)
                                     (body body)
                                     (parent (instantiate::modulefield))
                                     ;; what should really be outtype
                                     (outtype outtype))))
          (let ((body (node-within (car tree) (children-to-access end tree)
                                   ctx outtype)))
             (instantiate::sequence
              (opcode 'nop)
              (body body)
              (intype intype)
              (outtype outtype) ;; to fix
              (parent (instantiate::modulefield)))))))

(define (instruction->sequence::sequence i::instruction)
   (duplicate::sequence i (body (list i)) (opcode 'nop)))

(define (do-branch::sequence src::node-tree dst::node-tree ctx::pair-nil
                             outtype::pair-nil)
   (cond ((merge-node? dst)
          (print ctx)
          (print (-> dst idx))
          (print (label-index (-> dst idx) ctx))
          (instantiate::sequence
           (opcode 'nop)
           (intype (-> src outtype)) ;; to fix
           (outtype outtype)
           (parent (instantiate::modulefield))
           (body
            (list (instantiate::one-arg
                   (opcode 'br)
                   (intype '())
                   (outtype '(poly))
                   (parent (instantiate::modulefield))
                   (x (instantiate::labelidxp
                       (idx (label-index (-> dst idx) ctx))
                       (type (-> dst intype)))))))))

         ((back-inedge? src dst)
          (instantiate::sequence
           (opcode 'nop)
           (intype (-> src outtype)) ;; to fix
           (outtype outtype)
           (parent (instantiate::modulefield))
           (body
            (list (instantiate::one-arg
                   (opcode 'br)
                   (intype '())
                   (outtype '(poly))
                   (parent (instantiate::modulefield))
                   (x (instantiate::labelidxp
                       (idx (label-index (-> dst idx) ctx))
                       (type (-> dst intype)))))))))

         (else (with-access::node-tree dst (tree)
                  (do-tree tree ctx outtype)))))

(define-generic (branch-after-body::pair-nil j::jump src::node-tree
                                             ctx::pair-nil outtype::pair-nil))

(define-method (branch-after-body::pair-nil j::unconditional src::node-tree
                                            ctx::pair-nil outtype::pair-nil)
   (with-access::sequence (do-branch src (-> j dst) ctx outtype) (body)
      body))

(define-method (branch-after-body::pair-nil j::conditional src::node-tree
                                            ctx::pair-nil outtype::pair-nil)
   (list (instantiate::if-else
          (intype (-> src outtype))
          (outtype outtype)
          (parent (instantiate::modulefield))
          (opcode 'if)
          (then
           (do-branch src (-> j dst-true) (cons 'if-then-else ctx) outtype))
          (else
           (do-branch src (-> j dst-false) (cons 'if-then-else ctx) outtype)))))

(define-method (branch-after-body::pair-nil j::terminal src::node-tree
                                            ctx::pair-nil outtype::pair-nil)
   (list (-> j i)))

(define-method (branch-after-body::pair-nil j::on-cast src::node-tree
                                            ctx::pair-nil outtype::pair-nil)
   (with-access::sequence (do-branch src (-> j dst-cast-fail) ctx outtype)
                          (body)
      (with-access::node-tree (-> j dst-cast) (intype idx)
         (cons
          (instantiate::three-args
           (opcode 'br_on_cast)
           (intype `(,(-> j rt-src)))
           (outtype `(,(-> j rt-src))) ;; to fix with diff
           (parent (instantiate::modulefield)) ;; to fix
           (x (instantiate::labelidxp
               (idx (label-index idx ctx))
               (type intype)))
           (y (instantiate::typep
               (type (-> j rt-src))))
           (z (instantiate::typep
               (type (-> j rt-dst)))))
          body))))

(define-method (branch-after-body::pair-nil j::switch src::node-tree
                                            ctx::pair-nil outtype::pair-nil)
   (list (instantiate::br_table
          (opcode 'br_table)
          (labels (map (lambda (n::node-tree) (label-index (-> n idx) ctx))
                       (-> j dsts)))
          (intype '())
          (outtype '(poly))
          (parent (instantiate::modulefield)))))

(define (node-within n::node-tree trees::pair-nil ctx::pair-nil
                     outtype::pair-nil)
   (if (null? trees)
       (append
        (-> n body)
        (branch-after-body (-> n end) n ctx outtype))
       (with-access::sequence (do-tree (car trees) ctx outtype) (body intype)
          (cons
           (instantiate::block
            (opcode 'block)
            (intype (-> n intype)) ;; to fix
            (outtype intype)
            (parent (instantiate::modulefield))
            (body
             (node-within
              n
              (cdr trees)
              (cons `(block-followed-by
                      ,(with-access::node-tree (car (car trees)) (idx) idx))
                    ctx)
              intype)))
           body))))

(define (cfg->wasm g::cfg)
   (define tree-vect (make-vector (-> g size) '()))
   (define doms (dominance g))

   (for-each (lambda (n::cfg-node)
                (unless (eq? n (-> g entry))
                   (let* ((p::cfg-node (vector-ref doms (-fx 0 (-> n idx)))))
                      (vector-set! tree-vect (-fx 0 (-> p idx))
                                   (cons n (vector-ref tree-vect
                                                       (-fx 0 (-> p idx))))))))
             (-> g rpostorder))

   (define (build-tree n::cfg-node)
      (define (node<? x::cfg-node y::cfg-node)
         (>fx (-> x idx) (-> y idx)))
      (let ((t (cons
                n
                (map build-tree
                     (sort node<? (vector-ref tree-vect (-fx 0 (-> n idx))))))))
        (widen!::node-tree n (tree t))
        t))

   (do-tree (build-tree (-> g entry)) '()
            (with-access::func (-> g func) (type) (cadr type))))

(define (build-node l::pair-nil seq-intype::pair-nil st::pair-nil
                    body::pair-nil next::cfg-node labs::pair-nil)
   (define (end-current-block end::jump)
      (if (and (null? body) (isa? end unconditional))
          (with-access::unconditional end (dst) dst)
          (instantiate::cfg-node
           (intype seq-intype)
           (outtype (reverse (remove-top-outtype end st)))
           (body (reverse body))
           (end end))))

   (cond
    ((null? l)
     (if (null? body) next
         (end-current-block (instantiate::unconditional (dst next)))))

    ((isa? (car l) if-then)
     (with-access::if-then (car l) (then intype outtype)
        (let* ((new-st (append (reverse outtype) (drop st (length intype))))
               (n::cfg-node (build-node (cdr l) new-st new-st '() next
                                        labs))
               ;; (cdr intype) to remove the condition on top of the stack
               (bintype (cdr (reverse intype))))
          (end-current-block
           (instantiate::conditional
            (dst-true
             (build-node (with-access::sequence then (body) body)
                         bintype
                         bintype
                         '() n (cons n labs)))
            (dst-false
             (if (isa? (car l) if-else)
                 (with-access::if-else (car l) (else)
                    (build-node (with-access::sequence else (body) body)
                                bintype
                                bintype
                                '() n
                                (cons n labs)))
                 n)))))))

    ;; (block instr*) instr* could be interpreted as instr* end instr* maybe
    ;; to be able to attach the previous instructions to the body of the
    ;; block

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
               (n::cfg-node (build-node (cdr l) new-st new-st '() next labs))
               ;; created to break cyclicity
               (dummy-node::cfg-node (make-dummy-node))
               (loop-head::cfg-node
                (build-node loop-body intype intype '() n
                            (cons dummy-node labs))))

          (set! (-> dummy-node body) (-> loop-head body))
          (set! (-> dummy-node intype) (-> loop-head intype))
          (set! (-> dummy-node outtype) (-> loop-head outtype))
          (set! (-> dummy-node end) (-> loop-head end))

          (end-current-block
           (instantiate::unconditional (dst dummy-node))))))

    ((isa? (car l) try_table)
     (error "watib" "try_table not supported yet in CFG conversion" (car l)))

    ((isa? (car l) sequence)
     (with-access::sequence (car l) (body)
        (build-node (append body (cdr l)) seq-intype st body next labs)))

    (else
     (with-access::instruction (car l) (opcode intype outtype)
        (match-case opcode
           (br
            (with-access::one-arg (car l) (x)
               (with-access::labelidxp x (idx)
                  (instantiate::cfg-node
                   (intype seq-intype)
                   (outtype (reverse st))
                   (body (reverse body))
                   (end (instantiate::unconditional
                         (dst (list-ref labs idx))))))))

           (br_if
            (with-access::one-arg (car l) (x)
               (with-access::labelidxp x (idx)
                  (instantiate::cfg-node
                   (intype seq-intype)
                   (outtype (reverse st))
                   (body (reverse body))
                   (end (instantiate::conditional
                         (dst-true (list-ref labs idx))
                         (dst-false (build-node (cdr l) (reverse (cdr st))
                                                (cdr st)
                                                '() next labs))))))))

           ((or return return_call)
            (end-current-block
             (instantiate::terminal (i (car l)))))

           (br_on_cast
            (with-access::three-args (car l) (x y z)
               (with-access::labelidxp x (idx)
                  (end-current-block (instantiate::on-cast
                                      (dst-cast-fail
                                       (build-node (cdr l) st st '()
                                                   next labs))
                                      (rt-src y)
                                      (rt-dst z)
                                      (dst-cast (list-ref labs idx)))))))

           (else
            (let ((new-st (append (reverse outtype)
                                  (drop st (length intype)))))
               (build-node (cdr l) seq-intype new-st (cons (car l) body)
                           next labs))))))))

(define (func->cfg f::func)
   (let* ((ret-node (instantiate::cfg-node
                     (intype (cadr (-> f type)))
                     (outtype (cadr (-> f type)))
                     (body '())
                     (end (instantiate::terminal
                           (i (instantiate::instruction
                               (opcode 'return)
                               (intype (cadr (-> f type)))
                               (outtype '())
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
         (rpostorder rpostorder)
         (func f)))))
