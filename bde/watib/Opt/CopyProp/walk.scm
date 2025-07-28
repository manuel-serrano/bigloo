;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Copy propagation.
;;
;; The copy propagation is mostly local for now. When entering a loop we forget
;; everything we know because we could come from anywhere in the loop.
;;
;; We need to record the initialisation state of the local variables to treat
;; examples such as:
;; (block
;;    (i32.const 0)
;;    (local.tee $x)
;;    (local.set $y))
;; (local.get $y)
;; Here $x can't replace the last $y even though $y <- $x dominates the last
;; instruction.

(module opt_copyprop
   (static (class acp-state::object
              (nlabel::bint (default 1))
              (acps::vector (default (make-vector 10000 #f)))
              (init-var::pair (default (list (make-vector 10000 #f))))))
   (from (ast_node "Ast/node.scm"))
   (include "Misc/read-table.sch")
   (import (misc_letif "Misc/let-if.scm"))
   (export (copyprop! f::func)))

(define (copyprop! f::func)
   (local-copyprop! (-> f body) (make-vector 10000 #f) (instantiate::acp-state)))

;; the ACP (available copy) table of Muchnick 97, section 12.5 is represented,
;; for the current block as a vector mapping each variable to its left-hand side
;; expression represented by a pair of an index and a type
;;
;; it could be faster to also keep a way, for each variable a list of variables
;; for which it is a left-hand side

(define (update-lab-acp! l::labelidxp cur-acp::vector lab-acp::acp-state)
   (let ((x (- (-> lab-acp nlabel) (-> l idx) 1)))
      (vector-set! (-> lab-acp acps) x
                   (cons (vector-copy cur-acp)
                         (vector-ref (-> lab-acp acps) x)))))

(define (enter-frame lab-acp::acp-state)
   (with-access::acp-state lab-acp (nlabel acps init-var)
      (vector-set! acps nlabel '())
      (set! init-var (cons (vector-copy (car init-var)) init-var))
      (set! nlabel (+fx 1 nlabel))))

(define (exit-frame::pair-nil lab-acp::acp-state)
   (with-access::acp-state lab-acp (nlabel acps init-var)
      (set! nlabel (-fx nlabel 1))
      (set! init-var (cdr init-var))
      (vector-ref acps nlabel)))

(define (init-var! idx::long lab-acp::acp-state)
   (vector-set! (car (-> lab-acp init-var)) idx #t))

(define (clean-init! cur-acp::vector lab-acp::acp-state)
   (let ((v::vector (car (-> lab-acp init-var)))
         (n::long (vector-length cur-acp)))
      (do ((i 0 (+fx i 1)))
          ((>=fx i n))
         (let-if (r (vector-ref cur-acp i))
            (unless (vector-ref v (car r))
               (vector-set! cur-acp i #f))))))

(define (unify-acps acp . l)
   (let ((n (vector-length acp)))
      (define (loop right::bint i::bint l::pair-nil)
         (unless (null? l)
            (let ((r (vector-ref (car l) i)))
               (if (or (not r) (not (=fx (car r) right)))
                   (vector-set! acp i #f)
                   (loop right i (cdr l))))))
      (do ((i 0 (+fx i 1)))
          ((>=fx i n))
         (let-if (r (vector-ref acp i))
            (loop (car r) i l)))))

(define-generic (local-copyprop! i::instruction cur-acp::vector
                                 lab-acp::acp-state)
   #f)

(define-method (local-copyprop! i::one-arg cur-acp::vector lab-acp::acp-state)
   (cond ((eq? (-> i opcode) 'local.get)
          (with-access::localidxp (-> i x) (idx type)
             (let-if (right (vector-ref cur-acp idx))
                (set! type (cdr right))
                (set! idx (car right))
                (set! (-> i outtype) `(,(cdr right))))))

         ((or (eq? (-> i opcode) 'local.set) (eq? (-> i opcode) 'local.tee))
          (with-access::localidxp (-> i x) (idx type)
             (init-var! idx lab-acp)
             (vector-set! cur-acp idx #f)
             (do ((i 0 (+fx i 1)))
                 ((>=fx i (vector-length cur-acp)))
                (let-if (right (vector-ref cur-acp i))
                   (when (=fx (car right) idx)
                      (vector-set! cur-acp i #f))))))

         ((or (eq? (-> i opcode) 'br)
              (eq? (-> i opcode) 'br_if)
              (eq? (-> i opcode) 'br_on_null)
              (eq? (-> i opcode) 'br_on_non_null))
          (update-lab-acp! (-> i x) cur-acp lab-acp))))

(define-method (local-copyprop! i::three-args cur-acp::vector
                                lab-acp::acp-state)
   (if (or (eq? (-> i opcode) 'br_on_cast) (eq? (-> i opcode) 'br_on_cast_fail))
       (update-lab-acp! (-> i x) cur-acp lab-acp)))

(define-method (local-copyprop! i::br_table cur-acp::vector
                                lab-acp::acp-state)
   (for-each (lambda (l) (update-lab-acp! l cur-acp lab-acp)) (-> i labels)))

(define-method (local-copyprop! i::block cur-acp::vector lab-acp::acp-state)
   (enter-frame lab-acp)
   (call-next-method)
   (apply unify-acps cur-acp (exit-frame lab-acp))
   (clean-init! cur-acp lab-acp))

(define-method (local-copyprop! i::loop cur-acp::vector lab-acp::acp-state)
   (vector-fill! cur-acp #f)
   (enter-frame lab-acp)
   (call-next-method)
   ; this information should probably stored to allow a global copy propagation
   (exit-frame lab-acp)
   (clean-init! cur-acp lab-acp))

(define-method (local-copyprop! i::try_table cur-acp::vector
                                lab-acp::acp-state)
   (for-each (lambda (c::catch-branch)
               (update-lab-acp! (-> c label)
                                (make-vector (vector-length cur-acp) #f)
                                lab-acp))
             (-> i catches))
   (enter-frame lab-acp)
   (call-next-method)
   (apply unify-acps cur-acp (exit-frame lab-acp))
   (clean-init! cur-acp lab-acp))

(define-method (local-copyprop! i::if-then cur-acp::vector
                                lab-acp::acp-state)
   (let ((acp (vector-copy cur-acp)))
      (enter-frame lab-acp)
      (local-copyprop! (-> i then) cur-acp lab-acp)
      (apply unify-acps cur-acp acp (exit-frame lab-acp))))

(define-method (local-copyprop! i::if-else cur-acp::vector
                                lab-acp::acp-state)
   (let ((acp (vector-copy cur-acp 0 (vector-length cur-acp))))
      (enter-frame lab-acp)
      (local-copyprop! (-> i then) cur-acp lab-acp)
      (local-copyprop! (-> i else) acp lab-acp)
      (apply unify-acps cur-acp acp (exit-frame lab-acp))
      (clean-init! cur-acp lab-acp)))

(define (isa-local.get? i::instruction)
   (eq? (-> i opcode) 'local.get))

(define (isa-local.set? i::instruction)
   (eq? (-> i opcode) 'local.set))

(define-method (local-copyprop! i::sequence cur-acp::vector
                                lab-acp::acp-state)
   (define (walk-zipper!::pair-nil left::pair-nil right::pair-nil)
      (if (null? right)
          (reverse left)
          (begin
            (local-copyprop! (car right) cur-acp lab-acp)
            (if (and (not (null? left)) (isa-local.get? (car left))
                     (isa-local.set? (car right)))
                (let ((y::localidxp (with-access::one-arg (car left) (x) x))
                      (x::localidxp (with-access::one-arg (car right) (x) x)))
                   (vector-set! cur-acp (-> x idx)
                                (cons (-> y idx) (-> y type)))))
            (walk-zipper! (cons (car right) left) (cdr right)))))

   (set! (-> i body) (walk-zipper! '() (-> i body))))
