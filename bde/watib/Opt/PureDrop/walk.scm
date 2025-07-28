;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Removal of expressions of the form (drop e) when e has no side effect.
;;
;; The detection of side effects is coarse. For instance, jumps are considered
;; to be side effects. However in a block of label $l, jumping to $l should not
;; cause the block to be marked as having a side-effect.
;;
;; This optimisation is kind of hacky because the code is in linear structure,
;; having it in a tree like shape would simplify it



(module opt_puredrop
   (library srfi1)
   (from (ast_node "Ast/node.scm"))
   (include "Misc/read-table.sch")
   (import (misc_list "Misc/list.scm"))
   (export (puredrop! f::func)))

(read-table *side-effect* "Opt/PureDrop/side-effect-table.sch")

(define (puredrop! f::func)
   (side-effect!? (-> f body)))

;; to allow for a finer side-effect detection, we would return something bigger
;; than a boolean, for instance, we could add a list of labels we can branch to
(define-generic (side-effect!?::bool i::instruction)
   (if (hashtable-contains? *side-effect* (-> i opcode))
       (hashtable-get *side-effect* (-> i opcode))
       #t))

(define-method (side-effect!?::bool i::try_table)
   (call-next-method)
   #t) ; can branch

(define-method (side-effect!?::bool i::if-then)
   (side-effect!? (-> i then)))

(define-method (side-effect!?::bool i::if-else)
   (let ((b (side-effect!? (-> i else))))
      (or (call-next-method) b)))

(define (isa-drop? i::instruction)
   (eq? (-> i opcode) 'drop))

(define-method (side-effect!?::bool i::sequence)
   (define (drops::pair-nil n::bint)
      (list-tabulate n (lambda (-) (cons #f
                                         (instantiate::instruction
                                          (intype '(top))
                                          (outtype '())
                                          (opcode 'drop)
                                          (parent (-> i parent)))))))

   (define side-effect? #f)

   (define (remove-pures::pair-nil l::pair-nil n::bint)
      (cond
       ((=fx 0 n) l)
       ((null? l) (drops n))
       (else
        (with-access::instruction (cdr (car l)) (outtype intype)
           (let ((b (car (car l))))
              (set! side-effect? (or b side-effect?))
              (let ((on (length outtype))
                    (in (length intype)))
                 (cond
                  ((=fx in 0)
                   (append (if b (append (drops on) (list (car l))) '())
                           (remove-pures (cdr l) (-fx n on))))
                  ((and (=fx in 1) (not b))
                   (remove-pures (cdr l) (-fx (+fx n 1) on)))
                  (else (append (drops n) l)))))))))

   (define (walk-zipper::pair-nil left::pair-nil right::pair-nil)
      (if (null? right)
          (map cdr (reverse left))
          (multiple-value-bind (pre suf) (span isa-drop? right)
             (if (null? pre)
                 (let ((se (side-effect!? (car right))))
                    (set! side-effect? (or se side-effect?))
                    (walk-zipper (cons (cons se (car right)) left) (cdr right)))
                 (walk-zipper (remove-pures left (length pre)) suf)))))

   (set! (-> i body) (walk-zipper '() (-> i body)))
   side-effect?)
