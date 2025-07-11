;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Removal of some useless tests and casts.

(module opt_uncast
   (library srfi1)
   (from (ast_node "Ast/node.scm"))
   (import (type_match "Type/match.scm")
           (type_type "Type/type.scm"))
   (export (uncast! env::env f::func)))

(define (uncast! env::env f::func)
   (remove-casts! (-> f body) env))

(define-generic (remove-casts! i::instruction env::env)
   #f)

(define-method (remove-casts! i::if-then env::env)
   (remove-casts! (-> i then) env))

(define-method (remove-casts! i::if-else env::env)
   (call-next-method)
   (remove-casts! (-> i else) env))

(define (isa-ref.test? i::instruction)
   (eq? (-> i opcode) 'ref.test))

(define (isa-ref.cast? i::instruction)
   (eq? (-> i opcode) 'ref.cast))

(define (isa-ref.is_null? i::instruction)
   (eq? (-> i opcode) 'ref.is_null))

(define (isa-ref.as_non_null? i::instruction)
   (eq? (-> i opcode) 'ref.as_non_null))

(define (replace-top-with-const! i::instruction l::pair tl::pair-nil n::bint)
  (set-cdr! l `(,(instantiate::instruction
                  (intype (list (last (-> i outtype))))
                  (outtype '())
                  (parent (-> i parent))
                  (opcode 'drop))
                ,(instantiate::one-arg
                  (intype '())
                  (outtype '(i32))
                  (parent (-> i parent))
                  (opcode 'i32.const)
                  (x (instantiate::i32p (num n)))),@tl)))

(define-method (remove-casts! i::sequence env::env)
   (define (walk-list! l::pair-nil)
      (match-case l
         ((?i::instruction
           (and (? isa-ref.cast?) ?cast::one-arg) . ?tl)
          (remove-casts! i env)
          (with-access::typep (-> cast x) ((rt type))
             (if (and (not (null? (-> i outtype)))
                      (<vt= env (last (-> i outtype)) rt))
                 (begin
                    (set-cdr! l tl)
                    (walk-list! l))
                 (walk-list! (cdr l)))))

         ((?i::instruction
           (and (? isa-ref.test?) ?test::one-arg). ?tl)
          (remove-casts! i env)
          (with-access::typep (-> test x) ((rt type))
             (if (and (not (null? (-> i outtype)))
                      (<vt= env (last (-> i actouttype)) rt))
                 (begin
                    (replace-top-with-const! i l tl 1)
                    (walk-list! l))
                 (walk-list! (cdr l)))))

         ((?i::instruction
           (and (? isa-ref.is_null?) ?test::instruction). ?tl)
          (remove-casts! i env)
          (if (and (not (null? (-> i outtype)))
                   (not (nullable? (last (-> i actouttype)))))
              (begin
                 (replace-top-with-const! i l tl 0)
                 (walk-list! l))
              (walk-list! (cdr l))))

         ((?i::instruction
           (and (? isa-ref.as_non_null?) ?cast::instruction). ?tl)
          (remove-casts! i env)
          (if (and (not (null? (-> i outtype)))
                   (not (nullable? (last (-> i outtype)))))
              (begin
                 (set-cdr! l tl)
                 (walk-list! l))
              (walk-list! (cdr l))))

         ((?hd . ?tl)
          (remove-casts! hd env)
          (walk-list! tl))))
   (walk-list! (-> i body)))
