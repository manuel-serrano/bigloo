;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Constant folding.

(module opt_const
   (from (ast_node "Ast/node.scm"))
   (export (const! f::func)))

(define (const! f::func)
   (const-fold! (-> f body)))

(define-generic (const-fold! i::instruction)
   #f)

(define (isa-i32.const? i::instruction)
   (eq? 'i32.const (-> i opcode)))

(define (isa-if? i::instruction)
   (isa? i if-then))

(define-generic (const-fold-if! i::if-then x::i32p)
   (if (= 0 (-> x num))
       '()
       (with-access::sequence (-> i then) (body) body)))

(define-method (const-fold-if! i::if-else x::i32p)
   (duplicate::block
      (if (= 0 (-> x num))
	  (-> i else)
	  (-> i then))))

(define-method (const-fold! i::sequence)
   (define (walk-list l::pair-nil)
      (match-case l
         (() '())

         ((((and (? isa-i32.const?) ?cst::one-arg))
           ((and (? isa-if?) ?if::if-then)) . ?tl)
          (walk-list (append (const-fold-if! if (-> cst x)) tl)))
         ((?hd . ?tl) (cons hd (walk-list tl)))))

   (with-access::sequence i (body)
      (set! body (walk-list body))))

(define-method (const-fold! i::if-then)
   (const-fold! (-> i then)))

(define-method (const-fold! i::if-else)
   (const-fold! (-> i else)))
