;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Peephole optimisations.

(module opt_peephole
   (from (ast_node "Ast/node.scm"))
   (export (peephole! f::func)))

(define (peephole! f::func)
   (walk! (-> f body)))

(define-generic (walk! i::instruction)
   #f)

(define (isa-local.get? i::instruction)
   (eq? (-> i opcode) 'local.get))

(define (isa-local.set? i::instruction)
   (eq? (-> i opcode) 'local.set))

(define-method (walk! i::if-then)
   (walk! (-> i then)))

(define-method (walk! i::if-else)
   (call-next-method)
   (walk! (-> i else)))

(define-method (walk! i::sequence)
   (define (walk-zipper::pair-nil left::pair-nil right::pair-nil)
      (cond
       ((null? right) (reverse left))
       ((and (not (null? left))
             (isa-local.set? (car left))
             (isa-local.get? (car right)))
        (let ((x::localidxp (with-access::one-arg (car left) (x) x))
              (y::localidxp (with-access::one-arg (car right) (x) x)))
           (if (=fx (-> x idx) (-> y idx))
               (walk-zipper (cdr left) (cons (instantiate::one-arg
                                              (intype `(,(-> x type)))
                                              (outtype `(,(-> x type)))
                                              (parent (-> i parent))
                                              (opcode 'local.tee)
                                              (x x))
                                             (cdr right)))
               (walk-zipper (cons (car right) left) (cdr right)))))
       (else (walk-zipper (cons (car right) left) (cdr right)))))

   (set! (-> i body) (walk-zipper '() (-> i body))))
