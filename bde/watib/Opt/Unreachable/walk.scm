;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Removal of some unreachable code.

(module opt_unreachable
   (static (class label-state::object
              (nlabel::bint (default 1))
              (jumped?::vector (default (make-vector 10000)))))
   (from (ast_node "Ast/node.scm"))
   (export (unreachable! f::func)))

(define (unreachable! f::func)
   (unreachable!? (-> f body) (instantiate::label-state))
   #f)

(define (enter-frame st::label-state)
   (with-access::label-state st (nlabel jumped?)
      (vector-set! jumped? nlabel #f)
      (set! nlabel (+fx 1 nlabel))))

(define (exit-frame st::label-state)
   (with-access::label-state st (nlabel jumped?)
      (set! nlabel (-fx nlabel 1))
      (vector-ref jumped? nlabel)))

(define (poly-result? i::instruction)
   (and (not (null? (-> i outtype))) (eq? (car (-> i outtype)) 'poly)))

(define (unreachable::pair-nil i::instruction)
   (if (poly-result? i)
       '()
       (list (instantiate::instruction
              (intype '())
              (outtype '(poly))
              (parent (-> i parent))
              (opcode 'unreachable)))))

(define-generic (unreachable!?::bool i::instruction st::label-state)
   (poly-result? i))

(define-generic (update-state! p::parameter st::label-state)
   #f)

(define-method (update-state! p::labelidxp st::label-state)
   (vector-set! (-> st jumped?) (- (-> st nlabel) (-> p idx) 1) #t))

(define-method (unreachable!?::bool i::one-arg st::label-state)
   (update-state! (-> i x) st)
   (call-next-method))

(define-method (unreachable!?::bool i::two-args st::label-state)
   (update-state! (-> i x) st)
   (update-state! (-> i y) st)
   (call-next-method))

(define-method (unreachable!?::bool i::three-args st::label-state)
   (update-state! (-> i x) st)
   (update-state! (-> i y) st)
   (update-state! (-> i z) st)
   (call-next-method))

(define-method (unreachable!?::bool i::br_table st::label-state)
   (for-each (lambda (p::parameter) (update-state! p st)) (-> i labels)))

(define-method (unreachable!?::bool i::sequence st::label-state)
   (define (walk-list!?::bool l::pair-nil)
      (cond ((null? l) #f)
            ((unreachable!? (car l) st)
             (unless (null? (cdr l))
                (set-cdr! l (unreachable (car l))))
             #t)
            (else (walk-list!? (cdr l)))))
   (walk-list!? (-> i body)))

(define-method (unreachable!?::bool i::block st::label-state)
   (enter-frame st)
   (let ((b (call-next-method)))
      (and (not (exit-frame st)) b)))

(define-method (unreachable!?::bool i::loop st::label-state)
  (enter-frame st)
  (let ((b (call-next-method)))
     (exit-frame st)
     b))

(define-method (unreachable!?::bool i::if-then st::label-state)
   (enter-frame st)
   (let ((b (unreachable!? (-> i then) st)))
      (and (not (exit-frame st)) b)))

(define-method (unreachable!?::bool i::if-else st::label-state)
   (enter-frame st)
   (let ((b1 (unreachable!? (-> i then) st))
         (b2 (unreachable!? (-> i else) st)))
      (and (not (exit-frame st)) b1 b2)))

(define-method (unreachable!?::bool i::try_table st::label-state)
   (define (update-state-catch! c::catch-branch)
      (update-state! (-> c label) st))
   (for-each update-state-catch! (-> i catches))
   (enter-frame st)
   (let ((b (call-next-method)))
      (and (not (exit-frame st)) b)))
