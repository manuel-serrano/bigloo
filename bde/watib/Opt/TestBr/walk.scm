;; Copyright (c) 2025 Aghilas Y. Boussaa, see COPYING file

;; Replacement of (if (ref.test ...) ...) patterns with conditional branching.

(module opt_testbr
   (from (ast_node "Ast/node.scm"))
   (import (type_type "Type/type.scm"))
   (export (testbr! f::func)))

(define (testbr! f::func)
   (if-test->br! (-> f body)))

;; see later if copy propagation doesn't make replace-var useless (it should)

(define-generic (replace-var! i::instruction x::long y::long t)
   #f)

(define-method (replace-var! i::one-arg x::long y::long t)
   (if (and (eq? 'local.get (-> i opcode)))
       (with-access::localidxp (-> i x) (idx type)
          (when (=fx idx x)
             (set! (-> i outtype) (list t))
             (set! idx y)
             (set! type t))
          #f)
       (if (or (eq? 'local.set (-> i opcode)) (eq? 'local.tee (-> i opcode)))
           (with-access::localidxp (-> i x) (idx)
              (=fx x idx))
           #f)))

(define-method (replace-var! i::sequence x::long y::long t)
   (define (walk-list!?::bool l::pair-nil)
      (match-case l
         (() #f)
         ((?i . ?tl)
          (if (replace-var! i x y t)
              #t
              (walk-list!? tl)))))
   (walk-list!? (-> i body)))

(define-generic (replaces-x? i::instruction x::long)
   #f)

(define-method (replaces-x? i::one-arg x::long)
   (if (or (eq? 'local.set (-> i opcode)) (eq? 'local.tee (-> i opcode)))
       (with-access::localidxp (-> i x) (idx)
                               (=fx x idx))
       #f))

(define-method (replaces-x? i::sequence x::long)
   (any (lambda (i::instruction) (replaces-x? i x)) (-> i body)))

(define-method (replaces-x? i::if-then x::long)
   (replaces-x? (-> i then) x))

(define-method (replaces-x? i::if-else x::long)
   (or (replaces-x? (-> i then) x)
       (replaces-x? (-> i else) x)))


(define-generic (uses-x? i::instruction x::long)
   #f)

(define-method (uses-x? i::one-arg x::long)
   (if (or (eq? 'local.get (-> i opcode)))
       (with-access::localidxp (-> i x) (idx)
                               (=fx x idx))
       #f))

(define-method (uses-x? i::sequence x::long)
   (any (lambda (i::instruction) (uses-x? i x)) (-> i body)))

(define-method (uses-x? i::if-then x::long)
   (uses-x? (-> i then) x))

(define-method (uses-x? i::if-else x::long)
   (or (uses-x? (-> i then) x)
       (uses-x? (-> i else) x)))

;; rough approximation
(define-method (replace-var! i::loop x::long y::long t)
   (if (replaces-x? i x)
       #t
       (call-next-method)))

(define-method (replace-var! i::if-then x::long y::long t)
   (replace-var! (-> i then) x y t))

(define-method (replace-var! i::if-else x::long y::long t)
   ; we do it like that because we do not want lazy evaluation
   (let ((b (replace-var! (-> i else) x y t)))
     (or (call-next-method) b)))

(define-generic (incr-labels! i::instruction threshold::long)
  #f)

(define-generic (incr-labels-param! p::parameter threshold::long) #f)

(define-method (incr-labels-param! p::labelidxp threshold::long)
   (if (>= (-> p idx) threshold)
       (set! (-> p idx) (+fx 1 (-> p idx)))))

(define-method (incr-labels! i::one-arg threshold::long)
   (incr-labels-param! (-> i x) threshold))

(define-method (incr-labels! i::two-args threshold::long)
   (incr-labels-param! (-> i x) threshold)
   (incr-labels-param! (-> i y) threshold))

(define-method (incr-labels! i::three-args threshold::long)
   (incr-labels-param! (-> i x) threshold)
   (incr-labels-param! (-> i y) threshold)
   (incr-labels-param! (-> i z) threshold))

(define-method (incr-labels! i::sequence threshold::long)
   (for-each (lambda (i) (incr-labels! i (+fx 1 threshold))) (-> i body)))

(define-method (incr-labels! i::if-then threshold::long)
   (incr-labels! (-> i then) threshold))

(define-method (incr-labels! i::if-else threshold::long)
   (incr-labels! (-> i else) threshold)
   (call-next-method))

(define-method (incr-labels! i::try_table threshold::long)
   (define (incr-labels-catch! c::catch-branch)
      (with-access::labelidxp (-> c label) (idx)
	 (if (>=fx idx threshold)
	     (set! idx (+fx 1 idx)))))
   (for-each incr-labels-catch! (-> i catches))
   (call-next-method))

(define-method (incr-labels! i::br_table  threshold::long)
   (for-each (lambda (i) (incr-labels-param! i (+fx threshold 1)))
             (-> i labels)))

(define-generic (if-test->br! i::instruction)
   i)

(define-method (if-test->br! i::if-then)
   (if-test->br! (-> i then)))

(define-method (if-test->br! i::if-else)
   (if-test->br! (-> i else))
   (call-next-method))

(define (isa-ref.test? i::instruction)
   (eq? (-> i opcode) 'ref.test))

(define (isa-local.get? i::instruction)
   (eq? (-> i opcode) 'local.get))

(define (local-add! f::func t)
   (define (append-length l::pair-nil i::long)
      (if (null? l)
          (values (list t) i)
          (multiple-value-bind (tl n) (append-length (cdr l) (+fx 1 i))
             (values (cons (car l) tl) n))))

   (multiple-value-bind (l n) (append-length (-> f locals) 0)
      (set! (-> f locals) l)
      ; todo - avoid calculating the length each time
      (+ n (length (-> f formals)))))

;; (define-generic (block-gen! i::if-then lget::one-arg var::localidxp rt-src
;;                             rt-dst)
;;    (if-test->br! (-> i then))
;;    (let ((y (local-add! (-> i parent) rt-dst)))
;;       (replace-var! (-> i then) (-> var idx) y rt-dst)
;;       (with-access::sequence (-> i then) (body)
;;          (set! body
;;                `(,lget
;;                  ,(instantiate::three-args
;;                    (intype (list rt-src))
;;                    (outtype (list rt-dst))
;;                    (parent (-> i parent))
;;                    (opcode 'br_on_cast_fail)
;;                    (x (instantiate::labelidxp (idx 0) (type '())))
;;                    (y (instantiate::typep (type rt-src)))
;;                    (z (instantiate::typep (type rt-dst))))
;;                  ,(instantiate::one-arg
;;                    (intype (list rt-dst))
;;                    (outtype '())
;;                    (parent (-> i parent))
;;                    (opcode 'local.set)
;;                    (x (instantiate::localidxp (idx y) (init? #f)
;;                                               (type rt-dst))))
;;                  ,@body)))))
(define-generic (block-gen! i::if-then lget::one-arg var::localidxp rt-src
                             rt-dst)
   (let ((ni::if-else (duplicate::if-else i
                       (else (instantiate::sequence (intype '())
                                                    (outtype '())
                                                    (parent (-> i parent))
                                                    (opcode 'nop)
                                                    (body '()))))))
      (if (block-gen! ni lget var rt-src rt-dst)
          (begin (set! (-> i then) (-> ni then)) #t)
          #f)))

;; (define-method (block-gen! i::if-else lget::one-arg var::localidxp rt-src
;;                            rt-dst)
;;    (if-test->br! (-> i then))
;;    (if-test->br! (-> i else))
;;    (let ((y (local-add! (-> i parent) rt-dst)))
;;       (replace-var! (-> i then) (-> var idx) y rt-dst)
;;       (incr-labels! (-> i else) -1)
;;       (with-access::sequence (-> i then) (body (ot outtype))
;;          (set! body
;;                `(,(instantiate::block
;;                    (intype '())
;;                    (outtype (list rt-dst))
;;                    (parent (-> i parent))
;;                    (opcode 'block)
;;                    (body
;;                     `(,lget
;;                       ,(instantiate::three-args
;;                         (intype (list rt-src))
;;                         (outtype (list (rt-diff rt-src rt-dst)))
;;                         (parent (-> i parent))
;;                         (opcode 'br_on_cast)
;;                         (x (instantiate::labelidxp (idx 0)
;;                                                    (type (list rt-dst))))
;;                         (y (instantiate::typep (type rt-src)))
;;                         (z (instantiate::typep (type rt-dst))))
;;                       ,(instantiate::instruction
;;                         (intype (list (rt-diff rt-src rt-dst)))
;;                         (outtype '())
;;                         (parent (-> i parent))
;;                         (opcode 'drop))
;;                       ,@(with-access::sequence (-> i else) (body) body)
;;                       ; the type here is kind of a hack, we can't really do
;;                       ; better without maintaining the stack state
;;                       ,(instantiate::one-arg
;;                         (intype ot)
;;                         (outtype '(poly))
;;                         (parent (-> i parent))
;;                         (opcode 'br)
;;                         (x (instantiate::labelidxp (idx 1) (type ot)))))))
;;                  ,(instantiate::one-arg
;;                    (intype (list rt-dst))
;;                    (outtype '())
;;                    (parent (-> i parent))
;;                    (opcode 'local.set)
;;                    (x (instantiate::localidxp (idx y) (init? #f)
;;                                               (type rt-dst))))
;;                  ,@body)))))

(define-method (block-gen! i::if-else lget::one-arg var::localidxp rt-src
                           rt-dst)
   (if-test->br! (-> i then))
   (if-test->br! (-> i else))
   (if (uses-x? (-> i then) (-> var idx))
   (let ((y (local-add! (-> i parent) rt-dst)))
      (replace-var! (-> i then) (-> var idx) y rt-dst)
      (incr-labels! (-> i then) -1)
      (with-access::sequence (-> i then) (body (ot outtype))
         (set! body
               `(,(instantiate::block
                   (intype '())
                   (outtype (list rt-src))
                   (parent (-> i parent))
                   (opcode 'block)
                   (body
                    `(,lget
                      ,(instantiate::three-args
                        (intype (list rt-src))
                        (outtype (rt-diff rt-src rt-dst))
                        (parent (-> i parent))
                        (opcode 'br_on_cast_fail)
                        (x (instantiate::labelidxp (idx 0)
                                                   (type (list rt-dst))))
                        (y (instantiate::typep (type rt-src)))
                        (z (instantiate::typep (type rt-dst))))
                      ,(instantiate::one-arg
                        (intype (list rt-dst))
                        (outtype '())
                        (parent (-> i parent))
                        (opcode 'local.set)
                        (x (instantiate::localidxp (idx y) (init? #f)
                                                   (type rt-dst))))
                      ,@body
                      ; the type here is kind of a hack, we can't really do
                      ; better without maintaining the stack state
                      ,(instantiate::one-arg
                        (intype ot)
                        (outtype '(poly))
                        (parent (-> i parent))
                        (opcode 'br)
                        (x (instantiate::labelidxp (idx 1) (type ot)))))))
                 ,(instantiate::instruction
                        (intype (list (rt-diff rt-src rt-dst)))
                        (outtype '())
                        (parent (-> i parent))
                        (opcode 'drop))
                 ,@(with-access::sequence (-> i else) (body) body))))
      #t)
   #f))

(define-method (if-test->br! i::sequence)
   (define (walk-list! l::pair-nil)
      (match-case l
        (((and (? isa-local.get?) ?lget)
          (and (? isa-ref.test?) ?test)
          (and (? (lambda (i) (isa? i if-then))) ?i::if-then). ?tl)
         (with-access::one-arg lget ((var x) (ot outtype))
            (with-access::one-arg test ((rt x))
               ;;(when
               ;;    (block-gen! i lget var (car ot)
               ;;               (with-access::typep rt (type) type))
               ;;  (set-car! l (duplicate::block (-> i then)))
               ;;  (set-cdr! l tl))
               ;;(walk-list! tl))))
               (block-gen! i lget var (car ot)
                           (with-access::typep rt (type) type))
               (set-car! l (duplicate::block (-> i then)))
               (set-cdr! l tl))
               (walk-list! tl)))
        ((?hd . ?tl)
         (if-test->br! hd)
         (walk-list! tl))))

   (walk-list! (-> i body)))
