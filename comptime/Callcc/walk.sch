;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Callcc/walk.scm)
;; ==========================================================

;; The directives
(directives

;; local/cell
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/cell::local/cell id1113::symbol name1114::obj type1115::type value1116::value access1117::obj fast-alpha1118::obj removable1119::obj occurrence1120::long occurrencew1121::long user?1122::bool key1123::long)
    (inline local/cell?::bool ::obj)
    (local/cell-nil::local/cell)
    (inline local/cell-key::long ::local/cell)
    (inline local/cell-user?::bool ::local/cell)
    (inline local/cell-user?-set! ::local/cell ::bool)
    (inline local/cell-occurrencew::long ::local/cell)
    (inline local/cell-occurrencew-set! ::local/cell ::long)
    (inline local/cell-occurrence::long ::local/cell)
    (inline local/cell-occurrence-set! ::local/cell ::long)
    (inline local/cell-removable::obj ::local/cell)
    (inline local/cell-removable-set! ::local/cell ::obj)
    (inline local/cell-fast-alpha::obj ::local/cell)
    (inline local/cell-fast-alpha-set! ::local/cell ::obj)
    (inline local/cell-access::obj ::local/cell)
    (inline local/cell-access-set! ::local/cell ::obj)
    (inline local/cell-value::value ::local/cell)
    (inline local/cell-value-set! ::local/cell ::value)
    (inline local/cell-type::type ::local/cell)
    (inline local/cell-type-set! ::local/cell ::type)
    (inline local/cell-name::obj ::local/cell)
    (inline local/cell-name-set! ::local/cell ::obj)
    (inline local/cell-id::symbol ::local/cell)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; local/cell
(define-inline (make-local/cell::local/cell id1113::symbol name1114::obj type1115::type value1116::value access1117::obj fast-alpha1118::obj removable1119::obj occurrence1120::long occurrencew1121::long user?1122::bool key1123::long) (instantiate::local/cell (id id1113) (name name1114) (type type1115) (value value1116) (access access1117) (fast-alpha fast-alpha1118) (removable removable1119) (occurrence occurrence1120) (occurrencew occurrencew1121) (user? user?1122) (key key1123)))
(define-inline (local/cell?::bool obj::obj) ((@ isa? __object) obj (@ local/cell callcc_walk)))
(define (local/cell-nil::local/cell) (class-nil (@ local/cell callcc_walk)))
(define-inline (local/cell-key::long o::local/cell) (-> |#!bigloo_wallow| o key))
(define-inline (local/cell-key-set! o::local/cell v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (local/cell-user?::bool o::local/cell) (-> |#!bigloo_wallow| o user?))
(define-inline (local/cell-user?-set! o::local/cell v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (local/cell-occurrencew::long o::local/cell) (-> |#!bigloo_wallow| o occurrencew))
(define-inline (local/cell-occurrencew-set! o::local/cell v::long) (set! (-> |#!bigloo_wallow| o occurrencew) v))
(define-inline (local/cell-occurrence::long o::local/cell) (-> |#!bigloo_wallow| o occurrence))
(define-inline (local/cell-occurrence-set! o::local/cell v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (local/cell-removable::obj o::local/cell) (-> |#!bigloo_wallow| o removable))
(define-inline (local/cell-removable-set! o::local/cell v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (local/cell-fast-alpha::obj o::local/cell) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (local/cell-fast-alpha-set! o::local/cell v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (local/cell-access::obj o::local/cell) (-> |#!bigloo_wallow| o access))
(define-inline (local/cell-access-set! o::local/cell v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (local/cell-value::value o::local/cell) (-> |#!bigloo_wallow| o value))
(define-inline (local/cell-value-set! o::local/cell v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (local/cell-type::type o::local/cell) (-> |#!bigloo_wallow| o type))
(define-inline (local/cell-type-set! o::local/cell v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (local/cell-name::obj o::local/cell) (-> |#!bigloo_wallow| o name))
(define-inline (local/cell-name-set! o::local/cell v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (local/cell-id::symbol o::local/cell) (-> |#!bigloo_wallow| o id))
(define-inline (local/cell-id-set! o::local/cell v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
