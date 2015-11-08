;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Tailc/walk.scm)
;; ==========================================================

;; The directives
(directives

;; local-tail
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local-tail::local-tail id1113::symbol name1114::obj type1115::type value1116::value access1117::obj fast-alpha1118::obj removable1119::obj occurrence1120::long occurrencew1121::long user?1122::bool key1123::long)
    (inline local-tail?::bool ::obj)
    (local-tail-nil::local-tail)
    (inline local-tail-key::long ::local-tail)
    (inline local-tail-user?::bool ::local-tail)
    (inline local-tail-user?-set! ::local-tail ::bool)
    (inline local-tail-occurrencew::long ::local-tail)
    (inline local-tail-occurrencew-set! ::local-tail ::long)
    (inline local-tail-occurrence::long ::local-tail)
    (inline local-tail-occurrence-set! ::local-tail ::long)
    (inline local-tail-removable::obj ::local-tail)
    (inline local-tail-removable-set! ::local-tail ::obj)
    (inline local-tail-fast-alpha::obj ::local-tail)
    (inline local-tail-fast-alpha-set! ::local-tail ::obj)
    (inline local-tail-access::obj ::local-tail)
    (inline local-tail-access-set! ::local-tail ::obj)
    (inline local-tail-value::value ::local-tail)
    (inline local-tail-value-set! ::local-tail ::value)
    (inline local-tail-type::type ::local-tail)
    (inline local-tail-type-set! ::local-tail ::type)
    (inline local-tail-name::obj ::local-tail)
    (inline local-tail-name-set! ::local-tail ::obj)
    (inline local-tail-id::symbol ::local-tail)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; local-tail
(define-inline (make-local-tail::local-tail id1113::symbol name1114::obj type1115::type value1116::value access1117::obj fast-alpha1118::obj removable1119::obj occurrence1120::long occurrencew1121::long user?1122::bool key1123::long) (instantiate::local-tail (id id1113) (name name1114) (type type1115) (value value1116) (access access1117) (fast-alpha fast-alpha1118) (removable removable1119) (occurrence occurrence1120) (occurrencew occurrencew1121) (user? user?1122) (key key1123)))
(define-inline (local-tail?::bool obj::obj) ((@ isa? __object) obj (@ local-tail tailc_walk)))
(define (local-tail-nil::local-tail) (class-nil (@ local-tail tailc_walk)))
(define-inline (local-tail-key::long o::local-tail) (-> |#!bigloo_wallow| o key))
(define-inline (local-tail-key-set! o::local-tail v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (local-tail-user?::bool o::local-tail) (-> |#!bigloo_wallow| o user?))
(define-inline (local-tail-user?-set! o::local-tail v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (local-tail-occurrencew::long o::local-tail) (-> |#!bigloo_wallow| o occurrencew))
(define-inline (local-tail-occurrencew-set! o::local-tail v::long) (set! (-> |#!bigloo_wallow| o occurrencew) v))
(define-inline (local-tail-occurrence::long o::local-tail) (-> |#!bigloo_wallow| o occurrence))
(define-inline (local-tail-occurrence-set! o::local-tail v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (local-tail-removable::obj o::local-tail) (-> |#!bigloo_wallow| o removable))
(define-inline (local-tail-removable-set! o::local-tail v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (local-tail-fast-alpha::obj o::local-tail) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (local-tail-fast-alpha-set! o::local-tail v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (local-tail-access::obj o::local-tail) (-> |#!bigloo_wallow| o access))
(define-inline (local-tail-access-set! o::local-tail v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (local-tail-value::value o::local-tail) (-> |#!bigloo_wallow| o value))
(define-inline (local-tail-value-set! o::local-tail v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (local-tail-type::type o::local-tail) (-> |#!bigloo_wallow| o type))
(define-inline (local-tail-type-set! o::local-tail v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (local-tail-name::obj o::local-tail) (-> |#!bigloo_wallow| o name))
(define-inline (local-tail-name-set! o::local-tail v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (local-tail-id::symbol o::local-tail) (-> |#!bigloo_wallow| o id))
(define-inline (local-tail-id-set! o::local-tail v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
