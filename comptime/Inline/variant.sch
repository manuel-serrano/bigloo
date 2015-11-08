;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Inline/variant.scm)
;; ==========================================================

;; The directives
(directives

;; local/variant
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-local/variant::local/variant id1114::symbol name1115::obj type1116::type value1117::value access1118::obj fast-alpha1119::obj removable1120::obj occurrence1121::long occurrencew1122::long user?1123::bool key1124::long variant1125::bool)
    (inline local/variant?::bool ::obj)
    (local/variant-nil::local/variant)
    (inline local/variant-variant::bool ::local/variant)
    (inline local/variant-variant-set! ::local/variant ::bool)
    (inline local/variant-key::long ::local/variant)
    (inline local/variant-user?::bool ::local/variant)
    (inline local/variant-user?-set! ::local/variant ::bool)
    (inline local/variant-occurrencew::long ::local/variant)
    (inline local/variant-occurrencew-set! ::local/variant ::long)
    (inline local/variant-occurrence::long ::local/variant)
    (inline local/variant-occurrence-set! ::local/variant ::long)
    (inline local/variant-removable::obj ::local/variant)
    (inline local/variant-removable-set! ::local/variant ::obj)
    (inline local/variant-fast-alpha::obj ::local/variant)
    (inline local/variant-fast-alpha-set! ::local/variant ::obj)
    (inline local/variant-access::obj ::local/variant)
    (inline local/variant-access-set! ::local/variant ::obj)
    (inline local/variant-value::value ::local/variant)
    (inline local/variant-value-set! ::local/variant ::value)
    (inline local/variant-type::type ::local/variant)
    (inline local/variant-type-set! ::local/variant ::type)
    (inline local/variant-name::obj ::local/variant)
    (inline local/variant-name-set! ::local/variant ::obj)
    (inline local/variant-id::symbol ::local/variant)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; local/variant
(define-inline (make-local/variant::local/variant id1114::symbol name1115::obj type1116::type value1117::value access1118::obj fast-alpha1119::obj removable1120::obj occurrence1121::long occurrencew1122::long user?1123::bool key1124::long variant1125::bool) (instantiate::local/variant (id id1114) (name name1115) (type type1116) (value value1117) (access access1118) (fast-alpha fast-alpha1119) (removable removable1120) (occurrence occurrence1121) (occurrencew occurrencew1122) (user? user?1123) (key key1124) (variant variant1125)))
(define-inline (local/variant?::bool obj::obj) ((@ isa? __object) obj (@ local/variant inline_variant)))
(define (local/variant-nil::local/variant) (class-nil (@ local/variant inline_variant)))
(define-inline (local/variant-variant::bool o::local/variant) (-> |#!bigloo_wallow| o variant))
(define-inline (local/variant-variant-set! o::local/variant v::bool) (set! (-> |#!bigloo_wallow| o variant) v))
(define-inline (local/variant-key::long o::local/variant) (-> |#!bigloo_wallow| o key))
(define-inline (local/variant-key-set! o::local/variant v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (local/variant-user?::bool o::local/variant) (-> |#!bigloo_wallow| o user?))
(define-inline (local/variant-user?-set! o::local/variant v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (local/variant-occurrencew::long o::local/variant) (-> |#!bigloo_wallow| o occurrencew))
(define-inline (local/variant-occurrencew-set! o::local/variant v::long) (set! (-> |#!bigloo_wallow| o occurrencew) v))
(define-inline (local/variant-occurrence::long o::local/variant) (-> |#!bigloo_wallow| o occurrence))
(define-inline (local/variant-occurrence-set! o::local/variant v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (local/variant-removable::obj o::local/variant) (-> |#!bigloo_wallow| o removable))
(define-inline (local/variant-removable-set! o::local/variant v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (local/variant-fast-alpha::obj o::local/variant) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (local/variant-fast-alpha-set! o::local/variant v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (local/variant-access::obj o::local/variant) (-> |#!bigloo_wallow| o access))
(define-inline (local/variant-access-set! o::local/variant v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (local/variant-value::value o::local/variant) (-> |#!bigloo_wallow| o value))
(define-inline (local/variant-value-set! o::local/variant v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (local/variant-type::type o::local/variant) (-> |#!bigloo_wallow| o type))
(define-inline (local/variant-type-set! o::local/variant v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (local/variant-name::obj o::local/variant) (-> |#!bigloo_wallow| o name))
(define-inline (local/variant-name-set! o::local/variant v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (local/variant-id::symbol o::local/variant) (-> |#!bigloo_wallow| o id))
(define-inline (local/variant-id-set! o::local/variant v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
