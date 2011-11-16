;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 16:42:36 CET 2011 
;; (bigloo.new -classgen Inline/variant.scm)
;; ==========================================================

;; The directives
(directives

;; local/variant
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-local/variant::local/variant id1164::symbol name1165::obj type1167::type value1168::value access1169::obj fast-alpha1170::obj removable1171::obj occurrence1172::long occurrencew1173::long user?1174::bool key1175::long variant1176::bool)
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
(define-inline (make-local/variant::local/variant id1164::symbol name1165::obj type1167::type value1168::value access1169::obj fast-alpha1170::obj removable1171::obj occurrence1172::long occurrencew1173::long user?1174::bool key1175::long variant1176::bool) (instantiate::local/variant (id id1164) (name name1165) (type type1167) (value value1168) (access access1169) (fast-alpha fast-alpha1170) (removable removable1171) (occurrence occurrence1172) (occurrencew occurrencew1173) (user? user?1174) (key key1175) (variant variant1176)))
(define-inline (local/variant?::bool obj::obj) ((@ isa? __object) obj (@ local/variant inline_variant)))
(define (local/variant-nil::local/variant) (class-nil (@ local/variant inline_variant)))
(define-inline (local/variant-variant::bool o::local/variant) (with-access::local/variant o (variant) variant))
(define-inline (local/variant-variant-set! o::local/variant v::bool) (with-access::local/variant o (variant) (set! variant v)))
(define-inline (local/variant-key::long o::local/variant) (with-access::local/variant o (key) key))
(define-inline (local/variant-key-set! o::local/variant v::long) (with-access::local/variant o (key) (set! key v)))
(define-inline (local/variant-user?::bool o::local/variant) (with-access::local/variant o (user?) user?))
(define-inline (local/variant-user?-set! o::local/variant v::bool) (with-access::local/variant o (user?) (set! user? v)))
(define-inline (local/variant-occurrencew::long o::local/variant) (with-access::local/variant o (occurrencew) occurrencew))
(define-inline (local/variant-occurrencew-set! o::local/variant v::long) (with-access::local/variant o (occurrencew) (set! occurrencew v)))
(define-inline (local/variant-occurrence::long o::local/variant) (with-access::local/variant o (occurrence) occurrence))
(define-inline (local/variant-occurrence-set! o::local/variant v::long) (with-access::local/variant o (occurrence) (set! occurrence v)))
(define-inline (local/variant-removable::obj o::local/variant) (with-access::local/variant o (removable) removable))
(define-inline (local/variant-removable-set! o::local/variant v::obj) (with-access::local/variant o (removable) (set! removable v)))
(define-inline (local/variant-fast-alpha::obj o::local/variant) (with-access::local/variant o (fast-alpha) fast-alpha))
(define-inline (local/variant-fast-alpha-set! o::local/variant v::obj) (with-access::local/variant o (fast-alpha) (set! fast-alpha v)))
(define-inline (local/variant-access::obj o::local/variant) (with-access::local/variant o (access) access))
(define-inline (local/variant-access-set! o::local/variant v::obj) (with-access::local/variant o (access) (set! access v)))
(define-inline (local/variant-value::value o::local/variant) (with-access::local/variant o (value) value))
(define-inline (local/variant-value-set! o::local/variant v::value) (with-access::local/variant o (value) (set! value v)))
(define-inline (local/variant-type::type o::local/variant) (with-access::local/variant o (type) type))
(define-inline (local/variant-type-set! o::local/variant v::type) (with-access::local/variant o (type) (set! type v)))
(define-inline (local/variant-name::obj o::local/variant) (with-access::local/variant o (name) name))
(define-inline (local/variant-name-set! o::local/variant v::obj) (with-access::local/variant o (name) (set! name v)))
(define-inline (local/variant-id::symbol o::local/variant) (with-access::local/variant o (id) id))
(define-inline (local/variant-id-set! o::local/variant v::symbol) (with-access::local/variant o (id) (set! id v)))
))
