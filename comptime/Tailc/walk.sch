;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 17:23:21 CET 2011 
;; (bigloo.new -classgen Tailc/walk.scm)
;; ==========================================================

;; The directives
(directives

;; local-tail
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local-tail::local-tail id1163::symbol name1164::obj type1165::type value1166::value access1167::obj fast-alpha1168::obj removable1169::obj occurrence1170::long occurrencew1171::long user?1172::bool key1173::long)
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
(define-inline (make-local-tail::local-tail id1163::symbol name1164::obj type1165::type value1166::value access1167::obj fast-alpha1168::obj removable1169::obj occurrence1170::long occurrencew1171::long user?1172::bool key1173::long) (instantiate::local-tail (id id1163) (name name1164) (type type1165) (value value1166) (access access1167) (fast-alpha fast-alpha1168) (removable removable1169) (occurrence occurrence1170) (occurrencew occurrencew1171) (user? user?1172) (key key1173)))
(define-inline (local-tail?::bool obj::obj) ((@ isa? __object) obj (@ local-tail tailc_walk)))
(define (local-tail-nil::local-tail) (class-nil (@ local-tail tailc_walk)))
(define-inline (local-tail-key::long o::local-tail) (with-access::local-tail o (key) key))
(define-inline (local-tail-key-set! o::local-tail v::long) (with-access::local-tail o (key) (set! key v)))
(define-inline (local-tail-user?::bool o::local-tail) (with-access::local-tail o (user?) user?))
(define-inline (local-tail-user?-set! o::local-tail v::bool) (with-access::local-tail o (user?) (set! user? v)))
(define-inline (local-tail-occurrencew::long o::local-tail) (with-access::local-tail o (occurrencew) occurrencew))
(define-inline (local-tail-occurrencew-set! o::local-tail v::long) (with-access::local-tail o (occurrencew) (set! occurrencew v)))
(define-inline (local-tail-occurrence::long o::local-tail) (with-access::local-tail o (occurrence) occurrence))
(define-inline (local-tail-occurrence-set! o::local-tail v::long) (with-access::local-tail o (occurrence) (set! occurrence v)))
(define-inline (local-tail-removable::obj o::local-tail) (with-access::local-tail o (removable) removable))
(define-inline (local-tail-removable-set! o::local-tail v::obj) (with-access::local-tail o (removable) (set! removable v)))
(define-inline (local-tail-fast-alpha::obj o::local-tail) (with-access::local-tail o (fast-alpha) fast-alpha))
(define-inline (local-tail-fast-alpha-set! o::local-tail v::obj) (with-access::local-tail o (fast-alpha) (set! fast-alpha v)))
(define-inline (local-tail-access::obj o::local-tail) (with-access::local-tail o (access) access))
(define-inline (local-tail-access-set! o::local-tail v::obj) (with-access::local-tail o (access) (set! access v)))
(define-inline (local-tail-value::value o::local-tail) (with-access::local-tail o (value) value))
(define-inline (local-tail-value-set! o::local-tail v::value) (with-access::local-tail o (value) (set! value v)))
(define-inline (local-tail-type::type o::local-tail) (with-access::local-tail o (type) type))
(define-inline (local-tail-type-set! o::local-tail v::type) (with-access::local-tail o (type) (set! type v)))
(define-inline (local-tail-name::obj o::local-tail) (with-access::local-tail o (name) name))
(define-inline (local-tail-name-set! o::local-tail v::obj) (with-access::local-tail o (name) (set! name v)))
(define-inline (local-tail-id::symbol o::local-tail) (with-access::local-tail o (id) id))
(define-inline (local-tail-id-set! o::local-tail v::symbol) (with-access::local-tail o (id) (set! id v)))
))
