;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 17:23:21 CET 2011 
;; (bigloo.new -classgen Callcc/walk.scm)
;; ==========================================================

;; The directives
(directives

;; local/cell
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/cell::local/cell id1163::symbol name1164::obj type1165::type value1166::value access1167::obj fast-alpha1168::obj removable1169::obj occurrence1170::long occurrencew1171::long user?1172::bool key1173::long)
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
(define-inline (make-local/cell::local/cell id1163::symbol name1164::obj type1165::type value1166::value access1167::obj fast-alpha1168::obj removable1169::obj occurrence1170::long occurrencew1171::long user?1172::bool key1173::long) (instantiate::local/cell (id id1163) (name name1164) (type type1165) (value value1166) (access access1167) (fast-alpha fast-alpha1168) (removable removable1169) (occurrence occurrence1170) (occurrencew occurrencew1171) (user? user?1172) (key key1173)))
(define-inline (local/cell?::bool obj::obj) ((@ isa? __object) obj (@ local/cell callcc_walk)))
(define (local/cell-nil::local/cell) (class-nil (@ local/cell callcc_walk)))
(define-inline (local/cell-key::long o::local/cell) (with-access::local/cell o (key) key))
(define-inline (local/cell-key-set! o::local/cell v::long) (with-access::local/cell o (key) (set! key v)))
(define-inline (local/cell-user?::bool o::local/cell) (with-access::local/cell o (user?) user?))
(define-inline (local/cell-user?-set! o::local/cell v::bool) (with-access::local/cell o (user?) (set! user? v)))
(define-inline (local/cell-occurrencew::long o::local/cell) (with-access::local/cell o (occurrencew) occurrencew))
(define-inline (local/cell-occurrencew-set! o::local/cell v::long) (with-access::local/cell o (occurrencew) (set! occurrencew v)))
(define-inline (local/cell-occurrence::long o::local/cell) (with-access::local/cell o (occurrence) occurrence))
(define-inline (local/cell-occurrence-set! o::local/cell v::long) (with-access::local/cell o (occurrence) (set! occurrence v)))
(define-inline (local/cell-removable::obj o::local/cell) (with-access::local/cell o (removable) removable))
(define-inline (local/cell-removable-set! o::local/cell v::obj) (with-access::local/cell o (removable) (set! removable v)))
(define-inline (local/cell-fast-alpha::obj o::local/cell) (with-access::local/cell o (fast-alpha) fast-alpha))
(define-inline (local/cell-fast-alpha-set! o::local/cell v::obj) (with-access::local/cell o (fast-alpha) (set! fast-alpha v)))
(define-inline (local/cell-access::obj o::local/cell) (with-access::local/cell o (access) access))
(define-inline (local/cell-access-set! o::local/cell v::obj) (with-access::local/cell o (access) (set! access v)))
(define-inline (local/cell-value::value o::local/cell) (with-access::local/cell o (value) value))
(define-inline (local/cell-value-set! o::local/cell v::value) (with-access::local/cell o (value) (set! value v)))
(define-inline (local/cell-type::type o::local/cell) (with-access::local/cell o (type) type))
(define-inline (local/cell-type-set! o::local/cell v::type) (with-access::local/cell o (type) (set! type v)))
(define-inline (local/cell-name::obj o::local/cell) (with-access::local/cell o (name) name))
(define-inline (local/cell-name-set! o::local/cell v::obj) (with-access::local/cell o (name) (set! name v)))
(define-inline (local/cell-id::symbol o::local/cell) (with-access::local/cell o (id) id))
(define-inline (local/cell-id-set! o::local/cell v::symbol) (with-access::local/cell o (id) (set! id v)))
))
