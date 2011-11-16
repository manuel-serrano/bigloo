;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 16:42:36 CET 2011 
;; (bigloo.new -classgen Cnst/node.scm)
;; ==========================================================

;; The directives
(directives

;; local/bvalue
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/bvalue::local/bvalue id1164::symbol name1165::obj type1167::type value1168::value access1169::obj fast-alpha1170::obj removable1171::obj occurrence1172::long occurrencew1173::long user?1174::bool key1175::long binding-value1176::node)
    (inline local/bvalue?::bool ::obj)
    (local/bvalue-nil::local/bvalue)
    (inline local/bvalue-binding-value::node ::local/bvalue)
    (inline local/bvalue-key::long ::local/bvalue)
    (inline local/bvalue-user?::bool ::local/bvalue)
    (inline local/bvalue-user?-set! ::local/bvalue ::bool)
    (inline local/bvalue-occurrencew::long ::local/bvalue)
    (inline local/bvalue-occurrencew-set! ::local/bvalue ::long)
    (inline local/bvalue-occurrence::long ::local/bvalue)
    (inline local/bvalue-occurrence-set! ::local/bvalue ::long)
    (inline local/bvalue-removable::obj ::local/bvalue)
    (inline local/bvalue-removable-set! ::local/bvalue ::obj)
    (inline local/bvalue-fast-alpha::obj ::local/bvalue)
    (inline local/bvalue-fast-alpha-set! ::local/bvalue ::obj)
    (inline local/bvalue-access::obj ::local/bvalue)
    (inline local/bvalue-access-set! ::local/bvalue ::obj)
    (inline local/bvalue-value::value ::local/bvalue)
    (inline local/bvalue-value-set! ::local/bvalue ::value)
    (inline local/bvalue-type::type ::local/bvalue)
    (inline local/bvalue-type-set! ::local/bvalue ::type)
    (inline local/bvalue-name::obj ::local/bvalue)
    (inline local/bvalue-name-set! ::local/bvalue ::obj)
    (inline local/bvalue-id::symbol ::local/bvalue)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; local/bvalue
(define-inline (make-local/bvalue::local/bvalue id1164::symbol name1165::obj type1167::type value1168::value access1169::obj fast-alpha1170::obj removable1171::obj occurrence1172::long occurrencew1173::long user?1174::bool key1175::long binding-value1176::node) (instantiate::local/bvalue (id id1164) (name name1165) (type type1167) (value value1168) (access access1169) (fast-alpha fast-alpha1170) (removable removable1171) (occurrence occurrence1172) (occurrencew occurrencew1173) (user? user?1174) (key key1175) (binding-value binding-value1176)))
(define-inline (local/bvalue?::bool obj::obj) ((@ isa? __object) obj (@ local/bvalue cnst_node)))
(define (local/bvalue-nil::local/bvalue) (class-nil (@ local/bvalue cnst_node)))
(define-inline (local/bvalue-binding-value::node o::local/bvalue) (with-access::local/bvalue o (binding-value) binding-value))
(define-inline (local/bvalue-binding-value-set! o::local/bvalue v::node) (with-access::local/bvalue o (binding-value) (set! binding-value v)))
(define-inline (local/bvalue-key::long o::local/bvalue) (with-access::local/bvalue o (key) key))
(define-inline (local/bvalue-key-set! o::local/bvalue v::long) (with-access::local/bvalue o (key) (set! key v)))
(define-inline (local/bvalue-user?::bool o::local/bvalue) (with-access::local/bvalue o (user?) user?))
(define-inline (local/bvalue-user?-set! o::local/bvalue v::bool) (with-access::local/bvalue o (user?) (set! user? v)))
(define-inline (local/bvalue-occurrencew::long o::local/bvalue) (with-access::local/bvalue o (occurrencew) occurrencew))
(define-inline (local/bvalue-occurrencew-set! o::local/bvalue v::long) (with-access::local/bvalue o (occurrencew) (set! occurrencew v)))
(define-inline (local/bvalue-occurrence::long o::local/bvalue) (with-access::local/bvalue o (occurrence) occurrence))
(define-inline (local/bvalue-occurrence-set! o::local/bvalue v::long) (with-access::local/bvalue o (occurrence) (set! occurrence v)))
(define-inline (local/bvalue-removable::obj o::local/bvalue) (with-access::local/bvalue o (removable) removable))
(define-inline (local/bvalue-removable-set! o::local/bvalue v::obj) (with-access::local/bvalue o (removable) (set! removable v)))
(define-inline (local/bvalue-fast-alpha::obj o::local/bvalue) (with-access::local/bvalue o (fast-alpha) fast-alpha))
(define-inline (local/bvalue-fast-alpha-set! o::local/bvalue v::obj) (with-access::local/bvalue o (fast-alpha) (set! fast-alpha v)))
(define-inline (local/bvalue-access::obj o::local/bvalue) (with-access::local/bvalue o (access) access))
(define-inline (local/bvalue-access-set! o::local/bvalue v::obj) (with-access::local/bvalue o (access) (set! access v)))
(define-inline (local/bvalue-value::value o::local/bvalue) (with-access::local/bvalue o (value) value))
(define-inline (local/bvalue-value-set! o::local/bvalue v::value) (with-access::local/bvalue o (value) (set! value v)))
(define-inline (local/bvalue-type::type o::local/bvalue) (with-access::local/bvalue o (type) type))
(define-inline (local/bvalue-type-set! o::local/bvalue v::type) (with-access::local/bvalue o (type) (set! type v)))
(define-inline (local/bvalue-name::obj o::local/bvalue) (with-access::local/bvalue o (name) name))
(define-inline (local/bvalue-name-set! o::local/bvalue v::obj) (with-access::local/bvalue o (name) (set! name v)))
(define-inline (local/bvalue-id::symbol o::local/bvalue) (with-access::local/bvalue o (id) id))
(define-inline (local/bvalue-id-set! o::local/bvalue v::symbol) (with-access::local/bvalue o (id) (set! id v)))
))
