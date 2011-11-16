;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 16:42:36 CET 2011 
;; (bigloo.new -classgen Dataflow/walk.scm)
;; ==========================================================

;; The directives
(directives

;; local/value
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/value::local/value id1170::symbol name1171::obj type1172::type value1173::value access1174::obj fast-alpha1175::obj removable1176::obj occurrence1177::long occurrencew1178::long user?1179::bool key1180::long stamp1181::int node1182::node)
    (inline local/value?::bool ::obj)
    (local/value-nil::local/value)
    (inline local/value-node::node ::local/value)
    (inline local/value-stamp::int ::local/value)
    (inline local/value-key::long ::local/value)
    (inline local/value-user?::bool ::local/value)
    (inline local/value-user?-set! ::local/value ::bool)
    (inline local/value-occurrencew::long ::local/value)
    (inline local/value-occurrencew-set! ::local/value ::long)
    (inline local/value-occurrence::long ::local/value)
    (inline local/value-occurrence-set! ::local/value ::long)
    (inline local/value-removable::obj ::local/value)
    (inline local/value-removable-set! ::local/value ::obj)
    (inline local/value-fast-alpha::obj ::local/value)
    (inline local/value-fast-alpha-set! ::local/value ::obj)
    (inline local/value-access::obj ::local/value)
    (inline local/value-access-set! ::local/value ::obj)
    (inline local/value-value::value ::local/value)
    (inline local/value-value-set! ::local/value ::value)
    (inline local/value-type::type ::local/value)
    (inline local/value-type-set! ::local/value ::type)
    (inline local/value-name::obj ::local/value)
    (inline local/value-name-set! ::local/value ::obj)
    (inline local/value-id::symbol ::local/value)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; local/value
(define-inline (make-local/value::local/value id1170::symbol name1171::obj type1172::type value1173::value access1174::obj fast-alpha1175::obj removable1176::obj occurrence1177::long occurrencew1178::long user?1179::bool key1180::long stamp1181::int node1182::node) (instantiate::local/value (id id1170) (name name1171) (type type1172) (value value1173) (access access1174) (fast-alpha fast-alpha1175) (removable removable1176) (occurrence occurrence1177) (occurrencew occurrencew1178) (user? user?1179) (key key1180) (stamp stamp1181) (node node1182)))
(define-inline (local/value?::bool obj::obj) ((@ isa? __object) obj (@ local/value dataflow_walk)))
(define (local/value-nil::local/value) (class-nil (@ local/value dataflow_walk)))
(define-inline (local/value-node::node o::local/value) (with-access::local/value o (node) node))
(define-inline (local/value-node-set! o::local/value v::node) (with-access::local/value o (node) (set! node v)))
(define-inline (local/value-stamp::int o::local/value) (with-access::local/value o (stamp) stamp))
(define-inline (local/value-stamp-set! o::local/value v::int) (with-access::local/value o (stamp) (set! stamp v)))
(define-inline (local/value-key::long o::local/value) (with-access::local/value o (key) key))
(define-inline (local/value-key-set! o::local/value v::long) (with-access::local/value o (key) (set! key v)))
(define-inline (local/value-user?::bool o::local/value) (with-access::local/value o (user?) user?))
(define-inline (local/value-user?-set! o::local/value v::bool) (with-access::local/value o (user?) (set! user? v)))
(define-inline (local/value-occurrencew::long o::local/value) (with-access::local/value o (occurrencew) occurrencew))
(define-inline (local/value-occurrencew-set! o::local/value v::long) (with-access::local/value o (occurrencew) (set! occurrencew v)))
(define-inline (local/value-occurrence::long o::local/value) (with-access::local/value o (occurrence) occurrence))
(define-inline (local/value-occurrence-set! o::local/value v::long) (with-access::local/value o (occurrence) (set! occurrence v)))
(define-inline (local/value-removable::obj o::local/value) (with-access::local/value o (removable) removable))
(define-inline (local/value-removable-set! o::local/value v::obj) (with-access::local/value o (removable) (set! removable v)))
(define-inline (local/value-fast-alpha::obj o::local/value) (with-access::local/value o (fast-alpha) fast-alpha))
(define-inline (local/value-fast-alpha-set! o::local/value v::obj) (with-access::local/value o (fast-alpha) (set! fast-alpha v)))
(define-inline (local/value-access::obj o::local/value) (with-access::local/value o (access) access))
(define-inline (local/value-access-set! o::local/value v::obj) (with-access::local/value o (access) (set! access v)))
(define-inline (local/value-value::value o::local/value) (with-access::local/value o (value) value))
(define-inline (local/value-value-set! o::local/value v::value) (with-access::local/value o (value) (set! value v)))
(define-inline (local/value-type::type o::local/value) (with-access::local/value o (type) type))
(define-inline (local/value-type-set! o::local/value v::type) (with-access::local/value o (type) (set! type v)))
(define-inline (local/value-name::obj o::local/value) (with-access::local/value o (name) name))
(define-inline (local/value-name-set! o::local/value v::obj) (with-access::local/value o (name) (set! name v)))
(define-inline (local/value-id::symbol o::local/value) (with-access::local/value o (id) id))
(define-inline (local/value-id-set! o::local/value v::symbol) (with-access::local/value o (id) (set! id v)))
))
