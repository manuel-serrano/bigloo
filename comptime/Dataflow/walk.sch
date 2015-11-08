;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Dataflow/walk.scm)
;; ==========================================================

;; The directives
(directives

;; local/value
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/value::local/value id1120::symbol name1121::obj type1122::type value1123::value access1124::obj fast-alpha1125::obj removable1126::obj occurrence1127::long occurrencew1128::long user?1129::bool key1130::long stamp1131::int node1132::node)
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
(define-inline (make-local/value::local/value id1120::symbol name1121::obj type1122::type value1123::value access1124::obj fast-alpha1125::obj removable1126::obj occurrence1127::long occurrencew1128::long user?1129::bool key1130::long stamp1131::int node1132::node) (instantiate::local/value (id id1120) (name name1121) (type type1122) (value value1123) (access access1124) (fast-alpha fast-alpha1125) (removable removable1126) (occurrence occurrence1127) (occurrencew occurrencew1128) (user? user?1129) (key key1130) (stamp stamp1131) (node node1132)))
(define-inline (local/value?::bool obj::obj) ((@ isa? __object) obj (@ local/value dataflow_walk)))
(define (local/value-nil::local/value) (class-nil (@ local/value dataflow_walk)))
(define-inline (local/value-node::node o::local/value) (-> |#!bigloo_wallow| o node))
(define-inline (local/value-node-set! o::local/value v::node) (set! (-> |#!bigloo_wallow| o node) v))
(define-inline (local/value-stamp::int o::local/value) (-> |#!bigloo_wallow| o stamp))
(define-inline (local/value-stamp-set! o::local/value v::int) (set! (-> |#!bigloo_wallow| o stamp) v))
(define-inline (local/value-key::long o::local/value) (-> |#!bigloo_wallow| o key))
(define-inline (local/value-key-set! o::local/value v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (local/value-user?::bool o::local/value) (-> |#!bigloo_wallow| o user?))
(define-inline (local/value-user?-set! o::local/value v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (local/value-occurrencew::long o::local/value) (-> |#!bigloo_wallow| o occurrencew))
(define-inline (local/value-occurrencew-set! o::local/value v::long) (set! (-> |#!bigloo_wallow| o occurrencew) v))
(define-inline (local/value-occurrence::long o::local/value) (-> |#!bigloo_wallow| o occurrence))
(define-inline (local/value-occurrence-set! o::local/value v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (local/value-removable::obj o::local/value) (-> |#!bigloo_wallow| o removable))
(define-inline (local/value-removable-set! o::local/value v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (local/value-fast-alpha::obj o::local/value) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (local/value-fast-alpha-set! o::local/value v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (local/value-access::obj o::local/value) (-> |#!bigloo_wallow| o access))
(define-inline (local/value-access-set! o::local/value v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (local/value-value::value o::local/value) (-> |#!bigloo_wallow| o value))
(define-inline (local/value-value-set! o::local/value v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (local/value-type::type o::local/value) (-> |#!bigloo_wallow| o type))
(define-inline (local/value-type-set! o::local/value v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (local/value-name::obj o::local/value) (-> |#!bigloo_wallow| o name))
(define-inline (local/value-name-set! o::local/value v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (local/value-id::symbol o::local/value) (-> |#!bigloo_wallow| o id))
(define-inline (local/value-id-set! o::local/value v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
