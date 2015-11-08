;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Cnst/node.scm)
;; ==========================================================

;; The directives
(directives

;; local/bvalue
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-local/bvalue::local/bvalue id1114::symbol name1115::obj type1116::type value1117::value access1118::obj fast-alpha1119::obj removable1120::obj occurrence1121::long occurrencew1122::long user?1123::bool key1124::long binding-value1125::node)
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
(define-inline (make-local/bvalue::local/bvalue id1114::symbol name1115::obj type1116::type value1117::value access1118::obj fast-alpha1119::obj removable1120::obj occurrence1121::long occurrencew1122::long user?1123::bool key1124::long binding-value1125::node) (instantiate::local/bvalue (id id1114) (name name1115) (type type1116) (value value1117) (access access1118) (fast-alpha fast-alpha1119) (removable removable1120) (occurrence occurrence1121) (occurrencew occurrencew1122) (user? user?1123) (key key1124) (binding-value binding-value1125)))
(define-inline (local/bvalue?::bool obj::obj) ((@ isa? __object) obj (@ local/bvalue cnst_node)))
(define (local/bvalue-nil::local/bvalue) (class-nil (@ local/bvalue cnst_node)))
(define-inline (local/bvalue-binding-value::node o::local/bvalue) (-> |#!bigloo_wallow| o binding-value))
(define-inline (local/bvalue-binding-value-set! o::local/bvalue v::node) (set! (-> |#!bigloo_wallow| o binding-value) v))
(define-inline (local/bvalue-key::long o::local/bvalue) (-> |#!bigloo_wallow| o key))
(define-inline (local/bvalue-key-set! o::local/bvalue v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (local/bvalue-user?::bool o::local/bvalue) (-> |#!bigloo_wallow| o user?))
(define-inline (local/bvalue-user?-set! o::local/bvalue v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (local/bvalue-occurrencew::long o::local/bvalue) (-> |#!bigloo_wallow| o occurrencew))
(define-inline (local/bvalue-occurrencew-set! o::local/bvalue v::long) (set! (-> |#!bigloo_wallow| o occurrencew) v))
(define-inline (local/bvalue-occurrence::long o::local/bvalue) (-> |#!bigloo_wallow| o occurrence))
(define-inline (local/bvalue-occurrence-set! o::local/bvalue v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (local/bvalue-removable::obj o::local/bvalue) (-> |#!bigloo_wallow| o removable))
(define-inline (local/bvalue-removable-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (local/bvalue-fast-alpha::obj o::local/bvalue) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (local/bvalue-fast-alpha-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (local/bvalue-access::obj o::local/bvalue) (-> |#!bigloo_wallow| o access))
(define-inline (local/bvalue-access-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (local/bvalue-value::value o::local/bvalue) (-> |#!bigloo_wallow| o value))
(define-inline (local/bvalue-value-set! o::local/bvalue v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (local/bvalue-type::type o::local/bvalue) (-> |#!bigloo_wallow| o type))
(define-inline (local/bvalue-type-set! o::local/bvalue v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (local/bvalue-name::obj o::local/bvalue) (-> |#!bigloo_wallow| o name))
(define-inline (local/bvalue-name-set! o::local/bvalue v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (local/bvalue-id::symbol o::local/bvalue) (-> |#!bigloo_wallow| o id))
(define-inline (local/bvalue-id-set! o::local/bvalue v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
