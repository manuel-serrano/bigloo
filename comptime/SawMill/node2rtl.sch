;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen SawMill/node2rtl.scm)
;; ==========================================================

;; The directives
(directives

;; area
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-area::area entry1198::block exit1199::block)
    (inline area?::bool ::obj)
    (area-nil::area)
    (inline area-exit::block ::area)
    (inline area-exit-set! ::area ::block)
    (inline area-entry::block ::area)
    (inline area-entry-set! ::area ::block))))

;; reversed
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-reversed::reversed label1193::int preds1194::pair-nil succs1195::pair-nil first1196::pair)
    (inline reversed?::bool ::obj)
    (reversed-nil::reversed)
    (inline reversed-first::pair ::reversed)
    (inline reversed-first-set! ::reversed ::pair)
    (inline reversed-succs::pair-nil ::reversed)
    (inline reversed-succs-set! ::reversed ::pair-nil)
    (inline reversed-preds::pair-nil ::reversed)
    (inline reversed-preds-set! ::reversed ::pair-nil)
    (inline reversed-label::int ::reversed)
    (inline reversed-label-set! ::reversed ::int))))

;; rlocal
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rlocal::rlocal id1179::symbol name1180::obj type1181::type value1182::value access1183::obj fast-alpha1184::obj removable1185::obj occurrence1186::long occurrencew1187::long user?1188::bool key1189::long reg1190::obj code1191::obj)
    (inline rlocal?::bool ::obj)
    (rlocal-nil::rlocal)
    (inline rlocal-code::obj ::rlocal)
    (inline rlocal-code-set! ::rlocal ::obj)
    (inline rlocal-reg::obj ::rlocal)
    (inline rlocal-reg-set! ::rlocal ::obj)
    (inline rlocal-key::long ::rlocal)
    (inline rlocal-user?::bool ::rlocal)
    (inline rlocal-user?-set! ::rlocal ::bool)
    (inline rlocal-occurrencew::long ::rlocal)
    (inline rlocal-occurrencew-set! ::rlocal ::long)
    (inline rlocal-occurrence::long ::rlocal)
    (inline rlocal-occurrence-set! ::rlocal ::long)
    (inline rlocal-removable::obj ::rlocal)
    (inline rlocal-removable-set! ::rlocal ::obj)
    (inline rlocal-fast-alpha::obj ::rlocal)
    (inline rlocal-fast-alpha-set! ::rlocal ::obj)
    (inline rlocal-access::obj ::rlocal)
    (inline rlocal-access-set! ::rlocal ::obj)
    (inline rlocal-value::value ::rlocal)
    (inline rlocal-value-set! ::rlocal ::value)
    (inline rlocal-type::type ::rlocal)
    (inline rlocal-type-set! ::rlocal ::type)
    (inline rlocal-name::obj ::rlocal)
    (inline rlocal-name-set! ::rlocal ::obj)
    (inline rlocal-id::symbol ::rlocal)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; area
(define-inline (make-area::area entry1198::block exit1199::block) (instantiate::area (entry entry1198) (exit exit1199)))
(define-inline (area?::bool obj::obj) ((@ isa? __object) obj (@ area saw_node2rtl)))
(define (area-nil::area) (class-nil (@ area saw_node2rtl)))
(define-inline (area-exit::block o::area) (-> |#!bigloo_wallow| o exit))
(define-inline (area-exit-set! o::area v::block) (set! (-> |#!bigloo_wallow| o exit) v))
(define-inline (area-entry::block o::area) (-> |#!bigloo_wallow| o entry))
(define-inline (area-entry-set! o::area v::block) (set! (-> |#!bigloo_wallow| o entry) v))

;; reversed
(define-inline (make-reversed::reversed label1193::int preds1194::pair-nil succs1195::pair-nil first1196::pair) (instantiate::reversed (label label1193) (preds preds1194) (succs succs1195) (first first1196)))
(define-inline (reversed?::bool obj::obj) ((@ isa? __object) obj (@ reversed saw_node2rtl)))
(define (reversed-nil::reversed) (class-nil (@ reversed saw_node2rtl)))
(define-inline (reversed-first::pair o::reversed) (-> |#!bigloo_wallow| o first))
(define-inline (reversed-first-set! o::reversed v::pair) (set! (-> |#!bigloo_wallow| o first) v))
(define-inline (reversed-succs::pair-nil o::reversed) (-> |#!bigloo_wallow| o succs))
(define-inline (reversed-succs-set! o::reversed v::pair-nil) (set! (-> |#!bigloo_wallow| o succs) v))
(define-inline (reversed-preds::pair-nil o::reversed) (-> |#!bigloo_wallow| o preds))
(define-inline (reversed-preds-set! o::reversed v::pair-nil) (set! (-> |#!bigloo_wallow| o preds) v))
(define-inline (reversed-label::int o::reversed) (-> |#!bigloo_wallow| o label))
(define-inline (reversed-label-set! o::reversed v::int) (set! (-> |#!bigloo_wallow| o label) v))

;; rlocal
(define-inline (make-rlocal::rlocal id1179::symbol name1180::obj type1181::type value1182::value access1183::obj fast-alpha1184::obj removable1185::obj occurrence1186::long occurrencew1187::long user?1188::bool key1189::long reg1190::obj code1191::obj) (instantiate::rlocal (id id1179) (name name1180) (type type1181) (value value1182) (access access1183) (fast-alpha fast-alpha1184) (removable removable1185) (occurrence occurrence1186) (occurrencew occurrencew1187) (user? user?1188) (key key1189) (reg reg1190) (code code1191)))
(define-inline (rlocal?::bool obj::obj) ((@ isa? __object) obj (@ rlocal saw_node2rtl)))
(define (rlocal-nil::rlocal) (class-nil (@ rlocal saw_node2rtl)))
(define-inline (rlocal-code::obj o::rlocal) (-> |#!bigloo_wallow| o code))
(define-inline (rlocal-code-set! o::rlocal v::obj) (set! (-> |#!bigloo_wallow| o code) v))
(define-inline (rlocal-reg::obj o::rlocal) (-> |#!bigloo_wallow| o reg))
(define-inline (rlocal-reg-set! o::rlocal v::obj) (set! (-> |#!bigloo_wallow| o reg) v))
(define-inline (rlocal-key::long o::rlocal) (-> |#!bigloo_wallow| o key))
(define-inline (rlocal-key-set! o::rlocal v::long) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (rlocal-user?::bool o::rlocal) (-> |#!bigloo_wallow| o user?))
(define-inline (rlocal-user?-set! o::rlocal v::bool) (set! (-> |#!bigloo_wallow| o user?) v))
(define-inline (rlocal-occurrencew::long o::rlocal) (-> |#!bigloo_wallow| o occurrencew))
(define-inline (rlocal-occurrencew-set! o::rlocal v::long) (set! (-> |#!bigloo_wallow| o occurrencew) v))
(define-inline (rlocal-occurrence::long o::rlocal) (-> |#!bigloo_wallow| o occurrence))
(define-inline (rlocal-occurrence-set! o::rlocal v::long) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (rlocal-removable::obj o::rlocal) (-> |#!bigloo_wallow| o removable))
(define-inline (rlocal-removable-set! o::rlocal v::obj) (set! (-> |#!bigloo_wallow| o removable) v))
(define-inline (rlocal-fast-alpha::obj o::rlocal) (-> |#!bigloo_wallow| o fast-alpha))
(define-inline (rlocal-fast-alpha-set! o::rlocal v::obj) (set! (-> |#!bigloo_wallow| o fast-alpha) v))
(define-inline (rlocal-access::obj o::rlocal) (-> |#!bigloo_wallow| o access))
(define-inline (rlocal-access-set! o::rlocal v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (rlocal-value::value o::rlocal) (-> |#!bigloo_wallow| o value))
(define-inline (rlocal-value-set! o::rlocal v::value) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (rlocal-type::type o::rlocal) (-> |#!bigloo_wallow| o type))
(define-inline (rlocal-type-set! o::rlocal v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (rlocal-name::obj o::rlocal) (-> |#!bigloo_wallow| o name))
(define-inline (rlocal-name-set! o::rlocal v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (rlocal-id::symbol o::rlocal) (-> |#!bigloo_wallow| o id))
(define-inline (rlocal-id-set! o::rlocal v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
