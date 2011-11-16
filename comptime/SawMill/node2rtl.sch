;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 18:35:27 CET 2011 
;; (bigloo.new -classgen SawMill/node2rtl.scm)
;; ==========================================================

;; The directives
(directives

;; area
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-area::area entry1246::block exit1247::block)
    (inline area?::bool ::obj)
    (area-nil::area)
    (inline area-exit::block ::area)
    (inline area-exit-set! ::area ::block)
    (inline area-entry::block ::area)
    (inline area-entry-set! ::area ::block))))

;; reversed
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-reversed::reversed label1241::int preds1242::pair-nil succs1243::pair-nil first1244::pair)
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
    (inline make-rlocal::rlocal id1227::symbol name1228::obj type1229::type value1230::value access1231::obj fast-alpha1232::obj removable1233::obj occurrence1234::long occurrencew1235::long user?1236::bool key1237::long reg1238::obj code1239::obj)
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
(define-inline (make-area::area entry1246::block exit1247::block) (instantiate::area (entry entry1246) (exit exit1247)))
(define-inline (area?::bool obj::obj) ((@ isa? __object) obj (@ area saw_node2rtl)))
(define (area-nil::area) (class-nil (@ area saw_node2rtl)))
(define-inline (area-exit::block o::area) (with-access::area o (exit) exit))
(define-inline (area-exit-set! o::area v::block) (with-access::area o (exit) (set! exit v)))
(define-inline (area-entry::block o::area) (with-access::area o (entry) entry))
(define-inline (area-entry-set! o::area v::block) (with-access::area o (entry) (set! entry v)))

;; reversed
(define-inline (make-reversed::reversed label1241::int preds1242::pair-nil succs1243::pair-nil first1244::pair) (instantiate::reversed (label label1241) (preds preds1242) (succs succs1243) (first first1244)))
(define-inline (reversed?::bool obj::obj) ((@ isa? __object) obj (@ reversed saw_node2rtl)))
(define (reversed-nil::reversed) (class-nil (@ reversed saw_node2rtl)))
(define-inline (reversed-first::pair o::reversed) (with-access::reversed o (first) first))
(define-inline (reversed-first-set! o::reversed v::pair) (with-access::reversed o (first) (set! first v)))
(define-inline (reversed-succs::pair-nil o::reversed) (with-access::reversed o (succs) succs))
(define-inline (reversed-succs-set! o::reversed v::pair-nil) (with-access::reversed o (succs) (set! succs v)))
(define-inline (reversed-preds::pair-nil o::reversed) (with-access::reversed o (preds) preds))
(define-inline (reversed-preds-set! o::reversed v::pair-nil) (with-access::reversed o (preds) (set! preds v)))
(define-inline (reversed-label::int o::reversed) (with-access::reversed o (label) label))
(define-inline (reversed-label-set! o::reversed v::int) (with-access::reversed o (label) (set! label v)))

;; rlocal
(define-inline (make-rlocal::rlocal id1227::symbol name1228::obj type1229::type value1230::value access1231::obj fast-alpha1232::obj removable1233::obj occurrence1234::long occurrencew1235::long user?1236::bool key1237::long reg1238::obj code1239::obj) (instantiate::rlocal (id id1227) (name name1228) (type type1229) (value value1230) (access access1231) (fast-alpha fast-alpha1232) (removable removable1233) (occurrence occurrence1234) (occurrencew occurrencew1235) (user? user?1236) (key key1237) (reg reg1238) (code code1239)))
(define-inline (rlocal?::bool obj::obj) ((@ isa? __object) obj (@ rlocal saw_node2rtl)))
(define (rlocal-nil::rlocal) (class-nil (@ rlocal saw_node2rtl)))
(define-inline (rlocal-code::obj o::rlocal) (with-access::rlocal o (code) code))
(define-inline (rlocal-code-set! o::rlocal v::obj) (with-access::rlocal o (code) (set! code v)))
(define-inline (rlocal-reg::obj o::rlocal) (with-access::rlocal o (reg) reg))
(define-inline (rlocal-reg-set! o::rlocal v::obj) (with-access::rlocal o (reg) (set! reg v)))
(define-inline (rlocal-key::long o::rlocal) (with-access::rlocal o (key) key))
(define-inline (rlocal-key-set! o::rlocal v::long) (with-access::rlocal o (key) (set! key v)))
(define-inline (rlocal-user?::bool o::rlocal) (with-access::rlocal o (user?) user?))
(define-inline (rlocal-user?-set! o::rlocal v::bool) (with-access::rlocal o (user?) (set! user? v)))
(define-inline (rlocal-occurrencew::long o::rlocal) (with-access::rlocal o (occurrencew) occurrencew))
(define-inline (rlocal-occurrencew-set! o::rlocal v::long) (with-access::rlocal o (occurrencew) (set! occurrencew v)))
(define-inline (rlocal-occurrence::long o::rlocal) (with-access::rlocal o (occurrence) occurrence))
(define-inline (rlocal-occurrence-set! o::rlocal v::long) (with-access::rlocal o (occurrence) (set! occurrence v)))
(define-inline (rlocal-removable::obj o::rlocal) (with-access::rlocal o (removable) removable))
(define-inline (rlocal-removable-set! o::rlocal v::obj) (with-access::rlocal o (removable) (set! removable v)))
(define-inline (rlocal-fast-alpha::obj o::rlocal) (with-access::rlocal o (fast-alpha) fast-alpha))
(define-inline (rlocal-fast-alpha-set! o::rlocal v::obj) (with-access::rlocal o (fast-alpha) (set! fast-alpha v)))
(define-inline (rlocal-access::obj o::rlocal) (with-access::rlocal o (access) access))
(define-inline (rlocal-access-set! o::rlocal v::obj) (with-access::rlocal o (access) (set! access v)))
(define-inline (rlocal-value::value o::rlocal) (with-access::rlocal o (value) value))
(define-inline (rlocal-value-set! o::rlocal v::value) (with-access::rlocal o (value) (set! value v)))
(define-inline (rlocal-type::type o::rlocal) (with-access::rlocal o (type) type))
(define-inline (rlocal-type-set! o::rlocal v::type) (with-access::rlocal o (type) (set! type v)))
(define-inline (rlocal-name::obj o::rlocal) (with-access::rlocal o (name) name))
(define-inline (rlocal-name-set! o::rlocal v::obj) (with-access::rlocal o (name) (set! name v)))
(define-inline (rlocal-id::symbol o::rlocal) (with-access::rlocal o (id) id))
(define-inline (rlocal-id-set! o::rlocal v::symbol) (with-access::rlocal o (id) (set! id v)))
))
