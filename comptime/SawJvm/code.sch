;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:08:36 CET 2011 
;; (bigloo.new -classgen SawJvm/code.scm)
;; ==========================================================

;; The directives
(directives

;; lreg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-lreg::lreg type1235::type var1236::obj onexpr?1237::obj name1238::obj key1239::obj hardware1240::obj id1241::obj)
    (inline lreg?::bool ::obj)
    (lreg-nil::lreg)
    (inline lreg-id::obj ::lreg)
    (inline lreg-id-set! ::lreg ::obj)
    (inline lreg-hardware::obj ::lreg)
    (inline lreg-key::obj ::lreg)
    (inline lreg-name::obj ::lreg)
    (inline lreg-onexpr?::obj ::lreg)
    (inline lreg-onexpr?-set! ::lreg ::obj)
    (inline lreg-var::obj ::lreg)
    (inline lreg-var-set! ::lreg ::obj)
    (inline lreg-type::type ::lreg)
    (inline lreg-type-set! ::lreg ::type))))

;; liveblock
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-liveblock::liveblock label1228::int preds1229::pair-nil succs1230::pair-nil first1231::pair in1232::obj out1233::obj)
    (inline liveblock?::bool ::obj)
    (liveblock-nil::liveblock)
    (inline liveblock-out::obj ::liveblock)
    (inline liveblock-out-set! ::liveblock ::obj)
    (inline liveblock-in::obj ::liveblock)
    (inline liveblock-in-set! ::liveblock ::obj)
    (inline liveblock-first::pair ::liveblock)
    (inline liveblock-first-set! ::liveblock ::pair)
    (inline liveblock-succs::pair-nil ::liveblock)
    (inline liveblock-succs-set! ::liveblock ::pair-nil)
    (inline liveblock-preds::pair-nil ::liveblock)
    (inline liveblock-preds-set! ::liveblock ::pair-nil)
    (inline liveblock-label::int ::liveblock)
    (inline liveblock-label-set! ::liveblock ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; lreg
(define-inline (make-lreg::lreg type1235::type var1236::obj onexpr?1237::obj name1238::obj key1239::obj hardware1240::obj id1241::obj) (instantiate::lreg (type type1235) (var var1236) (onexpr? onexpr?1237) (name name1238) (key key1239) (hardware hardware1240) (id id1241)))
(define-inline (lreg?::bool obj::obj) ((@ isa? __object) obj (@ lreg saw_jvm_code)))
(define (lreg-nil::lreg) (class-nil (@ lreg saw_jvm_code)))
(define-inline (lreg-id::obj o::lreg) (with-access::lreg o (id) id))
(define-inline (lreg-id-set! o::lreg v::obj) (with-access::lreg o (id) (set! id v)))
(define-inline (lreg-hardware::obj o::lreg) (with-access::lreg o (hardware) hardware))
(define-inline (lreg-hardware-set! o::lreg v::obj) (with-access::lreg o (hardware) (set! hardware v)))
(define-inline (lreg-key::obj o::lreg) (with-access::lreg o (key) key))
(define-inline (lreg-key-set! o::lreg v::obj) (with-access::lreg o (key) (set! key v)))
(define-inline (lreg-name::obj o::lreg) (with-access::lreg o (name) name))
(define-inline (lreg-name-set! o::lreg v::obj) (with-access::lreg o (name) (set! name v)))
(define-inline (lreg-onexpr?::obj o::lreg) (with-access::lreg o (onexpr?) onexpr?))
(define-inline (lreg-onexpr?-set! o::lreg v::obj) (with-access::lreg o (onexpr?) (set! onexpr? v)))
(define-inline (lreg-var::obj o::lreg) (with-access::lreg o (var) var))
(define-inline (lreg-var-set! o::lreg v::obj) (with-access::lreg o (var) (set! var v)))
(define-inline (lreg-type::type o::lreg) (with-access::lreg o (type) type))
(define-inline (lreg-type-set! o::lreg v::type) (with-access::lreg o (type) (set! type v)))

;; liveblock
(define-inline (make-liveblock::liveblock label1228::int preds1229::pair-nil succs1230::pair-nil first1231::pair in1232::obj out1233::obj) (instantiate::liveblock (label label1228) (preds preds1229) (succs succs1230) (first first1231) (in in1232) (out out1233)))
(define-inline (liveblock?::bool obj::obj) ((@ isa? __object) obj (@ liveblock saw_jvm_code)))
(define (liveblock-nil::liveblock) (class-nil (@ liveblock saw_jvm_code)))
(define-inline (liveblock-out::obj o::liveblock) (with-access::liveblock o (out) out))
(define-inline (liveblock-out-set! o::liveblock v::obj) (with-access::liveblock o (out) (set! out v)))
(define-inline (liveblock-in::obj o::liveblock) (with-access::liveblock o (in) in))
(define-inline (liveblock-in-set! o::liveblock v::obj) (with-access::liveblock o (in) (set! in v)))
(define-inline (liveblock-first::pair o::liveblock) (with-access::liveblock o (first) first))
(define-inline (liveblock-first-set! o::liveblock v::pair) (with-access::liveblock o (first) (set! first v)))
(define-inline (liveblock-succs::pair-nil o::liveblock) (with-access::liveblock o (succs) succs))
(define-inline (liveblock-succs-set! o::liveblock v::pair-nil) (with-access::liveblock o (succs) (set! succs v)))
(define-inline (liveblock-preds::pair-nil o::liveblock) (with-access::liveblock o (preds) preds))
(define-inline (liveblock-preds-set! o::liveblock v::pair-nil) (with-access::liveblock o (preds) (set! preds v)))
(define-inline (liveblock-label::int o::liveblock) (with-access::liveblock o (label) label))
(define-inline (liveblock-label-set! o::liveblock v::int) (with-access::liveblock o (label) (set! label v)))
))
