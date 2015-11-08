;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen SawJvm/code.scm)
;; ==========================================================

;; The directives
(directives

;; lreg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-lreg::lreg type1183::type var1184::obj onexpr?1185::obj name1186::obj key1187::obj hardware1188::obj id1189::obj)
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
    (inline make-liveblock::liveblock label1175::int preds1176::pair-nil succs1177::pair-nil first1178::pair in1179::obj out1180::obj)
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
(define-inline (make-lreg::lreg type1183::type var1184::obj onexpr?1185::obj name1186::obj key1187::obj hardware1188::obj id1189::obj) (instantiate::lreg (type type1183) (var var1184) (onexpr? onexpr?1185) (name name1186) (key key1187) (hardware hardware1188) (id id1189)))
(define-inline (lreg?::bool obj::obj) ((@ isa? __object) obj (@ lreg saw_jvm_code)))
(define (lreg-nil::lreg) (class-nil (@ lreg saw_jvm_code)))
(define-inline (lreg-id::obj o::lreg) (-> |#!bigloo_wallow| o id))
(define-inline (lreg-id-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (lreg-hardware::obj o::lreg) (-> |#!bigloo_wallow| o hardware))
(define-inline (lreg-hardware-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o hardware) v))
(define-inline (lreg-key::obj o::lreg) (-> |#!bigloo_wallow| o key))
(define-inline (lreg-key-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (lreg-name::obj o::lreg) (-> |#!bigloo_wallow| o name))
(define-inline (lreg-name-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (lreg-onexpr?::obj o::lreg) (-> |#!bigloo_wallow| o onexpr?))
(define-inline (lreg-onexpr?-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o onexpr?) v))
(define-inline (lreg-var::obj o::lreg) (-> |#!bigloo_wallow| o var))
(define-inline (lreg-var-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (lreg-type::type o::lreg) (-> |#!bigloo_wallow| o type))
(define-inline (lreg-type-set! o::lreg v::type) (set! (-> |#!bigloo_wallow| o type) v))

;; liveblock
(define-inline (make-liveblock::liveblock label1175::int preds1176::pair-nil succs1177::pair-nil first1178::pair in1179::obj out1180::obj) (instantiate::liveblock (label label1175) (preds preds1176) (succs succs1177) (first first1178) (in in1179) (out out1180)))
(define-inline (liveblock?::bool obj::obj) ((@ isa? __object) obj (@ liveblock saw_jvm_code)))
(define (liveblock-nil::liveblock) (class-nil (@ liveblock saw_jvm_code)))
(define-inline (liveblock-out::obj o::liveblock) (-> |#!bigloo_wallow| o out))
(define-inline (liveblock-out-set! o::liveblock v::obj) (set! (-> |#!bigloo_wallow| o out) v))
(define-inline (liveblock-in::obj o::liveblock) (-> |#!bigloo_wallow| o in))
(define-inline (liveblock-in-set! o::liveblock v::obj) (set! (-> |#!bigloo_wallow| o in) v))
(define-inline (liveblock-first::pair o::liveblock) (-> |#!bigloo_wallow| o first))
(define-inline (liveblock-first-set! o::liveblock v::pair) (set! (-> |#!bigloo_wallow| o first) v))
(define-inline (liveblock-succs::pair-nil o::liveblock) (-> |#!bigloo_wallow| o succs))
(define-inline (liveblock-succs-set! o::liveblock v::pair-nil) (set! (-> |#!bigloo_wallow| o succs) v))
(define-inline (liveblock-preds::pair-nil o::liveblock) (-> |#!bigloo_wallow| o preds))
(define-inline (liveblock-preds-set! o::liveblock v::pair-nil) (set! (-> |#!bigloo_wallow| o preds) v))
(define-inline (liveblock-label::int o::liveblock) (-> |#!bigloo_wallow| o label))
(define-inline (liveblock-label-set! o::liveblock v::int) (set! (-> |#!bigloo_wallow| o label) v))
))
