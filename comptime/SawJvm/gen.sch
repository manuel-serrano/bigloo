;; ==========================================================
;; Class accessors
;; Bigloo (4.7b)
;; Inria -- Sophia Antipolis     Mon Mar 30 08:59:44 AM CEST 2026 
;; (bigloo.new -classgen SawJvm/gen.scm)
;; ==========================================================

;; The directives
(directives

;; lreg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-lreg::lreg type1193::type var1194::obj onexpr?1195::obj name1196::obj key1197::obj debugname1198::obj hardware1199::obj id1200::obj)
    (inline lreg?::bool ::obj)
    (lreg-nil::lreg)
    (inline lreg-id::obj ::lreg)
    (inline lreg-id-set! ::lreg ::obj)
    (inline lreg-hardware::obj ::lreg)
    (inline lreg-debugname::obj ::lreg)
    (inline lreg-debugname-set! ::lreg ::obj)
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
    (inline make-liveblock::liveblock label1185::int preds1186::pair-nil succs1187::pair-nil first1188::pair-nil in1189::obj out1190::obj)
    (inline liveblock?::bool ::obj)
    (liveblock-nil::liveblock)
    (inline liveblock-out::obj ::liveblock)
    (inline liveblock-out-set! ::liveblock ::obj)
    (inline liveblock-in::obj ::liveblock)
    (inline liveblock-in-set! ::liveblock ::obj)
    (inline liveblock-first::pair-nil ::liveblock)
    (inline liveblock-first-set! ::liveblock ::pair-nil)
    (inline liveblock-succs::pair-nil ::liveblock)
    (inline liveblock-succs-set! ::liveblock ::pair-nil)
    (inline liveblock-preds::pair-nil ::liveblock)
    (inline liveblock-preds-set! ::liveblock ::pair-nil)
    (inline liveblock-label::int ::liveblock)
    (inline liveblock-label-set! ::liveblock ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; lreg
(define-inline (make-lreg::lreg type1193::type var1194::obj onexpr?1195::obj name1196::obj key1197::obj debugname1198::obj hardware1199::obj id1200::obj) (instantiate::lreg (type type1193) (var var1194) (onexpr? onexpr?1195) (name name1196) (key key1197) (debugname debugname1198) (hardware hardware1199) (id id1200)))
(define-inline (lreg?::bool obj::obj) ((@ isa? __object) obj (@ lreg saw_jvm_gen)))
(define (lreg-nil::lreg) (class-nil (@ lreg saw_jvm_gen)))
(define-inline (lreg-id::obj o::lreg) (-> |#!bigloo_wallow| o id))
(define-inline (lreg-id-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (lreg-hardware::obj o::lreg) (-> |#!bigloo_wallow| o hardware))
(define-inline (lreg-hardware-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o hardware) v))
(define-inline (lreg-debugname::obj o::lreg) (-> |#!bigloo_wallow| o debugname))
(define-inline (lreg-debugname-set! o::lreg v::obj) (set! (-> |#!bigloo_wallow| o debugname) v))
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
(define-inline (make-liveblock::liveblock label1185::int preds1186::pair-nil succs1187::pair-nil first1188::pair-nil in1189::obj out1190::obj) (instantiate::liveblock (label label1185) (preds preds1186) (succs succs1187) (first first1188) (in in1189) (out out1190)))
(define-inline (liveblock?::bool obj::obj) ((@ isa? __object) obj (@ liveblock saw_jvm_gen)))
(define (liveblock-nil::liveblock) (class-nil (@ liveblock saw_jvm_gen)))
(define-inline (liveblock-out::obj o::liveblock) (-> |#!bigloo_wallow| o out))
(define-inline (liveblock-out-set! o::liveblock v::obj) (set! (-> |#!bigloo_wallow| o out) v))
(define-inline (liveblock-in::obj o::liveblock) (-> |#!bigloo_wallow| o in))
(define-inline (liveblock-in-set! o::liveblock v::obj) (set! (-> |#!bigloo_wallow| o in) v))
(define-inline (liveblock-first::pair-nil o::liveblock) (-> |#!bigloo_wallow| o first))
(define-inline (liveblock-first-set! o::liveblock v::pair-nil) (set! (-> |#!bigloo_wallow| o first) v))
(define-inline (liveblock-succs::pair-nil o::liveblock) (-> |#!bigloo_wallow| o succs))
(define-inline (liveblock-succs-set! o::liveblock v::pair-nil) (set! (-> |#!bigloo_wallow| o succs) v))
(define-inline (liveblock-preds::pair-nil o::liveblock) (-> |#!bigloo_wallow| o preds))
(define-inline (liveblock-preds-set! o::liveblock v::pair-nil) (set! (-> |#!bigloo_wallow| o preds) v))
(define-inline (liveblock-label::int o::liveblock) (-> |#!bigloo_wallow| o label))
(define-inline (liveblock-label-set! o::liveblock v::int) (set! (-> |#!bigloo_wallow| o label) v))
))
