;; ==========================================================
;; Class accessors
;; Bigloo (4.6a)
;; Inria -- Sophia Antipolis     Fri Sep 27 07:07:54 AM CEST 2024 
;; (bigloo -classgen SawWasm/code.scm)
;; ==========================================================

;; The directives
(directives

;; SawLocalReg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-SawLocalReg::SawLocalReg type1183::type var1184::obj onexpr?1185::obj name1186::obj key1187::obj debugname1188::obj hardware1189::obj index1190::obj nullable1191::bool)
    (inline SawLocalReg?::bool ::obj)
    (SawLocalReg-nil::SawLocalReg)
    (inline SawLocalReg-nullable::bool ::SawLocalReg)
    (inline SawLocalReg-nullable-set! ::SawLocalReg ::bool)
    (inline SawLocalReg-index::obj ::SawLocalReg)
    (inline SawLocalReg-index-set! ::SawLocalReg ::obj)
    (inline SawLocalReg-hardware::obj ::SawLocalReg)
    (inline SawLocalReg-debugname::obj ::SawLocalReg)
    (inline SawLocalReg-debugname-set! ::SawLocalReg ::obj)
    (inline SawLocalReg-key::obj ::SawLocalReg)
    (inline SawLocalReg-name::obj ::SawLocalReg)
    (inline SawLocalReg-onexpr?::obj ::SawLocalReg)
    (inline SawLocalReg-onexpr?-set! ::SawLocalReg ::obj)
    (inline SawLocalReg-var::obj ::SawLocalReg)
    (inline SawLocalReg-var-set! ::SawLocalReg ::obj)
    (inline SawLocalReg-type::type ::SawLocalReg)
    (inline SawLocalReg-type-set! ::SawLocalReg ::type)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; SawLocalReg
(define-inline (make-SawLocalReg::SawLocalReg type1183::type var1184::obj onexpr?1185::obj name1186::obj key1187::obj debugname1188::obj hardware1189::obj index1190::obj nullable1191::bool) (instantiate::SawLocalReg (type type1183) (var var1184) (onexpr? onexpr?1185) (name name1186) (key key1187) (debugname debugname1188) (hardware hardware1189) (index index1190) (nullable nullable1191)))
(define-inline (SawLocalReg?::bool obj::obj) ((@ isa? __object) obj (@ SawLocalReg saw_wasm_code)))
(define (SawLocalReg-nil::SawLocalReg) (class-nil (@ SawLocalReg saw_wasm_code)))
(define-inline (SawLocalReg-nullable::bool o::SawLocalReg) (-> |#!bigloo_wallow| o nullable))
(define-inline (SawLocalReg-nullable-set! o::SawLocalReg v::bool) (set! (-> |#!bigloo_wallow| o nullable) v))
(define-inline (SawLocalReg-index::obj o::SawLocalReg) (-> |#!bigloo_wallow| o index))
(define-inline (SawLocalReg-index-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o index) v))
(define-inline (SawLocalReg-hardware::obj o::SawLocalReg) (-> |#!bigloo_wallow| o hardware))
(define-inline (SawLocalReg-hardware-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o hardware) v))
(define-inline (SawLocalReg-debugname::obj o::SawLocalReg) (-> |#!bigloo_wallow| o debugname))
(define-inline (SawLocalReg-debugname-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o debugname) v))
(define-inline (SawLocalReg-key::obj o::SawLocalReg) (-> |#!bigloo_wallow| o key))
(define-inline (SawLocalReg-key-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (SawLocalReg-name::obj o::SawLocalReg) (-> |#!bigloo_wallow| o name))
(define-inline (SawLocalReg-name-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (SawLocalReg-onexpr?::obj o::SawLocalReg) (-> |#!bigloo_wallow| o onexpr?))
(define-inline (SawLocalReg-onexpr?-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o onexpr?) v))
(define-inline (SawLocalReg-var::obj o::SawLocalReg) (-> |#!bigloo_wallow| o var))
(define-inline (SawLocalReg-var-set! o::SawLocalReg v::obj) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (SawLocalReg-type::type o::SawLocalReg) (-> |#!bigloo_wallow| o type))
(define-inline (SawLocalReg-type-set! o::SawLocalReg v::type) (set! (-> |#!bigloo_wallow| o type) v))
))
