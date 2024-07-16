;; ==========================================================
;; Class accessors
;; Bigloo (4.6a)
;; Inria -- Sophia Antipolis     Fri Jun 14 12:43:15 CEST 2024 
;; (bigloo -classgen SawWasm/code.scm)
;; ==========================================================

;; The directives
(directives

;; SawCIreg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-SawCIreg::SawCIreg type1171::type var1172::obj onexpr?1173::obj name1174::obj key1175::obj debugname1176::obj hardware1177::obj index1178::obj)
    (inline SawCIreg?::bool ::obj)
    (SawCIreg-nil::SawCIreg)
    (inline SawCIreg-index::obj ::SawCIreg)
    (inline SawCIreg-index-set! ::SawCIreg ::obj)
    (inline SawCIreg-hardware::obj ::SawCIreg)
    (inline SawCIreg-debugname::obj ::SawCIreg)
    (inline SawCIreg-debugname-set! ::SawCIreg ::obj)
    (inline SawCIreg-key::obj ::SawCIreg)
    (inline SawCIreg-name::obj ::SawCIreg)
    (inline SawCIreg-onexpr?::obj ::SawCIreg)
    (inline SawCIreg-onexpr?-set! ::SawCIreg ::obj)
    (inline SawCIreg-var::obj ::SawCIreg)
    (inline SawCIreg-var-set! ::SawCIreg ::obj)
    (inline SawCIreg-type::type ::SawCIreg)
    (inline SawCIreg-type-set! ::SawCIreg ::type)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; SawCIreg
(define-inline (make-SawCIreg::SawCIreg type1171::type var1172::obj onexpr?1173::obj name1174::obj key1175::obj debugname1176::obj hardware1177::obj index1178::obj) (instantiate::SawCIreg (type type1171) (var var1172) (onexpr? onexpr?1173) (name name1174) (key key1175) (debugname debugname1176) (hardware hardware1177) (index index1178)))
(define-inline (SawCIreg?::bool obj::obj) ((@ isa? __object) obj (@ SawCIreg saw_wasm_code)))
(define (SawCIreg-nil::SawCIreg) (class-nil (@ SawCIreg saw_wasm_code)))
(define-inline (SawCIreg-index::obj o::SawCIreg) (-> |#!bigloo_wallow| o index))
(define-inline (SawCIreg-index-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o index) v))
(define-inline (SawCIreg-hardware::obj o::SawCIreg) (-> |#!bigloo_wallow| o hardware))
(define-inline (SawCIreg-hardware-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o hardware) v))
(define-inline (SawCIreg-debugname::obj o::SawCIreg) (-> |#!bigloo_wallow| o debugname))
(define-inline (SawCIreg-debugname-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o debugname) v))
(define-inline (SawCIreg-key::obj o::SawCIreg) (-> |#!bigloo_wallow| o key))
(define-inline (SawCIreg-key-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (SawCIreg-name::obj o::SawCIreg) (-> |#!bigloo_wallow| o name))
(define-inline (SawCIreg-name-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (SawCIreg-onexpr?::obj o::SawCIreg) (-> |#!bigloo_wallow| o onexpr?))
(define-inline (SawCIreg-onexpr?-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o onexpr?) v))
(define-inline (SawCIreg-var::obj o::SawCIreg) (-> |#!bigloo_wallow| o var))
(define-inline (SawCIreg-var-set! o::SawCIreg v::obj) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (SawCIreg-type::type o::SawCIreg) (-> |#!bigloo_wallow| o type))
(define-inline (SawCIreg-type-set! o::SawCIreg v::type) (set! (-> |#!bigloo_wallow| o type) v))
))
