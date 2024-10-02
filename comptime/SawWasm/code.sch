;; ==========================================================
;; Class accessors
;; Bigloo (4.6a)
;; Inria -- Sophia Antipolis     Fri Sep 27 06:51:53 AM CEST 2024 
;; (bigloo -classgen SawWasm/code.scm)
;; ==========================================================

;; The directives
(directives

;; wasm_local
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-wasm_local::wasm_local type1183::type var1184::obj onexpr?1185::obj name1186::obj key1187::obj debugname1188::obj hardware1189::obj index1190::obj nullable1191::bool)
    (inline wasm_local?::bool ::obj)
    (wasm_local-nil::wasm_local)
    (inline wasm_local-nullable::bool ::wasm_local)
    (inline wasm_local-nullable-set! ::wasm_local ::bool)
    (inline wasm_local-index::obj ::wasm_local)
    (inline wasm_local-index-set! ::wasm_local ::obj)
    (inline wasm_local-hardware::obj ::wasm_local)
    (inline wasm_local-debugname::obj ::wasm_local)
    (inline wasm_local-debugname-set! ::wasm_local ::obj)
    (inline wasm_local-key::obj ::wasm_local)
    (inline wasm_local-name::obj ::wasm_local)
    (inline wasm_local-onexpr?::obj ::wasm_local)
    (inline wasm_local-onexpr?-set! ::wasm_local ::obj)
    (inline wasm_local-var::obj ::wasm_local)
    (inline wasm_local-var-set! ::wasm_local ::obj)
    (inline wasm_local-type::type ::wasm_local)
    (inline wasm_local-type-set! ::wasm_local ::type)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; wasm_local
(define-inline (make-wasm_local::wasm_local type1183::type var1184::obj onexpr?1185::obj name1186::obj key1187::obj debugname1188::obj hardware1189::obj index1190::obj nullable1191::bool) (instantiate::wasm_local (type type1183) (var var1184) (onexpr? onexpr?1185) (name name1186) (key key1187) (debugname debugname1188) (hardware hardware1189) (index index1190) (nullable nullable1191)))
(define-inline (wasm_local?::bool obj::obj) ((@ isa? __object) obj (@ wasm_local saw_wasm_code)))
(define (wasm_local-nil::wasm_local) (class-nil (@ wasm_local saw_wasm_code)))
(define-inline (wasm_local-nullable::bool o::wasm_local) (-> |#!bigloo_wallow| o nullable))
(define-inline (wasm_local-nullable-set! o::wasm_local v::bool) (set! (-> |#!bigloo_wallow| o nullable) v))
(define-inline (wasm_local-index::obj o::wasm_local) (-> |#!bigloo_wallow| o index))
(define-inline (wasm_local-index-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o index) v))
(define-inline (wasm_local-hardware::obj o::wasm_local) (-> |#!bigloo_wallow| o hardware))
(define-inline (wasm_local-hardware-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o hardware) v))
(define-inline (wasm_local-debugname::obj o::wasm_local) (-> |#!bigloo_wallow| o debugname))
(define-inline (wasm_local-debugname-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o debugname) v))
(define-inline (wasm_local-key::obj o::wasm_local) (-> |#!bigloo_wallow| o key))
(define-inline (wasm_local-key-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (wasm_local-name::obj o::wasm_local) (-> |#!bigloo_wallow| o name))
(define-inline (wasm_local-name-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (wasm_local-onexpr?::obj o::wasm_local) (-> |#!bigloo_wallow| o onexpr?))
(define-inline (wasm_local-onexpr?-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o onexpr?) v))
(define-inline (wasm_local-var::obj o::wasm_local) (-> |#!bigloo_wallow| o var))
(define-inline (wasm_local-var-set! o::wasm_local v::obj) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (wasm_local-type::type o::wasm_local) (-> |#!bigloo_wallow| o type))
(define-inline (wasm_local-type-set! o::wasm_local v::type) (set! (-> |#!bigloo_wallow| o type) v))
))
