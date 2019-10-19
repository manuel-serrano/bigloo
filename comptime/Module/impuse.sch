;; ==========================================================
;; Class accessors
;; Bigloo (4.3g)
;; Inria -- Sophia Antipolis     Thu 05 Sep 2019 08:47:27 AM CEST 
;; (bigloo -classgen Module/impuse.scm)
;; ==========================================================

;; The directives
(directives

;; import
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-import::import module1081::symbol number1082::long mode1083::symbol vars1084::obj aliases1085::obj prefix1086::obj checksum1087::obj loc1088::obj src1089::obj decl1090::obj provide1091::pair-nil code1092::obj access1093::obj)
    (inline import?::bool ::obj)
    (import-nil::import)
    (inline import-access::obj ::import)
    (inline import-access-set! ::import ::obj)
    (inline import-code::obj ::import)
    (inline import-code-set! ::import ::obj)
    (inline import-provide::pair-nil ::import)
    (inline import-provide-set! ::import ::pair-nil)
    (inline import-decl::obj ::import)
    (inline import-decl-set! ::import ::obj)
    (inline import-src::obj ::import)
    (inline import-loc::obj ::import)
    (inline import-checksum::obj ::import)
    (inline import-checksum-set! ::import ::obj)
    (inline import-prefix::obj ::import)
    (inline import-prefix-set! ::import ::obj)
    (inline import-aliases::obj ::import)
    (inline import-aliases-set! ::import ::obj)
    (inline import-vars::obj ::import)
    (inline import-vars-set! ::import ::obj)
    (inline import-mode::symbol ::import)
    (inline import-mode-set! ::import ::symbol)
    (inline import-number::long ::import)
    (inline import-number-set! ::import ::long)
    (inline import-module::symbol ::import)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; import
(define-inline (make-import::import module1081::symbol number1082::long mode1083::symbol vars1084::obj aliases1085::obj prefix1086::obj checksum1087::obj loc1088::obj src1089::obj decl1090::obj provide1091::pair-nil code1092::obj access1093::obj) (instantiate::import (module module1081) (number number1082) (mode mode1083) (vars vars1084) (aliases aliases1085) (prefix prefix1086) (checksum checksum1087) (loc loc1088) (src src1089) (decl decl1090) (provide provide1091) (code code1092) (access access1093)))
(define-inline (import?::bool obj::obj) ((@ isa? __object) obj (@ import module_impuse)))
(define (import-nil::import) (class-nil (@ import module_impuse)))
(define-inline (import-access::obj o::import) (-> |#!bigloo_wallow| o access))
(define-inline (import-access-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o access) v))
(define-inline (import-code::obj o::import) (-> |#!bigloo_wallow| o code))
(define-inline (import-code-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o code) v))
(define-inline (import-provide::pair-nil o::import) (-> |#!bigloo_wallow| o provide))
(define-inline (import-provide-set! o::import v::pair-nil) (set! (-> |#!bigloo_wallow| o provide) v))
(define-inline (import-decl::obj o::import) (-> |#!bigloo_wallow| o decl))
(define-inline (import-decl-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o decl) v))
(define-inline (import-src::obj o::import) (-> |#!bigloo_wallow| o src))
(define-inline (import-src-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o src) v))
(define-inline (import-loc::obj o::import) (-> |#!bigloo_wallow| o loc))
(define-inline (import-loc-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
(define-inline (import-checksum::obj o::import) (-> |#!bigloo_wallow| o checksum))
(define-inline (import-checksum-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o checksum) v))
(define-inline (import-prefix::obj o::import) (-> |#!bigloo_wallow| o prefix))
(define-inline (import-prefix-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o prefix) v))
(define-inline (import-aliases::obj o::import) (-> |#!bigloo_wallow| o aliases))
(define-inline (import-aliases-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o aliases) v))
(define-inline (import-vars::obj o::import) (-> |#!bigloo_wallow| o vars))
(define-inline (import-vars-set! o::import v::obj) (set! (-> |#!bigloo_wallow| o vars) v))
(define-inline (import-mode::symbol o::import) (-> |#!bigloo_wallow| o mode))
(define-inline (import-mode-set! o::import v::symbol) (set! (-> |#!bigloo_wallow| o mode) v))
(define-inline (import-number::long o::import) (-> |#!bigloo_wallow| o number))
(define-inline (import-number-set! o::import v::long) (set! (-> |#!bigloo_wallow| o number) v))
(define-inline (import-module::symbol o::import) (-> |#!bigloo_wallow| o module))
(define-inline (import-module-set! o::import v::symbol) (set! (-> |#!bigloo_wallow| o module) v))
))
