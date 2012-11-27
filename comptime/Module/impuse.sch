;; ==========================================================
;; Class accessors
;; Bigloo (3.9b)
;; Inria -- Sophia Antipolis     Mon Nov 26 09:49:53 CET 2012 
;; (bigloo -classgen Module/impuse.scm)
;; ==========================================================

;; The directives
(directives

;; import
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-import::import module1080::symbol number1081::long mode1082::symbol vars1083::obj aliases1084::obj checksum1085::obj loc1086::obj src1087::obj decl1088::obj provide1089::pair-nil code1090::obj access1091::obj)
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
(define-inline (make-import::import module1080::symbol number1081::long mode1082::symbol vars1083::obj aliases1084::obj checksum1085::obj loc1086::obj src1087::obj decl1088::obj provide1089::pair-nil code1090::obj access1091::obj) (instantiate::import (module module1080) (number number1081) (mode mode1082) (vars vars1083) (aliases aliases1084) (checksum checksum1085) (loc loc1086) (src src1087) (decl decl1088) (provide provide1089) (code code1090) (access access1091)))
(define-inline (import?::bool obj::obj) ((@ isa? __object) obj (@ import module_impuse)))
(define (import-nil::import) (class-nil (@ import module_impuse)))
(define-inline (import-access::obj o::import) (with-access::import o (access) access))
(define-inline (import-access-set! o::import v::obj) (with-access::import o (access) (set! access v)))
(define-inline (import-code::obj o::import) (with-access::import o (code) code))
(define-inline (import-code-set! o::import v::obj) (with-access::import o (code) (set! code v)))
(define-inline (import-provide::pair-nil o::import) (with-access::import o (provide) provide))
(define-inline (import-provide-set! o::import v::pair-nil) (with-access::import o (provide) (set! provide v)))
(define-inline (import-decl::obj o::import) (with-access::import o (decl) decl))
(define-inline (import-decl-set! o::import v::obj) (with-access::import o (decl) (set! decl v)))
(define-inline (import-src::obj o::import) (with-access::import o (src) src))
(define-inline (import-src-set! o::import v::obj) (with-access::import o (src) (set! src v)))
(define-inline (import-loc::obj o::import) (with-access::import o (loc) loc))
(define-inline (import-loc-set! o::import v::obj) (with-access::import o (loc) (set! loc v)))
(define-inline (import-checksum::obj o::import) (with-access::import o (checksum) checksum))
(define-inline (import-checksum-set! o::import v::obj) (with-access::import o (checksum) (set! checksum v)))
(define-inline (import-aliases::obj o::import) (with-access::import o (aliases) aliases))
(define-inline (import-aliases-set! o::import v::obj) (with-access::import o (aliases) (set! aliases v)))
(define-inline (import-vars::obj o::import) (with-access::import o (vars) vars))
(define-inline (import-vars-set! o::import v::obj) (with-access::import o (vars) (set! vars v)))
(define-inline (import-mode::symbol o::import) (with-access::import o (mode) mode))
(define-inline (import-mode-set! o::import v::symbol) (with-access::import o (mode) (set! mode v)))
(define-inline (import-number::long o::import) (with-access::import o (number) number))
(define-inline (import-number-set! o::import v::long) (with-access::import o (number) (set! number v)))
(define-inline (import-module::symbol o::import) (with-access::import o (module) module))
(define-inline (import-module-set! o::import v::symbol) (with-access::import o (module) (set! module v)))
))
