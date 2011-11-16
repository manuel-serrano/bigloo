;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 16:42:36 CET 2011 
;; (bigloo.new -classgen Module/impuse.scm)
;; ==========================================================

;; The directives
(directives

;; import
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-import::import module1130::symbol number1131::long mode1132::symbol vars1133::obj checksum1134::obj loc1135::obj src1136::obj decl1137::obj provide1138::pair-nil code1139::obj access1140::obj)
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
(define-inline (make-import::import module1130::symbol number1131::long mode1132::symbol vars1133::obj checksum1134::obj loc1135::obj src1136::obj decl1137::obj provide1138::pair-nil code1139::obj access1140::obj) (instantiate::import (module module1130) (number number1131) (mode mode1132) (vars vars1133) (checksum checksum1134) (loc loc1135) (src src1136) (decl decl1137) (provide provide1138) (code code1139) (access access1140)))
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
(define-inline (import-vars::obj o::import) (with-access::import o (vars) vars))
(define-inline (import-vars-set! o::import v::obj) (with-access::import o (vars) (set! vars v)))
(define-inline (import-mode::symbol o::import) (with-access::import o (mode) mode))
(define-inline (import-mode-set! o::import v::symbol) (with-access::import o (mode) (set! mode v)))
(define-inline (import-number::long o::import) (with-access::import o (number) number))
(define-inline (import-number-set! o::import v::long) (with-access::import o (number) (set! number v)))
(define-inline (import-module::symbol o::import) (with-access::import o (module) module))
(define-inline (import-module-set! o::import v::symbol) (with-access::import o (module) (set! module v)))
))
