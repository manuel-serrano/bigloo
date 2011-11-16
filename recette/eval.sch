;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Sat Nov 12 21:05:01 CET 2011 
;; (bigloo.new eval.scm -classgen)
;; ==========================================================

;; The directives
(directives
   (cond-expand ((and bigloo-class-sans (not bigloo-class-generate))

;; evpt
(static
  (inline evpt?::bool ::obj)
  (inline make-evpt::evpt x1100::obj y1101::obj)
  (evpt-nil::evpt)
  (inline evpt-y::obj ::evpt)
  (inline evpt-x::obj ::evpt)
  (inline evpt-x-set! ::evpt ::obj)
)
)))

;; The definitions
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate));; evpt
(define-inline (evpt?::bool obj::obj) ((@ isa? __object) obj (@ evpt reval)))
(define-inline (make-evpt::evpt x1100::obj y1101::obj) (instantiate::evpt (x x1100) (y y1101)))
(define (evpt-nil::evpt) (let ((new1102::evpt (let ((new1103::evpt (free-pragma::evpt "((BgL_evptz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_evptz00_bgl) )))"))) (object-class-num-set! new1103 ((@ class-num __object) (@ evpt reval))) (object-widening-set! new1103 #f) new1103))) (set! (-> |#!bigloo| new1102 x) #unspecified) (set! (-> |#!bigloo| new1102 y) #unspecified) new1102))
(define-inline (evpt-y::obj o::evpt) (with-access::evpt o (y) y))
(define-inline (evpt-y-set! o::evpt v::obj) (with-access::evpt o (y) (set! y v)))
(define-inline (evpt-x::obj o::evpt) (with-access::evpt o (x) x))
(define-inline (evpt-x-set! o::evpt v::obj) (with-access::evpt o (x) (set! x v)))

))
