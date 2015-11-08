;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new eval.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; evpt
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-evpt::evpt x1018::obj y1019::obj)
    (inline evpt?::bool ::obj)
    (evpt-nil::evpt)
    (inline evpt-y::obj ::evpt)
    (inline evpt-x::obj ::evpt)
    (inline evpt-x-set! ::evpt ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; evpt
(define-inline (make-evpt::evpt x1018::obj y1019::obj) (instantiate::evpt (x x1018) (y y1019)))
(define-inline (evpt?::bool obj::obj) ((@ isa? __object) obj (@ evpt reval)))
(define (evpt-nil::evpt) (class-nil (@ evpt reval)))
(define-inline (evpt-y::obj o::evpt) (-> |#!bigloo_wallow| o y))
#f
(define-inline (evpt-x::obj o::evpt) (-> |#!bigloo_wallow| o x))
(define-inline (evpt-x-set! o::evpt v::obj) (set! (-> |#!bigloo_wallow| o x) v))
))
