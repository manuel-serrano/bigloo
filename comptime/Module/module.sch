;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Module/module.scm)
;; ==========================================================

;; The directives
(directives

;; ccomp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ccomp::ccomp id1019::symbol producer1020::procedure consumer1021::procedure finalizer1022::procedure)
    (inline ccomp?::bool ::obj)
    (ccomp-nil::ccomp)
    (inline ccomp-finalizer::procedure ::ccomp)
    (inline ccomp-consumer::procedure ::ccomp)
    (inline ccomp-producer::procedure ::ccomp)
    (inline ccomp-id::symbol ::ccomp)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ccomp
(define-inline (make-ccomp::ccomp id1019::symbol producer1020::procedure consumer1021::procedure finalizer1022::procedure) (instantiate::ccomp (id id1019) (producer producer1020) (consumer consumer1021) (finalizer finalizer1022)))
(define-inline (ccomp?::bool obj::obj) ((@ isa? __object) obj (@ ccomp module_module)))
(define (ccomp-nil::ccomp) (class-nil (@ ccomp module_module)))
(define-inline (ccomp-finalizer::procedure o::ccomp) (-> |#!bigloo_wallow| o finalizer))
(define-inline (ccomp-finalizer-set! o::ccomp v::procedure) (set! (-> |#!bigloo_wallow| o finalizer) v))
(define-inline (ccomp-consumer::procedure o::ccomp) (-> |#!bigloo_wallow| o consumer))
(define-inline (ccomp-consumer-set! o::ccomp v::procedure) (set! (-> |#!bigloo_wallow| o consumer) v))
(define-inline (ccomp-producer::procedure o::ccomp) (-> |#!bigloo_wallow| o producer))
(define-inline (ccomp-producer-set! o::ccomp v::procedure) (set! (-> |#!bigloo_wallow| o producer) v))
(define-inline (ccomp-id::symbol o::ccomp) (-> |#!bigloo_wallow| o id))
(define-inline (ccomp-id-set! o::ccomp v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
