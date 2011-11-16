;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 16:42:36 CET 2011 
;; (bigloo.new -classgen Module/module.scm)
;; ==========================================================

;; The directives
(directives

;; ccomp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ccomp::ccomp id1102::symbol producer1103::procedure consumer1104::procedure finalizer1105::procedure)
    (inline ccomp?::bool ::obj)
    (ccomp-nil::ccomp)
    (inline ccomp-finalizer::procedure ::ccomp)
    (inline ccomp-consumer::procedure ::ccomp)
    (inline ccomp-producer::procedure ::ccomp)
    (inline ccomp-id::symbol ::ccomp)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ccomp
(define-inline (make-ccomp::ccomp id1102::symbol producer1103::procedure consumer1104::procedure finalizer1105::procedure) (instantiate::ccomp (id id1102) (producer producer1103) (consumer consumer1104) (finalizer finalizer1105)))
(define-inline (ccomp?::bool obj::obj) ((@ isa? __object) obj (@ ccomp module_module)))
(define (ccomp-nil::ccomp) (class-nil (@ ccomp module_module)))
(define-inline (ccomp-finalizer::procedure o::ccomp) (with-access::ccomp o (finalizer) finalizer))
(define-inline (ccomp-finalizer-set! o::ccomp v::procedure) (with-access::ccomp o (finalizer) (set! finalizer v)))
(define-inline (ccomp-consumer::procedure o::ccomp) (with-access::ccomp o (consumer) consumer))
(define-inline (ccomp-consumer-set! o::ccomp v::procedure) (with-access::ccomp o (consumer) (set! consumer v)))
(define-inline (ccomp-producer::procedure o::ccomp) (with-access::ccomp o (producer) producer))
(define-inline (ccomp-producer-set! o::ccomp v::procedure) (with-access::ccomp o (producer) (set! producer v)))
(define-inline (ccomp-id::symbol o::ccomp) (with-access::ccomp o (id) id))
(define-inline (ccomp-id-set! o::ccomp v::symbol) (with-access::ccomp o (id) (set! id v)))
))
