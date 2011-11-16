;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:08:36 CET 2011 
;; (bigloo.new -classgen SawMsil/maxstack.scm)
;; ==========================================================

;; The directives
(directives

;; stacked
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-stacked::stacked label1202::int preds1203::pair-nil succs1204::pair-nil first1205::pair)
    (inline stacked?::bool ::obj)
    (stacked-nil::stacked)
    (inline stacked-first::pair ::stacked)
    (inline stacked-first-set! ::stacked ::pair)
    (inline stacked-succs::pair-nil ::stacked)
    (inline stacked-succs-set! ::stacked ::pair-nil)
    (inline stacked-preds::pair-nil ::stacked)
    (inline stacked-preds-set! ::stacked ::pair-nil)
    (inline stacked-label::int ::stacked)
    (inline stacked-label-set! ::stacked ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; stacked
(define-inline (make-stacked::stacked label1202::int preds1203::pair-nil succs1204::pair-nil first1205::pair) (instantiate::stacked (label label1202) (preds preds1203) (succs succs1204) (first first1205)))
(define-inline (stacked?::bool obj::obj) ((@ isa? __object) obj (@ stacked msil_maxstack)))
(define (stacked-nil::stacked) (class-nil (@ stacked msil_maxstack)))
(define-inline (stacked-first::pair o::stacked) (with-access::stacked o (first) first))
(define-inline (stacked-first-set! o::stacked v::pair) (with-access::stacked o (first) (set! first v)))
(define-inline (stacked-succs::pair-nil o::stacked) (with-access::stacked o (succs) succs))
(define-inline (stacked-succs-set! o::stacked v::pair-nil) (with-access::stacked o (succs) (set! succs v)))
(define-inline (stacked-preds::pair-nil o::stacked) (with-access::stacked o (preds) preds))
(define-inline (stacked-preds-set! o::stacked v::pair-nil) (with-access::stacked o (preds) (set! preds v)))
(define-inline (stacked-label::int o::stacked) (with-access::stacked o (label) label))
(define-inline (stacked-label-set! o::stacked v::int) (with-access::stacked o (label) (set! label v)))
))
