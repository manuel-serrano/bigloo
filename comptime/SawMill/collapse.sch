;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 18:35:27 CET 2011 
;; (bigloo.new -classgen SawMill/collapse.scm)
;; ==========================================================

;; The directives
(directives

;; collapsed
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-collapsed::collapsed label1202::int preds1203::pair-nil succs1204::pair-nil first1205::pair last1206::obj)
    (inline collapsed?::bool ::obj)
    (collapsed-nil::collapsed)
    (inline collapsed-last::obj ::collapsed)
    (inline collapsed-last-set! ::collapsed ::obj)
    (inline collapsed-first::pair ::collapsed)
    (inline collapsed-first-set! ::collapsed ::pair)
    (inline collapsed-succs::pair-nil ::collapsed)
    (inline collapsed-succs-set! ::collapsed ::pair-nil)
    (inline collapsed-preds::pair-nil ::collapsed)
    (inline collapsed-preds-set! ::collapsed ::pair-nil)
    (inline collapsed-label::int ::collapsed)
    (inline collapsed-label-set! ::collapsed ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; collapsed
(define-inline (make-collapsed::collapsed label1202::int preds1203::pair-nil succs1204::pair-nil first1205::pair last1206::obj) (instantiate::collapsed (label label1202) (preds preds1203) (succs succs1204) (first first1205) (last last1206)))
(define-inline (collapsed?::bool obj::obj) ((@ isa? __object) obj (@ collapsed saw_collapse)))
(define (collapsed-nil::collapsed) (class-nil (@ collapsed saw_collapse)))
(define-inline (collapsed-last::obj o::collapsed) (with-access::collapsed o (last) last))
(define-inline (collapsed-last-set! o::collapsed v::obj) (with-access::collapsed o (last) (set! last v)))
(define-inline (collapsed-first::pair o::collapsed) (with-access::collapsed o (first) first))
(define-inline (collapsed-first-set! o::collapsed v::pair) (with-access::collapsed o (first) (set! first v)))
(define-inline (collapsed-succs::pair-nil o::collapsed) (with-access::collapsed o (succs) succs))
(define-inline (collapsed-succs-set! o::collapsed v::pair-nil) (with-access::collapsed o (succs) (set! succs v)))
(define-inline (collapsed-preds::pair-nil o::collapsed) (with-access::collapsed o (preds) preds))
(define-inline (collapsed-preds-set! o::collapsed v::pair-nil) (with-access::collapsed o (preds) (set! preds v)))
(define-inline (collapsed-label::int o::collapsed) (with-access::collapsed o (label) label))
(define-inline (collapsed-label-set! o::collapsed v::int) (with-access::collapsed o (label) (set! label v)))
))
