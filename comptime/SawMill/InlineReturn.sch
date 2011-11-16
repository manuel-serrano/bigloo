;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 18:35:27 CET 2011 
;; (bigloo.new -classgen SawMill/InlineReturn.scm)
;; ==========================================================

;; The directives
(directives

;; dfs
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-dfs::dfs label1202::int preds1203::pair-nil succs1204::pair-nil first1205::pair)
    (inline dfs?::bool ::obj)
    (dfs-nil::dfs)
    (inline dfs-first::pair ::dfs)
    (inline dfs-first-set! ::dfs ::pair)
    (inline dfs-succs::pair-nil ::dfs)
    (inline dfs-succs-set! ::dfs ::pair-nil)
    (inline dfs-preds::pair-nil ::dfs)
    (inline dfs-preds-set! ::dfs ::pair-nil)
    (inline dfs-label::int ::dfs)
    (inline dfs-label-set! ::dfs ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; dfs
(define-inline (make-dfs::dfs label1202::int preds1203::pair-nil succs1204::pair-nil first1205::pair) (instantiate::dfs (label label1202) (preds preds1203) (succs succs1204) (first first1205)))
(define-inline (dfs?::bool obj::obj) ((@ isa? __object) obj (@ dfs saw_inline_return)))
(define (dfs-nil::dfs) (class-nil (@ dfs saw_inline_return)))
(define-inline (dfs-first::pair o::dfs) (with-access::dfs o (first) first))
(define-inline (dfs-first-set! o::dfs v::pair) (with-access::dfs o (first) (set! first v)))
(define-inline (dfs-succs::pair-nil o::dfs) (with-access::dfs o (succs) succs))
(define-inline (dfs-succs-set! o::dfs v::pair-nil) (with-access::dfs o (succs) (set! succs v)))
(define-inline (dfs-preds::pair-nil o::dfs) (with-access::dfs o (preds) preds))
(define-inline (dfs-preds-set! o::dfs v::pair-nil) (with-access::dfs o (preds) (set! preds v)))
(define-inline (dfs-label::int o::dfs) (with-access::dfs o (label) label))
(define-inline (dfs-label-set! o::dfs v::int) (with-access::dfs o (label) (set! label v)))
))
