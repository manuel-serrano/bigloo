;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 18:35:27 CET 2011 
;; (bigloo.new -classgen SawMill/blockorder.scm)
;; ==========================================================

;; The directives
(directives

;; done
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-done::done label1234::int preds1235::pair-nil succs1236::pair-nil first1237::pair)
    (inline done?::bool ::obj)
    (done-nil::done)
    (inline done-first::pair ::done)
    (inline done-first-set! ::done ::pair)
    (inline done-succs::pair-nil ::done)
    (inline done-succs-set! ::done ::pair-nil)
    (inline done-preds::pair-nil ::done)
    (inline done-preds-set! ::done ::pair-nil)
    (inline done-label::int ::done)
    (inline done-label-set! ::done ::int))))

;; rdone
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rdone::rdone label1229::int preds1230::pair-nil succs1231::pair-nil first1232::pair)
    (inline rdone?::bool ::obj)
    (rdone-nil::rdone)
    (inline rdone-first::pair ::rdone)
    (inline rdone-first-set! ::rdone ::pair)
    (inline rdone-succs::pair-nil ::rdone)
    (inline rdone-succs-set! ::rdone ::pair-nil)
    (inline rdone-preds::pair-nil ::rdone)
    (inline rdone-preds-set! ::rdone ::pair-nil)
    (inline rdone-label::int ::rdone)
    (inline rdone-label-set! ::rdone ::int))))

;; dfs
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-dfs::dfs label1223::int preds1224::pair-nil succs1225::pair-nil first1226::pair n1227::int)
    (inline dfs?::bool ::obj)
    (dfs-nil::dfs)
    (inline dfs-n::int ::dfs)
    (inline dfs-n-set! ::dfs ::int)
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
;; done
(define-inline (make-done::done label1234::int preds1235::pair-nil succs1236::pair-nil first1237::pair) (instantiate::done (label label1234) (preds preds1235) (succs succs1236) (first first1237)))
(define-inline (done?::bool obj::obj) ((@ isa? __object) obj (@ done saw_blockorder)))
(define (done-nil::done) (class-nil (@ done saw_blockorder)))
(define-inline (done-first::pair o::done) (with-access::done o (first) first))
(define-inline (done-first-set! o::done v::pair) (with-access::done o (first) (set! first v)))
(define-inline (done-succs::pair-nil o::done) (with-access::done o (succs) succs))
(define-inline (done-succs-set! o::done v::pair-nil) (with-access::done o (succs) (set! succs v)))
(define-inline (done-preds::pair-nil o::done) (with-access::done o (preds) preds))
(define-inline (done-preds-set! o::done v::pair-nil) (with-access::done o (preds) (set! preds v)))
(define-inline (done-label::int o::done) (with-access::done o (label) label))
(define-inline (done-label-set! o::done v::int) (with-access::done o (label) (set! label v)))

;; rdone
(define-inline (make-rdone::rdone label1229::int preds1230::pair-nil succs1231::pair-nil first1232::pair) (instantiate::rdone (label label1229) (preds preds1230) (succs succs1231) (first first1232)))
(define-inline (rdone?::bool obj::obj) ((@ isa? __object) obj (@ rdone saw_blockorder)))
(define (rdone-nil::rdone) (class-nil (@ rdone saw_blockorder)))
(define-inline (rdone-first::pair o::rdone) (with-access::rdone o (first) first))
(define-inline (rdone-first-set! o::rdone v::pair) (with-access::rdone o (first) (set! first v)))
(define-inline (rdone-succs::pair-nil o::rdone) (with-access::rdone o (succs) succs))
(define-inline (rdone-succs-set! o::rdone v::pair-nil) (with-access::rdone o (succs) (set! succs v)))
(define-inline (rdone-preds::pair-nil o::rdone) (with-access::rdone o (preds) preds))
(define-inline (rdone-preds-set! o::rdone v::pair-nil) (with-access::rdone o (preds) (set! preds v)))
(define-inline (rdone-label::int o::rdone) (with-access::rdone o (label) label))
(define-inline (rdone-label-set! o::rdone v::int) (with-access::rdone o (label) (set! label v)))

;; dfs
(define-inline (make-dfs::dfs label1223::int preds1224::pair-nil succs1225::pair-nil first1226::pair n1227::int) (instantiate::dfs (label label1223) (preds preds1224) (succs succs1225) (first first1226) (n n1227)))
(define-inline (dfs?::bool obj::obj) ((@ isa? __object) obj (@ dfs saw_blockorder)))
(define (dfs-nil::dfs) (class-nil (@ dfs saw_blockorder)))
(define-inline (dfs-n::int o::dfs) (with-access::dfs o (n) n))
(define-inline (dfs-n-set! o::dfs v::int) (with-access::dfs o (n) (set! n v)))
(define-inline (dfs-first::pair o::dfs) (with-access::dfs o (first) first))
(define-inline (dfs-first-set! o::dfs v::pair) (with-access::dfs o (first) (set! first v)))
(define-inline (dfs-succs::pair-nil o::dfs) (with-access::dfs o (succs) succs))
(define-inline (dfs-succs-set! o::dfs v::pair-nil) (with-access::dfs o (succs) (set! succs v)))
(define-inline (dfs-preds::pair-nil o::dfs) (with-access::dfs o (preds) preds))
(define-inline (dfs-preds-set! o::dfs v::pair-nil) (with-access::dfs o (preds) (set! preds v)))
(define-inline (dfs-label::int o::dfs) (with-access::dfs o (label) label))
(define-inline (dfs-label-set! o::dfs v::int) (with-access::dfs o (label) (set! label v)))
))
