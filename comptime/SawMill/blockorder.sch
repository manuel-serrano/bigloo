;; ==========================================================
;; Class accessors
;; Bigloo (3.8d)
;; Inria -- Sophia Antipolis     Mon Oct 15 08:01:14 CEST 2012 
;; (bigloo.new -classgen SawMill/blockorder.scm)
;; ==========================================================

;; The directives
(directives

;; SawDone
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-SawDone::SawDone label1181::int preds1182::pair-nil succs1183::pair-nil first1184::pair)
    (inline SawDone?::bool ::obj)
    (SawDone-nil::SawDone)
    (inline SawDone-first::pair ::SawDone)
    (inline SawDone-first-set! ::SawDone ::pair)
    (inline SawDone-succs::pair-nil ::SawDone)
    (inline SawDone-succs-set! ::SawDone ::pair-nil)
    (inline SawDone-preds::pair-nil ::SawDone)
    (inline SawDone-preds-set! ::SawDone ::pair-nil)
    (inline SawDone-label::int ::SawDone)
    (inline SawDone-label-set! ::SawDone ::int))))

;; SawRdone
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-SawRdone::SawRdone label1176::int preds1177::pair-nil succs1178::pair-nil first1179::pair)
    (inline SawRdone?::bool ::obj)
    (SawRdone-nil::SawRdone)
    (inline SawRdone-first::pair ::SawRdone)
    (inline SawRdone-first-set! ::SawRdone ::pair)
    (inline SawRdone-succs::pair-nil ::SawRdone)
    (inline SawRdone-succs-set! ::SawRdone ::pair-nil)
    (inline SawRdone-preds::pair-nil ::SawRdone)
    (inline SawRdone-preds-set! ::SawRdone ::pair-nil)
    (inline SawRdone-label::int ::SawRdone)
    (inline SawRdone-label-set! ::SawRdone ::int))))

;; SawDfs
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-SawDfs::SawDfs label1169::int preds1170::pair-nil succs1171::pair-nil first1172::pair n1173::int)
    (inline SawDfs?::bool ::obj)
    (SawDfs-nil::SawDfs)
    (inline SawDfs-n::int ::SawDfs)
    (inline SawDfs-n-set! ::SawDfs ::int)
    (inline SawDfs-first::pair ::SawDfs)
    (inline SawDfs-first-set! ::SawDfs ::pair)
    (inline SawDfs-succs::pair-nil ::SawDfs)
    (inline SawDfs-succs-set! ::SawDfs ::pair-nil)
    (inline SawDfs-preds::pair-nil ::SawDfs)
    (inline SawDfs-preds-set! ::SawDfs ::pair-nil)
    (inline SawDfs-label::int ::SawDfs)
    (inline SawDfs-label-set! ::SawDfs ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; SawDone
(define-inline (make-SawDone::SawDone label1181::int preds1182::pair-nil succs1183::pair-nil first1184::pair) (instantiate::SawDone (label label1181) (preds preds1182) (succs succs1183) (first first1184)))
(define-inline (SawDone?::bool obj::obj) ((@ isa? __object) obj (@ SawDone saw_blockorder)))
(define (SawDone-nil::SawDone) (class-nil (@ SawDone saw_blockorder)))
(define-inline (SawDone-first::pair o::SawDone) (with-access::SawDone o (first) first))
(define-inline (SawDone-first-set! o::SawDone v::pair) (with-access::SawDone o (first) (set! first v)))
(define-inline (SawDone-succs::pair-nil o::SawDone) (with-access::SawDone o (succs) succs))
(define-inline (SawDone-succs-set! o::SawDone v::pair-nil) (with-access::SawDone o (succs) (set! succs v)))
(define-inline (SawDone-preds::pair-nil o::SawDone) (with-access::SawDone o (preds) preds))
(define-inline (SawDone-preds-set! o::SawDone v::pair-nil) (with-access::SawDone o (preds) (set! preds v)))
(define-inline (SawDone-label::int o::SawDone) (with-access::SawDone o (label) label))
(define-inline (SawDone-label-set! o::SawDone v::int) (with-access::SawDone o (label) (set! label v)))

;; SawRdone
(define-inline (make-SawRdone::SawRdone label1176::int preds1177::pair-nil succs1178::pair-nil first1179::pair) (instantiate::SawRdone (label label1176) (preds preds1177) (succs succs1178) (first first1179)))
(define-inline (SawRdone?::bool obj::obj) ((@ isa? __object) obj (@ SawRdone saw_blockorder)))
(define (SawRdone-nil::SawRdone) (class-nil (@ SawRdone saw_blockorder)))
(define-inline (SawRdone-first::pair o::SawRdone) (with-access::SawRdone o (first) first))
(define-inline (SawRdone-first-set! o::SawRdone v::pair) (with-access::SawRdone o (first) (set! first v)))
(define-inline (SawRdone-succs::pair-nil o::SawRdone) (with-access::SawRdone o (succs) succs))
(define-inline (SawRdone-succs-set! o::SawRdone v::pair-nil) (with-access::SawRdone o (succs) (set! succs v)))
(define-inline (SawRdone-preds::pair-nil o::SawRdone) (with-access::SawRdone o (preds) preds))
(define-inline (SawRdone-preds-set! o::SawRdone v::pair-nil) (with-access::SawRdone o (preds) (set! preds v)))
(define-inline (SawRdone-label::int o::SawRdone) (with-access::SawRdone o (label) label))
(define-inline (SawRdone-label-set! o::SawRdone v::int) (with-access::SawRdone o (label) (set! label v)))

;; SawDfs
(define-inline (make-SawDfs::SawDfs label1169::int preds1170::pair-nil succs1171::pair-nil first1172::pair n1173::int) (instantiate::SawDfs (label label1169) (preds preds1170) (succs succs1171) (first first1172) (n n1173)))
(define-inline (SawDfs?::bool obj::obj) ((@ isa? __object) obj (@ SawDfs saw_blockorder)))
(define (SawDfs-nil::SawDfs) (class-nil (@ SawDfs saw_blockorder)))
(define-inline (SawDfs-n::int o::SawDfs) (with-access::SawDfs o (n) n))
(define-inline (SawDfs-n-set! o::SawDfs v::int) (with-access::SawDfs o (n) (set! n v)))
(define-inline (SawDfs-first::pair o::SawDfs) (with-access::SawDfs o (first) first))
(define-inline (SawDfs-first-set! o::SawDfs v::pair) (with-access::SawDfs o (first) (set! first v)))
(define-inline (SawDfs-succs::pair-nil o::SawDfs) (with-access::SawDfs o (succs) succs))
(define-inline (SawDfs-succs-set! o::SawDfs v::pair-nil) (with-access::SawDfs o (succs) (set! succs v)))
(define-inline (SawDfs-preds::pair-nil o::SawDfs) (with-access::SawDfs o (preds) preds))
(define-inline (SawDfs-preds-set! o::SawDfs v::pair-nil) (with-access::SawDfs o (preds) (set! preds v)))
(define-inline (SawDfs-label::int o::SawDfs) (with-access::SawDfs o (label) label))
(define-inline (SawDfs-label-set! o::SawDfs v::int) (with-access::SawDfs o (label) (set! label v)))
))
