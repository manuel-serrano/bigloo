;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:08:36 CET 2011 
;; (bigloo.new -classgen SawMill/regset.scm)
;; ==========================================================

;; The directives
(directives

;; regset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-regset::regset length1201::int msize1202::int regv1203::vector regl1204::pair-nil string1205::bstring)
    (inline regset?::bool ::obj)
    (regset-nil::regset)
    (inline regset-string::bstring ::regset)
    (inline regset-string-set! ::regset ::bstring)
    (inline regset-regl::pair-nil ::regset)
    (inline regset-regv::vector ::regset)
    (inline regset-msize::int ::regset)
    (inline regset-length::int ::regset)
    (inline regset-length-set! ::regset ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; regset
(define-inline (make-regset::regset length1201::int msize1202::int regv1203::vector regl1204::pair-nil string1205::bstring) (instantiate::regset (length length1201) (msize msize1202) (regv regv1203) (regl regl1204) (string string1205)))
(define-inline (regset?::bool obj::obj) ((@ isa? __object) obj (@ regset saw_regset)))
(define (regset-nil::regset) (class-nil (@ regset saw_regset)))
(define-inline (regset-string::bstring o::regset) (with-access::regset o (string) string))
(define-inline (regset-string-set! o::regset v::bstring) (with-access::regset o (string) (set! string v)))
(define-inline (regset-regl::pair-nil o::regset) (with-access::regset o (regl) regl))
(define-inline (regset-regl-set! o::regset v::pair-nil) (with-access::regset o (regl) (set! regl v)))
(define-inline (regset-regv::vector o::regset) (with-access::regset o (regv) regv))
(define-inline (regset-regv-set! o::regset v::vector) (with-access::regset o (regv) (set! regv v)))
(define-inline (regset-msize::int o::regset) (with-access::regset o (msize) msize))
(define-inline (regset-msize-set! o::regset v::int) (with-access::regset o (msize) (set! msize v)))
(define-inline (regset-length::int o::regset) (with-access::regset o (length) length))
(define-inline (regset-length-set! o::regset v::int) (with-access::regset o (length) (set! length v)))
))
