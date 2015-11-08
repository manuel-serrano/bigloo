;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen SawMill/regset.scm)
;; ==========================================================

;; The directives
(directives

;; regset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-regset::regset length1150::int msize1151::int regv1152::vector regl1153::pair-nil string1154::bstring)
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
(define-inline (make-regset::regset length1150::int msize1151::int regv1152::vector regl1153::pair-nil string1154::bstring) (instantiate::regset (length length1150) (msize msize1151) (regv regv1152) (regl regl1153) (string string1154)))
(define-inline (regset?::bool obj::obj) ((@ isa? __object) obj (@ regset saw_regset)))
(define (regset-nil::regset) (class-nil (@ regset saw_regset)))
(define-inline (regset-string::bstring o::regset) (-> |#!bigloo_wallow| o string))
(define-inline (regset-string-set! o::regset v::bstring) (set! (-> |#!bigloo_wallow| o string) v))
(define-inline (regset-regl::pair-nil o::regset) (-> |#!bigloo_wallow| o regl))
(define-inline (regset-regl-set! o::regset v::pair-nil) (set! (-> |#!bigloo_wallow| o regl) v))
(define-inline (regset-regv::vector o::regset) (-> |#!bigloo_wallow| o regv))
(define-inline (regset-regv-set! o::regset v::vector) (set! (-> |#!bigloo_wallow| o regv) v))
(define-inline (regset-msize::int o::regset) (-> |#!bigloo_wallow| o msize))
(define-inline (regset-msize-set! o::regset v::int) (set! (-> |#!bigloo_wallow| o msize) v))
(define-inline (regset-length::int o::regset) (-> |#!bigloo_wallow| o length))
(define-inline (regset-length-set! o::regset v::int) (set! (-> |#!bigloo_wallow| o length) v))
))
