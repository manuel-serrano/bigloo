;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Sat Nov 7 19:45:19 CET 2015 
;; (bigloo Llib/icalendar.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; line
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-line::line name1069::symbol params1070::pair-nil val1071::bstring fname1072::obj location1073::obj)
    (inline line?::bool ::obj)
    (line-nil::line)
    (inline line-location::obj ::line)
    (inline line-location-set! ::line ::obj)
    (inline line-fname::obj ::line)
    (inline line-fname-set! ::line ::obj)
    (inline line-val::bstring ::line)
    (inline line-params::pair-nil ::line)
    (inline line-name::symbol ::line))))

;; block
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-block::block l01065::line ln1066::line body1067::pair-nil)
    (inline block?::bool ::obj)
    (block-nil::block)
    (inline block-body::pair-nil ::block)
    (inline block-ln::line ::block)
    (inline block-l0::line ::block)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; line
(define-inline (make-line::line name1069::symbol params1070::pair-nil val1071::bstring fname1072::obj location1073::obj) (instantiate::line (name name1069) (params params1070) (val val1071) (fname fname1072) (location location1073)))
(define-inline (line?::bool obj::obj) ((@ isa? __object) obj (@ line __calendar_ical)))
(define (line-nil::line) (class-nil (@ line __calendar_ical)))
(define-inline (line-location::obj o::line) (-> |#!bigloo_wallow| o location))
(define-inline (line-location-set! o::line v::obj) (set! (-> |#!bigloo_wallow| o location) v))
(define-inline (line-fname::obj o::line) (-> |#!bigloo_wallow| o fname))
(define-inline (line-fname-set! o::line v::obj) (set! (-> |#!bigloo_wallow| o fname) v))
(define-inline (line-val::bstring o::line) (-> |#!bigloo_wallow| o val))
(define-inline (line-val-set! o::line v::bstring) (set! (-> |#!bigloo_wallow| o val) v))
(define-inline (line-params::pair-nil o::line) (-> |#!bigloo_wallow| o params))
(define-inline (line-params-set! o::line v::pair-nil) (set! (-> |#!bigloo_wallow| o params) v))
(define-inline (line-name::symbol o::line) (-> |#!bigloo_wallow| o name))
(define-inline (line-name-set! o::line v::symbol) (set! (-> |#!bigloo_wallow| o name) v))

;; block
(define-inline (make-block::block l01065::line ln1066::line body1067::pair-nil) (instantiate::block (l0 l01065) (ln ln1066) (body body1067)))
(define-inline (block?::bool obj::obj) ((@ isa? __object) obj (@ block __calendar_ical)))
(define (block-nil::block) (class-nil (@ block __calendar_ical)))
(define-inline (block-body::pair-nil o::block) (-> |#!bigloo_wallow| o body))
(define-inline (block-body-set! o::block v::pair-nil) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (block-ln::line o::block) (-> |#!bigloo_wallow| o ln))
(define-inline (block-ln-set! o::block v::line) (set! (-> |#!bigloo_wallow| o ln) v))
(define-inline (block-l0::line o::block) (-> |#!bigloo_wallow| o l0))
(define-inline (block-l0-set! o::block v::line) (set! (-> |#!bigloo_wallow| o l0) v))
))
