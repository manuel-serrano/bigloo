;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Tue Nov 15 09:56:41 CET 2011 
;; (/tmp/bgl2 Llib/icalendar.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; line
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-line::line name1033::symbol params1034::pair-nil val1035::bstring fname1036::obj location1037::obj)
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
    (inline make-block::block l01029::line ln1030::line body1031::pair-nil)
    (inline block?::bool ::obj)
    (block-nil::block)
    (inline block-body::pair-nil ::block)
    (inline block-ln::line ::block)
    (inline block-l0::line ::block)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; line
(define-inline (make-line::line name1033::symbol params1034::pair-nil val1035::bstring fname1036::obj location1037::obj) (instantiate::line (name name1033) (params params1034) (val val1035) (fname fname1036) (location location1037)))
(define-inline (line?::bool obj::obj) ((@ isa? __object) obj (@ line __calendar_ical)))
(define (line-nil::line) (class-nil (@ line __calendar_ical)))
(define-inline (line-location::obj o::line) (with-access::line o (location) location))
(define-inline (line-location-set! o::line v::obj) (with-access::line o (location) (set! location v)))
(define-inline (line-fname::obj o::line) (with-access::line o (fname) fname))
(define-inline (line-fname-set! o::line v::obj) (with-access::line o (fname) (set! fname v)))
(define-inline (line-val::bstring o::line) (with-access::line o (val) val))
(define-inline (line-val-set! o::line v::bstring) (with-access::line o (val) (set! val v)))
(define-inline (line-params::pair-nil o::line) (with-access::line o (params) params))
(define-inline (line-params-set! o::line v::pair-nil) (with-access::line o (params) (set! params v)))
(define-inline (line-name::symbol o::line) (with-access::line o (name) name))
(define-inline (line-name-set! o::line v::symbol) (with-access::line o (name) (set! name v)))

;; block
(define-inline (make-block::block l01029::line ln1030::line body1031::pair-nil) (instantiate::block (l0 l01029) (ln ln1030) (body body1031)))
(define-inline (block?::bool obj::obj) ((@ isa? __object) obj (@ block __calendar_ical)))
(define (block-nil::block) (class-nil (@ block __calendar_ical)))
(define-inline (block-body::pair-nil o::block) (with-access::block o (body) body))
(define-inline (block-body-set! o::block v::pair-nil) (with-access::block o (body) (set! body v)))
(define-inline (block-ln::line o::block) (with-access::block o (ln) ln))
(define-inline (block-ln-set! o::block v::line) (with-access::block o (ln) (set! ln v)))
(define-inline (block-l0::line o::block) (with-access::block o (l0) l0))
(define-inline (block-l0-set! o::block v::line) (with-access::block o (l0) (set! l0 v)))
))
