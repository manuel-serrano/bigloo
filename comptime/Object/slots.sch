;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Fri Nov 25 08:08:52 CET 2011 
;; (bigloo -classgen Object/slots.scm)
;; ==========================================================

;; The directives
(directives

;; slot
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-slot::slot id1078::symbol name1079::bstring src1080::obj class-owner1081::obj index1082::long type1083::obj read-only?1084::bool default-value1085::obj virtual-num1086::obj getter1087::obj setter1088::obj user-info1089::obj)
    (inline slot?::bool ::obj)
    (slot-nil::slot)
    (inline slot-user-info::obj ::slot)
    (inline slot-setter::obj ::slot)
    (inline slot-setter-set! ::slot ::obj)
    (inline slot-getter::obj ::slot)
    (inline slot-getter-set! ::slot ::obj)
    (inline slot-virtual-num::obj ::slot)
    (inline slot-virtual-num-set! ::slot ::obj)
    (inline slot-default-value::obj ::slot)
    (inline slot-read-only?::bool ::slot)
    (inline slot-type::obj ::slot)
    (inline slot-index::long ::slot)
    (inline slot-class-owner::obj ::slot)
    (inline slot-src::obj ::slot)
    (inline slot-name::bstring ::slot)
    (inline slot-id::symbol ::slot)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; slot
(define-inline (make-slot::slot id1078::symbol name1079::bstring src1080::obj class-owner1081::obj index1082::long type1083::obj read-only?1084::bool default-value1085::obj virtual-num1086::obj getter1087::obj setter1088::obj user-info1089::obj) (instantiate::slot (id id1078) (name name1079) (src src1080) (class-owner class-owner1081) (index index1082) (type type1083) (read-only? read-only?1084) (default-value default-value1085) (virtual-num virtual-num1086) (getter getter1087) (setter setter1088) (user-info user-info1089)))
(define-inline (slot?::bool obj::obj) ((@ isa? __object) obj (@ slot object_slots)))
(define (slot-nil::slot) (class-nil (@ slot object_slots)))
(define-inline (slot-user-info::obj o::slot) (with-access::slot o (user-info) user-info))
(define-inline (slot-user-info-set! o::slot v::obj) (with-access::slot o (user-info) (set! user-info v)))
(define-inline (slot-setter::obj o::slot) (with-access::slot o (setter) setter))
(define-inline (slot-setter-set! o::slot v::obj) (with-access::slot o (setter) (set! setter v)))
(define-inline (slot-getter::obj o::slot) (with-access::slot o (getter) getter))
(define-inline (slot-getter-set! o::slot v::obj) (with-access::slot o (getter) (set! getter v)))
(define-inline (slot-virtual-num::obj o::slot) (with-access::slot o (virtual-num) virtual-num))
(define-inline (slot-virtual-num-set! o::slot v::obj) (with-access::slot o (virtual-num) (set! virtual-num v)))
(define-inline (slot-default-value::obj o::slot) (with-access::slot o (default-value) default-value))
(define-inline (slot-default-value-set! o::slot v::obj) (with-access::slot o (default-value) (set! default-value v)))
(define-inline (slot-read-only?::bool o::slot) (with-access::slot o (read-only?) read-only?))
(define-inline (slot-read-only?-set! o::slot v::bool) (with-access::slot o (read-only?) (set! read-only? v)))
(define-inline (slot-type::obj o::slot) (with-access::slot o (type) type))
(define-inline (slot-type-set! o::slot v::obj) (with-access::slot o (type) (set! type v)))
(define-inline (slot-index::long o::slot) (with-access::slot o (index) index))
(define-inline (slot-index-set! o::slot v::long) (with-access::slot o (index) (set! index v)))
(define-inline (slot-class-owner::obj o::slot) (with-access::slot o (class-owner) class-owner))
(define-inline (slot-class-owner-set! o::slot v::obj) (with-access::slot o (class-owner) (set! class-owner v)))
(define-inline (slot-src::obj o::slot) (with-access::slot o (src) src))
(define-inline (slot-src-set! o::slot v::obj) (with-access::slot o (src) (set! src v)))
(define-inline (slot-name::bstring o::slot) (with-access::slot o (name) name))
(define-inline (slot-name-set! o::slot v::bstring) (with-access::slot o (name) (set! name v)))
(define-inline (slot-id::symbol o::slot) (with-access::slot o (id) id))
(define-inline (slot-id-set! o::slot v::symbol) (with-access::slot o (id) (set! id v)))
))
