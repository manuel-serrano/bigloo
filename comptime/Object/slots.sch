;; ==========================================================
;; Class accessors
;; Bigloo (3.8a)
;; Inria -- Sophia Antipolis     Tue Mar 20 13:57:04 CET 2012 
;; (bigloo Object/slots.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; slot
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-slot::slot id1079::symbol name1080::bstring src1081::obj class-owner1082::obj index1083::long type1084::obj read-only?1085::bool default-value1086::obj virtual-num1087::obj virtual-override1088::bool getter1089::obj setter1090::obj user-info1091::obj)
    (inline slot?::bool ::obj)
    (slot-nil::slot)
    (inline slot-user-info::obj ::slot)
    (inline slot-setter::obj ::slot)
    (inline slot-setter-set! ::slot ::obj)
    (inline slot-getter::obj ::slot)
    (inline slot-getter-set! ::slot ::obj)
    (inline slot-virtual-override::bool ::slot)
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
(define-inline (make-slot::slot id1079::symbol name1080::bstring src1081::obj class-owner1082::obj index1083::long type1084::obj read-only?1085::bool default-value1086::obj virtual-num1087::obj virtual-override1088::bool getter1089::obj setter1090::obj user-info1091::obj) (instantiate::slot (id id1079) (name name1080) (src src1081) (class-owner class-owner1082) (index index1083) (type type1084) (read-only? read-only?1085) (default-value default-value1086) (virtual-num virtual-num1087) (virtual-override virtual-override1088) (getter getter1089) (setter setter1090) (user-info user-info1091)))
(define-inline (slot?::bool obj::obj) ((@ isa? __object) obj (@ slot object_slots)))
(define (slot-nil::slot) (class-nil (@ slot object_slots)))
(define-inline (slot-user-info::obj o::slot) (with-access::slot o (user-info) user-info))
(define-inline (slot-user-info-set! o::slot v::obj) (with-access::slot o (user-info) (set! user-info v)))
(define-inline (slot-setter::obj o::slot) (with-access::slot o (setter) setter))
(define-inline (slot-setter-set! o::slot v::obj) (with-access::slot o (setter) (set! setter v)))
(define-inline (slot-getter::obj o::slot) (with-access::slot o (getter) getter))
(define-inline (slot-getter-set! o::slot v::obj) (with-access::slot o (getter) (set! getter v)))
(define-inline (slot-virtual-override::bool o::slot) (with-access::slot o (virtual-override) virtual-override))
(define-inline (slot-virtual-override-set! o::slot v::bool) (with-access::slot o (virtual-override) (set! virtual-override v)))
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
