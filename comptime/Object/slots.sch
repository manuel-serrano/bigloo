;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new Object/slots.scm -classgen)
;; ==========================================================

;; The directives
(directives

;; slot
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-slot::slot id1080::symbol name1081::bstring src1082::obj class-owner1083::obj index1084::long type1085::obj read-only?1086::bool default-value1087::obj virtual-num1088::obj virtual-override1089::bool getter1090::obj setter1091::obj user-info1092::obj)
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
(define-inline (make-slot::slot id1080::symbol name1081::bstring src1082::obj class-owner1083::obj index1084::long type1085::obj read-only?1086::bool default-value1087::obj virtual-num1088::obj virtual-override1089::bool getter1090::obj setter1091::obj user-info1092::obj) (instantiate::slot (id id1080) (name name1081) (src src1082) (class-owner class-owner1083) (index index1084) (type type1085) (read-only? read-only?1086) (default-value default-value1087) (virtual-num virtual-num1088) (virtual-override virtual-override1089) (getter getter1090) (setter setter1091) (user-info user-info1092)))
(define-inline (slot?::bool obj::obj) ((@ isa? __object) obj (@ slot object_slots)))
(define (slot-nil::slot) (class-nil (@ slot object_slots)))
(define-inline (slot-user-info::obj o::slot) (-> |#!bigloo_wallow| o user-info))
(define-inline (slot-user-info-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o user-info) v))
(define-inline (slot-setter::obj o::slot) (-> |#!bigloo_wallow| o setter))
(define-inline (slot-setter-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o setter) v))
(define-inline (slot-getter::obj o::slot) (-> |#!bigloo_wallow| o getter))
(define-inline (slot-getter-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o getter) v))
(define-inline (slot-virtual-override::bool o::slot) (-> |#!bigloo_wallow| o virtual-override))
(define-inline (slot-virtual-override-set! o::slot v::bool) (set! (-> |#!bigloo_wallow| o virtual-override) v))
(define-inline (slot-virtual-num::obj o::slot) (-> |#!bigloo_wallow| o virtual-num))
(define-inline (slot-virtual-num-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o virtual-num) v))
(define-inline (slot-default-value::obj o::slot) (-> |#!bigloo_wallow| o default-value))
(define-inline (slot-default-value-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o default-value) v))
(define-inline (slot-read-only?::bool o::slot) (-> |#!bigloo_wallow| o read-only?))
(define-inline (slot-read-only?-set! o::slot v::bool) (set! (-> |#!bigloo_wallow| o read-only?) v))
(define-inline (slot-type::obj o::slot) (-> |#!bigloo_wallow| o type))
(define-inline (slot-type-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (slot-index::long o::slot) (-> |#!bigloo_wallow| o index))
(define-inline (slot-index-set! o::slot v::long) (set! (-> |#!bigloo_wallow| o index) v))
(define-inline (slot-class-owner::obj o::slot) (-> |#!bigloo_wallow| o class-owner))
(define-inline (slot-class-owner-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o class-owner) v))
(define-inline (slot-src::obj o::slot) (-> |#!bigloo_wallow| o src))
(define-inline (slot-src-set! o::slot v::obj) (set! (-> |#!bigloo_wallow| o src) v))
(define-inline (slot-name::bstring o::slot) (-> |#!bigloo_wallow| o name))
(define-inline (slot-name-set! o::slot v::bstring) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (slot-id::symbol o::slot) (-> |#!bigloo_wallow| o id))
(define-inline (slot-id-set! o::slot v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
