;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:19:56 CET 2011 
;; (bigloo.new -classgen object5.scm)
;; ==========================================================

;; The directives
(directives

;; object5
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-object5::object5 x1161::obj y1162::obj z1163::obj)
    (inline object5?::bool ::obj)
    (object5-nil::object5)
    (inline object5-z::obj ::object5)
    (inline object5-z-set! ::object5 ::obj)
    (inline object5-y::obj ::object5)
    (inline object5-y-set! ::object5 ::obj)
    (inline object5-x::obj ::object5)
    (inline object5-x-set! ::object5 ::obj))))

;; object6
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-object6::object6 rec1159::obj)
    (inline object6?::bool ::obj)
    (object6-nil::object6)
    (inline object6-rec::obj ::object6)
    (inline object6-rec-set! ::object6 ::obj))))

;; object7
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-object7::object7 host1157::obj)
    (inline object7?::bool ::obj)
    (object7-nil::object7)
    (inline object7-host::obj ::object7)
    (inline object7-host-set! ::object7 ::obj))))

;; object8
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-object8::object8 value1155::byte)
    (inline object8?::bool ::obj)
    (object8-nil::object8)
    (inline object8-value::byte ::object8)
    (inline object8-value-set! ::object8 ::byte))))

;; recursive-dog
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-recursive-dog::recursive-dog name1152::obj next1153::recursive-dog)
    (inline recursive-dog?::bool ::obj)
    (recursive-dog-nil::recursive-dog)
    (inline recursive-dog-next::recursive-dog ::recursive-dog)
    (inline recursive-dog-next-set! ::recursive-dog ::recursive-dog)
    (inline recursive-dog-name::obj ::recursive-dog)
    (inline recursive-dog-name-set! ::recursive-dog ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; object5
(define-inline (make-object5::object5 x1161::obj y1162::obj z1163::obj) (instantiate::object5 (x x1161) (y y1162) (z z1163)))
(define-inline (object5?::bool obj::obj) ((@ isa? __object) obj (@ object5 object5)))
(define (object5-nil::object5) (class-nil (@ object5 object5)))
(define-inline (object5-z::obj o::object5) (with-access::object5 o (z) z))
(define-inline (object5-z-set! o::object5 v::obj) (with-access::object5 o (z) (set! z v)))
(define-inline (object5-y::obj o::object5) (with-access::object5 o (y) y))
(define-inline (object5-y-set! o::object5 v::obj) (with-access::object5 o (y) (set! y v)))
(define-inline (object5-x::obj o::object5) (with-access::object5 o (x) x))
(define-inline (object5-x-set! o::object5 v::obj) (with-access::object5 o (x) (set! x v)))

;; object6
(define-inline (make-object6::object6 rec1159::obj) (instantiate::object6 (rec rec1159)))
(define-inline (object6?::bool obj::obj) ((@ isa? __object) obj (@ object6 object5)))
(define (object6-nil::object6) (class-nil (@ object6 object5)))
(define-inline (object6-rec::obj o::object6) (with-access::object6 o (rec) rec))
(define-inline (object6-rec-set! o::object6 v::obj) (with-access::object6 o (rec) (set! rec v)))

;; object7
(define-inline (make-object7::object7 host1157::obj) (instantiate::object7 (host host1157)))
(define-inline (object7?::bool obj::obj) ((@ isa? __object) obj (@ object7 object5)))
(define (object7-nil::object7) (class-nil (@ object7 object5)))
(define-inline (object7-host::obj o::object7) (with-access::object7 o (host) host))
(define-inline (object7-host-set! o::object7 v::obj) (with-access::object7 o (host) (set! host v)))

;; object8
(define-inline (make-object8::object8 value1155::byte) (instantiate::object8 (value value1155)))
(define-inline (object8?::bool obj::obj) ((@ isa? __object) obj (@ object8 object5)))
(define (object8-nil::object8) (class-nil (@ object8 object5)))
(define-inline (object8-value::byte o::object8) (with-access::object8 o (value) value))
(define-inline (object8-value-set! o::object8 v::byte) (with-access::object8 o (value) (set! value v)))

;; recursive-dog
(define-inline (make-recursive-dog::recursive-dog name1152::obj next1153::recursive-dog) (instantiate::recursive-dog (name name1152) (next next1153)))
(define-inline (recursive-dog?::bool obj::obj) ((@ isa? __object) obj (@ recursive-dog object5)))
(define (recursive-dog-nil::recursive-dog) (class-nil (@ recursive-dog object5)))
(define-inline (recursive-dog-next::recursive-dog o::recursive-dog) (with-access::recursive-dog o (next) next))
(define-inline (recursive-dog-next-set! o::recursive-dog v::recursive-dog) (with-access::recursive-dog o (next) (set! next v)))
(define-inline (recursive-dog-name::obj o::recursive-dog) (with-access::recursive-dog o (name) name))
(define-inline (recursive-dog-name-set! o::recursive-dog v::obj) (with-access::recursive-dog o (name) (set! name v)))
))
