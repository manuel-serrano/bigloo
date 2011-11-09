;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 7 12:41:21 CET 2011 
;; (bigloo.new -classgen object5.scm)
;; ==========================================================

;; The directives
(directives
   (cond-expand ((and bigloo-class-sans (not bigloo-class-generate))

;; object5
(static
  (inline object5? ::obj)
  (inline make-object5::object5 x1640::obj y1641::obj z1642::obj)
  (object5-nil::object5)
  (inline object5-z::obj ::object5)
  (inline object5-z-set! ::object5 ::obj)
  (inline object5-y::obj ::object5)
  (inline object5-y-set! ::object5 ::obj)
  (inline object5-x::obj ::object5)
  (inline object5-x-set! ::object5 ::obj)
)
;; object6
(export
  (inline object6? ::obj)
  (inline make-object6::object6 rec1637::obj)
  (object6-nil::object6)
  (inline object6-rec::obj ::object6)
  (inline object6-rec-set! ::object6 ::obj)
)
;; object7
(export
  (inline object7? ::obj)
  (inline make-object7::object7 host1634::obj)
  (object7-nil::object7)
  (inline object7-host::obj ::object7)
  (inline object7-host-set! ::object7 ::obj)
)
;; object8
(export
  (inline object8? ::obj)
  (inline make-object8::object8 value1631::byte)
  (object8-nil::object8)
  (inline object8-value::byte ::object8)
  (inline object8-value-set! ::object8 ::byte)
)
;; recursive-dog
(static
  (inline recursive-dog? ::obj)
  (inline make-recursive-dog::recursive-dog name1627::obj next1628::recursive-dog)
  (recursive-dog-nil::recursive-dog)
  (inline recursive-dog-next::recursive-dog ::recursive-dog)
  (inline recursive-dog-next-set! ::recursive-dog ::recursive-dog)
  (inline recursive-dog-name::obj ::recursive-dog)
  (inline recursive-dog-name-set! ::recursive-dog ::obj)
)
)))

;; The definitions
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))

;; object5
(define-inline (object5?::bool obj::obj) ((@ is-a? __object) obj (@ object5 object5)))
(define-inline (make-object5::object5 x1640::obj y1641::obj z1642::obj) (instantiate::object5 (x x1640) (y y1641) (z z1642)))
(define (object5-nil::object5) (let ((new1643::object5 (let ((new1644::object5 (free-pragma::object5 ((BgL_object5z00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_object5z00_bgl) )))))) (object-class-num-set! new1644 ((@ class-num __object) (@ object5 object5))) (object-widening-set! new1644 #f) new1644))) (set! __bigloo__.new1643.x #unspecified) (set! __bigloo__.new1643.y #unspecified) (set! __bigloo__.new1643.z #unspecified) new1643))
(define-inline (object5-z::obj o::object5) (with-access::object5 o (z) z))
(define-inline (object5-z-set! o::object5 v::obj) (with-access::object5 o (z) (set! z v)))
(define-inline (object5-y::obj o::object5) (with-access::object5 o (y) y))
(define-inline (object5-y-set! o::object5 v::obj) (with-access::object5 o (y) (set! y v)))
(define-inline (object5-x::obj o::object5) (with-access::object5 o (x) x))
(define-inline (object5-x-set! o::object5 v::obj) (with-access::object5 o (x) (set! x v)))

;; object6
(define-inline (object6?::bool obj::obj) ((@ is-a? __object) obj (@ object6 object5)))
(define-inline (make-object6::object6 rec1637::obj) (instantiate::object6 (rec rec1637)))
(define (object6-nil::object6) (let ((new1638::object6 (let ((new1639::object6 (free-pragma::object6 ((BgL_object6z00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_object6z00_bgl) )))))) (object-class-num-set! new1639 ((@ class-num __object) (@ object6 object5))) (object-widening-set! new1639 #f) new1639))) (set! __bigloo__.new1638.rec #unspecified) new1638))
(define-inline (object6-rec::obj o::object6) (with-access::object6 o (rec) rec))
(define-inline (object6-rec-set! o::object6 v::obj) (with-access::object6 o (rec) (set! rec v)))

;; object7
(define-inline (object7?::bool obj::obj) ((@ is-a? __object) obj (@ object7 object5)))
(define-inline (make-object7::object7 host1634::obj) (instantiate::object7 (host host1634)))
(define (object7-nil::object7) (let ((new1635::object7 (let ((new1636::object7 (free-pragma::object7 ((BgL_object7z00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_object7z00_bgl) )))))) (object-class-num-set! new1636 ((@ class-num __object) (@ object7 object5))) (object-widening-set! new1636 #f) new1636))) (set! __bigloo__.new1635.host #unspecified) new1635))
(define-inline (object7-host::obj o::object7) (with-access::object7 o (host) host))
(define-inline (object7-host-set! o::object7 v::obj) (with-access::object7 o (host) (set! host v)))

;; object8
(define-inline (object8?::bool obj::obj) ((@ is-a? __object) obj (@ object8 object5)))
(define-inline (make-object8::object8 value1631::byte) (instantiate::object8 (value value1631)))
(define (object8-nil::object8) (let ((new1632::object8 (let ((new1633::object8 (free-pragma::object8 ((BgL_object8z00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_object8z00_bgl) )))))) (object-class-num-set! new1633 ((@ class-num __object) (@ object8 object5))) (object-widening-set! new1633 #f) new1633))) (set! __bigloo__.new1632.value 0) new1632))
(define-inline (object8-value::byte o::object8) (with-access::object8 o (value) value))
(define-inline (object8-value-set! o::object8 v::byte) (with-access::object8 o (value) (set! value v)))

;; recursive-dog
(define-inline (recursive-dog?::bool obj::obj) ((@ is-a? __object) obj (@ recursive-dog object5)))
(define-inline (make-recursive-dog::recursive-dog name1627::obj next1628::recursive-dog) (instantiate::recursive-dog (name name1627) (next next1628)))
(define (recursive-dog-nil::recursive-dog) (let ((new1629::recursive-dog (let ((new1630::recursive-dog (free-pragma::recursive-dog ((BgL_recursivezd2dogzd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_recursivezd2dogzd2_bgl) )))))) (object-class-num-set! new1630 ((@ class-num __object) (@ recursive-dog object5))) (object-widening-set! new1630 #f) new1630))) (set! __bigloo__.new1629.name #unspecified) (set! __bigloo__.new1629.next (class-nil recursive-dog)) new1629))
(define-inline (recursive-dog-next::recursive-dog o::recursive-dog) (with-access::recursive-dog o (next) next))
(define-inline (recursive-dog-next-set! o::recursive-dog v::recursive-dog) (with-access::recursive-dog o (next) (set! next v)))
(define-inline (recursive-dog-name::obj o::recursive-dog) (with-access::recursive-dog o (name) name))
(define-inline (recursive-dog-name-set! o::recursive-dog v::obj) (with-access::recursive-dog o (name) (set! name v)))

))
