;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:19:56 CET 2011 
;; (bigloo.new -classgen object.scm)
;; ==========================================================

;; The directives
(directives

;; foo
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-foo::foo x1288::long y1289::string z1290::string)
    (inline foo?::bool ::obj)
    (foo-nil::foo)
    (inline foo-z::string ::foo)
    (inline foo-z-set! ::foo ::string)
    (inline foo-y::string ::foo)
    (inline foo-y-set! ::foo ::string)
    (inline foo-x::long ::foo)
    (inline foo-x-set! ::foo ::long))))

;; gee
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-gee::gee x1285::obj y1286::obj)
    (inline gee?::bool ::obj)
    (gee-nil::gee)
    (inline gee-y::obj ::gee)
    (inline gee-y-set! ::gee ::obj)
    (inline gee-x::obj ::gee)
    (inline gee-x-set! ::gee ::obj))))

;; foo/l
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-foo/l::foo/l x1280::long y1281::string z1282::string dummy1283::obj)
    (inline foo/l?::bool ::obj)
    (foo/l-nil::foo/l)
    (inline foo/l-dummy::obj ::foo/l)
    (inline foo/l-dummy-set! ::foo/l ::obj)
    (inline foo/l-z::string ::foo/l)
    (inline foo/l-z-set! ::foo/l ::string)
    (inline foo/l-y::string ::foo/l)
    (inline foo/l-y-set! ::foo/l ::string)
    (inline foo/l-x::long ::foo/l)
    (inline foo/l-x-set! ::foo/l ::long))))

;; titi
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-titi::titi x1278::int)
    (inline titi?::bool ::obj)
    (titi-nil::titi)
    (inline titi-x::int ::titi)
    (inline titi-x-set! ::titi ::int))))

;; toto
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-toto::toto x1272::int y1273::char yy1274::char z1275::obj t1276::obj)
    (inline toto?::bool ::obj)
    (toto-nil::toto)
    (inline toto-t::obj ::toto)
    (inline toto-t-set! ::toto ::obj)
    (inline toto-z::obj ::toto)
    (inline toto-z-set! ::toto ::obj)
    (inline toto-yy::char ::toto)
    (inline toto-y::char ::toto)
    (inline toto-y-set! ::toto ::char)
    (inline toto-x::int ::toto)
    (inline toto-x-set! ::toto ::int))))

;; value
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-value::value)
    (inline value?::bool ::obj)
    (value-nil::value))))

;; fin
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-fin::fin x1269::obj)
    (inline fin?::bool ::obj)
    (fin-nil::fin)
    (inline fin-x::obj ::fin)
    (inline fin-x-set! ::fin ::obj))))

;; sfin
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-sfin::sfin x1266::obj y1267::obj)
    (inline sfin?::bool ::obj)
    (sfin-nil::sfin)
    (inline sfin-y::obj ::sfin)
    (inline sfin-y-set! ::sfin ::obj)
    (inline sfin-x::obj ::sfin)
    (inline sfin-x-set! ::sfin ::obj))))

;; cfin
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cfin::cfin x1263::obj z1264::obj)
    (inline cfin?::bool ::obj)
    (cfin-nil::cfin)
    (inline cfin-z::obj ::cfin)
    (inline cfin-z-set! ::cfin ::obj)
    (inline cfin-x::obj ::cfin)
    (inline cfin-x-set! ::cfin ::obj))))

;; point
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-point::point x1260::obj y1261::obj)
    (inline point?::bool ::obj)
    (point-nil::point)
    (inline point-y::obj ::point)
    (inline point-y-set! ::point ::obj)
    (inline point-x::obj ::point)
    (inline point-x-set! ::point ::obj))))

;; pointc
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-pointc::pointc x1256::obj y1257::obj color1258::obj)
    (inline pointc?::bool ::obj)
    (pointc-nil::pointc)
    (inline pointc-color::obj ::pointc)
    (inline pointc-color-set! ::pointc ::obj)
    (inline pointc-y::obj ::pointc)
    (inline pointc-y-set! ::pointc ::obj)
    (inline pointc-x::obj ::pointc)
    (inline pointc-x-set! ::pointc ::obj))))

;; point3
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-point3::point3 x1252::obj y1253::obj z1254::obj)
    (inline point3?::bool ::obj)
    (point3-nil::point3)
    (inline point3-z::obj ::point3)
    (inline point3-z-set! ::point3 ::obj)
    (inline point3-y::obj ::point3)
    (inline point3-y-set! ::point3 ::obj)
    (inline point3-x::obj ::point3)
    (inline point3-x-set! ::point3 ::obj))))

;; readc
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-readc::readc x1250::obj)
    (inline readc?::bool ::obj)
    (readc-nil::readc)
    (inline readc-x::obj ::readc))))

;; virtual-1
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-virtual-1::virtual-1)
    (inline virtual-1?::bool ::obj)
    (virtual-1-nil::virtual-1)
    (inline virtual-1-x::obj ::virtual-1))))

;; virtual-2
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-virtual-2::virtual-2 z1246::obj t1247::obj)
    (inline virtual-2?::bool ::obj)
    (virtual-2-nil::virtual-2)
    (inline virtual-2-x::obj ::virtual-2)
    (inline virtual-2-w::obj ::virtual-2)
    (inline virtual-2-w-set! ::virtual-2 ::obj)
    (inline virtual-2-t::obj ::virtual-2)
    (inline virtual-2-t-set! ::virtual-2 ::obj)
    (inline virtual-2-z::obj ::virtual-2))))

;; rec-1
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rec-1::rec-1 f11244::rec-2)
    (inline rec-1?::bool ::obj)
    (rec-1-nil::rec-1)
    (inline rec-1-f1::rec-2 ::rec-1)
    (inline rec-1-f1-set! ::rec-1 ::rec-2))))

;; rec-2
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rec-2::rec-2 f21242::rec-1)
    (inline rec-2?::bool ::obj)
    (rec-2-nil::rec-2)
    (inline rec-2-f2::rec-1 ::rec-2)
    (inline rec-2-f2-set! ::rec-2 ::rec-1))))

;; virtual-3
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-virtual-3::virtual-3)
    (inline virtual-3?::bool ::obj)
    (virtual-3-nil::virtual-3)
    (inline virtual-3-x::obj ::virtual-3)
    (inline virtual-3-a::obj ::virtual-3))))

;; virtual-4
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-virtual-4::virtual-4)
    (inline virtual-4?::bool ::obj)
    (virtual-4-nil::virtual-4)
    (inline virtual-4-z::obj ::virtual-4)
    (inline virtual-4-x::obj ::virtual-4)
    (inline virtual-4-a::obj ::virtual-4)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; foo
(define-inline (make-foo::foo x1288::long y1289::string z1290::string) (instantiate::foo (x x1288) (y y1289) (z z1290)))
(define-inline (foo?::bool obj::obj) ((@ isa? __object) obj (@ foo object)))
(define (foo-nil::foo) (class-nil (@ foo object)))
(define-inline (foo-z::string o::foo) (with-access::foo o (z) z))
(define-inline (foo-z-set! o::foo v::string) (with-access::foo o (z) (set! z v)))
(define-inline (foo-y::string o::foo) (with-access::foo o (y) y))
(define-inline (foo-y-set! o::foo v::string) (with-access::foo o (y) (set! y v)))
(define-inline (foo-x::long o::foo) (with-access::foo o (x) x))
(define-inline (foo-x-set! o::foo v::long) (with-access::foo o (x) (set! x v)))

;; gee
(define-inline (make-gee::gee x1285::obj y1286::obj) (instantiate::gee (x x1285) (y y1286)))
(define-inline (gee?::bool obj::obj) ((@ isa? __object) obj (@ gee object)))
(define (gee-nil::gee) (class-nil (@ gee object)))
(define-inline (gee-y::obj o::gee) (with-access::gee o (y) y))
(define-inline (gee-y-set! o::gee v::obj) (with-access::gee o (y) (set! y v)))
(define-inline (gee-x::obj o::gee) (with-access::gee o (x) x))
(define-inline (gee-x-set! o::gee v::obj) (with-access::gee o (x) (set! x v)))

;; foo/l
(define-inline (make-foo/l::foo/l x1280::long y1281::string z1282::string dummy1283::obj) (instantiate::foo/l (x x1280) (y y1281) (z z1282) (dummy dummy1283)))
(define-inline (foo/l?::bool obj::obj) ((@ isa? __object) obj (@ foo/l object)))
(define (foo/l-nil::foo/l) (class-nil (@ foo/l object)))
(define-inline (foo/l-dummy::obj o::foo/l) (with-access::foo/l o (dummy) dummy))
(define-inline (foo/l-dummy-set! o::foo/l v::obj) (with-access::foo/l o (dummy) (set! dummy v)))
(define-inline (foo/l-z::string o::foo/l) (with-access::foo/l o (z) z))
(define-inline (foo/l-z-set! o::foo/l v::string) (with-access::foo/l o (z) (set! z v)))
(define-inline (foo/l-y::string o::foo/l) (with-access::foo/l o (y) y))
(define-inline (foo/l-y-set! o::foo/l v::string) (with-access::foo/l o (y) (set! y v)))
(define-inline (foo/l-x::long o::foo/l) (with-access::foo/l o (x) x))
(define-inline (foo/l-x-set! o::foo/l v::long) (with-access::foo/l o (x) (set! x v)))

;; titi
(define-inline (make-titi::titi x1278::int) (instantiate::titi (x x1278)))
(define-inline (titi?::bool obj::obj) ((@ isa? __object) obj (@ titi object)))
(define (titi-nil::titi) (class-nil (@ titi object)))
(define-inline (titi-x::int o::titi) (with-access::titi o (x) x))
(define-inline (titi-x-set! o::titi v::int) (with-access::titi o (x) (set! x v)))

;; toto
(define-inline (make-toto::toto x1272::int y1273::char yy1274::char z1275::obj t1276::obj) (instantiate::toto (x x1272) (y y1273) (yy yy1274) (z z1275) (t t1276)))
(define-inline (toto?::bool obj::obj) ((@ isa? __object) obj (@ toto object)))
(define (toto-nil::toto) (class-nil (@ toto object)))
(define-inline (toto-t::obj o::toto) (with-access::toto o (t) t))
(define-inline (toto-t-set! o::toto v::obj) (with-access::toto o (t) (set! t v)))
(define-inline (toto-z::obj o::toto) (with-access::toto o (z) z))
(define-inline (toto-z-set! o::toto v::obj) (with-access::toto o (z) (set! z v)))
(define-inline (toto-yy::char o::toto) (with-access::toto o (yy) yy))
(define-inline (toto-yy-set! o::toto v::char) (with-access::toto o (yy) (set! yy v)))
(define-inline (toto-y::char o::toto) (with-access::toto o (y) y))
(define-inline (toto-y-set! o::toto v::char) (with-access::toto o (y) (set! y v)))
(define-inline (toto-x::int o::toto) (with-access::toto o (x) x))
(define-inline (toto-x-set! o::toto v::int) (with-access::toto o (x) (set! x v)))

;; value
(define-inline (make-value::value) (instantiate::value))
(define-inline (value?::bool obj::obj) ((@ isa? __object) obj (@ value object)))
(define (value-nil::value) (class-nil (@ value object)))

;; fin
(define-inline (make-fin::fin x1269::obj) (instantiate::fin (x x1269)))
(define-inline (fin?::bool obj::obj) ((@ isa? __object) obj (@ fin object)))
(define (fin-nil::fin) (class-nil (@ fin object)))
(define-inline (fin-x::obj o::fin) (with-access::fin o (x) x))
(define-inline (fin-x-set! o::fin v::obj) (with-access::fin o (x) (set! x v)))

;; sfin
(define-inline (make-sfin::sfin x1266::obj y1267::obj) (instantiate::sfin (x x1266) (y y1267)))
(define-inline (sfin?::bool obj::obj) ((@ isa? __object) obj (@ sfin object)))
(define (sfin-nil::sfin) (class-nil (@ sfin object)))
(define-inline (sfin-y::obj o::sfin) (with-access::sfin o (y) y))
(define-inline (sfin-y-set! o::sfin v::obj) (with-access::sfin o (y) (set! y v)))
(define-inline (sfin-x::obj o::sfin) (with-access::sfin o (x) x))
(define-inline (sfin-x-set! o::sfin v::obj) (with-access::sfin o (x) (set! x v)))

;; cfin
(define-inline (make-cfin::cfin x1263::obj z1264::obj) (instantiate::cfin (x x1263) (z z1264)))
(define-inline (cfin?::bool obj::obj) ((@ isa? __object) obj (@ cfin object)))
(define (cfin-nil::cfin) (class-nil (@ cfin object)))
(define-inline (cfin-z::obj o::cfin) (with-access::cfin o (z) z))
(define-inline (cfin-z-set! o::cfin v::obj) (with-access::cfin o (z) (set! z v)))
(define-inline (cfin-x::obj o::cfin) (with-access::cfin o (x) x))
(define-inline (cfin-x-set! o::cfin v::obj) (with-access::cfin o (x) (set! x v)))

;; point
(define-inline (make-point::point x1260::obj y1261::obj) (instantiate::point (x x1260) (y y1261)))
(define-inline (point?::bool obj::obj) ((@ isa? __object) obj (@ point object)))
(define (point-nil::point) (class-nil (@ point object)))
(define-inline (point-y::obj o::point) (with-access::point o (y) y))
(define-inline (point-y-set! o::point v::obj) (with-access::point o (y) (set! y v)))
(define-inline (point-x::obj o::point) (with-access::point o (x) x))
(define-inline (point-x-set! o::point v::obj) (with-access::point o (x) (set! x v)))

;; pointc
(define-inline (make-pointc::pointc x1256::obj y1257::obj color1258::obj) (instantiate::pointc (x x1256) (y y1257) (color color1258)))
(define-inline (pointc?::bool obj::obj) ((@ isa? __object) obj (@ pointc object)))
(define (pointc-nil::pointc) (class-nil (@ pointc object)))
(define-inline (pointc-color::obj o::pointc) (with-access::pointc o (color) color))
(define-inline (pointc-color-set! o::pointc v::obj) (with-access::pointc o (color) (set! color v)))
(define-inline (pointc-y::obj o::pointc) (with-access::pointc o (y) y))
(define-inline (pointc-y-set! o::pointc v::obj) (with-access::pointc o (y) (set! y v)))
(define-inline (pointc-x::obj o::pointc) (with-access::pointc o (x) x))
(define-inline (pointc-x-set! o::pointc v::obj) (with-access::pointc o (x) (set! x v)))

;; point3
(define-inline (make-point3::point3 x1252::obj y1253::obj z1254::obj) (instantiate::point3 (x x1252) (y y1253) (z z1254)))
(define-inline (point3?::bool obj::obj) ((@ isa? __object) obj (@ point3 object)))
(define (point3-nil::point3) (class-nil (@ point3 object)))
(define-inline (point3-z::obj o::point3) (with-access::point3 o (z) z))
(define-inline (point3-z-set! o::point3 v::obj) (with-access::point3 o (z) (set! z v)))
(define-inline (point3-y::obj o::point3) (with-access::point3 o (y) y))
(define-inline (point3-y-set! o::point3 v::obj) (with-access::point3 o (y) (set! y v)))
(define-inline (point3-x::obj o::point3) (with-access::point3 o (x) x))
(define-inline (point3-x-set! o::point3 v::obj) (with-access::point3 o (x) (set! x v)))

;; readc
(define-inline (make-readc::readc x1250::obj) (instantiate::readc (x x1250)))
(define-inline (readc?::bool obj::obj) ((@ isa? __object) obj (@ readc object)))
(define (readc-nil::readc) (class-nil (@ readc object)))
(define-inline (readc-x::obj o::readc) (with-access::readc o (x) x))
(define-inline (readc-x-set! o::readc v::obj) (with-access::readc o (x) (set! x v)))

;; virtual-1
(define-inline (make-virtual-1::virtual-1) (instantiate::virtual-1))
(define-inline (virtual-1?::bool obj::obj) ((@ isa? __object) obj (@ virtual-1 object)))
(define (virtual-1-nil::virtual-1) (class-nil (@ virtual-1 object)))
(define-inline (virtual-1-x::obj o::virtual-1) (with-access::virtual-1 o (x) x))
#f

;; virtual-2
(define-inline (make-virtual-2::virtual-2 z1246::obj t1247::obj) (instantiate::virtual-2 (z z1246) (t t1247)))
(define-inline (virtual-2?::bool obj::obj) ((@ isa? __object) obj (@ virtual-2 object)))
(define (virtual-2-nil::virtual-2) (class-nil (@ virtual-2 object)))
(define-inline (virtual-2-x::obj o::virtual-2) (with-access::virtual-2 o (x) x))
#f
(define-inline (virtual-2-w::obj o::virtual-2) (with-access::virtual-2 o (w) w))
(define-inline (virtual-2-w-set! o::virtual-2 v::obj) (with-access::virtual-2 o (w) (set! w v)))
(define-inline (virtual-2-t::obj o::virtual-2) (with-access::virtual-2 o (t) t))
(define-inline (virtual-2-t-set! o::virtual-2 v::obj) (with-access::virtual-2 o (t) (set! t v)))
(define-inline (virtual-2-z::obj o::virtual-2) (with-access::virtual-2 o (z) z))
(define-inline (virtual-2-z-set! o::virtual-2 v::obj) (with-access::virtual-2 o (z) (set! z v)))

;; rec-1
(define-inline (make-rec-1::rec-1 f11244::rec-2) (instantiate::rec-1 (f1 f11244)))
(define-inline (rec-1?::bool obj::obj) ((@ isa? __object) obj (@ rec-1 object)))
(define (rec-1-nil::rec-1) (class-nil (@ rec-1 object)))
(define-inline (rec-1-f1::rec-2 o::rec-1) (with-access::rec-1 o (f1) f1))
(define-inline (rec-1-f1-set! o::rec-1 v::rec-2) (with-access::rec-1 o (f1) (set! f1 v)))

;; rec-2
(define-inline (make-rec-2::rec-2 f21242::rec-1) (instantiate::rec-2 (f2 f21242)))
(define-inline (rec-2?::bool obj::obj) ((@ isa? __object) obj (@ rec-2 object)))
(define (rec-2-nil::rec-2) (class-nil (@ rec-2 object)))
(define-inline (rec-2-f2::rec-1 o::rec-2) (with-access::rec-2 o (f2) f2))
(define-inline (rec-2-f2-set! o::rec-2 v::rec-1) (with-access::rec-2 o (f2) (set! f2 v)))

;; virtual-3
(define-inline (make-virtual-3::virtual-3) (instantiate::virtual-3))
(define-inline (virtual-3?::bool obj::obj) ((@ isa? __object) obj (@ virtual-3 object)))
(define (virtual-3-nil::virtual-3) (class-nil (@ virtual-3 object)))
(define-inline (virtual-3-x::obj o::virtual-3) (with-access::virtual-3 o (x) x))
#f
(define-inline (virtual-3-a::obj o::virtual-3) (with-access::virtual-3 o (a) a))
#f

;; virtual-4
(define-inline (make-virtual-4::virtual-4) (instantiate::virtual-4))
(define-inline (virtual-4?::bool obj::obj) ((@ isa? __object) obj (@ virtual-4 object)))
(define (virtual-4-nil::virtual-4) (class-nil (@ virtual-4 object)))
(define-inline (virtual-4-z::obj o::virtual-4) (with-access::virtual-4 o (z) z))
#f
(define-inline (virtual-4-x::obj o::virtual-4) (with-access::virtual-4 o (x) x))
#f
(define-inline (virtual-4-a::obj o::virtual-4) (with-access::virtual-4 o (a) a))
#f
))
