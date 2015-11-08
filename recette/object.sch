;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen object.scm)
;; ==========================================================

;; The directives
(directives

;; foo
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-foo::foo x1257::long y1258::string z1259::string)
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
    (inline make-gee::gee x1253::obj y1254::obj)
    (inline gee?::bool ::obj)
    (gee-nil::gee)
    (inline gee-y::obj ::gee)
    (inline gee-y-set! ::gee ::obj)
    (inline gee-x::obj ::gee)
    (inline gee-x-set! ::gee ::obj))))

;; foo/l
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-foo/l::foo/l x1247::long y1248::string z1249::string dummy1250::obj)
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
    (inline make-titi::titi x1245::int)
    (inline titi?::bool ::obj)
    (titi-nil::titi)
    (inline titi-x::int ::titi)
    (inline titi-x-set! ::titi ::int))))

;; toto
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-toto::toto x1239::int y1240::char yy1241::char z1242::obj t1243::obj)
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
    (inline make-fin::fin x1236::obj)
    (inline fin?::bool ::obj)
    (fin-nil::fin)
    (inline fin-x::obj ::fin)
    (inline fin-x-set! ::fin ::obj))))

;; sfin
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-sfin::sfin x1233::obj y1234::obj)
    (inline sfin?::bool ::obj)
    (sfin-nil::sfin)
    (inline sfin-y::obj ::sfin)
    (inline sfin-y-set! ::sfin ::obj)
    (inline sfin-x::obj ::sfin)
    (inline sfin-x-set! ::sfin ::obj))))

;; cfin
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cfin::cfin x1230::obj z1231::obj)
    (inline cfin?::bool ::obj)
    (cfin-nil::cfin)
    (inline cfin-z::obj ::cfin)
    (inline cfin-z-set! ::cfin ::obj)
    (inline cfin-x::obj ::cfin)
    (inline cfin-x-set! ::cfin ::obj))))

;; point
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-point::point x1227::obj y1228::obj)
    (inline point?::bool ::obj)
    (point-nil::point)
    (inline point-y::obj ::point)
    (inline point-y-set! ::point ::obj)
    (inline point-x::obj ::point)
    (inline point-x-set! ::point ::obj))))

;; pointc
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-pointc::pointc x1223::obj y1224::obj color1225::obj)
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
    (inline make-point3::point3 x1219::obj y1220::obj z1221::obj)
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
    (inline make-readc::readc x1217::obj)
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
    (inline make-virtual-2::virtual-2 z1213::obj t1214::obj)
    (inline virtual-2?::bool ::obj)
    (virtual-2-nil::virtual-2)
    (inline virtual-2-w::obj ::virtual-2)
    (inline virtual-2-w-set! ::virtual-2 ::obj)
    (inline virtual-2-t::obj ::virtual-2)
    (inline virtual-2-t-set! ::virtual-2 ::obj)
    (inline virtual-2-z::obj ::virtual-2)
    (inline virtual-2-x::obj ::virtual-2))))

;; rec-1
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rec-1::rec-1 f11211::rec-2)
    (inline rec-1?::bool ::obj)
    (rec-1-nil::rec-1)
    (inline rec-1-f1::rec-2 ::rec-1)
    (inline rec-1-f1-set! ::rec-1 ::rec-2))))

;; rec-2
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rec-2::rec-2 f21209::rec-1)
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
    (inline virtual-4-a::obj ::virtual-4))))

;; class/constr
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-class/constr::class/constr x1205::obj)
    (inline class/constr?::bool ::obj)
    (class/constr-nil::class/constr)
    (inline class/constr-x::obj ::class/constr)
    (inline class/constr-x-set! ::class/constr ::obj))))

;; class2/constr
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-class2/constr::class2/constr x1203::obj)
    (inline class2/constr?::bool ::obj)
    (class2/constr-nil::class2/constr)
    (inline class2/constr-x::obj ::class2/constr)
    (inline class2/constr-x-set! ::class2/constr ::obj))))

;; def-foo
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-def-foo::def-foo x1201::obj)
    (inline def-foo?::bool ::obj)
    (def-foo-nil::def-foo)
    (inline def-foo-x::obj ::def-foo)
    (inline def-foo-x-set! ::def-foo ::obj))))

;; def-bar
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-def-bar::def-bar x1198::obj y1199::obj)
    (inline def-bar?::bool ::obj)
    (def-bar-nil::def-bar)
    (inline def-bar-y::obj ::def-bar)
    (inline def-bar-y-set! ::def-bar ::obj)
    (inline def-bar-x::obj ::def-bar)
    (inline def-bar-x-set! ::def-bar ::obj))))

;; def-gee
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-def-gee::def-gee x1193::obj y1194::obj z1195::obj)
    (inline def-gee?::bool ::obj)
    (def-gee-nil::def-gee)
    (inline def-gee-z::obj ::def-gee)
    (inline def-gee-z-set! ::def-gee ::obj)
    (inline def-gee-y::obj ::def-gee)
    (inline def-gee-y-set! ::def-gee ::obj)
    (inline def-gee-x::obj ::def-gee)
    (inline def-gee-x-set! ::def-gee ::obj))))

;; deftest
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-deftest::deftest x1191::obj)
    (inline deftest?::bool ::obj)
    (deftest-nil::deftest)
    (inline deftest-x::obj ::deftest)
    (inline deftest-x-set! ::deftest ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; foo
(define-inline (make-foo::foo x1257::long y1258::string z1259::string) (instantiate::foo (x x1257) (y y1258) (z z1259)))
(define-inline (foo?::bool obj::obj) ((@ isa? __object) obj (@ foo object)))
(define (foo-nil::foo) (class-nil (@ foo object)))
(define-inline (foo-z::string o::foo) (-> |#!bigloo_wallow| o z))
(define-inline (foo-z-set! o::foo v::string) (set! (-> |#!bigloo_wallow| o z) v))
(define-inline (foo-y::string o::foo) (-> |#!bigloo_wallow| o y))
(define-inline (foo-y-set! o::foo v::string) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (foo-x::long o::foo) (-> |#!bigloo_wallow| o x))
(define-inline (foo-x-set! o::foo v::long) (set! (-> |#!bigloo_wallow| o x) v))

;; gee
(define-inline (make-gee::gee x1253::obj y1254::obj) (instantiate::gee (x x1253) (y y1254)))
(define-inline (gee?::bool obj::obj) ((@ isa? __object) obj (@ gee object)))
(define (gee-nil::gee) (class-nil (@ gee object)))
(define-inline (gee-y::obj o::gee) (-> |#!bigloo_wallow| o y))
(define-inline (gee-y-set! o::gee v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (gee-x::obj o::gee) (-> |#!bigloo_wallow| o x))
(define-inline (gee-x-set! o::gee v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; foo/l
(define-inline (make-foo/l::foo/l x1247::long y1248::string z1249::string dummy1250::obj) (instantiate::foo/l (x x1247) (y y1248) (z z1249) (dummy dummy1250)))
(define-inline (foo/l?::bool obj::obj) ((@ isa? __object) obj (@ foo/l object)))
(define (foo/l-nil::foo/l) (class-nil (@ foo/l object)))
(define-inline (foo/l-dummy::obj o::foo/l) (-> |#!bigloo_wallow| o dummy))
(define-inline (foo/l-dummy-set! o::foo/l v::obj) (set! (-> |#!bigloo_wallow| o dummy) v))
(define-inline (foo/l-z::string o::foo/l) (-> |#!bigloo_wallow| o z))
(define-inline (foo/l-z-set! o::foo/l v::string) (set! (-> |#!bigloo_wallow| o z) v))
(define-inline (foo/l-y::string o::foo/l) (-> |#!bigloo_wallow| o y))
(define-inline (foo/l-y-set! o::foo/l v::string) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (foo/l-x::long o::foo/l) (-> |#!bigloo_wallow| o x))
(define-inline (foo/l-x-set! o::foo/l v::long) (set! (-> |#!bigloo_wallow| o x) v))

;; titi
(define-inline (make-titi::titi x1245::int) (instantiate::titi (x x1245)))
(define-inline (titi?::bool obj::obj) ((@ isa? __object) obj (@ titi object)))
(define (titi-nil::titi) (class-nil (@ titi object)))
(define-inline (titi-x::int o::titi) (-> |#!bigloo_wallow| o x))
(define-inline (titi-x-set! o::titi v::int) (set! (-> |#!bigloo_wallow| o x) v))

;; toto
(define-inline (make-toto::toto x1239::int y1240::char yy1241::char z1242::obj t1243::obj) (instantiate::toto (x x1239) (y y1240) (yy yy1241) (z z1242) (t t1243)))
(define-inline (toto?::bool obj::obj) ((@ isa? __object) obj (@ toto object)))
(define (toto-nil::toto) (class-nil (@ toto object)))
(define-inline (toto-t::obj o::toto) (-> |#!bigloo_wallow| o t))
(define-inline (toto-t-set! o::toto v::obj) (set! (-> |#!bigloo_wallow| o t) v))
(define-inline (toto-z::obj o::toto) (-> |#!bigloo_wallow| o z))
(define-inline (toto-z-set! o::toto v::obj) (set! (-> |#!bigloo_wallow| o z) v))
(define-inline (toto-yy::char o::toto) (-> |#!bigloo_wallow| o yy))
#f
(define-inline (toto-y::char o::toto) (-> |#!bigloo_wallow| o y))
(define-inline (toto-y-set! o::toto v::char) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (toto-x::int o::toto) (-> |#!bigloo_wallow| o x))
(define-inline (toto-x-set! o::toto v::int) (set! (-> |#!bigloo_wallow| o x) v))

;; value
(define-inline (make-value::value) (instantiate::value))
(define-inline (value?::bool obj::obj) ((@ isa? __object) obj (@ value object)))
(define (value-nil::value) (class-nil (@ value object)))

;; fin
(define-inline (make-fin::fin x1236::obj) (instantiate::fin (x x1236)))
(define-inline (fin?::bool obj::obj) ((@ isa? __object) obj (@ fin object)))
(define (fin-nil::fin) (class-nil (@ fin object)))
(define-inline (fin-x::obj o::fin) (-> |#!bigloo_wallow| o x))
(define-inline (fin-x-set! o::fin v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; sfin
(define-inline (make-sfin::sfin x1233::obj y1234::obj) (instantiate::sfin (x x1233) (y y1234)))
(define-inline (sfin?::bool obj::obj) ((@ isa? __object) obj (@ sfin object)))
(define (sfin-nil::sfin) (class-nil (@ sfin object)))
(define-inline (sfin-y::obj o::sfin) (-> |#!bigloo_wallow| o y))
(define-inline (sfin-y-set! o::sfin v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (sfin-x::obj o::sfin) (-> |#!bigloo_wallow| o x))
(define-inline (sfin-x-set! o::sfin v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; cfin
(define-inline (make-cfin::cfin x1230::obj z1231::obj) (instantiate::cfin (x x1230) (z z1231)))
(define-inline (cfin?::bool obj::obj) ((@ isa? __object) obj (@ cfin object)))
(define (cfin-nil::cfin) (class-nil (@ cfin object)))
(define-inline (cfin-z::obj o::cfin) (-> |#!bigloo_wallow| o z))
(define-inline (cfin-z-set! o::cfin v::obj) (set! (-> |#!bigloo_wallow| o z) v))
(define-inline (cfin-x::obj o::cfin) (-> |#!bigloo_wallow| o x))
(define-inline (cfin-x-set! o::cfin v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; point
(define-inline (make-point::point x1227::obj y1228::obj) (instantiate::point (x x1227) (y y1228)))
(define-inline (point?::bool obj::obj) ((@ isa? __object) obj (@ point object)))
(define (point-nil::point) (class-nil (@ point object)))
(define-inline (point-y::obj o::point) (-> |#!bigloo_wallow| o y))
(define-inline (point-y-set! o::point v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (point-x::obj o::point) (-> |#!bigloo_wallow| o x))
(define-inline (point-x-set! o::point v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; pointc
(define-inline (make-pointc::pointc x1223::obj y1224::obj color1225::obj) (instantiate::pointc (x x1223) (y y1224) (color color1225)))
(define-inline (pointc?::bool obj::obj) ((@ isa? __object) obj (@ pointc object)))
(define (pointc-nil::pointc) (class-nil (@ pointc object)))
(define-inline (pointc-color::obj o::pointc) (-> |#!bigloo_wallow| o color))
(define-inline (pointc-color-set! o::pointc v::obj) (set! (-> |#!bigloo_wallow| o color) v))
(define-inline (pointc-y::obj o::pointc) (-> |#!bigloo_wallow| o y))
(define-inline (pointc-y-set! o::pointc v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (pointc-x::obj o::pointc) (-> |#!bigloo_wallow| o x))
(define-inline (pointc-x-set! o::pointc v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; point3
(define-inline (make-point3::point3 x1219::obj y1220::obj z1221::obj) (instantiate::point3 (x x1219) (y y1220) (z z1221)))
(define-inline (point3?::bool obj::obj) ((@ isa? __object) obj (@ point3 object)))
(define (point3-nil::point3) (class-nil (@ point3 object)))
(define-inline (point3-z::obj o::point3) (-> |#!bigloo_wallow| o z))
(define-inline (point3-z-set! o::point3 v::obj) (set! (-> |#!bigloo_wallow| o z) v))
(define-inline (point3-y::obj o::point3) (-> |#!bigloo_wallow| o y))
(define-inline (point3-y-set! o::point3 v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (point3-x::obj o::point3) (-> |#!bigloo_wallow| o x))
(define-inline (point3-x-set! o::point3 v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; readc
(define-inline (make-readc::readc x1217::obj) (instantiate::readc (x x1217)))
(define-inline (readc?::bool obj::obj) ((@ isa? __object) obj (@ readc object)))
(define (readc-nil::readc) (class-nil (@ readc object)))
(define-inline (readc-x::obj o::readc) (-> |#!bigloo_wallow| o x))
#f

;; virtual-1
(define-inline (make-virtual-1::virtual-1) (instantiate::virtual-1))
(define-inline (virtual-1?::bool obj::obj) ((@ isa? __object) obj (@ virtual-1 object)))
(define (virtual-1-nil::virtual-1) (class-nil (@ virtual-1 object)))
(define-inline (virtual-1-x::obj o::virtual-1) (-> |#!bigloo_wallow| o x))
#f

;; virtual-2
(define-inline (make-virtual-2::virtual-2 z1213::obj t1214::obj) (instantiate::virtual-2 (z z1213) (t t1214)))
(define-inline (virtual-2?::bool obj::obj) ((@ isa? __object) obj (@ virtual-2 object)))
(define (virtual-2-nil::virtual-2) (class-nil (@ virtual-2 object)))
(define-inline (virtual-2-w::obj o::virtual-2) (-> |#!bigloo_wallow| o w))
(define-inline (virtual-2-w-set! o::virtual-2 v::obj) (set! (-> |#!bigloo_wallow| o w) v))
(define-inline (virtual-2-t::obj o::virtual-2) (-> |#!bigloo_wallow| o t))
(define-inline (virtual-2-t-set! o::virtual-2 v::obj) (set! (-> |#!bigloo_wallow| o t) v))
(define-inline (virtual-2-z::obj o::virtual-2) (-> |#!bigloo_wallow| o z))
#f
(define-inline (virtual-2-x::obj o::virtual-2) (-> |#!bigloo_wallow| o x))
#f

;; rec-1
(define-inline (make-rec-1::rec-1 f11211::rec-2) (instantiate::rec-1 (f1 f11211)))
(define-inline (rec-1?::bool obj::obj) ((@ isa? __object) obj (@ rec-1 object)))
(define (rec-1-nil::rec-1) (class-nil (@ rec-1 object)))
(define-inline (rec-1-f1::rec-2 o::rec-1) (-> |#!bigloo_wallow| o f1))
(define-inline (rec-1-f1-set! o::rec-1 v::rec-2) (set! (-> |#!bigloo_wallow| o f1) v))

;; rec-2
(define-inline (make-rec-2::rec-2 f21209::rec-1) (instantiate::rec-2 (f2 f21209)))
(define-inline (rec-2?::bool obj::obj) ((@ isa? __object) obj (@ rec-2 object)))
(define (rec-2-nil::rec-2) (class-nil (@ rec-2 object)))
(define-inline (rec-2-f2::rec-1 o::rec-2) (-> |#!bigloo_wallow| o f2))
(define-inline (rec-2-f2-set! o::rec-2 v::rec-1) (set! (-> |#!bigloo_wallow| o f2) v))

;; virtual-3
(define-inline (make-virtual-3::virtual-3) (instantiate::virtual-3))
(define-inline (virtual-3?::bool obj::obj) ((@ isa? __object) obj (@ virtual-3 object)))
(define (virtual-3-nil::virtual-3) (class-nil (@ virtual-3 object)))
(define-inline (virtual-3-x::obj o::virtual-3) (-> |#!bigloo_wallow| o x))
#f
(define-inline (virtual-3-a::obj o::virtual-3) (-> |#!bigloo_wallow| o a))
#f

;; virtual-4
(define-inline (make-virtual-4::virtual-4) (instantiate::virtual-4))
(define-inline (virtual-4?::bool obj::obj) ((@ isa? __object) obj (@ virtual-4 object)))
(define (virtual-4-nil::virtual-4) (class-nil (@ virtual-4 object)))
(define-inline (virtual-4-z::obj o::virtual-4) (-> |#!bigloo_wallow| o z))
#f
(define-inline (virtual-4-x::obj o::virtual-4) (-> |#!bigloo_wallow| o x))
#f
(define-inline (virtual-4-a::obj o::virtual-4) (-> |#!bigloo_wallow| o a))
#f

;; class/constr
(define-inline (make-class/constr::class/constr x1205::obj) (instantiate::class/constr (x x1205)))
(define-inline (class/constr?::bool obj::obj) ((@ isa? __object) obj (@ class/constr object)))
(define (class/constr-nil::class/constr) (class-nil (@ class/constr object)))
(define-inline (class/constr-x::obj o::class/constr) (-> |#!bigloo_wallow| o x))
(define-inline (class/constr-x-set! o::class/constr v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; class2/constr
(define-inline (make-class2/constr::class2/constr x1203::obj) (instantiate::class2/constr (x x1203)))
(define-inline (class2/constr?::bool obj::obj) ((@ isa? __object) obj (@ class2/constr object)))
(define (class2/constr-nil::class2/constr) (class-nil (@ class2/constr object)))
(define-inline (class2/constr-x::obj o::class2/constr) (-> |#!bigloo_wallow| o x))
(define-inline (class2/constr-x-set! o::class2/constr v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; def-foo
(define-inline (make-def-foo::def-foo x1201::obj) (instantiate::def-foo (x x1201)))
(define-inline (def-foo?::bool obj::obj) ((@ isa? __object) obj (@ def-foo object)))
(define (def-foo-nil::def-foo) (class-nil (@ def-foo object)))
(define-inline (def-foo-x::obj o::def-foo) (-> |#!bigloo_wallow| o x))
(define-inline (def-foo-x-set! o::def-foo v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; def-bar
(define-inline (make-def-bar::def-bar x1198::obj y1199::obj) (instantiate::def-bar (x x1198) (y y1199)))
(define-inline (def-bar?::bool obj::obj) ((@ isa? __object) obj (@ def-bar object)))
(define (def-bar-nil::def-bar) (class-nil (@ def-bar object)))
(define-inline (def-bar-y::obj o::def-bar) (-> |#!bigloo_wallow| o y))
(define-inline (def-bar-y-set! o::def-bar v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (def-bar-x::obj o::def-bar) (-> |#!bigloo_wallow| o x))
(define-inline (def-bar-x-set! o::def-bar v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; def-gee
(define-inline (make-def-gee::def-gee x1193::obj y1194::obj z1195::obj) (instantiate::def-gee (x x1193) (y y1194) (z z1195)))
(define-inline (def-gee?::bool obj::obj) ((@ isa? __object) obj (@ def-gee object)))
(define (def-gee-nil::def-gee) (class-nil (@ def-gee object)))
(define-inline (def-gee-z::obj o::def-gee) (-> |#!bigloo_wallow| o z))
(define-inline (def-gee-z-set! o::def-gee v::obj) (set! (-> |#!bigloo_wallow| o z) v))
(define-inline (def-gee-y::obj o::def-gee) (-> |#!bigloo_wallow| o y))
(define-inline (def-gee-y-set! o::def-gee v::obj) (set! (-> |#!bigloo_wallow| o y) v))
(define-inline (def-gee-x::obj o::def-gee) (-> |#!bigloo_wallow| o x))
(define-inline (def-gee-x-set! o::def-gee v::obj) (set! (-> |#!bigloo_wallow| o x) v))

;; deftest
(define-inline (make-deftest::deftest x1191::obj) (instantiate::deftest (x x1191)))
(define-inline (deftest?::bool obj::obj) ((@ isa? __object) obj (@ deftest object)))
(define (deftest-nil::deftest) (class-nil (@ deftest object)))
(define-inline (deftest-x::obj o::deftest) (-> |#!bigloo_wallow| o x))
(define-inline (deftest-x-set! o::deftest v::obj) (set! (-> |#!bigloo_wallow| o x) v))
))
