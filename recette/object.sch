;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 7 12:41:21 CET 2011 
;; (bigloo.new -classgen object.scm)
;; ==========================================================

;; The directives
(directives
   (cond-expand ((and bigloo-class-sans (not bigloo-class-generate))

;; foo
(static
  (inline foo? ::obj)
  (inline make-foo::foo x2185::long y2186::string z2187::string)
  (foo-nil::foo)
  (inline foo-z::string ::foo)
  (inline foo-z-set! ::foo ::string)
  (inline foo-y::string ::foo)
  (inline foo-y-set! ::foo ::string)
  (inline foo-x::long ::foo)
  (inline foo-x-set! ::foo ::long)
)
;; gee
(static
  (inline gee? ::obj)
  (inline make-gee::gee x2181::obj y2182::obj)
  (gee-nil::gee)
  (inline gee-y::obj ::gee)
  (inline gee-y-set! ::gee ::obj)
  (inline gee-x::obj ::gee)
  (inline gee-x-set! ::gee ::obj)
)
;; foo/l
(static
  (inline foo/l? ::obj)
  (inline make-foo/l::foo/l dummy2178::obj)
  (foo/l-nil::foo/l)
  (inline foo/l-dummy::obj ::foo/l)
  (inline foo/l-dummy-set! ::foo/l ::obj)
)
;; titi
(static
  (inline titi? ::obj)
  (inline make-titi::titi x2175::int)
  (titi-nil::titi)
  (inline titi-x::int ::titi)
  (inline titi-x-set! ::titi ::int)
)
;; toto
(static
  (inline toto? ::obj)
  (inline make-toto::toto x2168::int y2169::char yy2170::char z2171::obj t2172::obj)
  (toto-nil::toto)
  (inline toto-t::obj ::toto)
  (inline toto-t-set! ::toto ::obj)
  (inline toto-z::obj ::toto)
  (inline toto-z-set! ::toto ::obj)
  (inline toto-yy::char ::toto)
  (inline toto-yy-set! ::toto ::char)
  (inline toto-y::char ::toto)
  (inline toto-y-set! ::toto ::char)
  (inline toto-x::int ::toto)
  (inline toto-x-set! ::toto ::int)
)
;; value
(export
  (inline value? ::obj)
  (inline make-value::value)
  (value-nil::value)
)
;; fin
(export
  (inline fin? ::obj)
  (inline make-fin::fin x2163::obj)
  (fin-nil::fin)
  (inline fin-x::obj ::fin)
  (inline fin-x-set! ::fin ::obj)
)
;; sfin
(export
  (inline sfin? ::obj)
  (inline make-sfin::sfin x2159::obj y2160::obj)
  (sfin-nil::sfin)
  (inline sfin-y::obj ::sfin)
  (inline sfin-y-set! ::sfin ::obj)
  (inline sfin-x::obj ::sfin)
  (inline sfin-x-set! ::sfin ::obj)
)
;; cfin
(export
  (inline cfin? ::obj)
  (inline make-cfin::cfin x2155::obj z2156::obj)
  (cfin-nil::cfin)
  (inline cfin-z::obj ::cfin)
  (inline cfin-z-set! ::cfin ::obj)
  (inline cfin-x::obj ::cfin)
  (inline cfin-x-set! ::cfin ::obj)
)
;; point
(static
  (inline point? ::obj)
  (inline make-point::point x2151::obj y2152::obj)
  (point-nil::point)
  (inline point-y::obj ::point)
  (inline point-y-set! ::point ::obj)
  (inline point-x::obj ::point)
  (inline point-x-set! ::point ::obj)
)
;; pointc
(static
  (inline pointc? ::obj)
  (inline make-pointc::pointc color2148::obj)
  (pointc-nil::pointc)
  (inline pointc-color::obj ::pointc)
  (inline pointc-color-set! ::pointc ::obj)
)
;; point3
(static
  (inline point3? ::obj)
  (inline make-point3::point3 z2145::obj)
  (point3-nil::point3)
  (inline point3-z::obj ::point3)
  (inline point3-z-set! ::point3 ::obj)
)
;; readc
(static
  (inline readc? ::obj)
  (inline make-readc::readc x2142::obj)
  (readc-nil::readc)
  (inline readc-x::obj ::readc)
  (inline readc-x-set! ::readc ::obj)
)
;; virtual-1
(static
  (inline virtual-1? ::obj)
  (inline make-virtual-1::virtual-1)
  (virtual-1-nil::virtual-1)
  (inline virtual-1-x::obj ::virtual-1)
  (inline virtual-1-x-set! ::virtual-1 ::obj)
)
;; virtual-2
(static
  (inline virtual-2? ::obj)
  (inline make-virtual-2::virtual-2 z2136::obj t2137::obj)
  (virtual-2-nil::virtual-2)
  (inline virtual-2-x::obj ::virtual-2)
  (inline virtual-2-x-set! ::virtual-2 ::obj)
  (inline virtual-2-w::obj ::virtual-2)
  (inline virtual-2-w-set! ::virtual-2 ::obj)
  (inline virtual-2-t::obj ::virtual-2)
  (inline virtual-2-t-set! ::virtual-2 ::obj)
  (inline virtual-2-z::obj ::virtual-2)
  (inline virtual-2-z-set! ::virtual-2 ::obj)
)
;; rec-1
(static
  (inline rec-1? ::obj)
  (inline make-rec-1::rec-1 f12133::rec-2)
  (rec-1-nil::rec-1)
  (inline rec-1-f1::rec-2 ::rec-1)
  (inline rec-1-f1-set! ::rec-1 ::rec-2)
)
;; rec-2
(static
  (inline rec-2? ::obj)
  (inline make-rec-2::rec-2 f22130::rec-1)
  (rec-2-nil::rec-2)
  (inline rec-2-f2::rec-1 ::rec-2)
  (inline rec-2-f2-set! ::rec-2 ::rec-1)
)
;; virtual-3
(export
  (inline virtual-3? ::obj)
  (inline make-virtual-3::virtual-3)
  (virtual-3-nil::virtual-3)
  (inline virtual-3-x::obj ::virtual-3)
  (inline virtual-3-x-set! ::virtual-3 ::obj)
  (inline virtual-3-a::obj ::virtual-3)
  (inline virtual-3-a-set! ::virtual-3 ::obj)
)
;; virtual-4
(export
  (inline virtual-4? ::obj)
  (inline make-virtual-4::virtual-4)
  (virtual-4-nil::virtual-4)
  (inline virtual-4-z::obj ::virtual-4)
  (inline virtual-4-z-set! ::virtual-4 ::obj)
  (inline virtual-4-x::obj ::virtual-4)
  (inline virtual-4-x-set! ::virtual-4 ::obj)
  (inline virtual-4-a::obj ::virtual-4)
  (inline virtual-4-a-set! ::virtual-4 ::obj)
)
)))

;; The definitions
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))

;; foo
(define-inline (foo?::bool obj::obj) ((@ is-a? __object) obj (@ foo object)))
(define-inline (make-foo::foo x2185::long y2186::string z2187::string) (instantiate::foo (x x2185) (y y2186) (z z2187)))
(define (foo-nil::foo) (let ((new2188::foo (let ((new2189::foo (free-pragma::foo ((BgL_fooz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_fooz00_bgl) )))))) (object-class-num-set! new2189 ((@ class-num __object) (@ foo object))) (object-widening-set! new2189 #f) new2189))) (set! __bigloo__.new2188.x 0) (set! __bigloo__.new2188.y ) (set! __bigloo__.new2188.z ) new2188))
(define-inline (foo-z::string o::foo) (with-access::foo o (z) z))
(define-inline (foo-z-set! o::foo v::string) (with-access::foo o (z) (set! z v)))
(define-inline (foo-y::string o::foo) (with-access::foo o (y) y))
(define-inline (foo-y-set! o::foo v::string) (with-access::foo o (y) (set! y v)))
(define-inline (foo-x::long o::foo) (with-access::foo o (x) x))
(define-inline (foo-x-set! o::foo v::long) (with-access::foo o (x) (set! x v)))

;; gee
(define-inline (gee?::bool obj::obj) ((@ is-a? __object) obj (@ gee object)))
(define-inline (make-gee::gee x2181::obj y2182::obj) (instantiate::gee (x x2181) (y y2182)))
(define (gee-nil::gee) (let ((new2183::gee (let ((new2184::gee (free-pragma::gee ((BgL_geez00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_geez00_bgl) )))))) (object-class-num-set! new2184 ((@ class-num __object) (@ gee object))) (object-widening-set! new2184 #f) new2184))) (set! __bigloo__.new2183.x #unspecified) (set! __bigloo__.new2183.y #unspecified) new2183))
(define-inline (gee-y::obj o::gee) (with-access::gee o (y) y))
(define-inline (gee-y-set! o::gee v::obj) (with-access::gee o (y) (set! y v)))
(define-inline (gee-x::obj o::gee) (with-access::gee o (x) x))
(define-inline (gee-x-set! o::gee v::obj) (with-access::gee o (x) (set! x v)))

;; foo/l
(define-inline (foo/l?::bool obj::obj) ((@ is-a? __object) obj (@ foo/l object)))
(define-inline (make-foo/l::foo/l dummy2178::obj) (instantiate::foo/l (dummy dummy2178)))
(define (foo/l-nil::foo/l) (let ((new2179::foo/l (let ((new2180::foo/l (free-pragma::foo/l ((BgL_foozf2lzf2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_foozf2lzf2_bgl) )))))) (object-class-num-set! new2180 ((@ class-num __object) (@ foo/l object))) (object-widening-set! new2180 #f) new2180))) (set! __bigloo__.new2179.dummy #unspecified) new2179))
(define-inline (foo/l-dummy::obj o::foo/l) (with-access::foo/l o (dummy) dummy))
(define-inline (foo/l-dummy-set! o::foo/l v::obj) (with-access::foo/l o (dummy) (set! dummy v)))

;; titi
(define-inline (titi?::bool obj::obj) ((@ is-a? __object) obj (@ titi object)))
(define-inline (make-titi::titi x2175::int) (instantiate::titi (x x2175)))
(define (titi-nil::titi) (let ((new2176::titi (let ((new2177::titi (free-pragma::titi ((BgL_titiz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_titiz00_bgl) )))))) (object-class-num-set! new2177 ((@ class-num __object) (@ titi object))) (object-widening-set! new2177 #f) new2177))) (set! __bigloo__.new2176.x 0) new2176))
(define-inline (titi-x::int o::titi) (with-access::titi o (x) x))
(define-inline (titi-x-set! o::titi v::int) (with-access::titi o (x) (set! x v)))

;; toto
(define-inline (toto?::bool obj::obj) ((@ is-a? __object) obj (@ toto object)))
(define-inline (make-toto::toto x2168::int y2169::char yy2170::char z2171::obj t2172::obj) (instantiate::toto (x x2168) (y y2169) (yy yy2170) (z z2171) (t t2172)))
(define (toto-nil::toto) (let ((new2173::toto (let ((new2174::toto (free-pragma::toto ((BgL_totoz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_totoz00_bgl) )))))) (object-class-num-set! new2174 ((@ class-num __object) (@ toto object))) (object-widening-set! new2174 #f) new2174))) (set! __bigloo__.new2173.x 0) (set! __bigloo__.new2173.y _) (set! __bigloo__.new2173.yy _) (set! __bigloo__.new2173.z #unspecified) (set! __bigloo__.new2173.t #unspecified) new2173))
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
(define-inline (value?::bool obj::obj) ((@ is-a? __object) obj (@ value object)))
(define-inline (make-value::value) (instantiate::value))
(define (value-nil::value) (let ((new2166::value (let ((new2167::value (free-pragma::value ((BgL_valuez00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_valuez00_bgl) )))))) (object-class-num-set! new2167 ((@ class-num __object) (@ value object))) (object-widening-set! new2167 #f) new2167))) new2166))

;; fin
(define-inline (fin?::bool obj::obj) ((@ is-a? __object) obj (@ fin object)))
(define-inline (make-fin::fin x2163::obj) (instantiate::fin (x x2163)))
(define (fin-nil::fin) (let ((new2164::fin (let ((new2165::fin (free-pragma::fin ((BgL_finz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_finz00_bgl) )))))) (object-class-num-set! new2165 ((@ class-num __object) (@ fin object))) (object-widening-set! new2165 #f) new2165))) (set! __bigloo__.new2164.x #unspecified) new2164))
(define-inline (fin-x::obj o::fin) (with-access::fin o (x) x))
(define-inline (fin-x-set! o::fin v::obj) (with-access::fin o (x) (set! x v)))

;; sfin
(define-inline (sfin?::bool obj::obj) ((@ is-a? __object) obj (@ sfin object)))
(define-inline (make-sfin::sfin x2159::obj y2160::obj) (instantiate::sfin (x x2159) (y y2160)))
(define (sfin-nil::sfin) (let ((new2161::sfin (let ((new2162::sfin (free-pragma::sfin ((BgL_sfinz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_sfinz00_bgl) )))))) (object-class-num-set! new2162 ((@ class-num __object) (@ sfin object))) (object-widening-set! new2162 #f) new2162))) (set! __bigloo__.new2161.x #unspecified) (set! __bigloo__.new2161.y #unspecified) new2161))
(define-inline (sfin-y::obj o::sfin) (with-access::sfin o (y) y))
(define-inline (sfin-y-set! o::sfin v::obj) (with-access::sfin o (y) (set! y v)))
(define-inline (sfin-x::obj o::sfin) (with-access::sfin o (x) x))
(define-inline (sfin-x-set! o::sfin v::obj) (with-access::sfin o (x) (set! x v)))

;; cfin
(define-inline (cfin?::bool obj::obj) ((@ is-a? __object) obj (@ cfin object)))
(define-inline (make-cfin::cfin x2155::obj z2156::obj) (instantiate::cfin (x x2155) (z z2156)))
(define (cfin-nil::cfin) (let ((new2157::cfin (let ((new2158::cfin (free-pragma::cfin ((BgL_cfinz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_cfinz00_bgl) )))))) (object-class-num-set! new2158 ((@ class-num __object) (@ cfin object))) (object-widening-set! new2158 #f) new2158))) (set! __bigloo__.new2157.x #unspecified) (set! __bigloo__.new2157.z #unspecified) new2157))
(define-inline (cfin-z::obj o::cfin) (with-access::cfin o (z) z))
(define-inline (cfin-z-set! o::cfin v::obj) (with-access::cfin o (z) (set! z v)))
(define-inline (cfin-x::obj o::cfin) (with-access::cfin o (x) x))
(define-inline (cfin-x-set! o::cfin v::obj) (with-access::cfin o (x) (set! x v)))

;; point
(define-inline (point?::bool obj::obj) ((@ is-a? __object) obj (@ point object)))
(define-inline (make-point::point x2151::obj y2152::obj) (instantiate::point (x x2151) (y y2152)))
(define (point-nil::point) (let ((new2153::point (let ((new2154::point (free-pragma::point ((BgL_pointz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_pointz00_bgl) )))))) (object-class-num-set! new2154 ((@ class-num __object) (@ point object))) (object-widening-set! new2154 #f) new2154))) (set! __bigloo__.new2153.x #unspecified) (set! __bigloo__.new2153.y #unspecified) new2153))
(define-inline (point-y::obj o::point) (with-access::point o (y) y))
(define-inline (point-y-set! o::point v::obj) (with-access::point o (y) (set! y v)))
(define-inline (point-x::obj o::point) (with-access::point o (x) x))
(define-inline (point-x-set! o::point v::obj) (with-access::point o (x) (set! x v)))

;; pointc
(define-inline (pointc?::bool obj::obj) ((@ is-a? __object) obj (@ pointc object)))
(define-inline (make-pointc::pointc color2148::obj) (instantiate::pointc (color color2148)))
(define (pointc-nil::pointc) (let ((new2149::pointc (let ((new2150::pointc (free-pragma::pointc ((BgL_pointcz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_pointcz00_bgl) )))))) (object-class-num-set! new2150 ((@ class-num __object) (@ pointc object))) (object-widening-set! new2150 #f) new2150))) (set! __bigloo__.new2149.color #unspecified) new2149))
(define-inline (pointc-color::obj o::pointc) (with-access::pointc o (color) color))
(define-inline (pointc-color-set! o::pointc v::obj) (with-access::pointc o (color) (set! color v)))

;; point3
(define-inline (point3?::bool obj::obj) ((@ is-a? __object) obj (@ point3 object)))
(define-inline (make-point3::point3 z2145::obj) (instantiate::point3 (z z2145)))
(define (point3-nil::point3) (let ((new2146::point3 (let ((new2147::point3 (free-pragma::point3 ((BgL_point3z00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_point3z00_bgl) )))))) (object-class-num-set! new2147 ((@ class-num __object) (@ point3 object))) (object-widening-set! new2147 #f) new2147))) (set! __bigloo__.new2146.z #unspecified) new2146))
(define-inline (point3-z::obj o::point3) (with-access::point3 o (z) z))
(define-inline (point3-z-set! o::point3 v::obj) (with-access::point3 o (z) (set! z v)))

;; readc
(define-inline (readc?::bool obj::obj) ((@ is-a? __object) obj (@ readc object)))
(define-inline (make-readc::readc x2142::obj) (instantiate::readc (x x2142)))
(define (readc-nil::readc) (let ((new2143::readc (let ((new2144::readc (free-pragma::readc ((BgL_readcz00_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_readcz00_bgl) )))))) (object-class-num-set! new2144 ((@ class-num __object) (@ readc object))) (object-widening-set! new2144 #f) new2144))) (set! __bigloo__.new2143.x #unspecified) new2143))
(define-inline (readc-x::obj o::readc) (with-access::readc o (x) x))
(define-inline (readc-x-set! o::readc v::obj) (with-access::readc o (x) (set! x v)))

;; virtual-1
(define-inline (virtual-1?::bool obj::obj) ((@ is-a? __object) obj (@ virtual-1 object)))
(define-inline (make-virtual-1::virtual-1) (instantiate::virtual-1))
(define (virtual-1-nil::virtual-1) (let ((new2140::virtual-1 (let ((new2141::virtual-1 (free-pragma::virtual-1 ((BgL_virtualzd21zd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_virtualzd21zd2_bgl) )))))) (object-class-num-set! new2141 ((@ class-num __object) (@ virtual-1 object))) (object-widening-set! new2141 #f) new2141))) new2140))
(define-inline (virtual-1-x::obj o::virtual-1) (with-access::virtual-1 o (x) x))
(define-inline (virtual-1-x-set! o::virtual-1 v::obj) (with-access::virtual-1 o (x) (set! x v)))

;; virtual-2
(define-inline (virtual-2?::bool obj::obj) ((@ is-a? __object) obj (@ virtual-2 object)))
(define-inline (make-virtual-2::virtual-2 z2136::obj t2137::obj) (instantiate::virtual-2 (z z2136) (t t2137)))
(define (virtual-2-nil::virtual-2) (let ((new2138::virtual-2 (let ((new2139::virtual-2 (free-pragma::virtual-2 ((BgL_virtualzd22zd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_virtualzd22zd2_bgl) )))))) (object-class-num-set! new2139 ((@ class-num __object) (@ virtual-2 object))) (object-widening-set! new2139 #f) new2139))) (set! __bigloo__.new2138.z #unspecified) (set! __bigloo__.new2138.t #unspecified) new2138))
(define-inline (virtual-2-x::obj o::virtual-2) (with-access::virtual-2 o (x) x))
(define-inline (virtual-2-x-set! o::virtual-2 v::obj) (with-access::virtual-2 o (x) (set! x v)))
(define-inline (virtual-2-w::obj o::virtual-2) (with-access::virtual-2 o (w) w))
(define-inline (virtual-2-w-set! o::virtual-2 v::obj) (with-access::virtual-2 o (w) (set! w v)))
(define-inline (virtual-2-t::obj o::virtual-2) (with-access::virtual-2 o (t) t))
(define-inline (virtual-2-t-set! o::virtual-2 v::obj) (with-access::virtual-2 o (t) (set! t v)))
(define-inline (virtual-2-z::obj o::virtual-2) (with-access::virtual-2 o (z) z))
(define-inline (virtual-2-z-set! o::virtual-2 v::obj) (with-access::virtual-2 o (z) (set! z v)))

;; rec-1
(define-inline (rec-1?::bool obj::obj) ((@ is-a? __object) obj (@ rec-1 object)))
(define-inline (make-rec-1::rec-1 f12133::rec-2) (instantiate::rec-1 (f1 f12133)))
(define (rec-1-nil::rec-1) (let ((new2134::rec-1 (let ((new2135::rec-1 (free-pragma::rec-1 ((BgL_reczd21zd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_reczd21zd2_bgl) )))))) (object-class-num-set! new2135 ((@ class-num __object) (@ rec-1 object))) (object-widening-set! new2135 #f) new2135))) (set! __bigloo__.new2134.f1 (class-nil rec-2)) new2134))
(define-inline (rec-1-f1::rec-2 o::rec-1) (with-access::rec-1 o (f1) f1))
(define-inline (rec-1-f1-set! o::rec-1 v::rec-2) (with-access::rec-1 o (f1) (set! f1 v)))

;; rec-2
(define-inline (rec-2?::bool obj::obj) ((@ is-a? __object) obj (@ rec-2 object)))
(define-inline (make-rec-2::rec-2 f22130::rec-1) (instantiate::rec-2 (f2 f22130)))
(define (rec-2-nil::rec-2) (let ((new2131::rec-2 (let ((new2132::rec-2 (free-pragma::rec-2 ((BgL_reczd22zd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_reczd22zd2_bgl) )))))) (object-class-num-set! new2132 ((@ class-num __object) (@ rec-2 object))) (object-widening-set! new2132 #f) new2132))) (set! __bigloo__.new2131.f2 (class-nil rec-1)) new2131))
(define-inline (rec-2-f2::rec-1 o::rec-2) (with-access::rec-2 o (f2) f2))
(define-inline (rec-2-f2-set! o::rec-2 v::rec-1) (with-access::rec-2 o (f2) (set! f2 v)))

;; virtual-3
(define-inline (virtual-3?::bool obj::obj) ((@ is-a? __object) obj (@ virtual-3 object)))
(define-inline (make-virtual-3::virtual-3) (instantiate::virtual-3))
(define (virtual-3-nil::virtual-3) (let ((new2128::virtual-3 (let ((new2129::virtual-3 (free-pragma::virtual-3 ((BgL_virtualzd23zd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_virtualzd23zd2_bgl) )))))) (object-class-num-set! new2129 ((@ class-num __object) (@ virtual-3 object))) (object-widening-set! new2129 #f) new2129))) new2128))
(define-inline (virtual-3-x::obj o::virtual-3) (with-access::virtual-3 o (x) x))
(define-inline (virtual-3-x-set! o::virtual-3 v::obj) (with-access::virtual-3 o (x) (set! x v)))
(define-inline (virtual-3-a::obj o::virtual-3) (with-access::virtual-3 o (a) a))
(define-inline (virtual-3-a-set! o::virtual-3 v::obj) (with-access::virtual-3 o (a) (set! a v)))

;; virtual-4
(define-inline (virtual-4?::bool obj::obj) ((@ is-a? __object) obj (@ virtual-4 object)))
(define-inline (make-virtual-4::virtual-4) (instantiate::virtual-4))
(define (virtual-4-nil::virtual-4) (let ((new2126::virtual-4 (let ((new2127::virtual-4 (free-pragma::virtual-4 ((BgL_virtualzd24zd2_bglt)BREF( GC_MALLOC ( sizeof(struct BgL_virtualzd24zd2_bgl) )))))) (object-class-num-set! new2127 ((@ class-num __object) (@ virtual-4 object))) (object-widening-set! new2127 #f) new2127))) new2126))
(define-inline (virtual-4-z::obj o::virtual-4) (with-access::virtual-4 o (z) z))
(define-inline (virtual-4-z-set! o::virtual-4 v::obj) (with-access::virtual-4 o (z) (set! z v)))
(define-inline (virtual-4-x::obj o::virtual-4) (with-access::virtual-4 o (x) x))
(define-inline (virtual-4-x-set! o::virtual-4 v::obj) (with-access::virtual-4 o (x) (set! x v)))
(define-inline (virtual-4-a::obj o::virtual-4) (with-access::virtual-4 o (a) a))
(define-inline (virtual-4-a-set! o::virtual-4 v::obj) (with-access::virtual-4 o (a) (set! a v)))

))
