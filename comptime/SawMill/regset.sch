;; ==========================================================
;; Class accessors
;; Bigloo (4.3b)
;; Inria -- Sophia Antipolis     Wed Jul 12 09:01:32 CEST 2017 
;; (bigloo -classgen SawMill/regset.scm)
;; ==========================================================

;; The directives
(directives

;; rtl_reg/ra
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_reg/ra::rtl_reg/ra type1179::type var1180::obj onexpr?1181::obj name1182::obj key1183::obj hardware1184::obj num1185::int color1186::obj coalesce1187::obj occurrences1188::int interfere1189::obj interfere21190::obj)
    (inline rtl_reg/ra?::bool ::obj)
    (rtl_reg/ra-nil::rtl_reg/ra)
    (inline rtl_reg/ra-interfere2::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-interfere2-set! ::rtl_reg/ra ::obj)
    (inline rtl_reg/ra-interfere::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-interfere-set! ::rtl_reg/ra ::obj)
    (inline rtl_reg/ra-occurrences::int ::rtl_reg/ra)
    (inline rtl_reg/ra-occurrences-set! ::rtl_reg/ra ::int)
    (inline rtl_reg/ra-coalesce::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-coalesce-set! ::rtl_reg/ra ::obj)
    (inline rtl_reg/ra-color::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-color-set! ::rtl_reg/ra ::obj)
    (inline rtl_reg/ra-num::int ::rtl_reg/ra)
    (inline rtl_reg/ra-hardware::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-key::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-name::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-onexpr?::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-onexpr?-set! ::rtl_reg/ra ::obj)
    (inline rtl_reg/ra-var::obj ::rtl_reg/ra)
    (inline rtl_reg/ra-var-set! ::rtl_reg/ra ::obj)
    (inline rtl_reg/ra-type::type ::rtl_reg/ra)
    (inline rtl_reg/ra-type-set! ::rtl_reg/ra ::type))))

;; regset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-regset::regset length1172::int msize1173::int regv1174::vector regl1175::pair-nil string1176::bstring)
    (inline regset?::bool ::obj)
    (regset-nil::regset)
    (inline regset-string::bstring ::regset)
    (inline regset-string-set! ::regset ::bstring)
    (inline regset-regl::pair-nil ::regset)
    (inline regset-regv::vector ::regset)
    (inline regset-msize::int ::regset)
    (inline regset-length::int ::regset)
    (inline regset-length-set! ::regset ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; rtl_reg/ra
(define-inline (make-rtl_reg/ra::rtl_reg/ra type1179::type var1180::obj onexpr?1181::obj name1182::obj key1183::obj hardware1184::obj num1185::int color1186::obj coalesce1187::obj occurrences1188::int interfere1189::obj interfere21190::obj) (instantiate::rtl_reg/ra (type type1179) (var var1180) (onexpr? onexpr?1181) (name name1182) (key key1183) (hardware hardware1184) (num num1185) (color color1186) (coalesce coalesce1187) (occurrences occurrences1188) (interfere interfere1189) (interfere2 interfere21190)))
(define-inline (rtl_reg/ra?::bool obj::obj) ((@ isa? __object) obj (@ rtl_reg/ra saw_regset)))
(define (rtl_reg/ra-nil::rtl_reg/ra) (class-nil (@ rtl_reg/ra saw_regset)))
(define-inline (rtl_reg/ra-interfere2::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o interfere2))
(define-inline (rtl_reg/ra-interfere2-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o interfere2) v))
(define-inline (rtl_reg/ra-interfere::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o interfere))
(define-inline (rtl_reg/ra-interfere-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o interfere) v))
(define-inline (rtl_reg/ra-occurrences::int o::rtl_reg/ra) (-> |#!bigloo_wallow| o occurrences))
(define-inline (rtl_reg/ra-occurrences-set! o::rtl_reg/ra v::int) (set! (-> |#!bigloo_wallow| o occurrences) v))
(define-inline (rtl_reg/ra-coalesce::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o coalesce))
(define-inline (rtl_reg/ra-coalesce-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o coalesce) v))
(define-inline (rtl_reg/ra-color::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o color))
(define-inline (rtl_reg/ra-color-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o color) v))
(define-inline (rtl_reg/ra-num::int o::rtl_reg/ra) (-> |#!bigloo_wallow| o num))
(define-inline (rtl_reg/ra-num-set! o::rtl_reg/ra v::int) (set! (-> |#!bigloo_wallow| o num) v))
(define-inline (rtl_reg/ra-hardware::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o hardware))
(define-inline (rtl_reg/ra-hardware-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o hardware) v))
(define-inline (rtl_reg/ra-key::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o key))
(define-inline (rtl_reg/ra-key-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o key) v))
(define-inline (rtl_reg/ra-name::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o name))
(define-inline (rtl_reg/ra-name-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (rtl_reg/ra-onexpr?::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o onexpr?))
(define-inline (rtl_reg/ra-onexpr?-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o onexpr?) v))
(define-inline (rtl_reg/ra-var::obj o::rtl_reg/ra) (-> |#!bigloo_wallow| o var))
(define-inline (rtl_reg/ra-var-set! o::rtl_reg/ra v::obj) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (rtl_reg/ra-type::type o::rtl_reg/ra) (-> |#!bigloo_wallow| o type))
(define-inline (rtl_reg/ra-type-set! o::rtl_reg/ra v::type) (set! (-> |#!bigloo_wallow| o type) v))

;; regset
(define-inline (make-regset::regset length1172::int msize1173::int regv1174::vector regl1175::pair-nil string1176::bstring) (instantiate::regset (length length1172) (msize msize1173) (regv regv1174) (regl regl1175) (string string1176)))
(define-inline (regset?::bool obj::obj) ((@ isa? __object) obj (@ regset saw_regset)))
(define (regset-nil::regset) (class-nil (@ regset saw_regset)))
(define-inline (regset-string::bstring o::regset) (-> |#!bigloo_wallow| o string))
(define-inline (regset-string-set! o::regset v::bstring) (set! (-> |#!bigloo_wallow| o string) v))
(define-inline (regset-regl::pair-nil o::regset) (-> |#!bigloo_wallow| o regl))
(define-inline (regset-regl-set! o::regset v::pair-nil) (set! (-> |#!bigloo_wallow| o regl) v))
(define-inline (regset-regv::vector o::regset) (-> |#!bigloo_wallow| o regv))
(define-inline (regset-regv-set! o::regset v::vector) (set! (-> |#!bigloo_wallow| o regv) v))
(define-inline (regset-msize::int o::regset) (-> |#!bigloo_wallow| o msize))
(define-inline (regset-msize-set! o::regset v::int) (set! (-> |#!bigloo_wallow| o msize) v))
(define-inline (regset-length::int o::regset) (-> |#!bigloo_wallow| o length))
(define-inline (regset-length-set! o::regset v::int) (set! (-> |#!bigloo_wallow| o length) v))
))
