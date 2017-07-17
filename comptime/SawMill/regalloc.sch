;; ==========================================================
;; Class accessors
;; Bigloo (4.3b)
;; Inria -- Sophia Antipolis     Wed Jul 12 09:01:32 CEST 2017 
;; (bigloo -classgen SawMill/regalloc.scm)
;; ==========================================================

;; The directives
(directives

;; block/ra
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-block/ra::block/ra label1185::int preds1186::pair-nil succs1187::pair-nil first1188::pair last1189::pair-nil)
    (inline block/ra?::bool ::obj)
    (block/ra-nil::block/ra)
    (inline block/ra-last::pair-nil ::block/ra)
    (inline block/ra-last-set! ::block/ra ::pair-nil)
    (inline block/ra-first::pair ::block/ra)
    (inline block/ra-first-set! ::block/ra ::pair)
    (inline block/ra-succs::pair-nil ::block/ra)
    (inline block/ra-succs-set! ::block/ra ::pair-nil)
    (inline block/ra-preds::pair-nil ::block/ra)
    (inline block/ra-preds-set! ::block/ra ::pair-nil)
    (inline block/ra-label::int ::block/ra)
    (inline block/ra-label-set! ::block/ra ::int))))

;; rtl_ins/ra
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-rtl_ins/ra::rtl_ins/ra loc1174::obj %spill1175::pair-nil dest1176::obj fun1177::rtl_fun args1178::pair-nil def1179::obj out1180::obj in1181::obj spill1182::obj)
    (inline rtl_ins/ra?::bool ::obj)
    (rtl_ins/ra-nil::rtl_ins/ra)
    (inline rtl_ins/ra-spill::obj ::rtl_ins/ra)
    (inline rtl_ins/ra-spill-set! ::rtl_ins/ra ::obj)
    (inline rtl_ins/ra-in::obj ::rtl_ins/ra)
    (inline rtl_ins/ra-in-set! ::rtl_ins/ra ::obj)
    (inline rtl_ins/ra-out::obj ::rtl_ins/ra)
    (inline rtl_ins/ra-out-set! ::rtl_ins/ra ::obj)
    (inline rtl_ins/ra-def::obj ::rtl_ins/ra)
    (inline rtl_ins/ra-def-set! ::rtl_ins/ra ::obj)
    (inline rtl_ins/ra-args::pair-nil ::rtl_ins/ra)
    (inline rtl_ins/ra-args-set! ::rtl_ins/ra ::pair-nil)
    (inline rtl_ins/ra-fun::rtl_fun ::rtl_ins/ra)
    (inline rtl_ins/ra-fun-set! ::rtl_ins/ra ::rtl_fun)
    (inline rtl_ins/ra-dest::obj ::rtl_ins/ra)
    (inline rtl_ins/ra-dest-set! ::rtl_ins/ra ::obj)
    (inline rtl_ins/ra-%spill::pair-nil ::rtl_ins/ra)
    (inline rtl_ins/ra-%spill-set! ::rtl_ins/ra ::pair-nil)
    (inline rtl_ins/ra-loc::obj ::rtl_ins/ra)
    (inline rtl_ins/ra-loc-set! ::rtl_ins/ra ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; block/ra
(define-inline (make-block/ra::block/ra label1185::int preds1186::pair-nil succs1187::pair-nil first1188::pair last1189::pair-nil) (instantiate::block/ra (label label1185) (preds preds1186) (succs succs1187) (first first1188) (last last1189)))
(define-inline (block/ra?::bool obj::obj) ((@ isa? __object) obj (@ block/ra saw_register-allocation)))
(define (block/ra-nil::block/ra) (class-nil (@ block/ra saw_register-allocation)))
(define-inline (block/ra-last::pair-nil o::block/ra) (-> |#!bigloo_wallow| o last))
(define-inline (block/ra-last-set! o::block/ra v::pair-nil) (set! (-> |#!bigloo_wallow| o last) v))
(define-inline (block/ra-first::pair o::block/ra) (-> |#!bigloo_wallow| o first))
(define-inline (block/ra-first-set! o::block/ra v::pair) (set! (-> |#!bigloo_wallow| o first) v))
(define-inline (block/ra-succs::pair-nil o::block/ra) (-> |#!bigloo_wallow| o succs))
(define-inline (block/ra-succs-set! o::block/ra v::pair-nil) (set! (-> |#!bigloo_wallow| o succs) v))
(define-inline (block/ra-preds::pair-nil o::block/ra) (-> |#!bigloo_wallow| o preds))
(define-inline (block/ra-preds-set! o::block/ra v::pair-nil) (set! (-> |#!bigloo_wallow| o preds) v))
(define-inline (block/ra-label::int o::block/ra) (-> |#!bigloo_wallow| o label))
(define-inline (block/ra-label-set! o::block/ra v::int) (set! (-> |#!bigloo_wallow| o label) v))

;; rtl_ins/ra
(define-inline (make-rtl_ins/ra::rtl_ins/ra loc1174::obj %spill1175::pair-nil dest1176::obj fun1177::rtl_fun args1178::pair-nil def1179::obj out1180::obj in1181::obj spill1182::obj) (instantiate::rtl_ins/ra (loc loc1174) (%spill %spill1175) (dest dest1176) (fun fun1177) (args args1178) (def def1179) (out out1180) (in in1181) (spill spill1182)))
(define-inline (rtl_ins/ra?::bool obj::obj) ((@ isa? __object) obj (@ rtl_ins/ra saw_register-allocation)))
(define (rtl_ins/ra-nil::rtl_ins/ra) (class-nil (@ rtl_ins/ra saw_register-allocation)))
(define-inline (rtl_ins/ra-spill::obj o::rtl_ins/ra) (-> |#!bigloo_wallow| o spill))
(define-inline (rtl_ins/ra-spill-set! o::rtl_ins/ra v::obj) (set! (-> |#!bigloo_wallow| o spill) v))
(define-inline (rtl_ins/ra-in::obj o::rtl_ins/ra) (-> |#!bigloo_wallow| o in))
(define-inline (rtl_ins/ra-in-set! o::rtl_ins/ra v::obj) (set! (-> |#!bigloo_wallow| o in) v))
(define-inline (rtl_ins/ra-out::obj o::rtl_ins/ra) (-> |#!bigloo_wallow| o out))
(define-inline (rtl_ins/ra-out-set! o::rtl_ins/ra v::obj) (set! (-> |#!bigloo_wallow| o out) v))
(define-inline (rtl_ins/ra-def::obj o::rtl_ins/ra) (-> |#!bigloo_wallow| o def))
(define-inline (rtl_ins/ra-def-set! o::rtl_ins/ra v::obj) (set! (-> |#!bigloo_wallow| o def) v))
(define-inline (rtl_ins/ra-args::pair-nil o::rtl_ins/ra) (-> |#!bigloo_wallow| o args))
(define-inline (rtl_ins/ra-args-set! o::rtl_ins/ra v::pair-nil) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (rtl_ins/ra-fun::rtl_fun o::rtl_ins/ra) (-> |#!bigloo_wallow| o fun))
(define-inline (rtl_ins/ra-fun-set! o::rtl_ins/ra v::rtl_fun) (set! (-> |#!bigloo_wallow| o fun) v))
(define-inline (rtl_ins/ra-dest::obj o::rtl_ins/ra) (-> |#!bigloo_wallow| o dest))
(define-inline (rtl_ins/ra-dest-set! o::rtl_ins/ra v::obj) (set! (-> |#!bigloo_wallow| o dest) v))
(define-inline (rtl_ins/ra-%spill::pair-nil o::rtl_ins/ra) (-> |#!bigloo_wallow| o %spill))
(define-inline (rtl_ins/ra-%spill-set! o::rtl_ins/ra v::pair-nil) (set! (-> |#!bigloo_wallow| o %spill) v))
(define-inline (rtl_ins/ra-loc::obj o::rtl_ins/ra) (-> |#!bigloo_wallow| o loc))
(define-inline (rtl_ins/ra-loc-set! o::rtl_ins/ra v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
))
