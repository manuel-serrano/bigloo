;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:08:36 CET 2011 
;; (bigloo.new -classgen SawMsil/code.scm)
;; ==========================================================

;; The directives
(directives

;; lreg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-lreg::lreg type1247::type var1248::obj onexpr?1249::obj name1250::obj key1251::obj hardware1252::obj index1253::obj param?1254::obj)
    (inline lreg?::bool ::obj)
    (lreg-nil::lreg)
    (inline lreg-param?::obj ::lreg)
    (inline lreg-param?-set! ::lreg ::obj)
    (inline lreg-index::obj ::lreg)
    (inline lreg-index-set! ::lreg ::obj)
    (inline lreg-hardware::obj ::lreg)
    (inline lreg-key::obj ::lreg)
    (inline lreg-name::obj ::lreg)
    (inline lreg-onexpr?::obj ::lreg)
    (inline lreg-onexpr?-set! ::lreg ::obj)
    (inline lreg-var::obj ::lreg)
    (inline lreg-var-set! ::lreg ::obj)
    (inline lreg-type::type ::lreg)
    (inline lreg-type-set! ::lreg ::type))))

;; tail_call
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-tail_call::tail_call loc1244::obj var1245::global)
    (inline tail_call?::bool ::obj)
    (tail_call-nil::tail_call)
    (inline tail_call-var::global ::tail_call)
    (inline tail_call-var-set! ::tail_call ::global)
    (inline tail_call-loc::obj ::tail_call)
    (inline tail_call-loc-set! ::tail_call ::obj))))

;; tail_lightfuncall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-tail_lightfuncall::tail_lightfuncall loc1239::obj name1240::symbol funs1241::pair-nil rettype1242::obj)
    (inline tail_lightfuncall?::bool ::obj)
    (tail_lightfuncall-nil::tail_lightfuncall)
    (inline tail_lightfuncall-rettype::obj ::tail_lightfuncall)
    (inline tail_lightfuncall-rettype-set! ::tail_lightfuncall ::obj)
    (inline tail_lightfuncall-funs::pair-nil ::tail_lightfuncall)
    (inline tail_lightfuncall-funs-set! ::tail_lightfuncall ::pair-nil)
    (inline tail_lightfuncall-name::symbol ::tail_lightfuncall)
    (inline tail_lightfuncall-name-set! ::tail_lightfuncall ::symbol)
    (inline tail_lightfuncall-loc::obj ::tail_lightfuncall)
    (inline tail_lightfuncall-loc-set! ::tail_lightfuncall ::obj))))

;; tail_funcall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-tail_funcall::tail_funcall loc1237::obj)
    (inline tail_funcall?::bool ::obj)
    (tail_funcall-nil::tail_funcall)
    (inline tail_funcall-loc::obj ::tail_funcall)
    (inline tail_funcall-loc-set! ::tail_funcall ::obj))))

;; tail_apply
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-tail_apply::tail_apply loc1235::obj)
    (inline tail_apply?::bool ::obj)
    (tail_apply-nil::tail_apply)
    (inline tail_apply-loc::obj ::tail_apply)
    (inline tail_apply-loc-set! ::tail_apply ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; lreg
(define-inline (make-lreg::lreg type1247::type var1248::obj onexpr?1249::obj name1250::obj key1251::obj hardware1252::obj index1253::obj param?1254::obj) (instantiate::lreg (type type1247) (var var1248) (onexpr? onexpr?1249) (name name1250) (key key1251) (hardware hardware1252) (index index1253) (param? param?1254)))
(define-inline (lreg?::bool obj::obj) ((@ isa? __object) obj (@ lreg msil_code)))
(define (lreg-nil::lreg) (class-nil (@ lreg msil_code)))
(define-inline (lreg-param?::obj o::lreg) (with-access::lreg o (param?) param?))
(define-inline (lreg-param?-set! o::lreg v::obj) (with-access::lreg o (param?) (set! param? v)))
(define-inline (lreg-index::obj o::lreg) (with-access::lreg o (index) index))
(define-inline (lreg-index-set! o::lreg v::obj) (with-access::lreg o (index) (set! index v)))
(define-inline (lreg-hardware::obj o::lreg) (with-access::lreg o (hardware) hardware))
(define-inline (lreg-hardware-set! o::lreg v::obj) (with-access::lreg o (hardware) (set! hardware v)))
(define-inline (lreg-key::obj o::lreg) (with-access::lreg o (key) key))
(define-inline (lreg-key-set! o::lreg v::obj) (with-access::lreg o (key) (set! key v)))
(define-inline (lreg-name::obj o::lreg) (with-access::lreg o (name) name))
(define-inline (lreg-name-set! o::lreg v::obj) (with-access::lreg o (name) (set! name v)))
(define-inline (lreg-onexpr?::obj o::lreg) (with-access::lreg o (onexpr?) onexpr?))
(define-inline (lreg-onexpr?-set! o::lreg v::obj) (with-access::lreg o (onexpr?) (set! onexpr? v)))
(define-inline (lreg-var::obj o::lreg) (with-access::lreg o (var) var))
(define-inline (lreg-var-set! o::lreg v::obj) (with-access::lreg o (var) (set! var v)))
(define-inline (lreg-type::type o::lreg) (with-access::lreg o (type) type))
(define-inline (lreg-type-set! o::lreg v::type) (with-access::lreg o (type) (set! type v)))

;; tail_call
(define-inline (make-tail_call::tail_call loc1244::obj var1245::global) (instantiate::tail_call (loc loc1244) (var var1245)))
(define-inline (tail_call?::bool obj::obj) ((@ isa? __object) obj (@ tail_call msil_code)))
(define (tail_call-nil::tail_call) (class-nil (@ tail_call msil_code)))
(define-inline (tail_call-var::global o::tail_call) (with-access::tail_call o (var) var))
(define-inline (tail_call-var-set! o::tail_call v::global) (with-access::tail_call o (var) (set! var v)))
(define-inline (tail_call-loc::obj o::tail_call) (with-access::tail_call o (loc) loc))
(define-inline (tail_call-loc-set! o::tail_call v::obj) (with-access::tail_call o (loc) (set! loc v)))

;; tail_lightfuncall
(define-inline (make-tail_lightfuncall::tail_lightfuncall loc1239::obj name1240::symbol funs1241::pair-nil rettype1242::obj) (instantiate::tail_lightfuncall (loc loc1239) (name name1240) (funs funs1241) (rettype rettype1242)))
(define-inline (tail_lightfuncall?::bool obj::obj) ((@ isa? __object) obj (@ tail_lightfuncall msil_code)))
(define (tail_lightfuncall-nil::tail_lightfuncall) (class-nil (@ tail_lightfuncall msil_code)))
(define-inline (tail_lightfuncall-rettype::obj o::tail_lightfuncall) (with-access::tail_lightfuncall o (rettype) rettype))
(define-inline (tail_lightfuncall-rettype-set! o::tail_lightfuncall v::obj) (with-access::tail_lightfuncall o (rettype) (set! rettype v)))
(define-inline (tail_lightfuncall-funs::pair-nil o::tail_lightfuncall) (with-access::tail_lightfuncall o (funs) funs))
(define-inline (tail_lightfuncall-funs-set! o::tail_lightfuncall v::pair-nil) (with-access::tail_lightfuncall o (funs) (set! funs v)))
(define-inline (tail_lightfuncall-name::symbol o::tail_lightfuncall) (with-access::tail_lightfuncall o (name) name))
(define-inline (tail_lightfuncall-name-set! o::tail_lightfuncall v::symbol) (with-access::tail_lightfuncall o (name) (set! name v)))
(define-inline (tail_lightfuncall-loc::obj o::tail_lightfuncall) (with-access::tail_lightfuncall o (loc) loc))
(define-inline (tail_lightfuncall-loc-set! o::tail_lightfuncall v::obj) (with-access::tail_lightfuncall o (loc) (set! loc v)))

;; tail_funcall
(define-inline (make-tail_funcall::tail_funcall loc1237::obj) (instantiate::tail_funcall (loc loc1237)))
(define-inline (tail_funcall?::bool obj::obj) ((@ isa? __object) obj (@ tail_funcall msil_code)))
(define (tail_funcall-nil::tail_funcall) (class-nil (@ tail_funcall msil_code)))
(define-inline (tail_funcall-loc::obj o::tail_funcall) (with-access::tail_funcall o (loc) loc))
(define-inline (tail_funcall-loc-set! o::tail_funcall v::obj) (with-access::tail_funcall o (loc) (set! loc v)))

;; tail_apply
(define-inline (make-tail_apply::tail_apply loc1235::obj) (instantiate::tail_apply (loc loc1235)))
(define-inline (tail_apply?::bool obj::obj) ((@ isa? __object) obj (@ tail_apply msil_code)))
(define (tail_apply-nil::tail_apply) (class-nil (@ tail_apply msil_code)))
(define-inline (tail_apply-loc::obj o::tail_apply) (with-access::tail_apply o (loc) loc))
(define-inline (tail_apply-loc-set! o::tail_apply v::obj) (with-access::tail_apply o (loc) (set! loc v)))
))
