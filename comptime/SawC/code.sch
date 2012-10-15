;; ==========================================================
;; Class accessors
;; Bigloo (3.8d)
;; Inria -- Sophia Antipolis     Mon Oct 15 08:01:14 CEST 2012 
;; (bigloo.new -classgen SawC/code.scm)
;; ==========================================================

;; The directives
(directives

;; SawCIreg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-SawCIreg::SawCIreg type1159::type var1160::obj onexpr?1161::obj name1162::obj key1163::obj hardware1164::obj index1165::obj)
    (inline SawCIreg?::bool ::obj)
    (SawCIreg-nil::SawCIreg)
    (inline SawCIreg-index::obj ::SawCIreg)
    (inline SawCIreg-index-set! ::SawCIreg ::obj)
    (inline SawCIreg-hardware::obj ::SawCIreg)
    (inline SawCIreg-key::obj ::SawCIreg)
    (inline SawCIreg-name::obj ::SawCIreg)
    (inline SawCIreg-onexpr?::obj ::SawCIreg)
    (inline SawCIreg-onexpr?-set! ::SawCIreg ::obj)
    (inline SawCIreg-var::obj ::SawCIreg)
    (inline SawCIreg-var-set! ::SawCIreg ::obj)
    (inline SawCIreg-type::type ::SawCIreg)
    (inline SawCIreg-type-set! ::SawCIreg ::type)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; SawCIreg
(define-inline (make-SawCIreg::SawCIreg type1159::type var1160::obj onexpr?1161::obj name1162::obj key1163::obj hardware1164::obj index1165::obj) (instantiate::SawCIreg (type type1159) (var var1160) (onexpr? onexpr?1161) (name name1162) (key key1163) (hardware hardware1164) (index index1165)))
(define-inline (SawCIreg?::bool obj::obj) ((@ isa? __object) obj (@ SawCIreg saw_c_code)))
(define (SawCIreg-nil::SawCIreg) (class-nil (@ SawCIreg saw_c_code)))
(define-inline (SawCIreg-index::obj o::SawCIreg) (with-access::SawCIreg o (index) index))
(define-inline (SawCIreg-index-set! o::SawCIreg v::obj) (with-access::SawCIreg o (index) (set! index v)))
(define-inline (SawCIreg-hardware::obj o::SawCIreg) (with-access::SawCIreg o (hardware) hardware))
(define-inline (SawCIreg-hardware-set! o::SawCIreg v::obj) (with-access::SawCIreg o (hardware) (set! hardware v)))
(define-inline (SawCIreg-key::obj o::SawCIreg) (with-access::SawCIreg o (key) key))
(define-inline (SawCIreg-key-set! o::SawCIreg v::obj) (with-access::SawCIreg o (key) (set! key v)))
(define-inline (SawCIreg-name::obj o::SawCIreg) (with-access::SawCIreg o (name) name))
(define-inline (SawCIreg-name-set! o::SawCIreg v::obj) (with-access::SawCIreg o (name) (set! name v)))
(define-inline (SawCIreg-onexpr?::obj o::SawCIreg) (with-access::SawCIreg o (onexpr?) onexpr?))
(define-inline (SawCIreg-onexpr?-set! o::SawCIreg v::obj) (with-access::SawCIreg o (onexpr?) (set! onexpr? v)))
(define-inline (SawCIreg-var::obj o::SawCIreg) (with-access::SawCIreg o (var) var))
(define-inline (SawCIreg-var-set! o::SawCIreg v::obj) (with-access::SawCIreg o (var) (set! var v)))
(define-inline (SawCIreg-type::type o::SawCIreg) (with-access::SawCIreg o (type) type))
(define-inline (SawCIreg-type-set! o::SawCIreg v::type) (with-access::SawCIreg o (type) (set! type v)))
))
