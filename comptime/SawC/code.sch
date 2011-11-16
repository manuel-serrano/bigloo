;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 19:08:36 CET 2011 
;; (bigloo.new -classgen SawC/code.scm)
;; ==========================================================

;; The directives
(directives

;; ireg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-ireg::ireg type1211::type var1212::obj onexpr?1213::obj name1214::obj key1215::obj hardware1216::obj index1217::obj)
    (inline ireg?::bool ::obj)
    (ireg-nil::ireg)
    (inline ireg-index::obj ::ireg)
    (inline ireg-index-set! ::ireg ::obj)
    (inline ireg-hardware::obj ::ireg)
    (inline ireg-key::obj ::ireg)
    (inline ireg-name::obj ::ireg)
    (inline ireg-onexpr?::obj ::ireg)
    (inline ireg-onexpr?-set! ::ireg ::obj)
    (inline ireg-var::obj ::ireg)
    (inline ireg-var-set! ::ireg ::obj)
    (inline ireg-type::type ::ireg)
    (inline ireg-type-set! ::ireg ::type)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; ireg
(define-inline (make-ireg::ireg type1211::type var1212::obj onexpr?1213::obj name1214::obj key1215::obj hardware1216::obj index1217::obj) (instantiate::ireg (type type1211) (var var1212) (onexpr? onexpr?1213) (name name1214) (key key1215) (hardware hardware1216) (index index1217)))
(define-inline (ireg?::bool obj::obj) ((@ isa? __object) obj (@ ireg saw_c_code)))
(define (ireg-nil::ireg) (class-nil (@ ireg saw_c_code)))
(define-inline (ireg-index::obj o::ireg) (with-access::ireg o (index) index))
(define-inline (ireg-index-set! o::ireg v::obj) (with-access::ireg o (index) (set! index v)))
(define-inline (ireg-hardware::obj o::ireg) (with-access::ireg o (hardware) hardware))
(define-inline (ireg-hardware-set! o::ireg v::obj) (with-access::ireg o (hardware) (set! hardware v)))
(define-inline (ireg-key::obj o::ireg) (with-access::ireg o (key) key))
(define-inline (ireg-key-set! o::ireg v::obj) (with-access::ireg o (key) (set! key v)))
(define-inline (ireg-name::obj o::ireg) (with-access::ireg o (name) name))
(define-inline (ireg-name-set! o::ireg v::obj) (with-access::ireg o (name) (set! name v)))
(define-inline (ireg-onexpr?::obj o::ireg) (with-access::ireg o (onexpr?) onexpr?))
(define-inline (ireg-onexpr?-set! o::ireg v::obj) (with-access::ireg o (onexpr?) (set! onexpr? v)))
(define-inline (ireg-var::obj o::ireg) (with-access::ireg o (var) var))
(define-inline (ireg-var-set! o::ireg v::obj) (with-access::ireg o (var) (set! var v)))
(define-inline (ireg-type::type o::ireg) (with-access::ireg o (type) type))
(define-inline (ireg-type-set! o::ireg v::type) (with-access::ireg o (type) (set! type v)))
))
