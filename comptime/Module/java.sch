;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Module/java.scm)
;; ==========================================================

;; The directives
(directives

;; jklass
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jklass::jklass src1110::pair loc1111::obj id1112::symbol idd1113::symbol jname1114::obj package1115::obj fields1116::pair-nil methods1117::pair-nil constructors1118::pair-nil abstract?1119::bool module1120::obj)
    (inline jklass?::bool ::obj)
    (jklass-nil::jklass)
    (inline jklass-module::obj ::jklass)
    (inline jklass-module-set! ::jklass ::obj)
    (inline jklass-abstract?::bool ::jklass)
    (inline jklass-abstract?-set! ::jklass ::bool)
    (inline jklass-constructors::pair-nil ::jklass)
    (inline jklass-constructors-set! ::jklass ::pair-nil)
    (inline jklass-methods::pair-nil ::jklass)
    (inline jklass-methods-set! ::jklass ::pair-nil)
    (inline jklass-fields::pair-nil ::jklass)
    (inline jklass-fields-set! ::jklass ::pair-nil)
    (inline jklass-package::obj ::jklass)
    (inline jklass-package-set! ::jklass ::obj)
    (inline jklass-jname::obj ::jklass)
    (inline jklass-jname-set! ::jklass ::obj)
    (inline jklass-idd::symbol ::jklass)
    (inline jklass-id::symbol ::jklass)
    (inline jklass-loc::obj ::jklass)
    (inline jklass-src::pair ::jklass))))

;; jmethod
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jmethod::jmethod src1103::pair id1104::symbol args1105::pair-nil jname1106::bstring modifiers1107::pair-nil)
    (inline jmethod?::bool ::obj)
    (jmethod-nil::jmethod)
    (inline jmethod-modifiers::pair-nil ::jmethod)
    (inline jmethod-jname::bstring ::jmethod)
    (inline jmethod-args::pair-nil ::jmethod)
    (inline jmethod-id::symbol ::jmethod)
    (inline jmethod-src::pair ::jmethod))))

;; jconstructor
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jconstructor::jconstructor src1097::pair id1098::symbol args1099::pair-nil jname1100::bstring modifiers1101::pair-nil)
    (inline jconstructor?::bool ::obj)
    (jconstructor-nil::jconstructor)
    (inline jconstructor-modifiers::pair-nil ::jconstructor)
    (inline jconstructor-jname::bstring ::jconstructor)
    (inline jconstructor-args::pair-nil ::jconstructor)
    (inline jconstructor-id::symbol ::jconstructor)
    (inline jconstructor-src::pair ::jconstructor)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; jklass
(define-inline (make-jklass::jklass src1110::pair loc1111::obj id1112::symbol idd1113::symbol jname1114::obj package1115::obj fields1116::pair-nil methods1117::pair-nil constructors1118::pair-nil abstract?1119::bool module1120::obj) (instantiate::jklass (src src1110) (loc loc1111) (id id1112) (idd idd1113) (jname jname1114) (package package1115) (fields fields1116) (methods methods1117) (constructors constructors1118) (abstract? abstract?1119) (module module1120)))
(define-inline (jklass?::bool obj::obj) ((@ isa? __object) obj (@ jklass module_java)))
(define (jklass-nil::jklass) (class-nil (@ jklass module_java)))
(define-inline (jklass-module::obj o::jklass) (-> |#!bigloo_wallow| o module))
(define-inline (jklass-module-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o module) v))
(define-inline (jklass-abstract?::bool o::jklass) (-> |#!bigloo_wallow| o abstract?))
(define-inline (jklass-abstract?-set! o::jklass v::bool) (set! (-> |#!bigloo_wallow| o abstract?) v))
(define-inline (jklass-constructors::pair-nil o::jklass) (-> |#!bigloo_wallow| o constructors))
(define-inline (jklass-constructors-set! o::jklass v::pair-nil) (set! (-> |#!bigloo_wallow| o constructors) v))
(define-inline (jklass-methods::pair-nil o::jklass) (-> |#!bigloo_wallow| o methods))
(define-inline (jklass-methods-set! o::jklass v::pair-nil) (set! (-> |#!bigloo_wallow| o methods) v))
(define-inline (jklass-fields::pair-nil o::jklass) (-> |#!bigloo_wallow| o fields))
(define-inline (jklass-fields-set! o::jklass v::pair-nil) (set! (-> |#!bigloo_wallow| o fields) v))
(define-inline (jklass-package::obj o::jklass) (-> |#!bigloo_wallow| o package))
(define-inline (jklass-package-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o package) v))
(define-inline (jklass-jname::obj o::jklass) (-> |#!bigloo_wallow| o jname))
(define-inline (jklass-jname-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o jname) v))
(define-inline (jklass-idd::symbol o::jklass) (-> |#!bigloo_wallow| o idd))
(define-inline (jklass-idd-set! o::jklass v::symbol) (set! (-> |#!bigloo_wallow| o idd) v))
(define-inline (jklass-id::symbol o::jklass) (-> |#!bigloo_wallow| o id))
(define-inline (jklass-id-set! o::jklass v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (jklass-loc::obj o::jklass) (-> |#!bigloo_wallow| o loc))
(define-inline (jklass-loc-set! o::jklass v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
(define-inline (jklass-src::pair o::jklass) (-> |#!bigloo_wallow| o src))
(define-inline (jklass-src-set! o::jklass v::pair) (set! (-> |#!bigloo_wallow| o src) v))

;; jmethod
(define-inline (make-jmethod::jmethod src1103::pair id1104::symbol args1105::pair-nil jname1106::bstring modifiers1107::pair-nil) (instantiate::jmethod (src src1103) (id id1104) (args args1105) (jname jname1106) (modifiers modifiers1107)))
(define-inline (jmethod?::bool obj::obj) ((@ isa? __object) obj (@ jmethod module_java)))
(define (jmethod-nil::jmethod) (class-nil (@ jmethod module_java)))
(define-inline (jmethod-modifiers::pair-nil o::jmethod) (-> |#!bigloo_wallow| o modifiers))
(define-inline (jmethod-modifiers-set! o::jmethod v::pair-nil) (set! (-> |#!bigloo_wallow| o modifiers) v))
(define-inline (jmethod-jname::bstring o::jmethod) (-> |#!bigloo_wallow| o jname))
(define-inline (jmethod-jname-set! o::jmethod v::bstring) (set! (-> |#!bigloo_wallow| o jname) v))
(define-inline (jmethod-args::pair-nil o::jmethod) (-> |#!bigloo_wallow| o args))
(define-inline (jmethod-args-set! o::jmethod v::pair-nil) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (jmethod-id::symbol o::jmethod) (-> |#!bigloo_wallow| o id))
(define-inline (jmethod-id-set! o::jmethod v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (jmethod-src::pair o::jmethod) (-> |#!bigloo_wallow| o src))
(define-inline (jmethod-src-set! o::jmethod v::pair) (set! (-> |#!bigloo_wallow| o src) v))

;; jconstructor
(define-inline (make-jconstructor::jconstructor src1097::pair id1098::symbol args1099::pair-nil jname1100::bstring modifiers1101::pair-nil) (instantiate::jconstructor (src src1097) (id id1098) (args args1099) (jname jname1100) (modifiers modifiers1101)))
(define-inline (jconstructor?::bool obj::obj) ((@ isa? __object) obj (@ jconstructor module_java)))
(define (jconstructor-nil::jconstructor) (class-nil (@ jconstructor module_java)))
(define-inline (jconstructor-modifiers::pair-nil o::jconstructor) (-> |#!bigloo_wallow| o modifiers))
(define-inline (jconstructor-modifiers-set! o::jconstructor v::pair-nil) (set! (-> |#!bigloo_wallow| o modifiers) v))
(define-inline (jconstructor-jname::bstring o::jconstructor) (-> |#!bigloo_wallow| o jname))
(define-inline (jconstructor-jname-set! o::jconstructor v::bstring) (set! (-> |#!bigloo_wallow| o jname) v))
(define-inline (jconstructor-args::pair-nil o::jconstructor) (-> |#!bigloo_wallow| o args))
(define-inline (jconstructor-args-set! o::jconstructor v::pair-nil) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (jconstructor-id::symbol o::jconstructor) (-> |#!bigloo_wallow| o id))
(define-inline (jconstructor-id-set! o::jconstructor v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
(define-inline (jconstructor-src::pair o::jconstructor) (-> |#!bigloo_wallow| o src))
(define-inline (jconstructor-src-set! o::jconstructor v::pair) (set! (-> |#!bigloo_wallow| o src) v))
))
