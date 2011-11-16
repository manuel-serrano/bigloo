;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 16:42:36 CET 2011 
;; (bigloo.new -classgen Module/java.scm)
;; ==========================================================

;; The directives
(directives

;; jklass
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (static
    (inline make-jklass::jklass src1159::pair loc1160::obj id1161::symbol idd1162::symbol jname1163::obj package1164::obj fields1165::pair-nil methods1166::pair-nil constructors1167::pair-nil abstract?1168::bool module1169::obj)
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
    (inline make-jmethod::jmethod src1153::pair id1154::symbol args1155::pair-nil jname1156::bstring modifiers1157::pair-nil)
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
    (inline make-jconstructor::jconstructor src1147::pair id1148::symbol args1149::pair-nil jname1150::bstring modifiers1151::pair-nil)
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
(define-inline (make-jklass::jklass src1159::pair loc1160::obj id1161::symbol idd1162::symbol jname1163::obj package1164::obj fields1165::pair-nil methods1166::pair-nil constructors1167::pair-nil abstract?1168::bool module1169::obj) (instantiate::jklass (src src1159) (loc loc1160) (id id1161) (idd idd1162) (jname jname1163) (package package1164) (fields fields1165) (methods methods1166) (constructors constructors1167) (abstract? abstract?1168) (module module1169)))
(define-inline (jklass?::bool obj::obj) ((@ isa? __object) obj (@ jklass module_java)))
(define (jklass-nil::jklass) (class-nil (@ jklass module_java)))
(define-inline (jklass-module::obj o::jklass) (with-access::jklass o (module) module))
(define-inline (jklass-module-set! o::jklass v::obj) (with-access::jklass o (module) (set! module v)))
(define-inline (jklass-abstract?::bool o::jklass) (with-access::jklass o (abstract?) abstract?))
(define-inline (jklass-abstract?-set! o::jklass v::bool) (with-access::jklass o (abstract?) (set! abstract? v)))
(define-inline (jklass-constructors::pair-nil o::jklass) (with-access::jklass o (constructors) constructors))
(define-inline (jklass-constructors-set! o::jklass v::pair-nil) (with-access::jklass o (constructors) (set! constructors v)))
(define-inline (jklass-methods::pair-nil o::jklass) (with-access::jklass o (methods) methods))
(define-inline (jklass-methods-set! o::jklass v::pair-nil) (with-access::jklass o (methods) (set! methods v)))
(define-inline (jklass-fields::pair-nil o::jklass) (with-access::jklass o (fields) fields))
(define-inline (jklass-fields-set! o::jklass v::pair-nil) (with-access::jklass o (fields) (set! fields v)))
(define-inline (jklass-package::obj o::jklass) (with-access::jklass o (package) package))
(define-inline (jklass-package-set! o::jklass v::obj) (with-access::jklass o (package) (set! package v)))
(define-inline (jklass-jname::obj o::jklass) (with-access::jklass o (jname) jname))
(define-inline (jklass-jname-set! o::jklass v::obj) (with-access::jklass o (jname) (set! jname v)))
(define-inline (jklass-idd::symbol o::jklass) (with-access::jklass o (idd) idd))
(define-inline (jklass-idd-set! o::jklass v::symbol) (with-access::jklass o (idd) (set! idd v)))
(define-inline (jklass-id::symbol o::jklass) (with-access::jklass o (id) id))
(define-inline (jklass-id-set! o::jklass v::symbol) (with-access::jklass o (id) (set! id v)))
(define-inline (jklass-loc::obj o::jklass) (with-access::jklass o (loc) loc))
(define-inline (jklass-loc-set! o::jklass v::obj) (with-access::jklass o (loc) (set! loc v)))
(define-inline (jklass-src::pair o::jklass) (with-access::jklass o (src) src))
(define-inline (jklass-src-set! o::jklass v::pair) (with-access::jklass o (src) (set! src v)))

;; jmethod
(define-inline (make-jmethod::jmethod src1153::pair id1154::symbol args1155::pair-nil jname1156::bstring modifiers1157::pair-nil) (instantiate::jmethod (src src1153) (id id1154) (args args1155) (jname jname1156) (modifiers modifiers1157)))
(define-inline (jmethod?::bool obj::obj) ((@ isa? __object) obj (@ jmethod module_java)))
(define (jmethod-nil::jmethod) (class-nil (@ jmethod module_java)))
(define-inline (jmethod-modifiers::pair-nil o::jmethod) (with-access::jmethod o (modifiers) modifiers))
(define-inline (jmethod-modifiers-set! o::jmethod v::pair-nil) (with-access::jmethod o (modifiers) (set! modifiers v)))
(define-inline (jmethod-jname::bstring o::jmethod) (with-access::jmethod o (jname) jname))
(define-inline (jmethod-jname-set! o::jmethod v::bstring) (with-access::jmethod o (jname) (set! jname v)))
(define-inline (jmethod-args::pair-nil o::jmethod) (with-access::jmethod o (args) args))
(define-inline (jmethod-args-set! o::jmethod v::pair-nil) (with-access::jmethod o (args) (set! args v)))
(define-inline (jmethod-id::symbol o::jmethod) (with-access::jmethod o (id) id))
(define-inline (jmethod-id-set! o::jmethod v::symbol) (with-access::jmethod o (id) (set! id v)))
(define-inline (jmethod-src::pair o::jmethod) (with-access::jmethod o (src) src))
(define-inline (jmethod-src-set! o::jmethod v::pair) (with-access::jmethod o (src) (set! src v)))

;; jconstructor
(define-inline (make-jconstructor::jconstructor src1147::pair id1148::symbol args1149::pair-nil jname1150::bstring modifiers1151::pair-nil) (instantiate::jconstructor (src src1147) (id id1148) (args args1149) (jname jname1150) (modifiers modifiers1151)))
(define-inline (jconstructor?::bool obj::obj) ((@ isa? __object) obj (@ jconstructor module_java)))
(define (jconstructor-nil::jconstructor) (class-nil (@ jconstructor module_java)))
(define-inline (jconstructor-modifiers::pair-nil o::jconstructor) (with-access::jconstructor o (modifiers) modifiers))
(define-inline (jconstructor-modifiers-set! o::jconstructor v::pair-nil) (with-access::jconstructor o (modifiers) (set! modifiers v)))
(define-inline (jconstructor-jname::bstring o::jconstructor) (with-access::jconstructor o (jname) jname))
(define-inline (jconstructor-jname-set! o::jconstructor v::bstring) (with-access::jconstructor o (jname) (set! jname v)))
(define-inline (jconstructor-args::pair-nil o::jconstructor) (with-access::jconstructor o (args) args))
(define-inline (jconstructor-args-set! o::jconstructor v::pair-nil) (with-access::jconstructor o (args) (set! args v)))
(define-inline (jconstructor-id::symbol o::jconstructor) (with-access::jconstructor o (id) id))
(define-inline (jconstructor-id-set! o::jconstructor v::symbol) (with-access::jconstructor o (id) (set! id v)))
(define-inline (jconstructor-src::pair o::jconstructor) (with-access::jconstructor o (src) src))
(define-inline (jconstructor-src-set! o::jconstructor v::pair) (with-access::jconstructor o (src) (set! src v)))
))
