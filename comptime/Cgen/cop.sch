;; ==========================================================
;; Class accessors
;; Bigloo (4.2c)
;; Inria -- Sophia Antipolis     Fri Nov 6 10:55:25 CET 2015 
;; (bigloo.new -classgen Cgen/cop.scm)
;; ==========================================================

;; The directives
(directives

;; cop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cop::cop loc1465::obj)
    (inline cop?::bool ::obj)
    (cop-nil::cop)
    (inline cop-loc::obj ::cop)
    (inline cop-loc-set! ::cop ::obj))))

;; clabel
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-clabel::clabel loc1460::obj name1461::bstring used?1462::bool body1463::obj)
    (inline clabel?::bool ::obj)
    (clabel-nil::clabel)
    (inline clabel-body::obj ::clabel)
    (inline clabel-body-set! ::clabel ::obj)
    (inline clabel-used?::bool ::clabel)
    (inline clabel-used?-set! ::clabel ::bool)
    (inline clabel-name::bstring ::clabel)
    (inline clabel-loc::obj ::clabel)
    (inline clabel-loc-set! ::clabel ::obj))))

;; cgoto
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cgoto::cgoto loc1457::obj label1458::clabel)
    (inline cgoto?::bool ::obj)
    (cgoto-nil::cgoto)
    (inline cgoto-label::clabel ::cgoto)
    (inline cgoto-loc::obj ::cgoto)
    (inline cgoto-loc-set! ::cgoto ::obj))))

;; cblock
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cblock::cblock loc1454::obj body1455::cop)
    (inline cblock?::bool ::obj)
    (cblock-nil::cblock)
    (inline cblock-body::cop ::cblock)
    (inline cblock-loc::obj ::cblock)
    (inline cblock-loc-set! ::cblock ::obj))))

;; creturn
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-creturn::creturn loc1451::obj value1452::cop)
    (inline creturn?::bool ::obj)
    (creturn-nil::creturn)
    (inline creturn-value::cop ::creturn)
    (inline creturn-loc::obj ::creturn)
    (inline creturn-loc-set! ::creturn ::obj))))

;; cvoid
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cvoid::cvoid loc1448::obj value1449::cop)
    (inline cvoid?::bool ::obj)
    (cvoid-nil::cvoid)
    (inline cvoid-value::cop ::cvoid)
    (inline cvoid-loc::obj ::cvoid)
    (inline cvoid-loc-set! ::cvoid ::obj))))

;; catom
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-catom::catom loc1444::obj value1445::obj)
    (inline catom?::bool ::obj)
    (catom-nil::catom)
    (inline catom-value::obj ::catom)
    (inline catom-loc::obj ::catom)
    (inline catom-loc-set! ::catom ::obj))))

;; varc
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-varc::varc loc1441::obj variable1442::variable)
    (inline varc?::bool ::obj)
    (varc-nil::varc)
    (inline varc-variable::variable ::varc)
    (inline varc-loc::obj ::varc)
    (inline varc-loc-set! ::varc ::obj))))

;; cpragma
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cpragma::cpragma loc1437::obj format1438::bstring args1439::obj)
    (inline cpragma?::bool ::obj)
    (cpragma-nil::cpragma)
    (inline cpragma-args::obj ::cpragma)
    (inline cpragma-format::bstring ::cpragma)
    (inline cpragma-loc::obj ::cpragma)
    (inline cpragma-loc-set! ::cpragma ::obj))))

;; ccast
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ccast::ccast loc1383::obj type1384::type arg1435::cop)
    (inline ccast?::bool ::obj)
    (ccast-nil::ccast)
    (inline ccast-arg::cop ::ccast)
    (inline ccast-type::type ::ccast)
    (inline ccast-loc::obj ::ccast)
    (inline ccast-loc-set! ::ccast ::obj))))

;; csequence
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-csequence::csequence loc1379::obj c-exp?1380::bool cops1381::obj)
    (inline csequence?::bool ::obj)
    (csequence-nil::csequence)
    (inline csequence-cops::obj ::csequence)
    (inline csequence-c-exp?::bool ::csequence)
    (inline csequence-loc::obj ::csequence)
    (inline csequence-loc-set! ::csequence ::obj))))

;; nop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-nop::nop loc1377::obj)
    (inline nop?::bool ::obj)
    (nop-nil::nop)
    (inline nop-loc::obj ::nop)
    (inline nop-loc-set! ::nop ::obj))))

;; stop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-stop::stop loc1374::obj value1375::cop)
    (inline stop?::bool ::obj)
    (stop-nil::stop)
    (inline stop-value::cop ::stop)
    (inline stop-loc::obj ::stop)
    (inline stop-loc-set! ::stop ::obj))))

;; csetq
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-csetq::csetq loc1370::obj var1371::varc value1372::cop)
    (inline csetq?::bool ::obj)
    (csetq-nil::csetq)
    (inline csetq-value::cop ::csetq)
    (inline csetq-var::varc ::csetq)
    (inline csetq-loc::obj ::csetq)
    (inline csetq-loc-set! ::csetq ::obj))))

;; cif
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cif::cif loc1365::obj test1366::cop true1367::cop false1368::cop)
    (inline cif?::bool ::obj)
    (cif-nil::cif)
    (inline cif-false::cop ::cif)
    (inline cif-true::cop ::cif)
    (inline cif-test::cop ::cif)
    (inline cif-loc::obj ::cif)
    (inline cif-loc-set! ::cif ::obj))))

;; local-var
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-local-var::local-var loc1362::obj vars1363::obj)
    (inline local-var?::bool ::obj)
    (local-var-nil::local-var)
    (inline local-var-vars::obj ::local-var)
    (inline local-var-loc::obj ::local-var)
    (inline local-var-loc-set! ::local-var ::obj))))

;; cfuncall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cfuncall::cfuncall loc1356::obj fun1357::cop args1358::obj strength1359::symbol type1360::obj)
    (inline cfuncall?::bool ::obj)
    (cfuncall-nil::cfuncall)
    (inline cfuncall-type::obj ::cfuncall)
    (inline cfuncall-strength::symbol ::cfuncall)
    (inline cfuncall-args::obj ::cfuncall)
    (inline cfuncall-fun::cop ::cfuncall)
    (inline cfuncall-loc::obj ::cfuncall)
    (inline cfuncall-loc-set! ::cfuncall ::obj))))

;; capply
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-capply::capply loc1343::obj fun1344::cop arg1354::cop)
    (inline capply?::bool ::obj)
    (capply-nil::capply)
    (inline capply-arg::cop ::capply)
    (inline capply-fun::cop ::capply)
    (inline capply-loc::obj ::capply)
    (inline capply-loc-set! ::capply ::obj))))

;; capp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-capp::capp loc1339::obj fun1340::cop args1341::obj)
    (inline capp?::bool ::obj)
    (capp-nil::capp)
    (inline capp-args::obj ::capp)
    (inline capp-fun::cop ::capp)
    (inline capp-loc::obj ::capp)
    (inline capp-loc-set! ::capp ::obj))))

;; cfail
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cfail::cfail loc1334::obj proc1335::cop msg1336::cop obj1337::cop)
    (inline cfail?::bool ::obj)
    (cfail-nil::cfail)
    (inline cfail-obj::cop ::cfail)
    (inline cfail-msg::cop ::cfail)
    (inline cfail-proc::cop ::cfail)
    (inline cfail-loc::obj ::cfail)
    (inline cfail-loc-set! ::cfail ::obj))))

;; cswitch
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cswitch::cswitch loc1330::obj test1331::cop clauses1332::obj)
    (inline cswitch?::bool ::obj)
    (cswitch-nil::cswitch)
    (inline cswitch-clauses::obj ::cswitch)
    (inline cswitch-test::cop ::cswitch)
    (inline cswitch-loc::obj ::cswitch)
    (inline cswitch-loc-set! ::cswitch ::obj))))

;; cmake-box
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cmake-box::cmake-box loc1327::obj value1328::cop)
    (inline cmake-box?::bool ::obj)
    (cmake-box-nil::cmake-box)
    (inline cmake-box-value::cop ::cmake-box)
    (inline cmake-box-loc::obj ::cmake-box)
    (inline cmake-box-loc-set! ::cmake-box ::obj))))

;; cbox-ref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cbox-ref::cbox-ref loc1324::obj var1325::cop)
    (inline cbox-ref?::bool ::obj)
    (cbox-ref-nil::cbox-ref)
    (inline cbox-ref-var::cop ::cbox-ref)
    (inline cbox-ref-loc::obj ::cbox-ref)
    (inline cbox-ref-loc-set! ::cbox-ref ::obj))))

;; cbox-set!
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cbox-set!::cbox-set! loc1320::obj var1321::cop value1322::cop)
    (inline cbox-set!?::bool ::obj)
    (cbox-set!-nil::cbox-set!)
    (inline cbox-set!-value::cop ::cbox-set!)
    (inline cbox-set!-var::cop ::cbox-set!)
    (inline cbox-set!-loc::obj ::cbox-set!)
    (inline cbox-set!-loc-set! ::cbox-set! ::obj))))

;; cset-ex-it
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cset-ex-it::cset-ex-it loc1315::obj exit1316::cop jump-value1317::cop body1318::cop)
    (inline cset-ex-it?::bool ::obj)
    (cset-ex-it-nil::cset-ex-it)
    (inline cset-ex-it-body::cop ::cset-ex-it)
    (inline cset-ex-it-jump-value::cop ::cset-ex-it)
    (inline cset-ex-it-exit::cop ::cset-ex-it)
    (inline cset-ex-it-loc::obj ::cset-ex-it)
    (inline cset-ex-it-loc-set! ::cset-ex-it ::obj))))

;; cjump-ex-it
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cjump-ex-it::cjump-ex-it loc1311::obj exit1312::cop value1313::cop)
    (inline cjump-ex-it?::bool ::obj)
    (cjump-ex-it-nil::cjump-ex-it)
    (inline cjump-ex-it-value::cop ::cjump-ex-it)
    (inline cjump-ex-it-exit::cop ::cjump-ex-it)
    (inline cjump-ex-it-loc::obj ::cjump-ex-it)
    (inline cjump-ex-it-loc-set! ::cjump-ex-it ::obj))))

;; sfun/C
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-sfun/C::sfun/C arity1289::long side-effect1290::obj predicate-of1291::obj stack-allocator1292::obj top?1293::bool the-closure1294::obj effect1295::obj failsafe1296::obj property1297::obj args1298::obj args-name1299::obj body1300::obj class1301::obj dsssl-keywords1302::obj loc1303::obj optionals1304::obj keys1305::obj the-closure-global1306::obj strength1307::symbol label1308::clabel integrated1309::bool)
    (inline sfun/C?::bool ::obj)
    (sfun/C-nil::sfun/C)
    (inline sfun/C-integrated::bool ::sfun/C)
    (inline sfun/C-integrated-set! ::sfun/C ::bool)
    (inline sfun/C-label::clabel ::sfun/C)
    (inline sfun/C-strength::symbol ::sfun/C)
    (inline sfun/C-strength-set! ::sfun/C ::symbol)
    (inline sfun/C-the-closure-global::obj ::sfun/C)
    (inline sfun/C-the-closure-global-set! ::sfun/C ::obj)
    (inline sfun/C-keys::obj ::sfun/C)
    (inline sfun/C-optionals::obj ::sfun/C)
    (inline sfun/C-loc::obj ::sfun/C)
    (inline sfun/C-loc-set! ::sfun/C ::obj)
    (inline sfun/C-dsssl-keywords::obj ::sfun/C)
    (inline sfun/C-dsssl-keywords-set! ::sfun/C ::obj)
    (inline sfun/C-class::obj ::sfun/C)
    (inline sfun/C-class-set! ::sfun/C ::obj)
    (inline sfun/C-body::obj ::sfun/C)
    (inline sfun/C-body-set! ::sfun/C ::obj)
    (inline sfun/C-args-name::obj ::sfun/C)
    (inline sfun/C-args::obj ::sfun/C)
    (inline sfun/C-args-set! ::sfun/C ::obj)
    (inline sfun/C-property::obj ::sfun/C)
    (inline sfun/C-property-set! ::sfun/C ::obj)
    (inline sfun/C-failsafe::obj ::sfun/C)
    (inline sfun/C-failsafe-set! ::sfun/C ::obj)
    (inline sfun/C-effect::obj ::sfun/C)
    (inline sfun/C-effect-set! ::sfun/C ::obj)
    (inline sfun/C-the-closure::obj ::sfun/C)
    (inline sfun/C-the-closure-set! ::sfun/C ::obj)
    (inline sfun/C-top?::bool ::sfun/C)
    (inline sfun/C-top?-set! ::sfun/C ::bool)
    (inline sfun/C-stack-allocator::obj ::sfun/C)
    (inline sfun/C-stack-allocator-set! ::sfun/C ::obj)
    (inline sfun/C-predicate-of::obj ::sfun/C)
    (inline sfun/C-predicate-of-set! ::sfun/C ::obj)
    (inline sfun/C-side-effect::obj ::sfun/C)
    (inline sfun/C-side-effect-set! ::sfun/C ::obj)
    (inline sfun/C-arity::long ::sfun/C))))

;; bdb-block
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-bdb-block::bdb-block loc1286::obj body1287::cop)
    (inline bdb-block?::bool ::obj)
    (bdb-block-nil::bdb-block)
    (inline bdb-block-body::cop ::bdb-block)
    (inline bdb-block-loc::obj ::bdb-block)
    (inline bdb-block-loc-set! ::bdb-block ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; cop
(define-inline (make-cop::cop loc1465::obj) (instantiate::cop (loc loc1465)))
(define-inline (cop?::bool obj::obj) ((@ isa? __object) obj (@ cop cgen_cop)))
(define (cop-nil::cop) (class-nil (@ cop cgen_cop)))
(define-inline (cop-loc::obj o::cop) (-> |#!bigloo_wallow| o loc))
(define-inline (cop-loc-set! o::cop v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; clabel
(define-inline (make-clabel::clabel loc1460::obj name1461::bstring used?1462::bool body1463::obj) (instantiate::clabel (loc loc1460) (name name1461) (used? used?1462) (body body1463)))
(define-inline (clabel?::bool obj::obj) ((@ isa? __object) obj (@ clabel cgen_cop)))
(define (clabel-nil::clabel) (class-nil (@ clabel cgen_cop)))
(define-inline (clabel-body::obj o::clabel) (-> |#!bigloo_wallow| o body))
(define-inline (clabel-body-set! o::clabel v::obj) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (clabel-used?::bool o::clabel) (-> |#!bigloo_wallow| o used?))
(define-inline (clabel-used?-set! o::clabel v::bool) (set! (-> |#!bigloo_wallow| o used?) v))
(define-inline (clabel-name::bstring o::clabel) (-> |#!bigloo_wallow| o name))
(define-inline (clabel-name-set! o::clabel v::bstring) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (clabel-loc::obj o::clabel) (-> |#!bigloo_wallow| o loc))
(define-inline (clabel-loc-set! o::clabel v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cgoto
(define-inline (make-cgoto::cgoto loc1457::obj label1458::clabel) (instantiate::cgoto (loc loc1457) (label label1458)))
(define-inline (cgoto?::bool obj::obj) ((@ isa? __object) obj (@ cgoto cgen_cop)))
(define (cgoto-nil::cgoto) (class-nil (@ cgoto cgen_cop)))
(define-inline (cgoto-label::clabel o::cgoto) (-> |#!bigloo_wallow| o label))
(define-inline (cgoto-label-set! o::cgoto v::clabel) (set! (-> |#!bigloo_wallow| o label) v))
(define-inline (cgoto-loc::obj o::cgoto) (-> |#!bigloo_wallow| o loc))
(define-inline (cgoto-loc-set! o::cgoto v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cblock
(define-inline (make-cblock::cblock loc1454::obj body1455::cop) (instantiate::cblock (loc loc1454) (body body1455)))
(define-inline (cblock?::bool obj::obj) ((@ isa? __object) obj (@ cblock cgen_cop)))
(define (cblock-nil::cblock) (class-nil (@ cblock cgen_cop)))
(define-inline (cblock-body::cop o::cblock) (-> |#!bigloo_wallow| o body))
(define-inline (cblock-body-set! o::cblock v::cop) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (cblock-loc::obj o::cblock) (-> |#!bigloo_wallow| o loc))
(define-inline (cblock-loc-set! o::cblock v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; creturn
(define-inline (make-creturn::creturn loc1451::obj value1452::cop) (instantiate::creturn (loc loc1451) (value value1452)))
(define-inline (creturn?::bool obj::obj) ((@ isa? __object) obj (@ creturn cgen_cop)))
(define (creturn-nil::creturn) (class-nil (@ creturn cgen_cop)))
(define-inline (creturn-value::cop o::creturn) (-> |#!bigloo_wallow| o value))
(define-inline (creturn-value-set! o::creturn v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (creturn-loc::obj o::creturn) (-> |#!bigloo_wallow| o loc))
(define-inline (creturn-loc-set! o::creturn v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cvoid
(define-inline (make-cvoid::cvoid loc1448::obj value1449::cop) (instantiate::cvoid (loc loc1448) (value value1449)))
(define-inline (cvoid?::bool obj::obj) ((@ isa? __object) obj (@ cvoid cgen_cop)))
(define (cvoid-nil::cvoid) (class-nil (@ cvoid cgen_cop)))
(define-inline (cvoid-value::cop o::cvoid) (-> |#!bigloo_wallow| o value))
(define-inline (cvoid-value-set! o::cvoid v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (cvoid-loc::obj o::cvoid) (-> |#!bigloo_wallow| o loc))
(define-inline (cvoid-loc-set! o::cvoid v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; catom
(define-inline (make-catom::catom loc1444::obj value1445::obj) (instantiate::catom (loc loc1444) (value value1445)))
(define-inline (catom?::bool obj::obj) ((@ isa? __object) obj (@ catom cgen_cop)))
(define (catom-nil::catom) (class-nil (@ catom cgen_cop)))
(define-inline (catom-value::obj o::catom) (-> |#!bigloo_wallow| o value))
(define-inline (catom-value-set! o::catom v::obj) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (catom-loc::obj o::catom) (-> |#!bigloo_wallow| o loc))
(define-inline (catom-loc-set! o::catom v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; varc
(define-inline (make-varc::varc loc1441::obj variable1442::variable) (instantiate::varc (loc loc1441) (variable variable1442)))
(define-inline (varc?::bool obj::obj) ((@ isa? __object) obj (@ varc cgen_cop)))
(define (varc-nil::varc) (class-nil (@ varc cgen_cop)))
(define-inline (varc-variable::variable o::varc) (-> |#!bigloo_wallow| o variable))
(define-inline (varc-variable-set! o::varc v::variable) (set! (-> |#!bigloo_wallow| o variable) v))
(define-inline (varc-loc::obj o::varc) (-> |#!bigloo_wallow| o loc))
(define-inline (varc-loc-set! o::varc v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cpragma
(define-inline (make-cpragma::cpragma loc1437::obj format1438::bstring args1439::obj) (instantiate::cpragma (loc loc1437) (format format1438) (args args1439)))
(define-inline (cpragma?::bool obj::obj) ((@ isa? __object) obj (@ cpragma cgen_cop)))
(define (cpragma-nil::cpragma) (class-nil (@ cpragma cgen_cop)))
(define-inline (cpragma-args::obj o::cpragma) (-> |#!bigloo_wallow| o args))
(define-inline (cpragma-args-set! o::cpragma v::obj) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (cpragma-format::bstring o::cpragma) (-> |#!bigloo_wallow| o format))
(define-inline (cpragma-format-set! o::cpragma v::bstring) (set! (-> |#!bigloo_wallow| o format) v))
(define-inline (cpragma-loc::obj o::cpragma) (-> |#!bigloo_wallow| o loc))
(define-inline (cpragma-loc-set! o::cpragma v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; ccast
(define-inline (make-ccast::ccast loc1383::obj type1384::type arg1435::cop) (instantiate::ccast (loc loc1383) (type type1384) (arg arg1435)))
(define-inline (ccast?::bool obj::obj) ((@ isa? __object) obj (@ ccast cgen_cop)))
(define (ccast-nil::ccast) (class-nil (@ ccast cgen_cop)))
(define-inline (ccast-arg::cop o::ccast) (-> |#!bigloo_wallow| o arg))
(define-inline (ccast-arg-set! o::ccast v::cop) (set! (-> |#!bigloo_wallow| o arg) v))
(define-inline (ccast-type::type o::ccast) (-> |#!bigloo_wallow| o type))
(define-inline (ccast-type-set! o::ccast v::type) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (ccast-loc::obj o::ccast) (-> |#!bigloo_wallow| o loc))
(define-inline (ccast-loc-set! o::ccast v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; csequence
(define-inline (make-csequence::csequence loc1379::obj c-exp?1380::bool cops1381::obj) (instantiate::csequence (loc loc1379) (c-exp? c-exp?1380) (cops cops1381)))
(define-inline (csequence?::bool obj::obj) ((@ isa? __object) obj (@ csequence cgen_cop)))
(define (csequence-nil::csequence) (class-nil (@ csequence cgen_cop)))
(define-inline (csequence-cops::obj o::csequence) (-> |#!bigloo_wallow| o cops))
(define-inline (csequence-cops-set! o::csequence v::obj) (set! (-> |#!bigloo_wallow| o cops) v))
(define-inline (csequence-c-exp?::bool o::csequence) (-> |#!bigloo_wallow| o c-exp?))
(define-inline (csequence-c-exp?-set! o::csequence v::bool) (set! (-> |#!bigloo_wallow| o c-exp?) v))
(define-inline (csequence-loc::obj o::csequence) (-> |#!bigloo_wallow| o loc))
(define-inline (csequence-loc-set! o::csequence v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; nop
(define-inline (make-nop::nop loc1377::obj) (instantiate::nop (loc loc1377)))
(define-inline (nop?::bool obj::obj) ((@ isa? __object) obj (@ nop cgen_cop)))
(define (nop-nil::nop) (class-nil (@ nop cgen_cop)))
(define-inline (nop-loc::obj o::nop) (-> |#!bigloo_wallow| o loc))
(define-inline (nop-loc-set! o::nop v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; stop
(define-inline (make-stop::stop loc1374::obj value1375::cop) (instantiate::stop (loc loc1374) (value value1375)))
(define-inline (stop?::bool obj::obj) ((@ isa? __object) obj (@ stop cgen_cop)))
(define (stop-nil::stop) (class-nil (@ stop cgen_cop)))
(define-inline (stop-value::cop o::stop) (-> |#!bigloo_wallow| o value))
(define-inline (stop-value-set! o::stop v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (stop-loc::obj o::stop) (-> |#!bigloo_wallow| o loc))
(define-inline (stop-loc-set! o::stop v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; csetq
(define-inline (make-csetq::csetq loc1370::obj var1371::varc value1372::cop) (instantiate::csetq (loc loc1370) (var var1371) (value value1372)))
(define-inline (csetq?::bool obj::obj) ((@ isa? __object) obj (@ csetq cgen_cop)))
(define (csetq-nil::csetq) (class-nil (@ csetq cgen_cop)))
(define-inline (csetq-value::cop o::csetq) (-> |#!bigloo_wallow| o value))
(define-inline (csetq-value-set! o::csetq v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (csetq-var::varc o::csetq) (-> |#!bigloo_wallow| o var))
(define-inline (csetq-var-set! o::csetq v::varc) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (csetq-loc::obj o::csetq) (-> |#!bigloo_wallow| o loc))
(define-inline (csetq-loc-set! o::csetq v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cif
(define-inline (make-cif::cif loc1365::obj test1366::cop true1367::cop false1368::cop) (instantiate::cif (loc loc1365) (test test1366) (true true1367) (false false1368)))
(define-inline (cif?::bool obj::obj) ((@ isa? __object) obj (@ cif cgen_cop)))
(define (cif-nil::cif) (class-nil (@ cif cgen_cop)))
(define-inline (cif-false::cop o::cif) (-> |#!bigloo_wallow| o false))
(define-inline (cif-false-set! o::cif v::cop) (set! (-> |#!bigloo_wallow| o false) v))
(define-inline (cif-true::cop o::cif) (-> |#!bigloo_wallow| o true))
(define-inline (cif-true-set! o::cif v::cop) (set! (-> |#!bigloo_wallow| o true) v))
(define-inline (cif-test::cop o::cif) (-> |#!bigloo_wallow| o test))
(define-inline (cif-test-set! o::cif v::cop) (set! (-> |#!bigloo_wallow| o test) v))
(define-inline (cif-loc::obj o::cif) (-> |#!bigloo_wallow| o loc))
(define-inline (cif-loc-set! o::cif v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; local-var
(define-inline (make-local-var::local-var loc1362::obj vars1363::obj) (instantiate::local-var (loc loc1362) (vars vars1363)))
(define-inline (local-var?::bool obj::obj) ((@ isa? __object) obj (@ local-var cgen_cop)))
(define (local-var-nil::local-var) (class-nil (@ local-var cgen_cop)))
(define-inline (local-var-vars::obj o::local-var) (-> |#!bigloo_wallow| o vars))
(define-inline (local-var-vars-set! o::local-var v::obj) (set! (-> |#!bigloo_wallow| o vars) v))
(define-inline (local-var-loc::obj o::local-var) (-> |#!bigloo_wallow| o loc))
(define-inline (local-var-loc-set! o::local-var v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cfuncall
(define-inline (make-cfuncall::cfuncall loc1356::obj fun1357::cop args1358::obj strength1359::symbol type1360::obj) (instantiate::cfuncall (loc loc1356) (fun fun1357) (args args1358) (strength strength1359) (type type1360)))
(define-inline (cfuncall?::bool obj::obj) ((@ isa? __object) obj (@ cfuncall cgen_cop)))
(define (cfuncall-nil::cfuncall) (class-nil (@ cfuncall cgen_cop)))
(define-inline (cfuncall-type::obj o::cfuncall) (-> |#!bigloo_wallow| o type))
(define-inline (cfuncall-type-set! o::cfuncall v::obj) (set! (-> |#!bigloo_wallow| o type) v))
(define-inline (cfuncall-strength::symbol o::cfuncall) (-> |#!bigloo_wallow| o strength))
(define-inline (cfuncall-strength-set! o::cfuncall v::symbol) (set! (-> |#!bigloo_wallow| o strength) v))
(define-inline (cfuncall-args::obj o::cfuncall) (-> |#!bigloo_wallow| o args))
(define-inline (cfuncall-args-set! o::cfuncall v::obj) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (cfuncall-fun::cop o::cfuncall) (-> |#!bigloo_wallow| o fun))
(define-inline (cfuncall-fun-set! o::cfuncall v::cop) (set! (-> |#!bigloo_wallow| o fun) v))
(define-inline (cfuncall-loc::obj o::cfuncall) (-> |#!bigloo_wallow| o loc))
(define-inline (cfuncall-loc-set! o::cfuncall v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; capply
(define-inline (make-capply::capply loc1343::obj fun1344::cop arg1354::cop) (instantiate::capply (loc loc1343) (fun fun1344) (arg arg1354)))
(define-inline (capply?::bool obj::obj) ((@ isa? __object) obj (@ capply cgen_cop)))
(define (capply-nil::capply) (class-nil (@ capply cgen_cop)))
(define-inline (capply-arg::cop o::capply) (-> |#!bigloo_wallow| o arg))
(define-inline (capply-arg-set! o::capply v::cop) (set! (-> |#!bigloo_wallow| o arg) v))
(define-inline (capply-fun::cop o::capply) (-> |#!bigloo_wallow| o fun))
(define-inline (capply-fun-set! o::capply v::cop) (set! (-> |#!bigloo_wallow| o fun) v))
(define-inline (capply-loc::obj o::capply) (-> |#!bigloo_wallow| o loc))
(define-inline (capply-loc-set! o::capply v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; capp
(define-inline (make-capp::capp loc1339::obj fun1340::cop args1341::obj) (instantiate::capp (loc loc1339) (fun fun1340) (args args1341)))
(define-inline (capp?::bool obj::obj) ((@ isa? __object) obj (@ capp cgen_cop)))
(define (capp-nil::capp) (class-nil (@ capp cgen_cop)))
(define-inline (capp-args::obj o::capp) (-> |#!bigloo_wallow| o args))
(define-inline (capp-args-set! o::capp v::obj) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (capp-fun::cop o::capp) (-> |#!bigloo_wallow| o fun))
(define-inline (capp-fun-set! o::capp v::cop) (set! (-> |#!bigloo_wallow| o fun) v))
(define-inline (capp-loc::obj o::capp) (-> |#!bigloo_wallow| o loc))
(define-inline (capp-loc-set! o::capp v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cfail
(define-inline (make-cfail::cfail loc1334::obj proc1335::cop msg1336::cop obj1337::cop) (instantiate::cfail (loc loc1334) (proc proc1335) (msg msg1336) (obj obj1337)))
(define-inline (cfail?::bool obj::obj) ((@ isa? __object) obj (@ cfail cgen_cop)))
(define (cfail-nil::cfail) (class-nil (@ cfail cgen_cop)))
(define-inline (cfail-obj::cop o::cfail) (-> |#!bigloo_wallow| o obj))
(define-inline (cfail-obj-set! o::cfail v::cop) (set! (-> |#!bigloo_wallow| o obj) v))
(define-inline (cfail-msg::cop o::cfail) (-> |#!bigloo_wallow| o msg))
(define-inline (cfail-msg-set! o::cfail v::cop) (set! (-> |#!bigloo_wallow| o msg) v))
(define-inline (cfail-proc::cop o::cfail) (-> |#!bigloo_wallow| o proc))
(define-inline (cfail-proc-set! o::cfail v::cop) (set! (-> |#!bigloo_wallow| o proc) v))
(define-inline (cfail-loc::obj o::cfail) (-> |#!bigloo_wallow| o loc))
(define-inline (cfail-loc-set! o::cfail v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cswitch
(define-inline (make-cswitch::cswitch loc1330::obj test1331::cop clauses1332::obj) (instantiate::cswitch (loc loc1330) (test test1331) (clauses clauses1332)))
(define-inline (cswitch?::bool obj::obj) ((@ isa? __object) obj (@ cswitch cgen_cop)))
(define (cswitch-nil::cswitch) (class-nil (@ cswitch cgen_cop)))
(define-inline (cswitch-clauses::obj o::cswitch) (-> |#!bigloo_wallow| o clauses))
(define-inline (cswitch-clauses-set! o::cswitch v::obj) (set! (-> |#!bigloo_wallow| o clauses) v))
(define-inline (cswitch-test::cop o::cswitch) (-> |#!bigloo_wallow| o test))
(define-inline (cswitch-test-set! o::cswitch v::cop) (set! (-> |#!bigloo_wallow| o test) v))
(define-inline (cswitch-loc::obj o::cswitch) (-> |#!bigloo_wallow| o loc))
(define-inline (cswitch-loc-set! o::cswitch v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cmake-box
(define-inline (make-cmake-box::cmake-box loc1327::obj value1328::cop) (instantiate::cmake-box (loc loc1327) (value value1328)))
(define-inline (cmake-box?::bool obj::obj) ((@ isa? __object) obj (@ cmake-box cgen_cop)))
(define (cmake-box-nil::cmake-box) (class-nil (@ cmake-box cgen_cop)))
(define-inline (cmake-box-value::cop o::cmake-box) (-> |#!bigloo_wallow| o value))
(define-inline (cmake-box-value-set! o::cmake-box v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (cmake-box-loc::obj o::cmake-box) (-> |#!bigloo_wallow| o loc))
(define-inline (cmake-box-loc-set! o::cmake-box v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cbox-ref
(define-inline (make-cbox-ref::cbox-ref loc1324::obj var1325::cop) (instantiate::cbox-ref (loc loc1324) (var var1325)))
(define-inline (cbox-ref?::bool obj::obj) ((@ isa? __object) obj (@ cbox-ref cgen_cop)))
(define (cbox-ref-nil::cbox-ref) (class-nil (@ cbox-ref cgen_cop)))
(define-inline (cbox-ref-var::cop o::cbox-ref) (-> |#!bigloo_wallow| o var))
(define-inline (cbox-ref-var-set! o::cbox-ref v::cop) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (cbox-ref-loc::obj o::cbox-ref) (-> |#!bigloo_wallow| o loc))
(define-inline (cbox-ref-loc-set! o::cbox-ref v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cbox-set!
(define-inline (make-cbox-set!::cbox-set! loc1320::obj var1321::cop value1322::cop) (instantiate::cbox-set! (loc loc1320) (var var1321) (value value1322)))
(define-inline (cbox-set!?::bool obj::obj) ((@ isa? __object) obj (@ cbox-set! cgen_cop)))
(define (cbox-set!-nil::cbox-set!) (class-nil (@ cbox-set! cgen_cop)))
(define-inline (cbox-set!-value::cop o::cbox-set!) (-> |#!bigloo_wallow| o value))
(define-inline (cbox-set!-value-set! o::cbox-set! v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (cbox-set!-var::cop o::cbox-set!) (-> |#!bigloo_wallow| o var))
(define-inline (cbox-set!-var-set! o::cbox-set! v::cop) (set! (-> |#!bigloo_wallow| o var) v))
(define-inline (cbox-set!-loc::obj o::cbox-set!) (-> |#!bigloo_wallow| o loc))
(define-inline (cbox-set!-loc-set! o::cbox-set! v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cset-ex-it
(define-inline (make-cset-ex-it::cset-ex-it loc1315::obj exit1316::cop jump-value1317::cop body1318::cop) (instantiate::cset-ex-it (loc loc1315) (exit exit1316) (jump-value jump-value1317) (body body1318)))
(define-inline (cset-ex-it?::bool obj::obj) ((@ isa? __object) obj (@ cset-ex-it cgen_cop)))
(define (cset-ex-it-nil::cset-ex-it) (class-nil (@ cset-ex-it cgen_cop)))
(define-inline (cset-ex-it-body::cop o::cset-ex-it) (-> |#!bigloo_wallow| o body))
(define-inline (cset-ex-it-body-set! o::cset-ex-it v::cop) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (cset-ex-it-jump-value::cop o::cset-ex-it) (-> |#!bigloo_wallow| o jump-value))
(define-inline (cset-ex-it-jump-value-set! o::cset-ex-it v::cop) (set! (-> |#!bigloo_wallow| o jump-value) v))
(define-inline (cset-ex-it-exit::cop o::cset-ex-it) (-> |#!bigloo_wallow| o exit))
(define-inline (cset-ex-it-exit-set! o::cset-ex-it v::cop) (set! (-> |#!bigloo_wallow| o exit) v))
(define-inline (cset-ex-it-loc::obj o::cset-ex-it) (-> |#!bigloo_wallow| o loc))
(define-inline (cset-ex-it-loc-set! o::cset-ex-it v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; cjump-ex-it
(define-inline (make-cjump-ex-it::cjump-ex-it loc1311::obj exit1312::cop value1313::cop) (instantiate::cjump-ex-it (loc loc1311) (exit exit1312) (value value1313)))
(define-inline (cjump-ex-it?::bool obj::obj) ((@ isa? __object) obj (@ cjump-ex-it cgen_cop)))
(define (cjump-ex-it-nil::cjump-ex-it) (class-nil (@ cjump-ex-it cgen_cop)))
(define-inline (cjump-ex-it-value::cop o::cjump-ex-it) (-> |#!bigloo_wallow| o value))
(define-inline (cjump-ex-it-value-set! o::cjump-ex-it v::cop) (set! (-> |#!bigloo_wallow| o value) v))
(define-inline (cjump-ex-it-exit::cop o::cjump-ex-it) (-> |#!bigloo_wallow| o exit))
(define-inline (cjump-ex-it-exit-set! o::cjump-ex-it v::cop) (set! (-> |#!bigloo_wallow| o exit) v))
(define-inline (cjump-ex-it-loc::obj o::cjump-ex-it) (-> |#!bigloo_wallow| o loc))
(define-inline (cjump-ex-it-loc-set! o::cjump-ex-it v::obj) (set! (-> |#!bigloo_wallow| o loc) v))

;; sfun/C
(define-inline (make-sfun/C::sfun/C arity1289::long side-effect1290::obj predicate-of1291::obj stack-allocator1292::obj top?1293::bool the-closure1294::obj effect1295::obj failsafe1296::obj property1297::obj args1298::obj args-name1299::obj body1300::obj class1301::obj dsssl-keywords1302::obj loc1303::obj optionals1304::obj keys1305::obj the-closure-global1306::obj strength1307::symbol label1308::clabel integrated1309::bool) (instantiate::sfun/C (arity arity1289) (side-effect side-effect1290) (predicate-of predicate-of1291) (stack-allocator stack-allocator1292) (top? top?1293) (the-closure the-closure1294) (effect effect1295) (failsafe failsafe1296) (property property1297) (args args1298) (args-name args-name1299) (body body1300) (class class1301) (dsssl-keywords dsssl-keywords1302) (loc loc1303) (optionals optionals1304) (keys keys1305) (the-closure-global the-closure-global1306) (strength strength1307) (label label1308) (integrated integrated1309)))
(define-inline (sfun/C?::bool obj::obj) ((@ isa? __object) obj (@ sfun/C cgen_cop)))
(define (sfun/C-nil::sfun/C) (class-nil (@ sfun/C cgen_cop)))
(define-inline (sfun/C-integrated::bool o::sfun/C) (-> |#!bigloo_wallow| o integrated))
(define-inline (sfun/C-integrated-set! o::sfun/C v::bool) (set! (-> |#!bigloo_wallow| o integrated) v))
(define-inline (sfun/C-label::clabel o::sfun/C) (-> |#!bigloo_wallow| o label))
(define-inline (sfun/C-label-set! o::sfun/C v::clabel) (set! (-> |#!bigloo_wallow| o label) v))
(define-inline (sfun/C-strength::symbol o::sfun/C) (-> |#!bigloo_wallow| o strength))
(define-inline (sfun/C-strength-set! o::sfun/C v::symbol) (set! (-> |#!bigloo_wallow| o strength) v))
(define-inline (sfun/C-the-closure-global::obj o::sfun/C) (-> |#!bigloo_wallow| o the-closure-global))
(define-inline (sfun/C-the-closure-global-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o the-closure-global) v))
(define-inline (sfun/C-keys::obj o::sfun/C) (-> |#!bigloo_wallow| o keys))
(define-inline (sfun/C-keys-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o keys) v))
(define-inline (sfun/C-optionals::obj o::sfun/C) (-> |#!bigloo_wallow| o optionals))
(define-inline (sfun/C-optionals-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o optionals) v))
(define-inline (sfun/C-loc::obj o::sfun/C) (-> |#!bigloo_wallow| o loc))
(define-inline (sfun/C-loc-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
(define-inline (sfun/C-dsssl-keywords::obj o::sfun/C) (-> |#!bigloo_wallow| o dsssl-keywords))
(define-inline (sfun/C-dsssl-keywords-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o dsssl-keywords) v))
(define-inline (sfun/C-class::obj o::sfun/C) (-> |#!bigloo_wallow| o class))
(define-inline (sfun/C-class-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o class) v))
(define-inline (sfun/C-body::obj o::sfun/C) (-> |#!bigloo_wallow| o body))
(define-inline (sfun/C-body-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (sfun/C-args-name::obj o::sfun/C) (-> |#!bigloo_wallow| o args-name))
(define-inline (sfun/C-args-name-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o args-name) v))
(define-inline (sfun/C-args::obj o::sfun/C) (-> |#!bigloo_wallow| o args))
(define-inline (sfun/C-args-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o args) v))
(define-inline (sfun/C-property::obj o::sfun/C) (-> |#!bigloo_wallow| o property))
(define-inline (sfun/C-property-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o property) v))
(define-inline (sfun/C-failsafe::obj o::sfun/C) (-> |#!bigloo_wallow| o failsafe))
(define-inline (sfun/C-failsafe-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o failsafe) v))
(define-inline (sfun/C-effect::obj o::sfun/C) (-> |#!bigloo_wallow| o effect))
(define-inline (sfun/C-effect-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o effect) v))
(define-inline (sfun/C-the-closure::obj o::sfun/C) (-> |#!bigloo_wallow| o the-closure))
(define-inline (sfun/C-the-closure-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o the-closure) v))
(define-inline (sfun/C-top?::bool o::sfun/C) (-> |#!bigloo_wallow| o top?))
(define-inline (sfun/C-top?-set! o::sfun/C v::bool) (set! (-> |#!bigloo_wallow| o top?) v))
(define-inline (sfun/C-stack-allocator::obj o::sfun/C) (-> |#!bigloo_wallow| o stack-allocator))
(define-inline (sfun/C-stack-allocator-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o stack-allocator) v))
(define-inline (sfun/C-predicate-of::obj o::sfun/C) (-> |#!bigloo_wallow| o predicate-of))
(define-inline (sfun/C-predicate-of-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o predicate-of) v))
(define-inline (sfun/C-side-effect::obj o::sfun/C) (-> |#!bigloo_wallow| o side-effect))
(define-inline (sfun/C-side-effect-set! o::sfun/C v::obj) (set! (-> |#!bigloo_wallow| o side-effect) v))
(define-inline (sfun/C-arity::long o::sfun/C) (-> |#!bigloo_wallow| o arity))
(define-inline (sfun/C-arity-set! o::sfun/C v::long) (set! (-> |#!bigloo_wallow| o arity) v))

;; bdb-block
(define-inline (make-bdb-block::bdb-block loc1286::obj body1287::cop) (instantiate::bdb-block (loc loc1286) (body body1287)))
(define-inline (bdb-block?::bool obj::obj) ((@ isa? __object) obj (@ bdb-block cgen_cop)))
(define (bdb-block-nil::bdb-block) (class-nil (@ bdb-block cgen_cop)))
(define-inline (bdb-block-body::cop o::bdb-block) (-> |#!bigloo_wallow| o body))
(define-inline (bdb-block-body-set! o::bdb-block v::cop) (set! (-> |#!bigloo_wallow| o body) v))
(define-inline (bdb-block-loc::obj o::bdb-block) (-> |#!bigloo_wallow| o loc))
(define-inline (bdb-block-loc-set! o::bdb-block v::obj) (set! (-> |#!bigloo_wallow| o loc) v))
))
