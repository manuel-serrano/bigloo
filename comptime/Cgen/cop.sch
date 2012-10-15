;; ==========================================================
;; Class accessors
;; Bigloo (3.8d)
;; Inria -- Sophia Antipolis     Mon Oct 15 08:01:14 CEST 2012 
;; (bigloo.new -classgen Cgen/cop.scm)
;; ==========================================================

;; The directives
(directives

;; cop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cop::cop loc1413::obj)
    (inline cop?::bool ::obj)
    (cop-nil::cop)
    (inline cop-loc::obj ::cop)
    (inline cop-loc-set! ::cop ::obj))))

;; clabel
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-clabel::clabel loc1408::obj name1409::bstring used?1410::bool body1411::obj)
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
    (inline make-cgoto::cgoto loc1405::obj label1406::clabel)
    (inline cgoto?::bool ::obj)
    (cgoto-nil::cgoto)
    (inline cgoto-label::clabel ::cgoto)
    (inline cgoto-loc::obj ::cgoto)
    (inline cgoto-loc-set! ::cgoto ::obj))))

;; cblock
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cblock::cblock loc1402::obj body1403::cop)
    (inline cblock?::bool ::obj)
    (cblock-nil::cblock)
    (inline cblock-body::cop ::cblock)
    (inline cblock-loc::obj ::cblock)
    (inline cblock-loc-set! ::cblock ::obj))))

;; creturn
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-creturn::creturn loc1399::obj value1400::cop)
    (inline creturn?::bool ::obj)
    (creturn-nil::creturn)
    (inline creturn-value::cop ::creturn)
    (inline creturn-loc::obj ::creturn)
    (inline creturn-loc-set! ::creturn ::obj))))

;; cvoid
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cvoid::cvoid loc1396::obj value1397::cop)
    (inline cvoid?::bool ::obj)
    (cvoid-nil::cvoid)
    (inline cvoid-value::cop ::cvoid)
    (inline cvoid-loc::obj ::cvoid)
    (inline cvoid-loc-set! ::cvoid ::obj))))

;; catom
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-catom::catom loc1393::obj value1394::obj)
    (inline catom?::bool ::obj)
    (catom-nil::catom)
    (inline catom-value::obj ::catom)
    (inline catom-loc::obj ::catom)
    (inline catom-loc-set! ::catom ::obj))))

;; varc
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-varc::varc loc1390::obj variable1391::variable)
    (inline varc?::bool ::obj)
    (varc-nil::varc)
    (inline varc-variable::variable ::varc)
    (inline varc-loc::obj ::varc)
    (inline varc-loc-set! ::varc ::obj))))

;; cpragma
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cpragma::cpragma loc1386::obj format1387::bstring args1388::obj)
    (inline cpragma?::bool ::obj)
    (cpragma-nil::cpragma)
    (inline cpragma-args::obj ::cpragma)
    (inline cpragma-format::bstring ::cpragma)
    (inline cpragma-loc::obj ::cpragma)
    (inline cpragma-loc-set! ::cpragma ::obj))))

;; ccast
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-ccast::ccast loc1373::obj type1374::type arg1384::cop)
    (inline ccast?::bool ::obj)
    (ccast-nil::ccast)
    (inline ccast-arg::cop ::ccast)
    (inline ccast-type::type ::ccast)
    (inline ccast-loc::obj ::ccast)
    (inline ccast-loc-set! ::ccast ::obj))))

;; csequence
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-csequence::csequence loc1369::obj c-exp?1370::bool cops1371::obj)
    (inline csequence?::bool ::obj)
    (csequence-nil::csequence)
    (inline csequence-cops::obj ::csequence)
    (inline csequence-c-exp?::bool ::csequence)
    (inline csequence-loc::obj ::csequence)
    (inline csequence-loc-set! ::csequence ::obj))))

;; nop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-nop::nop loc1367::obj)
    (inline nop?::bool ::obj)
    (nop-nil::nop)
    (inline nop-loc::obj ::nop)
    (inline nop-loc-set! ::nop ::obj))))

;; stop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-stop::stop loc1364::obj value1365::cop)
    (inline stop?::bool ::obj)
    (stop-nil::stop)
    (inline stop-value::cop ::stop)
    (inline stop-loc::obj ::stop)
    (inline stop-loc-set! ::stop ::obj))))

;; csetq
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-csetq::csetq loc1360::obj var1361::varc value1362::cop)
    (inline csetq?::bool ::obj)
    (csetq-nil::csetq)
    (inline csetq-value::cop ::csetq)
    (inline csetq-var::varc ::csetq)
    (inline csetq-loc::obj ::csetq)
    (inline csetq-loc-set! ::csetq ::obj))))

;; cif
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cif::cif loc1355::obj test1356::cop true1357::cop false1358::cop)
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
    (inline make-local-var::local-var loc1352::obj vars1353::obj)
    (inline local-var?::bool ::obj)
    (local-var-nil::local-var)
    (inline local-var-vars::obj ::local-var)
    (inline local-var-loc::obj ::local-var)
    (inline local-var-loc-set! ::local-var ::obj))))

;; cfuncall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cfuncall::cfuncall loc1346::obj fun1347::cop args1348::obj strength1349::symbol type1350::obj)
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
    (inline make-capply::capply loc1342::obj fun1343::cop arg1344::cop)
    (inline capply?::bool ::obj)
    (capply-nil::capply)
    (inline capply-arg::cop ::capply)
    (inline capply-fun::cop ::capply)
    (inline capply-loc::obj ::capply)
    (inline capply-loc-set! ::capply ::obj))))

;; capp
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-capp::capp loc1338::obj fun1339::cop args1340::obj)
    (inline capp?::bool ::obj)
    (capp-nil::capp)
    (inline capp-args::obj ::capp)
    (inline capp-fun::cop ::capp)
    (inline capp-loc::obj ::capp)
    (inline capp-loc-set! ::capp ::obj))))

;; cfail
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cfail::cfail loc1333::obj proc1334::cop msg1335::cop obj1336::cop)
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
    (inline make-cswitch::cswitch loc1327::obj test1330::cop clauses1331::obj)
    (inline cswitch?::bool ::obj)
    (cswitch-nil::cswitch)
    (inline cswitch-clauses::obj ::cswitch)
    (inline cswitch-test::cop ::cswitch)
    (inline cswitch-loc::obj ::cswitch)
    (inline cswitch-loc-set! ::cswitch ::obj))))

;; cmake-box
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cmake-box::cmake-box loc1324::obj value1325::cop)
    (inline cmake-box?::bool ::obj)
    (cmake-box-nil::cmake-box)
    (inline cmake-box-value::cop ::cmake-box)
    (inline cmake-box-loc::obj ::cmake-box)
    (inline cmake-box-loc-set! ::cmake-box ::obj))))

;; cbox-ref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cbox-ref::cbox-ref loc1321::obj var1322::cop)
    (inline cbox-ref?::bool ::obj)
    (cbox-ref-nil::cbox-ref)
    (inline cbox-ref-var::cop ::cbox-ref)
    (inline cbox-ref-loc::obj ::cbox-ref)
    (inline cbox-ref-loc-set! ::cbox-ref ::obj))))

;; cbox-set!
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cbox-set!::cbox-set! loc1317::obj var1318::cop value1319::cop)
    (inline cbox-set!?::bool ::obj)
    (cbox-set!-nil::cbox-set!)
    (inline cbox-set!-value::cop ::cbox-set!)
    (inline cbox-set!-var::cop ::cbox-set!)
    (inline cbox-set!-loc::obj ::cbox-set!)
    (inline cbox-set!-loc-set! ::cbox-set! ::obj))))

;; cset-ex-it
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-cset-ex-it::cset-ex-it loc1312::obj exit1313::cop jump-value1314::cop body1315::cop)
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
    (inline make-cjump-ex-it::cjump-ex-it loc1308::obj exit1309::cop value1310::cop)
    (inline cjump-ex-it?::bool ::obj)
    (cjump-ex-it-nil::cjump-ex-it)
    (inline cjump-ex-it-value::cop ::cjump-ex-it)
    (inline cjump-ex-it-exit::cop ::cjump-ex-it)
    (inline cjump-ex-it-loc::obj ::cjump-ex-it)
    (inline cjump-ex-it-loc-set! ::cjump-ex-it ::obj))))

;; sfun/C
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-sfun/C::sfun/C arity1287::long side-effect1288::obj predicate-of1289::obj stack-allocator1290::obj top?1291::bool the-closure1292::obj effect1293::obj property1294::obj args1295::obj args-name1296::obj body1297::obj class1298::obj dsssl-keywords1299::obj loc1300::obj optionals1301::obj keys1302::obj the-closure-global1303::obj strength1304::symbol label1305::clabel integrated1306::bool)
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
    (inline make-bdb-block::bdb-block loc1284::obj body1285::cop)
    (inline bdb-block?::bool ::obj)
    (bdb-block-nil::bdb-block)
    (inline bdb-block-body::cop ::bdb-block)
    (inline bdb-block-loc::obj ::bdb-block)
    (inline bdb-block-loc-set! ::bdb-block ::obj)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; cop
(define-inline (make-cop::cop loc1413::obj) (instantiate::cop (loc loc1413)))
(define-inline (cop?::bool obj::obj) ((@ isa? __object) obj (@ cop cgen_cop)))
(define (cop-nil::cop) (class-nil (@ cop cgen_cop)))
(define-inline (cop-loc::obj o::cop) (with-access::cop o (loc) loc))
(define-inline (cop-loc-set! o::cop v::obj) (with-access::cop o (loc) (set! loc v)))

;; clabel
(define-inline (make-clabel::clabel loc1408::obj name1409::bstring used?1410::bool body1411::obj) (instantiate::clabel (loc loc1408) (name name1409) (used? used?1410) (body body1411)))
(define-inline (clabel?::bool obj::obj) ((@ isa? __object) obj (@ clabel cgen_cop)))
(define (clabel-nil::clabel) (class-nil (@ clabel cgen_cop)))
(define-inline (clabel-body::obj o::clabel) (with-access::clabel o (body) body))
(define-inline (clabel-body-set! o::clabel v::obj) (with-access::clabel o (body) (set! body v)))
(define-inline (clabel-used?::bool o::clabel) (with-access::clabel o (used?) used?))
(define-inline (clabel-used?-set! o::clabel v::bool) (with-access::clabel o (used?) (set! used? v)))
(define-inline (clabel-name::bstring o::clabel) (with-access::clabel o (name) name))
(define-inline (clabel-name-set! o::clabel v::bstring) (with-access::clabel o (name) (set! name v)))
(define-inline (clabel-loc::obj o::clabel) (with-access::clabel o (loc) loc))
(define-inline (clabel-loc-set! o::clabel v::obj) (with-access::clabel o (loc) (set! loc v)))

;; cgoto
(define-inline (make-cgoto::cgoto loc1405::obj label1406::clabel) (instantiate::cgoto (loc loc1405) (label label1406)))
(define-inline (cgoto?::bool obj::obj) ((@ isa? __object) obj (@ cgoto cgen_cop)))
(define (cgoto-nil::cgoto) (class-nil (@ cgoto cgen_cop)))
(define-inline (cgoto-label::clabel o::cgoto) (with-access::cgoto o (label) label))
(define-inline (cgoto-label-set! o::cgoto v::clabel) (with-access::cgoto o (label) (set! label v)))
(define-inline (cgoto-loc::obj o::cgoto) (with-access::cgoto o (loc) loc))
(define-inline (cgoto-loc-set! o::cgoto v::obj) (with-access::cgoto o (loc) (set! loc v)))

;; cblock
(define-inline (make-cblock::cblock loc1402::obj body1403::cop) (instantiate::cblock (loc loc1402) (body body1403)))
(define-inline (cblock?::bool obj::obj) ((@ isa? __object) obj (@ cblock cgen_cop)))
(define (cblock-nil::cblock) (class-nil (@ cblock cgen_cop)))
(define-inline (cblock-body::cop o::cblock) (with-access::cblock o (body) body))
(define-inline (cblock-body-set! o::cblock v::cop) (with-access::cblock o (body) (set! body v)))
(define-inline (cblock-loc::obj o::cblock) (with-access::cblock o (loc) loc))
(define-inline (cblock-loc-set! o::cblock v::obj) (with-access::cblock o (loc) (set! loc v)))

;; creturn
(define-inline (make-creturn::creturn loc1399::obj value1400::cop) (instantiate::creturn (loc loc1399) (value value1400)))
(define-inline (creturn?::bool obj::obj) ((@ isa? __object) obj (@ creturn cgen_cop)))
(define (creturn-nil::creturn) (class-nil (@ creturn cgen_cop)))
(define-inline (creturn-value::cop o::creturn) (with-access::creturn o (value) value))
(define-inline (creturn-value-set! o::creturn v::cop) (with-access::creturn o (value) (set! value v)))
(define-inline (creturn-loc::obj o::creturn) (with-access::creturn o (loc) loc))
(define-inline (creturn-loc-set! o::creturn v::obj) (with-access::creturn o (loc) (set! loc v)))

;; cvoid
(define-inline (make-cvoid::cvoid loc1396::obj value1397::cop) (instantiate::cvoid (loc loc1396) (value value1397)))
(define-inline (cvoid?::bool obj::obj) ((@ isa? __object) obj (@ cvoid cgen_cop)))
(define (cvoid-nil::cvoid) (class-nil (@ cvoid cgen_cop)))
(define-inline (cvoid-value::cop o::cvoid) (with-access::cvoid o (value) value))
(define-inline (cvoid-value-set! o::cvoid v::cop) (with-access::cvoid o (value) (set! value v)))
(define-inline (cvoid-loc::obj o::cvoid) (with-access::cvoid o (loc) loc))
(define-inline (cvoid-loc-set! o::cvoid v::obj) (with-access::cvoid o (loc) (set! loc v)))

;; catom
(define-inline (make-catom::catom loc1393::obj value1394::obj) (instantiate::catom (loc loc1393) (value value1394)))
(define-inline (catom?::bool obj::obj) ((@ isa? __object) obj (@ catom cgen_cop)))
(define (catom-nil::catom) (class-nil (@ catom cgen_cop)))
(define-inline (catom-value::obj o::catom) (with-access::catom o (value) value))
(define-inline (catom-value-set! o::catom v::obj) (with-access::catom o (value) (set! value v)))
(define-inline (catom-loc::obj o::catom) (with-access::catom o (loc) loc))
(define-inline (catom-loc-set! o::catom v::obj) (with-access::catom o (loc) (set! loc v)))

;; varc
(define-inline (make-varc::varc loc1390::obj variable1391::variable) (instantiate::varc (loc loc1390) (variable variable1391)))
(define-inline (varc?::bool obj::obj) ((@ isa? __object) obj (@ varc cgen_cop)))
(define (varc-nil::varc) (class-nil (@ varc cgen_cop)))
(define-inline (varc-variable::variable o::varc) (with-access::varc o (variable) variable))
(define-inline (varc-variable-set! o::varc v::variable) (with-access::varc o (variable) (set! variable v)))
(define-inline (varc-loc::obj o::varc) (with-access::varc o (loc) loc))
(define-inline (varc-loc-set! o::varc v::obj) (with-access::varc o (loc) (set! loc v)))

;; cpragma
(define-inline (make-cpragma::cpragma loc1386::obj format1387::bstring args1388::obj) (instantiate::cpragma (loc loc1386) (format format1387) (args args1388)))
(define-inline (cpragma?::bool obj::obj) ((@ isa? __object) obj (@ cpragma cgen_cop)))
(define (cpragma-nil::cpragma) (class-nil (@ cpragma cgen_cop)))
(define-inline (cpragma-args::obj o::cpragma) (with-access::cpragma o (args) args))
(define-inline (cpragma-args-set! o::cpragma v::obj) (with-access::cpragma o (args) (set! args v)))
(define-inline (cpragma-format::bstring o::cpragma) (with-access::cpragma o (format) format))
(define-inline (cpragma-format-set! o::cpragma v::bstring) (with-access::cpragma o (format) (set! format v)))
(define-inline (cpragma-loc::obj o::cpragma) (with-access::cpragma o (loc) loc))
(define-inline (cpragma-loc-set! o::cpragma v::obj) (with-access::cpragma o (loc) (set! loc v)))

;; ccast
(define-inline (make-ccast::ccast loc1373::obj type1374::type arg1384::cop) (instantiate::ccast (loc loc1373) (type type1374) (arg arg1384)))
(define-inline (ccast?::bool obj::obj) ((@ isa? __object) obj (@ ccast cgen_cop)))
(define (ccast-nil::ccast) (class-nil (@ ccast cgen_cop)))
(define-inline (ccast-arg::cop o::ccast) (with-access::ccast o (arg) arg))
(define-inline (ccast-arg-set! o::ccast v::cop) (with-access::ccast o (arg) (set! arg v)))
(define-inline (ccast-type::type o::ccast) (with-access::ccast o (type) type))
(define-inline (ccast-type-set! o::ccast v::type) (with-access::ccast o (type) (set! type v)))
(define-inline (ccast-loc::obj o::ccast) (with-access::ccast o (loc) loc))
(define-inline (ccast-loc-set! o::ccast v::obj) (with-access::ccast o (loc) (set! loc v)))

;; csequence
(define-inline (make-csequence::csequence loc1369::obj c-exp?1370::bool cops1371::obj) (instantiate::csequence (loc loc1369) (c-exp? c-exp?1370) (cops cops1371)))
(define-inline (csequence?::bool obj::obj) ((@ isa? __object) obj (@ csequence cgen_cop)))
(define (csequence-nil::csequence) (class-nil (@ csequence cgen_cop)))
(define-inline (csequence-cops::obj o::csequence) (with-access::csequence o (cops) cops))
(define-inline (csequence-cops-set! o::csequence v::obj) (with-access::csequence o (cops) (set! cops v)))
(define-inline (csequence-c-exp?::bool o::csequence) (with-access::csequence o (c-exp?) c-exp?))
(define-inline (csequence-c-exp?-set! o::csequence v::bool) (with-access::csequence o (c-exp?) (set! c-exp? v)))
(define-inline (csequence-loc::obj o::csequence) (with-access::csequence o (loc) loc))
(define-inline (csequence-loc-set! o::csequence v::obj) (with-access::csequence o (loc) (set! loc v)))

;; nop
(define-inline (make-nop::nop loc1367::obj) (instantiate::nop (loc loc1367)))
(define-inline (nop?::bool obj::obj) ((@ isa? __object) obj (@ nop cgen_cop)))
(define (nop-nil::nop) (class-nil (@ nop cgen_cop)))
(define-inline (nop-loc::obj o::nop) (with-access::nop o (loc) loc))
(define-inline (nop-loc-set! o::nop v::obj) (with-access::nop o (loc) (set! loc v)))

;; stop
(define-inline (make-stop::stop loc1364::obj value1365::cop) (instantiate::stop (loc loc1364) (value value1365)))
(define-inline (stop?::bool obj::obj) ((@ isa? __object) obj (@ stop cgen_cop)))
(define (stop-nil::stop) (class-nil (@ stop cgen_cop)))
(define-inline (stop-value::cop o::stop) (with-access::stop o (value) value))
(define-inline (stop-value-set! o::stop v::cop) (with-access::stop o (value) (set! value v)))
(define-inline (stop-loc::obj o::stop) (with-access::stop o (loc) loc))
(define-inline (stop-loc-set! o::stop v::obj) (with-access::stop o (loc) (set! loc v)))

;; csetq
(define-inline (make-csetq::csetq loc1360::obj var1361::varc value1362::cop) (instantiate::csetq (loc loc1360) (var var1361) (value value1362)))
(define-inline (csetq?::bool obj::obj) ((@ isa? __object) obj (@ csetq cgen_cop)))
(define (csetq-nil::csetq) (class-nil (@ csetq cgen_cop)))
(define-inline (csetq-value::cop o::csetq) (with-access::csetq o (value) value))
(define-inline (csetq-value-set! o::csetq v::cop) (with-access::csetq o (value) (set! value v)))
(define-inline (csetq-var::varc o::csetq) (with-access::csetq o (var) var))
(define-inline (csetq-var-set! o::csetq v::varc) (with-access::csetq o (var) (set! var v)))
(define-inline (csetq-loc::obj o::csetq) (with-access::csetq o (loc) loc))
(define-inline (csetq-loc-set! o::csetq v::obj) (with-access::csetq o (loc) (set! loc v)))

;; cif
(define-inline (make-cif::cif loc1355::obj test1356::cop true1357::cop false1358::cop) (instantiate::cif (loc loc1355) (test test1356) (true true1357) (false false1358)))
(define-inline (cif?::bool obj::obj) ((@ isa? __object) obj (@ cif cgen_cop)))
(define (cif-nil::cif) (class-nil (@ cif cgen_cop)))
(define-inline (cif-false::cop o::cif) (with-access::cif o (false) false))
(define-inline (cif-false-set! o::cif v::cop) (with-access::cif o (false) (set! false v)))
(define-inline (cif-true::cop o::cif) (with-access::cif o (true) true))
(define-inline (cif-true-set! o::cif v::cop) (with-access::cif o (true) (set! true v)))
(define-inline (cif-test::cop o::cif) (with-access::cif o (test) test))
(define-inline (cif-test-set! o::cif v::cop) (with-access::cif o (test) (set! test v)))
(define-inline (cif-loc::obj o::cif) (with-access::cif o (loc) loc))
(define-inline (cif-loc-set! o::cif v::obj) (with-access::cif o (loc) (set! loc v)))

;; local-var
(define-inline (make-local-var::local-var loc1352::obj vars1353::obj) (instantiate::local-var (loc loc1352) (vars vars1353)))
(define-inline (local-var?::bool obj::obj) ((@ isa? __object) obj (@ local-var cgen_cop)))
(define (local-var-nil::local-var) (class-nil (@ local-var cgen_cop)))
(define-inline (local-var-vars::obj o::local-var) (with-access::local-var o (vars) vars))
(define-inline (local-var-vars-set! o::local-var v::obj) (with-access::local-var o (vars) (set! vars v)))
(define-inline (local-var-loc::obj o::local-var) (with-access::local-var o (loc) loc))
(define-inline (local-var-loc-set! o::local-var v::obj) (with-access::local-var o (loc) (set! loc v)))

;; cfuncall
(define-inline (make-cfuncall::cfuncall loc1346::obj fun1347::cop args1348::obj strength1349::symbol type1350::obj) (instantiate::cfuncall (loc loc1346) (fun fun1347) (args args1348) (strength strength1349) (type type1350)))
(define-inline (cfuncall?::bool obj::obj) ((@ isa? __object) obj (@ cfuncall cgen_cop)))
(define (cfuncall-nil::cfuncall) (class-nil (@ cfuncall cgen_cop)))
(define-inline (cfuncall-type::obj o::cfuncall) (with-access::cfuncall o (type) type))
(define-inline (cfuncall-type-set! o::cfuncall v::obj) (with-access::cfuncall o (type) (set! type v)))
(define-inline (cfuncall-strength::symbol o::cfuncall) (with-access::cfuncall o (strength) strength))
(define-inline (cfuncall-strength-set! o::cfuncall v::symbol) (with-access::cfuncall o (strength) (set! strength v)))
(define-inline (cfuncall-args::obj o::cfuncall) (with-access::cfuncall o (args) args))
(define-inline (cfuncall-args-set! o::cfuncall v::obj) (with-access::cfuncall o (args) (set! args v)))
(define-inline (cfuncall-fun::cop o::cfuncall) (with-access::cfuncall o (fun) fun))
(define-inline (cfuncall-fun-set! o::cfuncall v::cop) (with-access::cfuncall o (fun) (set! fun v)))
(define-inline (cfuncall-loc::obj o::cfuncall) (with-access::cfuncall o (loc) loc))
(define-inline (cfuncall-loc-set! o::cfuncall v::obj) (with-access::cfuncall o (loc) (set! loc v)))

;; capply
(define-inline (make-capply::capply loc1342::obj fun1343::cop arg1344::cop) (instantiate::capply (loc loc1342) (fun fun1343) (arg arg1344)))
(define-inline (capply?::bool obj::obj) ((@ isa? __object) obj (@ capply cgen_cop)))
(define (capply-nil::capply) (class-nil (@ capply cgen_cop)))
(define-inline (capply-arg::cop o::capply) (with-access::capply o (arg) arg))
(define-inline (capply-arg-set! o::capply v::cop) (with-access::capply o (arg) (set! arg v)))
(define-inline (capply-fun::cop o::capply) (with-access::capply o (fun) fun))
(define-inline (capply-fun-set! o::capply v::cop) (with-access::capply o (fun) (set! fun v)))
(define-inline (capply-loc::obj o::capply) (with-access::capply o (loc) loc))
(define-inline (capply-loc-set! o::capply v::obj) (with-access::capply o (loc) (set! loc v)))

;; capp
(define-inline (make-capp::capp loc1338::obj fun1339::cop args1340::obj) (instantiate::capp (loc loc1338) (fun fun1339) (args args1340)))
(define-inline (capp?::bool obj::obj) ((@ isa? __object) obj (@ capp cgen_cop)))
(define (capp-nil::capp) (class-nil (@ capp cgen_cop)))
(define-inline (capp-args::obj o::capp) (with-access::capp o (args) args))
(define-inline (capp-args-set! o::capp v::obj) (with-access::capp o (args) (set! args v)))
(define-inline (capp-fun::cop o::capp) (with-access::capp o (fun) fun))
(define-inline (capp-fun-set! o::capp v::cop) (with-access::capp o (fun) (set! fun v)))
(define-inline (capp-loc::obj o::capp) (with-access::capp o (loc) loc))
(define-inline (capp-loc-set! o::capp v::obj) (with-access::capp o (loc) (set! loc v)))

;; cfail
(define-inline (make-cfail::cfail loc1333::obj proc1334::cop msg1335::cop obj1336::cop) (instantiate::cfail (loc loc1333) (proc proc1334) (msg msg1335) (obj obj1336)))
(define-inline (cfail?::bool obj::obj) ((@ isa? __object) obj (@ cfail cgen_cop)))
(define (cfail-nil::cfail) (class-nil (@ cfail cgen_cop)))
(define-inline (cfail-obj::cop o::cfail) (with-access::cfail o (obj) obj))
(define-inline (cfail-obj-set! o::cfail v::cop) (with-access::cfail o (obj) (set! obj v)))
(define-inline (cfail-msg::cop o::cfail) (with-access::cfail o (msg) msg))
(define-inline (cfail-msg-set! o::cfail v::cop) (with-access::cfail o (msg) (set! msg v)))
(define-inline (cfail-proc::cop o::cfail) (with-access::cfail o (proc) proc))
(define-inline (cfail-proc-set! o::cfail v::cop) (with-access::cfail o (proc) (set! proc v)))
(define-inline (cfail-loc::obj o::cfail) (with-access::cfail o (loc) loc))
(define-inline (cfail-loc-set! o::cfail v::obj) (with-access::cfail o (loc) (set! loc v)))

;; cswitch
(define-inline (make-cswitch::cswitch loc1327::obj test1330::cop clauses1331::obj) (instantiate::cswitch (loc loc1327) (test test1330) (clauses clauses1331)))
(define-inline (cswitch?::bool obj::obj) ((@ isa? __object) obj (@ cswitch cgen_cop)))
(define (cswitch-nil::cswitch) (class-nil (@ cswitch cgen_cop)))
(define-inline (cswitch-clauses::obj o::cswitch) (with-access::cswitch o (clauses) clauses))
(define-inline (cswitch-clauses-set! o::cswitch v::obj) (with-access::cswitch o (clauses) (set! clauses v)))
(define-inline (cswitch-test::cop o::cswitch) (with-access::cswitch o (test) test))
(define-inline (cswitch-test-set! o::cswitch v::cop) (with-access::cswitch o (test) (set! test v)))
(define-inline (cswitch-loc::obj o::cswitch) (with-access::cswitch o (loc) loc))
(define-inline (cswitch-loc-set! o::cswitch v::obj) (with-access::cswitch o (loc) (set! loc v)))

;; cmake-box
(define-inline (make-cmake-box::cmake-box loc1324::obj value1325::cop) (instantiate::cmake-box (loc loc1324) (value value1325)))
(define-inline (cmake-box?::bool obj::obj) ((@ isa? __object) obj (@ cmake-box cgen_cop)))
(define (cmake-box-nil::cmake-box) (class-nil (@ cmake-box cgen_cop)))
(define-inline (cmake-box-value::cop o::cmake-box) (with-access::cmake-box o (value) value))
(define-inline (cmake-box-value-set! o::cmake-box v::cop) (with-access::cmake-box o (value) (set! value v)))
(define-inline (cmake-box-loc::obj o::cmake-box) (with-access::cmake-box o (loc) loc))
(define-inline (cmake-box-loc-set! o::cmake-box v::obj) (with-access::cmake-box o (loc) (set! loc v)))

;; cbox-ref
(define-inline (make-cbox-ref::cbox-ref loc1321::obj var1322::cop) (instantiate::cbox-ref (loc loc1321) (var var1322)))
(define-inline (cbox-ref?::bool obj::obj) ((@ isa? __object) obj (@ cbox-ref cgen_cop)))
(define (cbox-ref-nil::cbox-ref) (class-nil (@ cbox-ref cgen_cop)))
(define-inline (cbox-ref-var::cop o::cbox-ref) (with-access::cbox-ref o (var) var))
(define-inline (cbox-ref-var-set! o::cbox-ref v::cop) (with-access::cbox-ref o (var) (set! var v)))
(define-inline (cbox-ref-loc::obj o::cbox-ref) (with-access::cbox-ref o (loc) loc))
(define-inline (cbox-ref-loc-set! o::cbox-ref v::obj) (with-access::cbox-ref o (loc) (set! loc v)))

;; cbox-set!
(define-inline (make-cbox-set!::cbox-set! loc1317::obj var1318::cop value1319::cop) (instantiate::cbox-set! (loc loc1317) (var var1318) (value value1319)))
(define-inline (cbox-set!?::bool obj::obj) ((@ isa? __object) obj (@ cbox-set! cgen_cop)))
(define (cbox-set!-nil::cbox-set!) (class-nil (@ cbox-set! cgen_cop)))
(define-inline (cbox-set!-value::cop o::cbox-set!) (with-access::cbox-set! o (value) value))
(define-inline (cbox-set!-value-set! o::cbox-set! v::cop) (with-access::cbox-set! o (value) (set! value v)))
(define-inline (cbox-set!-var::cop o::cbox-set!) (with-access::cbox-set! o (var) var))
(define-inline (cbox-set!-var-set! o::cbox-set! v::cop) (with-access::cbox-set! o (var) (set! var v)))
(define-inline (cbox-set!-loc::obj o::cbox-set!) (with-access::cbox-set! o (loc) loc))
(define-inline (cbox-set!-loc-set! o::cbox-set! v::obj) (with-access::cbox-set! o (loc) (set! loc v)))

;; cset-ex-it
(define-inline (make-cset-ex-it::cset-ex-it loc1312::obj exit1313::cop jump-value1314::cop body1315::cop) (instantiate::cset-ex-it (loc loc1312) (exit exit1313) (jump-value jump-value1314) (body body1315)))
(define-inline (cset-ex-it?::bool obj::obj) ((@ isa? __object) obj (@ cset-ex-it cgen_cop)))
(define (cset-ex-it-nil::cset-ex-it) (class-nil (@ cset-ex-it cgen_cop)))
(define-inline (cset-ex-it-body::cop o::cset-ex-it) (with-access::cset-ex-it o (body) body))
(define-inline (cset-ex-it-body-set! o::cset-ex-it v::cop) (with-access::cset-ex-it o (body) (set! body v)))
(define-inline (cset-ex-it-jump-value::cop o::cset-ex-it) (with-access::cset-ex-it o (jump-value) jump-value))
(define-inline (cset-ex-it-jump-value-set! o::cset-ex-it v::cop) (with-access::cset-ex-it o (jump-value) (set! jump-value v)))
(define-inline (cset-ex-it-exit::cop o::cset-ex-it) (with-access::cset-ex-it o (exit) exit))
(define-inline (cset-ex-it-exit-set! o::cset-ex-it v::cop) (with-access::cset-ex-it o (exit) (set! exit v)))
(define-inline (cset-ex-it-loc::obj o::cset-ex-it) (with-access::cset-ex-it o (loc) loc))
(define-inline (cset-ex-it-loc-set! o::cset-ex-it v::obj) (with-access::cset-ex-it o (loc) (set! loc v)))

;; cjump-ex-it
(define-inline (make-cjump-ex-it::cjump-ex-it loc1308::obj exit1309::cop value1310::cop) (instantiate::cjump-ex-it (loc loc1308) (exit exit1309) (value value1310)))
(define-inline (cjump-ex-it?::bool obj::obj) ((@ isa? __object) obj (@ cjump-ex-it cgen_cop)))
(define (cjump-ex-it-nil::cjump-ex-it) (class-nil (@ cjump-ex-it cgen_cop)))
(define-inline (cjump-ex-it-value::cop o::cjump-ex-it) (with-access::cjump-ex-it o (value) value))
(define-inline (cjump-ex-it-value-set! o::cjump-ex-it v::cop) (with-access::cjump-ex-it o (value) (set! value v)))
(define-inline (cjump-ex-it-exit::cop o::cjump-ex-it) (with-access::cjump-ex-it o (exit) exit))
(define-inline (cjump-ex-it-exit-set! o::cjump-ex-it v::cop) (with-access::cjump-ex-it o (exit) (set! exit v)))
(define-inline (cjump-ex-it-loc::obj o::cjump-ex-it) (with-access::cjump-ex-it o (loc) loc))
(define-inline (cjump-ex-it-loc-set! o::cjump-ex-it v::obj) (with-access::cjump-ex-it o (loc) (set! loc v)))

;; sfun/C
(define-inline (make-sfun/C::sfun/C arity1287::long side-effect1288::obj predicate-of1289::obj stack-allocator1290::obj top?1291::bool the-closure1292::obj effect1293::obj property1294::obj args1295::obj args-name1296::obj body1297::obj class1298::obj dsssl-keywords1299::obj loc1300::obj optionals1301::obj keys1302::obj the-closure-global1303::obj strength1304::symbol label1305::clabel integrated1306::bool) (instantiate::sfun/C (arity arity1287) (side-effect side-effect1288) (predicate-of predicate-of1289) (stack-allocator stack-allocator1290) (top? top?1291) (the-closure the-closure1292) (effect effect1293) (property property1294) (args args1295) (args-name args-name1296) (body body1297) (class class1298) (dsssl-keywords dsssl-keywords1299) (loc loc1300) (optionals optionals1301) (keys keys1302) (the-closure-global the-closure-global1303) (strength strength1304) (label label1305) (integrated integrated1306)))
(define-inline (sfun/C?::bool obj::obj) ((@ isa? __object) obj (@ sfun/C cgen_cop)))
(define (sfun/C-nil::sfun/C) (class-nil (@ sfun/C cgen_cop)))
(define-inline (sfun/C-integrated::bool o::sfun/C) (with-access::sfun/C o (integrated) integrated))
(define-inline (sfun/C-integrated-set! o::sfun/C v::bool) (with-access::sfun/C o (integrated) (set! integrated v)))
(define-inline (sfun/C-label::clabel o::sfun/C) (with-access::sfun/C o (label) label))
(define-inline (sfun/C-label-set! o::sfun/C v::clabel) (with-access::sfun/C o (label) (set! label v)))
(define-inline (sfun/C-strength::symbol o::sfun/C) (with-access::sfun/C o (strength) strength))
(define-inline (sfun/C-strength-set! o::sfun/C v::symbol) (with-access::sfun/C o (strength) (set! strength v)))
(define-inline (sfun/C-the-closure-global::obj o::sfun/C) (with-access::sfun/C o (the-closure-global) the-closure-global))
(define-inline (sfun/C-the-closure-global-set! o::sfun/C v::obj) (with-access::sfun/C o (the-closure-global) (set! the-closure-global v)))
(define-inline (sfun/C-keys::obj o::sfun/C) (with-access::sfun/C o (keys) keys))
(define-inline (sfun/C-keys-set! o::sfun/C v::obj) (with-access::sfun/C o (keys) (set! keys v)))
(define-inline (sfun/C-optionals::obj o::sfun/C) (with-access::sfun/C o (optionals) optionals))
(define-inline (sfun/C-optionals-set! o::sfun/C v::obj) (with-access::sfun/C o (optionals) (set! optionals v)))
(define-inline (sfun/C-loc::obj o::sfun/C) (with-access::sfun/C o (loc) loc))
(define-inline (sfun/C-loc-set! o::sfun/C v::obj) (with-access::sfun/C o (loc) (set! loc v)))
(define-inline (sfun/C-dsssl-keywords::obj o::sfun/C) (with-access::sfun/C o (dsssl-keywords) dsssl-keywords))
(define-inline (sfun/C-dsssl-keywords-set! o::sfun/C v::obj) (with-access::sfun/C o (dsssl-keywords) (set! dsssl-keywords v)))
(define-inline (sfun/C-class::obj o::sfun/C) (with-access::sfun/C o (class) class))
(define-inline (sfun/C-class-set! o::sfun/C v::obj) (with-access::sfun/C o (class) (set! class v)))
(define-inline (sfun/C-body::obj o::sfun/C) (with-access::sfun/C o (body) body))
(define-inline (sfun/C-body-set! o::sfun/C v::obj) (with-access::sfun/C o (body) (set! body v)))
(define-inline (sfun/C-args-name::obj o::sfun/C) (with-access::sfun/C o (args-name) args-name))
(define-inline (sfun/C-args-name-set! o::sfun/C v::obj) (with-access::sfun/C o (args-name) (set! args-name v)))
(define-inline (sfun/C-args::obj o::sfun/C) (with-access::sfun/C o (args) args))
(define-inline (sfun/C-args-set! o::sfun/C v::obj) (with-access::sfun/C o (args) (set! args v)))
(define-inline (sfun/C-property::obj o::sfun/C) (with-access::sfun/C o (property) property))
(define-inline (sfun/C-property-set! o::sfun/C v::obj) (with-access::sfun/C o (property) (set! property v)))
(define-inline (sfun/C-effect::obj o::sfun/C) (with-access::sfun/C o (effect) effect))
(define-inline (sfun/C-effect-set! o::sfun/C v::obj) (with-access::sfun/C o (effect) (set! effect v)))
(define-inline (sfun/C-the-closure::obj o::sfun/C) (with-access::sfun/C o (the-closure) the-closure))
(define-inline (sfun/C-the-closure-set! o::sfun/C v::obj) (with-access::sfun/C o (the-closure) (set! the-closure v)))
(define-inline (sfun/C-top?::bool o::sfun/C) (with-access::sfun/C o (top?) top?))
(define-inline (sfun/C-top?-set! o::sfun/C v::bool) (with-access::sfun/C o (top?) (set! top? v)))
(define-inline (sfun/C-stack-allocator::obj o::sfun/C) (with-access::sfun/C o (stack-allocator) stack-allocator))
(define-inline (sfun/C-stack-allocator-set! o::sfun/C v::obj) (with-access::sfun/C o (stack-allocator) (set! stack-allocator v)))
(define-inline (sfun/C-predicate-of::obj o::sfun/C) (with-access::sfun/C o (predicate-of) predicate-of))
(define-inline (sfun/C-predicate-of-set! o::sfun/C v::obj) (with-access::sfun/C o (predicate-of) (set! predicate-of v)))
(define-inline (sfun/C-side-effect::obj o::sfun/C) (with-access::sfun/C o (side-effect) side-effect))
(define-inline (sfun/C-side-effect-set! o::sfun/C v::obj) (with-access::sfun/C o (side-effect) (set! side-effect v)))
(define-inline (sfun/C-arity::long o::sfun/C) (with-access::sfun/C o (arity) arity))
(define-inline (sfun/C-arity-set! o::sfun/C v::long) (with-access::sfun/C o (arity) (set! arity v)))

;; bdb-block
(define-inline (make-bdb-block::bdb-block loc1284::obj body1285::cop) (instantiate::bdb-block (loc loc1284) (body body1285)))
(define-inline (bdb-block?::bool obj::obj) ((@ isa? __object) obj (@ bdb-block cgen_cop)))
(define (bdb-block-nil::bdb-block) (class-nil (@ bdb-block cgen_cop)))
(define-inline (bdb-block-body::cop o::bdb-block) (with-access::bdb-block o (body) body))
(define-inline (bdb-block-body-set! o::bdb-block v::cop) (with-access::bdb-block o (body) (set! body v)))
(define-inline (bdb-block-loc::obj o::bdb-block) (with-access::bdb-block o (loc) loc))
(define-inline (bdb-block-loc-set! o::bdb-block v::obj) (with-access::bdb-block o (loc) (set! loc v)))
))
