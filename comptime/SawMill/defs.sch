;; ==========================================================
;; Class accessors
;; Bigloo (3.7b)
;; Inria -- Sophia Antipolis     Mon Nov 14 18:35:27 CET 2011 
;; (bigloo.new -classgen SawMill/defs.scm)
;; ==========================================================

;; The directives
(directives

;; rtl_reg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_reg::rtl_reg type1514::type var1515::obj onexpr?1516::obj name1517::obj key1518::obj hardware1519::obj)
    (inline rtl_reg?::bool ::obj)
    (rtl_reg-nil::rtl_reg)
    (inline rtl_reg-hardware::obj ::rtl_reg)
    (inline rtl_reg-key::obj ::rtl_reg)
    (inline rtl_reg-name::obj ::rtl_reg)
    (inline rtl_reg-onexpr?::obj ::rtl_reg)
    (inline rtl_reg-onexpr?-set! ::rtl_reg ::obj)
    (inline rtl_reg-var::obj ::rtl_reg)
    (inline rtl_reg-var-set! ::rtl_reg ::obj)
    (inline rtl_reg-type::type ::rtl_reg)
    (inline rtl_reg-type-set! ::rtl_reg ::type))))

;; rtl_fun
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_fun::rtl_fun loc1512::obj)
    (inline rtl_fun?::bool ::obj)
    (rtl_fun-nil::rtl_fun)
    (inline rtl_fun-loc::obj ::rtl_fun)
    (inline rtl_fun-loc-set! ::rtl_fun ::obj))))

;; rtl_last
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_last::rtl_last loc1510::obj)
    (inline rtl_last?::bool ::obj)
    (rtl_last-nil::rtl_last)
    (inline rtl_last-loc::obj ::rtl_last)
    (inline rtl_last-loc-set! ::rtl_last ::obj))))

;; rtl_return
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_return::rtl_return loc1507::obj type1508::type)
    (inline rtl_return?::bool ::obj)
    (rtl_return-nil::rtl_return)
    (inline rtl_return-type::type ::rtl_return)
    (inline rtl_return-type-set! ::rtl_return ::type)
    (inline rtl_return-loc::obj ::rtl_return)
    (inline rtl_return-loc-set! ::rtl_return ::obj))))

;; rtl_jumpexit
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_jumpexit::rtl_jumpexit loc1505::obj)
    (inline rtl_jumpexit?::bool ::obj)
    (rtl_jumpexit-nil::rtl_jumpexit)
    (inline rtl_jumpexit-loc::obj ::rtl_jumpexit)
    (inline rtl_jumpexit-loc-set! ::rtl_jumpexit ::obj))))

;; rtl_fail
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_fail::rtl_fail loc1503::obj)
    (inline rtl_fail?::bool ::obj)
    (rtl_fail-nil::rtl_fail)
    (inline rtl_fail-loc::obj ::rtl_fail)
    (inline rtl_fail-loc-set! ::rtl_fail ::obj))))

;; rtl_notseq
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_notseq::rtl_notseq loc1501::obj)
    (inline rtl_notseq?::bool ::obj)
    (rtl_notseq-nil::rtl_notseq)
    (inline rtl_notseq-loc::obj ::rtl_notseq)
    (inline rtl_notseq-loc-set! ::rtl_notseq ::obj))))

;; rtl_if
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_if::rtl_if loc1499::obj)
    (inline rtl_if?::bool ::obj)
    (rtl_if-nil::rtl_if)
    (inline rtl_if-loc::obj ::rtl_if)
    (inline rtl_if-loc-set! ::rtl_if ::obj))))

;; rtl_select
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_select::rtl_select loc1495::obj type1496::type patterns1497::obj)
    (inline rtl_select?::bool ::obj)
    (rtl_select-nil::rtl_select)
    (inline rtl_select-patterns::obj ::rtl_select)
    (inline rtl_select-patterns-set! ::rtl_select ::obj)
    (inline rtl_select-type::type ::rtl_select)
    (inline rtl_select-type-set! ::rtl_select ::type)
    (inline rtl_select-loc::obj ::rtl_select)
    (inline rtl_select-loc-set! ::rtl_select ::obj))))

;; rtl_switch
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_switch::rtl_switch loc1490::obj type1491::type patterns1492::obj labels1493::obj)
    (inline rtl_switch?::bool ::obj)
    (rtl_switch-nil::rtl_switch)
    (inline rtl_switch-labels::obj ::rtl_switch)
    (inline rtl_switch-labels-set! ::rtl_switch ::obj)
    (inline rtl_switch-patterns::obj ::rtl_switch)
    (inline rtl_switch-patterns-set! ::rtl_switch ::obj)
    (inline rtl_switch-type::type ::rtl_switch)
    (inline rtl_switch-type-set! ::rtl_switch ::type)
    (inline rtl_switch-loc::obj ::rtl_switch)
    (inline rtl_switch-loc-set! ::rtl_switch ::obj))))

;; rtl_ifeq
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_ifeq::rtl_ifeq loc1487::obj then1488::block)
    (inline rtl_ifeq?::bool ::obj)
    (rtl_ifeq-nil::rtl_ifeq)
    (inline rtl_ifeq-then::block ::rtl_ifeq)
    (inline rtl_ifeq-then-set! ::rtl_ifeq ::block)
    (inline rtl_ifeq-loc::obj ::rtl_ifeq)
    (inline rtl_ifeq-loc-set! ::rtl_ifeq ::obj))))

;; rtl_ifne
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_ifne::rtl_ifne loc1484::obj then1485::block)
    (inline rtl_ifne?::bool ::obj)
    (rtl_ifne-nil::rtl_ifne)
    (inline rtl_ifne-then::block ::rtl_ifne)
    (inline rtl_ifne-then-set! ::rtl_ifne ::block)
    (inline rtl_ifne-loc::obj ::rtl_ifne)
    (inline rtl_ifne-loc-set! ::rtl_ifne ::obj))))

;; rtl_go
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_go::rtl_go loc1480::obj to1481::block)
    (inline rtl_go?::bool ::obj)
    (rtl_go-nil::rtl_go)
    (inline rtl_go-to::block ::rtl_go)
    (inline rtl_go-to-set! ::rtl_go ::block)
    (inline rtl_go-loc::obj ::rtl_go)
    (inline rtl_go-loc-set! ::rtl_go ::obj))))

;; rtl_pure
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_pure::rtl_pure loc1478::obj)
    (inline rtl_pure?::bool ::obj)
    (rtl_pure-nil::rtl_pure)
    (inline rtl_pure-loc::obj ::rtl_pure)
    (inline rtl_pure-loc-set! ::rtl_pure ::obj))))

;; rtl_nop
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_nop::rtl_nop loc1475::obj)
    (inline rtl_nop?::bool ::obj)
    (rtl_nop-nil::rtl_nop)
    (inline rtl_nop-loc::obj ::rtl_nop)
    (inline rtl_nop-loc-set! ::rtl_nop ::obj))))

;; rtl_mov
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_mov::rtl_mov loc1473::obj)
    (inline rtl_mov?::bool ::obj)
    (rtl_mov-nil::rtl_mov)
    (inline rtl_mov-loc::obj ::rtl_mov)
    (inline rtl_mov-loc-set! ::rtl_mov ::obj))))

;; rtl_loadi
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_loadi::rtl_loadi loc1470::obj constant1471::atom)
    (inline rtl_loadi?::bool ::obj)
    (rtl_loadi-nil::rtl_loadi)
    (inline rtl_loadi-constant::atom ::rtl_loadi)
    (inline rtl_loadi-constant-set! ::rtl_loadi ::atom)
    (inline rtl_loadi-loc::obj ::rtl_loadi)
    (inline rtl_loadi-loc-set! ::rtl_loadi ::obj))))

;; rtl_loadg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_loadg::rtl_loadg loc1466::obj var1467::global)
    (inline rtl_loadg?::bool ::obj)
    (rtl_loadg-nil::rtl_loadg)
    (inline rtl_loadg-var::global ::rtl_loadg)
    (inline rtl_loadg-var-set! ::rtl_loadg ::global)
    (inline rtl_loadg-loc::obj ::rtl_loadg)
    (inline rtl_loadg-loc-set! ::rtl_loadg ::obj))))

;; rtl_loadfun
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_loadfun::rtl_loadfun loc1463::obj var1464::global)
    (inline rtl_loadfun?::bool ::obj)
    (rtl_loadfun-nil::rtl_loadfun)
    (inline rtl_loadfun-var::global ::rtl_loadfun)
    (inline rtl_loadfun-var-set! ::rtl_loadfun ::global)
    (inline rtl_loadfun-loc::obj ::rtl_loadfun)
    (inline rtl_loadfun-loc-set! ::rtl_loadfun ::obj))))

;; rtl_globalref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_globalref::rtl_globalref loc1460::obj var1461::global)
    (inline rtl_globalref?::bool ::obj)
    (rtl_globalref-nil::rtl_globalref)
    (inline rtl_globalref-var::global ::rtl_globalref)
    (inline rtl_globalref-var-set! ::rtl_globalref ::global)
    (inline rtl_globalref-loc::obj ::rtl_globalref)
    (inline rtl_globalref-loc-set! ::rtl_globalref ::obj))))

;; rtl_getfield
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_getfield::rtl_getfield loc1455::obj name1456::bstring objtype1457::type type1458::type)
    (inline rtl_getfield?::bool ::obj)
    (rtl_getfield-nil::rtl_getfield)
    (inline rtl_getfield-type::type ::rtl_getfield)
    (inline rtl_getfield-type-set! ::rtl_getfield ::type)
    (inline rtl_getfield-objtype::type ::rtl_getfield)
    (inline rtl_getfield-objtype-set! ::rtl_getfield ::type)
    (inline rtl_getfield-name::bstring ::rtl_getfield)
    (inline rtl_getfield-name-set! ::rtl_getfield ::bstring)
    (inline rtl_getfield-loc::obj ::rtl_getfield)
    (inline rtl_getfield-loc-set! ::rtl_getfield ::obj))))

;; rtl_valloc
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_valloc::rtl_valloc loc1450::obj type1451::type vtype1452::type)
    (inline rtl_valloc?::bool ::obj)
    (rtl_valloc-nil::rtl_valloc)
    (inline rtl_valloc-vtype::type ::rtl_valloc)
    (inline rtl_valloc-vtype-set! ::rtl_valloc ::type)
    (inline rtl_valloc-type::type ::rtl_valloc)
    (inline rtl_valloc-type-set! ::rtl_valloc ::type)
    (inline rtl_valloc-loc::obj ::rtl_valloc)
    (inline rtl_valloc-loc-set! ::rtl_valloc ::obj))))

;; rtl_vref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_vref::rtl_vref loc1445::obj type1446::type vtype1447::type)
    (inline rtl_vref?::bool ::obj)
    (rtl_vref-nil::rtl_vref)
    (inline rtl_vref-vtype::type ::rtl_vref)
    (inline rtl_vref-vtype-set! ::rtl_vref ::type)
    (inline rtl_vref-type::type ::rtl_vref)
    (inline rtl_vref-type-set! ::rtl_vref ::type)
    (inline rtl_vref-loc::obj ::rtl_vref)
    (inline rtl_vref-loc-set! ::rtl_vref ::obj))))

;; rtl_vlength
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_vlength::rtl_vlength loc1442::obj type1443::type)
    (inline rtl_vlength?::bool ::obj)
    (rtl_vlength-nil::rtl_vlength)
    (inline rtl_vlength-type::type ::rtl_vlength)
    (inline rtl_vlength-type-set! ::rtl_vlength ::type)
    (inline rtl_vlength-loc::obj ::rtl_vlength)
    (inline rtl_vlength-loc-set! ::rtl_vlength ::obj))))

;; rtl_instanceof
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_instanceof::rtl_instanceof loc1439::obj type1440::type)
    (inline rtl_instanceof?::bool ::obj)
    (rtl_instanceof-nil::rtl_instanceof)
    (inline rtl_instanceof-type::type ::rtl_instanceof)
    (inline rtl_instanceof-type-set! ::rtl_instanceof ::type)
    (inline rtl_instanceof-loc::obj ::rtl_instanceof)
    (inline rtl_instanceof-loc-set! ::rtl_instanceof ::obj))))

;; rtl_makebox
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_makebox::rtl_makebox loc1437::obj)
    (inline rtl_makebox?::bool ::obj)
    (rtl_makebox-nil::rtl_makebox)
    (inline rtl_makebox-loc::obj ::rtl_makebox)
    (inline rtl_makebox-loc-set! ::rtl_makebox ::obj))))

;; rtl_boxref
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_boxref::rtl_boxref loc1435::obj)
    (inline rtl_boxref?::bool ::obj)
    (rtl_boxref-nil::rtl_boxref)
    (inline rtl_boxref-loc::obj ::rtl_boxref)
    (inline rtl_boxref-loc-set! ::rtl_boxref ::obj))))

;; rtl_effect
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_effect::rtl_effect loc1433::obj)
    (inline rtl_effect?::bool ::obj)
    (rtl_effect-nil::rtl_effect)
    (inline rtl_effect-loc::obj ::rtl_effect)
    (inline rtl_effect-loc-set! ::rtl_effect ::obj))))

;; rtl_storeg
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_storeg::rtl_storeg loc1428::obj var1430::global)
    (inline rtl_storeg?::bool ::obj)
    (rtl_storeg-nil::rtl_storeg)
    (inline rtl_storeg-var::global ::rtl_storeg)
    (inline rtl_storeg-var-set! ::rtl_storeg ::global)
    (inline rtl_storeg-loc::obj ::rtl_storeg)
    (inline rtl_storeg-loc-set! ::rtl_storeg ::obj))))

;; rtl_setfield
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_setfield::rtl_setfield loc1422::obj name1423::bstring objtype1424::type type1425::type)
    (inline rtl_setfield?::bool ::obj)
    (rtl_setfield-nil::rtl_setfield)
    (inline rtl_setfield-type::type ::rtl_setfield)
    (inline rtl_setfield-type-set! ::rtl_setfield ::type)
    (inline rtl_setfield-objtype::type ::rtl_setfield)
    (inline rtl_setfield-objtype-set! ::rtl_setfield ::type)
    (inline rtl_setfield-name::bstring ::rtl_setfield)
    (inline rtl_setfield-name-set! ::rtl_setfield ::bstring)
    (inline rtl_setfield-loc::obj ::rtl_setfield)
    (inline rtl_setfield-loc-set! ::rtl_setfield ::obj))))

;; rtl_vset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_vset::rtl_vset loc1418::obj type1419::type vtype1420::type)
    (inline rtl_vset?::bool ::obj)
    (rtl_vset-nil::rtl_vset)
    (inline rtl_vset-vtype::type ::rtl_vset)
    (inline rtl_vset-vtype-set! ::rtl_vset ::type)
    (inline rtl_vset-type::type ::rtl_vset)
    (inline rtl_vset-type-set! ::rtl_vset ::type)
    (inline rtl_vset-loc::obj ::rtl_vset)
    (inline rtl_vset-loc-set! ::rtl_vset ::obj))))

;; rtl_boxset
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_boxset::rtl_boxset loc1416::obj)
    (inline rtl_boxset?::bool ::obj)
    (rtl_boxset-nil::rtl_boxset)
    (inline rtl_boxset-loc::obj ::rtl_boxset)
    (inline rtl_boxset-loc-set! ::rtl_boxset ::obj))))

;; rtl_new
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_new::rtl_new loc1412::obj type1413::type constr1414::pair-nil)
    (inline rtl_new?::bool ::obj)
    (rtl_new-nil::rtl_new)
    (inline rtl_new-constr::pair-nil ::rtl_new)
    (inline rtl_new-constr-set! ::rtl_new ::pair-nil)
    (inline rtl_new-type::type ::rtl_new)
    (inline rtl_new-type-set! ::rtl_new ::type)
    (inline rtl_new-loc::obj ::rtl_new)
    (inline rtl_new-loc-set! ::rtl_new ::obj))))

;; rtl_call
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_call::rtl_call loc1409::obj var1410::global)
    (inline rtl_call?::bool ::obj)
    (rtl_call-nil::rtl_call)
    (inline rtl_call-var::global ::rtl_call)
    (inline rtl_call-var-set! ::rtl_call ::global)
    (inline rtl_call-loc::obj ::rtl_call)
    (inline rtl_call-loc-set! ::rtl_call ::obj))))

;; rtl_apply
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_apply::rtl_apply loc1407::obj)
    (inline rtl_apply?::bool ::obj)
    (rtl_apply-nil::rtl_apply)
    (inline rtl_apply-loc::obj ::rtl_apply)
    (inline rtl_apply-loc-set! ::rtl_apply ::obj))))

;; rtl_lightfuncall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_lightfuncall::rtl_lightfuncall loc1402::obj name1403::symbol funs1404::pair-nil rettype1405::obj)
    (inline rtl_lightfuncall?::bool ::obj)
    (rtl_lightfuncall-nil::rtl_lightfuncall)
    (inline rtl_lightfuncall-rettype::obj ::rtl_lightfuncall)
    (inline rtl_lightfuncall-rettype-set! ::rtl_lightfuncall ::obj)
    (inline rtl_lightfuncall-funs::pair-nil ::rtl_lightfuncall)
    (inline rtl_lightfuncall-funs-set! ::rtl_lightfuncall ::pair-nil)
    (inline rtl_lightfuncall-name::symbol ::rtl_lightfuncall)
    (inline rtl_lightfuncall-name-set! ::rtl_lightfuncall ::symbol)
    (inline rtl_lightfuncall-loc::obj ::rtl_lightfuncall)
    (inline rtl_lightfuncall-loc-set! ::rtl_lightfuncall ::obj))))

;; rtl_funcall
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_funcall::rtl_funcall loc1400::obj)
    (inline rtl_funcall?::bool ::obj)
    (rtl_funcall-nil::rtl_funcall)
    (inline rtl_funcall-loc::obj ::rtl_funcall)
    (inline rtl_funcall-loc-set! ::rtl_funcall ::obj))))

;; rtl_pragma
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_pragma::rtl_pragma loc1397::obj format1398::bstring)
    (inline rtl_pragma?::bool ::obj)
    (rtl_pragma-nil::rtl_pragma)
    (inline rtl_pragma-format::bstring ::rtl_pragma)
    (inline rtl_pragma-format-set! ::rtl_pragma ::bstring)
    (inline rtl_pragma-loc::obj ::rtl_pragma)
    (inline rtl_pragma-loc-set! ::rtl_pragma ::obj))))

;; rtl_cast
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_cast::rtl_cast loc1394::obj type1395::type)
    (inline rtl_cast?::bool ::obj)
    (rtl_cast-nil::rtl_cast)
    (inline rtl_cast-type::type ::rtl_cast)
    (inline rtl_cast-type-set! ::rtl_cast ::type)
    (inline rtl_cast-loc::obj ::rtl_cast)
    (inline rtl_cast-loc-set! ::rtl_cast ::obj))))

;; rtl_cast_null
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_cast_null::rtl_cast_null loc1391::obj type1392::type)
    (inline rtl_cast_null?::bool ::obj)
    (rtl_cast_null-nil::rtl_cast_null)
    (inline rtl_cast_null-type::type ::rtl_cast_null)
    (inline rtl_cast_null-type-set! ::rtl_cast_null ::type)
    (inline rtl_cast_null-loc::obj ::rtl_cast_null)
    (inline rtl_cast_null-loc-set! ::rtl_cast_null ::obj))))

;; rtl_protect
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_protect::rtl_protect loc1389::obj)
    (inline rtl_protect?::bool ::obj)
    (rtl_protect-nil::rtl_protect)
    (inline rtl_protect-loc::obj ::rtl_protect)
    (inline rtl_protect-loc-set! ::rtl_protect ::obj))))

;; rtl_protected
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_protected::rtl_protected loc1387::obj)
    (inline rtl_protected?::bool ::obj)
    (rtl_protected-nil::rtl_protected)
    (inline rtl_protected-loc::obj ::rtl_protected)
    (inline rtl_protected-loc-set! ::rtl_protected ::obj))))

;; rtl_ins
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-rtl_ins::rtl_ins loc1381::obj %spill1382::pair-nil dest1383::obj fun1384::rtl_fun args1385::pair-nil)
    (inline rtl_ins?::bool ::obj)
    (rtl_ins-nil::rtl_ins)
    (inline rtl_ins-args::pair-nil ::rtl_ins)
    (inline rtl_ins-args-set! ::rtl_ins ::pair-nil)
    (inline rtl_ins-fun::rtl_fun ::rtl_ins)
    (inline rtl_ins-fun-set! ::rtl_ins ::rtl_fun)
    (inline rtl_ins-dest::obj ::rtl_ins)
    (inline rtl_ins-dest-set! ::rtl_ins ::obj)
    (inline rtl_ins-%spill::pair-nil ::rtl_ins)
    (inline rtl_ins-%spill-set! ::rtl_ins ::pair-nil)
    (inline rtl_ins-loc::obj ::rtl_ins)
    (inline rtl_ins-loc-set! ::rtl_ins ::obj))))

;; block
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-block::block label1376::int preds1377::pair-nil succs1378::pair-nil first1379::pair)
    (inline block?::bool ::obj)
    (block-nil::block)
    (inline block-first::pair ::block)
    (inline block-first-set! ::block ::pair)
    (inline block-succs::pair-nil ::block)
    (inline block-succs-set! ::block ::pair-nil)
    (inline block-preds::pair-nil ::block)
    (inline block-preds-set! ::block ::pair-nil)
    (inline block-label::int ::block)
    (inline block-label-set! ::block ::int)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; rtl_reg
(define-inline (make-rtl_reg::rtl_reg type1514::type var1515::obj onexpr?1516::obj name1517::obj key1518::obj hardware1519::obj) (instantiate::rtl_reg (type type1514) (var var1515) (onexpr? onexpr?1516) (name name1517) (key key1518) (hardware hardware1519)))
(define-inline (rtl_reg?::bool obj::obj) ((@ isa? __object) obj (@ rtl_reg saw_defs)))
(define (rtl_reg-nil::rtl_reg) (class-nil (@ rtl_reg saw_defs)))
(define-inline (rtl_reg-hardware::obj o::rtl_reg) (with-access::rtl_reg o (hardware) hardware))
(define-inline (rtl_reg-hardware-set! o::rtl_reg v::obj) (with-access::rtl_reg o (hardware) (set! hardware v)))
(define-inline (rtl_reg-key::obj o::rtl_reg) (with-access::rtl_reg o (key) key))
(define-inline (rtl_reg-key-set! o::rtl_reg v::obj) (with-access::rtl_reg o (key) (set! key v)))
(define-inline (rtl_reg-name::obj o::rtl_reg) (with-access::rtl_reg o (name) name))
(define-inline (rtl_reg-name-set! o::rtl_reg v::obj) (with-access::rtl_reg o (name) (set! name v)))
(define-inline (rtl_reg-onexpr?::obj o::rtl_reg) (with-access::rtl_reg o (onexpr?) onexpr?))
(define-inline (rtl_reg-onexpr?-set! o::rtl_reg v::obj) (with-access::rtl_reg o (onexpr?) (set! onexpr? v)))
(define-inline (rtl_reg-var::obj o::rtl_reg) (with-access::rtl_reg o (var) var))
(define-inline (rtl_reg-var-set! o::rtl_reg v::obj) (with-access::rtl_reg o (var) (set! var v)))
(define-inline (rtl_reg-type::type o::rtl_reg) (with-access::rtl_reg o (type) type))
(define-inline (rtl_reg-type-set! o::rtl_reg v::type) (with-access::rtl_reg o (type) (set! type v)))

;; rtl_fun
(define-inline (make-rtl_fun::rtl_fun loc1512::obj) (instantiate::rtl_fun (loc loc1512)))
(define-inline (rtl_fun?::bool obj::obj) ((@ isa? __object) obj (@ rtl_fun saw_defs)))
(define (rtl_fun-nil::rtl_fun) (class-nil (@ rtl_fun saw_defs)))
(define-inline (rtl_fun-loc::obj o::rtl_fun) (with-access::rtl_fun o (loc) loc))
(define-inline (rtl_fun-loc-set! o::rtl_fun v::obj) (with-access::rtl_fun o (loc) (set! loc v)))

;; rtl_last
(define-inline (make-rtl_last::rtl_last loc1510::obj) (instantiate::rtl_last (loc loc1510)))
(define-inline (rtl_last?::bool obj::obj) ((@ isa? __object) obj (@ rtl_last saw_defs)))
(define (rtl_last-nil::rtl_last) (class-nil (@ rtl_last saw_defs)))
(define-inline (rtl_last-loc::obj o::rtl_last) (with-access::rtl_last o (loc) loc))
(define-inline (rtl_last-loc-set! o::rtl_last v::obj) (with-access::rtl_last o (loc) (set! loc v)))

;; rtl_return
(define-inline (make-rtl_return::rtl_return loc1507::obj type1508::type) (instantiate::rtl_return (loc loc1507) (type type1508)))
(define-inline (rtl_return?::bool obj::obj) ((@ isa? __object) obj (@ rtl_return saw_defs)))
(define (rtl_return-nil::rtl_return) (class-nil (@ rtl_return saw_defs)))
(define-inline (rtl_return-type::type o::rtl_return) (with-access::rtl_return o (type) type))
(define-inline (rtl_return-type-set! o::rtl_return v::type) (with-access::rtl_return o (type) (set! type v)))
(define-inline (rtl_return-loc::obj o::rtl_return) (with-access::rtl_return o (loc) loc))
(define-inline (rtl_return-loc-set! o::rtl_return v::obj) (with-access::rtl_return o (loc) (set! loc v)))

;; rtl_jumpexit
(define-inline (make-rtl_jumpexit::rtl_jumpexit loc1505::obj) (instantiate::rtl_jumpexit (loc loc1505)))
(define-inline (rtl_jumpexit?::bool obj::obj) ((@ isa? __object) obj (@ rtl_jumpexit saw_defs)))
(define (rtl_jumpexit-nil::rtl_jumpexit) (class-nil (@ rtl_jumpexit saw_defs)))
(define-inline (rtl_jumpexit-loc::obj o::rtl_jumpexit) (with-access::rtl_jumpexit o (loc) loc))
(define-inline (rtl_jumpexit-loc-set! o::rtl_jumpexit v::obj) (with-access::rtl_jumpexit o (loc) (set! loc v)))

;; rtl_fail
(define-inline (make-rtl_fail::rtl_fail loc1503::obj) (instantiate::rtl_fail (loc loc1503)))
(define-inline (rtl_fail?::bool obj::obj) ((@ isa? __object) obj (@ rtl_fail saw_defs)))
(define (rtl_fail-nil::rtl_fail) (class-nil (@ rtl_fail saw_defs)))
(define-inline (rtl_fail-loc::obj o::rtl_fail) (with-access::rtl_fail o (loc) loc))
(define-inline (rtl_fail-loc-set! o::rtl_fail v::obj) (with-access::rtl_fail o (loc) (set! loc v)))

;; rtl_notseq
(define-inline (make-rtl_notseq::rtl_notseq loc1501::obj) (instantiate::rtl_notseq (loc loc1501)))
(define-inline (rtl_notseq?::bool obj::obj) ((@ isa? __object) obj (@ rtl_notseq saw_defs)))
(define (rtl_notseq-nil::rtl_notseq) (class-nil (@ rtl_notseq saw_defs)))
(define-inline (rtl_notseq-loc::obj o::rtl_notseq) (with-access::rtl_notseq o (loc) loc))
(define-inline (rtl_notseq-loc-set! o::rtl_notseq v::obj) (with-access::rtl_notseq o (loc) (set! loc v)))

;; rtl_if
(define-inline (make-rtl_if::rtl_if loc1499::obj) (instantiate::rtl_if (loc loc1499)))
(define-inline (rtl_if?::bool obj::obj) ((@ isa? __object) obj (@ rtl_if saw_defs)))
(define (rtl_if-nil::rtl_if) (class-nil (@ rtl_if saw_defs)))
(define-inline (rtl_if-loc::obj o::rtl_if) (with-access::rtl_if o (loc) loc))
(define-inline (rtl_if-loc-set! o::rtl_if v::obj) (with-access::rtl_if o (loc) (set! loc v)))

;; rtl_select
(define-inline (make-rtl_select::rtl_select loc1495::obj type1496::type patterns1497::obj) (instantiate::rtl_select (loc loc1495) (type type1496) (patterns patterns1497)))
(define-inline (rtl_select?::bool obj::obj) ((@ isa? __object) obj (@ rtl_select saw_defs)))
(define (rtl_select-nil::rtl_select) (class-nil (@ rtl_select saw_defs)))
(define-inline (rtl_select-patterns::obj o::rtl_select) (with-access::rtl_select o (patterns) patterns))
(define-inline (rtl_select-patterns-set! o::rtl_select v::obj) (with-access::rtl_select o (patterns) (set! patterns v)))
(define-inline (rtl_select-type::type o::rtl_select) (with-access::rtl_select o (type) type))
(define-inline (rtl_select-type-set! o::rtl_select v::type) (with-access::rtl_select o (type) (set! type v)))
(define-inline (rtl_select-loc::obj o::rtl_select) (with-access::rtl_select o (loc) loc))
(define-inline (rtl_select-loc-set! o::rtl_select v::obj) (with-access::rtl_select o (loc) (set! loc v)))

;; rtl_switch
(define-inline (make-rtl_switch::rtl_switch loc1490::obj type1491::type patterns1492::obj labels1493::obj) (instantiate::rtl_switch (loc loc1490) (type type1491) (patterns patterns1492) (labels labels1493)))
(define-inline (rtl_switch?::bool obj::obj) ((@ isa? __object) obj (@ rtl_switch saw_defs)))
(define (rtl_switch-nil::rtl_switch) (class-nil (@ rtl_switch saw_defs)))
(define-inline (rtl_switch-labels::obj o::rtl_switch) (with-access::rtl_switch o (labels) labels))
(define-inline (rtl_switch-labels-set! o::rtl_switch v::obj) (with-access::rtl_switch o (labels) (set! labels v)))
(define-inline (rtl_switch-patterns::obj o::rtl_switch) (with-access::rtl_switch o (patterns) patterns))
(define-inline (rtl_switch-patterns-set! o::rtl_switch v::obj) (with-access::rtl_switch o (patterns) (set! patterns v)))
(define-inline (rtl_switch-type::type o::rtl_switch) (with-access::rtl_switch o (type) type))
(define-inline (rtl_switch-type-set! o::rtl_switch v::type) (with-access::rtl_switch o (type) (set! type v)))
(define-inline (rtl_switch-loc::obj o::rtl_switch) (with-access::rtl_switch o (loc) loc))
(define-inline (rtl_switch-loc-set! o::rtl_switch v::obj) (with-access::rtl_switch o (loc) (set! loc v)))

;; rtl_ifeq
(define-inline (make-rtl_ifeq::rtl_ifeq loc1487::obj then1488::block) (instantiate::rtl_ifeq (loc loc1487) (then then1488)))
(define-inline (rtl_ifeq?::bool obj::obj) ((@ isa? __object) obj (@ rtl_ifeq saw_defs)))
(define (rtl_ifeq-nil::rtl_ifeq) (class-nil (@ rtl_ifeq saw_defs)))
(define-inline (rtl_ifeq-then::block o::rtl_ifeq) (with-access::rtl_ifeq o (then) then))
(define-inline (rtl_ifeq-then-set! o::rtl_ifeq v::block) (with-access::rtl_ifeq o (then) (set! then v)))
(define-inline (rtl_ifeq-loc::obj o::rtl_ifeq) (with-access::rtl_ifeq o (loc) loc))
(define-inline (rtl_ifeq-loc-set! o::rtl_ifeq v::obj) (with-access::rtl_ifeq o (loc) (set! loc v)))

;; rtl_ifne
(define-inline (make-rtl_ifne::rtl_ifne loc1484::obj then1485::block) (instantiate::rtl_ifne (loc loc1484) (then then1485)))
(define-inline (rtl_ifne?::bool obj::obj) ((@ isa? __object) obj (@ rtl_ifne saw_defs)))
(define (rtl_ifne-nil::rtl_ifne) (class-nil (@ rtl_ifne saw_defs)))
(define-inline (rtl_ifne-then::block o::rtl_ifne) (with-access::rtl_ifne o (then) then))
(define-inline (rtl_ifne-then-set! o::rtl_ifne v::block) (with-access::rtl_ifne o (then) (set! then v)))
(define-inline (rtl_ifne-loc::obj o::rtl_ifne) (with-access::rtl_ifne o (loc) loc))
(define-inline (rtl_ifne-loc-set! o::rtl_ifne v::obj) (with-access::rtl_ifne o (loc) (set! loc v)))

;; rtl_go
(define-inline (make-rtl_go::rtl_go loc1480::obj to1481::block) (instantiate::rtl_go (loc loc1480) (to to1481)))
(define-inline (rtl_go?::bool obj::obj) ((@ isa? __object) obj (@ rtl_go saw_defs)))
(define (rtl_go-nil::rtl_go) (class-nil (@ rtl_go saw_defs)))
(define-inline (rtl_go-to::block o::rtl_go) (with-access::rtl_go o (to) to))
(define-inline (rtl_go-to-set! o::rtl_go v::block) (with-access::rtl_go o (to) (set! to v)))
(define-inline (rtl_go-loc::obj o::rtl_go) (with-access::rtl_go o (loc) loc))
(define-inline (rtl_go-loc-set! o::rtl_go v::obj) (with-access::rtl_go o (loc) (set! loc v)))

;; rtl_pure
(define-inline (make-rtl_pure::rtl_pure loc1478::obj) (instantiate::rtl_pure (loc loc1478)))
(define-inline (rtl_pure?::bool obj::obj) ((@ isa? __object) obj (@ rtl_pure saw_defs)))
(define (rtl_pure-nil::rtl_pure) (class-nil (@ rtl_pure saw_defs)))
(define-inline (rtl_pure-loc::obj o::rtl_pure) (with-access::rtl_pure o (loc) loc))
(define-inline (rtl_pure-loc-set! o::rtl_pure v::obj) (with-access::rtl_pure o (loc) (set! loc v)))

;; rtl_nop
(define-inline (make-rtl_nop::rtl_nop loc1475::obj) (instantiate::rtl_nop (loc loc1475)))
(define-inline (rtl_nop?::bool obj::obj) ((@ isa? __object) obj (@ rtl_nop saw_defs)))
(define (rtl_nop-nil::rtl_nop) (class-nil (@ rtl_nop saw_defs)))
(define-inline (rtl_nop-loc::obj o::rtl_nop) (with-access::rtl_nop o (loc) loc))
(define-inline (rtl_nop-loc-set! o::rtl_nop v::obj) (with-access::rtl_nop o (loc) (set! loc v)))

;; rtl_mov
(define-inline (make-rtl_mov::rtl_mov loc1473::obj) (instantiate::rtl_mov (loc loc1473)))
(define-inline (rtl_mov?::bool obj::obj) ((@ isa? __object) obj (@ rtl_mov saw_defs)))
(define (rtl_mov-nil::rtl_mov) (class-nil (@ rtl_mov saw_defs)))
(define-inline (rtl_mov-loc::obj o::rtl_mov) (with-access::rtl_mov o (loc) loc))
(define-inline (rtl_mov-loc-set! o::rtl_mov v::obj) (with-access::rtl_mov o (loc) (set! loc v)))

;; rtl_loadi
(define-inline (make-rtl_loadi::rtl_loadi loc1470::obj constant1471::atom) (instantiate::rtl_loadi (loc loc1470) (constant constant1471)))
(define-inline (rtl_loadi?::bool obj::obj) ((@ isa? __object) obj (@ rtl_loadi saw_defs)))
(define (rtl_loadi-nil::rtl_loadi) (class-nil (@ rtl_loadi saw_defs)))
(define-inline (rtl_loadi-constant::atom o::rtl_loadi) (with-access::rtl_loadi o (constant) constant))
(define-inline (rtl_loadi-constant-set! o::rtl_loadi v::atom) (with-access::rtl_loadi o (constant) (set! constant v)))
(define-inline (rtl_loadi-loc::obj o::rtl_loadi) (with-access::rtl_loadi o (loc) loc))
(define-inline (rtl_loadi-loc-set! o::rtl_loadi v::obj) (with-access::rtl_loadi o (loc) (set! loc v)))

;; rtl_loadg
(define-inline (make-rtl_loadg::rtl_loadg loc1466::obj var1467::global) (instantiate::rtl_loadg (loc loc1466) (var var1467)))
(define-inline (rtl_loadg?::bool obj::obj) ((@ isa? __object) obj (@ rtl_loadg saw_defs)))
(define (rtl_loadg-nil::rtl_loadg) (class-nil (@ rtl_loadg saw_defs)))
(define-inline (rtl_loadg-var::global o::rtl_loadg) (with-access::rtl_loadg o (var) var))
(define-inline (rtl_loadg-var-set! o::rtl_loadg v::global) (with-access::rtl_loadg o (var) (set! var v)))
(define-inline (rtl_loadg-loc::obj o::rtl_loadg) (with-access::rtl_loadg o (loc) loc))
(define-inline (rtl_loadg-loc-set! o::rtl_loadg v::obj) (with-access::rtl_loadg o (loc) (set! loc v)))

;; rtl_loadfun
(define-inline (make-rtl_loadfun::rtl_loadfun loc1463::obj var1464::global) (instantiate::rtl_loadfun (loc loc1463) (var var1464)))
(define-inline (rtl_loadfun?::bool obj::obj) ((@ isa? __object) obj (@ rtl_loadfun saw_defs)))
(define (rtl_loadfun-nil::rtl_loadfun) (class-nil (@ rtl_loadfun saw_defs)))
(define-inline (rtl_loadfun-var::global o::rtl_loadfun) (with-access::rtl_loadfun o (var) var))
(define-inline (rtl_loadfun-var-set! o::rtl_loadfun v::global) (with-access::rtl_loadfun o (var) (set! var v)))
(define-inline (rtl_loadfun-loc::obj o::rtl_loadfun) (with-access::rtl_loadfun o (loc) loc))
(define-inline (rtl_loadfun-loc-set! o::rtl_loadfun v::obj) (with-access::rtl_loadfun o (loc) (set! loc v)))

;; rtl_globalref
(define-inline (make-rtl_globalref::rtl_globalref loc1460::obj var1461::global) (instantiate::rtl_globalref (loc loc1460) (var var1461)))
(define-inline (rtl_globalref?::bool obj::obj) ((@ isa? __object) obj (@ rtl_globalref saw_defs)))
(define (rtl_globalref-nil::rtl_globalref) (class-nil (@ rtl_globalref saw_defs)))
(define-inline (rtl_globalref-var::global o::rtl_globalref) (with-access::rtl_globalref o (var) var))
(define-inline (rtl_globalref-var-set! o::rtl_globalref v::global) (with-access::rtl_globalref o (var) (set! var v)))
(define-inline (rtl_globalref-loc::obj o::rtl_globalref) (with-access::rtl_globalref o (loc) loc))
(define-inline (rtl_globalref-loc-set! o::rtl_globalref v::obj) (with-access::rtl_globalref o (loc) (set! loc v)))

;; rtl_getfield
(define-inline (make-rtl_getfield::rtl_getfield loc1455::obj name1456::bstring objtype1457::type type1458::type) (instantiate::rtl_getfield (loc loc1455) (name name1456) (objtype objtype1457) (type type1458)))
(define-inline (rtl_getfield?::bool obj::obj) ((@ isa? __object) obj (@ rtl_getfield saw_defs)))
(define (rtl_getfield-nil::rtl_getfield) (class-nil (@ rtl_getfield saw_defs)))
(define-inline (rtl_getfield-type::type o::rtl_getfield) (with-access::rtl_getfield o (type) type))
(define-inline (rtl_getfield-type-set! o::rtl_getfield v::type) (with-access::rtl_getfield o (type) (set! type v)))
(define-inline (rtl_getfield-objtype::type o::rtl_getfield) (with-access::rtl_getfield o (objtype) objtype))
(define-inline (rtl_getfield-objtype-set! o::rtl_getfield v::type) (with-access::rtl_getfield o (objtype) (set! objtype v)))
(define-inline (rtl_getfield-name::bstring o::rtl_getfield) (with-access::rtl_getfield o (name) name))
(define-inline (rtl_getfield-name-set! o::rtl_getfield v::bstring) (with-access::rtl_getfield o (name) (set! name v)))
(define-inline (rtl_getfield-loc::obj o::rtl_getfield) (with-access::rtl_getfield o (loc) loc))
(define-inline (rtl_getfield-loc-set! o::rtl_getfield v::obj) (with-access::rtl_getfield o (loc) (set! loc v)))

;; rtl_valloc
(define-inline (make-rtl_valloc::rtl_valloc loc1450::obj type1451::type vtype1452::type) (instantiate::rtl_valloc (loc loc1450) (type type1451) (vtype vtype1452)))
(define-inline (rtl_valloc?::bool obj::obj) ((@ isa? __object) obj (@ rtl_valloc saw_defs)))
(define (rtl_valloc-nil::rtl_valloc) (class-nil (@ rtl_valloc saw_defs)))
(define-inline (rtl_valloc-vtype::type o::rtl_valloc) (with-access::rtl_valloc o (vtype) vtype))
(define-inline (rtl_valloc-vtype-set! o::rtl_valloc v::type) (with-access::rtl_valloc o (vtype) (set! vtype v)))
(define-inline (rtl_valloc-type::type o::rtl_valloc) (with-access::rtl_valloc o (type) type))
(define-inline (rtl_valloc-type-set! o::rtl_valloc v::type) (with-access::rtl_valloc o (type) (set! type v)))
(define-inline (rtl_valloc-loc::obj o::rtl_valloc) (with-access::rtl_valloc o (loc) loc))
(define-inline (rtl_valloc-loc-set! o::rtl_valloc v::obj) (with-access::rtl_valloc o (loc) (set! loc v)))

;; rtl_vref
(define-inline (make-rtl_vref::rtl_vref loc1445::obj type1446::type vtype1447::type) (instantiate::rtl_vref (loc loc1445) (type type1446) (vtype vtype1447)))
(define-inline (rtl_vref?::bool obj::obj) ((@ isa? __object) obj (@ rtl_vref saw_defs)))
(define (rtl_vref-nil::rtl_vref) (class-nil (@ rtl_vref saw_defs)))
(define-inline (rtl_vref-vtype::type o::rtl_vref) (with-access::rtl_vref o (vtype) vtype))
(define-inline (rtl_vref-vtype-set! o::rtl_vref v::type) (with-access::rtl_vref o (vtype) (set! vtype v)))
(define-inline (rtl_vref-type::type o::rtl_vref) (with-access::rtl_vref o (type) type))
(define-inline (rtl_vref-type-set! o::rtl_vref v::type) (with-access::rtl_vref o (type) (set! type v)))
(define-inline (rtl_vref-loc::obj o::rtl_vref) (with-access::rtl_vref o (loc) loc))
(define-inline (rtl_vref-loc-set! o::rtl_vref v::obj) (with-access::rtl_vref o (loc) (set! loc v)))

;; rtl_vlength
(define-inline (make-rtl_vlength::rtl_vlength loc1442::obj type1443::type) (instantiate::rtl_vlength (loc loc1442) (type type1443)))
(define-inline (rtl_vlength?::bool obj::obj) ((@ isa? __object) obj (@ rtl_vlength saw_defs)))
(define (rtl_vlength-nil::rtl_vlength) (class-nil (@ rtl_vlength saw_defs)))
(define-inline (rtl_vlength-type::type o::rtl_vlength) (with-access::rtl_vlength o (type) type))
(define-inline (rtl_vlength-type-set! o::rtl_vlength v::type) (with-access::rtl_vlength o (type) (set! type v)))
(define-inline (rtl_vlength-loc::obj o::rtl_vlength) (with-access::rtl_vlength o (loc) loc))
(define-inline (rtl_vlength-loc-set! o::rtl_vlength v::obj) (with-access::rtl_vlength o (loc) (set! loc v)))

;; rtl_instanceof
(define-inline (make-rtl_instanceof::rtl_instanceof loc1439::obj type1440::type) (instantiate::rtl_instanceof (loc loc1439) (type type1440)))
(define-inline (rtl_instanceof?::bool obj::obj) ((@ isa? __object) obj (@ rtl_instanceof saw_defs)))
(define (rtl_instanceof-nil::rtl_instanceof) (class-nil (@ rtl_instanceof saw_defs)))
(define-inline (rtl_instanceof-type::type o::rtl_instanceof) (with-access::rtl_instanceof o (type) type))
(define-inline (rtl_instanceof-type-set! o::rtl_instanceof v::type) (with-access::rtl_instanceof o (type) (set! type v)))
(define-inline (rtl_instanceof-loc::obj o::rtl_instanceof) (with-access::rtl_instanceof o (loc) loc))
(define-inline (rtl_instanceof-loc-set! o::rtl_instanceof v::obj) (with-access::rtl_instanceof o (loc) (set! loc v)))

;; rtl_makebox
(define-inline (make-rtl_makebox::rtl_makebox loc1437::obj) (instantiate::rtl_makebox (loc loc1437)))
(define-inline (rtl_makebox?::bool obj::obj) ((@ isa? __object) obj (@ rtl_makebox saw_defs)))
(define (rtl_makebox-nil::rtl_makebox) (class-nil (@ rtl_makebox saw_defs)))
(define-inline (rtl_makebox-loc::obj o::rtl_makebox) (with-access::rtl_makebox o (loc) loc))
(define-inline (rtl_makebox-loc-set! o::rtl_makebox v::obj) (with-access::rtl_makebox o (loc) (set! loc v)))

;; rtl_boxref
(define-inline (make-rtl_boxref::rtl_boxref loc1435::obj) (instantiate::rtl_boxref (loc loc1435)))
(define-inline (rtl_boxref?::bool obj::obj) ((@ isa? __object) obj (@ rtl_boxref saw_defs)))
(define (rtl_boxref-nil::rtl_boxref) (class-nil (@ rtl_boxref saw_defs)))
(define-inline (rtl_boxref-loc::obj o::rtl_boxref) (with-access::rtl_boxref o (loc) loc))
(define-inline (rtl_boxref-loc-set! o::rtl_boxref v::obj) (with-access::rtl_boxref o (loc) (set! loc v)))

;; rtl_effect
(define-inline (make-rtl_effect::rtl_effect loc1433::obj) (instantiate::rtl_effect (loc loc1433)))
(define-inline (rtl_effect?::bool obj::obj) ((@ isa? __object) obj (@ rtl_effect saw_defs)))
(define (rtl_effect-nil::rtl_effect) (class-nil (@ rtl_effect saw_defs)))
(define-inline (rtl_effect-loc::obj o::rtl_effect) (with-access::rtl_effect o (loc) loc))
(define-inline (rtl_effect-loc-set! o::rtl_effect v::obj) (with-access::rtl_effect o (loc) (set! loc v)))

;; rtl_storeg
(define-inline (make-rtl_storeg::rtl_storeg loc1428::obj var1430::global) (instantiate::rtl_storeg (loc loc1428) (var var1430)))
(define-inline (rtl_storeg?::bool obj::obj) ((@ isa? __object) obj (@ rtl_storeg saw_defs)))
(define (rtl_storeg-nil::rtl_storeg) (class-nil (@ rtl_storeg saw_defs)))
(define-inline (rtl_storeg-var::global o::rtl_storeg) (with-access::rtl_storeg o (var) var))
(define-inline (rtl_storeg-var-set! o::rtl_storeg v::global) (with-access::rtl_storeg o (var) (set! var v)))
(define-inline (rtl_storeg-loc::obj o::rtl_storeg) (with-access::rtl_storeg o (loc) loc))
(define-inline (rtl_storeg-loc-set! o::rtl_storeg v::obj) (with-access::rtl_storeg o (loc) (set! loc v)))

;; rtl_setfield
(define-inline (make-rtl_setfield::rtl_setfield loc1422::obj name1423::bstring objtype1424::type type1425::type) (instantiate::rtl_setfield (loc loc1422) (name name1423) (objtype objtype1424) (type type1425)))
(define-inline (rtl_setfield?::bool obj::obj) ((@ isa? __object) obj (@ rtl_setfield saw_defs)))
(define (rtl_setfield-nil::rtl_setfield) (class-nil (@ rtl_setfield saw_defs)))
(define-inline (rtl_setfield-type::type o::rtl_setfield) (with-access::rtl_setfield o (type) type))
(define-inline (rtl_setfield-type-set! o::rtl_setfield v::type) (with-access::rtl_setfield o (type) (set! type v)))
(define-inline (rtl_setfield-objtype::type o::rtl_setfield) (with-access::rtl_setfield o (objtype) objtype))
(define-inline (rtl_setfield-objtype-set! o::rtl_setfield v::type) (with-access::rtl_setfield o (objtype) (set! objtype v)))
(define-inline (rtl_setfield-name::bstring o::rtl_setfield) (with-access::rtl_setfield o (name) name))
(define-inline (rtl_setfield-name-set! o::rtl_setfield v::bstring) (with-access::rtl_setfield o (name) (set! name v)))
(define-inline (rtl_setfield-loc::obj o::rtl_setfield) (with-access::rtl_setfield o (loc) loc))
(define-inline (rtl_setfield-loc-set! o::rtl_setfield v::obj) (with-access::rtl_setfield o (loc) (set! loc v)))

;; rtl_vset
(define-inline (make-rtl_vset::rtl_vset loc1418::obj type1419::type vtype1420::type) (instantiate::rtl_vset (loc loc1418) (type type1419) (vtype vtype1420)))
(define-inline (rtl_vset?::bool obj::obj) ((@ isa? __object) obj (@ rtl_vset saw_defs)))
(define (rtl_vset-nil::rtl_vset) (class-nil (@ rtl_vset saw_defs)))
(define-inline (rtl_vset-vtype::type o::rtl_vset) (with-access::rtl_vset o (vtype) vtype))
(define-inline (rtl_vset-vtype-set! o::rtl_vset v::type) (with-access::rtl_vset o (vtype) (set! vtype v)))
(define-inline (rtl_vset-type::type o::rtl_vset) (with-access::rtl_vset o (type) type))
(define-inline (rtl_vset-type-set! o::rtl_vset v::type) (with-access::rtl_vset o (type) (set! type v)))
(define-inline (rtl_vset-loc::obj o::rtl_vset) (with-access::rtl_vset o (loc) loc))
(define-inline (rtl_vset-loc-set! o::rtl_vset v::obj) (with-access::rtl_vset o (loc) (set! loc v)))

;; rtl_boxset
(define-inline (make-rtl_boxset::rtl_boxset loc1416::obj) (instantiate::rtl_boxset (loc loc1416)))
(define-inline (rtl_boxset?::bool obj::obj) ((@ isa? __object) obj (@ rtl_boxset saw_defs)))
(define (rtl_boxset-nil::rtl_boxset) (class-nil (@ rtl_boxset saw_defs)))
(define-inline (rtl_boxset-loc::obj o::rtl_boxset) (with-access::rtl_boxset o (loc) loc))
(define-inline (rtl_boxset-loc-set! o::rtl_boxset v::obj) (with-access::rtl_boxset o (loc) (set! loc v)))

;; rtl_new
(define-inline (make-rtl_new::rtl_new loc1412::obj type1413::type constr1414::pair-nil) (instantiate::rtl_new (loc loc1412) (type type1413) (constr constr1414)))
(define-inline (rtl_new?::bool obj::obj) ((@ isa? __object) obj (@ rtl_new saw_defs)))
(define (rtl_new-nil::rtl_new) (class-nil (@ rtl_new saw_defs)))
(define-inline (rtl_new-constr::pair-nil o::rtl_new) (with-access::rtl_new o (constr) constr))
(define-inline (rtl_new-constr-set! o::rtl_new v::pair-nil) (with-access::rtl_new o (constr) (set! constr v)))
(define-inline (rtl_new-type::type o::rtl_new) (with-access::rtl_new o (type) type))
(define-inline (rtl_new-type-set! o::rtl_new v::type) (with-access::rtl_new o (type) (set! type v)))
(define-inline (rtl_new-loc::obj o::rtl_new) (with-access::rtl_new o (loc) loc))
(define-inline (rtl_new-loc-set! o::rtl_new v::obj) (with-access::rtl_new o (loc) (set! loc v)))

;; rtl_call
(define-inline (make-rtl_call::rtl_call loc1409::obj var1410::global) (instantiate::rtl_call (loc loc1409) (var var1410)))
(define-inline (rtl_call?::bool obj::obj) ((@ isa? __object) obj (@ rtl_call saw_defs)))
(define (rtl_call-nil::rtl_call) (class-nil (@ rtl_call saw_defs)))
(define-inline (rtl_call-var::global o::rtl_call) (with-access::rtl_call o (var) var))
(define-inline (rtl_call-var-set! o::rtl_call v::global) (with-access::rtl_call o (var) (set! var v)))
(define-inline (rtl_call-loc::obj o::rtl_call) (with-access::rtl_call o (loc) loc))
(define-inline (rtl_call-loc-set! o::rtl_call v::obj) (with-access::rtl_call o (loc) (set! loc v)))

;; rtl_apply
(define-inline (make-rtl_apply::rtl_apply loc1407::obj) (instantiate::rtl_apply (loc loc1407)))
(define-inline (rtl_apply?::bool obj::obj) ((@ isa? __object) obj (@ rtl_apply saw_defs)))
(define (rtl_apply-nil::rtl_apply) (class-nil (@ rtl_apply saw_defs)))
(define-inline (rtl_apply-loc::obj o::rtl_apply) (with-access::rtl_apply o (loc) loc))
(define-inline (rtl_apply-loc-set! o::rtl_apply v::obj) (with-access::rtl_apply o (loc) (set! loc v)))

;; rtl_lightfuncall
(define-inline (make-rtl_lightfuncall::rtl_lightfuncall loc1402::obj name1403::symbol funs1404::pair-nil rettype1405::obj) (instantiate::rtl_lightfuncall (loc loc1402) (name name1403) (funs funs1404) (rettype rettype1405)))
(define-inline (rtl_lightfuncall?::bool obj::obj) ((@ isa? __object) obj (@ rtl_lightfuncall saw_defs)))
(define (rtl_lightfuncall-nil::rtl_lightfuncall) (class-nil (@ rtl_lightfuncall saw_defs)))
(define-inline (rtl_lightfuncall-rettype::obj o::rtl_lightfuncall) (with-access::rtl_lightfuncall o (rettype) rettype))
(define-inline (rtl_lightfuncall-rettype-set! o::rtl_lightfuncall v::obj) (with-access::rtl_lightfuncall o (rettype) (set! rettype v)))
(define-inline (rtl_lightfuncall-funs::pair-nil o::rtl_lightfuncall) (with-access::rtl_lightfuncall o (funs) funs))
(define-inline (rtl_lightfuncall-funs-set! o::rtl_lightfuncall v::pair-nil) (with-access::rtl_lightfuncall o (funs) (set! funs v)))
(define-inline (rtl_lightfuncall-name::symbol o::rtl_lightfuncall) (with-access::rtl_lightfuncall o (name) name))
(define-inline (rtl_lightfuncall-name-set! o::rtl_lightfuncall v::symbol) (with-access::rtl_lightfuncall o (name) (set! name v)))
(define-inline (rtl_lightfuncall-loc::obj o::rtl_lightfuncall) (with-access::rtl_lightfuncall o (loc) loc))
(define-inline (rtl_lightfuncall-loc-set! o::rtl_lightfuncall v::obj) (with-access::rtl_lightfuncall o (loc) (set! loc v)))

;; rtl_funcall
(define-inline (make-rtl_funcall::rtl_funcall loc1400::obj) (instantiate::rtl_funcall (loc loc1400)))
(define-inline (rtl_funcall?::bool obj::obj) ((@ isa? __object) obj (@ rtl_funcall saw_defs)))
(define (rtl_funcall-nil::rtl_funcall) (class-nil (@ rtl_funcall saw_defs)))
(define-inline (rtl_funcall-loc::obj o::rtl_funcall) (with-access::rtl_funcall o (loc) loc))
(define-inline (rtl_funcall-loc-set! o::rtl_funcall v::obj) (with-access::rtl_funcall o (loc) (set! loc v)))

;; rtl_pragma
(define-inline (make-rtl_pragma::rtl_pragma loc1397::obj format1398::bstring) (instantiate::rtl_pragma (loc loc1397) (format format1398)))
(define-inline (rtl_pragma?::bool obj::obj) ((@ isa? __object) obj (@ rtl_pragma saw_defs)))
(define (rtl_pragma-nil::rtl_pragma) (class-nil (@ rtl_pragma saw_defs)))
(define-inline (rtl_pragma-format::bstring o::rtl_pragma) (with-access::rtl_pragma o (format) format))
(define-inline (rtl_pragma-format-set! o::rtl_pragma v::bstring) (with-access::rtl_pragma o (format) (set! format v)))
(define-inline (rtl_pragma-loc::obj o::rtl_pragma) (with-access::rtl_pragma o (loc) loc))
(define-inline (rtl_pragma-loc-set! o::rtl_pragma v::obj) (with-access::rtl_pragma o (loc) (set! loc v)))

;; rtl_cast
(define-inline (make-rtl_cast::rtl_cast loc1394::obj type1395::type) (instantiate::rtl_cast (loc loc1394) (type type1395)))
(define-inline (rtl_cast?::bool obj::obj) ((@ isa? __object) obj (@ rtl_cast saw_defs)))
(define (rtl_cast-nil::rtl_cast) (class-nil (@ rtl_cast saw_defs)))
(define-inline (rtl_cast-type::type o::rtl_cast) (with-access::rtl_cast o (type) type))
(define-inline (rtl_cast-type-set! o::rtl_cast v::type) (with-access::rtl_cast o (type) (set! type v)))
(define-inline (rtl_cast-loc::obj o::rtl_cast) (with-access::rtl_cast o (loc) loc))
(define-inline (rtl_cast-loc-set! o::rtl_cast v::obj) (with-access::rtl_cast o (loc) (set! loc v)))

;; rtl_cast_null
(define-inline (make-rtl_cast_null::rtl_cast_null loc1391::obj type1392::type) (instantiate::rtl_cast_null (loc loc1391) (type type1392)))
(define-inline (rtl_cast_null?::bool obj::obj) ((@ isa? __object) obj (@ rtl_cast_null saw_defs)))
(define (rtl_cast_null-nil::rtl_cast_null) (class-nil (@ rtl_cast_null saw_defs)))
(define-inline (rtl_cast_null-type::type o::rtl_cast_null) (with-access::rtl_cast_null o (type) type))
(define-inline (rtl_cast_null-type-set! o::rtl_cast_null v::type) (with-access::rtl_cast_null o (type) (set! type v)))
(define-inline (rtl_cast_null-loc::obj o::rtl_cast_null) (with-access::rtl_cast_null o (loc) loc))
(define-inline (rtl_cast_null-loc-set! o::rtl_cast_null v::obj) (with-access::rtl_cast_null o (loc) (set! loc v)))

;; rtl_protect
(define-inline (make-rtl_protect::rtl_protect loc1389::obj) (instantiate::rtl_protect (loc loc1389)))
(define-inline (rtl_protect?::bool obj::obj) ((@ isa? __object) obj (@ rtl_protect saw_defs)))
(define (rtl_protect-nil::rtl_protect) (class-nil (@ rtl_protect saw_defs)))
(define-inline (rtl_protect-loc::obj o::rtl_protect) (with-access::rtl_protect o (loc) loc))
(define-inline (rtl_protect-loc-set! o::rtl_protect v::obj) (with-access::rtl_protect o (loc) (set! loc v)))

;; rtl_protected
(define-inline (make-rtl_protected::rtl_protected loc1387::obj) (instantiate::rtl_protected (loc loc1387)))
(define-inline (rtl_protected?::bool obj::obj) ((@ isa? __object) obj (@ rtl_protected saw_defs)))
(define (rtl_protected-nil::rtl_protected) (class-nil (@ rtl_protected saw_defs)))
(define-inline (rtl_protected-loc::obj o::rtl_protected) (with-access::rtl_protected o (loc) loc))
(define-inline (rtl_protected-loc-set! o::rtl_protected v::obj) (with-access::rtl_protected o (loc) (set! loc v)))

;; rtl_ins
(define-inline (make-rtl_ins::rtl_ins loc1381::obj %spill1382::pair-nil dest1383::obj fun1384::rtl_fun args1385::pair-nil) (instantiate::rtl_ins (loc loc1381) (%spill %spill1382) (dest dest1383) (fun fun1384) (args args1385)))
(define-inline (rtl_ins?::bool obj::obj) ((@ isa? __object) obj (@ rtl_ins saw_defs)))
(define (rtl_ins-nil::rtl_ins) (class-nil (@ rtl_ins saw_defs)))
(define-inline (rtl_ins-args::pair-nil o::rtl_ins) (with-access::rtl_ins o (args) args))
(define-inline (rtl_ins-args-set! o::rtl_ins v::pair-nil) (with-access::rtl_ins o (args) (set! args v)))
(define-inline (rtl_ins-fun::rtl_fun o::rtl_ins) (with-access::rtl_ins o (fun) fun))
(define-inline (rtl_ins-fun-set! o::rtl_ins v::rtl_fun) (with-access::rtl_ins o (fun) (set! fun v)))
(define-inline (rtl_ins-dest::obj o::rtl_ins) (with-access::rtl_ins o (dest) dest))
(define-inline (rtl_ins-dest-set! o::rtl_ins v::obj) (with-access::rtl_ins o (dest) (set! dest v)))
(define-inline (rtl_ins-%spill::pair-nil o::rtl_ins) (with-access::rtl_ins o (%spill) %spill))
(define-inline (rtl_ins-%spill-set! o::rtl_ins v::pair-nil) (with-access::rtl_ins o (%spill) (set! %spill v)))
(define-inline (rtl_ins-loc::obj o::rtl_ins) (with-access::rtl_ins o (loc) loc))
(define-inline (rtl_ins-loc-set! o::rtl_ins v::obj) (with-access::rtl_ins o (loc) (set! loc v)))

;; block
(define-inline (make-block::block label1376::int preds1377::pair-nil succs1378::pair-nil first1379::pair) (instantiate::block (label label1376) (preds preds1377) (succs succs1378) (first first1379)))
(define-inline (block?::bool obj::obj) ((@ isa? __object) obj (@ block saw_defs)))
(define (block-nil::block) (class-nil (@ block saw_defs)))
(define-inline (block-first::pair o::block) (with-access::block o (first) first))
(define-inline (block-first-set! o::block v::pair) (with-access::block o (first) (set! first v)))
(define-inline (block-succs::pair-nil o::block) (with-access::block o (succs) succs))
(define-inline (block-succs-set! o::block v::pair-nil) (with-access::block o (succs) (set! succs v)))
(define-inline (block-preds::pair-nil o::block) (with-access::block o (preds) preds))
(define-inline (block-preds-set! o::block v::pair-nil) (with-access::block o (preds) (set! preds v)))
(define-inline (block-label::int o::block) (with-access::block o (label) label))
(define-inline (block-label-set! o::block v::int) (with-access::block o (label) (set! label v)))
))
