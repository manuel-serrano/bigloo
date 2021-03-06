;; ==========================================================
;; Class accessors
;; Bigloo (4.3a)
;; Inria -- Sophia Antipolis     Mon Feb 20 07:54:14 CET 2017 
;; (bigloo.new -classgen Object/class.scm)
;; ==========================================================

;; The directives
(directives

;; tclass
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-tclass::tclass id1179::symbol name1180::obj size1181::obj class1182::obj coerce-to1183::obj parents1184::obj init?1185::bool magic?1186::bool $1187::obj alias1188::obj pointed-to-by1189::obj tvector1190::obj location1191::obj import-location1192::obj occurrence1193::int its-super1194::obj slots1195::obj holder1196::global widening1197::obj depth1198::long final?1199::bool constructor1200::obj virtual-slots-number1201::obj abstract?1202::bool wide-type1203::obj subclasses1204::pair-nil)
    (inline tclass?::bool ::obj)
    (tclass-nil::tclass)
    (inline tclass-subclasses::pair-nil ::tclass)
    (inline tclass-subclasses-set! ::tclass ::pair-nil)
    (inline tclass-wide-type::obj ::tclass)
    (inline tclass-wide-type-set! ::tclass ::obj)
    (inline tclass-abstract?::bool ::tclass)
    (inline tclass-virtual-slots-number::obj ::tclass)
    (inline tclass-virtual-slots-number-set! ::tclass ::obj)
    (inline tclass-constructor::obj ::tclass)
    (inline tclass-final?::bool ::tclass)
    (inline tclass-depth::long ::tclass)
    (inline tclass-depth-set! ::tclass ::long)
    (inline tclass-widening::obj ::tclass)
    (inline tclass-holder::global ::tclass)
    (inline tclass-slots::obj ::tclass)
    (inline tclass-slots-set! ::tclass ::obj)
    (inline tclass-its-super::obj ::tclass)
    (inline tclass-its-super-set! ::tclass ::obj)
    (inline tclass-occurrence::int ::tclass)
    (inline tclass-occurrence-set! ::tclass ::int)
    (inline tclass-import-location::obj ::tclass)
    (inline tclass-import-location-set! ::tclass ::obj)
    (inline tclass-location::obj ::tclass)
    (inline tclass-location-set! ::tclass ::obj)
    (inline tclass-tvector::obj ::tclass)
    (inline tclass-tvector-set! ::tclass ::obj)
    (inline tclass-pointed-to-by::obj ::tclass)
    (inline tclass-pointed-to-by-set! ::tclass ::obj)
    (inline tclass-alias::obj ::tclass)
    (inline tclass-alias-set! ::tclass ::obj)
    (inline tclass-$::obj ::tclass)
    (inline tclass-$-set! ::tclass ::obj)
    (inline tclass-magic?::bool ::tclass)
    (inline tclass-magic?-set! ::tclass ::bool)
    (inline tclass-init?::bool ::tclass)
    (inline tclass-init?-set! ::tclass ::bool)
    (inline tclass-parents::obj ::tclass)
    (inline tclass-parents-set! ::tclass ::obj)
    (inline tclass-coerce-to::obj ::tclass)
    (inline tclass-coerce-to-set! ::tclass ::obj)
    (inline tclass-class::obj ::tclass)
    (inline tclass-class-set! ::tclass ::obj)
    (inline tclass-size::obj ::tclass)
    (inline tclass-size-set! ::tclass ::obj)
    (inline tclass-name::obj ::tclass)
    (inline tclass-name-set! ::tclass ::obj)
    (inline tclass-id::symbol ::tclass))))

;; jclass
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-jclass::jclass id1159::symbol name1160::obj size1161::obj class1162::obj coerce-to1163::obj parents1164::obj init?1165::bool magic?1166::bool $1167::obj alias1168::obj pointed-to-by1169::obj tvector1170::obj location1171::obj import-location1172::obj occurrence1173::int its-super1174::obj slots1175::obj package1176::bstring)
    (inline jclass?::bool ::obj)
    (jclass-nil::jclass)
    (inline jclass-package::bstring ::jclass)
    (inline jclass-slots::obj ::jclass)
    (inline jclass-slots-set! ::jclass ::obj)
    (inline jclass-its-super::obj ::jclass)
    (inline jclass-its-super-set! ::jclass ::obj)
    (inline jclass-occurrence::int ::jclass)
    (inline jclass-occurrence-set! ::jclass ::int)
    (inline jclass-import-location::obj ::jclass)
    (inline jclass-import-location-set! ::jclass ::obj)
    (inline jclass-location::obj ::jclass)
    (inline jclass-location-set! ::jclass ::obj)
    (inline jclass-tvector::obj ::jclass)
    (inline jclass-tvector-set! ::jclass ::obj)
    (inline jclass-pointed-to-by::obj ::jclass)
    (inline jclass-pointed-to-by-set! ::jclass ::obj)
    (inline jclass-alias::obj ::jclass)
    (inline jclass-alias-set! ::jclass ::obj)
    (inline jclass-$::obj ::jclass)
    (inline jclass-$-set! ::jclass ::obj)
    (inline jclass-magic?::bool ::jclass)
    (inline jclass-magic?-set! ::jclass ::bool)
    (inline jclass-init?::bool ::jclass)
    (inline jclass-init?-set! ::jclass ::bool)
    (inline jclass-parents::obj ::jclass)
    (inline jclass-parents-set! ::jclass ::obj)
    (inline jclass-coerce-to::obj ::jclass)
    (inline jclass-coerce-to-set! ::jclass ::obj)
    (inline jclass-class::obj ::jclass)
    (inline jclass-class-set! ::jclass ::obj)
    (inline jclass-size::obj ::jclass)
    (inline jclass-size-set! ::jclass ::obj)
    (inline jclass-name::obj ::jclass)
    (inline jclass-name-set! ::jclass ::obj)
    (inline jclass-id::symbol ::jclass))))

;; wclass
(cond-expand ((and bigloo-class-sans (not bigloo-class-generate))
  (export
    (inline make-wclass::wclass id1141::symbol name1142::obj size1143::obj class1144::obj coerce-to1145::obj parents1146::obj init?1147::bool magic?1148::bool $1149::obj alias1150::obj pointed-to-by1151::obj tvector1152::obj location1153::obj import-location1154::obj occurrence1155::int its-class1156::obj)
    (inline wclass?::bool ::obj)
    (wclass-nil::wclass)
    (inline wclass-its-class::obj ::wclass)
    (inline wclass-its-class-set! ::wclass ::obj)
    (inline wclass-occurrence::int ::wclass)
    (inline wclass-occurrence-set! ::wclass ::int)
    (inline wclass-import-location::obj ::wclass)
    (inline wclass-import-location-set! ::wclass ::obj)
    (inline wclass-location::obj ::wclass)
    (inline wclass-location-set! ::wclass ::obj)
    (inline wclass-tvector::obj ::wclass)
    (inline wclass-tvector-set! ::wclass ::obj)
    (inline wclass-pointed-to-by::obj ::wclass)
    (inline wclass-pointed-to-by-set! ::wclass ::obj)
    (inline wclass-alias::obj ::wclass)
    (inline wclass-alias-set! ::wclass ::obj)
    (inline wclass-$::obj ::wclass)
    (inline wclass-$-set! ::wclass ::obj)
    (inline wclass-magic?::bool ::wclass)
    (inline wclass-magic?-set! ::wclass ::bool)
    (inline wclass-init?::bool ::wclass)
    (inline wclass-init?-set! ::wclass ::bool)
    (inline wclass-parents::obj ::wclass)
    (inline wclass-parents-set! ::wclass ::obj)
    (inline wclass-coerce-to::obj ::wclass)
    (inline wclass-coerce-to-set! ::wclass ::obj)
    (inline wclass-class::obj ::wclass)
    (inline wclass-class-set! ::wclass ::obj)
    (inline wclass-size::obj ::wclass)
    (inline wclass-size-set! ::wclass ::obj)
    (inline wclass-name::obj ::wclass)
    (inline wclass-name-set! ::wclass ::obj)
    (inline wclass-id::symbol ::wclass)))))

;; The definitions
(cond-expand (bigloo-class-sans
;; tclass
(define-inline (make-tclass::tclass id1179::symbol name1180::obj size1181::obj class1182::obj coerce-to1183::obj parents1184::obj init?1185::bool magic?1186::bool $1187::obj alias1188::obj pointed-to-by1189::obj tvector1190::obj location1191::obj import-location1192::obj occurrence1193::int its-super1194::obj slots1195::obj holder1196::global widening1197::obj depth1198::long final?1199::bool constructor1200::obj virtual-slots-number1201::obj abstract?1202::bool wide-type1203::obj subclasses1204::pair-nil) (instantiate::tclass (id id1179) (name name1180) (size size1181) (class class1182) (coerce-to coerce-to1183) (parents parents1184) (init? init?1185) (magic? magic?1186) ($ $1187) (alias alias1188) (pointed-to-by pointed-to-by1189) (tvector tvector1190) (location location1191) (import-location import-location1192) (occurrence occurrence1193) (its-super its-super1194) (slots slots1195) (holder holder1196) (widening widening1197) (depth depth1198) (final? final?1199) (constructor constructor1200) (virtual-slots-number virtual-slots-number1201) (abstract? abstract?1202) (wide-type wide-type1203) (subclasses subclasses1204)))
(define-inline (tclass?::bool obj::obj) ((@ isa? __object) obj (@ tclass object_class)))
(define (tclass-nil::tclass) (class-nil (@ tclass object_class)))
(define-inline (tclass-subclasses::pair-nil o::tclass) (-> |#!bigloo_wallow| o subclasses))
(define-inline (tclass-subclasses-set! o::tclass v::pair-nil) (set! (-> |#!bigloo_wallow| o subclasses) v))
(define-inline (tclass-wide-type::obj o::tclass) (-> |#!bigloo_wallow| o wide-type))
(define-inline (tclass-wide-type-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o wide-type) v))
(define-inline (tclass-abstract?::bool o::tclass) (-> |#!bigloo_wallow| o abstract?))
(define-inline (tclass-abstract?-set! o::tclass v::bool) (set! (-> |#!bigloo_wallow| o abstract?) v))
(define-inline (tclass-virtual-slots-number::obj o::tclass) (-> |#!bigloo_wallow| o virtual-slots-number))
(define-inline (tclass-virtual-slots-number-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o virtual-slots-number) v))
(define-inline (tclass-constructor::obj o::tclass) (-> |#!bigloo_wallow| o constructor))
(define-inline (tclass-constructor-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o constructor) v))
(define-inline (tclass-final?::bool o::tclass) (-> |#!bigloo_wallow| o final?))
(define-inline (tclass-final?-set! o::tclass v::bool) (set! (-> |#!bigloo_wallow| o final?) v))
(define-inline (tclass-depth::long o::tclass) (-> |#!bigloo_wallow| o depth))
(define-inline (tclass-depth-set! o::tclass v::long) (set! (-> |#!bigloo_wallow| o depth) v))
(define-inline (tclass-widening::obj o::tclass) (-> |#!bigloo_wallow| o widening))
(define-inline (tclass-widening-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o widening) v))
(define-inline (tclass-holder::global o::tclass) (-> |#!bigloo_wallow| o holder))
(define-inline (tclass-holder-set! o::tclass v::global) (set! (-> |#!bigloo_wallow| o holder) v))
(define-inline (tclass-slots::obj o::tclass) (-> |#!bigloo_wallow| o slots))
(define-inline (tclass-slots-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o slots) v))
(define-inline (tclass-its-super::obj o::tclass) (-> |#!bigloo_wallow| o its-super))
(define-inline (tclass-its-super-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o its-super) v))
(define-inline (tclass-occurrence::int o::tclass) (-> |#!bigloo_wallow| o occurrence))
(define-inline (tclass-occurrence-set! o::tclass v::int) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (tclass-import-location::obj o::tclass) (-> |#!bigloo_wallow| o import-location))
(define-inline (tclass-import-location-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o import-location) v))
(define-inline (tclass-location::obj o::tclass) (-> |#!bigloo_wallow| o location))
(define-inline (tclass-location-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o location) v))
(define-inline (tclass-tvector::obj o::tclass) (-> |#!bigloo_wallow| o tvector))
(define-inline (tclass-tvector-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o tvector) v))
(define-inline (tclass-pointed-to-by::obj o::tclass) (-> |#!bigloo_wallow| o pointed-to-by))
(define-inline (tclass-pointed-to-by-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o pointed-to-by) v))
(define-inline (tclass-alias::obj o::tclass) (-> |#!bigloo_wallow| o alias))
(define-inline (tclass-alias-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o alias) v))
(define-inline (tclass-$::obj o::tclass) (-> |#!bigloo_wallow| o $))
(define-inline (tclass-$-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o $) v))
(define-inline (tclass-magic?::bool o::tclass) (-> |#!bigloo_wallow| o magic?))
(define-inline (tclass-magic?-set! o::tclass v::bool) (set! (-> |#!bigloo_wallow| o magic?) v))
(define-inline (tclass-init?::bool o::tclass) (-> |#!bigloo_wallow| o init?))
(define-inline (tclass-init?-set! o::tclass v::bool) (set! (-> |#!bigloo_wallow| o init?) v))
(define-inline (tclass-parents::obj o::tclass) (-> |#!bigloo_wallow| o parents))
(define-inline (tclass-parents-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o parents) v))
(define-inline (tclass-coerce-to::obj o::tclass) (-> |#!bigloo_wallow| o coerce-to))
(define-inline (tclass-coerce-to-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o coerce-to) v))
(define-inline (tclass-class::obj o::tclass) (-> |#!bigloo_wallow| o class))
(define-inline (tclass-class-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o class) v))
(define-inline (tclass-size::obj o::tclass) (-> |#!bigloo_wallow| o size))
(define-inline (tclass-size-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (tclass-name::obj o::tclass) (-> |#!bigloo_wallow| o name))
(define-inline (tclass-name-set! o::tclass v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (tclass-id::symbol o::tclass) (-> |#!bigloo_wallow| o id))
(define-inline (tclass-id-set! o::tclass v::symbol) (set! (-> |#!bigloo_wallow| o id) v))

;; jclass
(define-inline (make-jclass::jclass id1159::symbol name1160::obj size1161::obj class1162::obj coerce-to1163::obj parents1164::obj init?1165::bool magic?1166::bool $1167::obj alias1168::obj pointed-to-by1169::obj tvector1170::obj location1171::obj import-location1172::obj occurrence1173::int its-super1174::obj slots1175::obj package1176::bstring) (instantiate::jclass (id id1159) (name name1160) (size size1161) (class class1162) (coerce-to coerce-to1163) (parents parents1164) (init? init?1165) (magic? magic?1166) ($ $1167) (alias alias1168) (pointed-to-by pointed-to-by1169) (tvector tvector1170) (location location1171) (import-location import-location1172) (occurrence occurrence1173) (its-super its-super1174) (slots slots1175) (package package1176)))
(define-inline (jclass?::bool obj::obj) ((@ isa? __object) obj (@ jclass object_class)))
(define (jclass-nil::jclass) (class-nil (@ jclass object_class)))
(define-inline (jclass-package::bstring o::jclass) (-> |#!bigloo_wallow| o package))
(define-inline (jclass-package-set! o::jclass v::bstring) (set! (-> |#!bigloo_wallow| o package) v))
(define-inline (jclass-slots::obj o::jclass) (-> |#!bigloo_wallow| o slots))
(define-inline (jclass-slots-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o slots) v))
(define-inline (jclass-its-super::obj o::jclass) (-> |#!bigloo_wallow| o its-super))
(define-inline (jclass-its-super-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o its-super) v))
(define-inline (jclass-occurrence::int o::jclass) (-> |#!bigloo_wallow| o occurrence))
(define-inline (jclass-occurrence-set! o::jclass v::int) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (jclass-import-location::obj o::jclass) (-> |#!bigloo_wallow| o import-location))
(define-inline (jclass-import-location-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o import-location) v))
(define-inline (jclass-location::obj o::jclass) (-> |#!bigloo_wallow| o location))
(define-inline (jclass-location-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o location) v))
(define-inline (jclass-tvector::obj o::jclass) (-> |#!bigloo_wallow| o tvector))
(define-inline (jclass-tvector-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o tvector) v))
(define-inline (jclass-pointed-to-by::obj o::jclass) (-> |#!bigloo_wallow| o pointed-to-by))
(define-inline (jclass-pointed-to-by-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o pointed-to-by) v))
(define-inline (jclass-alias::obj o::jclass) (-> |#!bigloo_wallow| o alias))
(define-inline (jclass-alias-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o alias) v))
(define-inline (jclass-$::obj o::jclass) (-> |#!bigloo_wallow| o $))
(define-inline (jclass-$-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o $) v))
(define-inline (jclass-magic?::bool o::jclass) (-> |#!bigloo_wallow| o magic?))
(define-inline (jclass-magic?-set! o::jclass v::bool) (set! (-> |#!bigloo_wallow| o magic?) v))
(define-inline (jclass-init?::bool o::jclass) (-> |#!bigloo_wallow| o init?))
(define-inline (jclass-init?-set! o::jclass v::bool) (set! (-> |#!bigloo_wallow| o init?) v))
(define-inline (jclass-parents::obj o::jclass) (-> |#!bigloo_wallow| o parents))
(define-inline (jclass-parents-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o parents) v))
(define-inline (jclass-coerce-to::obj o::jclass) (-> |#!bigloo_wallow| o coerce-to))
(define-inline (jclass-coerce-to-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o coerce-to) v))
(define-inline (jclass-class::obj o::jclass) (-> |#!bigloo_wallow| o class))
(define-inline (jclass-class-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o class) v))
(define-inline (jclass-size::obj o::jclass) (-> |#!bigloo_wallow| o size))
(define-inline (jclass-size-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (jclass-name::obj o::jclass) (-> |#!bigloo_wallow| o name))
(define-inline (jclass-name-set! o::jclass v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (jclass-id::symbol o::jclass) (-> |#!bigloo_wallow| o id))
(define-inline (jclass-id-set! o::jclass v::symbol) (set! (-> |#!bigloo_wallow| o id) v))

;; wclass
(define-inline (make-wclass::wclass id1141::symbol name1142::obj size1143::obj class1144::obj coerce-to1145::obj parents1146::obj init?1147::bool magic?1148::bool $1149::obj alias1150::obj pointed-to-by1151::obj tvector1152::obj location1153::obj import-location1154::obj occurrence1155::int its-class1156::obj) (instantiate::wclass (id id1141) (name name1142) (size size1143) (class class1144) (coerce-to coerce-to1145) (parents parents1146) (init? init?1147) (magic? magic?1148) ($ $1149) (alias alias1150) (pointed-to-by pointed-to-by1151) (tvector tvector1152) (location location1153) (import-location import-location1154) (occurrence occurrence1155) (its-class its-class1156)))
(define-inline (wclass?::bool obj::obj) ((@ isa? __object) obj (@ wclass object_class)))
(define (wclass-nil::wclass) (class-nil (@ wclass object_class)))
(define-inline (wclass-its-class::obj o::wclass) (-> |#!bigloo_wallow| o its-class))
(define-inline (wclass-its-class-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o its-class) v))
(define-inline (wclass-occurrence::int o::wclass) (-> |#!bigloo_wallow| o occurrence))
(define-inline (wclass-occurrence-set! o::wclass v::int) (set! (-> |#!bigloo_wallow| o occurrence) v))
(define-inline (wclass-import-location::obj o::wclass) (-> |#!bigloo_wallow| o import-location))
(define-inline (wclass-import-location-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o import-location) v))
(define-inline (wclass-location::obj o::wclass) (-> |#!bigloo_wallow| o location))
(define-inline (wclass-location-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o location) v))
(define-inline (wclass-tvector::obj o::wclass) (-> |#!bigloo_wallow| o tvector))
(define-inline (wclass-tvector-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o tvector) v))
(define-inline (wclass-pointed-to-by::obj o::wclass) (-> |#!bigloo_wallow| o pointed-to-by))
(define-inline (wclass-pointed-to-by-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o pointed-to-by) v))
(define-inline (wclass-alias::obj o::wclass) (-> |#!bigloo_wallow| o alias))
(define-inline (wclass-alias-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o alias) v))
(define-inline (wclass-$::obj o::wclass) (-> |#!bigloo_wallow| o $))
(define-inline (wclass-$-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o $) v))
(define-inline (wclass-magic?::bool o::wclass) (-> |#!bigloo_wallow| o magic?))
(define-inline (wclass-magic?-set! o::wclass v::bool) (set! (-> |#!bigloo_wallow| o magic?) v))
(define-inline (wclass-init?::bool o::wclass) (-> |#!bigloo_wallow| o init?))
(define-inline (wclass-init?-set! o::wclass v::bool) (set! (-> |#!bigloo_wallow| o init?) v))
(define-inline (wclass-parents::obj o::wclass) (-> |#!bigloo_wallow| o parents))
(define-inline (wclass-parents-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o parents) v))
(define-inline (wclass-coerce-to::obj o::wclass) (-> |#!bigloo_wallow| o coerce-to))
(define-inline (wclass-coerce-to-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o coerce-to) v))
(define-inline (wclass-class::obj o::wclass) (-> |#!bigloo_wallow| o class))
(define-inline (wclass-class-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o class) v))
(define-inline (wclass-size::obj o::wclass) (-> |#!bigloo_wallow| o size))
(define-inline (wclass-size-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o size) v))
(define-inline (wclass-name::obj o::wclass) (-> |#!bigloo_wallow| o name))
(define-inline (wclass-name-set! o::wclass v::obj) (set! (-> |#!bigloo_wallow| o name) v))
(define-inline (wclass-id::symbol o::wclass) (-> |#!bigloo_wallow| o id))
(define-inline (wclass-id-set! o::wclass v::symbol) (set! (-> |#!bigloo_wallow| o id) v))
))
