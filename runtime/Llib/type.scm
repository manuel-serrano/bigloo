;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/type.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  8 08:52:32 1995                          */
;*    Last change :  Wed Sep  8 08:42:09 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The type description                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __type
   
   (use __error
	__bigloo
	__tvector
        __weakptr
	__ucs2
	__unicode
	__bignum
	__r4_equivalence_6_2
	__r4_numbers_6_5_fixnum
	__r4_booleans_6_1
	__r4_characters_6_6
	__r4_pairs_and_lists_6_3
	__r4_vectors_6_8
	__r4_numbers_6_5_flonum
	__r4_symbols_6_4
	__r4_strings_6_7
	__evenv)
   
   (type
    
    ;; all type declarations
    (magic magic           "obj_t"          bigloo)
    (magic _               "_"              bigloo)
    
    ;; debugging traces (used by `the_failure')
    (trace "struct trace *" _)

    ;; we start by the obj hierarchy
    (obj                   "obj_t"          bigloo)
    (cnst*                 "obj_t"          bigloo)
    
    (subtype dynamic-env   "obj_t"          (obj))
    (subtype procedure     "obj_t"          (obj))
    (subtype procedure-el  "obj_t"          (obj))
    (subtype procedure-el1 "obj_t"          (obj))
    (subtype pair          "obj_t"          (obj))
    (subtype epair         "obj_t"          (obj))
    (subtype nil           "obj_t"          (obj))
    ;; There is a relationship that exists between pair-nil, nil and
    ;; pair which is nil > pair-nil and pair > pair-nil. This has introduced
    ;; a bug into one compiler optimization that attempts to statically
    ;; reduce runtime type check. I have not been able to find a better
    ;; solution than to hard code this relation into the compiler.
    ;; @ref ../../comptime/Reduce/typec.scm:pair-nil subtyping@
    ;; @label pair-nil subtyping@
    (subtype pair-nil      "obj_t"          (obj))
    (subtype bint          "obj_t"          (obj))
    (subtype belong        "obj_t"          (obj))
    (subtype bllong        "obj_t"          (obj))
    (subtype bignum        "obj_t"          (obj))
    (subtype bbool         "obj_t"          (obj))
    (subtype cnst          "obj_t"          (obj))
    (subtype bstring       "obj_t"          (obj))
    (subtype ucs2string    "obj_t"          (obj))
    (subtype bchar         "obj_t"          (obj))
    (subtype bucs2         "obj_t"          (obj))
    (subtype real          "obj_t"          (obj))
    (subtype vector        "obj_t"          (obj))
    (subtype tvector       "obj_t"          (obj))
    (subtype weakptr       "obj_t"          (obj))
    (subtype struct        "obj_t"          (obj))
    (subtype tstruct       "obj_t"          (obj))
    (subtype output-port   "obj_t"          (obj))
    (subtype input-port    "obj_t"          (obj))
    (subtype binary-port   "obj_t"          (obj))
    (subtype unspecified   "obj_t"          (obj))
    (subtype symbol        "obj_t"          (obj))
    (subtype keyword       "obj_t"          (obj))
    (subtype cell          "obj_t"          (obj))
    (subtype exit          "void *"         (obj))
    (subtype foreign       "obj_t"          (obj))
    (subtype process       "obj_t"          (obj))
    (subtype socket        "obj_t"          (obj))
    (subtype custom        "obj_t"          (obj))
    (subtype date          "obj_t"          (obj))
    (subtype mutex         "obj_t"          (obj))
    (subtype condvar       "obj_t"          (obj))
    (subtype mmap          "obj_t"          (obj))
    (subtype opaque        "obj_t"          (obj))

    ;; srfi4 vectors
    (subtype s8vector      "obj_t"          (obj))
    (subtype u8vector      "obj_t"          (obj))
    (subtype s16vector     "obj_t"          (obj))
    (subtype u16vector     "obj_t"          (obj))
    (subtype s32vector     "obj_t"          (obj))
    (subtype u32vector     "obj_t"          (obj))
    (subtype s64vector     "obj_t"          (obj))
    (subtype u64vector     "obj_t"          (obj))

    (subtype f32vector     "obj_t"          (obj))
    (subtype f64vector     "obj_t"          (obj))

    ;; we give now the foreign hierarchy
    (cobj                  "long"           C)
    
    (subtype char          "unsigned char"  (cobj))
    (subtype uchar         "unsigned char"  (cobj))
    (subtype ucs2          "ucs2_t"         (cobj))
;; MS 7 mars 2007    
;;    (subtype byte          "signed char"    (cobj))
    (subtype byte          "char"           (cobj))
    (subtype ubyte         "unsigned char"  (cobj))
    (subtype short         "short"          (cobj))
    (subtype ushort        "unsigned short" (cobj))
    (subtype int           "int"            (cobj))
    (subtype uint          "unsigned int"   (cobj))
    (subtype long          "long"           (cobj))
    (subtype ulong         "unsigned long"  (cobj))
    (subtype elong         "long"           (cobj))
    (subtype uelong        "unsigned long"  (cobj))
    (subtype llong         "BGL_LONGLONG_T" (cobj))
    (subtype ullong        "unsigned BGL_LONGLONG_T" (cobj))
    (subtype bool          "bool_t"         (cobj))
    (subtype string        "char *"         (cobj))
    (subtype file          "FILE *"         (cobj))
    (subtype double        "double"         (cobj))
    (subtype float         "float"          (cobj))
    (subtype void          "void"           (cobj))
    (subtype void*         "void *"         (cobj))
    (subtype function      "(long *)()"     (cobj))

    ;; obj coercions
    (coerce obj magic          ()               ())
    (coerce obj trace          ()               ())
    (coerce obj cobj           ()               ($obj->cobj))
    (coerce obj unspecified    ()               ((lambda (x) #unspecified)))
    (coerce obj bbool          (c-boolean?)     ())
    (coerce obj bool           ()               ($obj->bool))
    (coerce obj bchar          (c-char?)        ())
    (coerce obj bucs2          (c-ucs2?)        ())
    (coerce obj bint           (c-fixnum?)      ())
    (coerce obj belong         (c-elong?)       ())
    (coerce obj bllong         (c-llong?)       ())
    (coerce obj bignum         ($bignum?)       ())
    (coerce obj symbol         (c-symbol?)      ())
    (coerce obj keyword        (c-keyword?)     ())
    (coerce obj bstring        ($string?)       ())
    (coerce obj ucs2string     (c-ucs2-string?) ())
    (coerce obj real           (c-flonum?)      ())
    (coerce obj vector         ($vector?)       ())
    (coerce obj s8vector       ($s8vector?)     ())
    (coerce obj u8vector       ($u8vector?)     ())
    (coerce obj s16vector      ($s16vector?)    ())
    (coerce obj u16vector      ($u16vector?)    ())
    (coerce obj s32vector      ($s32vector?)    ())
    (coerce obj u32vector      ($u32vector?)    ())
    (coerce obj s64vector      ($s64vector?)    ())
    (coerce obj u64vector      ($u64vector?)    ())
    (coerce obj f32vector      ($f32vector?)    ())
    (coerce obj f64vector      ($f64vector?)    ())
    (coerce obj tvector        ($tvector?)      ())
    (coerce obj weakptr        (c-weakptr?)     ())
    (coerce obj dynamic-env    ($dynamic-env?)  ())
    (coerce obj procedure      (c-procedure?)   ())
    (coerce obj struct         (c-struct?)      ())
    (coerce obj tstruct        (c-tstruct?)     ())
    (coerce obj pair           (c-pair?)        ())
    (coerce obj epair          (c-epair?)       ())
    (coerce obj nil            (c-null?)        ())
    (coerce obj pair-nil       (pair-or-null?)  ())
    (coerce obj cell           ()               ())
    (coerce obj exit           ()               ())
    (coerce obj input-port     (c-input-port?)  ())
    (coerce obj output-port    (c-output-port?) ())
    (coerce obj binary-port    (c-binary-port?) ())
    (coerce obj void*          (c-foreign?)     ($obj->void*))
    (coerce obj foreign        (c-foreign?)     ())
    (coerce obj process        (c-process?)     ())
    (coerce obj socket         (c-socket?)      ())
    (coerce obj custom         (c-custom?)      ())
    (coerce obj date           (c-date?)        ())
    (coerce obj mutex          ($mutex?)        ())
    (coerce obj condvar        ($condvar?)      ())
    (coerce obj mmap           ($mmap?)         ())
    (coerce obj opaque         (c-opaque?)      ())
    
    ;; cobj
    (coerce cobj obj           ()               ($cobj->obj))
    (coerce cobj bool          ()               ())
    (coerce cobj char          ()               ())
    (coerce cobj uchar         ()               ())
    (coerce cobj ucs2          ()               ())
    (coerce cobj byte          ()               ())
    (coerce cobj ubyte         ()               ())
    (coerce cobj short         ()               ())
    (coerce cobj ushort        ()               ())
    (coerce cobj int           ()               ())
    (coerce cobj uint          ()               ())
    (coerce cobj long          ()               ())
    (coerce cobj ulong         ()               ())
    (coerce cobj elong         ()               ())
    (coerce cobj uelong        ()               ())
    (coerce cobj llong         ()               ())
    (coerce cobj ullong        ()               ())
    (coerce cobj string        ()               ())
    (coerce cobj double        ()               ())
    (coerce cobj float         ()               ())
    (coerce cobj file          ()               ())
    (coerce cobj void          ()               ())
    (coerce cobj void*         ()               ())

    ;; -> obj
    (coerce bbool obj          ()               ())
    (coerce cnst obj           ()               ())
    (coerce unspecified obj    ()               ())
    (coerce symbol obj         ()               ())
    (coerce keyword obj        ()               ())
    (coerce bstring obj        ()               ())
    (coerce ucs2string obj     ()               ())
    (coerce vector obj         ()               ())
    (coerce s8vector obj       ()               ())
    (coerce u8vector obj       ()               ())
    (coerce s16vector obj      ()               ())
    (coerce u16vector obj      ()               ())
    (coerce s32vector obj      ()               ())
    (coerce u32vector obj      ()               ())
    (coerce s64vector obj      ()               ())
    (coerce u64vector obj      ()               ())
    (coerce f32vector obj      ()               ())
    (coerce f64vector obj      ()               ())
    (coerce bucs2 obj          ()               ())
    (coerce bchar obj          ()               ())
    (coerce bint obj           ()               ())
    (coerce belong obj         ()               ())
    (coerce bllong obj         ()               ())
    (coerce bignum obj         ()               ())
    (coerce real obj           ()               ())
    (coerce tvector obj        ()               ())
    (coerce weakptr obj        ()               ())
    (coerce dynamic-env obj    ()               ())
    (coerce procedure obj      ()               ())
    (coerce struct obj         ()               ())
    (coerce tstruct obj        ()               ())
    (coerce pair obj           ()               ())
    (coerce epair obj          ()               ())
    (coerce pair-nil obj       ()               ())
    (coerce nil obj            ()               ())
    (coerce cell obj           ()               ())
    (coerce exit obj           ()               ())
    (coerce input-port obj     ()               ())
    (coerce output-port obj    ()               ())
    (coerce binary-port obj    ()               ())
    (coerce void* obj          ()               ($void*->obj))
    (coerce foreign obj        ()               ())
    (coerce process obj        ()               ())
    (coerce socket obj         ()               ())
    (coerce custom obj         ()               ())
    (coerce date obj           ()               ())
    (coerce mutex obj          ()               ())
    (coerce condvar obj        ()               ())
    (coerce mmap obj           ()               ())
    (coerce opaque obj         ()               ())

    ;; -> cobj
    (coerce bool cobj          ()               ())
    (coerce string cobj        ()               ())
    (coerce ucs2 cobj          ()               ())
    (coerce char cobj          ()               ())
    (coerce uchar cobj         ()               ())
    (coerce byte cobj          ()               ())
    (coerce ubyte cobj         ()               ())
    (coerce short cobj         ()               ())
    (coerce ushort cobj        ()               ())
    (coerce int cobj           ()               ())
    (coerce uint cobj          ()               ())
    (coerce long cobj          ()               ())
    (coerce ulong cobj         ()               ())
    (coerce elong cobj         ()               ())
    (coerce uelong cobj        ()               ())
    (coerce llong cobj         ()               ())
    (coerce ullong cobj        ()               ())
    (coerce float cobj         ()               ())
    (coerce double cobj        ()               ())
    (coerce file cobj          ()               ())
    (coerce void cobj          ()               ())
    (coerce void* cobj         ()               ())
    
    ;; bool
    (coerce bool bbool         ()               ($bool->bbool))
    (coerce bbool bool         ()               ($bbool->bool))

    ;; cnst
    (coerce cnst bool          ()               ((lambda (x) #t)))

    ;; unspecified
    (coerce unspecified  bool  ()               ((lambda (x) #t)))

    ;; symbol
    (coerce symbol bool        ()               ((lambda (x) #t)))

    ;; keyword
    (coerce keyword bool       ()               ((lambda (x) #t)))

    ;; bstring
    (coerce bstring string     ()               ($bstring->string))
    (coerce bstring bool       ()               ((lambda (x) #t)))
    (coerce string bool        ()               ((lambda (x::string) #t)))
    (coerce string bstring     ()               ($string->bstring))

    ;; ucs2string
    (coerce ucs2string bool    ()               ((lambda (x) #t)))

    ;; vector
    (coerce vector bool        ()               ((lambda (x) #t)))

    ;; s8vector
    (coerce s8vector bool      ()               ((lambda (x) #t)))

    ;; u8vector
    (coerce u8vector bool      ()               ((lambda (x) #t)))

    ;; s16vector
    (coerce s16vector bool     ()               ((lambda (x) #t)))

    ;; u16vector
    (coerce u16vector bool     ()               ((lambda (x) #t)))

    ;; s32vector
    (coerce s32vector bool     ()               ((lambda (x) #t)))

    ;; u32vector
    (coerce u32vector bool     ()               ((lambda (x) #t)))

    ;; s64vector
    (coerce s64vector bool     ()               ((lambda (x) #t)))

    ;; u64vector
    (coerce u64vector bool     ()               ((lambda (x) #t)))

    ;; f32vector
    (coerce f32vector bool     ()               ((lambda (x) #t)))

    ;; f64vector
    (coerce f64vector bool     ()               ((lambda (x) #t)))

    ;; bucs2
    (coerce bucs2 ucs2         ()               ($bucs2->ucs2))
    (coerce bucs2 bool         ()               ((lambda (x) #t)))
    (coerce ucs2 bool          ()               ((lambda (x::char) #t)))
    (coerce ucs2 bucs2         ()               ($ucs2->bucs2))

    ;; bchar
    (coerce bchar char         ()               ($bchar->char))
    (coerce bchar uchar        ()               ($bchar->uchar))
    (coerce bchar bool         ()               ((lambda (x) #t)))

    ;; char
    (coerce char bool          ()               ((lambda (x::char) #t)))
    (coerce char uchar         ()               ($char->uchar))
    (coerce char bchar         ()               ($char->bchar))

    ;; uchar
    (coerce uchar bool         ()               ((lambda (x::uchar) #t)))
    (coerce uchar char         ()               ($uchar->char))
    (coerce uchar bchar        ()               ($uchar->bchar))

    ;; byte
    (coerce byte bool          ()               ((lambda (x::byte) #t)))
    (coerce byte bint          ()               ($byte->bint))
    (coerce byte char          ()               ($byte->char))
    (coerce byte uchar         ()               ($byte->uchar))
    (coerce byte bool          ()               ((lambda (x::byte) #t)))
    (coerce byte ubyte         ()               ($byte->ubyte))
    (coerce byte short         ()               ($byte->short))
    (coerce byte ushort        ()               ($byte->ushort))
    (coerce byte int           ()               ($byte->int))
    (coerce byte uint          ()               ($byte->uint))
    (coerce byte long          ()               ($byte->long))
    (coerce byte ulong         ()               ($byte->ulong))

    ;; ubyte
    (coerce ubyte bool         ()               ((lambda (x::ubyte) #t)))
    (coerce ubyte char         ()               ($ubyte->char))
    (coerce ubyte uchar        ()               ($ubyte->uchar))
    (coerce ubyte bool         ()               ((lambda (x::ubyte) #t)))
    (coerce ubyte byte         ()               ($ubyte->byte))
    (coerce ubyte short        ()               ($ubyte->short))
    (coerce ubyte ushort       ()               ($ubyte->ushort))
    (coerce ubyte int          ()               ($ubyte->int))
    (coerce ubyte uint         ()               ($ubyte->uint))
    (coerce ubyte long         ()               ($ubyte->long))
    (coerce ubyte ulong        ()               ($ubyte->ulong))
    (coerce ubyte bint         ()               ($ubyte->bint))

    ;; short
    (coerce short bool         ()               ((lambda (x::short) #t)))
    (coerce short byte         ()               ($short->byte))
    (coerce short ubyte        ()               ($short->ubyte))
    (coerce short ushort       ()               ($short->ushort))
    (coerce short int          ()               ($short->int))
    (coerce short uint         ()               ($short->uint))
    (coerce short long         ()               ($short->long))
    (coerce short ulong        ()               ($short->ulong))
    (coerce short bint         ()               ($short->bint))

    ;; ushort
    (coerce ushort bool        ()               ((lambda (x::ushort) #t)))
    (coerce ushort byte        ()               ($ushort->byte))
    (coerce ushort ubyte       ()               ($ushort->ubyte))
    (coerce ushort short       ()               ($ushort->short))
    (coerce ushort int         ()               ($ushort->int))
    (coerce ushort uint        ()               ($ushort->uint))
    (coerce ushort long        ()               ($ushort->long))
    (coerce ushort ulong       ()               ($ushort->ulong))
    (coerce ushort bint        ()               ($ushort->bint))

    ;; integer
    (coerce bint byte          ()               ($bint->byte))
    (coerce bint ubyte         ()               ($bint->ubyte))
    (coerce bint short         ()               ($bint->short))
    (coerce bint ushort        ()               ($bint->ushort))
    (coerce bint int           ()               ($bint->int))
    (coerce bint uint          ()               ($bint->uint))
    (coerce bint long          ()               ($bint->long))
    (coerce bint ulong         ()               ($bint->ulong))
    (coerce bint bool          ()               ((lambda (x) #t)))

    ;; int
    (coerce int bool           ()               ((lambda (x::int) #t)))
    (coerce int byte           ()               ($int->byte))
    (coerce int ubyte          ()               ($int->ubyte))
    (coerce int short          ()               ($int->short))
    (coerce int ushort         ()               ($int->ushort))
    (coerce int uint           ()               ($int->uint))
    (coerce int long           ()               ($int->long))
    (coerce int ulong          ()               ($int->ulong))
    (coerce int bint           ()               ($int->bint))
    (coerce int bool           ()               ())

    ;; uint
    (coerce uint bool          ()               ((lambda (x::uint) #t)))
    (coerce uint byte          ()               ($uint->byte))
    (coerce uint ubyte         ()               ($uint->ubyte))
    (coerce uint short         ()               ($uint->short))
    (coerce uint ushort        ()               ($uint->ushort))
    (coerce uint int           ()               ($uint->int))
    (coerce uint long          ()               ($uint->long))
    (coerce uint ulong         ()               ($uint->ulong))
    (coerce uint bint          ()               ($uint->bint))

    ;; long
    (coerce long elong         ()               ($long->elong))
    (coerce long uelong        ()               ($long->elong))
    (coerce long llong         ()               ($long->llong))
    (coerce long ullong        ()               ($long->ullong))
    (coerce long magic         ()               ($long->bint))
    (coerce long bool          ()               ((lambda (x::long) #t)))
    (coerce long byte          ()               ($long->byte))
    (coerce long ubyte         ()               ($long->ubyte))
    (coerce long short         ()               ($long->short))
    (coerce long ushort        ()               ($long->ushort))
    (coerce long int           ()               ($long->int))
    (coerce long uint          ()               ($long->uint))
    (coerce long ulong         ()               ($long->ulong))
    (coerce long bint          ()               ($long->bint))

    ;; ulong
    (coerce ulong bool         ()               ((lambda (x::ulong) #t)))
    (coerce ulong byte         ()               ($ulong->byte))
    (coerce ulong ubyte        ()               ($ulong->ubyte))
    (coerce ulong short        ()               ($ulong->short))
    (coerce ulong ushort       ()               ($ulong->ushort))
    (coerce ulong int          ()               ($ulong->int))
    (coerce ulong uint         ()               ($ulong->uint))
    (coerce ulong long         ()               ($ulong->long))
    (coerce ulong bint         ()               ($ulong->bint))

    ;; elong
    (coerce belong elong       ()               ($belong->elong))
    (coerce belong uelong      ()               ($belong->elong))

    (coerce elong long         ()               ($elong->long))
    (coerce elong belong       ()               ($elong->belong))

    (coerce uelong long        ()               ($elong->long))
    (coerce uelong belong      ()               ($elong->belong))

    (coerce uelong elong       ()               ($uelong->elong))
    (coerce elong uelong       ()               ($elong->uelong))

    ;; llong
    (coerce bllong ullong      ()               ($bllong->ullong))
    (coerce bllong llong       ()               ($bllong->llong))

    (coerce llong long         ()               ($llong->long))
    (coerce llong bllong       ()               ($llong->bllong))

    (coerce ullong long        ()               ($ullong->long))
    (coerce ullong bllong      ()               ($ullong->bllong))

    (coerce ullong llong       ()               ($ullong->llong))
    (coerce llong ullong       ()               ($llong->ullong))

    ;; double
    (coerce real float         ()               ($real->float))
    (coerce real double        ()               ($real->double))
    (coerce real bool          ()               ((lambda (x) #t)))

    (coerce float double       ()               ($float->double))
    (coerce float bool         ()               ((lambda (x::float) #t)))
    (coerce float real         ()               ($float->real))

    (coerce double bool        ()               ((lambda (x::double) #t)))
    (coerce double real        ()               ($double->real))
    (coerce double float       ()               ($double->float))

    ;; bignum
    (coerce bignum bool        ()               ((lambda (x) #t)))
    (coerce bignum int         ()               ($bignum->fixnum))

    ;; tvector
    (coerce tvector bool       ()               ((lambda (x) #t)))

    ;; weakptr
    (coerce weakptr bool       ()               ((lambda (x) #t)))

    ;; dynamic-env
    (coerce dynamic-env bool   ()               ((lambda (x) #t)))

    ;; procedure
    (coerce procedure bool     ()               ((lambda (x) #t)))

    ;; procedure
    (coerce procedure-el procedure ()           ())
    (coerce procedure procedure-el ()           ())
    (coerce procedure-el1 procedure ()          ())
    (coerce procedure procedure-el1 ()          ())

    ;; struct
    (coerce struct bool        ()               ((lambda (x) #t)))

    ;; tstruct
    (coerce tstruct bool       ()               ((lambda (x) #t)))

    ;; pair
    (coerce pair epair         (c-epair?)       ())
    (coerce pair bool          ()               ((lambda (x) #t)))

    ;; epair
    (coerce epair pair         ()               ())
    (coerce epair pair-nil     ()               ())
    (coerce epair bool         ()               ((lambda (x) #t)))

    ;; pair-nil
    (coerce pair-nil pair      (pair?)          ())
    (coerce pair pair-nil      ()               ())
    (coerce pair-nil epair     (epair?)         ())
    (coerce pair-nil nil       (null?)          ())
    (coerce pair-nil bool      ()               ((lambda (x) #t)))

    ;; nil
    (coerce nil pair-nil       ()               ())
    (coerce nil bool           ()               ((lambda (x) #t)))

    ;; cell
    (coerce cell bool          ()               ((lambda (x) #t)))

    ;; ports
    (coerce file input-port    ()               ($file->input-port))
    (coerce file output-port   ()               ($file->output-port))

    (coerce input-port bool    ()               ((lambda (x) #t)))

    (coerce output-port file   ()               ($output-port->file))
    (coerce output-port bool   ()               ((lambda (x) #t)))

    (coerce binary-port file   ()               ($binary-port->file))
    (coerce binary-port bool   ()               ((lambda (x) #t)))

    ;; process
    (coerce process bool       ()               ((lambda (x) #t)))

    ;; srfi4 vectors
    (coerce s8vector bool      ()               ((lambda (x) #t)))
    (coerce u8vector bool      ()               ((lambda (x) #t)))
    (coerce s16vector bool     ()               ((lambda (x) #t)))
    (coerce u16vector bool     ()               ((lambda (x) #t)))
    (coerce s32vector bool     ()               ((lambda (x) #t)))
    (coerce u32vector bool     ()               ((lambda (x) #t)))
    (coerce s64vector bool     ()               ((lambda (x) #t)))
    (coerce u64vector bool     ()               ((lambda (x) #t)))

    (coerce f32vector bool     ()               ((lambda (x) #t)))
    (coerce f64vector bool     ()               ((lambda (x) #t)))
    
    ;; socket
    (coerce socket bool        ()               ((lambda (x) #t)))

    ;; custom
    (coerce custom bool        ()               ((lambda (x) #t)))

    ;; date
    (coerce date bool          ()               ((lambda (x) #t)))

    ;; mutex
    (coerce mutex bool         ()               ((lambda (x) #t)))

    ;; condvar
    (coerce condvar bool       ()               ((lambda (x) #t)))

    ;; mmap
    (coerce mmap bool          ()               ((lambda (x) #t)))

    ;; opaque
    (coerce opaque bool        ()               ((lambda (x) #t))))
   (extern
    (macro $bool->bbool::bbool (::bool) "BBOOL")
    (macro $obj->bool::bool (::obj) "CBOOL")
    (macro $bbool->bool::bool (::bbool) "CBOOL")

    (macro $byte->bint::bint (::byte) "BINT")
    (macro $ubyte->bint::bint (::ubyte) "BINT")
    (macro $short->bint::bint (::short) "BINT")
    (macro $ushort->bint::bint (::ushort) "BINT")
    (macro $int->bint::bint (::int) "BINT")
    (macro $uint->bint::bint (::uint) "BINT")
    (macro $long->bint::bint (::long) "BINT")
    (macro $ulong->bint::bint (::ulong) "BINT")

    (macro $bint->byte::byte (::bint) "(signed char)CINT")
    (macro $bint->char::char (::bint) "(signed char)CINT")
    (macro $bint->ubyte::ubyte (::bint) "(unsigned char)CINT")
    (macro $bint->short::short (::bint) "(short)CINT")
    (macro $bint->ushort::ushort (::bint) "(unsigned short)CINT")
    (macro $bint->int::int (::bint) "CINT")
    (macro $bint->uint::uint (::bint) "(unsigned int)CINT")
    (macro $bint->long::long (::bint) "(long)CINT")
    (macro $bint->ulong::ulong (::bint) "(unsigned long)CINT")

    (macro $ubyte->byte::byte (::ubyte) "(signed char)")
    (macro $byte->ubyte::ubyte (::byte) "(unsigned char)")

    (macro $ubyte->short::short (::ubyte) "(short)")
    (macro $ubyte->ushort::ushort (::ubyte) "(unsigned short)")
    (macro $ubyte->int::int (::ubyte) "(int)")
    (macro $ubyte->uint::uint (::ubyte) "(unsigned int)")
    (macro $ubyte->long::long (::ubyte) "(long)")
    (macro $ubyte->ulong::ulong (::ubyte) "(unsigned long)")

    (macro $byte->short::short (::byte) "(short)")
    (macro $byte->ushort::ushort (::byte) "(unsigned short)")
    (macro $byte->int::int (::byte) "(int)")
    (macro $byte->uint::uint (::byte) "(unsigned int)")
    (macro $byte->long::long (::byte) "(long)")
    (macro $byte->ulong::ulong (::byte) "(unsigned long)")

    (macro $ubyte->short::short (::ubyte) "(short)")
    (macro $ubyte->ushort::ushort (::ubyte) "(unsigned short)")
    (macro $ubyte->int::int (::ubyte) "(int)")
    (macro $ubyte->uint::uint (::ubyte) "(unsigned int)")
    (macro $ubyte->long::long (::ubyte) "(long)")
    (macro $ubyte->ulong::ulong (::ubyte) "(unsigned long)")

    (macro $short->byte::byte (::short) "(signed char)")
    (macro $short->ubyte::ubyte (::short) "(unsigned char)")
    (macro $short->ushort::ushort (::short) "(unsigned short)")
    (macro $short->int::int (::short) "(int)")
    (macro $short->uint::uint (::short) "(unsigned int)")
    (macro $short->long::long (::short) "(long)")
    (macro $short->ulong::ulong (::short) "(unsigned long)")

    (macro $ushort->byte::byte (::ushort) "(signed char)")
    (macro $ushort->ubyte::ubyte (::ushort) "(unsigned char)")
    (macro $ushort->short::short (::ushort) "(short)")
    (macro $ushort->int::int (::ushort) "(int)")
    (macro $ushort->uint::uint (::ushort) "(unsigned int)")
    (macro $ushort->long::long (::ushort) "(long)")
    (macro $ushort->ulong::ulong (::ushort) "(unsigned long)")

    (macro $int->byte::byte (::int) "(signed char)")
    (macro $int->ubyte::ubyte (::int) "(unsigned char)")
    (macro $int->short::short (::int) "(short)")
    (macro $int->ushort::ushort (::int) "(unsigned short)")
    (macro $int->uint::uint (::int) "(unsigned int)")
    (macro $int->long::long (::int) "(long)")
    (macro $int->ulong::ulong (::int) "(unsigned long)")

    (macro $uint->byte::byte (::uint) "(signed char)")
    (macro $uint->ubyte::ubyte (::uint) "(unsigned char)")
    (macro $uint->short::short (::uint) "(short)")
    (macro $uint->ushort::ushort (::uint) "(unsigned short)")
    (macro $uint->int::uint (::uint) "(int)")
    (macro $uint->long::long (::uint) "(long)")
    (macro $uint->ulong::ulong (::uint) "(unsigned long)")

    (macro $long->byte::byte (::long) "(signed char)")
    (macro $long->char::char (::long) "(signed char)")
    (macro $long->ubyte::ubyte (::long) "(unsigned char)")
    (macro $long->short::short (::long) "(short)")
    (macro $long->ushort::ushort (::long) "(unsigned short)")
    (macro $long->int::int (::long) "(int)")
    (macro $long->uint::uint (::long) "(unsigned int)")
    (macro $long->ulong::ulong (::long) "(unsigned long)")

    (macro $ulong->byte::byte (::ulong) "(signed char)")
    (macro $ulong->char::char (::ulong) "(signed char)")
    (macro $ulong->ubyte::ubyte (::ulong) "(unsigned char)")
    (macro $ulong->short::short (::ulong) "(short)")
    (macro $ulong->ushort::ushort (::ulong) "(unsigned short)")
    (macro $ulong->int::int (::ulong) "(int)")
    (macro $ulong->uint::uint (::ulong) "(unsigned int)")
    (macro $ulong->long::long (::ulong) "(long)")

    (macro $uelong->elong::elong (::uelong) "(long)")
    (macro $elong->uelong::uelong (::elong) "(unsigned long)")

    (macro $ullong->llong::llong (::ullong) "(BGL_LONGLONG_T)")
    (macro $llong->ullong::ullong (::llong) "(unsigned BGL_LONGLONG_T)")

    ($string->bstring::bstring (::string) "string_to_bstring")
    (macro $bstring->string::string (::bstring) "BSTRING_TO_STRING")

    (macro $char->bchar::bchar (::char) "BCHAR")
    (macro $char->uchar::uchar (::char) "(unsigned char)")
    (macro $uchar->bchar::bchar (::uchar) "BCHAR")
    (macro $uchar->char::char (::uchar) "(char)")
    (macro $bchar->char::char (::bchar) "CCHAR")
    (macro $bchar->uchar::uchar (::bchar) "CCHAR")
    (macro $double->real::real (::double) "DOUBLE_TO_REAL")
    (macro $real->double::double (::real) "REAL_TO_DOUBLE")
    (macro $float->real::real (::float) "FLOAT_TO_REAL")
    (macro $real->float::float (::real) "REAL_TO_FLOAT")
    (macro $double->float::float (::double) "(float)")
    (macro $float->double::double (::float) "(double)")

    (macro $ucs2->bucs2::bucs2 (::ucs2) "BUCS2")
    (macro $bucs2->ucs2::ucs2 (::bucs2) "CUCS2")

    (macro $output-port->file::file (::output-port) "OUTPUT_PORT_TO_FILE")
    (macro $file->output-port::output-port (::file) "FILE_TO_OUTPUT_PORT")

    ($file->input-port::input-port (::file) "bgl_file_to_input_port")

    (macro $binary-port->file::file (::binary-port) "BINARY_PORT_TO_FILE")

    (macro $cobj->obj::obj (::cobj) "(obj_t)")
    ($obj->cobj::cobj (::obj) "obj_to_cobj")

    (macro $ubyte->char::char (::ubyte) "(char)")
    (macro $char->ubyte::ubyte (::char) "(unsigned char)")

    (macro $elong->belong::belong (::elong) "make_belong")
    (macro $uelong->belong::belong (::uelong) "make_belong")
    (macro $long->belong::belong (::long) "make_belong")
    (macro $long->elong::elong (::long) "LONG_TO_ELONG")
    (macro $long->uelong::uelong (::long) "LONG_TO_ELONG")
    (macro $elong->long::elong (::long) "ELONG_TO_LONG")
    (macro $uelong->long::elong (::long) "ELONG_TO_LONG")
    (macro $belong->elong::elong (::belong) "BELONG_TO_LONG")
    (macro $belong->uelong::elong (::belong) "BELONG_TO_LONG")
    (macro $belong->long::long (::belong) "BELONG_TO_LONG")

    (macro $llong->bllong::bllong (::llong) "make_bllong")
    (macro $ullong->bllong::bllong (::ullong) "make_bllong")
    (macro $long->bllong::bllong (::long) "LONG_TO_BLLONG")
    (macro $long->llong::llong (::long) "LONG_TO_LLONG")
    (macro $long->ullong::ullong (::llong) "LONG_TO_LLONG")
    (macro $llong->long::long (::llong) "LLONG_TO_LONG")
    (macro $ullong->long::elong (::ullong) "LLONG_TO_LONG")
    (macro $bllong->llong::llong (::bllong) "BLLONG_TO_LLONG")
    (macro $bllong->ullong::ullong (::bllong) "BLLONG_TO_LLONG")
    (macro $bllong->long::long (::bllong) "BLLONG_TO_LONG")

    (macro $obj->void*::void* (::foreign) "FOREIGN_TO_COBJ")
    ($void*->obj::foreign (::void*) "void_star_to_obj"))

   (java
    (abstract-class foreign
       (method static $bool->bbool::bbool (::bool) "BBOOL")
       (method static $obj->bool::bool (::obj) "CBOOL")
       (method static $bbool->bool::bool (::bbool) "CBOOL")

       (method static $byte->bint::bint (::byte) "BINT")
       (method static $ubyte->bint::bint (::ubyte) "BINT")
       (method static $short->bint::bint (::short) "BINT")
       (method static $ushort->bint::bint (::ushort) "BINT")
       (method static $int->bint::bint (::int) "BINT")
       (method static $uint->bint::bint (::uint) "BINT")
       (method static $long->bint::bint (::long) "BINT")
       (method static $ulong->bint::bint (::ulong) "BINT")

       (method static $bint->byte::byte (::bint) "BINT_TO_BYTE")
       (method static $bint->char::char (::bint) "BINT_TO_CHAR")
       (method static $bint->ubyte::ubyte (::bint) "BINT_TO_UBYTE")
       (method static $bint->short::short (::bint) "BINT_TO_SHORT")
       (method static $bint->ushort::ushort (::bint) "BINT_TO_USHORT")
       (method static $bint->int::int (::bint) "CINT")
       (method static $bint->uint::uint (::bint) "BINT_TO_UINT")
       (method static $bint->long::long (::bint) "<inlined>")
       (method static $bint->ulong::ulong (::bint) "BINT_TO_ULONG")

       (method static $ubyte->byte::byte (::ubyte) "UBYTE_TO_BYTE")
       (method static $ubyte->short::short (::ubyte) "UBYTE_TO_SHORT")
       (method static $ubyte->ushort::ushort (::ubyte) "UBYTE_TO_USHORT")
       (method static $ubyte->int::int (::ubyte) "UBYTE_TO_INT")
       (method static $ubyte->uint::uint (::ubyte) "UBYTE_TO_INT")
       (method static $ubyte->long::long (::ubyte) "UBYTE_TO_LONG")
       (method static $ubyte->ulong::ulong (::ubyte) "UBYTE_TO_ULONG")

       (method static $byte->ubyte::ubyte (::byte) "BYTE_TO_UBYTE")
       (method static $byte->short::short (::byte) "BYTE_TO_SHORT")
       (method static $byte->ushort::ushort (::byte) "BYTE_TO_USHORT")
       (method static $byte->int::int (::byte) "BYTE_TO_INT")
       (method static $byte->uint::uint (::byte) "BYTE_TO_UINT")
       (method static $byte->long::long (::byte) "BYTE_TO_LONG")
       (method static $byte->ulong::ulong (::byte) "BYTE_TO_ULONG")

       (method static $ubyte->short::short (::ubyte) "UBYTE_TO_SHORT")
       (method static $ubyte->ushort::ushort (::ubyte) "UBYTE_TO_USHORT")
       (method static $ubyte->int::int (::ubyte) "UBYTE_TO_INT")
       (method static $ubyte->uint::uint (::ubyte) "UBYTE_TO_UINT")
       (method static $ubyte->long::long (::ubyte) "UBYTE_TO_LONG")
       (method static $ubyte->ulong::ulong (::ubyte) "UBYTE_TO_ULONG")

       (method static $short->byte::byte (::short) "SHORT_TO_BYTE")
       (method static $short->ubyte::ubyte (::short) "SHORT_TO_UBYTE")
       (method static $short->ushort::ushort (::short) "SHORT_TO_USHORT")
       (method static $short->int::int (::short) "SHORT_TO_INT")
       (method static $short->uint::uint (::short) "SHORT_TO_UINT")
       (method static $short->long::long (::short) "SHORT_TO_LONG")
       (method static $short->ulong::ulong (::short) "SHORT_TO_ULONG")

       (method static $ushort->byte::byte (::ushort) "USHORT_TO_BYTE")
       (method static $ushort->ubyte::ubyte (::ushort) "USHORT_TO_UBYTE")
       (method static $ushort->short::short (::ushort) "USHORT_TO_SHORT")
       (method static $ushort->int::int (::ushort) "USHORT_TO_INT")
       (method static $ushort->uint::uint (::ushort) "USHORT_TO_UINT")
       (method static $ushort->long::long (::ushort) "USHORT_TO_LONG")
       (method static $ushort->ulong::ulong (::ushort) "USHORT_TO_ULONG")

       (method static $int->byte::byte (::int) "INT_TO_BYTE")
       (method static $int->ubyte::ubyte (::int) "INT_TO_UBYTE")
       (method static $int->short::short (::int) "INT_TO_SHORT")
       (method static $int->ushort::ushort (::int) "INT_TO_USHORT")
       (method static $int->uint::uint (::int) "INT_TO_UINT")
       (method static $int->long::long (::int) "<inlined>")
       (method static $int->ulong::ulong (::int) "INT_TO_ULONG")

       (method static $uint->byte::byte (::uint) "UINT_TO_BYTE")
       (method static $uint->ubyte::ubyte (::uint) "UINT_TO_UBYTE")
       (method static $uint->short::short (::uint) "UINT_TO_SHORT")
       (method static $uint->ushort::ushort (::uint) "UINT_TO_USHORT")
       (method static $uint->int::uint (::uint) "UINT_TO_INT")
       (method static $uint->long::long (::uint) "UINT_TO_LONG")
       (method static $uint->ulong::ulong (::uint) "UINT_TO_ULONG")

       (method static $long->byte::byte (::long) "LONG_TO_BYTE")
       (method static $long->ubyte::ubyte (::long) "LONG_TO_UBYTE")
       (method static $long->char::char (::long) "LONG_TO_CHAR")
       (method static $long->short::short (::long) "LONG_TO_SHORT")
       (method static $long->ushort::ushort (::long) "LONG_TO_USHORT")
       (method static $long->int::int (::long) "LONG_TO_INT")
       (method static $long->uint::uint (::long) "LONG_TO_UINT")
       (method static $long->ulong::ulong (::long) "LONG_TO_ULONG")

       (method static $ulong->byte::byte (::ulong) "ULONG_TO_BYTE")
       (method static $ulong->ubyte::ubyte (::ulong) "ULONG_TO_UBYTE")
       (method static $ulong->char::char (::ulong) "ULONG_TO_CHAR")
       (method static $ulong->short::short (::ulong) "ULONG_TO_SHORT")
       (method static $ulong->ushort::ushort (::ulong) "ULONG_TO_USHORT")
       (method static $ulong->int::int (::ulong) "ULONG_TO_INT")
       (method static $ulong->uint::uint (::ulong) "ULONG_TO_UINT")
       (method static $ulong->long::long (::ulong) "ULONG_TO_LONG")

       (method static $string->bstring::bstring (::string) "string_to_bstring")
       (method static $bstring->string::string (::bstring) "BSTRING_TO_STRING")

       (method static $char->bchar::bchar (::char) "BCHAR")
       (method static $char->uchar::uchar (::char) "CHAR_TO_UCHAR")
       (method static $uchar->bchar::bchar (::uchar) "BCHAR")
       (method static $uchar->char::char (::uchar) "UCHAR_TO_CHAR")
       (method static $bchar->char::char (::bchar) "CCHAR")
       (method static $bchar->uchar::uchar (::bchar) "BCHAR_TO_UCHAR")
       (method static $double->real::real (::double) "DOUBLE_TO_REAL")
       (method static $real->double::double (::real) "REAL_TO_DOUBLE")
       (method static $float->real::real (::float) "FLOAT_TO_REAL")
       (method static $real->float::float (::real) "REAL_TO_FLOAT")
       (method static $double->float::float (::double) "DOUBLE_TO_FLOAT")
       (method static $float->double::double (::float) "FLOAT_TO_DOUBLE")

       (method static $ucs2->bucs2::bucs2 (::ucs2) "BUCS2")
       (method static $bucs2->ucs2::ucs2 (::bucs2) "CUCS2")

       (method static $output-port->file::file (::output-port) "OUTPUT_PORT_TO_FILE")
       (method static $file->output-port::output-port (::file) "FILE_TO_OUTPUT_PORT")

       (method static $file->input-port::input-port (::file) "file_to_input_port")

       (method static $binary-port->file::file (::binary-port) "BINARY_PORT_TO_FILE")

       (method static $cobj->obj::obj (::cobj) "COBJ_TO_OBJ")
       (method static $obj->cobj::cobj (::obj) "obj_to_cobj")

       (method static $ubyte->char::char (::ubyte) "UBYTE_TO_CHAR")
       (method static $char->ubyte::ubyte (::char) "CHAR_TO_UBYTE")

       (method static $elong->belong::belong (::elong) "ELONG_TO_BELONG")
       (method static $long->belong::belong (::long) "LONG_TO_BELONG")
       (method static $long->elong::elong (::long) "LONG_TO_ELONG")
       (method static $elong->long::long (::elong) "ELONG_TO_LONG")
       (method static $belong->elong::elong (::belong) "BELONG_TO_ELONG")
       (method static $belong->long::long (::belong) "BELONG_TO_LONG")
       (method static $uelong->elong::elong (::uelong) "UELONG_TO_ELONG")
       (method static $elong->uelong::uelong (::elong) "ELONG_TO_UELONG")

       (method static $llong->bllong::bllong (::llong) "LLONG_TO_BLLONG")
       (method static $long->bllong::bllong (::long) "LONG_TO_BLLONG")
       (method static $long->llong::llong (::long) "LONG_TO_LLONG")
       (method static $long->ullong::ullong (::long) "LONG_TO_ULLONG")
       (method static $llong->long::long (::llong) "LLONG_TO_LONG")
       (method static $ullong->long::long (::ullong) "ULLONG_TO_LONG")
       (method static $ullong->bllong::bllong (::ullong) "ULLONG_TO_BLLONG")
       (method static $bllong->llong::llong (::bllong) "BLLONG_TO_LLONG")
       (method static $bllong->ullong::ullong (::bllong) "BLLONG_TO_ULLONG")
       (method static $bllong->long::long (::bllong) "BLLONG_TO_LONG")
       (method static $ullong->llong::llong (::ullong) "ULLONG_TO_LLONG")
       (method static $llong->ullong::ullong (::llong) "LLONG_TO_ULLONG")

       (method static $obj->void*::cobj (::void*) "FOREIGN_TO_COBJ")
       (method static $void*->obj::foreign (::void*) "void_star_to_obj")))

   (pragma
    ($bool->bbool side-effect-free nesting args-safe (effect))
    ($bbool->bool side-effect-free nesting args-safe (effect))
    ($obj->bool side-effect-free nesting args-safe (effect))
    ($byte->bint side-effect-free nesting args-safe (effect))
    ($ubyte->bint side-effect-free nesting args-safe (effect))
    ($short->bint side-effect-free nesting args-safe (effect))
    ($ushort->bint side-effect-free nesting args-safe (effect))
    ($int->bint side-effect-free nesting args-safe (effect))
    ($uint->bint side-effect-free nesting args-safe (effect))
    ($long->bint side-effect-free nesting args-safe (effect))
    ($ulong->bint side-effect-free nesting args-safe (effect))
    ($bint->byte side-effect-free nesting args-safe (effect))
    ($bint->ubyte side-effect-free nesting args-safe (effect))
    ($bint->short side-effect-free nesting args-safe (effect))
    ($bint->ushort side-effect-free nesting args-safe (effect))
    ($bint->int side-effect-free nesting args-safe (effect))
    ($bint->uint side-effect-free nesting args-safe (effect))
    ($bint->long side-effect-free nesting args-safe (effect))
    ($bint->ulong side-effect-free nesting args-safe (effect))
    ($ubyte->short side-effect-free nesting args-safe (effect))
    ($ubyte->ushort side-effect-free nesting args-safe (effect))
    ($ubyte->int side-effect-free nesting args-safe (effect))
    ($ubyte->uint side-effect-free nesting args-safe (effect))
    ($ubyte->long side-effect-free nesting args-safe (effect))
    ($ubyte->ulong side-effect-free nesting args-safe (effect))
    ($byte->short side-effect-free nesting args-safe (effect))
    ($byte->ushort side-effect-free nesting args-safe (effect))
    ($byte->int side-effect-free nesting args-safe (effect))
    ($byte->uint side-effect-free nesting args-safe (effect))
    ($byte->long side-effect-free nesting args-safe (effect))
    ($byte->ulong side-effect-free nesting args-safe (effect))
    ($short->byte side-effect-free nesting args-safe (effect))
    ($short->ubyte side-effect-free nesting args-safe (effect))
    ($short->ushort side-effect-free nesting args-safe (effect))
    ($short->int side-effect-free nesting args-safe (effect))
    ($short->uint side-effect-free nesting args-safe (effect))
    ($short->long side-effect-free nesting args-safe (effect))
    ($short->ulong side-effect-free nesting args-safe (effect))
    ($ushort->byte side-effect-free nesting args-safe (effect))
    ($ushort->ubyte side-effect-free nesting args-safe (effect))
    ($ushort->short side-effect-free nesting args-safe (effect))
    ($ushort->int side-effect-free nesting args-safe (effect))
    ($ushort->uint side-effect-free nesting args-safe (effect))
    ($ushort->long side-effect-free nesting args-safe (effect))
    ($ushort->ulong side-effect-free nesting args-safe (effect))
    ($int->byte side-effect-free nesting args-safe (effect))
    ($int->ubyte side-effect-free nesting args-safe (effect))
    ($int->short side-effect-free nesting args-safe (effect))
    ($int->ushort side-effect-free nesting args-safe (effect))
    ($int->uint side-effect-free nesting args-safe (effect))
    ($int->long side-effect-free nesting args-safe (effect))
    ($int->ulong side-effect-free nesting args-safe (effect))
    ($uint->byte side-effect-free nesting args-safe (effect))
    ($uint->ubyte side-effect-free nesting args-safe (effect))
    ($uint->short side-effect-free nesting args-safe (effect))
    ($uint->ushort side-effect-free nesting args-safe (effect))
    ($uint->int side-effect-free nesting args-safe (effect))
    ($uint->long side-effect-free nesting args-safe (effect))
    ($uint->ulong side-effect-free nesting args-safe (effect))
    ($long->byte side-effect-free nesting args-safe (effect))
    ($long->ubyte side-effect-free nesting args-safe (effect))
    ($long->short side-effect-free nesting args-safe (effect))
    ($long->ushort side-effect-free nesting args-safe (effect))
    ($long->int side-effect-free nesting args-safe (effect))
    ($long->uint side-effect-free nesting args-safe (effect))
    ($long->ulong side-effect-free nesting args-safe (effect))
    ($ulong->byte side-effect-free nesting args-safe (effect))
    ($ulong->ubyte side-effect-free nesting args-safe (effect))
    ($ulong->short side-effect-free nesting args-safe (effect))
    ($ulong->ushort side-effect-free nesting args-safe (effect))
    ($ulong->int side-effect-free nesting args-safe (effect))
    ($ulong->uint side-effect-free nesting args-safe (effect))
    ($ulong->long side-effect-free nesting args-safe (effect))
    ($bstring->string side-effect-free nesting args-safe (effect))
    ($string->bstring side-effect-free nesting args-safe (effect))
    ($char->bchar side-effect-free nesting args-safe (effect))
    ($char->uchar side-effect-free nesting args-safe (effect))
    ($uchar->bchar side-effect-free nesting args-safe (effect))
    ($uchar->char side-effect-free nesting args-safe (effect))
    ($bchar->char side-effect-free nesting args-safe (effect))
    ($bchar->uchar side-effect-free nesting args-safe (effect))
    ($ucs2->bucs2 side-effect-free nesting args-safe (effect))
    ($bucs2->ucs2 side-effect-free nesting args-safe (effect))
    ($double->real side-effect-free args-safe (effect))
    ($real->double side-effect-free args-safe (effect))
    ($double->float side-effect-free nesting args-safe (effect))
    ($float->double side-effect-free nesting args-safe (effect))
    ($float->real side-effect-free args-safe (effect))
    ($real->float side-effect-free nesting args-safe (effect))
    ($cobj->obj side-effect-free nesting args-safe (effect))
    ($obj->cobj side-effect-free nesting args-safe (effect))
    ($ubyte->char side-effect-free nesting args-safe (effect))
    ($char->ubyte side-effect-free nesting args-safe (effect))
    ($long->belong side-effect-free args-safe (effect))
    ($llong->bllong side-effect-free args-safe (effect))
    ($ullong->bllong side-effect-free args-safe (effect))
    ($bllong->llong side-effect-free nesting args-safe (effect))
    ($bllong->ullong side-effect-free nesting args-safe (effect))
    ($obj->void* side-effect-free nesting args-safe (effect))
    ($void*->obj side-effect-free nesting args-safe (effect))))

