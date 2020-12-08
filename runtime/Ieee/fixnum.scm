;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/fixnum.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 10:06:37 1995                          */
;*    Last change :  Sun Jan  5 18:36:39 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.5. Numbers (page 18, r4) The `fixnum' functions                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_numbers_6_5_fixnum
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __r4_equivalence_6_2
	    __r4_numbers_6_5
	    __r4_numbers_6_5_flonum
	    __r4_booleans_6_1
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    
	    __evenv)

   (include "Ieee/bignum.sch")

   (extern (macro c-fixnum?::bool (::obj) "INTEGERP")
	   (macro c-elong?::bool (::obj) "ELONGP")
	   (macro c-llong?::bool (::obj) "LLONGP")
	   (macro $int8?::bool (::obj) "BGL_INT8P")
	   (macro $uint8?::bool (::obj) "BGL_UINT8P")
	   (macro $int16?::bool (::obj) "BGL_INT16P")
	   (macro $uint16?::bool (::obj) "BGL_UINT16P")
	   (macro $int32?::bool (::obj) "BGL_INT32P")
	   (macro $uint32?::bool (::obj) "BGL_UINT32P")
	   (macro $int64?::bool (::obj) "BGL_INT64P")
	   (macro $uint64?::bool (::obj) "BGL_UINT64P")
	   (macro $minvalfx::long "BGL_LONG_MIN")
	   (macro $maxvalfx::long "BGL_LONG_MAX")
	   (macro $minvalelong::elong "LONG_MIN")
	   (macro $maxvalelong::elong "LONG_MAX")
	   (macro $minvalllong::llong "BGL_LONGLONG_MIN")
	   (macro $maxvalllong::llong "BGL_LONGLONG_MAX")
	   (macro $elong->llong::llong (::elong) "(BGL_LONGLONG_T)")
	   (macro $llong->elong::elong (::llong)  "(long)")
	   
	   (macro $elong->int8::uint8 (::elong) "(uint8_t)")
	   (macro $elong->uint8::uint8 (::elong) "(uint8_t)")
	   (macro $uint8->elong::elong (::uint8)  "(long)")
	   (macro $int8->elong::elong (::int8)  "(long)")
	   
	   (macro $elong->int16::uint16 (::elong) "(uint16_t)")
	   (macro $elong->uint16::uint16 (::elong) "(uint16_t)")
	   (macro $uint16->elong::elong (::uint16)  "(long)")
	   (macro $int16->elong::elong (::int16)  "(long)")
	   
	   (macro $elong->int32::int32 (::elong) "(int32_t)")
	   (macro $int32->elong::elong (::int32)  "(long)")
	   (macro $elong->uint32::uint32 (::elong) "(uint32_t)")
	   (macro $int32->llong::llong (::int32)  "(BGL_LONGLONG_T)")
	   (macro $uint32->elong::elong (::uint32)  "(long)")
	   (macro $uint32->llong::llong (::uint32)  "(BGL_LONGLONG_T)")
	   (macro $uint32->uint64::uint64 (::uint32)  "(uint64_t)")
	   (macro $llong->uint32::uint32 (::llong)  "(long)")
	   (macro $llong->int32::int32 (::llong)  "(long)")
	   
	   (macro $int64->elong::elong (::int64)  "(long)")
	   (macro $uint64->elong::elong (::uint64)  "(long)")
	   (macro $elong->int64::int64 (::elong) "(int64_t)")
	   (macro $int64->llong::llong (::int64)  "(BGL_LONGLONG_T)")
	   (macro $llong->int64::int64 (::llong) "(int64_t)")
	   (macro $llong->uint64::uint64 (::llong) "(uint64_t)")
	   (macro $uint64->llong::llong (::uint64)  "(BGL_LONGLONG_T)")
	   
	   (infix macro c-=fx::bool (::long ::long) "==")
	   (infix macro c-=elong::bool (::elong ::elong) "==")
	   (infix macro c-=llong::bool (::llong ::llong) "==")
	   (infix macro $=s8::bool (::int8 ::int8) "==")
	   (infix macro $=u8::bool (::uint8 ::uint8) "==")
	   (infix macro $=s16::bool (::int16 ::int16) "==")
	   (infix macro $=u16::bool (::uint16 ::uint16) "==")
	   (infix macro $=s32::bool (::int32 ::int32) "==")
	   (infix macro $=u32::bool (::uint32 ::uint32) "==")
	   (infix macro $=s64::bool (::int64 ::int64) "==")
	   (infix macro $=u64::bool (::uint64 ::uint64) "==")
	   (infix macro c-<fx::bool (::long ::long) "<")
	   (infix macro c-<elong::bool (::elong ::elong) "<")
	   (infix macro c-<llong::bool (::llong ::llong) "<")
	   (infix macro $<s8::bool (::int8 ::int8) "<")
	   (infix macro $<u8::bool (::uint8 ::uint8) "<")
	   (infix macro $<s16::bool (::int16 ::int16) "<")
	   (infix macro $<u16::bool (::uint16 ::uint16) "<")
	   (infix macro $<s32::bool (::int32 ::int32) "<")
	   (infix macro $<u32::bool (::uint32 ::uint32) "<")
	   (infix macro $<s64::bool (::int64 ::int64) "<")
	   (infix macro $<u64::bool (::uint64 ::uint64) "<")
	   (infix macro c-<=fx::bool (::long ::long) "<=")
	   (infix macro c-<=elong::bool (::elong ::elong) "<=")
	   (infix macro c-<=llong::bool (::llong ::llong) "<=")
	   (infix macro c-<fx::bool (::long ::long) "<")
	   (infix macro c-<elong::bool (::elong ::elong) "<")
	   (infix macro c-<llong::bool (::llong ::llong) "<")
	   (infix macro $<=s8::bool (::int8 ::int8) "<=")
	   (infix macro $<=u8::bool (::uint8 ::uint8) "<=")
	   (infix macro $<=s16::bool (::int16 ::int16) "<=")
	   (infix macro $<=u16::bool (::uint16 ::uint16) "<=")
	   (infix macro $<=s32::bool (::int32 ::int32) "<=")
	   (infix macro $<=u32::bool (::uint32 ::uint32) "<=")
	   (infix macro $<=s64::bool (::int64 ::int64) "<=")
	   (infix macro $<=u64::bool (::uint64 ::uint64) "<=")
	   (infix macro c->fx::bool (::long ::long) ">")
	   (infix macro c->elong::bool (::elong ::elong) ">")
	   (infix macro c->llong::bool (::llong ::llong) ">")
	   (infix macro $>s8::bool (::int8 ::int8) ">")
	   (infix macro $>u8::bool (::uint8 ::uint8) ">")
	   (infix macro $>s16::bool (::int16 ::int16) ">")
	   (infix macro $>u16::bool (::uint16 ::uint16) ">")
	   (infix macro $>s32::bool (::int32 ::int32) ">")
	   (infix macro $>u32::bool (::uint32 ::uint32) ">")
	   (infix macro $>s64::bool (::int64 ::int64) ">")
	   (infix macro $>u64::bool (::uint64 ::uint64) ">")
	   (infix macro c->=fx::bool (::long ::long) ">=")
	   (infix macro c->=elong::bool (::elong ::elong) ">=")
	   (infix macro c->=llong::bool (::llong ::llong) ">=")
	   (infix macro $>=s8::bool (::int8 ::int8) ">=")
	   (infix macro $>=u8::bool (::uint8 ::uint8) ">=")
	   (infix macro $>=s16::bool (::int16 ::int16) ">=")
	   (infix macro $>=u16::bool (::uint16 ::uint16) ">=")
	   (infix macro $>=s32::bool (::int32 ::int32) ">=")
	   (infix macro $>=u32::bool (::uint32 ::uint32) ">=")
	   (infix macro $>=s64::bool (::int64 ::int64) ">=")
	   (infix macro $>=u64::bool (::uint64 ::uint64) ">=")
	   (macro c-evenfx?::bool (::long) "EVENP_FX")
	   (macro c-oddfx?::bool (::long) "ODDP_FX")
	   (infix macro c-+fx::long (::long ::long) "+")
	   (infix macro c-+elong::elong (::elong ::elong) "+")
	   (infix macro c-+llong::llong (::llong ::llong) "+")
	   (infix macro $+s8::int8 (::int8 ::int8) "+")
	   (infix macro $+u8::uint8 (::uint8 ::uint8) "+")
	   (infix macro $+s16::int16 (::int16 ::int16) "+")
	   (infix macro $+u16::uint16 (::uint16 ::uint16) "+")
	   (infix macro $+s32::int32 (::int32 ::int32) "+")
	   (infix macro $+u32::uint32 (::uint32 ::uint32) "+")
	   (infix macro $+s64::int64 (::int64 ::int64) "+")
	   (infix macro $+u64::uint64 (::uint64 ::uint64) "+")
	   (infix macro c--fx::long (::long ::long) "-")
	   (infix macro c--elong::long (::elong ::elong) "-")
	   (infix macro c--llong::llong (::llong ::llong) "-")
	   (infix macro $-s8::int8 (::int8 ::int8) "-")
	   (infix macro $-u8::uint8 (::uint8 ::uint8) "-")
	   (infix macro $-s16::int16 (::int16 ::int16) "-")
	   (infix macro $-u16::uint16 (::uint16 ::uint16) "-")
	   (infix macro $-s32::int32 (::int32 ::int32) "-")
	   (infix macro $-u32::uint32 (::uint32 ::uint32) "-")
	   (infix macro $-s64::int64 (::int64 ::int64) "-")
	   (infix macro $-u64::uint64 (::uint64 ::uint64) "-")
	   (infix macro c-*fx::long (::long ::long) "*")
	   (infix macro c-*elong::elong (::elong ::elong) "*")
	   (infix macro c-*llong::llong (::llong ::llong) "*")
	   (infix macro $*s8::int8 (::int8 ::int8) "*")
	   (infix macro $*u8::uint8 (::uint8 ::uint8) "*")
	   (infix macro $*s16::int16 (::int16 ::int16) "*")
	   (infix macro $*u16::uint16 (::uint16 ::uint16) "*")
	   (infix macro $*s32::int32 (::int32 ::int32) "*")
	   (infix macro $*u32::uint32 (::uint32 ::uint32) "*")
	   (infix macro $*s64::int64 (::int64 ::int64) "*")
	   (infix macro $*u64::uint64 (::uint64 ::uint64) "*")
	   (infix macro c-/fx::long (::long ::long) "/")
	   (infix macro c-/elong::elong (::elong ::elong) "/")
	   (infix macro c-/llong::llong (::llong ::llong) "/")
	   (infix macro $/s8::int8 (::int8 ::int8) "/")
	   (infix macro $/u8::uint8 (::uint8 ::uint8) "/")
	   (infix macro $/s16::int16 (::int16 ::int16) "/")
	   (infix macro $/u16::uint16 (::uint16 ::uint16) "/")
	   (infix macro $/s32::int32 (::int32 ::int32) "/")
	   (infix macro $/u32::uint32 (::uint32 ::uint32) "/")
	   (infix macro $/s64::int64 (::int64 ::int64) "/")
	   (infix macro $/u64::uint64 (::uint64 ::uint64) "/")
	   (macro c-negfx::long (::long) "NEG")
	   (macro c-negelong::elong (::elong) "NEG")
	   (macro c-negllong::llong (::llong) "NEG")
	   (infix macro c-quotientfx::long (::long ::long) "/")
	   (infix macro c-quotientelong::elong (::elong ::elong) "/")
	   (infix macro c-quotientllong::llong (::llong ::llong) "/")
	   (infix macro $quotients8::int8 (::int8 ::int8) "/")
	   (infix macro $quotientu8::uint8 (::uint8 ::int8) "/")
	   (infix macro $quotients16::int16 (::int16 ::int16) "/")
	   (infix macro $quotientu16::uint16 (::uint16 ::int16) "/")
	   (infix macro $quotients32::int32 (::int32 ::int32) "/")
	   (infix macro $quotientu32::uint32 (::uint32 ::int32) "/")
	   (infix macro $quotients64::int64 (::int64 ::int64) "/")
	   (infix macro $quotientu64::uint64 (::uint64 ::int64) "/")
	   (infix macro c-remainderfx::long (::long ::long) "%")
	   (infix macro c-remainderelong::elong (::elong ::elong) "%")
	   (infix macro c-remainderllong::llong (::llong ::llong) "%")
	   (infix macro $remainders8::int8 (::int8 ::int8) "%")
	   (infix macro $remainderu8::uint8 (::uint8 ::int8) "%")
	   (infix macro $remainders16::int16 (::int16 ::int16) "%")
	   (infix macro $remainderu16::uint16 (::uint16 ::int16) "%")
	   (infix macro $remainders32::int32 (::int32 ::int32) "%")
	   (infix macro $remainderu32::uint32 (::uint32 ::int32) "%")
	   (infix macro $remainders64::int64 (::int64 ::int64) "%")
	   (infix macro $remainderu64::uint64 (::uint64 ::int64) "%")
	   (macro strtol::long (::string ::long ::long) "BGL_STRTOL")
	   (macro $strtoul::long (::string ::long ::long) "BGL_STRTOUL")
	   (macro strtoel::elong (::string ::long ::long) "BGL_STRTOL")
	   (macro $strtoeul::elong (::string ::long ::long) "BGL_STRTOUL")
	   (macro strtoll::llong (::string ::long ::long) "BGL_STRTOLL")
	   (macro $strtoull::llong (::string ::long ::long) "BGL_STRTOULL")
	   (c-fixnum->string::bstring  (::long ::long) "integer_to_string")
	   
	   ($integer->string/padding::bstring  (::long ::long ::long) "integer_to_string_padding")
	   (c-elong->string::bstring  (::elong ::long) "integer_to_string")
	   (c-llong->string::bstring  (::llong ::long) "llong_to_string")
	   ($unsigned->string::bstring  (::long ::long) "unsigned_to_string")
	   ($uelong->string::bstring  (::elong ::long) "unsigned_to_string")
	   ($ullong->string::bstring  (::llong ::long) "ullong_to_string")
	   (macro $rand::int () "rand"))

   ;; tagged fixnum operation, for the C backend only
   (extern  (macro $addfx::bint (::bint ::bint) "ADDFX")
	    (macro $subfx::bint (::bint ::bint) "SUBFX")
	    (macro $ltfx::bint (::bint ::bint) "LTFX")
	    (macro $lefx::bint (::bint ::bint) "LEFX")
	    (macro $gtfx::bint (::bint ::bint) "GTFX")
	    (macro $gefx::bint (::bint ::bint) "GEFX")
	    (macro $egfx::bint (::bint ::bint) "EGFX"))

   (java    (class foreign
	       (field static $minvalfx::long "MIN_VALUE_FX")
	       (field static $maxvalfx::long "MAX_VALUE_FX")
	       (field static $minvalelong::elong "MIN_VALUE_ELONG")
	       (field static $maxvalelong::elong "MAX_VALUE_ELONG")
	       (field static $minvalllong::llong "MIN_VALUE_ELONG")
	       (field static $maxvalllong::llong "MAX_VALUE_ELONG")
	       (method static c-fixnum?::bool (::obj)
		  "INTEGERP")
	       (method static c-elong?::bool (::obj)
		  "ELONGP")
	       (method static c-llong?::bool (::obj)
		  "LLONGP")
	       (method static $int8?::bool (::obj)
		  "BGL_INT8P")
	       (method static $uint8?::bool (::obj)
		  "BGL_INT8P")
	       (method static $int16?::bool (::obj)
		  "BGL_INT16P")
	       (method static $uint16?::bool (::obj)
		  "BGL_INT16P")
	       (method static $int32?::bool (::obj)
		  "BGL_INT32P")
	       (method static $uint32?::bool (::obj)
		  "BGL_INT32P")
	       (method static $int64?::bool (::obj)
		  "BGL_INT64P")
	       (method static $uint64?::bool (::obj)
		  "BGL_INT64P")
	       (method static $elong->llong::llong (::elong)
		  "ELONG_TO_LLONG")
	       (method static $llong->elong::elong (::llong)
		  "LLONG_TO_ELONG")

	       (method static $elong->int8::int8 (::elong)
		  "ELONG_TO_INT8")
	       (method static $int8->elong::elong (::int8)
		  "INT8_TO_ELONG")
	       (method static $elong->uint8::uint8 (::elong)
		  "ELONG_TO_INT8")
	       (method static $uint8->elong::elong (::uint8)
		  "INT8_TO_ELONG")
	       (method static $elong->int16::int16 (::elong)
		  "ELONG_TO_INT16")
	       (method static $int16->elong::elong (::int16)
		  "INT16_TO_ELONG")
	       (method static $elong->uint16::uint16 (::elong)
		  "ELONG_TO_INT16")
	       (method static $uint16->elong::elong (::uint16)
		  "INT16_TO_ELONG")
	       (method static $elong->int32::int32 (::elong)
		  "ELONG_TO_INT32")
	       (method static $int32->elong::elong (::int32)
		  "INT32_TO_ELONG")
	       (method static $elong->uint32::uint32 (::elong)
		  "ELONG_TO_INT32")
	       (method static $uint32->elong::elong (::uint32)
		  "INT32_TO_ELONG")
	       (method static $int32->llong::llong (::int32)
		  "INT32_TO_LLONG")
	       (method static $uint32->llong::llong (::uint32)
		  "INT32_TO_LLONG")
	       (method static $uint32->uint64::uint64 (::uint32)
		  "INT32_TO_LLONG")
	       (method static $llong->int32::uint32 (::llong)
		  "LLONG_TO_INT32")
	       (method static $llong->uint32::uint32 (::llong)
		  "LLONG_TO_INT32")
	       (method static $llong->int64::int64 (::llong)
		  "LLONG_TO_INT64")
	       (method static $int64->elong::elong (::int64)
		  "INT64_TO_ELONG")
	       (method static $uint64->elong::elong (::uint64)
		  "INT64_TO_ELONG")
	       (method static $elong->sing64::int64 (::elong)
		  "ELONG_TO_INT64")
	       (method static $int64->llong::llong (::int64)
		  "INT64_TO_LLONG")
	       (method static $llong->uint64::uint64 (::llong)
		  "LLONG_TO_INT64")
	       (method static $uint64->llong::llong (::uint64)
		  "INT64_TO_LLONG")

 	       (method static c-=fx::bool (::long ::long)
		  "EQ_FX")
	       (method static c-=elong::bool (::elong ::elong)
		  "EQ_ELONG")
	       (method static c-=llong::bool (::llong ::llong)
		  "EQ_LLONG")
	       (method static $=s8::bool (::int8 ::int8)
		  "EQ_INT8")
	       (method static $=u8::bool (::uint8 ::uint8)
		  "EQ_INT8")
	       (method static $=s16::bool (::int16 ::int16)
		  "EQ_INT16")
	       (method static $=u16::bool (::uint16 ::uint16)
		  "EQ_INT16")
	       (method static $=s32::bool (::int32 ::int32)
		  "EQ_INT32")
	       (method static $=u32::bool (::uint32 ::uint32)
		  "EQ_INT32")
	       (method static $=s64::bool (::int64 ::int64)
		  "EQ_INT64")
	       (method static $=u64::bool (::uint64 ::uint64)
		  "EQ_INT64")
	       (method static c-<fx::bool (::long ::long)
		  "LT_FX")
	       (method static c-<elong::bool (::elong ::elong)
		  "LT_ELONG")
	       (method static c-<llong::bool (::llong ::llong)
		  "LT_LLONG")
	       (method static $<s8::bool (::int8 ::int8)
		  "LT_INT8")
	       (method static $<u8::bool (::uint8 ::uint8)
		  "LT_INT8")
	       (method static $<s16::bool (::int16 ::int16)
		  "LT_INT16")
	       (method static $<u16::bool (::uint16 ::uint16)
		  "LT_INT16")
	       (method static $<s32::bool (::int32 ::int32)
		  "LT_INT32")
	       (method static $<u32::bool (::uint32 ::uint32)
		  "LT_INT32")
	       (method static $<s64::bool (::int64 ::int64)
		  "LT_INT64")
	       (method static $<u64::bool (::uint64 ::uint64)
		  "LT_INT64")
	       (method static c-<=fx::bool (::long ::long)
		  "LE_FX")
	       (method static c-<=elong::bool (::elong ::elong)
		  "LE_ELONG")
	       (method static c-<=llong::bool (::llong ::llong)
		  "LE_LLONG")
	       (method static $<=s8::bool (::int8 ::int8)
		  "LE_INT8")
	       (method static $<=u8::bool (::uint8 ::uint8)
		  "LE_INT8")
	       (method static $<=s16::bool (::int16 ::int16)
		  "LE_INT16")
	       (method static $<=u16::bool (::uint16 ::uint16)
		  "LE_INT16")
	       (method static $<=s32::bool (::int32 ::int32)
		  "LE_INT32")
	       (method static $<=u32::bool (::uint32 ::uint32)
		  "LE_INT32")
	       (method static $<=s64::bool (::int64 ::int64)
		  "LE_INT64")
	       (method static $<=u64::bool (::uint64 ::uint64)
		  "LE_INT64")
	       (method static c->fx::bool (::long ::long)
		  "GT_FX")
	       (method static c->elong::bool (::elong ::elong)
		  "GT_ELONG")
	       (method static c->llong::bool (::llong ::llong)
		  "GT_LLONG")
	       (method static $>s8::bool (::int8 ::int8)
		  "GT_INT8")
	       (method static $>u8::bool (::uint8 ::uint8)
		  "GT_INT8")
	       (method static $>s16::bool (::int16 ::int16)
		  "GT_INT16")
	       (method static $>u16::bool (::uint16 ::uint16)
		  "GT_INT16")
	       (method static $>s32::bool (::int32 ::int32)
		  "GT_INT32")
	       (method static $>u32::bool (::uint32 ::uint32)
		  "GT_INT32")
	       (method static $>s64::bool (::int64 ::int64)
		  "GT_INT64")
	       (method static $>u64::bool (::uint64 ::uint64)
		  "GT_INT64")
	       (method static c->=fx::bool (::long ::long)
		  "GE_FX")
	       (method static c->=elong::bool (::elong ::elong)
		  "GE_ELONG")
	       (method static c->=llong::bool (::llong ::llong)
		  "GE_LLONG")
	       (method static $>=s8::bool (::int8 ::int8)
		  "GE_INT8")
	       (method static $>=u8::bool (::uint8 ::uint8)
		  "GE_INT8")
	       (method static $>=s16::bool (::int16 ::int16)
		  "GE_INT16")
	       (method static $>=u16::bool (::uint16 ::uint16)
		  "GE_INT16")
	       (method static $>=s32::bool (::int32 ::int32)
		  "GE_INT32")
	       (method static $>=u32::bool (::uint32 ::uint32)
		  "GE_INT32")
	       (method static $>=s64::bool (::int64 ::int64)
		  "GE_INT64")
	       (method static $>=u64::bool (::uint64 ::uint64)
		  "GE_INT64")
	       (method static c-evenfx?::bool (::long)
		  "EVENP_FX")
	       (method static c-oddfx?::bool (::long)
		  "ODDP_FX")
	       (method static c-+fx::long (::long ::long)
		  "PLUS_FX")
	       (method static c-+elong::elong (::elong ::elong)
		  "PLUS_ELONG")
	       (method static c-+llong::llong (::llong ::llong)
		  "PLUS_LLONG")
	       (method static $+s8::int8 (::int8 ::int8)
		  "PLUS_S8")
	       (method static $+u8::uint8 (::uint8 ::uint8)
		  "PLUS_S8")
	       (method static $+s16::int16 (::int16 ::int16)
		  "PLUS_S16")
	       (method static $+u16::uint16 (::uint16 ::uint16)
		  "PLUS_S16")
	       (method static $+s32::int32 (::int32 ::int32)
		  "PLUS_S32")
	       (method static $+u32::uint32 (::uint32 ::uint32)
		  "PLUS_S32")
	       (method static $+s64::int64 (::int64 ::int64)
		  "PLUS_S64")
	       (method static $+u64::uint64 (::uint64 ::uint64)
		  "PLUS_S64")
 	       (method static c--fx::long (::long ::long)
		  "MINUS_FX")
	       (method static c--elong::elong (::elong ::elong)
		  "MINUS_ELONG")
	       (method static c--llong::llong (::llong ::llong)
		  "MINUS_LLONG")
	       (method static $-s8::int8 (::int8 ::int8)
		  "MINUS_S8")
	       (method static $-u8::uint8 (::uint8 ::uint8)
		  "MINUS_S8")
	       (method static $-s16::int16 (::int16 ::int16)
		  "MINUS_S16")
	       (method static $-u16::uint16 (::uint16 ::uint16)
		  "MINUS_S16")
	       (method static $-s32::int32 (::int32 ::int32)
		  "MINUS_S32")
	       (method static $-u32::uint32 (::uint32 ::uint32)
		  "MINUS_S32")
	       (method static $-s64::int64 (::int64 ::int64)
		  "MINUS_S64")
	       (method static $-u64::uint64 (::uint64 ::uint64)
		  "MINUS_S64")
 	       (method static c-*fx::long (::long ::long)
		  "MUL_FX")
	       (method static c-*elong::elong (::elong ::elong)
		  "MUL_ELONG")
	       (method static c-*llong::llong (::llong ::llong)
		  "MUL_LLONG")
	       (method static $*s8::int8 (::int8 ::int8)
		  "MUL_S8")
	       (method static $*u8::uint8 (::uint8 ::uint8)
		  "MUL_S8")
	       (method static $*s16::int16 (::int16 ::int16)
		  "MUL_S16")
	       (method static $*u16::uint16 (::uint16 ::uint16)
		  "MUL_S16")
	       (method static $*s32::int32 (::int32 ::int32)
		  "MUL_S32")
	       (method static $*u32::uint32 (::uint32 ::uint32)
		  "MUL_S32")
	       (method static $*s64::int64 (::int64 ::int64)
		  "MUL_S64")
	       (method static $*u64::uint64 (::uint64 ::uint64)
		  "MUL_S64")
 	       (method static c-/fx::long (::long ::long)
		  "DIV_FX")
	       (method static c-/elong::elong (::elong ::elong)
		  "DIV_ELONG")
	       (method static c-/llong::llong (::llong ::llong)
		  "DIV_LLONG")
	       (method static $/s8::int8 (::int8 ::int8)
		  "DIV_S8")
	       (method static $/u8::uint8 (::uint8 ::uint8)
		  "DIV_S8")
	       (method static $/s16::int16 (::int16 ::int16)
		  "DIV_S16")
	       (method static $/u16::uint16 (::uint16 ::uint16)
		  "DIV_S16")
	       (method static $/s32::int32 (::int32 ::int32)
		  "DIV_S32")
	       (method static $/u32::uint32 (::uint32 ::uint32)
		  "DIV_S32")
	       (method static $/s64::int64 (::int64 ::int64)
		  "DIV_S64")
	       (method static $/u64::uint64 (::uint64 ::uint64)
		  "DIV_S64")
 	       (method static c-negfx::long (::long)
		  "NEG_FX")
	       (method static c-negelong::elong (::elong)
		  "NEG_ELONG")
	       (method static c-negllong::llong (::llong)
		  "NEG_LLONG")
	       (method static c-quotientfx::long (::long ::long)
		  "QUOTIENT_FX")
	       (method static c-quotientelong::elong (::elong ::elong)
		  "QUOTIENT_ELONG")
	       (method static c-quotientllong::llong (::llong ::llong)
		  "QUOTIENT_LLONG")
	       (method static $quotients8::int8 (::int8 ::int8)
		  "QUOTIENT_S8")
	       (method static $quotientu8::uint8 (::uint8 ::uint8)
		  "QUOTIENT_S8")
	       (method static $quotients16::int16 (::int16 ::int16)
		  "QUOTIENT_S16")
	       (method static $quotientu16::uint16 (::uint16 ::uint16)
		  "QUOTIENT_S16")
	       (method static $quotients32::int32 (::int32 ::int32)
		  "QUOTIENT_S32")
	       (method static $quotientu32::uint32 (::uint32 ::uint32)
		  "QUOTIENT_S32")
	       (method static $quotients64::int64 (::int64 ::int64)
		  "QUOTIENT_S64")
	       (method static $quotientu64::uint64 (::uint64 ::uint64)
		  "QUOTIENT_S64")
	       (method static c-remainderfx::long (::long ::long)
		  "REMAINDER_FX")
	       (method static c-remainderelong::elong (::elong ::elong)
		  "REMAINDER_ELONG")
	       (method static c-remainderllong::llong (::llong ::llong)
		  "REMAINDER_LLONG")
	       (method static $remainders8::int8 (::int8 ::int8)
		  "REMAINDER_S8")
	       (method static $remainderu8::uint8 (::uint8 ::uint8)
		  "REMAINDER_S8")
	       (method static $remainders16::int16 (::int16 ::int16)
		  "REMAINDER_S16")
	       (method static $remainderu16::uint16 (::uint16 ::uint16)
		  "REMAINDER_S16")
	       (method static $remainders32::int32 (::int32 ::int32)
		  "REMAINDER_S32")
	       (method static $remainderu32::uint32 (::uint32 ::uint32)
		  "REMAINDER_S32")
	       (method static $remainders64::int64 (::int64 ::int64)
		  "REMAINDER_S64")
	       (method static $remainderu64::uint64 (::uint64 ::uint64)
		  "REMAINDER_S64")
	       (method static strtol::long (::string ::long ::long)
		  "strtol")
	       (method static $strtoul::long (::string ::long ::long)
		  "strtoul")
	       (method static strtoel::elong (::string ::long ::long)
		  "strtoll")
	       (method static $strtoeul::elong (::string ::long ::long)
		  "strtoll")
	       (method static strtouel::elong (::string ::long ::long)
		  "strtoull")
	       (method static strtoll::llong (::string ::long ::long)
		  "strtoll")
	       (method static $strtoull::llong (::string ::long ::long)
		  "strtoull")
	       (method static c-fixnum->string::bstring (::long ::long)
		  "integer_to_string")
	       (method static $string->integer-obj::obj (::string ::long)
		  "bgl_string_to_integer_obj")
	       (method static $integer->string/padding::bstring  (::long ::long ::long)
		  "integer_to_string_padding")
	       
	       (method static c-elong->string::bstring (::elong ::long)
		  "elong_to_string")
	       (method static c-llong->string::bstring (::llong ::long)
		  "llong_to_string")
	       (method static $unsigned->string::bstring  (::long ::long)
		  "unsigned_to_string")
	       (method static $uelong->string::bstring (::elong ::long)
		  "uelong_to_string")
	       (method static $ullong->string::bstring (::llong ::long)
		  "ullong_to_string")
	       (method static $rand::int ()
		  "rand")
	       (method static $seed-rand::void (::int)
		  "srand")))
   
   (export  (integer?::bool ::obj)
	    (inline fixnum?::bool ::obj)
	    (inline elong?::bool ::obj)
	    (inline llong?::bool ::obj)
	    (inline int8?::bool ::obj)
	    (inline uint8?::bool ::obj)
	    (inline int16?::bool ::obj)
	    (inline uint16?::bool ::obj)
	    (inline int32?::bool ::obj)
	    (inline uint32?::bool ::obj)
	    (inline int64?::bool ::obj)
	    (inline uint64?::bool ::obj)
	    (inline bignum?::bool ::obj)
 	    (inline make-elong::belong ::long)
	    (inline make-llong::bllong ::long)
	    
	    (inline elong->llong::llong ::elong)
	    (inline llong->elong::elong ::llong)
	    
	    (inline elong->int32::int32 ::elong)
	    (inline int32->elong::elong ::int32)
	    (inline elong->uint32::uint32 ::elong)
	    (inline uint32->elong::elong ::uint32)
	    
	    (inline int32->llong::llong ::int32)
	    (inline uint32->llong::llong ::uint32)
	    (inline llong->int32::int32 ::llong)
	    (inline llong->uint32::uint32 ::llong)
	    
	    (inline llong->int64::int64 ::llong)
	    (inline llong->uint64::uint64 ::llong)
	    (inline int64->llong::llong ::int64)
	    (inline uint64->llong::llong ::uint64)
	    (inline int64->elong::elong ::int64)
	    (inline uint64->elong::elong ::uint64)

	    (inline fixnum->int8::int8 ::long)
	    (inline fixnum->uint8::uint8 ::long)
	    (inline int8->fixnum::long ::int8)
	    (inline uint8->fixnum::long ::uint8)
	    (inline fixnum->int16::int16 ::long)
	    (inline fixnum->uint16::uint16 ::long)
	    (inline int16->fixnum::long ::int16)
	    (inline uint16->fixnum::long ::uint16)
	    (inline fixnum->int32::int32 ::long)
	    (inline fixnum->uint32::uint32 ::long)
	    (inline fixnum->int64::int64 ::long)
	    (inline fixnum->uint64::uint64 ::long)
	    (inline int32->fixnum::long ::int32)
	    (inline uint32->fixnum::long ::uint32)
	    (inline int64->fixnum::long ::int64)
	    (inline uint64->fixnum::long ::uint64)
	    (inline fixnum->byte::byte ::long)
	    (inline fixnum->ubyte::ubyte ::long)
	    (inline byte->fixnum::long ::byte)
	    (inline ubyte->fixnum::long ::byte)
	    (inline fixnum->elong::elong ::long)
	    (inline elong->fixnum::long ::elong)
	    (inline fixnum->llong::llong ::long)
	    (inline llong->fixnum::long ::llong)
	    
	    (inline int8->uint8::uint8 ::int8)
	    (inline uint8->int8::int8 ::uint8)
	    (inline int16->uint16::uint16 ::int16)
	    (inline uint16->int16::int16 ::uint16)
	    (inline int32->uint32::uint32 ::int32)
	    (inline uint32->int32::int32 ::uint32)
	    (inline uint32->uint64::uint64 ::uint32)
	    (inline int64->uint64::uint64 ::int64)
	    (inline uint64->int64::int64 ::uint64)
	    
	    (inline fixnum->bignum::bignum ::long)
	    (inline bignum->fixnum::long ::bignum)
	    (inline bignum->elong::elong ::bignum)
	    (inline bignum->llong::llong ::bignum)
	    (inline elong->bignum::bignum ::elong)
	    (inline llong->bignum::bignum ::llong)
	    (inline minvalfx::long)
	    (inline maxvalfx::long)
	    (inline minvalelong::elong)
	    (inline maxvalelong::elong)
	    (inline minvalllong::llong)
	    (inline maxvalllong::llong)
	    (inline =fx::bool ::long ::long)
	    (inline =elong::bool ::elong ::elong)
	    (inline =llong::bool ::llong ::llong)
	    (inline =s8::bool ::int8 ::int8)
	    (inline =u8::bool ::uint8 ::uint8)
	    (inline =s16::bool ::int16 ::int16)
	    (inline =u16::bool ::uint16 ::uint16)
	    (inline =s32::bool ::int32 ::int32)
	    (inline =u32::bool ::uint32 ::uint32)
	    (inline =s64::bool ::int64 ::int64)
	    (inline =u64::bool ::uint64 ::uint64)
	    (inline =bx::bool ::bignum ::bignum)
 	    (inline >fx::bool ::long ::long)
	    (inline >elong::bool ::elong ::elong)
	    (inline >llong::bool ::llong ::llong)
	    (inline >s8::bool ::int8 ::int8)
	    (inline >u8::bool ::uint8 ::uint8)
	    (inline >s16::bool ::int16 ::int16)
	    (inline >u16::bool ::uint16 ::uint16)
	    (inline >s32::bool ::int32 ::int32)
	    (inline >u32::bool ::uint32 ::uint32)
	    (inline >s64::bool ::int64 ::int64)
	    (inline >u64::bool ::uint64 ::uint64)
	    (inline >bx::bool ::bignum ::bignum)
 	    (inline >=fx::bool ::long ::long)
	    (inline >=elong::bool ::elong ::elong)
	    (inline >=llong::bool ::llong ::llong)
	    (inline >=s8::bool ::int8 ::int8)
	    (inline >=u8::bool ::uint8 ::uint8)
	    (inline >=s16::bool ::int16 ::int16)
	    (inline >=u16::bool ::uint16 ::uint16)
	    (inline >=s32::bool ::int32 ::int32)
	    (inline >=u32::bool ::uint32 ::uint32)
	    (inline >=s64::bool ::int64 ::int64)
	    (inline >=u64::bool ::uint64 ::uint64)
	    (inline >=bx::bool ::bignum ::bignum)
 	    (inline <fx::bool ::long ::long)
	    (inline <elong::bool ::elong ::elong)
	    (inline <llong::bool ::llong ::llong)
	    (inline <s8::bool ::int8 ::int8)
	    (inline <u8::bool ::uint8 ::uint8)
	    (inline <s16::bool ::int16 ::int16)
	    (inline <u16::bool ::uint16 ::uint16)
	    (inline <s32::bool ::int32 ::int32)
	    (inline <u32::bool ::uint32 ::uint32)
	    (inline <s64::bool ::int64 ::int64)
	    (inline <u64::bool ::uint64 ::uint64)
	    (inline <bx::bool ::bignum ::bignum)
 	    (inline <=fx::bool ::long ::long)
	    (inline <=elong::bool ::elong ::elong)
	    (inline <=llong::bool ::llong ::llong)
	    (inline <=s8::bool ::int8 ::int8)
	    (inline <=u8::bool ::uint8 ::uint8)
	    (inline <=s16::bool ::int16 ::int16)
	    (inline <=u16::bool ::uint16 ::uint16)
	    (inline <=s32::bool ::int32 ::int32)
	    (inline <=u32::bool ::uint32 ::uint32)
	    (inline <=s64::bool ::int64 ::int64)
	    (inline <=u64::bool ::uint64 ::uint64)
	    (inline <=bx::bool ::bignum ::bignum)
 	    (inline zerofx?::bool ::long)
	    (inline zeroelong?::bool ::elong)
	    (inline zerollong?::bool ::llong)
	    (inline zeros8?::bool ::int8)
	    (inline zerou8?::bool ::uint8)
	    (inline zeros16?::bool ::int16)
	    (inline zerou16?::bool ::uint16)
	    (inline zeros32?::bool ::int32)
	    (inline zerou32?::bool ::uint32)
	    (inline zeros64?::bool ::int64)
	    (inline zerou64?::bool ::uint64)
	    (inline zerobx?::bool ::bignum)
 	    (inline positivefx?::bool ::long)
	    (inline positiveelong?::bool ::elong)
	    (inline positivellong?::bool ::llong)
	    (inline positives8?::bool ::int8)
	    (inline positiveu8?::bool ::uint8)
	    (inline positives16?::bool ::int16)
	    (inline positiveu16?::bool ::uint16)
	    (inline positives32?::bool ::int32)
	    (inline positiveu32?::bool ::uint32)
	    (inline positives64?::bool ::int64)
	    (inline positiveu64?::bool ::uint64)
	    (inline positivebx?::bool ::bignum)
 	    (inline negativefx?::bool ::long)
	    (inline negativeelong?::bool ::elong)
	    (inline negativellong?::bool ::llong)
	    (inline negatives8?::bool ::int8)
	    (inline negativeu8?::bool ::uint8)
	    (inline negatives16?::bool ::int16)
	    (inline negativeu16?::bool ::uint16)
	    (inline negatives32?::bool ::int32)
	    (inline negativeu32?::bool ::uint32)
	    (inline negatives64?::bool ::int64)
	    (inline negativeu64?::bool ::uint64)
	    (inline negativebx?::bool ::bignum)
 	    (odd?::bool ::obj)
	    (inline oddfx?::bool ::long)
	    (inline oddelong?::bool ::elong)
	    (inline oddllong?::bool ::llong)
	    (inline odds8?::bool ::int8)
	    (inline oddu8?::bool ::uint8)
	    (inline odds16?::bool ::int16)
	    (inline oddu16?::bool ::uint16)
	    (inline odds32?::bool ::int32)
	    (inline oddu32?::bool ::uint32)
	    (inline odds64?::bool ::int64)
	    (inline oddu64?::bool ::uint64)
	    (inline oddbx?::bool ::bignum)
 	    (even?::bool ::obj)
	    (inline evenfx?::bool ::long)
	    (inline evenelong?::bool ::elong)
	    (inline evenllong?::bool ::llong)
	    (inline evens8?::bool ::int8)
	    (inline evenu8?::bool ::uint8)
	    (inline evens16?::bool ::int16)
	    (inline evenu16?::bool ::uint16)
	    (inline evens32?::bool ::int32)
	    (inline evenu32?::bool ::uint32)
	    (inline evens64?::bool ::int64)
	    (inline evenu64?::bool ::uint64)
	    (inline evenbx?::bool ::bignum)
 	    (minfx::long ::long . pair)
	    (minelong::elong ::elong . pair)
	    (minllong::llong ::llong . pair)
 	    (mins8::int8 ::int8 . pair)
 	    (minu8::uint8 ::uint8 . pair)
 	    (mins16::int16 ::int16 . pair)
 	    (minu16::uint16 ::uint16 . pair)
 	    (mins32::int32 ::int32 . pair)
 	    (minu32::uint32 ::uint32 . pair)
 	    (mins64::int64 ::int64 . pair)
 	    (minu64::uint64 ::uint64 . pair)
	    (minbx::bignum ::bignum . pair)
 	    (maxfx::long ::long . pair)
	    (maxelong::elong ::elong . pair)
	    (maxllong::llong ::llong . pair)
 	    (maxs8::int8 ::int8 . pair)
 	    (maxu8::uint8 ::uint8 . pair)
 	    (maxs16::int16 ::int16 . pair)
 	    (maxu16::uint16 ::uint16 . pair)
 	    (maxs32::int32 ::int32 . pair)
 	    (maxu32::uint32 ::uint32 . pair)
 	    (maxs64::int64 ::int64 . pair)
 	    (maxu64::uint64 ::uint64 . pair)
	    (maxbx::bignum ::bignum . pair)
 	    (inline +fx::long ::long ::long)
	    (inline +elong::elong ::elong ::elong)
	    (inline +llong::llong ::llong ::llong)
 	    (inline +s8::int8 ::int8 ::int8)
 	    (inline +u8::uint8 ::uint8 ::uint8)
 	    (inline +s16::int16 ::int16 ::int16)
 	    (inline +u16::uint16 ::uint16 ::uint16)
 	    (inline +s32::int32 ::int32 ::int32)
 	    (inline +u32::uint32 ::uint32 ::uint32)
 	    (inline +s64::int64 ::int64 ::int64)
 	    (inline +u64::uint64 ::uint64 ::uint64)
	    (inline +bx::bignum ::bignum ::bignum)
 	    (inline -fx::long ::long ::long)
	    (inline -elong::elong ::elong ::elong)
	    (inline -llong::llong ::llong ::llong)
 	    (inline -s8::int8 ::int8 ::int8)
 	    (inline -u8::uint8 ::uint8 ::uint8)
 	    (inline -s16::int16 ::int16 ::int16)
 	    (inline -u16::uint16 ::uint16 ::uint16)
 	    (inline -s32::int32 ::int32 ::int32)
 	    (inline -u32::uint32 ::uint32 ::uint32)
 	    (inline -s64::int64 ::int64 ::int64)
 	    (inline -u64::uint64 ::uint64 ::uint64)
	    (inline -bx::bignum ::bignum ::bignum)
 	    (inline *fx::long ::long ::long)
	    (inline *elong::elong ::elong ::elong)
	    (inline *llong::llong ::llong ::llong)
	    (inline *s8::int8 ::int8 ::int8)
 	    (inline *u8::uint8 ::uint8 ::uint8)
 	    (inline *s16::int16 ::int16 ::int16)
 	    (inline *u16::uint16 ::uint16 ::uint16)
 	    (inline *s32::int32 ::int32 ::int32)
 	    (inline *u32::uint32 ::uint32 ::uint32)
 	    (inline *s64::int64 ::int64 ::int64)
 	    (inline *u64::uint64 ::uint64 ::uint64)
	    (inline *bx::bignum ::bignum ::bignum)
 	    (inline /fx::long ::long ::long)
	    (inline /elong::elong ::elong ::elong)
	    (inline /llong::llong ::llong ::llong)
	    (inline /s8::int8 ::int8 ::int8)
 	    (inline /u8::uint8 ::uint8 ::uint8)
 	    (inline /s16::int16 ::int16 ::int16)
 	    (inline /u16::uint16 ::uint16 ::uint16)
 	    (inline /s32::int32 ::int32 ::int32)
 	    (inline /u32::uint32 ::uint32 ::uint32)
 	    (inline /s64::int64 ::int64 ::int64)
 	    (inline /u64::uint64 ::uint64 ::uint64)
	    (inline /bx::bignum ::bignum ::bignum)
 	    (inline negfx::long ::long)
	    (inline negelong::elong ::elong)
	    (inline negllong::llong ::llong)
	    (inline negs8::int8 ::int8)
	    (inline negu8::uint8 ::uint8)
	    (inline negs16::int16 ::int16)
	    (inline negu16::uint16 ::uint16)
	    (inline negs32::int32 ::int32)
	    (inline negu32::uint32 ::uint32)
	    (inline negs64::int64 ::int64)
	    (inline negu64::uint64 ::uint64)
	    (inline negbx::bignum ::bignum)
 	    (inline absfx::long ::long)
	    (inline abselong::elong ::elong)
	    (inline absllong::llong ::llong)
	    (inline abss8::int8 ::int8)
	    (inline absu8::uint8 ::uint8)
	    (inline abss16::int16 ::int16)
	    (inline absu16::uint16 ::uint16)
	    (inline abss32::int32 ::int32)
	    (inline absu32::uint32 ::uint32)
	    (inline abss64::int64 ::int64)
	    (inline absu64::uint64 ::uint64)
	    (inline absbx::bignum ::bignum)
 	    (remainder::obj ::obj ::obj)
	    (inline remainderfx::long ::long ::long)
	    (inline remainderelong::elong ::elong ::elong)
	    (inline remainderllong::llong ::llong ::llong)
	    (inline remainders8::int8 ::int8 ::int8)
	    (inline remainderu8::uint8 ::uint8 ::uint8)
	    (inline remainders16::int16 ::int16 ::int16)
	    (inline remainderu16::uint16 ::uint16 ::uint16)
	    (inline remainders32::int32 ::int32 ::int32)
	    (inline remainderu32::uint32 ::uint32 ::uint32)
	    (inline remainders64::int64 ::int64 ::int64)
	    (inline remainderu64::uint64 ::uint64 ::uint64)
	    (inline remainderbx::bignum ::bignum ::bignum)
 	    (quotient::obj ::obj ::obj)
	    (inline quotientfx::long ::long ::long)
	    (inline quotientelong::elong ::elong ::elong)
	    (inline quotientllong::llong ::llong ::llong)
	    (inline quotients8::int8 ::int8 ::int8)
	    (inline quotientu8::uint8 ::uint8 ::uint8)
	    (inline quotients16::int16 ::int16 ::int16)
	    (inline quotientu16::uint16 ::uint16 ::uint16)
	    (inline quotients32::int32 ::int32 ::int32)
	    (inline quotientu32::uint32 ::uint32 ::uint32)
	    (inline quotients64::int64 ::int64 ::int64)
	    (inline quotientu64::uint64 ::uint64 ::uint64)
	    (inline quotientbx::bignum ::bignum ::bignum)
 	    (modulo::obj ::obj ::obj)
	    (modulofx::long ::long ::long)
	    (moduloelong::elong ::elong ::elong)
	    (modulollong::llong ::llong ::llong)
	    (modulos8::int8 ::int8 ::int8)
	    (modulou8::uint8 ::uint8 ::uint8)
	    (modulos16::int16 ::int16 ::int16)
	    (modulou16::uint16 ::uint16 ::uint16)
	    (modulos32::int32 ::int32 ::int32)
	    (modulou32::uint32 ::uint32 ::uint32)
	    (modulos64::int64 ::int64 ::int64)
	    (modulou64::uint64 ::uint64 ::uint64)
	    (modulobx::bignum ::bignum ::bignum)
 	    (gcd::obj . pair)
	    (gcdfx::long . pair)
	    (gcdelong::elong . pair)
	    (gcdllong::llong . pair)
	    (gcds8::int8 . pair)
	    (gcdu8::uint8 . pair)
	    (gcds16::int16 . pair)
	    (gcdu16::uint16 . pair)
	    (gcds32::int32 . pair)
	    (gcdu32::uint32 . pair)
	    (gcds64::int64 . pair)
	    (gcdu64::uint64 . pair)
	    (gcdbx::bignum . pair)
 	    (lcm::obj . pair)
	    (lcmfx::long . pair)
	    (lcmelong::elong . pair)
	    (lcmllong::llong . pair)
	    (lcms8::int8 . pair)
	    (lcmu8::uint8 . pair)
	    (lcms16::int16 . pair)
	    (lcmu16::uint16 . pair)
	    (lcms32::int32 . pair)
	    (lcmu32::uint32 . pair)
	    (lcms64::int64 . pair)
	    (lcmu64::uint64 . pair)
	    (lcmbx::bignum . pair)
	    (exptfx::long ::long ::long)
	    (expts32::int32 ::int32 ::int32)
	    (exptu32::uint32 ::uint32 ::uint32)
	    (exptbx::bignum ::bignum ::bignum)
	    (integer->string::bstring ::long #!optional (radix::long 10))
	    (fixnum->string::bstring ::long #!optional (radix::long 10))
	    (integer->string/padding::bstring ::long ::long #!optional (radix::long 10))
	    (unsigned->string::bstring ::obj #!optional (radix::long 16))
	    (string->integer::long ::bstring #!optional (radix::long 10) (start::long 0))
	    (elong->string::bstring ::elong . pair)
	    (string->elong::elong ::bstring #!optional (radix::long 10))
	    (llong->string::bstring ::llong . pair)
	    (string->llong::llong ::bstring #!optional (radix::long 10))
	    (bignum->string::bstring ::bignum #!optional (radix::long 10))
	    (string->bignum::bignum ::bstring #!optional (radix::long 10))
	    (bignum->octet-string::bstring ::bignum)
	    (octet-string->bignum::bignum ::bstring)
	    (string->integer-obj::obj ::bstring ::long)
 	    (inline random::long ::long)
 	    (inline randombx::bignum ::bignum)
	    (seed-random! ::int))
   
   (pragma  (fixnum? no-alloc (predicate-of bint) no-cfa-top nesting fail-safe)
	    (c-fixnum? no-alloc side-effect-free (predicate-of bint) no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-elong? no-alloc side-effect-free (predicate-of belong) no-cfa-top nesting (effect) fail-safe)
	    (c-llong? no-alloc side-effect-free (predicate-of bllong) no-cfa-top nesting (effect) fail-safe)
	    ($int8? no-alloc side-effect-free (predicate-of bint8) no-cfa-top nesting (effect) fail-safe)	
	    ($uint8? no-alloc side-effect-free (predicate-of buint8) no-cfa-top nesting (effect) fail-safe)	
	    ($int16? no-alloc side-effect-free (predicate-of bint16) no-cfa-top nesting (effect) fail-safe)	
	    ($uint16? no-alloc side-effect-free (predicate-of buint16) no-cfa-top nesting (effect) fail-safe)	
	    ($int32? no-alloc side-effect-free (predicate-of bint32) no-cfa-top nesting (effect) fail-safe)	
	    ($uint32? no-alloc side-effect-free (predicate-of buint32) no-cfa-top nesting (effect) fail-safe)	
	    ($int64? no-alloc side-effect-free (predicate-of bint64) no-cfa-top nesting (effect) fail-safe)	
	    ($uint64? no-alloc side-effect-free (predicate-of buint64) no-cfa-top nesting (effect) fail-safe)	
	    (bignum? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    ($elong->llong no-alloc side-effect-free args-safe (effect) fail-safe)
	    ($llong->elong no-alloc side-effect-free args-safe (effect) fail-safe)
	    (fixnum->bignum side-effect-free no-cfa-top nesting (effect))
	    (bignum->fixnum no-alloc side-effect-free no-cfa-top nesting (effect))
	    (bignum->elong no-alloc side-effect-free no-cfa-top nesting (effect))
	    (bignum->llong no-alloc side-effect-free no-cfa-top nesting (effect))
	    (elong->bignum side-effect-free no-cfa-top nesting (effect))
	    (llong->bignum side-effect-free no-cfa-top nesting (effect))
	    (integer? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (=bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (>fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (>=fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (>=bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (<fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (<=fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (<=bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (odd? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (odds8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (oddu8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (odds16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (oddu16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (odds32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (oddu32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (odds64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (oddu64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (oddbx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (c-oddfx? no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
 	    (even? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evens8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evenu8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evens16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evenu16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evens32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evenu32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evens64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evenu64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (evenbx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (c-evenfx? no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
 	    (+fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (+elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (+llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (+u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (+bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (-elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (-llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (-u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (-bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*fx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (*elong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (*llong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (*u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (*bx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/fx no-alloc side-effect-free no-cfa-top nesting (effect))
	    (/elong no-alloc side-effect-free no-cfa-top nesting (effect))
	    (/llong no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (/s8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/u8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/s16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/u16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/s32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/u32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/s64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (/u64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (/bx no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (remainderfx no-alloc side-effect-free no-cfa-top nesting (effect))
	    (remainderelong no-alloc side-effect-free no-cfa-top nesting (effect))
	    (remainderllong no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (remainders8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainderu8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainders16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainderu16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainders32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainderu32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainders64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (remainderu64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (remainderbx no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (fixnum->string side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (integer->string side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (string->integer side-effect-free no-cfa-top nesting (effect))
	    (bignum->string side-effect-free no-cfa-top nesting (effect))
	    (bignum->octet-string side-effect-free no-cfa-top nesting (effect))
	    (string->bignum side-effect-free no-cfa-top nesting (effect))
	    (octet-string->bignum side-effect-free no-cfa-top nesting (effect))
 	    (modulo no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (modulos8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulou8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulos16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulou16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulos32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulou32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulos64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (modulou64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (modulobx no-alloc side-effect-free no-cfa-top nesting (effect))
	    (quotientfx no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (quotients8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotientu8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotients16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotientu16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotients32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotientu32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotients64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (quotientu64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (quotientbx no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (gcdfx no-alloc side-effect-free no-cfa-top nesting (effect))
	    (gcdelong no-alloc side-effect-free no-cfa-top nesting (effect))
	    (gcdllong no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (gcds8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcdu8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcds16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcdu16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcds32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcdu32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcds64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (gcdu64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (gcdbx side-effect-free no-cfa-top nesting (effect))
 	    (lcmfx side-effect-free no-cfa-top nesting (effect))
	    (lcmelong no-alloc side-effect-free no-cfa-top nesting (effect))
	    (lcmllong no-alloc side-effect-free no-cfa-top nesting (effect))
 	    (lcms8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcmu8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcms16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcmu16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcms32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcmu32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcms64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (lcmu64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (lcmbx no-alloc side-effect-free no-cfa-top nesting (effect))
	    (exptfx no-alloc side-effect-free no-cfa-top nesting (effect))
	    (expts32 no-alloc side-effect-free no-cfa-top nesting (effect))
	    (exptu32 no-alloc side-effect-free no-cfa-top nesting (effect))
	    (exptbx side-effect-free no-cfa-top nesting (effect))
 	    (positivefx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (positiveelong? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (positivellong? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positives8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positiveu8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positives16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positiveu16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positives32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positiveu32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positives64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (positiveu64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (positivebx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negativefx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (negativeelong? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (negativellong? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negatives8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negativeu8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negatives16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negativeu16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negatives32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negativeu32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negatives64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negativeu64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (negativebx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zerofx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (zeroelong? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (zerollong? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zeros8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zerou8? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zeros16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zerou16? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zeros32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zerou32? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zeros64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (zerou64? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (zerobx? no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negfx no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (negelong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (negllong no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negs8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negu8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negs16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negu16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negs32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negu32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negs64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (negu64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
	    (negbx side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (abss8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (absu8 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (abss16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (absu16 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (abss32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (absu32 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (abss64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (absu64 no-alloc side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (absbx side-effect-free no-cfa-top nesting (effect) fail-safe)
 	    (c-=fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-=elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-=llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c->fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c->elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c->llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c->=fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c->=elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c->=llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($>=u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-<fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-<elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-<llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-<=fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-<=elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-<=llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($<=u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-oddfx? no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-+fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-+elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-+llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($+u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
 	    (c--fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c--elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c--llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($-u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
 	    (c-*fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-*elong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-*llong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($*u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
 	    (c-/fx no-alloc side-effect-free no-cfa-top nesting args-safe (effect))
	    ($/s8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/u8 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/s16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/u16 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/s32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/u32 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/s64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($/u64 no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
 	    (c-negfx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-negelong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (c-negllong no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    (random side-effect-free no-cfa-top nesting (effect))
	    (randombx side-effect-free no-cfa-top nesting (effect)))

   (cond-expand
      (bigloo-c
       (pragma
	  ($addfx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($subfx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($ltfx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($lefx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($gtfx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($gefx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
	    ($egfx no-alloc side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)))))

;*---------------------------------------------------------------------*/
;*    integer? ...                                                     */
;*---------------------------------------------------------------------*/
(define (integer? obj)
   (or (c-fixnum? obj)
       (c-elong? obj)
       (c-llong? obj)
       ($int8? obj)
       ($uint8? obj)
       ($int16? obj)
       ($uint16? obj)
       ($int32? obj)
       ($uint32? obj)
       ($int64? obj)
       ($uint64? obj)
       ($bignum? obj)
       (and (c-flonum? obj) (integerfl? obj))))

;*---------------------------------------------------------------------*/
;*    predicates ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (fixnum? obj) (c-fixnum? obj))

(define-inline (elong? obj) (c-elong? obj))
(define-inline (llong? obj) (c-llong? obj))

(define-inline (int8? obj) ($int8? obj))
(define-inline (uint8? obj) ($uint8? obj))

(define-inline (int16? obj) ($int16? obj))
(define-inline (uint16? obj) ($uint16? obj))

(define-inline (int32? obj) ($int32? obj))
(define-inline (uint32? obj) ($uint32? obj))

(define-inline (int64? obj) ($int64? obj))
(define-inline (uint64? obj) ($uint64? obj))

(define-inline (bignum? obj) ($bignum? obj))

;*---------------------------------------------------------------------*/
;*    constructors and casts                                           */
;*---------------------------------------------------------------------*/
(define-inline (make-elong long) ($long->belong long))
(define-inline (make-llong long) ($long->bllong long))

(define-inline (elong->llong x) ($elong->llong x))
(define-inline (llong->elong x) ($llong->elong x))

(define-inline (elong->int32 n) ($elong->int32 n))
(define-inline (elong->uint32 n) ($elong->uint32 n))

(define-inline (int32->elong n) ($int32->elong n))
(define-inline (uint32->elong n) ($uint32->elong n))

(define-inline (int32->llong n) ($int32->llong n))
(define-inline (uint32->llong n) ($uint32->llong n))
(define-inline (uint32->uint64 n) ($uint32->uint64 n))
(define-inline (llong->int32 n) ($llong->int32 n))
(define-inline (llong->uint32 n) ($llong->uint32 n))

(define-inline (llong->int64 n) ($llong->int64 n))
(define-inline (llong->uint64 n) ($llong->uint64 n))

(define-inline (int64->llong n) ($int64->llong n))
(define-inline (uint64->llong n) ($uint64->llong n))

(define-inline (int64->elong n) ($int64->elong n))
(define-inline (uint64->elong n) ($uint64->elong n))

(define-inline (uint8->int8 n) ($uint8->int8 n))
(define-inline (int8->uint8 n) ($int8->uint8 n))

(define-inline (uint16->int16 n) ($uint16->int16 n))
(define-inline (int16->uint16 n) ($int16->uint16 n))

(define-inline (uint32->int32 n) ($uint32->int32 n))
(define-inline (int32->uint32 n) ($int32->uint32 n))

(define-inline (uint64->int64 n) ($uint64->int64 n))
(define-inline (int64->uint64 n) ($int64->uint64 n))

(define-inline (fixnum->bignum x) ($fixnum->bignum x))
(define-inline (bignum->fixnum x) ($bignum->fixnum x))

(define-inline (bignum->elong x) ($bignum->elong x))
(define-inline (bignum->llong x) ($bignum->llong x))

(define-inline (elong->bignum x) ($elong->bignum x))
(define-inline (llong->bignum x) ($llong->bignum x))

;*---------------------------------------------------------------------*/
;*    fixnum->stding ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (fixnum->int8 x) ($long->int8 x))
(define-inline (fixnum->uint8 x) ($long->uint8 x))
(define-inline (int8->fixnum x) (elong->fixnum ($int8->elong x)))
(define-inline (uint8->fixnum x) (elong->fixnum ($uint8->elong x)))

(define-inline (fixnum->int16 x) ($long->int16 x))
(define-inline (fixnum->uint16 x) ($long->uint16 x))
(define-inline (int16->fixnum x) (elong->fixnum ($int16->elong x)))
(define-inline (uint16->fixnum x) (elong->fixnum ($uint16->elong x)))

(define-inline (fixnum->int32 x) ($long->int32 x))
(define-inline (fixnum->uint32 x) ($long->uint32 x))
(define-inline (int32->fixnum x) (elong->fixnum ($int32->elong x)))
(define-inline (uint32->fixnum x) ($uint32->long x))

(define-inline (fixnum->int64 x) ($long->int64 x))
(define-inline (fixnum->uint64 x) ($long->uint64 x))
(define-inline (int64->fixnum x) (elong->fixnum ($int64->elong x)))
(define-inline (uint64->fixnum x) (elong->fixnum ($uint64->elong x)))

;*---------------------------------------------------------------------*/
;*    fixnum casts                                                     */
;*---------------------------------------------------------------------*/
(define-inline (fixnum->byte x) ($long->byte x))
(define-inline (fixnum->ubyte x) ($long->ubyte x))
(define-inline (byte->fixnum x) ($byte->long x))
(define-inline (ubyte->fixnum x) ($ubyte->long x))

(define-inline (fixnum->elong x) ($long->elong x))
(define-inline (elong->fixnum x) ($elong->long x))
		       
(define-inline (fixnum->llong x) ($long->llong x))
(define-inline (llong->fixnum x) ($llong->long x))
		       
;*---------------------------------------------------------------------*/
;*    {min,max}val{fx,elong,llong} ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (minvalfx) $minvalfx)
(define-inline (minvalelong) $minvalelong)
(define-inline (minvalllong) $minvalllong)

(define-inline (maxvalfx) $maxvalfx)
(define-inline (maxvalelong) $maxvalelong)
(define-inline (maxvalllong) $maxvalllong)

;*---------------------------------------------------------------------*/
;*    =                                                                */
;*---------------------------------------------------------------------*/
(define-inline (=fx n1 n2) (c-=fx n1 n2))

(define-inline (=elong n1 n2) (c-=elong n1 n2))
(define-inline (=llong n1 n2) (c-=llong n1 n2))

(define-inline (=s8 n1 n2) ($=s8 n1 n2))
(define-inline (=u8 n1 n2) ($=u8 n1 n2))

(define-inline (=s16 n1 n2) ($=s16 n1 n2))
(define-inline (=u16 n1 n2) ($=u16 n1 n2))

(define-inline (=s32 n1 n2) ($=s32 n1 n2))
(define-inline (=u32 n1 n2) ($=u32 n1 n2))

(define-inline (=s64 n1 n2) ($=s64 n1 n2))
(define-inline (=u64 n1 n2) ($=u64 n1 n2))

(define-inline (=bx n1 n2) (=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    < ...                                                            */
;*---------------------------------------------------------------------*/
(define-inline (<fx n1 n2) (c-<fx n1 n2))

(define-inline (<elong n1 n2) (c-<elong n1 n2))
(define-inline (<llong n1 n2) (c-<llong n1 n2))

(define-inline (<s8 n1 n2) ($<s8 n1 n2))
(define-inline (<u8 n1 n2) ($<u8 n1 n2))

(define-inline (<s16 n1 n2) ($<s16 n1 n2))
(define-inline (<u16 n1 n2) ($<u16 n1 n2))

(define-inline (<s32 n1 n2) ($<s32 n1 n2))
(define-inline (<u32 n1 n2) ($<u32 n1 n2))

(define-inline (<s64 n1 n2) ($<s64 n1 n2))
(define-inline (<u64 n1 n2) ($<u64 n1 n2))

(define-inline (<bx n1 n2) (<fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    > ...                                                            */
;*---------------------------------------------------------------------*/
(define-inline (>fx n1 n2) (c->fx n1 n2))

(define-inline (>elong n1 n2) (c->elong n1 n2))
(define-inline (>llong n1 n2) (c->llong n1 n2))

(define-inline (>s8 n1 n2) ($>s8 n1 n2))
(define-inline (>u8 n1 n2) ($>u8 n1 n2))

(define-inline (>s16 n1 n2) ($>s16 n1 n2))
(define-inline (>u16 n1 n2) ($>u16 n1 n2))

(define-inline (>s32 n1 n2) ($>s32 n1 n2))
(define-inline (>u32 n1 n2) ($>u32 n1 n2))

(define-inline (>s64 n1 n2) ($>s64 n1 n2))
(define-inline (>u64 n1 n2) ($>u64 n1 n2))

(define-inline (>bx n1 n2) (>fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    <= ...                                                           */
;*---------------------------------------------------------------------*/
(define-inline (<=fx n1 n2) (c-<=fx n1 n2))

(define-inline (<=elong n1 n2) (c-<=elong n1 n2))
(define-inline (<=llong n1 n2) (c-<=llong n1 n2))

(define-inline (<=s8 n1 n2) ($<=s8 n1 n2))
(define-inline (<=u8 n1 n2) ($<=u8 n1 n2))

(define-inline (<=s16 n1 n2) ($<=s16 n1 n2))
(define-inline (<=u16 n1 n2) ($<=u16 n1 n2))

(define-inline (<=s32 n1 n2) ($<=s32 n1 n2))
(define-inline (<=u32 n1 n2) ($<=u32 n1 n2))

(define-inline (<=s64 n1 n2) ($<=s64 n1 n2))
(define-inline (<=u64 n1 n2) ($<=u64 n1 n2))

(define-inline (<=bx n1 n2) (<=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    >= ...                                                           */
;*---------------------------------------------------------------------*/
(define-inline (>=fx n1 n2) (c->=fx n1 n2))

(define-inline (>=elong n1 n2) (c->=elong n1 n2))
(define-inline (>=llong n1 n2) (c->=llong n1 n2))

(define-inline (>=s8 n1 n2) ($>=s8 n1 n2))
(define-inline (>=u8 n1 n2) ($>=u8 n1 n2))

(define-inline (>=s16 n1 n2) ($>=s16 n1 n2))
(define-inline (>=u16 n1 n2) ($>=u16 n1 n2))

(define-inline (>=s32 n1 n2) ($>=s32 n1 n2))
(define-inline (>=u32 n1 n2) ($>=u32 n1 n2))

(define-inline (>=s64 n1 n2) ($>=s64 n1 n2))
(define-inline (>=u64 n1 n2) ($>=u64 n1 n2))

(define-inline (>=bx n1 n2) (>=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    zero? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (zerofx? n) (=fx n 0))

(define-inline (zeroelong? n) (=elong n #e0))
(define-inline (zerollong? n) (=llong n #l0))

(define-inline (zeros8? n1) (=s8 n1 (fixnum->int8 0)))
(define-inline (zerou8? n1) (=u8 n1 (fixnum->uint8 0)))

(define-inline (zeros16? n1) (=s16 n1 (fixnum->int16 0)))
(define-inline (zerou16? n1) (=u16 n1 (fixnum->uint16 0)))

(define-inline (zeros32? n1) (=s32 n1 (fixnum->int32 0)))
(define-inline (zerou32? n1) (=u32 n1 (fixnum->uint32 0)))

(define-inline (zeros64? n1) (=s64 n1 (fixnum->int64 0)))
(define-inline (zerou64? n1) (=u64 n1 (fixnum->uint64 0)))

(define-inline (zerobx? n) ($zerobx? n))

;*---------------------------------------------------------------------*/
;*    positive? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (positivefx? n) (>fx n 0))

(define-inline (positiveelong? n) (>elong n #e0))
(define-inline (positivellong? n) (>llong n #l0))

(define-inline (positives8? n1) (>s8 n1 (fixnum->int8 0)))
(define-inline (positiveu8? n1) (>u8 n1 (fixnum->uint8 0)))

(define-inline (positives16? n1) (>s16 n1 (fixnum->int16 0)))
(define-inline (positiveu16? n1) (>u16 n1 (fixnum->uint16 0)))

(define-inline (positives32? n1) (>s32 n1 (fixnum->int32 0)))
(define-inline (positiveu32? n1) (>u32 n1 (fixnum->uint32 0)))

(define-inline (positives64? n1) (>s64 n1 (fixnum->int64 0)))
(define-inline (positiveu64? n1) (>u64 n1 (fixnum->uint64 0)))

(define-inline (positivebx? n) ($positivebx? n))

;*---------------------------------------------------------------------*/
;*    negative? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (negativefx? n) (<fx n 0))

(define-inline (negativeelong? n) (<elong n #e0))
(define-inline (negativellong? n) (<llong n #l0))

(define-inline (negatives8? n1) (<s8 n1 (fixnum->int8 0)))
(define-inline (negativeu8? n1) (<u8 n1 (fixnum->uint8 0)))

(define-inline (negatives16? n1) (<s16 n1 (fixnum->int16 0)))
(define-inline (negativeu16? n1) (<u16 n1 (fixnum->uint16 0)))

(define-inline (negatives32? n1) (<s32 n1 (fixnum->int32 0)))
(define-inline (negativeu32? n1) (<u32 n1 (fixnum->uint32 0)))

(define-inline (negatives64? n1) (<s64 n1 (fixnum->int64 0)))
(define-inline (negativeu64? n1) (<u64 n1 (fixnum->uint64 0)))

(define-inline (negativebx? n) ($negativebx? n))

;*---------------------------------------------------------------------*/
;*    odd? ...                                                         */
;*---------------------------------------------------------------------*/
(define (odd? x)
  (cond
     ((fixnum? x) (oddfx? x))
     ((elong? x) (oddelong? x))
     ((llong? x) (oddllong? x))
     ((bignum? x) (oddbx? x))
     (else (error "odd?" "Illegal integer" x))))

(define-inline (oddfx? x) (c-oddfx? x))

(define-inline (oddelong? x)
   (if (evenelong? x) #f #t))
(define-inline (oddllong? x)
   (if (evenllong? x) #f #t))

(define-inline (odds8? n)
   (=s8 (remainders8 n (fixnum->int8 2)) (fixnum->int8 1)))
(define-inline (oddu8? n)
   (=u8 (remainderu8 n (fixnum->uint8 2)) (fixnum->uint8 1)))

(define-inline (odds16? n)
   (=s16 (remainders16 n (fixnum->int16 2)) (fixnum->int16 1)))
(define-inline (oddu16? n)
   (=u16 (remainderu16 n (fixnum->uint16 2)) (fixnum->uint16 1)))

(define-inline (odds32? n)
   (=s32 (remainders32 n (fixnum->int32 2)) (fixnum->int32 1)))
(define-inline (oddu32? n)
   (=u32 (remainderu32 n (fixnum->uint32 2)) (fixnum->uint32 1)))

(define-inline (odds64? n)
   (=s64 (remainders64 n (fixnum->int64 2)) (fixnum->int64 1)))
(define-inline (oddu64? n)
   (=u64 (remainderu64 n (fixnum->uint64 2)) (fixnum->uint64 1)))

(define-inline (oddbx? x)
   ($oddbx? x))

;*---------------------------------------------------------------------*/
;*    even? ...                                                        */
;*---------------------------------------------------------------------*/
(define (even? x)
  (cond
     ((fixnum? x) (evenfx? x))
     ((elong? x) (evenelong? x))
     ((llong? x) (evenllong? x))
     ((bignum? x) (evenbx? x))
     (else (error "even?" "Illegal integer" x))))

(define-inline (evenfx? x)
   (c-evenfx? x))

(define-inline (evenelong? x) (zeroelong? (remainderelong x #e2)))
(define-inline (evenllong? x) (zerollong? (remainderllong x #l2)))

(define-inline (evens8? n1) (not (odds8? n1)))
(define-inline (evenu8? n1) (not (oddu8? n1)))

(define-inline (evens16? n1) (not (odds16? n1)))
(define-inline (evenu16? n1) (not (oddu16? n1)))

(define-inline (evens32? n1) (not (odds32? n1)))
(define-inline (evenu32? n1) (not (oddu32? n1)))

(define-inline (evens64? n1) (not (odds64? n1)))
(define-inline (evenu64? n1) (not (oddu64? n1)))

(define-inline (evenbx? x) ($evenbx? x))

;*---------------------------------------------------------------------*/
;*    min/max ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (min/max test-proc x . xs)
  `(let loop ((y ,x)
	      (xs ,xs))
     (if (null? xs)
	 y
	 (if (,test-proc (car xs) y)
	     (loop (car xs) (cdr xs))
	     (loop y (cdr xs))))))

(define (minfx n1 . nn) (min/max <fx n1 . nn))

(define (minelong n1 . nn) (min/max <elong n1 . nn))
(define (minllong n1 . nn) (min/max <llong n1 . nn))

(define (mins8 n1 . nn) (min/max <s8 n1 . nn))
(define (minu8 n1 . nn) (min/max <u8 n1 . nn))
   
(define (mins16 n1 . nn) (min/max <s16 n1 . nn))
(define (minu16 n1 . nn) (min/max <u16 n1 . nn))
   
(define (mins32 n1 . nn) (min/max <s32 n1 . nn))
(define (minu32 n1 . nn) (min/max <u32 n1 . nn))
   
(define (mins64 n1 . nn) (min/max <s64 n1 . nn))
(define (minu64 n1 . nn) (min/max <u64 n1 . nn))
   
(define (minbx n1 . nn) (min/max <bx n1 . nn))

;*---------------------------------------------------------------------*/
;*    maxfx ...                                                        */
;*---------------------------------------------------------------------*/
(define (maxfx n1 . nn)
   (min/max >fx n1 . nn))

(define (maxelong n1 . nn) (min/max >elong n1 . nn))
(define (maxllong n1 . nn) (min/max >llong n1 . nn))

(define (maxs8 n1 . nn) (min/max >s8 n1 . nn))
(define (maxu8 n1 . nn) (min/max >u8 n1 . nn))
   
(define (maxs16 n1 . nn) (min/max >s16 n1 . nn))
(define (maxu16 n1 . nn) (min/max >u16 n1 . nn))
   
(define (maxs32 n1 . nn) (min/max >s32 n1 . nn))
(define (maxu32 n1 . nn) (min/max >u32 n1 . nn))
   
(define (maxs64 n1 . nn) (min/max >s64 n1 . nn))
(define (maxu64 n1 . nn) (min/max >u64 n1 . nn))
   
(define (maxbx n1 . nn) (min/max >bx n1 . nn))

;*---------------------------------------------------------------------*/
;*    + ...                                                            */
;*---------------------------------------------------------------------*/
(define-inline (+fx z1 z2) (c-+fx z1 z2))

(define-inline (+elong z1 z2) (c-+elong z1 z2))
(define-inline (+llong z1 z2) (c-+llong z1 z2))

(define-inline (+s8 z1 z2) ($+s8 z1 z2))
(define-inline (+u8 z1 z2) ($+u8 z1 z2))

(define-inline (+s16 z1 z2) ($+s16 z1 z2))
(define-inline (+u16 z1 z2) ($+u16 z1 z2))

(define-inline (+s32 z1 z2) ($+s32 z1 z2))
(define-inline (+u32 z1 z2) ($+u32 z1 z2))

(define-inline (+s64 z1 z2) ($+s64 z1 z2))
(define-inline (+u64 z1 z2) ($+u64 z1 z2))

(define-inline (+bx z1 z2) ($+bx z1 z2))

;*---------------------------------------------------------------------*/
;*    - ...                                                            */
;*---------------------------------------------------------------------*/
(define-inline (-fx z1 z2) (c--fx z1 z2))

(define-inline (-elong z1 z2) (c--elong z1 z2))
(define-inline (-llong z1 z2) (c--llong z1 z2))

(define-inline (-s8 z1 z2) ($-s8 z1 z2))
(define-inline (-u8 z1 z2) ($-u8 z1 z2))

(define-inline (-s16 z1 z2) ($-s16 z1 z2))
(define-inline (-u16 z1 z2) ($-u16 z1 z2))

(define-inline (-s32 z1 z2) ($-s32 z1 z2))
(define-inline (-u32 z1 z2) ($-u32 z1 z2))

(define-inline (-s64 z1 z2) ($-s64 z1 z2))
(define-inline (-u64 z1 z2) ($-u64 z1 z2))

(define-inline (-bx z1 z2) ($-bx z1 z2))

;*---------------------------------------------------------------------*/
;*    * ...                                                            */
;*---------------------------------------------------------------------*/
(define-inline (*fx z1 z2) (c-*fx z1 z2))

(define-inline (*elong z1 z2) (c-*elong z1 z2))
(define-inline (*llong z1 z2) (c-*llong z1 z2))

(define-inline (*s8 z1 z2) ($*s8 z1 z2))
(define-inline (*u8 z1 z2) ($*u8 z1 z2))

(define-inline (*s16 z1 z2) ($*s16 z1 z2))
(define-inline (*u16 z1 z2) ($*u16 z1 z2))

(define-inline (*s32 z1 z2) ($*s32 z1 z2))
(define-inline (*u32 z1 z2) ($*u32 z1 z2))

(define-inline (*s64 z1 z2) ($*s64 z1 z2))
(define-inline (*u64 z1 z2) ($*u64 z1 z2))

(define-inline (*bx z1 z2) ($*bx z1 z2))

;*---------------------------------------------------------------------*/
;*    / ...                                                            */
;*---------------------------------------------------------------------*/
(define-inline (/fx z1 z2) (c-/fx z1 z2))

(define-inline (/elong z1 z2) (c-/elong z1 z2))
(define-inline (/llong z1 z2) (c-/llong z1 z2))

(define-inline (/s8 z1 z2) ($/s8 z1 z2))
(define-inline (/u8 z1 z2) ($/u8 z1 z2))

(define-inline (/s16 z1 z2) ($/s16 z1 z2))
(define-inline (/u16 z1 z2) ($/u16 z1 z2))

(define-inline (/s32 z1 z2) ($/s32 z1 z2))
(define-inline (/u32 z1 z2) ($/u32 z1 z2))

(define-inline (/s64 z1 z2) ($/s64 z1 z2))
(define-inline (/u64 z1 z2) ($/u64 z1 z2))

(define-inline (/bx z1 z2) ($quotientbx z1 z2))

;*---------------------------------------------------------------------*/
;*    neg ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (negfx n1) (c-negfx n1))

(define-inline (negelong n1) (c-negelong n1))
(define-inline (negllong n1) (c-negllong n1))

(define-inline (negs8 z) (-s8 (fixnum->int8 0) z))
(define-inline (negu8 z) (-u8 (fixnum->uint8 0) z))

(define-inline (negs16 z) (-s16 (fixnum->int16 0) z))
(define-inline (negu16 z) (-u16 (fixnum->uint16 0) z))

(define-inline (negs32 z) (-s32 (fixnum->int32 0) z))
(define-inline (negu32 z) (-u32 (fixnum->uint32 0) z))

(define-inline (negs64 z) (-s64 (fixnum->int64 0) z))
(define-inline (negu64 z) (-u64 (fixnum->uint64 0) z))

(define-inline (negbx n1) ($negbx n1))

;*---------------------------------------------------------------------*/
;*    abs ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (absfx n) (if (<fx n 0) (negfx n) n))

(define-inline (abselong n) (if (<elong n #e0) (negelong n) n))
(define-inline (absllong n) (if (<llong n #l0) (negllong n) n))

(define-inline (abss8 z) (if (<s8 z (fixnum->int8 0)) (negs8 z) z))
(define-inline (absu8 z) z)

(define-inline (abss16 z) (if (<s16 z (fixnum->int16 0)) (negs16 z) z))
(define-inline (absu16 z) z)

(define-inline (abss32 z) (if (<s32 z (fixnum->int32 0)) (negs32 z) z))
(define-inline (absu32 z) z)

(define-inline (abss64 z) (if (<s64 z (fixnum->int64 0)) (negs64 z) z))
(define-inline (absu64 z) z)

(define-inline (absbx n) ($absbx n))

;*---------------------------------------------------------------------*/
;*    int2op ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (int2op op x y . safe)
  (let ((opfx (symbol-append op 'fx))
	(opelong (symbol-append op 'elong))
	(opllong (symbol-append op 'llong))
	(opbx (symbol-append op 'bx)))
    (if (not (null? safe))
	(begin
	  (set! opfx (symbol-append '$ opfx '-safe))
	  (set! opelong (symbol-append '$ opelong '-safe))
	  (set! opllong (symbol-append '$ opllong '-safe))))
    `(cond ((fixnum? ,x)
	    (cond ((fixnum? ,y)
		   (,opfx ,x ,y))
		  ((elong? ,y)
		   (,opelong (fixnum->elong ,x) ,y))
		  ((llong? ,y)
		   (,opllong (fixnum->llong ,x) ,y))
		  ((bignum? ,y)
		   (,opbx (fixnum->bignum ,x) ,y))
		  (else
		   (error ',op "not an integer" ,y))))
	   ((elong? ,x)
	    (cond ((fixnum? ,y)
		   (,opelong ,x (fixnum->elong ,y)))
		  ((elong? ,y)
		   (,opelong ,x ,y))
		  ((llong? ,y)
		   (,opllong ($elong->llong ,x) ,y))
		  ((bignum? ,y)
		   (,opbx (elong->bignum ,x) ,y))
		  (else
		   (error ',op "not an integer" ,y))))
	   ((llong? ,x)
	    (cond ((fixnum? ,y)
		   (,opllong ,x (fixnum->llong ,y)))
		  ((elong? ,y)
		   (,opllong ,x ($elong->llong ,y)))
		  ((llong? ,y)
		   (,opllong ,x ,y))
		  ((bignum? ,y)
		   (,opbx (llong->bignum ,x) ,y))
		  (else
		   (error ',op "not an integer" ,y))))
	   ((bignum? ,x)
	    (cond ((fixnum? ,y)
		   (,opbx ,x (fixnum->bignum ,y)))
		  ((elong? ,y)
		   (,opbx ,x (elong->bignum ,y)))
		  ((llong? ,y)
		   (,opbx ,x (llong->bignum ,y)))
		  ((bignum? ,y)
		   (,opbx ,x ,y))
		  (else
		   (error ',op "not an integer" ,y))))
	   (else
	    (error ',op "not an integer" ,x)))))

;*---------------------------------------------------------------------*/
;*    quotient ...                                                     */
;*---------------------------------------------------------------------*/
(define (quotient n1 n2) (int2op quotient n1 n2 'safe))

(define-inline (quotientfx n1 n2) (c-quotientfx n1 n2))

(define-inline (quotientelong n1 n2) (c-quotientelong n1 n2))
(define-inline (quotientllong n1 n2) (c-quotientllong n1 n2))

(define-inline (quotients8 n1 n2) ($quotients8 n1 n2))
(define-inline (quotientu8 n1 n2) ($quotientu8 n1 n2))

(define-inline (quotients16 n1 n2) ($quotients16 n1 n2))
(define-inline (quotientu16 n1 n2) ($quotientu16 n1 n2))

(define-inline (quotients32 n1 n2) ($quotients32 n1 n2))
(define-inline (quotientu32 n1 n2) ($quotientu32 n1 n2))

(define-inline (quotients64 n1 n2) ($quotients64 n1 n2))
(define-inline (quotientu64 n1 n2) ($quotientu64 n1 n2))

(define-inline (quotientbx n1 n2) ($quotientbx n1 n2))

;*---------------------------------------------------------------------*/
;*    remainder ...                                                    */
;*---------------------------------------------------------------------*/
(define (remainder n1 n2) (int2op remainder n1 n2))

(define-inline (remainderfx n1 n2)
   ;; on a 64bit machines, if the two arguments are 32bit integer, use
   ;; a 32 bit division which is significantly faster than a 64bit operation
   (cond-expand
      ((and (or bint61 bint64) bigloo-c)
       ;; should use bit-or and bit-and but this would force to import __bit
       ;; in all library modules
       ;; -2147483648 == 1111...111000...000
       ;;                `--------'`--------'
       ;;                 33 1-bit  31 0-bit
       (if (=fx (pragma::long "((($1) | ($2)) & -2147483648)" n1 n2) 0)
	   (int32->fixnum ($remainders32 (fixnum->int32 n1) (fixnum->int32 n2)))
	   (c-remainderfx n1 n2)))
      (else
       (c-remainderfx n1 n2))))

(define-inline (remainderelong n1 n2) (c-remainderelong n1 n2))
(define-inline (remainderllong n1 n2) (c-remainderllong n1 n2))

(define-inline (remainders8 n1 n2) ($remainders8 n1 n2))
(define-inline (remainderu8 n1 n2) ($remainderu8 n1 n2))

(define-inline (remainders16 n1 n2) ($remainders16 n1 n2))
(define-inline (remainderu16 n1 n2) ($remainderu16 n1 n2))

(define-inline (remainders32 n1 n2) ($remainders32 n1 n2))
(define-inline (remainderu32 n1 n2) ($remainderu32 n1 n2))

(define-inline (remainders64 n1 n2) ($remainders64 n1 n2))
(define-inline (remainderu64 n1 n2) ($remainderu64 n1 n2))

(define-inline (remainderbx n1 n2) ($remainderbx n1 n2))

;*---------------------------------------------------------------------*/
;*    modulo ...                                                       */
;*---------------------------------------------------------------------*/
(define (modulo n1 n2)
  (int2op modulo n1 n2))

(define (modulofx x y)
  (let ((r (remainderfx x y)))
    (if (zerofx? r)
	r
	(if (positivefx? y)
	    (if (positivefx? r) r (+fx y r))
	    (if (negativefx? r) r (+fx y r))))))

(define (moduloelong x y)
  (let ((r (remainderelong x y)))
    (if (zeroelong? r)
	r
	(if (positiveelong? y)
	    (if (positiveelong? r) r (+elong y r))
	    (if (negativeelong? r) r (+elong y r))))))

(define (modulollong x y)
  (let ((r (remainderllong x y)))
    (if (zerollong? r)
	r
	(if (positivellong? y)
	    (if (positivellong? r) r (+llong y r))
	    (if (negativellong? r) r (+llong y r))))))

(define (modulos8 x y)
  (let ((r (remainders8 x y)))
    (if (zeros8? r)
	r
	(if (positives8? y)
	    (if (positives8? r) r (+s8 y r))
	    (if (negatives8? r) r (+s8 y r))))))
(define (modulou8 x y)
  (let ((r (remainderu8 x y)))
    (if (zerou8? r)
	r
	(if (positiveu8? y)
	    (if (positiveu8? r) r (+u8 y r))
	    (if (negativeu8? r) r (+u8 y r))))))

(define (modulos16 x y)
  (let ((r (remainders16 x y)))
    (if (zeros16? r)
	r
	(if (positives16? y)
	    (if (positives16? r) r (+s16 y r))
	    (if (negatives16? r) r (+s16 y r))))))
(define (modulou16 x y)
  (let ((r (remainderu16 x y)))
    (if (zerou16? r)
	r
	(if (positiveu16? y)
	    (if (positiveu16? r) r (+u16 y r))
	    (if (negativeu16? r) r (+u16 y r))))))

(define (modulos32 x y)
  (let ((r (remainders32 x y)))
    (if (zeros32? r)
	r
	(if (positives32? y)
	    (if (positives32? r) r (+s32 y r))
	    (if (negatives32? r) r (+s32 y r))))))
(define (modulou32 x y)
  (let ((r (remainderu32 x y)))
    (if (zerou32? r)
	r
	(if (positiveu32? y)
	    (if (positiveu32? r) r (+u32 y r))
	    (if (negativeu32? r) r (+u32 y r))))))

(define (modulos64 x y)
  (let ((r (remainders64 x y)))
    (if (zeros64? r)
	r
	(if (positives64? y)
	    (if (positives64? r) r (+s64 y r))
	    (if (negatives64? r) r (+s64 y r))))))
(define (modulou64 x y)
  (let ((r (remainderu64 x y)))
    (if (zerou64? r)
	r
	(if (positiveu64? y)
	    (if (positiveu64? r) r (+u64 y r))
	    (if (negativeu64? r) r (+u64 y r))))))

(define (modulobx x y)
   (let ((r (remainderbx x y)))
      (if (zerobx? r)
	  r
	  (if (positivebx? y)
	      (if (positivebx? r) r (+bx y r))
	      (if (negativebx? r) r (+bx y r))))))

;*---------------------------------------------------------------------*/
;*    gcdop ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (gcdop op x 0op)
   (let ((=op (symbol-append '= op))
	 (zeroop? (symbol-append (symbol-append 'zero op) '?))
	 (absop (symbol-append 'abs op))
	 (remainderop (symbol-append 'remainder op)))
      `(cond ((null? ,x) ,0op)
	     ((null? (cdr ,x)) (,absop (car ,x)))
	     (else
	      (letrec ((gcd2 (lambda (m n)
				(if (,zeroop? n)
				    m
				    (let ((r (,remainderop m n)))
				       (if (,=op r ,0op)
					   n
					   (gcd2 n r)))))))
		 (let loop ((result (gcd2 (,absop (car ,x)) (,absop (cadr ,x))))
			    (left (cddr ,x)))
		    (if (pair? left)
			(loop (gcd2 result (,absop (car left))) (cdr left))
			result)))))))

(define (gcd . x) (gcdop || x 0))

(define (gcdfx . x) (gcdop fx x 0))

(define (gcdelong . x) (gcdop elong x #e0))
(define (gcdllong . x) (gcdop llong x #l0))

(define (gcds8 . x) (gcdop s8 x (fixnum->int8 0)))
(define (gcdu8 . x) (gcdop u8 x (fixnum->uint8 0)))

(define (gcds16 . x) (gcdop s16 x (fixnum->int16 0)))
(define (gcdu16 . x) (gcdop u16 x (fixnum->uint16 0)))

(define (gcds32 . x) (gcdop s32 x (fixnum->int32 0)))
(define (gcdu32 . x) (gcdop u32 x (fixnum->uint32 0)))

(define (gcds64 . x) (gcdop s64 x (fixnum->int64 0)))
(define (gcdu64 . x) (gcdop u64 x (fixnum->uint64 0)))

(define (gcdbx . x)
  (cond ((null? x) (fixnum->bignum 0))
	((null? (cdr x)) (absbx (car x)))
	(else
	 (let loop ((result ($gcdbx (absbx (car x)) (absbx (cadr x))))
		    (left (cddr x)))
	   (if (pair? left)
	       (loop ($gcdbx result (absbx (car left))) (cdr left))
	       result)))))

;*---------------------------------------------------------------------*/
;*    lcmop ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (lcmop op x 0op 1op)
   (let ((=op (symbol-append '= op))
	 (*op (symbol-append '* op))
	 (/op (symbol-append '/ op))
	 (absop (symbol-append 'abs op))
	 (gcdop (symbol-append 'gcd op))
	 (remainderop (symbol-append 'remainder op)))
      `(cond ((null? ,x) ,1op)
	     ((null? (cdr ,x)) (,absop (car ,x)))
	     (else
	      (letrec ((lcm2 (lambda (m n)
				(let ((m (,absop m)) (n (,absop n)))
				   (cond ((,=op m n) m)
					 ((,=op (,remainderop m n) ,0op) m)
					 ((,=op (,remainderop n m) ,0op) n)
					 (else (,*op (,/op m (,gcdop m n)) n)))))))
		 (let loop ((result (lcm2 (car ,x) (cadr ,x)))
			    (left (cddr ,x)))
		    (if (pair? left)
			(loop (lcm2 result (car left)) (cdr left))
			result)))))))

(define (lcm . x) (lcmop || x 0 1))

(define (lcmfx . x) (lcmop fx x 0 1))

(define (lcmelong . x) (lcmop elong x #e0 #e1))

(define (lcmllong . x) (lcmop llong x #l0 #l1))

(define (lcms8 . x) (lcmop s8 x (fixnum->int8 0) (fixnum->int8 1)))
(define (lcmu8 . x) (lcmop u8 x (fixnum->uint8 0) (fixnum->uint8 1)))

(define (lcms16 . x) (lcmop s16 x (fixnum->int16 0) (fixnum->int16 1)))
(define (lcmu16 . x) (lcmop u16 x (fixnum->uint16 0) (fixnum->uint16 1)))

(define (lcms32 . x) (lcmop s32 x (fixnum->int32 0) (fixnum->int32 1)))
(define (lcmu32 . x) (lcmop u32 x (fixnum->uint32 0) (fixnum->uint32 1)))

(define (lcms64 . x) (lcmop s64 x (fixnum->int64 0) (fixnum->int64 1)))
(define (lcmu64 . x) (lcmop u64 x (fixnum->uint64 0) (fixnum->uint64 1)))

(define (lcmbx . x)
  (cond ((null? x) (fixnum->bignum 1))
	((null? (cdr x)) (absbx (car x)))
	(else
	 (let loop ((result ($lcmbx (car x) (cadr x)))
		    (left (cddr x)))
	   (if (pair? left)
	       (loop ($lcmbx result (car left)) (cdr left))
	       result)))))

;*---------------------------------------------------------------------*/
;*    exptfx ...                                                       */
;*---------------------------------------------------------------------*/
(define (exptfx x y)
   (cond
      ((zerofx? y) 1)
      ((evenfx? y) (exptfx (*fx x x) (quotientfx y 2)))
      (else (*fx x (exptfx x (-fx y 1))))))

;*---------------------------------------------------------------------*/
;*    expts32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (expts32 x y)
   (cond
      ((zeros32? y) #s32:1)
      ((evens32? y) (expts32 (*s32 x x) (quotients32 y #s32:2)))
      (else (*s32 x (expts32 x (-s32 y #s32:1))))))

;*---------------------------------------------------------------------*/
;*    exptu32 ...                                                      */
;*---------------------------------------------------------------------*/
(define (exptu32 x y)
   (cond
      ((zerou32? y) #u32:1)
      ((evenu32? y) (exptu32 (*u32 x x) (quotientu32 y #u32:2)))
      (else (*u32 x (exptu32 x (-u32 y #u32:1))))))

;*---------------------------------------------------------------------*/
;*    exptbx ...                                                       */
;*---------------------------------------------------------------------*/
(define (exptbx x y)
   (cond
      ((zerobx? y) #z1)
      ((evenbx? y) (exptbx (*bx x x) (quotientbx y #z2)))
      (else (*bx x (exptbx x (-bx y #z1))))))

;*---------------------------------------------------------------------*/
;*    integer->string-op ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (integer->string-op op x radix)
   (let* ((op->string (symbol-append op '->string))
	  (c-op->string (if (eq? op 'bignum)
			    '$bignum->string
			    (symbol-append 'c- op->string)))
	  (radixv (gensym 'radix)))
      `(let ((,radixv ,radix))
	  (cond
	     ((and (>=fx ,radixv 2) (<=fx ,radixv 36))
	      (,c-op->string ,x ,radixv))
	     (else
	      (error ,(symbol->string op->string) "Illegal radix" ,radixv))))))

;*---------------------------------------------------------------------*/
;*    fixnum->string ...                                               */
;*---------------------------------------------------------------------*/
(define (fixnum->string x #!optional (radix::long 10))
   (integer->string-op fixnum x radix))

;*---------------------------------------------------------------------*/
;*    integer->string ...                                              */
;*---------------------------------------------------------------------*/
(define (integer->string x #!optional (radix::long 10))
   (cond
      ((fixnum? x)
       (fixnum->string x radix))
      ((elong? x)
       (elong->string x radix))
      ((llong? x)
       (llong->string x radix))
      ((bignum? x)
       (bignum->string x radix))
      (else
       (error "->string" "Illegal integer" x))))

;*---------------------------------------------------------------------*/
;*    integer->string/padding ...                                      */
;*---------------------------------------------------------------------*/
(define (integer->string/padding x padding #!optional (radix::long 10))
   (case radix
      ((2 8 10 16)
       ($integer->string/padding x padding radix))
      (else
       (error "integer->string/padding" "Illegal radix" radix))))
   
;*---------------------------------------------------------------------*/
;*    unsigned->string ...                                             */
;*---------------------------------------------------------------------*/
(define (unsigned->string x #!optional (radix::long 16))
   (case radix
      ((2 8 16)
       (cond
	  ((fixnum? x)
	   ($unsigned->string x radix))
	  ((elong? x)
	   ($uelong->string x radix))
	  ((llong? x)
	   ($ullong->string x radix))
	  (else
	   (error "unsigned->string" "Illegal integer" x))))
      (else
       (error "unsigned->string" "Illegal radix" radix))))

;*---------------------------------------------------------------------*/
;*    elong->string ...                                                */
;*---------------------------------------------------------------------*/
(define (elong->string x . radix)
  (integer->string-op elong x (if (null? radix) 10 (car radix))))

;*---------------------------------------------------------------------*/
;*    llong->string ...                                                */
;*---------------------------------------------------------------------*/
(define (llong->string x . radix)
  (integer->string-op llong x (if (null? radix) 10 (car radix))))

;*---------------------------------------------------------------------*/
;*    bignum->string ...                                               */
;*---------------------------------------------------------------------*/
(define (bignum->string x #!optional (radix::long 10))
  (integer->string-op bignum x radix))

;*---------------------------------------------------------------------*/
;*    bignum->octet-string ...                                         */
;*---------------------------------------------------------------------*/
(define (bignum->octet-string x)
   (define (/ceilingfx x y)
      (let ((q (quotientfx x y))
	    (r (remainderfx x y)))
	 (cond
	    ((zerofx? r) q)
	    ((>fx r 0)   (+fx q 1))
	    (else        (-fx q 1)))))

   (define (bignum-bit-length::long b::bignum)
      (let loop ((b b)
		 (res 0))
	 (let ((divided (/bx b #z256)))
	    (cond
	       ((zerobx? b) res)
	       ((zerobx? divided) ;; this is the last octet
		(let ((x (bignum->fixnum b)))
		   (cond
		      ((<fx x #x02) (+fx res 1))
		      ((<fx x #x04) (+fx res 2))
		      ((<fx x #x08) (+fx res 3))
		      ((<fx x #x10) (+fx res 4))
		      ((<fx x #x20) (+fx res 5))
		      ((<fx x #x40) (+fx res 6))
		      ((<fx x #x80) (+fx res 7))
		      (else (+fx res 8)))))
	       (else
		(loop divided (+fx res 8)))))))

   (define (last-char-digit::char x::bignum)
      (integer->char-ur (bignum->fixnum (remainderbx x #z256))))

   (let* ((len (/ceilingfx (bignum-bit-length x) 8))
	  (buffer (make-string len)))
      (let loop ((x x)
		 (i (-fx len 1)))
	 (cond
	    ((and (<fx i 0)
		  (zerobx? x))
	     buffer)
	    ((<fx i 0)
	     (error "bignum->bin-str!" "integer too large" x))
	    (else
	     (string-set! buffer i (last-char-digit x))
	     (loop (/bx x #z256) (-fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    octet-string->bignum ...                                         */
;*---------------------------------------------------------------------*/
(define (octet-string->bignum str)
   (let loop ((i 0)
	      (res #z0))
      (if (=fx i (string-length str))
	  res
	  (loop (+fx i 1)
		(+bx (*bx res #z256)
		     (fixnum->bignum (char->integer (string-ref str i))))))))

;*---------------------------------------------------------------------*/
;*    string->integer ...                                              */
;*    -------------------------------------------------------------    */
;*    Not inlined because it is overriden by a macro.                  */
;*---------------------------------------------------------------------*/
(define (string->integer string #!optional (radix::long 10) (start::long 0))
   (if (and (>=fx radix 2) (<=fx radix 36))
       ;; strtol cannot be renamed as it is used by the compiler
       ;; to optmize the call
       (strtol string start radix)
       (error "string->integer" "Illegal radix" radix)))

;*---------------------------------------------------------------------*/
;*    string->elong ...                                                */
;*---------------------------------------------------------------------*/
(define (string->elong string #!optional (radix::long 10))
   (if (and (>=fx radix 2) (<=fx radix 36))
       (strtoel string 0 radix)
       (error "string->elong" "Illegal radix" radix)))

;*---------------------------------------------------------------------*/
;*    string->llong ...                                                */
;*---------------------------------------------------------------------*/
(define (string->llong string #!optional (radix::long 10))
   (if (and (>=fx radix 2) (<=fx radix 36))
       (strtoll string 0 radix)
       (error "string->llong" "Illegal radix" radix)))

;*---------------------------------------------------------------------*/
;*    string->bignum ...                                               */
;*---------------------------------------------------------------------*/
(define (string->bignum string #!optional (radix::long 10))
  (if (and (>=fx radix 2) (<=fx radix 36))
      ($string->bignum string radix)
      (error "string->bignum" "Illegal radix" radix)))

;*---------------------------------------------------------------------*/
;*    string->integer_obj ...                                          */
;*---------------------------------------------------------------------*/
(define (string->integer-obj string radix)
  ($string->integer-obj string radix))

;*---------------------------------------------------------------------*/
;*    random ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (random max::long)
   (if (=fx max 0) 0 (modulofx ($rand) max)))

;*---------------------------------------------------------------------*/
;*    randombx ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (randombx max::bignum)
   (if (=bx max #z0) #z0 ($randbx max)))

;*---------------------------------------------------------------------*/
;*    seed-random! ...                                                 */
;*---------------------------------------------------------------------*/
(define (seed-random! seed)
   ($seed-rand seed)
   seed)
