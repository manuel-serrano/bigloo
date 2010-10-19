;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Ieee/fixnum.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 10:06:37 1995                          */
;*    Last change :  Mon Oct 18 19:35:10 2010 (serrano)                */
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

   (extern  (macro c-fixnum?::bool (::obj) "INTEGERP")
	    (macro c-elong?::bool (::obj) "ELONGP")
	    (macro c-llong?::bool (::obj) "LLONGP")
	    (macro $minvalfx::long "(LONG_MIN >> TAG_SHIFT)")
	    (macro $maxvalfx::long "(LONG_MAX >> TAG_SHIFT)")
	    (macro $minvalelong::elong "LONG_MIN")
	    (macro $maxvalelong::elong "LONG_MAX")
	    (macro $minvalllong::llong "BGL_LONGLONG_MIN")
	    (macro $maxvalllong::llong "BGL_LONGLONG_MAX")
	    (macro $elong->llong::llong (::elong) "(BGL_LONGLONG_T)")
	    (macro $llong->elong::elong (::llong)  "(long)")
 	    (infix macro c-=fx::bool (::long ::long) "==")
	    (infix macro c-=elong::bool (::elong ::elong) "==")
	    (infix macro c-=llong::bool (::llong ::llong) "==")
	    (infix macro c-<fx::bool (::long ::long) "<")
	    (infix macro c-<elong::bool (::elong ::elong) "<")
	    (infix macro c-<llong::bool (::llong ::llong) "<")
	    (infix macro c-<=fx::bool (::long ::long) "<=")
	    (infix macro c-<=elong::bool (::elong ::elong) "<=")
	    (infix macro c-<=llong::bool (::llong ::llong) "<=")
	    (infix macro c->fx::bool (::long ::long) ">")
	    (infix macro c->elong::bool (::elong ::elong) ">")
	    (infix macro c->llong::bool (::llong ::llong) ">")
	    (infix macro c->=fx::bool (::long ::long) ">=")
	    (infix macro c->=elong::bool (::elong ::elong) ">=")
	    (infix macro c->=llong::bool (::llong ::llong) ">=")
	    (macro c-evenfx?::bool (::long) "EVENP_FX")
 	    (macro c-oddfx?::bool (::long) "ODDP_FX")
 	    (infix macro c-+fx::long (::long ::long) "+")
	    (infix macro c-+elong::elong (::elong ::elong) "+")
	    (infix macro c-+llong::llong (::llong ::llong) "+")
 	    (infix macro c--fx::long (::long ::long) "-")
	    (infix macro c--elong::long (::elong ::elong) "-")
	    (infix macro c--llong::llong (::llong ::llong) "-")
 	    (infix macro c-*fx::long (::long ::long) "*")
	    (infix macro c-*elong::elong (::elong ::elong) "*")
	    (infix macro c-*llong::llong (::llong ::llong) "*")
 	    (infix macro c-/fx::long (::long ::long) "/")
	    (infix macro c-/elong::elong (::elong ::elong) "/")
	    (infix macro c-/llong::llong (::llong ::llong) "/")
 	    (macro c-negfx::long (::long) "NEG")
	    (macro c-negelong::elong (::elong) "NEG")
	    (macro c-negllong::llong (::llong) "NEG")
	    (infix macro c-quotientfx::long (::long ::long) "/")
	    (infix macro c-quotientelong::elong (::elong ::elong) "/")
	    (infix macro c-quotientllong::llong (::llong ::llong) "/")
   	    (infix macro c-remainderfx::long (::long ::long) "%")
	    (infix macro c-remainderelong::elong (::elong ::elong) "%")
	    (infix macro c-remainderllong::llong (::llong ::llong) "%")
	    (macro strtol::long (::string ::long ::long) "strtol")
	    (macro $strtoul::long (::string ::long ::long) "strtoul")
	    (macro strtoel::elong (::string ::long ::long) "strtol")
	    (macro $strtoeul::elong (::string ::long ::long) "strtoul")
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
	       (method static $elong->llong::llong (::elong)
		       "ELONG_TO_LLONG")
	       (method static $llong->elong::elong (::llong)
		       "LLONG_TO_ELONG")
 	       (method static c-=fx::bool (::long ::long)
		       "EQ_FX")
	       (method static c-=elong::bool (::elong ::elong)
		       "EQ_ELONG")
	       (method static c-=llong::bool (::llong ::llong)
		       "EQ_LLONG")
	       (method static c-=fx::bool (::long ::long)
		       "EQ_FX")
	       (method static c-=elong::bool (::elong ::elong)
		       "EQ_ELONG")
	       (method static c-=llong::bool (::llong ::llong)
		       "EQ_LLONG")	       
	       (method static c-<fx::bool (::long ::long)
		       "LT_FX")
	       (method static c-<elong::bool (::elong ::elong)
		       "LT_ELONG")
	       (method static c-<llong::bool (::llong ::llong)
		       "LT_LLONG")
	       (method static c-<=fx::bool (::long ::long)
		       "LE_FX")
	       (method static c-<=elong::bool (::elong ::elong)
		       "LE_ELONG")
	       (method static c-<=llong::bool (::llong ::llong)
		       "LE_LLONG")
	       (method static c->fx::bool (::long ::long)
		       "GT_FX")
	       (method static c->elong::bool (::elong ::elong)
		       "GT_ELONG")
	       (method static c->llong::bool (::llong ::llong)
		       "GT_LLONG")
	       (method static c->=fx::bool (::long ::long)
		       "GE_FX")
	       (method static c->=elong::bool (::elong ::elong)
		       "GE_ELONG")
	       (method static c->=llong::bool (::llong ::llong)
		       "GE_LLONG")
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
 	       (method static c--fx::long (::long ::long)
		       "MINUS_FX")
	       (method static c--elong::elong (::elong ::elong)
		       "MINUS_ELONG")
	       (method static c--llong::llong (::llong ::llong)
		       "MINUS_LLONG")
 	       (method static c-*fx::long (::long ::long)
		       "MUL_FX")
	       (method static c-*elong::elong (::elong ::elong)
		       "MUL_ELONG")
	       (method static c-*llong::llong (::llong ::llong)
		       "MUL_LLONG")
 	       (method static c-/fx::long (::long ::long)
		       "DIV_FX")
	       (method static c-/elong::elong (::elong ::elong)
		       "DIV_ELONG")
	       (method static c-/llong::llong (::llong ::llong)
		       "DIV_LLONG")
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
	       (method static c-remainderfx::long (::long ::long)
		       "REMAINDER_FX")
	       (method static c-remainderelong::elong (::elong ::elong)
		       "REMAINDER_ELONG")
	       (method static c-remainderllong::llong (::llong ::llong)
		       "REMAINDER_LLONG")
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
	    (inline bignum?::bool ::obj)
 	    (inline make-elong::belong ::long)
	    (inline make-llong::bllong ::long)
	    (inline elong->llong::llong ::elong)
	    (inline llong->elong::elong ::llong)
	    (inline fixnum->bignum::bignum ::long)
	    (inline bignum->fixnum::long ::bignum)
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
	    (inline =bx::bool ::bignum ::bignum)
 	    (inline >fx::bool ::long ::long)
	    (inline >elong::bool ::elong ::elong)
	    (inline >llong::bool ::llong ::llong)
	    (inline >bx::bool ::bignum ::bignum)
 	    (inline >=fx::bool ::long ::long)
	    (inline >=elong::bool ::elong ::elong)
	    (inline >=llong::bool ::llong ::llong)
	    (inline >=bx::bool ::bignum ::bignum)
 	    (inline <fx::bool ::long ::long)
	    (inline <elong::bool ::elong ::elong)
	    (inline <llong::bool ::llong ::llong)
	    (inline <bx::bool ::bignum ::bignum)
 	    (inline <=fx::bool ::long ::long)
	    (inline <=elong::bool ::elong ::elong)
	    (inline <=llong::bool ::llong ::llong)
	    (inline <=bx::bool ::bignum ::bignum)
 	    (inline zerofx?::bool ::long)
	    (inline zeroelong?::bool ::elong)
	    (inline zerollong?::bool ::llong)
	    (inline zerobx?::bool ::bignum)
 	    (inline positivefx?::bool ::long)
	    (inline positiveelong?::bool ::elong)
	    (inline positivellong?::bool ::llong)
	    (inline positivebx?::bool ::bignum)
 	    (inline negativefx?::bool ::long)
	    (inline negativeelong?::bool ::elong)
	    (inline negativellong?::bool ::llong)
	    (inline negativebx?::bool ::bignum)
 	    (odd?::bool ::obj)
	    (inline oddfx?::bool ::long)
	    (inline oddelong?::bool ::elong)
	    (inline oddllong?::bool ::llong)
	    (inline oddbx?::bool ::bignum)
 	    (even?::bool ::obj)
	    (inline evenfx?::bool ::long)
	    (inline evenelong?::bool ::elong)
	    (inline evenllong?::bool ::llong)
	    (inline evenbx?::bool ::bignum)
 	    (minfx::long ::long . pair)
	    (minelong::elong ::elong . pair)
	    (minllong::llong ::llong . pair)
	    (minbx::bignum ::bignum . pair)
 	    (maxfx::long ::long . pair)
	    (maxelong::elong ::elong . pair)
	    (maxllong::llong ::llong . pair)
	    (maxbx::bignum ::bignum . pair)
 	    (inline +fx::long ::long ::long)
	    (inline +elong::elong ::elong ::elong)
	    (inline +llong::llong ::llong ::llong)
	    (inline +bx::bignum ::bignum ::bignum)
 	    (inline -fx::long ::long ::long)
	    (inline -elong::elong ::elong ::elong)
	    (inline -llong::llong ::llong ::llong)
	    (inline -bx::bignum ::bignum ::bignum)
 	    (inline *fx::long ::long ::long)
	    (inline *elong::elong ::elong ::elong)
	    (inline *llong::llong ::llong ::llong)
	    (inline *bx::bignum ::bignum ::bignum)
 	    (inline /fx::long ::long ::long)
	    (inline /elong::elong ::elong ::elong)
	    (inline /llong::llong ::llong ::llong)
	    (inline /bx::bignum ::bignum ::bignum)
 	    (inline negfx::long ::long)
	    (inline negelong::elong ::elong)
	    (inline negllong::llong ::llong)
	    (inline negbx::bignum ::bignum)
 	    (inline absfx::long ::long)
	    (inline abselong::elong ::elong)
	    (inline absllong::llong ::llong)
	    (inline absbx::bignum ::bignum)
 	    (remainder::obj ::obj ::obj)
	    (inline remainderfx::long ::long ::long)
	    (inline remainderelong::elong ::elong ::elong)
	    (inline remainderllong::llong ::llong ::llong)
	    (inline remainderbx::bignum ::bignum ::bignum)
 	    (quotient::obj ::obj ::obj)
	    (inline quotientfx::long ::long ::long)
	    (inline quotientelong::elong ::elong ::elong)
	    (inline quotientllong::llong ::llong ::llong)
	    (inline quotientbx::bignum ::bignum ::bignum)
 	    (modulo::obj ::obj ::obj)
	    (modulofx::long ::long ::long)
	    (moduloelong::elong ::elong ::elong)
	    (modulollong::llong ::llong ::llong)
	    (modulobx::bignum ::bignum ::bignum)
 	    (gcd::obj . pair)
	    (gcdfx::long . pair)
	    (gcdelong::elong . pair)
	    (gcdllong::llong . pair)
	    (gcdbx::bignum . pair)
 	    (lcm::obj . pair)
	    (lcmfx::long . pair)
	    (lcmelong::elong . pair)
	    (lcmllong::llong . pair)
	    (lcmbx::bignum . pair)
	    (exptfx::long ::long ::long)
	    (exptbx::bignum ::bignum ::bignum)
	    (integer->string::bstring ::long #!optional (radix::long 10))
	    (fixnum->string::bstring ::long #!optional (radix::long 10))
	    (integer->string/padding::bstring ::long ::long #!optional (radix::long 10))
	    (unsigned->string::bstring ::obj #!optional (radix::long 16))
	    (string->integer::long ::bstring . pair)
	    (elong->string::bstring ::elong . pair)
	    (string->elong::elong ::bstring . pair)
	    (llong->string::bstring ::llong . pair)
	    (string->llong::llong ::bstring . pair)
	    (bignum->string::bstring ::bignum #!optional (radix::long 10))
	    (string->bignum::bignum ::bstring #!optional (radix::long 10))
	    (bignum->octet-string::bstring ::bignum)
	    (octet-string->bignum::bignum ::bstring)
	    (string->integer-obj::obj ::bstring ::long)
 	    (inline random::long ::long)
 	    (inline randombx::bignum ::bignum)
	    (seed-random! ::int))
   
   (pragma  (fixnum? (predicate-of bint) no-cfa-top nesting)
	    (c-fixnum? side-effect-free (predicate-of bint) no-cfa-top nesting args-safe (effect))
	    (c-elong? side-effect-free (predicate-of belong) no-cfa-top nesting (effect))
	    (c-llong? side-effect-free (predicate-of bllong) no-cfa-top nesting (effect))
	    (bignum? side-effect-free no-cfa-top nesting (effect))
 	    ($elong->llong side-effect-free args-safe (effect))
	    ($llong->elong side-effect-free args-safe (effect))
	    (fixnum->bignum side-effect-free no-cfa-top nesting (effect))
	    (bignum->fixnum side-effect-free no-cfa-top nesting (effect))
	    (elong->bignum side-effect-free no-cfa-top nesting (effect))
	    (llong->bignum side-effect-free no-cfa-top nesting (effect))
	    (integer? side-effect-free no-cfa-top nesting (effect))
	    (=fx side-effect-free no-cfa-top nesting (effect))
	    (=elong side-effect-free no-cfa-top nesting (effect))
	    (=llong side-effect-free no-cfa-top nesting (effect))
	    (=bx side-effect-free no-cfa-top nesting (effect))
 	    (>fx side-effect-free no-cfa-top nesting (effect))
	    (>elong side-effect-free no-cfa-top nesting (effect))
	    (>llong side-effect-free no-cfa-top nesting (effect))
	    (>bx side-effect-free no-cfa-top nesting (effect))
 	    (>=fx side-effect-free no-cfa-top nesting (effect))
	    (>=elong side-effect-free no-cfa-top nesting (effect))
	    (>=llong side-effect-free no-cfa-top nesting (effect))
	    (>=bx side-effect-free no-cfa-top nesting (effect))
 	    (<fx side-effect-free no-cfa-top nesting (effect))
	    (<elong side-effect-free no-cfa-top nesting (effect))
	    (<llong side-effect-free no-cfa-top nesting (effect))
	    (<bx side-effect-free no-cfa-top nesting (effect))
 	    (<=fx side-effect-free no-cfa-top nesting (effect))
	    (<=elong side-effect-free no-cfa-top nesting (effect))
	    (<=llong side-effect-free no-cfa-top nesting (effect))
	    (<=bx side-effect-free no-cfa-top nesting (effect))
 	    (odd? side-effect-free no-cfa-top nesting (effect))
	    (oddbx? side-effect-free no-cfa-top nesting (effect))
 	    (c-oddfx? side-effect-free no-cfa-top nesting args-safe (effect))
 	    (even? side-effect-free no-cfa-top nesting (effect))
	    (evenbx? side-effect-free no-cfa-top nesting (effect))
 	    (c-evenfx? side-effect-free no-cfa-top nesting args-safe (effect))
 	    (+fx side-effect-free no-cfa-top nesting (effect))
	    (+elong side-effect-free no-cfa-top nesting (effect))
	    (+llong side-effect-free no-cfa-top nesting (effect))
	    (+bx side-effect-free no-cfa-top nesting (effect))
 	    (-fx side-effect-free no-cfa-top nesting (effect))
	    (-elong side-effect-free no-cfa-top nesting (effect))
	    (-llong side-effect-free no-cfa-top nesting (effect))
	    (-bx side-effect-free no-cfa-top nesting (effect))
 	    (*fx side-effect-free no-cfa-top nesting (effect))
	    (*elong side-effect-free no-cfa-top nesting (effect))
	    (*llong side-effect-free no-cfa-top nesting (effect))
	    (*bx side-effect-free no-cfa-top nesting (effect))
 	    (/fx side-effect-free no-cfa-top nesting (effect))
	    (/elong side-effect-free no-cfa-top nesting (effect))
	    (/llong side-effect-free no-cfa-top nesting (effect))
	    (/bx side-effect-free no-cfa-top nesting (effect))
 	    (remainderfx side-effect-free no-cfa-top nesting (effect))
	    (remainderelong side-effect-free no-cfa-top nesting (effect))
	    (remainderllong side-effect-free no-cfa-top nesting (effect))
	    (remainderbx side-effect-free no-cfa-top nesting (effect))
 	    (fixnum->string side-effect-free no-cfa-top nesting (effect))
 	    (integer->string side-effect-free no-cfa-top nesting (effect))
	    (string->integer side-effect-free no-cfa-top nesting (effect))
	    (bignum->string side-effect-free no-cfa-top nesting (effect))
	    (bignum->octet-string side-effect-free no-cfa-top nesting (effect))
	    (string->bignum side-effect-free no-cfa-top nesting (effect))
	    (octet-string->bignum side-effect-free no-cfa-top nesting (effect))
 	    (modulo side-effect-free no-cfa-top nesting (effect))
	    (modulobx side-effect-free no-cfa-top nesting (effect))
	    (quotientfx side-effect-free no-cfa-top nesting (effect))
	    (quotientbx side-effect-free no-cfa-top nesting (effect))
 	    (gcdfx side-effect-free no-cfa-top nesting (effect))
	    (gcdelong side-effect-free no-cfa-top nesting (effect))
	    (gcdllong side-effect-free no-cfa-top nesting (effect))
	    (gcdbx side-effect-free no-cfa-top nesting (effect))
 	    (lcmfx side-effect-free no-cfa-top nesting (effect))
	    (lcmelong side-effect-free no-cfa-top nesting (effect))
	    (lcmllong side-effect-free no-cfa-top nesting (effect))
	    (lcmbx side-effect-free no-cfa-top nesting (effect))
	    (exptfx side-effect-free no-cfa-top nesting (effect))
	    (exptbx side-effect-free no-cfa-top nesting (effect))
 	    (positivefx? side-effect-free no-cfa-top nesting (effect))
	    (positiveelong? side-effect-free no-cfa-top nesting (effect))
	    (positivellong? side-effect-free no-cfa-top nesting (effect))
	    (positivebx? side-effect-free no-cfa-top nesting (effect))
 	    (negativefx? side-effect-free no-cfa-top nesting (effect))
	    (negativeelong? side-effect-free no-cfa-top nesting (effect))
	    (negativellong? side-effect-free no-cfa-top nesting (effect))
	    (negativebx? side-effect-free no-cfa-top nesting (effect))
 	    (zerofx? side-effect-free no-cfa-top nesting (effect))
	    (zeroelong? side-effect-free no-cfa-top nesting (effect))
	    (zerollong? side-effect-free no-cfa-top nesting (effect))
	    (zerobx? side-effect-free no-cfa-top nesting (effect))
 	    (negfx side-effect-free no-cfa-top nesting (effect))
	    (negelong side-effect-free no-cfa-top nesting (effect))
	    (negllong side-effect-free no-cfa-top nesting (effect))
	    (negbx side-effect-free no-cfa-top nesting (effect))
 	    (absbx side-effect-free no-cfa-top nesting (effect))
 	    (c-=fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-=elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-=llong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c->fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c->elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c->llong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c->=fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c->=elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c->=llong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-<fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-<elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-<llong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-<=fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-<=elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-<=llong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-oddfx? side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-+fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-+elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-+llong side-effect-free no-cfa-top nesting args-safe (effect))
 	    (c--fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c--elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c--llong side-effect-free no-cfa-top nesting args-safe (effect))
 	    (c-*fx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-*elong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-*llong side-effect-free no-cfa-top nesting args-safe (effect))
 	    (c-/fx side-effect-free no-cfa-top nesting args-safe (effect))
 	    (c-negfx side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-negelong side-effect-free no-cfa-top nesting args-safe (effect))
	    (c-negllong side-effect-free no-cfa-top nesting args-safe (effect))
	    (random side-effect-free no-cfa-top nesting (effect))
	    (randombx side-effect-free no-cfa-top nesting (effect))))

;*---------------------------------------------------------------------*/
;*    integer? ...                                                     */
;*---------------------------------------------------------------------*/
(define (integer? obj)
   (or (c-fixnum? obj)
       (c-elong? obj)
       (c-llong? obj)
       ($bignum? obj)
       (and (c-flonum? obj) (=fl obj (roundfl obj)))))

;*---------------------------------------------------------------------*/
;*    fixnum? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (fixnum? obj)
   (c-fixnum? obj))

;*---------------------------------------------------------------------*/
;*    elong? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (elong? obj)
   (c-elong? obj))

;*---------------------------------------------------------------------*/
;*    llong? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (llong? obj)
   (c-llong? obj))

;*---------------------------------------------------------------------*/
;*    bignum? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (bignum? obj)
   ($bignum? obj))

;*---------------------------------------------------------------------*/
;*    make-elong ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (make-elong long)
   ($long->belong long))

;*---------------------------------------------------------------------*/
;*    make-llong ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (make-llong long)
   ($long->bllong long))

;*---------------------------------------------------------------------*/
;*    elong->llong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (elong->llong x)
   ($elong->llong x))

;*---------------------------------------------------------------------*/
;*    llong->elong ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (llong->elong x)
   ($llong->elong x))

;*---------------------------------------------------------------------*/
;*    fixnum->bignum ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (fixnum->bignum x)
   ($fixnum->bignum x))

;*---------------------------------------------------------------------*/
;*    bignum->fixnum ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (bignum->fixnum x)
   ($bignum->fixnum x))

;*---------------------------------------------------------------------*/
;*    elong->bignum ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (elong->bignum x)
   ($elong->bignum x))

;*---------------------------------------------------------------------*/
;*    llong->bignum ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (llong->bignum x)
   ($llong->bignum x))

;*---------------------------------------------------------------------*/
;*    {min,max}val{fx,elong,llong} ...                                 */
;*---------------------------------------------------------------------*/
(define-inline (minvalfx) $minvalfx)
(define-inline (maxvalfx) $maxvalfx)
(define-inline (minvalelong) $minvalelong)
(define-inline (maxvalelong) $maxvalelong)
(define-inline (minvalllong) $minvalllong)
(define-inline (maxvalllong) $maxvalllong)

;*---------------------------------------------------------------------*/
;*    =fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (=fx n1 n2)
   (c-=fx n1 n2))

;*---------------------------------------------------------------------*/
;*    =elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (=elong n1 n2)
   (c-=elong n1 n2))

;*---------------------------------------------------------------------*/
;*    =llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (=llong n1 n2)
   (c-=llong n1 n2))

;*---------------------------------------------------------------------*/
;*    =bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (=bx n1 n2)
   (=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    <fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (<fx n1 n2)
   (c-<fx n1 n2))

;*---------------------------------------------------------------------*/
;*    <elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (<elong n1 n2)
   (c-<elong n1 n2))

;*---------------------------------------------------------------------*/
;*    <llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (<llong n1 n2)
   (c-<llong n1 n2))

;*---------------------------------------------------------------------*/
;*    <bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (<bx n1 n2)
   (<fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    >fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (>fx n1 n2)
   (c->fx n1 n2))

;*---------------------------------------------------------------------*/
;*    >elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (>elong n1 n2)
   (c->elong n1 n2))

;*---------------------------------------------------------------------*/
;*    >llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (>llong n1 n2)
   (c->llong n1 n2))

;*---------------------------------------------------------------------*/
;*    >bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (>bx n1 n2)
   (>fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    <=fx ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (<=fx n1 n2)
   (c-<=fx n1 n2))

;*---------------------------------------------------------------------*/
;*    <=elong ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (<=elong n1 n2)
   (c-<=elong n1 n2))

;*---------------------------------------------------------------------*/
;*    <=llong ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (<=llong n1 n2)
   (c-<=llong n1 n2))

;*---------------------------------------------------------------------*/
;*    <=bx ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (<=bx n1 n2)
   (<=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    >=fx ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (>=fx n1 n2)
   (c->=fx n1 n2))

;*---------------------------------------------------------------------*/
;*    >=elong ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (>=elong n1 n2)
   (c->=elong n1 n2))

;*---------------------------------------------------------------------*/
;*    >=llong ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (>=llong n1 n2)
   (c->=llong n1 n2))

;*---------------------------------------------------------------------*/
;*    >=bx ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (>=bx n1 n2)
   (>=fx ($bignum-cmp n1 n2) 0))

;*---------------------------------------------------------------------*/
;*    zerofx? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (zerofx? n)
   (=fx n 0))

;*---------------------------------------------------------------------*/
;*    zeroelong? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (zeroelong? n)
   (=elong n #e0))

;*---------------------------------------------------------------------*/
;*    zerollong? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (zerollong? n)
   (=llong n #l0))

;*---------------------------------------------------------------------*/
;*    zerobx? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (zerobx? n)
   ($zerobx? n))

;*---------------------------------------------------------------------*/
;*    positivefx?  ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (positivefx? n)
   (>fx n 0))

;*---------------------------------------------------------------------*/
;*    positiveelong?  ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (positiveelong? n)
   (>elong n #e0))

;*---------------------------------------------------------------------*/
;*    positivellong?  ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (positivellong? n)
   (>llong n #l0))

;*---------------------------------------------------------------------*/
;*    positivebx?  ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (positivebx? n)
   ($positivebx? n))

;*---------------------------------------------------------------------*/
;*    negativefx? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (negativefx? n)
   (<fx n 0))

;*---------------------------------------------------------------------*/
;*    negativeelong? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (negativeelong? n)
   (<elong n #e0))

;*---------------------------------------------------------------------*/
;*    negativellong? ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (negativellong? n)
   (<llong n #l0))

;*---------------------------------------------------------------------*/
;*    negativebx? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (negativebx? n)
   ($negativebx? n))

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

;*---------------------------------------------------------------------*/
;*    oddfx? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (oddfx? x)
   (c-oddfx? x))

;*---------------------------------------------------------------------*/
;*    oddelong? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (oddelong? x)
   (if (evenelong? x) #f #t))

;*---------------------------------------------------------------------*/
;*    oddllong? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (oddllong? x)
   (if (evenllong? x) #f #t))

;*---------------------------------------------------------------------*/
;*    oddbx? ...                                                       */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    evenfx? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (evenfx? x)
   (c-evenfx? x))

;*---------------------------------------------------------------------*/
;*    evenelong? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (evenelong? x)
   (zeroelong? (remainderelong x #e2)))

;*---------------------------------------------------------------------*/
;*    evenllong? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (evenllong? x)
   (zerollong? (remainderllong x #l2)))

;*---------------------------------------------------------------------*/
;*    evenbx? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (evenbx? x)
   ($evenbx? x))

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

;*---------------------------------------------------------------------*/
;*    minfx ...                                                        */
;*---------------------------------------------------------------------*/
(define (minfx n1 . nn)
   (min/max <fx n1 . nn))

;*---------------------------------------------------------------------*/
;*    minelong ...                                                     */
;*---------------------------------------------------------------------*/
(define (minelong n1 . nn)
   (min/max <elong n1 . nn))

;*---------------------------------------------------------------------*/
;*    minllong ...                                                     */
;*---------------------------------------------------------------------*/
(define (minllong n1 . nn)
   (min/max <llong n1 . nn))

;*---------------------------------------------------------------------*/
;*    minbx ...                                                        */
;*---------------------------------------------------------------------*/
(define (minbx n1 . nn)
   (min/max <bx n1 . nn))

;*---------------------------------------------------------------------*/
;*    maxfx ...                                                        */
;*---------------------------------------------------------------------*/
(define (maxfx n1 . nn)
   (min/max >fx n1 . nn))

;*---------------------------------------------------------------------*/
;*    maxelong ...                                                     */
;*---------------------------------------------------------------------*/
(define (maxelong n1 . nn)
   (min/max >elong n1 . nn))

;*---------------------------------------------------------------------*/
;*    maxllong ...                                                     */
;*---------------------------------------------------------------------*/
(define (maxllong n1 . nn)
   (min/max >llong n1 . nn))

;*---------------------------------------------------------------------*/
;*    maxbx ...                                                        */
;*---------------------------------------------------------------------*/
(define (maxbx n1 . nn)
   (min/max >bx n1 . nn))

;*---------------------------------------------------------------------*/
;*    +fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (+fx z1 z2)
   (c-+fx z1 z2))

;*---------------------------------------------------------------------*/
;*    +elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (+elong z1 z2)
   (c-+elong z1 z2))

;*---------------------------------------------------------------------*/
;*    +llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (+llong z1 z2)
   (c-+llong z1 z2))

;*---------------------------------------------------------------------*/
;*    +bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (+bx z1 z2)
   ($+bx z1 z2))

;*---------------------------------------------------------------------*/
;*    -fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (-fx z1 z2)
   (c--fx z1 z2))

;*---------------------------------------------------------------------*/
;*    -elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (-elong z1 z2)
   (c--elong z1 z2))

;*---------------------------------------------------------------------*/
;*    -llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (-llong z1 z2)
   (c--llong z1 z2))

;*---------------------------------------------------------------------*/
;*    -bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (-bx z1 z2)
   ($-bx z1 z2))

;*---------------------------------------------------------------------*/
;*    *fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (*fx z1 z2)
   (c-*fx z1 z2))

;*---------------------------------------------------------------------*/
;*    *elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (*elong z1 z2)
   (c-*elong z1 z2))

;*---------------------------------------------------------------------*/
;*    *llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (*llong z1 z2)
   (c-*llong z1 z2))

;*---------------------------------------------------------------------*/
;*    *bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (*bx z1 z2)
   ($*bx z1 z2))

;*---------------------------------------------------------------------*/
;*    /fx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (/fx z1 z2)
   (c-/fx z1 z2))

;*---------------------------------------------------------------------*/
;*    /elong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (/elong z1 z2)
   (c-/elong z1 z2))

;*---------------------------------------------------------------------*/
;*    /llong ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (/llong z1 z2)
   (c-/llong z1 z2))

;*---------------------------------------------------------------------*/
;*    /bx ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (/bx z1 z2)
   ($quotientbx z1 z2))

;*---------------------------------------------------------------------*/
;*    negfx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (negfx n1)
   (c-negfx n1))

;*---------------------------------------------------------------------*/
;*    negelong ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (negelong n1)
   (c-negelong n1))

;*---------------------------------------------------------------------*/
;*    negllong ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (negllong n1)
   (c-negllong n1))

;*---------------------------------------------------------------------*/
;*    negbx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (negbx n1)
   ($negbx n1))

;*---------------------------------------------------------------------*/
;*    absfx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (absfx n)
   (if (<fx n 0)
       (negfx n)
       n))

;*---------------------------------------------------------------------*/
;*    abselong ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (abselong n)
   (if (<elong n #e0)
       (negelong n)
       n))

;*---------------------------------------------------------------------*/
;*    absllong ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (absllong n)
   (if (<llong n #l0)
       (negllong n)
       n))

;*---------------------------------------------------------------------*/
;*    absbx ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (absbx n)
   ($absbx n))

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
(define (quotient n1 n2)
  (int2op quotient n1 n2 'safe))

;*---------------------------------------------------------------------*/
;*    quotientfx ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (quotientfx n1 n2)
   (c-quotientfx n1 n2))

;*---------------------------------------------------------------------*/
;*    quotientelong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (quotientelong n1 n2)
   (c-quotientelong n1 n2))

;*---------------------------------------------------------------------*/
;*    quotientllong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (quotientllong n1 n2)
   (c-quotientllong n1 n2))

;*---------------------------------------------------------------------*/
;*    quotientbx ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (quotientbx n1 n2)
   ($quotientbx n1 n2))

;*---------------------------------------------------------------------*/
;*    remainder ...                                                    */
;*---------------------------------------------------------------------*/
(define (remainder n1 n2)
  (int2op remainder n1 n2))

;*---------------------------------------------------------------------*/
;*    remainderfx ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (remainderfx n1 n2)
   (c-remainderfx n1 n2))

;*---------------------------------------------------------------------*/
;*    remainderelong ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (remainderelong n1 n2)
   (c-remainderelong n1 n2))

;*---------------------------------------------------------------------*/
;*    remainderllong ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (remainderllong n1 n2)
   (c-remainderllong n1 n2))

;*---------------------------------------------------------------------*/
;*    remainderbx ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (remainderbx n1 n2)
   ($remainderbx n1 n2))

;*---------------------------------------------------------------------*/
;*    modulo ...                                                       */
;*---------------------------------------------------------------------*/
(define (modulo n1 n2)
  (int2op modulo n1 n2))

;*---------------------------------------------------------------------*/
;*    modulofx ...                                                     */
;*---------------------------------------------------------------------*/
(define (modulofx x y)
  (let ((r (remainderfx x y)))
    (if (zerofx? r)
	r
	(if (positivefx? y)
	    (if (positivefx? r) r (+fx y r))
	    (if (negativefx? r) r (+fx y r))))))

;*---------------------------------------------------------------------*/
;*    moduloelong ...                                                  */
;*---------------------------------------------------------------------*/
(define (moduloelong x y)
  (let ((r (remainderelong x y)))
    (if (zeroelong? r)
	r
	(if (positiveelong? y)
	    (if (positiveelong? r) r (+elong y r))
	    (if (negativeelong? r) r (+elong y r))))))

;*---------------------------------------------------------------------*/
;*    modulollong ...                                                  */
;*---------------------------------------------------------------------*/
(define (modulollong x y)
  (let ((r (remainderllong x y)))
    (if (zerollong? r)
	r
	(if (positivellong? y)
	    (if (positivellong? r) r (+llong y r))
	    (if (negativellong? r) r (+llong y r))))))

;*---------------------------------------------------------------------*/
;*    modulobx ...                                                     */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    gcdfx ...                                                        */
;*---------------------------------------------------------------------*/
(define (gcdfx . x)
   (gcdop fx x 0))

;*---------------------------------------------------------------------*/
;*    gcdelong ...                                                     */
;*---------------------------------------------------------------------*/
(define (gcdelong . x)
   (gcdop elong x #e0))

;*---------------------------------------------------------------------*/
;*    gcdllong ...                                                     */
;*---------------------------------------------------------------------*/
(define (gcdllong . x)
   (gcdop llong x #l0))

;*---------------------------------------------------------------------*/
;*    gcdbx ...                                                        */
;*---------------------------------------------------------------------*/
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
;*    gcd ...                                                          */
;*---------------------------------------------------------------------*/
(define (gcd . x)
   (gcdop || x 0))

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

;*---------------------------------------------------------------------*/
;*    lcmfx ...                                                        */
;*---------------------------------------------------------------------*/
(define (lcmfx . x)
   (lcmop fx x 0 1))

;*---------------------------------------------------------------------*/
;*    lcmelong ...                                                     */
;*---------------------------------------------------------------------*/
(define (lcmelong . x)
   (lcmop elong x #e0 #e1))

;*---------------------------------------------------------------------*/
;*    lcmllong ...                                                     */
;*---------------------------------------------------------------------*/
(define (lcmllong . x)
   (lcmop llong x #l0 #l1))

;*---------------------------------------------------------------------*/
;*    lcmbx ...                                                        */
;*---------------------------------------------------------------------*/
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
;*    lcm ...                                                          */
;*---------------------------------------------------------------------*/
(define (lcm . x)
   (lcmop || x 0 1))

;*---------------------------------------------------------------------*/
;*    exptfx ...                                                       */
;*---------------------------------------------------------------------*/
(define (exptfx x y)
   (cond
      ((zerofx? y)
       1)
      ((evenfx? y)
       (exptfx (*fx x x) (quotientfx y 2)))
      (else
       (*fx x (exptfx x (-fx y 1))))))

;*---------------------------------------------------------------------*/
;*    exptbx ...                                                       */
;*---------------------------------------------------------------------*/
(define (exptbx x y)
   (cond
      ((zerobx? y)
       #z1)
      ((evenbx? y)
       (exptbx (*bx x x) (quotientbx y #z2)))
      (else
       (*bx x (exptbx x (-bx y #z1))))))

;*---------------------------------------------------------------------*/
;*    integer->string-op ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (integer->string-op op x radix)
   (let* ((op->string (symbol-append op '->string))
	  (c-op->string (symbol-append 'c- op->string))
	  (radixv (gensym 'radix)))
      `(let ((,radixv ,radix))
	  (case ,radixv
	     ((2 8 10 16)
	      (,c-op->string ,x ,radixv))
	     (else
	      (error ,(symbol->string op->string) "Illegal radix" ,radixv))))))

;*---------------------------------------------------------------------*/
;*    fixnum->string ...                                               */
;*---------------------------------------------------------------------*/
(define (fixnum->string x #!optional (radix::long 10))
   (integer->string-op fixnum x radix))

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
;*---------------------------------------------------------------------*/
(define (string->integer string . radix)
   (let ((r (if (null? radix) 10 (car radix))))
      (case r
	 ((2 8 10 16)
	  (strtol string 0 r))
	 (else
	  (error "string->integer" "Illegal radix" r)))))

;*---------------------------------------------------------------------*/
;*    string->elong ...                                                */
;*---------------------------------------------------------------------*/
(define (string->elong string . radix)
   (let ((r (if (null? radix) 10 (car radix))))
      (case r
	 ((2 8 10 16)
	  (strtoel string 0 r))
	 (else
	  (error "string->elong" "Illegal radix" r)))))

;*---------------------------------------------------------------------*/
;*    string->llong ...                                                */
;*---------------------------------------------------------------------*/
(define (string->llong string . radix)
   (let ((r (if (null? radix) 10 (car radix))))
      (case r
	 ((2 8 10 16)
	  (strtoll string 0 r))
	 (else
	  (error "string->llong" "Illegal radix" r)))))

;*---------------------------------------------------------------------*/
;*    string->bignum ...                                               */
;*---------------------------------------------------------------------*/
(define (string->bignum string #!optional (radix::long 10))
  (if (and (>=fx radix 2)
	   (<=fx radix 36))
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
   (modulofx ($rand) max))

;*---------------------------------------------------------------------*/
;*    randombx ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (randombx max::bignum)
   ($randbx max))

;*---------------------------------------------------------------------*/
;*    seed-random! ...                                                 */
;*---------------------------------------------------------------------*/
(define (seed-random! seed)
   ($seed-rand seed)
   seed)
