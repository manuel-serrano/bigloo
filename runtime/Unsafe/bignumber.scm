;*=====================================================================*/
;*    /priv/serrano2/bigloo/wasm/runtime/Unsafe/bignumber.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Marc Feeley                                       */
;*    Creation    :  Tue Mar 11 11:32:17 2008                          */
;*    Last change :  Wed Sep 25 10:31:38 2024 (serrano)                */
;*    Copyright   :  2006-24 Marc Feeley                               */
;*    -------------------------------------------------------------    */
;*    Bigloo two implementations                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bignum

   (import  __error
	    __object
	    __thread
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __dsssl
	    __bexit
	    __srfi4
	    __bit
	    __evenv
	    
	    __r5_control_features_6_4
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_control_features_6_9)

   (cond-expand
      ((and enable-gmp (not boot))
       (include "./Unsafe/bignumber-gmp.sch"))
      (bigloo-wasm
       (include "./Unsafe/bignumber-gmp.sch"))
      (else
       (include "./Unsafe/bignumber-generic.sch")))

   (extern
      (macro $bignum?::bool (::obj) "BIGNUMP")
      
      (macro $bignum->fixnum-safe::obj (::obj) "BGL_SAFE_BX_TO_FX")
      (macro +fx-safe::obj (::long ::long) "BGL_SAFE_PLUS_FX")
      (macro -fx-safe::obj (::long ::long) "BGL_SAFE_MINUS_FX")
      (macro *fx-safe::obj (::long ::long) "BGL_SAFE_MUL_FX")
      
      (macro +elong-safe::obj (::elong ::elong) "BGL_SAFE_PLUS_ELONG")
      (macro -elong-safe::obj (::elong ::elong) "BGL_SAFE_MINUS_ELONG")
      (macro *elong-safe::obj (::elong ::elong) "BGL_SAFE_MUL_ELONG")
      
      (macro +llong-safe::obj (::llong ::llong) "BGL_SAFE_PLUS_LLONG")
      (macro -llong-safe::obj (::llong ::llong) "BGL_SAFE_MINUS_LLONG")
      (macro *llong-safe::obj (::llong ::llong) "BGL_SAFE_MUL_LLONG")
      
      (macro $quotientfx-safe::obj (::long ::long) "BGL_SAFE_QUOTIENT_FX")
      (macro $quotientelong-safe::obj (::elong ::elong) "BGL_SAFE_QUOTIENT_ELONG")
      (macro $quotientllong-safe::obj (::llong ::llong) "BGL_SAFE_QUOTIENT_LLONG"))

   (wasm
      ($bignum? "(ref.test (ref $bignum) ~0)")
      ($fixnum->bignum "(call $bgl_long_to_bignum ~0)"))

   (java
      (class foreign
	 (method static $bignum?::bool (::obj)
	    "BIGNUMP")
	 (method static $fixnum->bignum::bignum (::long)
	    "LONG_TO_BIGNUM")
	 (method static $elong->bignum::bignum (::elong)
	    "ELONG_TO_BIGNUM")
	 (method static $llong->bignum::bignum (::llong)
	    "LLONG_TO_BIGNUM")
	 (method static $uint64->bignum::bignum (::uint64)
	    "LLONG_TO_BIGNUM")
	 (method static $int64->bignum::bignum (::int64)
	    "LLONG_TO_BIGNUM")
	 (method static $bignum->int64::llong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum->uint64::llong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum->fixnum::long (::bignum)
	    "bgl_bignum_to_long")
	 (method static $bignum->elong::elong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum->llong::llong (::bignum)
	    "bgl_bignum_to_llong")
	 (method static $bignum-cmp::int (::bignum ::bignum)
	    "CMP_BIGNUM")
	 (method static $zerobx?::bool (::bignum)
	    "ZEROP_BIGNUM")
	 (method static $evenbx?::bool (::bignum)
	    "EVENP_BIGNUM")
	 (method static $oddbx?::bool (::bignum)
	    "ODDP_BIGNUM")
	 (method static $positivebx?::bool (::bignum)
	    "POSITIVEP_BIGNUM")
	 (method static $negativebx?::bool (::bignum)
	    "NEGATIVEP_BIGNUM")
	 (method static $absbx::bignum (::bignum)
	    "ABS_BIGNUM")
	 (method static $negbx::bignum (::bignum)
	    "NEG_BIGNUM")
	 (method static $+bx::bignum (::bignum ::bignum)
	    "PLUS_BIGNUM")
	 (method static $-bx::bignum (::bignum ::bignum)
	    "MINUS_BIGNUM")
	 (method static $*bx::bignum (::bignum ::bignum)
	    "MUL_BIGNUM")
	 (method static $divrembx::bignum (::bignum ::bignum)
	    "DIVREM_BIGNUM")
	 (method static $quotientbx::bignum (::bignum ::bignum)
	    "QUOTIENT_BIGNUM")
	 (method static $bignum->fixnum-safe::obj (::obj)
	    "BGL_SAFE_BX_TO_FX")
	 (method static $quotientfx-safe::obj (::long ::long)
	    "SAFE_DIV_FX")
	 (method static $quotientelong-safe::obj (::elong ::elong)
	    "SAFE_DIV_ELONG")
	 (method static $quotientllong-safe::obj (::llong ::llong)
	    "SAFE_DIV_LLONG")
	 (method static $remainderbx::bignum (::bignum ::bignum)
	    "REMAINDER_BIGNUM")
	 (method static $gcdbx::bignum (::bignum ::bignum)
	    "GCD_BIGNUM")
	 (method static $lcmbx::bignum (::bignum ::bignum)
	    "LCM_BIGNUM")
	 (method static $bignum->string::string (::bignum ::long)
	    "bgl_bignum_to_string")
	 (method static $string->bignum::bignum (::string ::int)
	    "bgl_string_to_bignum")
	 (method static $string->integer-obj::obj (::string ::int)
	    "bgl_string_to_integer_obj")
	 (method static $randbx::bignum (::bignum)
	    "bgl_rand_bignum")
	 (method static +fx-safe::obj (::long ::long)
	    "SAFE_PLUS_FX")
	 (method static -fx-safe::obj (::long ::long)
	    "SAFE_MINUS_FX")
	 (method static *fx-safe::obj (::long ::long)
	    "SAFE_MUL_FX")
	 (method static $bitlshbx::bignum (::bignum ::long)
	    "BITLSH_BIGNUM")
	 (method static $bitrshbx::bignum (::bignum ::long)
	    "BITRSH_BIGNUM")
	 (method static $bitorbx::bignum (::bignum ::bignum)
	    "BITOR_BIGNUM")
	 (method static $bitxorbx::bignum (::bignum ::bignum)
	    "BITXOR_BIGNUM")
	 (method static $bitandbx::bignum (::bignum ::bignum)
	    "BITAND_BIGNUM")
	 (method static $bitmaskbx::bignum (::bignum ::long)
	    "BITMASK_BIGNUM")
	 (method static $bitnotbx::bignum (::bignum)
	    "BITNOT_BIGNUM")
	 (method static +elong-safe::obj (::elong ::elong)
	    "SAFE_PLUS_ELONG")
	 (method static -elong-safe::obj (::elong ::elong)
	    "SAFE_MINUS_ELONG")
	 (method static *elong-safe::obj (::elong ::elong)
	    "SAFE_MUL_ELONG")
	 
	 (method static +llong-safe::obj (::llong ::llong)
	    "SAFE_PLUS_LLONG")
	 (method static -llong-safe::obj (::llong ::llong)
	    "SAFE_MINUS_LLONG")
	 (method static *llong-safe::obj (::llong ::llong)
	    "SAFE_MUL_LLONG")
	 
	 (method static $bignum->flonum::double (::bignum)
	    "BIGNUM_TO_FLONUM")
	 (method static $flonum->bignum::bignum (::double)
	    "FLONUM_TO_BIGNUM")))

    (pragma
      ($bignum? side-effect-free (predicate-of bignum) no-cfa-top nesting fail-safe)
      ($bignum->fixnum-safe fail-safe)
      (+fx-safe fail-safe side-effect-free)
      (-fx-safe fail-safe side-effect-free)
      (*fx-safe fail-safe side-effect-free)
      (+elong-safe fail-safe side-effect-free)
      (-elong-safe fail-safe side-effect-free)
      (*elong-safe fail-safe side-effect-free)
      (+llong-safe fail-safe side-effect-free)
      (-llong-safe fail-safe side-effect-free)
      (*llong-safe fail-safe side-effect-free)))
