;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Unsafe/bignumber-gmp.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Aug 29 07:38:03 2024                          */
;*    Last change :  Thu Aug 29 08:34:49 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    bignumber gmp implementation                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (extern
      ($fixnum->bignum::bignum (::long) "bgl_long_to_bignum")
      ($elong->bignum::bignum (::elong) "bgl_long_to_bignum")
      ($llong->bignum::bignum (::llong) "bgl_llong_to_bignum")
      ($uint64->bignum::bignum (::uint64) "bgl_uint64_to_bignum")
      ($int64->bignum::bignum (::uint64) "bgl_int64_to_bignum")
      ($bignum->fixnum::long (::bignum) "bgl_bignum_to_long")
      ($bignum->elong::elong (::bignum) "bgl_bignum_to_long")
      ($bignum->llong::llong (::bignum) "bgl_bignum_to_llong")
      ($bignum->int64::int64 (::bignum) "bgl_bignum_to_int64")
      ($bignum->uint64::uint64 (::bignum) "bgl_bignum_to_uint64")
      ($bignum-cmp::int (::bignum ::bignum) "bgl_bignum_cmp")
      ($absbx::bignum (::bignum) "bgl_bignum_abs")
      ($negbx::bignum (::bignum) "bgl_bignum_neg")
      ($divrembx::obj (::bignum ::bignum) "bgl_bignum_div")
      ($evenbx?::bool (::bignum) "bgl_bignum_even")
      ($oddbx?::bool (::bignum) "bgl_bignum_odd")
      ($+bx::bignum (::bignum ::bignum) "bgl_bignum_add")
      ($-bx::bignum (::bignum ::bignum) "bgl_bignum_sub")
      ($*bx::bignum (::bignum ::bignum) "bgl_bignum_mul")
      ($exptbx::bignum (::bignum ::bignum) "bgl_bignum_expt")
      ($quotientbx::bignum (::bignum ::bignum) "bgl_bignum_quotient")
      ($remainderbx::bignum (::bignum ::bignum) "bgl_bignum_remainder")
      ($gcdbx::bignum (::bignum ::bignum) "bgl_bignum_gcd")
      ($lcmbx::bignum (::bignum ::bignum) "bgl_bignum_lcm")
      ($string->bignum::bignum (::string ::int) "bgl_string_to_bignum")
      ($string->integer-obj::obj (::string ::int) "bgl_string_to_integer_obj")
      ($bignum->string::bstring  (::bignum ::int) "bgl_bignum_to_string")
      ($randbx::bignum (::bignum) "bgl_rand_bignum")
      ($seed-rand::void (::long) "bgl_seed_rand")
      
      ($bitlshbx::bignum (::bignum ::long) "bgl_bignum_lsh")
      ($bitrshbx::bignum (::bignum ::long) "bgl_bignum_rsh")
      ($bitorbx::bignum (::bignum ::bignum) "bgl_bignum_or")
      ($bitxorbx::bignum (::bignum ::bignum) "bgl_bignum_xor")
      ($bitandbx::bignum (::bignum ::bignum) "bgl_bignum_and")
      ($bitmaskbx::bignum (::bignum ::long) "bgl_bignum_mask")
      ($bitnotbx::bignum (::bignum) "bgl_bignum_not")
      
      (macro $zerobx?::bool (::bignum) "BXZERO")
      (macro $positivebx?::bool (::bignum) "BXPOSITIVE")
      (macro $negativebx?::bool (::bignum) "BXNEGATIVE")
      
      ($bignum->flonum::double (::bignum) "bgl_bignum_to_flonum")
      ($flonum->bignum::bignum (::double) "bgl_flonum_to_bignum"))
   
   (pragma
      ($fixnum->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($elong->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($llong->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($uint64->bignum side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($oddbx? side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($evenbx? side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($bignum-cmp side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($zerobx? side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($positivebx? side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($negativebx? side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($absbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($negbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($+bx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($-bx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($*bx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($exptbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($quotientbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($remainderbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($gcdbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($lcmbx side-effect-free no-cfa-top nesting args-safe (effect) fail-safe)
      ($flonum->bignum side-effect-free args-safe (effect) fail-safe)
      ($bignum->flonum side-effect-free no-cfa-top nesting (effect) fail-safe)
      ($bitlshbx side-effect-free args-safe (effect) fail-safe)))
