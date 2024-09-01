;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Unsafe/regexp.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Aug 28 13:00:16 2024                          */
;*    Last change :  Wed Aug 28 13:08:06 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Regular expressions                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(module __regexp
   
   (import  __error)
   
   (use     __type
            __bigloo
            __tvector
            __ucs2
            __dsssl
            __bexit
            __bignum
            __object
            __thread
	    __bit
            
            __r4_output_6_10_3
            
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
            __r4_numbers_6_5
            __r4_equivalence_6_2
            __r4_vectors_6_8
            __r4_booleans_6_1
            __r4_characters_6_6
            __r4_symbols_6_4
            __r4_pairs_and_lists_6_3
            __r4_strings_6_7
            __r4_ports_6_10_1
            __r4_control_features_6_9

            __evenv)

   (cond-expand
      (enable-pcre2 (include "./Unsafe/pcre.scm"))
      (enable-pcre (include "./Unsafe/pcre.scm"))
      (enable-regex (include "./Unsafe/regex.scm"))
      (else (include "./Unsafe/pregexp.scm"))))
