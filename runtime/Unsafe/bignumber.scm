;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Unsafe/bignumber.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Marc Feeley                                       */
;*    Creation    :  Tue Mar 11 11:32:17 2008                          */
;*    Last change :  Thu Aug 29 07:37:18 2024 (serrano)                */
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
      (enable-gmp
       (include "./Unsafe/bignumber-gmp.sch"))
      (else
       (include "./Unsafe/bignumber-generic.sch"))))
