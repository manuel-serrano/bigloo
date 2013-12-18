;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/i18n.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 18 17:02:42 2013                          */
;*    Last change :  Wed Dec 18 18:05:11 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    i18n support                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __i18n
   
   (import  __error
	    __param
	    __bexit
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __ucs2
	    __bignum
	    __bit
	    
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    
	    __evenv)

   (extern  (macro $utf8-locale-compare3::int (::bstring ::bstring)
	       "BGL_UTF8_LOCALE_COMPARE3"))

   (java    (class foreign
	       (method static $utf8-locale-compare3::int (::bstring ::bstring)
		  "bgl_utf8_locale_compare3")))
   
   (export  (inline utf8-locale-compare3::int ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    utf8-locale-compare3 ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (utf8-locale-compare3 left right)
   ($utf8-locale-compare3 left right))
