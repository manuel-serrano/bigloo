;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/i18n.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 18 17:02:42 2013                          */
;*    Last change :  Tue Dec 24 13:51:10 2013 (serrano)                */
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

   (extern  (macro $utf8-string-locale-compare3::int (::bstring ::bstring)
		   "BGL_UTF8_STRING_LOCALE_COMPARE3")
	    ($utf8-string-locale-upcase::bstring (::bstring)
	       "bgl_utf8_string_locale_upcase")
	    ($utf8-string-locale-downcase::bstring (::bstring)
	       "bgl_utf8_string_locale_downcase")
	    ($utf8-string-locale-capitalize::bstring (::bstring)
	       "bgl_utf8_string_locale_capitalize"))

   (java    (class foreign
	       (method static $utf8-string-locale-compare3::int
		  (::bstring ::bstring)
		  "bgl_utf8_string_locale_compare3")
	       (method static $utf8-string-locale-upcase::bstring
		  (::bstring)
		  "bgl_utf8_string_locale_upcase")
	       (method static $utf8-string-locale-downcase::bstring
		  (::bstring)
		  "bgl_utf8_string_locale-downcase")
	       (method static $utf8-string-locale-capitalize::bstring
		  (::bstring)
		  "bgl_utf8_string_locale_capitalize")))
   
   (export  (inline utf8-string-locale-compare3::int ::bstring ::bstring)
	    (inline utf8-string-locale-upcase::bstring ::bstring)
	    (inline utf8-string-locale-downcase::bstring ::bstring)
	    (inline utf8-string-locale-capitalize::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    utf8-string-locale-compare3 ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string-locale-compare3 left right)
   ($utf8-string-locale-compare3 left right))

;*---------------------------------------------------------------------*/
;*    utf8-string-locale-upcase ...                                    */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string-locale-upcase str)
   (cond-expand
      (bigloo-jvm (string-upcase str))
      (else ($utf8-string-locale-upcase str))))

;*---------------------------------------------------------------------*/
;*    utf8-string-locale-downcase ...                                  */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string-locale-downcase str)
   (cond-expand
      (bigloo-jvm (string-downcase str))
      (else ($utf8-string-locale-downcase str))))

;*---------------------------------------------------------------------*/
;*    utf8-string-locale-capitalize ...                                */
;*---------------------------------------------------------------------*/
(define-inline (utf8-string-locale-capitalize str)
   (cond-expand
      (bigloo-jvm (string-capitalize str))
      (else ($utf8-string-locale-capitalize str))))
