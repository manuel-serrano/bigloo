;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/custom.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 16 17:22:43 1999                          */
;*    Last change :  Sun Aug 25 09:09:39 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Scheme custom type manipulation.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __custom
   
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
	    __r4_control_features_6_9

	    __evenv)

   (extern  (macro c-custom?::bool (::obj)
		   "CUSTOMP")
	    (macro c-custom-equal?::bool  (::custom ::custom)
		   "CUSTOM_CMP")
	    (macro c-custom-hash::int (::custom)
		   "CUSTOM_HASH_NUMBER")
	    (macro c-custom-identifier::string (::custom)
		   "CUSTOM_IDENTIFIER")
   	    (macro c-custom-identifier-set!::obj (::custom ::string)
		   "CUSTOM_IDENTIFIER_SET")
	    (c-custom-nil::custom () "bgl_custom_nil"))
   
   (java    (class foreign
	       (method static c-custom?::bool (::obj)
		       "CUSTOMP")
	       (method static c-custom-nil::custom ()
		       "bgl_custom_nil")
	       (method static c-custom-equal?::bool  (::custom ::custom)
		       "CUSTOM_CMP")
	       (method static c-custom-hash::int (::custom)
		       "CUSTOM_HASH_NUMBER")
	       (method static c-custom-identifier::string (::custom)
		       "CUSTOM_IDENTIFIER")
	       (method static c-custom-identifier-set!::obj (::custom ::string)
		       "CUSTOM_IDENTIFIER_SET")))

   (export  (inline custom?::bool ::obj)
	    (inline custom-nil::custom)
	    (inline custom-equal?::bool ::custom ::custom)
	    (inline custom-identifier::string ::custom)
	    (inline custom-identifier-set! ::custom ::string)
	    (custom-hash ::custom ::int))

   (pragma  (c-custom? nesting)
	    (c-custom-equal? nesting args-safe)
	    (c-custom-identifier nesting args-safe)
	    (c-custom-identifier-set! nesting args-safe)))

;*---------------------------------------------------------------------*/
;*    custom? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (custom? obj)
   (c-custom? obj))

;*---------------------------------------------------------------------*/
;*    custom-nil ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (custom-nil)
   (c-custom-nil))

;*---------------------------------------------------------------------*/
;*    custom-equal? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (custom-equal? obj1 obj2)
   (c-custom-equal? obj1 obj2))

;*---------------------------------------------------------------------*/
;*    custom-identifier ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (custom-identifier::string custom::custom)
   (c-custom-identifier custom))

;*---------------------------------------------------------------------*/
;*    custom-identifier-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (custom-identifier-set! custom::custom str::string)
   (c-custom-identifier-set! custom str))

;*---------------------------------------------------------------------*/
;*    custom-hash ...                                                  */
;*---------------------------------------------------------------------*/
(define (custom-hash custom::custom mod::int)
   (let ((num (c-custom-hash custom)))
      (remainderfx num mod)))
