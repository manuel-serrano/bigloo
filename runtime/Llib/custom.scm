;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/runtime/Llib/custom.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Aug 16 17:22:43 1999                          */
;*    Last change :  Thu Oct 22 09:51:06 2026 (serrano)                */
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

   (extern  (macro $custom?::bool (::obj)
		   "CUSTOMP")
	    (macro $custom-equal?::bool  (::custom ::custom)
		   "CUSTOM_CMP")
	    (macro $custom-hash::int (::custom)
		   "CUSTOM_HASH_NUMBER")
	    (macro $custom-identifier::bytes (::custom)
		   "CUSTOM_IDENTIFIER")
   	    (macro $custom-identifier-set!::obj (::custom ::bytes)
		   "CUSTOM_IDENTIFIER_SET")
	    ($custom-nil::custom () "bgl_custom_nil"))

   (wasm    ($custom? "(ref.test (ref $custom) ~0)")
            ($custom-equal? "(ref.eq ~0 ~1)")
            ($custom-hash "(i32.const 0)") ;; TODO: implement hashing for custom type
            ($custom-identifier "(struct.get $custom $ident ~0)")
            ($custom-nil "(struct.new $custom (global.get $bstring-default-value))"))
   
   (java    (class foreign
	       (method static $custom?::bool (::obj)
		       "CUSTOMP")
	       (method static $custom-nil::custom ()
		       "bgl_custom_nil")
	       (method static $custom-equal?::bool  (::custom ::custom)
		       "CUSTOM_CMP")
	       (method static $custom-hash::int (::custom)
		       "CUSTOM_HASH_NUMBER")
	       (method static $custom-identifier::bytes (::custom)
		       "CUSTOM_IDENTIFIER")
	       (method static $custom-identifier-set!::obj (::custom ::bytes)
		       "CUSTOM_IDENTIFIER_SET")))

   (export  (inline custom?::bool ::obj)
	    (inline custom-nil::custom)
	    (inline custom-equal?::bool ::custom ::custom)
	    (inline custom-identifier::bytes ::custom)
	    (inline custom-identifier-set! ::custom ::bytes)
	    (custom-hash ::custom ::int))

   (pragma  ($custom? nesting)
	    ($custom-equal? nesting args-safe)
	    ($custom-identifier nesting args-safe)
	    ($custom-identifier-set! nesting args-safe)))

;*---------------------------------------------------------------------*/
;*    custom? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (custom? obj)
   ($custom? obj))

;*---------------------------------------------------------------------*/
;*    custom-nil ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (custom-nil)
   ($custom-nil))

;*---------------------------------------------------------------------*/
;*    custom-equal? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (custom-equal? obj1 obj2)
   ($custom-equal? obj1 obj2))

;*---------------------------------------------------------------------*/
;*    custom-identifier ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (custom-identifier::bytes custom::custom)
   ($custom-identifier custom))

;*---------------------------------------------------------------------*/
;*    custom-identifier-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (custom-identifier-set! custom::custom str::bytes)
   ($custom-identifier-set! custom str))

;*---------------------------------------------------------------------*/
;*    custom-hash ...                                                  */
;*---------------------------------------------------------------------*/
(define (custom-hash custom::custom mod::int)
   (let ((num ($custom-hash custom)))
      (remainderfx num mod)))
