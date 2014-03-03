;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/ucs2.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 18 10:24:03 1997                          */
;*    Last change :  Mon Mar  3 08:53:57 2014 (serrano)                */
;*    -------------------------------------------------------------    */
;*    UCS-2 Characters Scheme management.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __ucs2
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    
	    __r4_equivalence_6_2
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3

	    __evenv)
  
   (extern  (macro c-ucs2?::bool (::obj)  "UCS2P")
	    (c-ucs2-letter?::bool (::ucs2) "ucs2_letterp")
	    (c-ucs2-digit?::bool (::ucs2) "ucs2_digitp")
	    (c-ucs2-whitespace?::bool (::ucs2) "ucs2_whitespacep")
	    (c-ucs2-upperp::bool (::ucs2) "ucs2_upperp")
	    (c-ucs2-lowerp::bool (::ucs2) "ucs2_lowerp")
	    (c-ucs2-upcase::ucs2 (::ucs2) "ucs2_toupper")
	    (c-ucs2-downcase::ucs2 (::ucs2) "ucs2_tolower")
	    (c-ucs2-defined?::bool (::int) "ucs2_definedp")
	    (macro c-integer->ucs2::ucs2 (::int) "BGL_INT_TO_UCS2")
	    (macro c-ucs2->integer::int (::bucs2) "CUCS2")
	    (infix macro c-ucs2=?::bool (::ucs2 ::ucs2) "==")
	    (infix macro c-ucs2<?::bool (::ucs2 ::ucs2) "<")
	    (infix macro c-ucs2>?::bool (::ucs2 ::ucs2) ">")
	    (infix macro c-ucs2<=?::bool (::ucs2 ::ucs2) "<=")
	    (infix macro c-ucs2>=?::bool (::ucs2 ::ucs2) ">="))
   
   (java    (class foreign
	       (method static c-ucs2?::bool (::obj)
		       "UCS2P")
	       (method static c-ucs2-letter?::bool (::ucs2)
		       "ucs2_letterp")
	       (method static c-ucs2-digit?::bool (::ucs2)
		       "ucs2_digitp")
	       (method static c-ucs2-whitespace?::bool (::ucs2)
		       "ucs2_whitespacep")
	       (method static c-ucs2-upperp::bool (::ucs2)
		       "ucs2_upperp")
	       (method static c-ucs2-lowerp::bool (::ucs2)
		       "ucs2_lowerp")
	       (method static c-ucs2-upcase::ucs2 (::ucs2)
		       "ucs2_toupper")
	       (method static c-ucs2-downcase::ucs2 (::ucs2)
		       "ucs2_tolower")
	       (method static c-ucs2-defined?::bool (::int)
		       "ucs2_definedp")
	       (method static c-integer->ucs2::ucs2 (::int)
		       "BGL_INT_TO_UCS2")
	       (method static c-ucs2->integer::int (::ucs2)
		       "CUCS2")
	       (method static c-ucs2=?::bool (::ucs2 ::ucs2)
		       "UCS2_EQ")
	       (method static c-ucs2<?::bool (::ucs2 ::ucs2)
		       "UCS2_LT")
	       (method static c-ucs2>?::bool (::ucs2 ::ucs2)
		       "UCS2_GT")
	       (method static c-ucs2<=?::bool (::ucs2 ::ucs2)
		       "UCS2_LE")
	       (method static c-ucs2>=?::bool (::ucs2 ::ucs2)
		       "UCS2_GE")))
   
   (export  (inline ucs2?::bool ::obj)
	    (inline ucs2=?::bool ::ucs2 ::ucs2)
	    (inline ucs2<?::bool ::ucs2 ::ucs2)
	    (inline ucs2>?::bool ::ucs2 ::ucs2)
	    (inline ucs2<=?::bool ::ucs2 ::ucs2)
	    (inline ucs2>=?::bool ::ucs2 ::ucs2) 
	    (inline ucs2-ci=?::bool ::ucs2 ::ucs2)
	    (inline ucs2-ci<?::bool ::ucs2 ::ucs2)
	    (inline ucs2-ci>?::bool ::ucs2 ::ucs2)
	    (inline ucs2-ci<=?::bool ::ucs2 ::ucs2)
	    (inline ucs2-ci>=?::bool ::ucs2 ::ucs2)
	    (inline ucs2-alphabetic?::bool ::ucs2)
	    (inline ucs2-numeric?::bool ::ucs2)
	    (inline ucs2-whitespace?::bool ::ucs2)
	    (inline ucs2-upper-case?::bool ::ucs2)
	    (inline ucs2-lower-case?::bool ::ucs2)
	    (inline ucs2->integer::int ::ucs2)
	    (integer->ucs2::ucs2 ::int)
	    (inline integer->ucs2-ur::ucs2 ::int)
	    (ucs2->char::char ::ucs2)
	    (inline char->ucs2::ucs2 ::char)
	    (inline ucs2-upcase::ucs2 ::ucs2)
	    (inline ucs2-downcase::ucs2 ::ucs2))
   
   (pragma  (c-ucs2? (predicate-of bucs2) side-effect-free no-cfa-top nesting)
	    (ucs2? side-effect-free no-cfa-top nesting)
	    (c-ucs2-upcase side-effect-free nesting args-safe)
	    (c-ucs2-downcase side-effect-free nesting args-safe)
	    (c-ucs2=? side-effect-free nesting args-safe)
	    (c-ucs2<? side-effect-free nesting args-safe)
	    (c-ucs2>? side-effect-free nesting args-safe)
	    (c-ucs2<=? side-effect-free nesting args-safe)
	    (c-ucs2>=? side-effect-free nesting args-safe)
	    (ucs2=? side-effect-free nesting)
	    (ucs2<? side-effect-free nesting)
	    (ucs2>? side-effect-free nesting)
	    (ucs2<=? side-effect-free nesting)
	    (ucs2>=? side-effect-free nesting)
	    (ucs2-ci=? side-effect-free nesting)
	    (ucs2-ci<? side-effect-free nesting)
	    (ucs2-ci>? side-effect-free nesting)
	    (ucs2-ci<=? side-effect-free nesting)
	    (ucs2-ci>=? side-effect-free nesting)
	    (ucs2-alphabetic? side-effect-free nesting)
	    (ucs2-numeric? side-effect-free nesting)
	    (ucs2-whitespace? side-effect-free nesting)
	    (ucs2-upper-case? side-effect-free nesting)
	    (ucs2-lower-case? side-effect-free nesting)
	    (ucs2->integer side-effect-free nesting)
	    (integer->ucs2 side-effect-free nesting)
	    (integer->ucs2-ur side-effect-free nesting)
	    (c-integer->ucs2 side-effect-free nesting)
	    (ucs2->char side-effect-free nesting)
	    (char->ucs2 side-effect-free nesting)
	    (ucs2-upcase side-effect-free nesting)
	    (ucs2-downcase side-effect-free nesting)))
 
;*---------------------------------------------------------------------*/
;*    ucs2? ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (ucs2? obj)
   (c-ucs2? obj))

;*---------------------------------------------------------------------*/
;*    ucs2=? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (ucs2=? ucs2a ucs2b)
   (c-ucs2=? ucs2a ucs2b))

;*---------------------------------------------------------------------*/
;*    ucs2<? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline (ucs2<? ucs2a ucs2b)
   (c-ucs2<? ucs2a ucs2b))

;*---------------------------------------------------------------------*/
;*    ucs2>? ...                                                       */
;*---------------------------------------------------------------------*/
(define-inline  (ucs2>? ucs2a ucs2b)
   (c-ucs2>? ucs2a ucs2b))

;*---------------------------------------------------------------------*/
;*    ucs2<=? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (ucs2<=? ucs2a ucs2b)
   (c-ucs2<=? ucs2a ucs2b))

;*---------------------------------------------------------------------*/
;*    ucs2>=? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline  (ucs2>=? ucs2a ucs2b)
   (c-ucs2>=? ucs2a ucs2b))

;*---------------------------------------------------------------------*/
;*    ucs2-ci=? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-ci=? ucs2a ucs2b)
   (ucs2=? (ucs2-upcase ucs2a) (ucs2-upcase ucs2b)))

;*---------------------------------------------------------------------*/
;*    ucs2-ci<? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-ci<? ucs2a ucs2b)
   (ucs2<? (ucs2-upcase ucs2a) (ucs2-upcase ucs2b))) 

;*---------------------------------------------------------------------*/
;*    ucs2-ci>? ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline  (ucs2-ci>? ucs2a ucs2b)
   (ucs2>? (ucs2-upcase ucs2a) (ucs2-upcase ucs2b)))

;*---------------------------------------------------------------------*/
;*    ucs2-ci<=? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-ci<=? ucs2a ucs2b)
   (ucs2<=? (ucs2-upcase ucs2a) (ucs2-upcase ucs2b)))

;*---------------------------------------------------------------------*/
;*    ucs2-ci>=? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-ci>=? ucs2a ucs2b)
   (ucs2>=? (ucs2-upcase ucs2a) (ucs2-upcase ucs2b)))

;*---------------------------------------------------------------------*/
;*    ucs2-alphabetic? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-alphabetic? ucs2)
   (c-ucs2-letter? ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-numeric? ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-numeric? ucs2)
   (c-ucs2-digit? ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-withespace? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-whitespace? ucs2)
   (c-ucs2-whitespace? ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-upper-case? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-upper-case? ucs2)
   (c-ucs2-upperp ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-lower-case? ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-lower-case? ucs2)
   (c-ucs2-lowerp ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-upcase ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-upcase ucs2)
   (c-ucs2-upcase ucs2))

;*---------------------------------------------------------------------*/
;*    ucs2-downcase ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2-downcase ucs2)
   (c-ucs2-downcase ucs2))
		     
;*---------------------------------------------------------------------*/
;*    integer->ucs2 ...                                                */
;*---------------------------------------------------------------------*/
(define (integer->ucs2::ucs2 int::int)
   (if (and (>=fx int 0) (<fx int 65536))
       (if (c-ucs2-defined? int)
	   (c-integer->ucs2 int)
	   (error 'integer->ucs2 "Undefined UCS-2 character" int))
       (error 'integer->ucs2 "integer out of range or " int)))

;*---------------------------------------------------------------------*/
;*    integer->ucs2-ur ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (integer->ucs2-ur::ucs2 int::int)
   (c-integer->ucs2 int))

;*---------------------------------------------------------------------*/
;*    ucs2->integer ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (ucs2->integer::int ucs2::ucs2)
   (c-ucs2->integer ucs2))

;*---------------------------------------------------------------------*/
;*    char->ucs2 ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (char->ucs2::ucs2 char::char)
   (integer->ucs2-ur (char->integer char)))

;*---------------------------------------------------------------------*/
;*    ucs2->char ...                                                   */
;*---------------------------------------------------------------------*/
(define (ucs2->char::char ucs2::ucs2)
   (let ((int (ucs2->integer ucs2)))
      (if (<fx int 256)
	  (integer->char int)
	  (error 'ucs2->char
		 "UCS-2 character out of ISO-LATIN-1 range"
		 ucs2))))

 
