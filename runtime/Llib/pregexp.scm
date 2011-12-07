;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/pregexp.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Dorai Sitaram                                     */
;*    Creation    :  Mon Jan 19 17:35:12 1998                          */
;*    Last change :  Wed Dec  7 17:58:48 2011 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Posix regular expressions                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pregexp
   
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

   (extern (macro $regexp?::bool (::obj) "BGL_REGEXPP")
           (macro $regexp-pattern::bstring (::regexp) "BGL_REGEXP_PAT"))
   
   (java   (class foreign
	      (method static $regexp?::bool (::obj)
		 "BGL_REGEXPP")
	      (method static $regexp-pattern::bstring (::obj)
		 "BGL_REGEXP_PAT")))
 
   (export (inline regexp?::bool ::obj)
	   (inline regexp-pattern::bstring ::regexp)
	   (pregexp ::bstring)
	   (pregexp-match-positions pat ::bstring . opt-args)
	   (pregexp-match pat ::bstring . opt-args)
	   (pregexp-replace::bstring pat ::bstring ins::bstring)
	   (pregexp-split::pair-nil pat ::bstring)
	   (pregexp-replace*::bstring pat ::bstring ins::bstring)
	   (pregexp-quote::bstring ::bstring))

   (cond-expand
      ((and enable-regex bigloo-c)
       (include "Llib/regex.sch"))
      ((and enable-pcre bigloo-c)
       (include "Llib/pcre.sch"))
      (else
       (include "Llib/pregexp.sch")))

   (option (set! *arithmetic-genericity* #f)
           (set! *arithmetic-overflow* #f)))

;*---------------------------------------------------------------------*/
;*    regexp? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (regexp? obj)
   ($regexp? obj))

;*---------------------------------------------------------------------*/
;*    regexp-pattern ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (regexp-pattern re)
   ($regexp-pattern re))
