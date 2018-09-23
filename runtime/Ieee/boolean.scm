;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/boolean.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 08:34:45 1995                          */
;*    Last change :  Sun Sep 23 17:33:52 2018 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.1. Booleans (page 13, r4)                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_booleans_6_1
   
   (use     __type
	    __param
	    __error
	    __bigloo
	    __tvector
	    __bignum
	    __r4_equivalence_6_2
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_vectors_6_8
	    __r4_pairs_and_lists_6_3
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    
	    __evenv)
   
   (extern  (macro c-boolean?::bool (::obj) "BOOLEANP")
	    (macro btrue::bbool "BTRUE")
	    (macro bfalse::bbool "BFALSE"))
   
   (java    (class foreign
	       (method static c-boolean?::bool (::obj) "BOOLEANP")
	       (field static btrue::bbool "BTRUE")
	       (field static bfalse::bbool "BFALSE")))
    
   (export  (inline not::bool ::obj)
	    (inline boolean?::bool ::obj))
   
   (pragma  (not side-effect-free nesting)
	    (c-boolean? (predicate-of bbool) no-cfa-top nesting)
	    (boolean? no-cfa-top side-effect-free nesting)))

;*---------------------------------------------------------------------*/
;*    not ...                                                          */
;*---------------------------------------------------------------------*/
(define-inline (not obj)
   (if obj #f #t))

;*---------------------------------------------------------------------*/
;*    boolean? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (boolean? obj)
   (c-boolean? obj))
   
