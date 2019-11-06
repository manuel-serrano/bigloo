;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/weakptr.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Stephane Epardaud                                 */
;*    Creation    :  Wed Dec 13 15:32:17 CET 2006                      */
;*    Last change :  Sun Aug 25 09:09:06 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Weak pointers.                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __weakptr

   (import  __error
	    __hash
	    __reader
	    __param
	    __bexit
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __structure
	    __bignum
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_vectors_6_8
	    __r4_pairs_and_lists_6_3
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_output_6_10_3
	    __r4_ports_6_10_1
	    __r4_control_features_6_9
	    __evenv
	    __bit)

   (extern (macro c-weakptr?::bool (::obj)
		  "BGL_WEAKPTRP")
	   (c-weakptr-data::obj (::weakptr)
		  "weakptr_data")
	   (c-weakptr-data-set!::void (::weakptr ::obj)
		  "weakptr_data_set")
	   (c-make-weakptr::weakptr (::obj)
		  "make_weakptr"))
	   
   (java   (class foreign
            (method static c-weakptr?::bool (::obj)
                    "BGL_WEAKPTRP")
            (method static c-weakptr-data::obj (::weakptr)
                    "weakptr_data")
            (method static c-weakptr-data-set!::void (::weakptr ::obj)
                    "weakptr_data_set")
            (method static c-make-weakptr::weakptr (::obj)
                    "make_weakptr")))

   (export  (inline weakptr?::bool ::obj)
	    (inline weakptr-data::obj ::weakptr)
	    (inline weakptr-data-set! ::weakptr ::obj)
	    (inline make-weakptr::weakptr ::obj))

   (pragma  (c-weakptr? (predicate-of weakptr) no-cfa-top nesting)
	    (weakptr? side-effect-free no-cfa-top nesting)))

;*---------------------------------------------------------------------*/
;*    weakptr? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (weakptr? obj)
   (c-weakptr? obj))

;*---------------------------------------------------------------------*/
;*    weakptr-data ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (weakptr-data obj)
   (c-weakptr-data obj))

;*---------------------------------------------------------------------*/
;*    weakptr-data-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (weakptr-data-set! ptr obj)
   (c-weakptr-data-set! ptr obj)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    make-weakptr ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (make-weakptr obj)
   (c-make-weakptr obj))

