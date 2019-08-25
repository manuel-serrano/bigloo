;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Llib/semaphore.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr 20 09:27:10 2017                          */
;*    Last change :  Sun Aug 25 09:10:42 2019 (serrano)                */
;*    Copyright   :  2017-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Semaphore                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __semaphore
   
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    __bit
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2 
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __foreign
	    __evenv)
   
   (extern  (macro $semaphore?::bool (::obj) "BGL_SEMAPHOREP"))
   
   (java    (class foreign
	       (method static $semaphore?::bool (::obj)
		  "BGL_SEMAPHOREP")))
   
   (export  (inline semaphore?::bool ::obj)))

;*---------------------------------------------------------------------*/
;*    semaphore? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (semaphore? o)
   ($semaphore? o))
