;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evaluate.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Bernard Serpette                                  */
;*    Creation    :  Fri Jul  2 10:01:28 2010                          */
;*    Last change :  Fri Jul  2 10:11:16 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    New Bigloo interpreter                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evaluate
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __bit
	    __param
	    __bexit
	    __object
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __progn
	    __expand
	    __evenv
	    __evcompile
	    __everror
	    __evmodule)

   (export  (evaluate2 sexp env loc)))

;*---------------------------------------------------------------------*/
;*    evaluate2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (evaluate2 sexp env loc)
   #f)
