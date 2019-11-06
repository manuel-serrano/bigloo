;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/expdtry.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel SERRANO                                    */
;*    Creation    :  Tue Sep  1 16:21:59 1992                          */
;*    Last change :  Sun Aug 25 09:14:23 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Try form expansion                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __expander_try
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __param
	    __object
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __progn
	    __expand)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (expand-try <expression> <expander>)))

;*---------------------------------------------------------------------*/
;*    expand-try ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-try x e)
   (match-case x
      ((?- (and ?body (not ())) ?handler)
       (let ((n (e `(&try (lambda () ,body) ,handler) e)))
	  (evepairify n x)))
      (else
       (expand-error "try" "Illegal form" x))))
