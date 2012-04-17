;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/everror.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 14 13:46:57 2004                          */
;*    Last change :  Tue Apr 17 07:46:25 2012 (serrano)                */
;*    Copyright   :  2004-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The error of evmeaning                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __everror
   
   (import  __type
 	    __object
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
	    __r4_output_6_10_3)
   
   (export (everror ::obj ::obj ::obj ::obj)
	   (evtype-error ::obj ::obj ::obj ::obj)
	   (evarity-error ::obj ::obj ::int ::int)
	   (evwarning ::obj . ::obj)))

;*---------------------------------------------------------------------*/
;*    everror ...                                                      */
;*---------------------------------------------------------------------*/
(define (everror loc proc mes obj)
   (match-case loc
      ((at ?fname ?loc)
       (error/location proc mes obj fname loc))
      (else
       (error proc mes obj))))

;*---------------------------------------------------------------------*/
;*    evtype-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (evtype-error loc proc mes obj)
   (match-case loc
      ((at ?fname ?loc)
       (bigloo-type-error/location proc mes obj fname loc))
      (else
       (bigloo-type-error proc mes obj))))
   
;*---------------------------------------------------------------------*/
;*    evarity-error ...                                                */
;*---------------------------------------------------------------------*/
(define (evarity-error loc name provide expect)
   (let ((msg (format "Wrong number of arguments: ~a expected, ~a provided "
		      expect provide)))
      (everror loc "eval" msg name)))

;*---------------------------------------------------------------------*/
;*    evwarning ...                                                    */
;*---------------------------------------------------------------------*/
(define (evwarning loc . args)
   (match-case loc
      ((at ?fname ?loc)
       (warning-notify
	  (instantiate::&eval-warning
	     (fname fname)
	     (location loc)
	     (stack (get-trace-stack))
	     (args args))))
      (else
       (warning-notify
	  (instantiate::&eval-warning
	     (fname #f)
	     (location #f)
	     (stack (get-trace-stack))
	     (args args))))))

