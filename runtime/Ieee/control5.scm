;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Ieee/control5.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Feb 27 14:11:26 1998                          */
;*    Last change :  Tue Mar 11 15:55:59 2008 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An implementation of the R5RS multiple values.                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r5_control_features_6_4
    
   (import  __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9

	    __evenv)

   (extern  (macro $get-mvalues-number::int ()
		   "BGL_MVALUES_NUMBER")
	    (macro $set-mvalues-number!::int (::int)
		   "BGL_MVALUES_NUMBER_SET")
	    
	    (macro $get-mvalues-val::obj (::int)
		   "BGL_MVALUES_VAL")
	    (macro $set-mvalues-val!::obj (::int ::obj)
		   "BGL_MVALUES_VAL_SET"))
	    
   (java    (class foreign
	       (method static $get-mvalues-number::int ()
		       "BGL_MVALUES_NUMBER")
	       (method static  $set-mvalues-number!::int (::int)
		       "BGL_MVALUES_NUMBER_SET")
	       (method static $get-mvalues-val::obj (::int)
		       "BGL_MVALUES_VAL")
	       (method static $set-mvalues-val!::obj (::int ::obj)
		       "BGL_MVALUES_VAL_SET")))
	    
   (export  (values . args)
	    (call-with-values ::procedure ::procedure)

	    (inline %get-mvalues-number::int)
	    (inline %set-mvalues-number!::int ::int)
	    
	    (inline %get-mvalues-val::obj ::int)
	    (inline %set-mvalues-val!::obj ::int ::obj)))

;*---------------------------------------------------------------------*/
;*    %get-mvalues-number ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (%get-mvalues-number)
   ($get-mvalues-number))

;*---------------------------------------------------------------------*/
;*    %set-mvalues-number ...                                          */
;*---------------------------------------------------------------------*/
(define-inline (%set-mvalues-number! n)
   ($set-mvalues-number! n))

;*---------------------------------------------------------------------*/
;*    %get-mvalues-val ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (%get-mvalues-val n)
   ($get-mvalues-val n))

;*---------------------------------------------------------------------*/
;*    %set-mvalues-val ...                                             */
;*---------------------------------------------------------------------*/
(define-inline (%set-mvalues-val! n o)
   ($set-mvalues-val! n o))

;*---------------------------------------------------------------------*/
;*    values ...                                                       */
;*    -------------------------------------------------------------    */
;*    Values with exactly one argument are not boxed.                  */
;*---------------------------------------------------------------------*/
(define (values . args)
   (if (null? args)
       (%set-mvalues-number! 0)
       (if (null? (cdr args))
	   (begin
	      (%set-mvalues-number! 1)
	      (car args))
	   (let ((res0 (car args))
		 (all-args args))
	      (let loop ((i 1)
			 (args (cdr args)))
		 (cond
		    ((null? args)
		     (%set-mvalues-number! i)
		     res0)
		    ((=fx i 16)
		     (%set-mvalues-number! -1)
		     all-args)
		    (else
		     (%set-mvalues-val! i (car args))
		     (loop (+fx i 1) (cdr args)))))))))
	      
;*---------------------------------------------------------------------*/
;*    call-with-values ...                                             */
;*---------------------------------------------------------------------*/
(define (call-with-values producer consumer)
   (%set-mvalues-number! 1)
   (let ((res0 (producer)))
      (case (%get-mvalues-number)
	 ((-1)
	  (apply consumer res0))
	 ((0)
	  (consumer))
	 ((1)
	  (consumer res0))
	 ((2)
	  (consumer res0
		    (%get-mvalues-val 1)))
	 ((3)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)))
	 ((4)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)))
	 ((5)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)))
	 ((6)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)))
	 ((7)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)))
	 ((8)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)))
	 ((9)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)))
	 ((10)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)))
	 ((11)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)
		    (%get-mvalues-val 10)))
	 ((12)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)
		    (%get-mvalues-val 10)
		    (%get-mvalues-val 11)))
	 ((13)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)
		    (%get-mvalues-val 10)
		    (%get-mvalues-val 11)
		    (%get-mvalues-val 12)))
	 ((14)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)
		    (%get-mvalues-val 10)
		    (%get-mvalues-val 11)
		    (%get-mvalues-val 12)
		    (%get-mvalues-val 13)))
	 ((15)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)
		    (%get-mvalues-val 10)
		    (%get-mvalues-val 11)
		    (%get-mvalues-val 12)
		    (%get-mvalues-val 13)
		    (%get-mvalues-val 14)))
	 ((16)
	  (consumer res0
		    (%get-mvalues-val 1)
		    (%get-mvalues-val 2)
		    (%get-mvalues-val 3)
		    (%get-mvalues-val 4)
		    (%get-mvalues-val 5)
		    (%get-mvalues-val 6)
		    (%get-mvalues-val 7)
		    (%get-mvalues-val 8)
		    (%get-mvalues-val 9)
		    (%get-mvalues-val 10)
		    (%get-mvalues-val 11)
		    (%get-mvalues-val 12)
		    (%get-mvalues-val 13)
		    (%get-mvalues-val 14)
		    (%get-mvalues-val 15)))
	 (else
	  (apply consumer res0)))))


