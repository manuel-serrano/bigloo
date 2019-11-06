;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/expdcase.scm     */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  3 10:13:16 1992                          */
;*    Last change :  Sun Aug 25 09:14:00 2019 (serrano)                */
;*                                                                     */
;*    On macro-expanse ce satane-case                                  */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expander_case
   
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
   
   (export  (expand-eval-case <expression> <expander>)))
	   
;*---------------------------------------------------------------------*/
;*    expand-eval-case ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-eval-case x e)
   (match-case x
      ((?- ?value . ?clauses)
       (generic-case x value clauses e))
      (else
       (expand-error "case" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    generic-case ...                                                 */
;*    sexp x < datum+ x sexp+ >+ x (sexp x sexp --> sexp)              */  
;*---------------------------------------------------------------------*/
(define (generic-case x value clauses e)
   (e `(let ((case-value ,value))
	  ,(let loop ((clauses clauses))
	      (if (null? clauses)
		  #unspecified
		  (match-case (car clauses)
		     (()
		      #unspecified)
		     ((else . ?body)
		      (if (or (not (null? (cdr clauses))) (null? body))
			  (expand-error "case" "Illegal `case' form" x)
			  (expand-progn body)))
		     (((and ?datums (?- . (?- ???-))) . ?body)
		      (if (null? body)
			  (expand-error "case" "Illegal `case' form" x)
			  (evepairify
			     `(if (memv case-value ',datums)
				  ,(expand-progn body)
				  ,(loop (cdr clauses)))
			     (car clauses))))
		     (((?datums) . ?body)
		      (if (null? body)
			  (expand-error "case" "Illegal `case' form" x)
			  (evepairify
			     `(if (eqv? case-value ',datums)
				  ,(expand-progn body)
				  ,(loop (cdr clauses)))
			     (car clauses))))
		     (else
		      (expand-error "case" "Illegal `case' form" x))))))
      e))



	  
