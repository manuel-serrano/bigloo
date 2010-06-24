;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/runtime/Eval/expdcase.scm            */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jul  3 10:13:16 1992                          */
;*    Last change :  Tue Jun 22 05:53:33 2010 (serrano)                */
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
	    
	    __progn)
   
   (use     __type
	    __evenv)
   
   (export  (expand-eval-case <expression> <expander>)))
	   
;*---------------------------------------------------------------------*/
;*    expand-eval-case ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-eval-case x e)
   (match-case x
      ((?- ?value . ?clauses)
       (generic-case x value clauses e))
      (else
       (error "case" "Illegal form" x))))

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
			  (error "case" "Illegal `case' form" x)
			  (expand-progn body)))
		     (((and ?datums (?- . (?- ???-))) . ?body)
		      (if (null? body)
			  (error "case" "Illegal `case' form" x)
			  `(if (memv case-value ',datums)
			       ,(expand-progn body)
			       ,(loop (cdr clauses)))))
		     (((?datums) . ?body)
		      (if (null? body)
			  (error "case" "Illegal `case' form" x)
			  `(if (eqv? case-value ',datums)
			       ,(expand-progn body)
			       ,(loop (cdr clauses)))))
		     (else
		      (error "case" "Illegal `case' form" x))))))
      e))



	  
