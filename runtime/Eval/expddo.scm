;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/expddo.scm       */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Sep 28 16:06:31 1992                          */
;*    Last change :  Sun Aug 25 09:14:12 2019 (serrano)                */
;*                                                                     */
;*    La macro `DO'                                                    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expander_do
   
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

	    __expand
	    __progn)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (expand-do <expression> <expander>)))


;*---------------------------------------------------------------------*/
;*    gen-doloop-name ...                                              */
;*---------------------------------------------------------------------*/
(define (gen-doloop-name)
   (gensym "do-loop--"))

;*---------------------------------------------------------------------*/
;*    expand-do ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-do exp e)
   (match-case exp
      ((?- ?bindings ?end . ?command)
       (let* ((let-bindings  bindings)
	      (vars '())
	      (inits '())
	      (steps '())
 	      (loop (gen-doloop-name))
	      (test (if (pair? end)
			(car end)
			(expand-error "do" "Illegal form" exp)))
	      (ending (if (null? (cdr end))
			  (list #f)
			  (cdr end)))
	      (body command))
	  (for-each
	   (lambda (var-init-step)
	      (if (and (>=fx (length var-init-step) 2)
		       (<=fx (length var-init-step) 3))
		  (let* ((var (car var-init-step))
			 (init (cadr var-init-step))
			 (step (if (not (null? (cddr var-init-step)))
				   (car (cddr var-init-step))
				   var)))
		     (set! vars  (cons var vars))
		     (set! steps (cons step steps))
		     (set! inits (cons init inits)))
		  (expand-error "do" "Illegal form:" var-init-step)))
	   (reverse let-bindings))
	  (e (evepairify-deep
		`(letrec ((,loop (lambda ,vars
				    (if ,test
					(begin ,@ending)
					(begin ,@body (,loop ,@steps))))))
		    (,loop ,@inits))
		exp)
	     e)))
      (else
       (expand-error "do" "Illegal form" exp))))
