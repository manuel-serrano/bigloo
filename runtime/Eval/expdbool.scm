;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/runtime/Eval/expdbool.scm            */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan  4 17:12:21 1993                          */
;*    Last change :  Sat Apr 24 06:48:59 2010 (serrano)                */
;*                                                                     */
;*    Les expanseurs des formes booleenes.                             */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expander_bool
   
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

   (export  (expand-cond <expression>)))

;*---------------------------------------------------------------------*/
;*    get-new-test-name ...                                            */
;*---------------------------------------------------------------------*/
(define (get-new-test-name string)
   (let ((symbol (gensym)))
      ;; the non user annotation is used for better bdb code production.
      ;; the non-user property is used by the compiler function
      ;; mark-symbol-non-user! (inside the ast_ident module).
      ;; @ref ../../comptime/Ast/ident.scm:mark-symbol-non-user!@
      (putprop! symbol 'non-user #t)
      symbol))

;*---------------------------------------------------------------------*/
;*    expand-cond ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-cond exp)
   (let* ((clauses  (cdr exp))
	  (clause1  (if (pair? clauses)
			(car clauses)
			'()))
	  (clause2+ (if (pair? clause1)
			(cdr clauses)
			#f)))
      (cond
	 ((null? clause1)
	  '#f)
	 ((or (not (pair? clause1)) (equal? clause1 '(else)))
	  (error "cond-clause" "Illegal form" exp))
	 ((null? (cdr clause1))
	  (let ((res `(or ,(car clause1) (cond ,@clause2+))))
	     (if (epair? (car clause1))
		 (econs (car res) (cdr res) (cer (car clause1)))
		 (evepairify res exp))))
	 ((and (eq? (cadr clause1) '=>) (=fx (length clause1) 3))
	  (let* ((aux (get-new-test-name "cd"))
		 (test (get-new-test-name "test"))
		 (res `(let ((,test ,(car clause1)))
			  (if ,test
			      (let ((,aux ,test))
				 (,(caddr clause1) ,aux))
			      (cond ,@clause2+)))))
	     (if (epair? (car clause1))
		 (econs (car res) (cdr res) (cer (car clause1)))
		 (evepairify res exp))))
	 ((eq? (car clause1) 'else)
	  (when (and (pair? clause2+) (> (bigloo-warning) 0))
	     (warning 'cond "ignored COND clauses -- " clause2+))
	  (expand-progn (cdr clause1)))
	 (else
	  (let* ((ncond (let ((nc `(cond ,@clause2+)))
			   (cond
			      ((and (pair? clause2+) (epair? (car clause2+)))
			       (econs (car nc) (cdr nc) (cer (car clause2+))))
			      (else
			       nc))))
		 (res `(if ,(car clause1)
			   ,(expand-progn (cdr clause1))
			   ,ncond)))
	     (cond
		((epair? (car clause1))
		 (econs (car res) (cdr res) (cer (car clause1))))
		((epair? clause1)
		 (econs (car res) (cdr res) (cer clause1)))
		((and (pair? (cdr clause1)) (epair? (cadr clause1)))
		 (econs (car res) (cdr res) (cer (cadr clause1))))
		(else
		 (evepairify res exp))))))))
