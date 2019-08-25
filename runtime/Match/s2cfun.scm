;;;--------------------------------------------------------------------*/
;;;   geffroy/Match3.0/s2cfun.scm ...                                  */
;;;                                                                    */
;;;   Author      :  Jean-Marie Geffroy                                */
;;;   Creation    :  Wed Mar 10 14:48:39 1993                          */
;;;   Last change :  Mon May  3 17:50:00 1993  (geffroy)               */
;;;                                                                    */
;;;   Some non-standard utilities...                                   */
;;;--------------------------------------------------------------------*/

(module __match_s2cfun

   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __thread
	    __rgc
	    __bit
	    
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
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __evenv)
	    
  (export   (atom? e)
	    (concat . args)
	    jim-gensym
	    (andmap p . args)
	    (ormap p . args)))

;;; Some non-standard utilities
(define (atom? e)
  (not (pair? e)) )

(define (concat . args)
  (string->symbol 
   (apply string-append
	  (map (lambda (s)
		    (cond ((string? s) s)
			  ((symbol? s) (symbol->string s))
			  ((number? s) (number->string s))
			  (else (error 'concat "" args)) ) )
		  args ) ) ) )

(define jim-gensym
  (let ((counter 100))
    (lambda args
      (set! counter (+ counter 1))
      (let ((symbol (concat (if (pair? args) (car args) 'G) counter ) ) )
	 ;; mark-symbol-non-user! (inside the ast_ident module).
	 (putprop! symbol 'non-user #t)
	 symbol))))

(define (andmap p . args)
  ;; use "first-finish" rule
  (let andmap ((args args) (value #t))
    (if (let any-at-end? ((ls args))
          (and (pair? ls)
               (or (not (pair? (car ls)))
                   (any-at-end? (cdr ls)))))
        value
        (let ((value (apply p (map car args))))
          (and value (andmap (map cdr args) value))))))

; ORMAP
(define (ormap p . args)
  ;; use "first-finish" rule
  (if (= (length args) 1)
      (member #t (map p (car args)))
      (let ormap ((args args) (value #f))
	(if (let any-at-end? ((ls args))
	      (and (pair? ls)
		   (or (not (pair? (car ls)))
		       (any-at-end? (cdr ls)))))
	    value
	    (let ((value (apply p (map car args))))
	      (or value (ormap (map cdr args) value)))))))

