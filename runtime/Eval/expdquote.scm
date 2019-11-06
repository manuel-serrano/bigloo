;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expdquote.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 09:53:05 1994                          */
;*    Last change :  Sun Aug 25 09:15:18 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    L'expansion des formes `',@                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expander_quote
   
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
	    
	    __expand)
   
   (use     __type
	    __evenv
	    __bit)
    
   (export  (expand-quote   <expr> <expander>)
	    (quasiquotation <integer> <expr>)))

;*---------------------------------------------------------------------*/
;*    expand-quote ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-quote x e)
   (match-case x
      ((?- . (?- . ()))
       x)
      ((?- ?value)
       (cond
	  ((real? value)
	   value)
	  ((fixnum? value)
	   value)
	  ((string? value)
	   value)
	  ((char? value)
	   value)
	  ((boolean? value)
	   value)
	  ((null? value)
	   x)
	  ((cnst? value)
	   value)
	  (else
	   x)))
      (else
       (expand-error "quote" "Illegal `quote' form" x))))

;*---------------------------------------------------------------------*/
;*    quasiquotation ...                                               */
;*---------------------------------------------------------------------*/
(define (quasiquotation d exp)
    (if (and (pair? exp) (pair? (cdr exp)) (null? (cddr exp)))
	(template d (cadr exp))
	(expand-error "quasiquotation" "illegal `quasiquote' form" exp)))

;*---------------------------------------------------------------------*/
;*    template ...                                                     */
;*---------------------------------------------------------------------*/
(define (template d exp)
   (cond ((=fx d 0)
	  exp)
	 ((and (pair? exp) (eq? (car exp) 'unquote))
	  (if (and (pair? (cdr exp)) (null? (cddr exp)))
	      (if (eq? d 1)
		  (template (-fx d 1) (cadr exp))
		  (list 'list ''unquote (template (-fx d 1) (cadr exp))))
	      (expand-error "unquote" "Illegal `unquote' form" exp)))
	 ((vector? exp)
	  (vector-template d exp))
	 ((pair? exp)
	  (list-template d exp))
	 ((null? exp)
	  (list 'quote exp))
	 ((or (char? exp) (fixnum? exp) (string? exp) (cnst? exp))
	  exp)
	 (else
	  (list 'quote exp))))

;*---------------------------------------------------------------------*/
;*    list-template ...                                                */
;*---------------------------------------------------------------------*/
(define (list-template d exp)
   (cond ((and (and (pair? exp) (pair? (cdr exp)) (null? (cddr exp)))
	       (eq? (car exp) 'quote) (pair? (cadr exp))
	       (eq? (car (cadr exp)) 'quasiquote))
	  (quasiquotation d (cadr exp)))
	 ((eq? (car exp) 'quasiquote)
	  (if (eq? d 0)
	      (quasiquotation (+ d 1) exp)
	      (list 'list ''quasiquote (quasiquotation (+ d 1) exp))))
	 (else
	  (if (epair? exp)
	      (let ((er (cer exp)))
		 (econs 'cons* (template-or-splice-list d exp) er))
	      (cons 'cons* (template-or-splice-list d exp))))))

;*---------------------------------------------------------------------*/
;*    vector-template ...                                              */
;*---------------------------------------------------------------------*/
(define (vector-template d exp)
   (let ((tag-val (vector-tag exp))
	 (res-val (list 'list->vector
			(cons 'cons* (template-or-splice-list
				      d
				      (vector->list exp))))))
      (if (=fx tag-val 0)
	  res-val
	  (let ((res-var (gensym)))
	     `(let ((,res-var ,res-val))
		 (vector-tag-set! ,res-var ,tag-val)
		 ,res-var)))))

;*---------------------------------------------------------------------*/
;*    template-or-splice-list ...                                      */
;*---------------------------------------------------------------------*/
(define (template-or-splice-list d exp)
    (cond ((null? exp) '('()))
	  ((pair? exp)
	   (cond ((eq? (car exp) 'unquote)
		  (list (template d exp)))
		 ((and (pair? (car exp)) (eq? (car (car exp))
					      'unquote-splicing))
		  (list (list 'eappend
			      (template-or-splice d (car exp))
			      (cons 'cons*
				    (template-or-splice-list d (cdr exp))))))
		 (else (cons (template-or-splice d (car exp))
			     (template-or-splice-list d (cdr exp))))))
	  (else (list (template-or-splice d exp)))))

;*---------------------------------------------------------------------*/
;*    template-or-splice ...                                           */
;*---------------------------------------------------------------------*/
(define (template-or-splice d exp)
    (if (and (pair? exp) (eq? (car exp) 'unquote-splicing))
	(if (and (pair? exp) (pair? (cdr exp)) (null? (cddr exp)))
	    (if (eq? d 1)
		(template (-fx d 1) (cadr exp))
		(list 'list (list 'list ''unquote-splicing
				  (template (-fx d 1) (cadr exp)))))
	    (expand-error "unquote-splicing"
	       "Illegal `unquote-splicing' form" exp))
	(template d exp)))


