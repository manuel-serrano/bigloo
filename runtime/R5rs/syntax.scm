;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/R5rs/syntax.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 17:24:01 2002                          */
;*    Last change :  Tue Mar 11 16:00:06 2008 (serrano)                */
;*    Copyright   :  2002-08 Dorai Sitaram, Manuel Serrano             */
;*    -------------------------------------------------------------    */
;*    The implementation of R5Rs macros.                               */
;*    To a large extend, this code has been copied from Dorai Sitaram  */
;*    code mbe.lsp. The original source code may be found at:          */
;*    http://www.cs.rice.edu/~dorai                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r5_macro_4_3_syntax

   (import  __r5_macro_4_3_hygiene
	    __error)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bit
	    __bignum
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_strings_6_7
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r5_control_features_6_4
	    
	    __evenv)

   (export  (expand-define-syntax ::obj ::procedure)
	    (expand-letrec-syntax ::obj ::procedure)
	    (expand-let-syntax ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    map2 ...                                                         */
;*    -------------------------------------------------------------    */
;*    A version of MAP that checks if its argument is a proper list    */
;*---------------------------------------------------------------------*/
(define (map2 f l0)
   (let loop1 ((l l0))
      (cond
	 ((null? l)
	  '())
	 ((not (pair? l))
	  (error "syntax-rules" "Illegal list" l0))
	 (else
	  (cons (f (car l)) (loop1 (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    syntax-rules-proc ...                                            */
;*---------------------------------------------------------------------*/
(define (syntax-rules-proc macro-name kk cc arg-sym kk-sym)
   (define (syntax-rule-proc c)
      (match-case c
	 ((?in-pat ?out-pat)
	  `((r5rs-macro-matches-pattern? ',in-pat ,arg-sym ,kk-sym)
	    (multiple-value-bind (tout-pat alist)
	       ;; tag
	       (r5rs-hygiene-tag ',out-pat
				 (append (r5rs-hygiene-flatten ',in-pat)
					 ,kk-sym) 
				 '())
	       ;; untag
	       (r5rs-hygiene-untag
		(r5rs-macro-expand-pattern
		 tout-pat
		 (r5rs-macro-get-bindings ',in-pat ,arg-sym ,kk-sym)
		 ,kk-sym)
		alist
		'()))))
	 (else
	  (error "syntax-rules" "Illegal clause" c))))
   (let ((kk (cons macro-name kk)))
      `(let ((,arg-sym (cons ',macro-name ,arg-sym))
	     (,kk-sym ',kk))
	  (cond ,@(map2 syntax-rule-proc cc)
		(else
		 (error "syntax-rules" "No matching clause" ',macro-name))))))

;*---------------------------------------------------------------------*/
;*    expand-define-syntax ...                                         */
;*---------------------------------------------------------------------*/
(define (expand-define-syntax x e)
   (match-case x
      ((?- (and (? symbol?) ?macroname) (syntax-rules ?ks . ?clauses))
       (let ((rules (gensym)))
	  (e `(define-macro (,macroname . ,rules)
		 ,(syntax-rules-proc macroname ks clauses
				     rules
				     (gensym)))
	     e)))
      (else
       (error "define-syntax" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-letrec-syntax ...                                         */
;*---------------------------------------------------------------------*/
(define (expand-letrec-syntax x e)
   (match-case x
      ((?- ?bindings . ?body)
       (let ((e1 (let loop ((bindings bindings))
		    (if (null? bindings)
			e
			(match-case (car bindings)
			   (((and (? symbol?) ?m) (syntax-rules ?ks . ?clauses))
			    (lambda (x e2)
			       (if (and (pair? x) (eq? (car x) m))
				   (let loop ((clauses clauses))
				      (if (null? clauses)
					  (error "letrec-syntax" "No matching clause" m)
					  (let ((c (car clauses)))
					     (match-case c
						((?in-pat ?out-pat)
						 (if (r5rs-macro-matches-pattern? in-pat x ks)
						     (multiple-value-bind (tout-pat alist)
							(r5rs-hygiene-tag out-pat
									  (append in-pat ks)
									  '())
							(let ((r (r5rs-macro-get-bindings in-pat x ks)))
							   (e2 (r5rs-hygiene-untag (r5rs-macro-expand-pattern tout-pat r ks)
										   alist
										   '())
							       e2)))
						     (loop (cdr clauses))))
						(else
						 (error "letrec-syntax" "Illegal clause" c))))))
				   (let ((e3 (loop (cdr bindings))))
				      (e3 x e2)))))
			   (else
			    (error "letrec-syntax" "Illegal bindings" bindings)))))))
	  (e1 `(begin ,@body) e1)))
      (else
       (error "letrec-syntax" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-let-syntax ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-let-syntax x e)
   (match-case x
      ((?- ?bindings . ?body)
       (let ((e1 (let loop ((bindings bindings))
		    (if (null? bindings)
			e
			(match-case (car bindings)
			   (((and (? symbol?) ?m) (syntax-rules ?ks . ?clauses))
			    (lambda (x e2)
			       (if (and (pair? x) (eq? (car x) m))
				   (let loop ((clauses clauses))
				      (if (null? clauses)
					  (error "let-syntax" "No matching clause" m)
					  (let ((c (car clauses)))
					     (match-case c
						((?in-pat ?out-pat)
						 (if (r5rs-macro-matches-pattern? in-pat x ks)
						     (multiple-value-bind (tout-pat alist)
							(r5rs-hygiene-tag out-pat
									  (append in-pat ks)
									  '())
							(let ((r (r5rs-macro-get-bindings in-pat x ks)))
							   (e (r5rs-hygiene-untag (r5rs-macro-expand-pattern tout-pat r ks)
										  alist
										  '())
							      e)))
						     (loop (cdr clauses))))
						(else
						 (error "let-syntax" "Illegal clause" c))))))
				   (let ((e3 (loop (cdr bindings))))
				      (e3 x e2)))))
			   (else
			    (error "let-syntax" "Illegal bindings" bindings)))))))
	  (e1 `(begin ,@body) e1)))
      (else
       (error "let-syntax" "Illegal form" x))))
