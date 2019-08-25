;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/expdlet.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan  4 17:10:13 1993                          */
;*    Last change :  Sun Aug 25 09:13:25 2019 (serrano)                */
;*    Copyright   :  2004-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Let forms expansion                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __expander_let
   
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

	    __expander_define
	    __evcompile
	    __expand
	    __evutils)
   
   (use     __type
	    __evenv
	    __bit)
   
   (export  (expand-eval-let <expression> <expander>)
	    (expand-eval-let* <expression> <expander>)
	    (expand-eval-letrec <expression> <expander>)
	    (expand-eval-letrec* <expression> <expander>)
	    (expand-eval-labels <expression> <expander>)))

;*---------------------------------------------------------------------*/
;*    expand-eval-let ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-eval-let x e)
   
   (define (expand-let-loop loop bindings body x e)
      (if (not (every (lambda (x)
			  (match-case x
			     ((?- ?-) #t)
			     (else #f)))
		       bindings))
	  (expand-error "let" "Illegal form" x)
	  (let* ((vars (map (lambda (x)
			       (if (and (null? (cddr x))
					(not (pair? (cadr x))))
				   (cons #f (cadr x))
				   (cons #t (gensym))))
			    bindings))
		 (aux (filter-map (lambda (x y)
				     (and (car x) (cons (cdr x) (cdr y))))
				  vars bindings))
		 (rec `(letrec ((,loop (lambda ,(map car bindings)
					  ,(expand-progn body))))
			  (,loop ,@(map cdr vars))))
		 (exp (if (pair? aux) `(let ,aux ,rec) rec)))
	     (e exp e))))

   (define (make-binding var val x)
      (evepairify (list var val) x))
   
   (define (expand-let-simple bindings body x e)
      (let loop ((bindings bindings)
		 (nbindings '())
		 (ebdgs '()))
	 (if (null? bindings)
	     `(let ,(reverse! nbindings)
		 ,(%with-lexical ebdgs (expand-progn body) e #f))
	     (match-case (car bindings)
		((and (? symbol?) ?var)
		 (loop (cdr bindings)
		       (cons (make-binding var #unspecified bindings) nbindings)
		       (cons var ebdgs)))
		(((and (? symbol?) ?var) ?val)
		 (loop (cdr bindings)
		       (cons (make-binding var (e val e) (car bindings)) nbindings)
		       (cons var ebdgs)))
		(else
		 (expand-error "let" "Illegal binding form" x))))))

   (let* ((e (eval-begin-expander e))
	  (res (match-case x
		  ((?- () . (and ?body (not ())))
		   (e (expand-progn body) e))
		  ((?- (and (? symbol?) ?l) ?bindings . (and ?body (not ())))
		   (expand-let-loop l bindings body x e))
		  ((?- (and (? list?) ?bindings) . (and ?body (not ())))
		   (expand-let-simple bindings body x e))
		  (else
		   (expand-error "let" "Illegal `let' form" x)))))
      (evepairify res x)))
	   
;*---------------------------------------------------------------------*/
;*    expand-eval-let* ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-eval-let* x e)
   
   (define (make-binding var val x)
      (evepairify (list var val) x))
   
   (let* ((e (eval-begin-expander e))
	  (res (match-case x
		  ((?- () . (and ?body (not ())))
		   (e (expand-progn body) e))
		  ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		   (let loop ((bindings bindings)
			      (nbindings '())
			      (ebdgs '()))
		      (cond
			 ((null? bindings)
			  `(let* ,(reverse! nbindings)
			      ,(%with-lexical
				ebdgs (expand-progn body) e #f)))
			 ((not (pair? (car bindings)))
			  (loop (cdr bindings)
				(cons (make-binding (car bindings) #unspecified
					 bindings)
				      nbindings)
				(cons (car bindings) ebdgs)))
			 ((or (not (pair? (cdar bindings)))
			      (not (null? (cddar bindings))))
			  (expand-error "let*" "Illegal bindings form" x))
			 (else
			  (loop (cdr bindings)
				(cons (make-binding (caar bindings)
					 (%with-lexical
					    ebdgs
					    (expand-progn (cdar bindings))
					    e
					    #f)
					 (car bindings))
				      nbindings)
				(cons (caar bindings) ebdgs))))))
		  (else
		   (expand-error "let*" "Illegal form" x)))))
      (evepairify res x)))
   
;*---------------------------------------------------------------------*/
;*    expand-eval-letrec ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-eval-letrec x e)
   (let* ((e (eval-begin-expander e))
	  (res (match-case x
		  ((?- () . (and ?body (not ())))
		   (e (expand-progn body) e))
		  ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		   (let loop ((bindings bindings)
			      (nbindings '()))
		      (cond
			 ((null? bindings)
			  (let* ((nbindings (reverse! nbindings))
				 (ebody (%with-lexical
					 (bindings->list bindings)
					 (expand-progn body) e #f)))
			     `(letrec ,nbindings ,ebody)))
			 ((not (pair? (car bindings)))
			  (loop (cdr bindings)
				(cons (list (car bindings) #unspecified)
				      nbindings)))
			 ((or (not (pair? (cdar bindings)))
			      (not (null? (cddar bindings))))
			  (expand-error "letrec" "Illegal binding form" x))
			 (else
			  (loop (cdr bindings)
				(cons (list (caar bindings)
					    (e (expand-progn (cdar bindings)) e))
				      nbindings))))))
		  (else
		   (expand-error "letrec" "Illegal form" x)))))
      (evepairify res x)))

;*---------------------------------------------------------------------*/
;*    expand-eval-letrec* ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-eval-letrec* x e)
   
   (define (lambda? b)
      (and (pair? (cadr b)) (eq? (car (cadr b)) 'lambda)))
   
   (define (untype-ident id::symbol)
      (let* ((string (symbol->string id))
	     (len (string-length string)))
	 (let loop ((walker  0))
	    (cond
	       ((=fx walker len)
		id)
	       ((and (char=? (string-ref string walker) #\:)
		     (<fx walker (-fx len 1))
		     (char=? (string-ref string (+fx walker 1)) #\:))
		(string->symbol (substring string 0 walker)))
	       (else
		(loop (+fx walker 1)))))))
   
   (let* ((e (eval-begin-expander e))
	  (res (match-case x
		  ((?- () . (and ?body (not ())))
		   (e (expand-progn body) e))
		  ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		   ;; check the bindings
		   (for-each (lambda (b)
				(unless (and (pair? b)
					     (symbol? (car b))
					     (pair? (cdr b)))
				   (expand-error "letrec*" "Illegal form" x)))
		      bindings)
		   (if (every lambda? bindings)
		       ;; all bindings bind lambda form, we
		       ;; can omit intermediate variables
		       (e (evepairify
			     `(letrec ,(map (lambda (b)
					       (list (car b)
						  (e (expand-progn (cdr b)) e)))
					  bindings)
				 ,@body)
			     x)
			  e)
		       (e (evepairify
			     `(let ,(map (lambda (b)
					    (list (car b) #unspecified))
				       bindings)
				 ,@(map (lambda (b)
					   `(set! ,(untype-ident (car b))
					       ,(e (expand-progn (cdr b)) e)))
				      bindings)
				 ,@body)
			     x)
			  e)))
		  (else
		   (expand-error "letrec*" "Illegal form" x)))))
      (evepairify res x)))

;*---------------------------------------------------------------------*/
;*    expand-eval-labels ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-eval-labels x e)
   (let ((res (match-case x
		 ((?- () . (and ?body (not ())))
		  (e `((lambda () ,(expand-progn body))) e))
		 ((?- ?bindings . (and ?body (not ())))
		  (let ((new (let loop ((bindings bindings))
				(cond
				   ((null? bindings)
				    '())
				   ((not (pair? bindings))
				    (expand-error "expand-labels" "Illegal form" x))
				   (else
				    (match-case (car bindings)
				       ((?name ?args . ?lbody)
					(cons `(,name (lambda ,args ,@lbody))
					      (loop (cdr bindings))))
				       (else
					(expand-error "expand-labels"
					       "Illegal form"
					       x))))))))
		     (e `(letrec ,new ,@body) e)))
		 (else
		  (expand-error "expand-labels" "Illegal form" x)))))
      (evepairify res x)))
