;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/R5rs/hygiene.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  E. Kolbecker.                                     */
;*    Creation    :  Tue Jul  9 17:48:37 2002                          */
;*    Last change :  Tue Mar 11 16:00:13 2008 (serrano)                */
;*    Copyright   :  2002-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The hygienic filter to be applied for getting hygiene.           */
;*    I have lost the historic of this file. As far as I understand,   */
;*    the first version is due to E. Kolbecker. The version I have     */
;*    directly used is the one of STk by E. Gallesio.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r5_macro_4_3_hygiene
   
   (import  __error)

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
	    __r5_macro_4_3_syntax)
   
   (export  (r5rs-macro-matches-pattern? p e k)
	    (r5rs-macro-get-bindings p e k)
	    (r5rs-macro-expand-pattern p r k)
	    
	    (r5rs-hygiene-flatten::pair-nil ::pair-nil)
	    (r5rs-hygiene-tag e kk al)
	    (r5rs-hygiene-untag e al tmps)))

;*---------------------------------------------------------------------*/
;*    r5rs-macro-matches-pattern? ...                                  */
;*---------------------------------------------------------------------*/
(define (r5rs-macro-matches-pattern? p e k)
   (cond
      ((ellipsis? p)
       (if (not (=fx (length p) 2))
	   (error "syntax-rules" "Illegal ellipsis" p)
	   (and (list? e)
		(let ((p0 (car p)))
		   (every (lambda (ei)
			     (r5rs-macro-matches-pattern? p0 ei k))
			  e)))))
      ((pair? p)
       (and (pair? e)
	    (r5rs-macro-matches-pattern? (car p) (car e) k)
	    (r5rs-macro-matches-pattern? (cdr p) (cdr e) k)))
      ((symbol? p)
       (if (memq p k) (eq? p e) #t))
      (else
       (equal? p e))))

;*---------------------------------------------------------------------*/
;*    r5rs-macro-get-bindings ...                                      */
;*---------------------------------------------------------------------*/
(define (r5rs-macro-get-bindings p e k)
   (cond
      ((ellipsis? p)
       (let ((p0 (car p)))
	  (list (cons (get-ellipsis-nestings p0 k)
		      (map (lambda (ei)
			      (r5rs-macro-get-bindings p0 ei k))
			   e)))))
      ((pair? p)
       (append (r5rs-macro-get-bindings (car p) (car e) k)
	       (r5rs-macro-get-bindings (cdr p) (cdr e) k)))
      ((symbol? p)
       (if (memq p k) '() (list (cons p e))))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    r5rs-macro-expand-pattern ...                                    */
;*---------------------------------------------------------------------*/
(define (r5rs-macro-expand-pattern p r k)
   (cond
      ((ellipsis? p)
       (append (let* ((p0 (car p))
		      (nestings (get-ellipsis-nestings p0 k))
		      (rr (ellipsis-sub-envs nestings r)))
		  (if (not (list? rr))
		      (error 'r5rs-macro-expansion
			     "argument should be a list"
			     r)
		      (map (lambda (ri)
			      (r5rs-macro-expand-pattern p0 (append ri r) k))
			   rr)))
	       (r5rs-macro-expand-pattern (cddr p) r k)))
      ((pair? p)
       (cons (r5rs-macro-expand-pattern (car p) r k)
	     (r5rs-macro-expand-pattern (cdr p) r k)))
      ((symbol? p)
       (if (memq p k)
	   p
	   (let ((x (assq p r)))
	      (if (pair? x)
		  (cdr x)
		  p))))
      (else
       p)))

;*---------------------------------------------------------------------*/
;*    get-ellipsis-nestings ...                                        */
;*---------------------------------------------------------------------*/
(define (get-ellipsis-nestings p k)
   (let sub ((p p))
      (cond
	 ((ellipsis? p)
	  (cons (sub (car p)) (sub (cddr p))))
	 ((pair? p)
	  (append (sub (car p)) (sub (cdr p))))
	 ((symbol? p)
	  (if (memq p k) '() (list p)))
	 (else
	  '()))))

;*---------------------------------------------------------------------*/
;*    ellipsis? ...                                                    */
;*---------------------------------------------------------------------*/
(define (ellipsis? x)
   (and (pair? x)
	(pair? (cdr x))
	(eq? (cadr x) '...)))

;*---------------------------------------------------------------------*/
;*    ellipsis-sub-envs ...                                            */
;*---------------------------------------------------------------------*/
(define (ellipsis-sub-envs nestings r)
   (some (lambda (c) (if (intersect? nestings (car c)) (cdr c) #f)) r))

;*---------------------------------------------------------------------*/
;*    intersect? ...                                                   */
;*---------------------------------------------------------------------*/
(define (intersect? v y)
   (if (or (symbol? v) (symbol? y))
       (eq? v y)
       (some (lambda (vi) (some (lambda (yj) (intersect? vi yj)) y)) v)))
 
;*---------------------------------------------------------------------*/
;*    r5rs-hygiene-flatten ...                                         */
;*---------------------------------------------------------------------*/
(define (r5rs-hygiene-flatten e)
   (let loop2 ((e e) (r '()))
      (cond ((pair? e) (loop2 (car e) (loop2 (cdr e) r)))
	    ((null? e) r)
	    (else (cons e r)))))

;*---------------------------------------------------------------------*/
;*    r5rs-hygiene-tag ...                                             */
;*---------------------------------------------------------------------*/
(define (r5rs-hygiene-tag e kk al)
   (define (hygiene-tag-inner e kk al)
      (cond ((pair? e)
	     (let* ((a-te-al (hygiene-tag-inner (car e) kk al))
		    (d-te-al (hygiene-tag-inner (cdr e) kk (cdr a-te-al))))
		(cons (cons (car a-te-al) (car d-te-al))
		      (cdr d-te-al))))
	    ((vector? e)
	     (let ((r (hygiene-tag-inner (vector->list e) kk al)))
		(cons (list->vector (car r)) (cdr r))))
	    ((symbol? e)
	     (cond ((eq? e '...) (cons '... al))
		   ((memq e kk) (cons e al))
		   ((hygiene-rassq e al) => (lambda (c) (cons (car c) al)))
		   (else
		    (let ((te (gensym)))
		       (cons te (cons (cons te e) al))))))
	    (else (cons e al))))
   (let ((r (hygiene-tag-inner e kk al)))
      (values (car r) (cdr r))))

;*---------------------------------------------------------------------*/
;*    r5rs-hygiene-untag ...                                           */
;*---------------------------------------------------------------------*/
(define (r5rs-hygiene-untag e al tmps)
   (if (pair? e)
       (let ((a (r5rs-hygiene-untag (car e) al tmps)))
	  (if (list? e)
	      (case a
		 ((quote)
		  (hygiene-untag-no-tags e al))
		 ((if begin)
		  `(,a ,@(map (lambda (e1)
				 (r5rs-hygiene-untag e1 al tmps)) (cdr e))))
		 ((set! define)
		  `(,a ,(hygiene-untag-vanilla (cadr e) al tmps)
		       ,@(map (lambda (e1)
				 (r5rs-hygiene-untag e1 al tmps)) (cddr e))))
		 ((lambda)
		  (hygiene-untag-lambda (cadr e) (cddr e) al tmps))
		 ((bind-exit)
		  (hygiene-untag-bind-exit (cadr e) (cddr e) al tmps))
		 ((letrec)
		  (hygiene-untag-letrec (cadr e) (cddr e) al tmps))
		 ((let)
		  (match-case e
		     ((?- (and ?e2 (? symbol?)) ?bdgs . ?body)
		      (hygiene-untag-named-let e2 bdgs body al tmps))
		     ((?- ?bdgs . ?body)
		      (hygiene-untag-let bdgs body al tmps))
		     (else
		      (cons a (hygiene-untag-list (cdr e) al tmps)))))
		 ((let*)
		  (hygiene-untag-let* (cadr e) (cddr e) al tmps))
		 ((do)
		  (hygiene-untag-do (cadr e) (caddr e) (cdddr e) al tmps))
		 ((case)
		  `(case ,(hygiene-untag-vanilla (cadr e) al tmps)
		      ,@(map
			 (lambda (c)
			    `(,(hygiene-untag-vanilla (car c) al tmps)
			      ,@(hygiene-untag-list (cdr c) al tmps)))
			 (cddr e))))
		 ((cond)
		  `(cond ,@(map
			    (lambda (c)
			       (hygiene-untag-list c al tmps))
			    (cdr e))))
		 (else (cons a (hygiene-untag-list (cdr e) al tmps))))
	      (cons a (hygiene-untag-list* (cdr e) al tmps))))
       (hygiene-untag-vanilla e al tmps)))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-list ...                                           */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-list ee al tmps)
   (map (lambda (e) (r5rs-hygiene-untag e al tmps)) ee))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-list* ...                                          */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-list* ee al tmps)
   (let loop3 ((ee ee))
      (if (pair? ee)
	  (cons (r5rs-hygiene-untag (car ee) al tmps)
		(loop3 (cdr ee)))
	  (r5rs-hygiene-untag ee al tmps))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-no-tags ...                                        */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-no-tags e al)
   (cond ((pair? e)
	  (cons (hygiene-untag-no-tags (car e) al)
		(hygiene-untag-no-tags (cdr e) al)))
	 ((vector? e)
	  (list->vector (hygiene-untag-no-tags (vector->list e) al)))
	 ((not (symbol? e)) e)
	 ((assq e al) => cdr)
	 (else e)))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-lambda ...                                         */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-lambda bvv body al tmps)
   (let ((tmps2 (cond
		   ((pair? bvv)
                    (append! (r5rs-hygiene-flatten bvv) tmps))
		   ((null? bvv)
		    tmps)
		   (else
		    (cons bvv tmps)))))
      `(lambda ,bvv
	  ,@(hygiene-untag-list body al tmps2))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-bind-exit ...                                      */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-bind-exit bvv body al tmps)
   (let ((tmps2 (append! (r5rs-hygiene-flatten bvv) tmps)))
      `(bind-exit ,bvv
	  ,@(hygiene-untag-list body al tmps2))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-letrec ...                                         */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-letrec varvals body al tmps)
   (let ((tmps (append! (map (lambda (x)
				(cond
				   ((pair? x)
				    (car x))
				   ((symbol? x)
				    x)
				   (else
				    '_)))
			     varvals)
			tmps)))
      `(letrec ,(map (lambda (varval)
			(match-case varval
			   ((?var ?val)
			    `(,var ,(r5rs-hygiene-untag val al tmps)))
			   (else
			    (r5rs-hygiene-untag varval al tmps))))
		     varvals)
	  ,@(hygiene-untag-list body al tmps))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-let ...                                            */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-let varvals body al tmps)
   (cond
      ((null? varvals)
       `(let () ,@(hygiene-untag-list body al tmps)))
      ((list? varvals)
       (let ((tmps2 (append! (map (lambda (x)
				     (cond
					((pair? x)
					 (car x))
					((symbol? x)
					 x)
					(else
					 '_)))
				  varvals)
			     tmps)))
	  `(let ,(map (lambda (varval)
			 (match-case varval
			    ((?var ?val)
			     `(,var ,(r5rs-hygiene-untag val al tmps)))
			    (else
			     (r5rs-hygiene-untag varval al tmps))))
		      varvals)
	      ,@(hygiene-untag-list body al tmps2))))
      (else
       `(let ,(r5rs-hygiene-untag varvals al tmps)
	   ,@(hygiene-untag-list body al tmps)))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-named-let ...                                      */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-named-let lname varvals body al tmps)
   (let ((tmps2 (cons lname (append! (map (lambda (x)
					     (cond
						((pair? x)
						 (car x))
						((symbol? x)
						 x)
						(else
						 '_)))
					  varvals)
				     tmps))))
      `(let ,lname
	  ,(map (lambda (varval)
		   (match-case varval
		      ((?var ?val)
		       `(,var ,(r5rs-hygiene-untag val al tmps)))
		      (else
		       (r5rs-hygiene-untag varval al tmps))))
		varvals)
	  ,@(hygiene-untag-list body al tmps2))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-let* ...                                           */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-let* varvals body al tmps)
   (let ((tmps2 (append! (reverse! (map (lambda (x)
					   (cond
					      ((pair? x)
					       (car x))
					      ((symbol? x)
					       x)
					      (else
					       '_)))
					varvals))
			 tmps)))
      `(let* ,(let loop4 ((varvals varvals)
			 (i (length varvals)))
		 (if (null? varvals) '()
		     (let ((varval (car varvals)))
			(cons (match-case varval
				 ((?var ?val)
				  `(,var ,(r5rs-hygiene-untag
					   val
					   al (list-tail tmps2 i))))
				 (else
				  (r5rs-hygiene-untag varval al tmps)))
			      (loop4 (cdr varvals) (-fx i 1))))))
	  ,@(hygiene-untag-list body al tmps2))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-do ...                                             */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-do varinistps exit-test body al tmps)
   (let ((tmps2 (append! (map car varinistps) tmps)))
      `(do ,(map
	     (lambda (varinistp)
		(let ((var (car varinistp)))
		   `(,var ,@(hygiene-untag-list (cdr varinistp) al
						(cons var tmps)))))
	     varinistps)
	   ,(hygiene-untag-list exit-test al tmps2)
	   ,@(hygiene-untag-list body al tmps2))))

;*---------------------------------------------------------------------*/
;*    hygiene-untag-vanilla ...                                        */
;*---------------------------------------------------------------------*/
(define (hygiene-untag-vanilla e al tmps)
   (cond ((pair? e)
	  (cons (hygiene-untag-vanilla (car e) al tmps)
		(hygiene-untag-vanilla (cdr e) al tmps)))
	 ((vector? e)
	  (list->vector (hygiene-untag-vanilla (vector->list e) al tmps)))
	 ((not (symbol? e)) e)
	 ((memq e tmps) e)
	 ((assq e al) => cdr)
	 (else e)))

;*---------------------------------------------------------------------*/
;*    hygiene-rassq ...                                                */
;*---------------------------------------------------------------------*/
(define (hygiene-rassq k al)
   (let loop5 ((al al))
      (if (null? al) #f
	  (let ((c (car al)))
	     (if (eq? (cdr c) k) c
		 (loop5 (cdr al)))))))

;*---------------------------------------------------------------------*/
;*    some ...                                                         */
;*---------------------------------------------------------------------*/
(define (some pred l)
  (if (null? l) 
      #f
      (or (pred (car l)) 
	  (some pred (cdr l)))))

