;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/generator.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Mon Feb  9 16:19:48 2026                          */
;*    Last change :  Mon Feb  9 16:40:42 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Generators (aka lambda*)                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __generator
   
   (import  __error
	    __object)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    __structure
	    __date
	    __os
	    __bit
	    __thread

	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
	    __r4_characters_6_6
	    __r4_equivalence_6_2 
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __r5_control_features_6_4
	    
	    __foreign
	    __evenv)

   (export  (class generator)
	    (lambda*-expander x e)))

;*---------------------------------------------------------------------*/
;*    cps-gensym ...                                                   */
;*---------------------------------------------------------------------*/
(define-expander gensym
   (lambda (x e)
      (if #f
	  (e `(begin
		 (set! *gencnt* (+fx *gencnt* 1))
		 (string->symbol (format "~a~a" ,prefix *gencnt*)))
	     e)
	  (match-case x
	     ((?- ?prefix)
	      `(gensym ,(e prefix e)))
	     ((?-)
	      '(gensym))))))

;*---------------------------------------------------------------------*/
;*    *gencnt* ...                                                     */
;*---------------------------------------------------------------------*/
(define *gencnt* 0)

;*---------------------------------------------------------------------*/
;*    lambda*-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (lambda*-expander x e)
   (match-case x
      ((lambda* ?args . ?body)
       (let ((v (gensym 'e))
	     (t (gensym 't)))
	  (e `(lambda ,args
		 (instantiate::generator
		    (kont (lambda (,v ,t)
			     ,(cps `(begin ,@body) (lambda (x) x))))))
	     e)))
      (else
       (error "lambda*" "wrong syntax" x))))
      
;*---------------------------------------------------------------------*/
;*    cps ...                                                          */
;*---------------------------------------------------------------------*/
(define (cps x::obj ck::procedure)
   
   (define (cps-value a)
      (ck a))
   
   (define (cps-yield y)
      (let ((e (gensym 'e))
	    (v (gensym 'v)))
	 (cps y
	    (lambda (x)
	       `(generator-yield g ,x
		   (lambda (,e ,v) ,(ck e)))))))
   
   (define (cps-> o p)
      (cps o
	 (lambda (x)
	    (ck `(-> ,x ,p)))))

   (define (cps-set v exp)
      (cps exp
	 (lambda (e)
	    (ck `(set! ,v ,e)))))
   
   (define (cps-set-> o p e)
      (cps o
	 (lambda (o)
	    (cps e
	       (lambda (e)
		  (ck `(set! (-> ,o ,p) ,e)))))))
   
   (define (cps-if exp then otherwise)
      (let ((k (gensym 'k))
	    (x (gensym 'x)))
	 `(let ((,k (lambda (,x) ,(ck x))))
	     ,(cps exp
		 (lambda (e)
		    `(if ,e
			 ,(cps then (lambda (x) `(,k ,x)))
			 ,(cps otherwise (lambda (x) `(,k ,x)))))))))
   
   (define (cps-begin exps)
      (if (null? exps)
	  (ck #unspecified)
	  (let loop ((exp (car exps))
		     (exps (cdr exps)))
	     (if (null? exps)
		 (cps exp ck)
		 (cps exp
		    (lambda (x)
		       `(begin ,x ,(loop (car exps) (cdr exps)))))))))

   (define (cps-app fun args)
      (if (symbol? fun)
	  (let loop ((args args)
		     (vals '()))
	     (if (null? args)
		 (ck `(,fun ,@(reverse vals)))
		 (cps (car args)
		    (lambda (x)
		       (loop (cdr args) (cons x vals))))))
	  (let ((f (gensym 'f)))
	     (cps `(let ((,f ,fun)) (,f ,@args)) ck))))

   (define (cps-loop lp bdgs body)
      (let loop ((bdgs bdgs)
		 (tmps '()))
	 (if (null? bdgs)
	     `(let ,lp ,tmps
		   ,(cps (normalize-exp* body) ck))
	     (let ((bdg (car bdgs))
		   (tmp (gensym 't)))
		(if (symbol? bdg)
		    `(let (,tmp)
			,(loop (cdr bdgs)
			    (cons (list bdg tmp) tmps)))
		    (cps (cadr bdg)
		       (lambda (x)
			  `(let ((,tmp ,x))
			      ,(loop (cdr bdgs)
				  (cons (list (car bdg) tmp) tmps))))))))))

   (define (cps-let keyword bdgs body)
      (let loop ((bdgs bdgs)
		 (tmps '()))
	 (if (null? bdgs)
	     `(,keyword ,(reverse tmps)
		 ,(cps (normalize-exp* body) ck))
	     (let ((bdg (car bdgs))
		   (tmp (gensym 't)))
		(if (symbol? bdg)
		    `(let (,tmp)
			,(loop (cdr bdgs)
			    (cons (list bdg tmp) tmps)))
		    (cps (cadr bdg)
		       (lambda (x)
			  `(let ((,tmp ,x))
			      ,(loop (cdr bdgs)
				  (cons (list (car bdg) tmp) tmps))))))))))

   (define (cps-labels bdgs body)
      `(labels ,bdgs ,(cps body ck)))

   (define (cps-case exp clauses)
      (let ((k (gensym 'k))
	    (x (gensym 'x)))
	 `(let ((,k (lambda (,x) ,(ck x))))
	     ,(cps exp
		 (lambda (e)
		    `(case ,e
			,@(map (lambda (c)
				  (match-case c
				     ((?vals . ?body)
				      `(,vals ,(cps (normalize-exp* body)
						  (lambda (x) `(,k ,x)))))))
			   clauses)))))))

   (define (normalize-exp* exp*)
      (match-case exp*
	 (() #unspecified)
	 (((begin . ?-)) (car exp*))
	 (else `(begin ,@exp*))))
	  
   (define (normalize-begin x)
      (match-case x
	 ((begin . ?-)
	  `(begin
	      ,@(let normalize ((x x))
		   (match-case x
		      ((begin)
		       '())
		      ((begin ?x)
		       x)
		      ((begin ?x (begin ?y))
		       (cons x (normalize y)))
		      ((begin ?x (begin . ?y))
		       (cons x y))
		      ((begin ?x . ?y)
		       (cons x y))
		      (else
		       x)))))
	 (else
	  x)))

   (define (normalize-if x)
      (match-case x
	 ((let ((?k (lambda (?v) ?v))) (if ?test (?k ?then) (?k ?otherwise)))
	  `(if ,test ,then ,otherwise))
	 ((let ((?k (lambda (?v) ?e))) (if ?test (?k ?then) (?k ?otherwise)))
	  `((lamdbda (,v) ,e) (if ,test ,then ,otherwise)))
	 (else
	  x)))

   (define (normalize-let x)
      (let loop ((x x)
		 (tmps '()))
	 (match-case x
	    ((let ((and ?tmp (?- ?-))) (and ?nx (let ?- . ?-)))
	     (loop nx (cons tmp tmps)))
	    ((let ?bindings (and ?nx (let ?- . ?-)))
	     (if (pair? tmps)
		 `(let ,tmps ,x)
		 x))
	    ((let ?bindings . ?nx)
	     (cond
		((not (=fx (length bindings) (length tmps)))
		 (if (pair? tmps)
		     `(letx ,tmps ,x)
		     x))
		((filter-map (lambda (b t)
				(match-case b
				   ((?var ?val)
				    (when (eq? val (car t))
				       (list var (cadr t))))
				   (else #f)))
		    bindings (reverse tmps))
		 =>
		 (lambda (nbindings)
		    `(let ,nbindings ,@nx)))
		(else
		 `(letx ,tmps ,@x))))
	    ((generator-yield ?g ?v (lambda ?args (and ?nx (let . ?-))))
	     `(generator-yield ,g ,v (lambda ,args ,(normalize-let nx))))
	    (else
	     (if (null? tmps)
		 x
		 `(let ,tmps ,x))))))

   (match-case x
      ((quote ?-) (cps-value x))
      ((atom ?a) (cps-value a))
      ((@ (? symbol?) (? symbol?)) (cps-value x))
      ((yield ?v) (cps-yield v))
      ((-> ?o (and ?p (? symbol?))) (cps-> o p))
      ((lambda . ?-) (cps-value x))
      ((set! (and (? symbol?) ?v) ?exp) (cps-set v exp))
      ((set! (-> ?o (and ?p (? symbol?))) ?e) (cps-set-> o p e))
      ((if ?exp ?then ?otherwise) (normalize-if (cps-if exp then otherwise)))
      ((begin . ?exps) (normalize-begin (cps-begin exps)))
      ((let (and (? symbol?) ?loop) ?bdgs . ?body) (cps-loop loop bdgs body))
      ((let ?bdgs . ?body) (normalize-let (cps-let 'let bdgs body)))
      ((let* ?bdgs . ?body) (normalize-let (cps-let 'let* bdgs body)))
      ((letrec ?bdgs . ?body) (normalize-let (cps-let 'letrec bdgs body)))
      ((letrec* ?bdgs . ?body) (normalize-let (cps-let 'letrec* bdgs body)))
      ((labels ?bdgs . ?body) (cps-labels bdgs body))
      ((case ?e . ?clauses) (cps-case e clauses))
      ((?fun . ?args) (cps-app fun args))
      (else 'todo)))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
'(define (test)
   (for-each (lambda (e)
		(set! *gencnt* 0)
		(print (car e))
		(let ((c (cps (car e) (lambda (x) x))))
		   (if (equal? c (cadr e))
		       (print "ok")
		       (begin
			  (print "==>")
			  (pp (cadr e))
			  (print "<==")
			  (pp c)))
		   (newline)))
      '(
	;; set!
	((set! v 1)
	 (set! v 1))
	((set! v (yield 1))
	 (generator-yield g 1 (lambda (e1 v2) (set! v e1))))
	;; ->
	((-> o x)
	 (-> o x))
	((-> (yield 1) x)
	 (generator-yield g 1 (lambda (e1 v2) (-> e1 x))))
	((yield (-> o x))
	 (generator-yield g (-> o x) (lambda (e1 v2) e1)))
	((set! (-> o p) 3)
	 (set! (-> o p) 3))
	((set! (-> (yield 1) p) 3)
	 (generator-yield g 1
	    (lambda (e1 v2) (set! (-> e1 p) 3))))
	((set! (-> (yield 1) p) (yield 2))
	 (generator-yield g 1
	    (lambda (e1 v2)
	       (generator-yield g 2
		  (lambda (e3 v4)
		     (set! (-> e1 p) e3))))))
	;; yield + yield
	((yield (yield 1))
	 (generator-yield g 1
	    (lambda (e3 v4)
	       (generator-yield g e3
		  (lambda (e1 v2) e1)))))
	;; begin
	((begin 1 2 3)
	 (begin 1 2 3))
	((begin 1 2 (yield 3))
	 (begin 1 2 (generator-yield g 3 (lambda (e1 v2) e1))))
	((begin 1 (yield 2) 3)
	 (begin 1 (generator-yield g 2 (lambda (e1 v2) (begin e1 3)))))
	;; if
	((if 1 2 3)
	 (if 1 2 3))
	((if (yield 1) 2 3)
	 (let ((k1 (lambda (x2) x2)))
	    (generator-yield g 1 (lambda (e3 v4) (if e3 (k1 2) (k1 3))))))
	((if (yield 1) 2 (yield 3))
	 (let ((k1 (lambda (x2) x2)))
	    (generator-yield g 1
	       (lambda (e3 v4)
		  (if e3
		      (k1 2)
		      (generator-yield g 3 (lambda (e5 v6) (k1 e5))))))))
	((if 1 (yield 2) 3)
	 (let ((k1 (lambda (x2) x2)))
	    (if 1 (generator-yield g 2 (lambda (e3 v4) (k1 e3))) (k1 3))))
	((if 1 (begin (yield 2) 3) 4)
	 (let ((k1 (lambda (x2) x2)))
	    (if 1 (generator-yield g 2 (lambda (e3 v4) (begin e3 (k1 3)))) (k1 4))))
	((if 1 (yield 2) (yield 3))
	 (let ((k1 (lambda (x2) x2)))
	    (if 1
		(generator-yield g 2 (lambda (e3 v4) (k1 e3)))
		(generator-yield g 3 (lambda (e5 v6) (k1 e5))))))
	;; begin + if
	((begin 1 (if 2 (yield 3) 4) 5)
	 (begin
	    1
	    (let ((k1 (lambda (x2) (begin x2 5))))
	       (if 2
		   (generator-yield g 3 (lambda (e3 v4) (k1 e3)))
		   (k1 4)))))
	;; app
	((f 1 2 3)
	 (f 1 2 3))
	((f (yield 1) 2 3)
	 (generator-yield g 1 (lambda (e1 v2) (f e1 2 3))))
	((f (yield 1) (yield 2) 3)
	 (generator-yield g 1
	    (lambda (e1 v2)
	       (generator-yield g 2
		  (lambda (e3 v4)
		     (f e1 e3 3))))))
	((begin (yield 1) (f (yield 2)) 3)
	 (generator-yield g 1
	    (lambda (e1 v2)
	       (begin
		  e1
		  (generator-yield g 2
		     (lambda (e3 v4)
			(begin (f e3) 3)))))))
	;; let
	((let ((x 1)) x)
	 (let ((x 1)) x))
	((let ((x 1) (y 2)) (+ x y))
	 (let ((x 1) (y 2)) (+ x y)))
	((let ((x 1) (y 2) (z 3)) (+ x y z))
	 (let ((x 1) (y 2) (z 3)) (+ x y z)))
	((let ((x 1)) 1 x)
	 (let ((x 1)) (begin 1 x)))
	((let ((x 1)) (yield 1) x)
	 (let ((x 1)) (generator-yield g 1 (lambda (e2 v3) (begin e2 x)))))
	((let ((x 1)) 1 (yield x))
	 (let ((x 1)) (begin 1 (generator-yield g x (lambda (e2 v3) e2)))))
	((let ((x 1)) (yield 1) (yield x))
	 (let ((x 1)) (generator-yield g 1 (lambda (e2 v3) (begin e2 (generator-yield g x (lambda (e4 v5) e4)))))))
	((let ((x (yield 1))) 1 x)
	 (generator-yield g 1 (lambda (e2 v3) (let ((x e2)) (begin 1 x)))))
	((let ((x (yield 1))) (yield 2) x)
	 (generator-yield g 1
	    (lambda (e2 v3)
	       (let ((x e2))
		  (generator-yield g 2 (lambda (e4 v5) (begin e4 x)))))))
	((let ((x (yield 1))) (yield 2) (yield x))
	 (generator-yield g 1
	    (lambda (e2 v3)
	       (let ((x e2))
		  (generator-yield g 2
		     (lambda (e4 v5)
			(begin
			   e4
			   (generator-yield g x
			      (lambda (e6 v7) e6)))))))))
	((let ((x (yield 1)) (y 2)) (+ x y))
	 (generator-yield g 1
	    (lambda (e2 v3)
	       (let ((x e2) (y 2)) (+ x y)))))
	((let ((x (yield 1)) (y (yield 2))) (+ x y))
	 (generator-yield g 1
	    (lambda (e2 v3)
	       (let ((t1 e2))
		  (generator-yield g 2
		     (lambda (e5 v6)
			(let ((t4 e5))
			   (let ((x t1) (y t4)) (+ x y)))))))))
	((let* ((x (yield 1)) (y (yield 2))) (yield (+ x y)))
	 (generator-yield
	    g
	    1
	    (lambda (e2 v3)
	       (let ((t1 e2))
		  (generator-yield
		     g
		     2
		     (lambda (e5 v6)
			(let ((t4 e5))
			   (let* ((x t1) (y t4))
			      (generator-yield g (+ x y) (lambda (e7 v8) e7))))))))))
	;; let loop
	((let loop ((x 1)) (yield x) (loop (+ x 1)))
	 (let ((t1 1))
	    (let loop ((x t1))
	       (generator-yield g x
		  (lambda (e2 v3)
		     (begin e2 (loop (+ x 1))))))))
	((let loop ((x (yield 1))) (loop (yield x)))
	 (generator-yield g 1
	    (lambda (e2 v3)
	       (let ((t1 e2))
		  (let loop ((x t1))
		     (generator-yield g x
			(lambda (e4 v5)
			   (loop e4))))))))
	;; case
	((case (yield 1) ((a b) 2) (else 3))
	 (let ((k1 (lambda (x2) x2)))
	    (generator-yield g 1
	       (lambda (e3 v4)
		  (case e3 ((a b) (k1 2)) (else (k1 3)))))))
	)))
