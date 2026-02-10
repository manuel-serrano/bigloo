;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/generator.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Mon Feb  9 16:19:48 2026                          */
;*    Last change :  Tue Feb 10 05:38:20 2026 (serrano)                */
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

   (export  (class generator
	       (kont::procedure (default list))
	       (done::bool (default #f)))
	    (inline generator-yield ::generator ::obj ::procedure)
	    (inline next ::generator ::obj)
	    (inline throw ::generator ::obj)
	    (expand-lambda* x::pair e::procedure)
	    (generator-cps x::obj g::symbol ck::procedure gensym::procedure)))

;*---------------------------------------------------------------------*/
;*    generator-yield ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (generator-yield g::generator value::obj kont::procedure)
   (set! (-> g kont) kont)
   value)

;*---------------------------------------------------------------------*/
;*    next ...                                                         */
;*---------------------------------------------------------------------*/
(define-inline (next g::generator value::obj)
   ((-> g kont) value #f))
   
;*---------------------------------------------------------------------*/
;*    throw ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (throw g::generator value::obj)
   ((-> g kont) value #t))
   
;*---------------------------------------------------------------------*/
;*    expand-lambda* ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-lambda* x e)
   (match-case x
      ((lambda* ?args . ?body)
       (let ((v (gensym 'e))
	     (t (gensym 't))
	     (g (gensym 'g))
	     (r (gensym 'r)))
	  (let* ((le (lambda (x ne)
			(match-case x
			   ((let (and ?lp (? symbol?)) ?bindings . ?body)
			    ;; loops must be preserved until the cps conversion
			    `(let ,lp ,(map (lambda (b)
					       (match-case b
						  ((?var ?val)
						   (list var (e val e)))))
					  bindings)
				  ,@(map (lambda (x) (e x e)) body)))
			   (else
			    (e x ne)))))
		 (k0 (lambda (x)
			`(with-access::generator ,g (done)
			    (set! done #t)
			    ,x)))
		 (cps-body (generator-cps (le `(begin ,@body) le) g k0 gensym)))
	     (e `(lambda ,args
		    (let ((,g (instantiate::generator)))
		       (with-access::generator ,g (kont)
			  (set! kont
			     (lambda (,v ,t)
				,cps-body)))
		       ,g))
		e))))
      (else
       (error "lambda*" "wrong syntax" x))))

;*---------------------------------------------------------------------*/
;*    generator-cps ...                                                */
;*---------------------------------------------------------------------*/
(define (generator-cps x g::symbol ck::procedure gensym::procedure)

   (define (cps-value a stack ck)
      (ck a))
   
   (define (cps-variable v stack ck)
      (let ((c (assq v stack)))
	 (if (and (pair? c) (cdr c))
	     (error "generator" "Illegal loop-bound variable usage" v)
	     (ck v))))
   
   (define (cps-yield y stack ck)
      (let ((e (gensym 'e))
	    (v (gensym 'v)))
	 (cps g y stack
	    (lambda (x)
	       `(generator-yield ,g ,x
		   (lambda (,e ,v) ,(ck e)))))))
   
   (define (cps-> o p stack ck)
      (cps g o stack
	 (lambda (x)
	    (ck `(-> ,x ,p)))))
   
   (define (cps-set v exp stack ck)
      (cps g exp stack
	 (lambda (e)
	    (ck `(set! ,v ,e)))))
   
   (define (cps-set-> o p e stack ck)
      (cps g o stack
	 (lambda (o)
	    (cps g e stack
	       (lambda (e)
		  (ck `(set! (-> ,o ,p) ,e)))))))
   
   (define (cps-if exp then otherwise stack ck)
      (let ((k (gensym 'k))
	    (x (gensym 'x)))
	 `(let ((,k (lambda (,x) ,(ck x))))
	     ,(cps g exp stack
		 (lambda (e)
		    `(if ,e
			 ,(cps g then stack (lambda (x) `(,k ,x)))
			 ,(cps g otherwise stack (lambda (x) `(,k ,x)))))))))
   
   (define (cps-begin exps stack ck)
      (if (null? exps)
	  (ck #unspecified)
	  (let loop ((exp (car exps))
		     (exps (cdr exps)))
	     (if (null? exps)
		 (cps g exp stack ck)
		 (cps g exp stack
		    (lambda (x)
		       `(begin ,x ,(loop (car exps) (cdr exps)))))))))
   
   (define (cps-with-access keyword obj props body stack ck)
      (cps g obj stack
	 (lambda (x)
	    `(,keyword ,x ,props
		,(cps g (normalize-exp* body)
		    (append (args->frame props) stack)
		    ck)))))
   
   (define (cps-instantiate keyword props stack ck)
      (let ((tmps (map (lambda (p) (gensym (car p))) props)))
	 (let ((x `(let ,(map (lambda (tmp prop) (list tmp (cadr prop)))
			    tmps props)
		      (,keyword ,@(map (lambda (tmp prop) (list (car prop) tmp))
				     tmps props)))))
	    (cps g x stack ck))))
   
   (define (cps-app fun args::pair-nil stack::pair-nil ck::procedure)
      (cond
	 ((not (symbol? fun))
	  (let ((f (gensym 'f)))
	     (cps g `(let ((,f ,fun)) (,f ,@args)) stack ck)))
	 ((loop? fun stack)
	  (let loop ((args args)
		     (vals '()))
	     (if (null? args)
		 (let ((x (gensym 'x)))
		    `(,fun (lambda (,x) ,(ck x)) ,@(reverse vals)))
		 (cps g (car args) stack
		    (lambda (x)
		       (loop (cdr args) (cons x vals)))))))
	 (else
	  (let loop ((args args)
		     (vals '()))
	     (if (null? args)
		 (ck `(,fun ,@(reverse vals)))
		 (cps g (car args) stack
		    (lambda (x)
		       (loop (cdr args) (cons x vals)))))))))
   
   (define (cps-loop lp bdgs body stack ck)
      (let ((k (gensym 'k))
	    (x (gensym 'x)))
	 (let loop ((bdgs bdgs)
		    (tmps '()))
	    (if (null? bdgs)
		(let ((frame (args->frame (map car tmps))))
		   `(let ,lp ((,k (lambda (,x) ,(ck x))) ,@tmps)
			 ,(cps g (normalize-exp* body)
			     (cons (cons lp k) (append frame stack))
			     (lambda (x) `(,k ,x)))))
		(let ((bdg (car bdgs))
		      (tmp (gensym 't)))
		   (if (symbol? bdg)
		       `(let (,tmp)
			   ,(loop (cdr bdgs)
			       (cons (list bdg tmp) tmps)))
		       (cps g (cadr bdg) stack
			  (lambda (x)
			     `(let ((,tmp ,x))
				 ,(loop (cdr bdgs)
				     (cons (list (car bdg) tmp) tmps)))))))))))
   
   (define (cps-let keyword bdgs body stack ck)
      (let loop ((bdgs bdgs)
		 (tmps '()))
	 (if (null? bdgs)
	     (let ((frame (args->frame (map car tmps))))
		`(,keyword ,(reverse tmps)
		    ,(cps g (normalize-exp* body) (append frame stack) ck)))
	     (let ((bdg (car bdgs))
		   (tmp (gensym 't)))
		(if (symbol? bdg)
		    `(let (,tmp)
			,(loop (cdr bdgs)
			    (cons (list bdg tmp) tmps)))
		    (cps g (cadr bdg) stack
		       (lambda (x)
			  `(let ((,tmp ,x))
			      ,(loop (cdr bdgs)
				  (cons (list (car bdg) tmp) tmps))))))))))
   
   (define (cps-labels bdgs body stack ck)
      (let ((frame (args->frame (map car bdgs))))
	 `(labels ,bdgs ,(cps g body (append frame stack) ck))))
   
   (define (cps-case exp clauses stack ck)
      (let ((k (gensym 'k))
	    (x (gensym 'x)))
	 `(let ((,k (lambda (,x) ,(ck x))))
	     ,(cps g exp stack
		 (lambda (e)
		    `(case ,e
			,@(map (lambda (c)
				  (match-case c
				     ((?vals . ?body)
				      `(,vals ,(cps g (normalize-exp* body) stack
						  (lambda (x) `(,k ,x)))))))
			   clauses)))))))
   
   (define (with-access? o)
      (when (symbol? o)
	 (string-prefix? "with-access::" (symbol->string! o))))
   
   (define (instantiate? o)
      (when (symbol? o)
	 (string-prefix? "instantiate::" (symbol->string! o))))
   
   (define (loop? s stack)
      (let ((c (assq s stack)))
	 (and (pair? c) (cdr c))))

   (define (args->frame l)
      (let loop ((l l)
		 (acc '()))
	 (cond
	    ((null? l) acc)
	    ((not (pair? l)) (cons (cons l #f) acc))
	    (else (loop (cdr l) (cons (cons (car l) #f) acc))))))

   (define (cps g::symbol x::obj stack::pair-nil ck::procedure)
      (match-case x
	 ((quote ?-)
	  (cps-value x stack ck))
	 ((? symbol?)
	  (cps-variable x stack ck))
	 ((atom ?a)
	  (cps-value a stack ck))
	 ((@ (? symbol?) (? symbol?))
	  (cps-value x stack ck))
	 ((yield ?v)
	  (cps-yield v stack ck))
	 ((-> ?o (and ?p (? symbol?)))
	  (cps-> o p stack ck))
	 ((lambda . ?-)
	  (cps-value x stack ck))
	 ((set! (and (? symbol?) ?v) ?exp)
	  (cps-set v exp stack ck))
	 ((set! (-> ?o (and ?p (? symbol?))) ?e)
	  (cps-set-> o p e stack ck))
	 ((if ?exp ?then ?otherwise)
	  (normalize-if (cps-if exp then otherwise stack ck)))
	 ((begin . ?exps)
	  (normalize-begin (cps-begin exps stack ck)))
	 ((let (and (? symbol?) ?loop) ?bdgs . ?body)
	  (normalize-loop (cps-loop loop bdgs body stack ck)))
	 ((let ?bdgs . ?body)
	  (normalize-let (cps-let 'let bdgs body stack ck)))
	 ((let* ?bdgs . ?body)
	  (normalize-let (cps-let 'let* bdgs body stack ck)))
;*       ((letrec ((?var (lambda (and (? list?) ?args) . ?body))) (?var . ?vals)) */
;*        (if (= (length args) (length vals))                          */
;* 	   (normalize-loop (cps-loop var (map list args vals) body))   */
;* 	   (normalize-let (cps-let 'letrec (cadr x) body))))           */
	 ((letrec ?bdgs . ?body)
	  (normalize-let (cps-let 'letrec (cadr x) body stack ck)))
	 ((letrec* ?bdgs . ?body)
	  (normalize-let (cps-let 'letrec* bdgs body stack ck)))
	 ((labels ?bdgs . ?body)
	  (cps-labels bdgs body stack ck))
	 ((case ?e . ?clauses)
	  (cps-case e clauses stack ck))
	 (((and ?w (? with-access?)) ?o ?p . ?body)
	  (cps-with-access w o p body stack ck))
	 (((and ?i (? instantiate?)) . ?p)
	  (cps-instantiate i p stack ck))
	 ((?fun . ?args)
	  (cps-app fun args stack ck))
	 (else
	  (error "cps" "form not supported" x))))

   (cps g x '() ck))

;*---------------------------------------------------------------------*/
;*    normalize-exp* ...                                               */
;*---------------------------------------------------------------------*/
(define (normalize-exp* exp*)
   (match-case exp*
      (() #unspecified)
      (((begin . ?-)) (car exp*))
      (else `(begin ,@exp*))))

;*---------------------------------------------------------------------*/
;*    normalize-begin ...                                              */
;*---------------------------------------------------------------------*/
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

;*---------------------------------------------------------------------*/
;*    normalize-if ...                                                 */
;*---------------------------------------------------------------------*/
(define (normalize-if x)
   (match-case x
      ((let ((?k (lambda (?v) ?v))) (if ?test (?k ?then) (?k ?otherwise)))
       `(if ,test ,then ,otherwise))
      ((let ((?k (lambda (?v) ?e))) (if ?test (?k ?then) (?k ?otherwise)))
       `((lamdbda (,v) ,e) (if ,test ,then ,otherwise)))
      (else
       x)))

;*---------------------------------------------------------------------*/
;*    normalize-loop ...                                               */
;*---------------------------------------------------------------------*/
(define (normalize-loop x)
   (let loop ((x x)
	      (tmps '()))
      (match-case x
	 ((let (?tmp) ?x)
	  (loop x (cons tmp tmps)))
	 ((let (and ?loop (? symbol?)) (?k . ?bindings) . ?body)
	  `(let ,loop (,k ,@(map (lambda (b t) (list (car b) (cadr t)))
			       bindings tmps))
		,@body))
	 (else
	  (if (null? tmps)
	      x
	      `(let* ,(reverse tmps) ,x))))))

;*---------------------------------------------------------------------*/
;*    normalize-let ...                                                */
;*---------------------------------------------------------------------*/
(define (normalize-let x)
   (let loop ((x x)
	      (tmps '()))
      (match-case x
	 ((let ((?tmp ?val)) (let ((?var ?tmp)) . ?nx))
	  (loop `(let ((,var ,val)) ,@nx) tmps))
	 ((let ((and ?tmp (?- ?-))) (and ?nx (let ?- . ?-)))
	  (loop nx (cons tmp tmps)))
	 ((let ?bindings (and ?nx (let ?- . ?-)))
	  (if (pair? tmps)
	      `(let* ,(reverse tmps) ,x)
	      x))
	 ((let (? symbol?) ?bindings . ?nx)
	  (if (null? tmps)
	      x
	      `(let* ,(reverse tmps) ,x)))
	 ((let ?bindings . ?nx)
	  (let loop ((bindings bindings)
		     (tmps tmps)
		     (nbindings '()))
	     (cond
		((null? bindings)
		 (let ((nx `(let ,(reverse nbindings) ,@nx)))
		    (if (null? tmps)
			nx
			`(let* ,(reverse tmps) ,nx))))
		((null? tmps)
		 `(let (,@(reverse nbindings) ,@bindings) ,@nx))
		(else
		 (let ((b (car bindings)))
		    (match-case b
		       ((?var (and (? symbol?) ?tmp))
			(let ((c (assq tmp tmps)))
			   (if (pair? c)
			       (loop (cdr bindings)
				  (filter (lambda (c)
					     (not (eq? (car c) tmp)))
				     tmps)
				  (cons b bindings))
			       (loop (cdr bindings)
				  tmps
				  (cons b bindings)))))
		       (else
			(loop (cdr bindings)
			   tmps
			   (cons b nbindings)))))))))
	 ((generator-yield ?g ?v (lambda ?args (and ?nx (let . ?-))))
	  `(generator-yield ,g ,v (lambda ,args ,(normalize-let nx))))
	 (else
	  (if (null? tmps)
	      x
	      `(let* ,(reverse tmps) ,x))))))

;*---------------------------------------------------------------------*/
;*    test-cps ...                                                     */
;*---------------------------------------------------------------------*/
(define (test-cps pp)
   
   (define *gencnt* 0)
   
   (define (gensym . prefix)
      (string->symbol (format "~a~a" (car prefix) *gencnt*)))
   
   (for-each (lambda (e)
		(set! *gencnt* 0)
		(print (car e))
		(let ((c (generator-cps (car e) 'g (lambda (x) x) gensym)))
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
	 (let loop ((x 1))
	    (generator-yield g x
	       (lambda (e2 v3)
		  (begin e2 (loop (+ x 1)))))))
	((let loop ((x (yield 1))) (loop (yield x)))
	 (generator-yield g 1
	    (lambda (e2 v3)
	       (let ((t1 e2))
		  (let loop ((x t1))
		     (generator-yield g x
			(lambda (e4 v5)
			   (loop e4))))))))
	((let loop ((n 0)) (if (< n 10) (loop (+ n (yield 5))) #f))
	 (let loop ((n 0))
	    (let ((k2 (lambda (x3) x3)))
	       (if (< n 10)
		   (generator-yield g 5
		      (lambda (e4 v5)
			 (k2 (loop (+ n e4)))))
		   (k2 #f)))))
	;; case
	((case (yield 1) ((a b) 2) (else 3))
	 (let ((k1 (lambda (x2) x2)))
	    (generator-yield g 1
	       (lambda (e3 v4)
		  (case e3 ((a b) (k1 2)) (else (k1 3)))))))
	)))
