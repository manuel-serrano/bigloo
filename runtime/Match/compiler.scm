;*---------------------------------------------------------------------*/
;*    .../prgm/project/bigloo/bigloo/runtime/Match/compiler.scm        */
;*                                                                     */
;*    Author      :  Jean-Marie Geffroy                                */
;*    Creation    :  Thu Jan 14 10:29:50 1993                          */
;*    Last change :  Sun Aug 25 09:16:40 2019 (serrano)                */
;*                                                                     */
;*---------------------------------------------------------------------*/
;*    A hand-written pattern "compiler"                                */
;*---------------------------------------------------------------------*/
;*    References:                                                      */
;*      [1] C.Queinnec & J.M. Geffroy, "Pattern matching in Scheme"    */
;*          ( tech. report )                                           */
;*      [2] C.Queinnec & J.M. Geffroy, "Partial Evaluation applied to  */
;*          Symbolic Pattern Matching with intelligent backtrack"      */
;*          (Workshop on Static Analysis, 92, Bordeaux)                */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Important: we deliberately choosed to allow "non-linear patterns"*/
;*    (in which variables may appear more than once). We test          */
;*    the equality between subparts of the datum trough the EQ?        */
;*    predicate.                                                       */
;*---------------------------------------------------------------------*/

(module __match_compiler
   
   (export  (pcompile f))

   (import  __error
	    __match_s2cfun
	    __match_descriptions
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
	    __evenv))
	    
(define (pcompile f)
   (let ((s (jim-gensym "E-")))
   `(lambda (,s)
       ,(compile f s r-init m-init
		 k-init z-init d-init))))

(define (compile f e r m k z d)
   (cond
      ((more-precise? d f) (k r z d))
      ((compatible? d f)
       (case (car f)
	  ((any)      (compile-any e r m k z d))
	  ((check)    (compile-check (cadr f) e r m k z d))
	  ((quote)    (compile-quote f e r m k z d))
	  ((var)      (compile-var (cadr f) e r m k z d))
	  ((not)      (compile-not (cadr f) e r m k z d)) 
	  ((or)       (compile-or (cadr f) (caddr f) e r m k z d))
	  ((tagged-or)(compile-tagged-or (cadr f) (caddr f) (cadddr f)
		       e r m k z d))
	  ((and)      (compile-and (cadr f) (caddr f) e r m k z d))
	  ((cons)     (compile-cons (cadr f) (caddr f) e r m k z d))
	  ((times)     (compile-times  
		       (cadr f) (caddr f) (cadddr f) e r m k z d))
	  ((tree)     (compile-tree
		       (cadr f) (caddr f) (cadddr f) e r m k z d))
	  ((hole)     (compile-hole (cadr f) e r m k z d))
	  ;; Extension to vectors...
	  ((vector-begin) (compile-vector-begin (cadr f) (caddr f)
						e r m k z d))
	  ((vector-end) (compile-vector-end e r m k z d))
	  ((vector-any) (compile-vector-any e r m k z d))
	  ((vector-cons) (compile-vector-cons (cadr f) (caddr f)
					      e r m k z d))
	  ((vector-times) (compile-vector-times (cadr f) (caddr f) (cadddr f)
						e r m k z d))
	  ((struct-pat) (compile-struct-pat f e r m k z d))
	  (else       (wrong "Unrecognized pattern" f))))
      (#t (z d) ) ))
   

(define (compile-any e r m k z d)
  (k r z d)) 

;*---------------------------------------------------------------------*/
;*    check doesn't allow to extend the description since we do not    */
;*    restrict the predicates that can be used.                        */
;*---------------------------------------------------------------------*/
(define (compile-check p e r m k z d)
  `(if (,p ,e)
       ,(k r z d)  
       ,(z d)))

(define (build-atom-equality-test e const)
   (cond
      ;; John Malecki improvement (15 aug 2003)
      ((null? const)
       `(null? ,e))
      ((or (fixnum? const)
	   (char? const)
	   (boolean? const)
	   (symbol? const))
       `(eq? ,e (quote ,const)))
      ((flonum? const)
       `(and (flonum? ,e) (=fl ,e ,const)))
      ((number? const)
       `(and (number? ,e) (= ,e ,const)))
      ((string? const)
       `(and (string? ,e) (string=? ,e ,const)))
      ((null? const)
       `(null? ,e))
      (else
       `(equal? ,e (quote ,const)))))

(define (compile-quote f e r m k z d)
   (build-if (build-atom-equality-test e (cadr f))
	     (k r z `(quote ,(cadr f)))
	     (z (pattern-minus d f))))

(define (compile-not f e r m k z d)
   (compile f e r m (lambda (r2 z2 d2) (z d2)) 
	    (lambda (d2) (k r z d2))
	    d) )

;*---------------------------------------------------------------------*/
;*    Both branches of an or pattern must contain the same variables   */
;*---------------------------------------------------------------------*/

(define (compile-or f1 f2 e r m k z d)
   (let* ((*k* (jim-gensym "KAP-"))
	  (*vars* (pattern-variables f1))
	  (*call* `(,*k* ,@*vars*))
	  (success-form (k (extend* r *vars*) z d)))

      (if (or (eq? success-form #f)
	      (equal? (cdr *call*) (cdr success-form))
	      (isDirectCall? success-form))
	  (compile f1 e r m 
		    (lambda (r z d) success-form)
		    (lambda (d) 
		       (compile f2 e r m
				(lambda (r z d) success-form)
				z d) )
		    d)
	  (let ((res (compile f1 e r m 
			      (lambda (r z d) *call*)
			      (lambda (d) 
				 (compile f2 e r m
					  (lambda (r z d) *call*)
					  z d) )
			      d)))
	     (if res 
		 `(labels ((,*k* ,*vars* ,success-form))
		     ,res)
		 res) ) ) ) )

;*---------------------------------------------------------------------*/
;*    We try to avoid (labels ((g (x) (f x))) ...)                     */
;*---------------------------------------------------------------------*/
(define (isDirectCall? e)
   (and (pair? e)
	(let ((pr (car e)))
	   (and (symbol? pr)
		(let ((s (symbol->string pr)))
		   (and (>fx (string-length s) 3)
			(let ((s (substring s 0 3)))
			   (or (string=? s "KAP")
			       (string=? s "TAG")))))))))

;*---------------------------------------------------------------------*/
;*    Branches of a tagged-or do not need to contain the same vars     */
;*---------------------------------------------------------------------*/
(define (compile-tagged-or f1 t1 f2 e r m k z d)
   (let ((*vars* (pattern-variables f1)))
      (compile f1 e r m 
	       (lambda (r z c)
		  `(,t1 ,@*vars*))
	       (lambda (d) 
		  (compile f2 e r m k z d) )
	       d)))

(define (compile-and f1 f2 e r m k z c)
   (if (compatible? f1 f2)
       (compile f1 e r m
		(lambda (r2 z2 c2)
                   (compile f2 e r2 m k z2 c2 ) )
		z c )
       (z c) )  )

;*---------------------------------------------------------------------*/
;*    Non linear patterns                                              */
;*---------------------------------------------------------------------*/
(define (compile-var n e r m k z c)
   (if (boundp n r)
       (build-if `(eq? ,n ,e)
		 (k r z (pattern-plus c (list 'var n)))
		 (z (pattern-minus c (list 'var n))) )
       (let ((body (k (extend-alist r n e) z
		      (pattern-plus c (list 'var n)))))
	  (if (> (count-occurrences n body 0) 1)
	      `(let ((,n ,e))
		  ,body)
	      (unfold n e body) ) ) ) ) 

(define (count-occurrences s e acc) 
   (if (null? e)
       acc
       (if (atom? e)
	   (if (eq? s e)
	       (+ acc 1)
	       acc)
	   (if (pair? e)
	       (if (eq? (car e) 'quote)
		   acc
		   (+ (count-occurrences s (car e) acc)
		      (count-occurrences s (cdr e) acc)) ) ) ) ) )

(define-inline (caddddr x) (cadr (cdddr x)))

       

;*---------------------------------------------------------------------*/
;*    La version suivante de cons ne permet pas de compiler            */
;*    correctement les filtres tree: elle engendre un code tail-rec    */
;*---------------------------------------------------------------------*/

(define (compile-cons f1 f2 e r m k z c)
   (if (isCons? c)
       (succes-cons f1 f2 e r m k z c)
       `(if (pair? ,e)
	    ,(succes-cons f1 f2 e r m k z c) 
	    ,(z (pattern-minus c (list 'cons '(any) '(any)))))))

(define (succes-cons f1 f2 e r m k z c)
   (let* ((*car* (jim-gensym "CAR-"))
	  (*cdr* (jim-gensym "CDR-"))
	  (body (compile 
		 f1 *car* r m
		 (lambda (r2 z2 c2)
		    (compile f2 *cdr* r2 m
			     (lambda (r3 z3 c3) 
				(k r3 z3 (list 'cons c2 c3) ) )
			     (lambda (c3) (z (list 'cons c2 c3)))
			     (pattern-cdr c)) )
		 (lambda (c2) (z (list 'cons c2 (pattern-cdr c))))
		 (pattern-car c) ) ) )
      (build-let *car* *cdr* e body) ) )

;*---------------------------------------------------------------------*/
;*    Instanciate-try may be re-written in order to avoid generating   */
;*    more than X specialized versions of a recursive pattern: we just */
;*    have to generate a "generic' version of the TIMES pattern, with a */
;*    (any) description, and to update a counter of instances.         */
;*---------------------------------------------------------------------*/
;*---------------------------------------------------------------------*/
;*    In order to compile recursive patterns of the form               */
;*    (tree L (cons (hole L) (hole L)) <stg>)                          */
;*    we should differenciate the occurrences of HOLE, and give them   */
;*    distinct continuations.                                          */
;*---------------------------------------------------------------------*/

(define (compile-times n f1 f2 e r m k0 z0 d0)

   (let ((F-env '()) (D-env '()))
      (letrec
	    ((instanciate-try
	      (lambda (r m k z d)
		 (let ((tmp (look-for-descr d D-env)))
		    (if tmp 
			(cadr tmp)
			(let ((g (jim-gensym "G-"))
			      (try (jim-gensym "TRY-")))
			   (set! D-env (cons (list d try) D-env))
			   (let ((new-def (list try
					`(letrec
					       ((,try 
						 (lambda (,g)
						    ,(compile
						      f2 g r m k
						      (lambda (d2)
							 (compile f1 g r
								  (extend
								   m n
								   instanciate-try)
								  k z d2) ) ;;; '(any)
						      d) ) ) ) ) ) ) ) 
			      (set! F-env (cons new-def F-env) )
			      try) ) ) ) ) ) )

	 
	 (let ((res-body (instanciate-try
			  r m
			  (lambda (r z d) (k0 r z d0))
			  (lambda (d) (z0 d0))
			  d0)))
	    `(letrec ,(map caadadr F-env) (,res-body  ,e) ) ) )  ) )


(define (compile-hole n e r m k z d)
   `(,((m n) r m k z d) ,e) )

(define (compile-tree n f1 f2 e r m k0 z0 d0)
   (wrong "Tree not yet allowed"))

;*---------------------------------------------------------------------*/
;*    Extension to vectors                                             */
;*---------------------------------------------------------------------*/
;*---------------------------------------------------------------------*/
;*    Description: (vector lg [d1 d2 ...dlg]) or (not (vector))        */
;*---------------------------------------------------------------------*/

(define (compile-vector-begin lgmin f e r m k z d)
   (if (isVector? d)
       (if (>= (cadr d) lgmin)
	   ((compile
	     f e r m k z d) 0)
	   `(if (>= (vector-length ,e) ,lgmin)
		,((compile
		   f e r m k z d) 0)
		,(z d)))
       `(if (vector? ,e)
	    (if (>= (vector-length ,e) ,lgmin)
		,((compile
		   f e r m k z
		   `(vector ,lgmin
			    ,(make-vector lgmin '(any))) ) 0)
		,(z `(vector 0
			     ,(make-vector 0 '(any)))))
	    ,(z (pattern-plus d '(not (vector)))))))

;*---------------------------------------------------------------------*/
;*    We know that the datum is a vector of the desired minimal        */
;*    length, and the descr. is (vector ...)                           */
;*---------------------------------------------------------------------*/
(define (compile-vector-cons f1 f2 e r m k z d)
   (lambda (i)
      (if (>= i (vector-length (caddr d)))
	  (set-car! (cddr d)
		    (extend-vector (caddr d) (+ i 1) '(any)))
	  #t)
      (compile f1 `(vector-ref ,e ,i) r m
	       (lambda (r z1 d1)
		  ((compile f2 e r m k z (vector-plus d i d1))
		   (+ i 1)))
	       (lambda (d1)
		  (z (vector-plus d i d1)))
	       (vector-ref (caddr d) i))))

(define (compile-vector-end e r m k z d)
   (lambda (i)
      (build-if `(eq? ,i (vector-length ,e))
		(k r z d)
		(z d))))

(define (compile-vector-any e r m k z d)
   (lambda (i)
      (k r z d)))

(define (compile-vector-times n f1 f2 e r m k0 z0 d0)
   (wrong "Not yet allowed"))

;*---------------------------------------------------------------------*/
;*    Pattern matching on structures                                   */
;*---------------------------------------------------------------------*/
(define (compile-struct-pat f e r m k z d)
   (let* ((nom (cadr f))
	  (pred (caddr f))
	  (p*  (cdddr f))
	  (*k* (jim-gensym "KAP-"))
	  (*vars* (pattern-variables f))
	  (*call* `(,*k* ,@*vars*))
	  (success-form (k (extend* r *vars*) z d))
	  (failure-form (z d))
	  (indexes (integers 0 (- (length p*) 1))))
      (build-if `(,pred
		  ,e)
		(compile* p* indexes e r m
				    (lambda (r z d) success-form)
				    (lambda (d) failure-form)
				    '(any))
		failure-form)))

(define (compile* p* i* e r m k z d)
   (if (null? p*)
       (k r z d)
       (compile (car p*) `(struct-ref ,e ,(car i*)) r m
		(lambda (rr zz dd)
		   (compile* (cdr p*) (cdr i*) e rr m k z '(any)))
		z
		'(any))))

;*---------------------------------------------------------------------*/
(define (look-for-descr d D-env)
   (if (null? D-env)
       #f
       (if (equal? (caar D-env) d)
	   (car D-env)
	   (look-for-descr d (cdr D-env)))))


(define (k-init r z d) #t)

(define (z-init d) #f)

(define d-init '(any))

;*---------------------------------------------------------------------*/
;*    We do not need anything better...                                */
;*---------------------------------------------------------------------*/
(define r-init '())

(define (extend-alist l pt im)
   `((,pt . ,im) ,@l))

(define (alistlookup env pt)
   (if (assq pt env)
       (cdr (assq pt env))
       'unbound))

(define (extend* r v*)
   (if (null? v*)
       r
       (extend-alist (extend* r (cdr v*))
	       (car v*)

	       'dummy)))

(define (m-init n)  
   (lambda (e r k z c)
      (wrong "No current repetition named" n) ) )

(define (boundp pt env)
   (not (eqv? (alistlookup env pt) 'unbound)))

;*---------------------------------------------------------------------*/
;*    Miscellaneous                                                    */
;*---------------------------------------------------------------------*/
(define (wrong . args)
   (error "Incorrect pattern: " args " *** "))

(define (caadadr x) (car (cadadr x)))


(define (extend fn pt im)
   (lambda (x)
      (if (eq? x pt) im (fn x))))

(define (build-if tst then else)
   (cond
      ((eq? tst #t) then)
      ((eq? tst #f) else)
      ((and (eq? then #t)
	    (eq? else #f))
       tst)
      ((and (eq? then #f)
	    (eq? else #t))
       `(not ,tst))
      (else `(if ,tst ,then ,else))))

(define (build-let *car* *cdr* e body)
   (if (> (count-occurrences *car* body 0) 1)
       (if (> (count-occurrences *cdr* body 0) 1)
	   `(let ((,*car* (car ,e)) (,*cdr* (cdr ,e)))
	       ,body )
	   `(let ((,*car* (car ,e)))
	       ,(unfold *cdr* `(cdr ,e) body )))
       (if (> (count-occurrences *cdr* body 0) 1)
	   `(let ((,*cdr* (cdr ,e)))
	       ,(unfold *car* `(car ,e) body) )
	   (unfold *cdr* `(cdr ,e)
		   (unfold *car* `(car ,e) body ) ) ) ) )

;*---------------------------------------------------------------------*/
;*    (unfold s v e) replaces s by v in e                              */
;*---------------------------------------------------------------------*/
(define (unfold s v e)
   (if (null? e)
       '()
       (if (atom? e)
	   (if (eq? e s) v e)
	   (if (pair? e)
	       (if (eq? (car e) 'quote)
		   e
		   (let ((f (car e)) (args (cdr e)))
		      `(,(unfold s v f) . ,(unfold s v args) ) ) ) ) ) ) )

(define (integers from to)
   (if (> from to)
       '()
       (cons from (integers (+ from 1) to))))
