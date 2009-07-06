;*=====================================================================*/
;*    serrano/trashcan/leval.scm                                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 26 08:30:11 1998                          */
;*    Last change :  Sat Dec 26 08:37:28 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    An interpreter with lambda (from M. Feeley's one).               */
;*=====================================================================*/

(module leval (main main))

;*---------------------------------------------------------------------*/
;*    defmacro ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (defmacro name+args . body)
   `(define-macro ,name+args ,@body))

;*---------------------------------------------------------------------*/
;*    repeat ...                                                       */
;*---------------------------------------------------------------------*/
(define-macro (repeat num exp)
   `(letrec ((rrun (lambda () ,exp))
             (loop (lambda (num res)
                      (if (<fx num 1)
                          res
                          (loop (-fx num 1) (rrun))))))
       (loop ,num (rrun))))

;*---------------------------------------------------------------------*/
;*    print-res ...                                                    */
;*---------------------------------------------------------------------*/
(define (print-res provided expected)
   (print "Bigloo results    provided [" provided
          "]   expected [" expected #\]))

;*---------------------------------------------------------------------*/
;*    Les environments ...                                             */
;*---------------------------------------------------------------------*/
(define the-global-environment '())

(define (errow a b c)
   (print "*** ERROW: " a #\Newline b " -- " c))

(define (local-assq obj alist)
   (let loop ((alist alist))
      (if (null? alist)
          #f
          (if (eq? (car (car alist)) obj)
              (car alist)
              (loop (cdr alist))))))


;*---------------------------------------------------------------------*/
;*    ewal ...                                                         */
;*    sexp x env --> sexp                                              */
;*---------------------------------------------------------------------*/
(define (ewal exp . env)
   (for-each (lambda (x)
		((comp-define (car x) (cdr x)) '()))
	     env)
   (meaning (comp exp '() #f) '()))

;*---------------------------------------------------------------------*/
;*    meaning ...                                                      */
;*---------------------------------------------------------------------*/
(define (meaning pre-compd-expression dynamic-env)
   (pre-compd-expression dynamic-env))

;*---------------------------------------------------------------------*/
;*    comp ...                                                         */
;*    s-exp x env x { t, f } --> (lambda () ...)                       */
;*    -------------------------------------------------------------    */
;*    La phase d'expansion a genere une syntaxe correcte. On n'a donc  */
;*    plus du tout a la tester maintenant.                             */
;*---------------------------------------------------------------------*/
(define (comp exp env tail?)
   (cond
      ((not (pair? exp))
       (let ((atom exp))
	  (cond
	     ((symbol? atom)
	      (comp-ref (variable atom env) tail?))
	     (else
	      (comp-cnst atom tail?)))))
      ((eq? (car exp) 'module)
       (lambda (dynamic-env) #f))
      ((eq? (car exp) 'quote)
       (let ((cnst (cadr exp)))
	  (comp-cnst cnst tail?)))
      ((eq? (car exp) 'if)
       (let ((si (cadr exp))
	     (alors (caddr exp))
	     (sinon (cadddr exp)))
	  (comp-if (comp si env #f)
		      (comp alors env tail?)
		      (comp sinon env tail?))))
      ((eq? (car exp) 'begin)
       (let ((rest (cdr exp)))
	  (comp-begin rest env)))
      ((eq? (car exp) 'define)
       (let ((var (cadr exp))
	     (val (caddr exp)))
	  (comp-define var (comp val '() #f))))
      ((eq? (car exp) 'set!)
       (let ((var (cadr exp))
	     (val (caddr exp)))
	  (comp-set (variable var env) (comp val env #f))))
      ((eq? (car exp) 'lambda)
       (let ((formals (cadr exp))
	     (body    (caddr exp)))
	  (comp-lambda formals
			  (comp body (extend-env! formals env) #t)
			  tail?)))
      ((not (pair? (car exp)))
       (let ((fun (car exp))
	     (args (cdr exp)))
	  (let ((actuals (map (lambda (a) (comp a env #f)) args)))
	     (cond
		((symbol? fun)
		 (let ((proc (variable fun env)))
		    (cond
		       ((global? proc)
			(comp-global-application proc
						    actuals
						    tail?))
		       (else
			(comp-application (comp-ref proc #f)
					     actuals
					     tail?)))))
		((procedure? fun)
		 (comp-compd-application fun actuals tail?))
		(else
		 (errow "ewal" "Not a procedure" fun))))))
      (else
       (let ((fun (car exp))
	     (args (cdr exp)))
	  (let ((actuals (map (lambda (a) (comp a env #f)) args))
		(proc    (comp fun env #f)))
	     (comp-application proc actuals tail?))))))

;*---------------------------------------------------------------------*/
;*    variable ...                                                     */
;*---------------------------------------------------------------------*/
(define (variable symbol env)
   (let ((offset (let loop ((env   env)
			    (count 0))
		    (cond
		       ((null? env)
			#f)
		       ((eq? (car env) symbol)
			count)
		       (else
			(loop (cdr env) (+fx count 1)))))))
      (if offset
	  offset
	  (let ((global (local-assq symbol the-global-environment)))
	     (if (not global)
		 `#(,symbol)
		 global)))))

;*---------------------------------------------------------------------*/
;*    global? ...                                                      */
;*---------------------------------------------------------------------*/
(define (global? variable)
   (pair? variable))

;*---------------------------------------------------------------------*/
;*    dynamic? ...                                                     */
;*---------------------------------------------------------------------*/
(define (dynamic? variable)
   (vector? variable))

;*---------------------------------------------------------------------*/
;*    comp-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (comp-ref variable tail?)
   (cond
      ((global? variable)
       (lambda (dynamic-env) (cdr variable)))
      ((dynamic? variable)
       (lambda (dynamic-env) (let ((global (local-assq (vector-ref variable 0)
						 the-global-environment)))
				(if (not global)
				    (errow "ewal"
					   "Unbound variable"
					   (vector-ref variable 0))
				    (cdr global)))))
      (else
       (case variable
	  ((0)
	   (lambda (dynamic-env) (car dynamic-env)))
	  ((1)
	   (lambda (dynamic-env) (cadr dynamic-env)))
	  ((2)
	   (lambda (dynamic-env) (caddr dynamic-env)))
	  ((3)
	   (lambda (dynamic-env) (cadddr dynamic-env)))
	  (else
	   (lambda (dynamic-env)
	      (do ((i 0 (+fx i 1))
		   (env dynamic-env (cdr env)))
		    ((=fx i variable) (car env)))))))))

;*---------------------------------------------------------------------*/
;*    comp-set ...                                                     */
;*---------------------------------------------------------------------*/
(define (comp-set variable value)
   (cond
      ((global? variable)
       (lambda (dynamic-env) (update! variable value dynamic-env)))
      ((dynamic? variable)
       (lambda (dynamic-env)
	  (let ((global (local-assq (vector-ref variable 0)
			      the-global-environment)))
	     (if (not global)
		 (errow "ewal"
			"Unbound variable"
			(vector-ref variable 0))
		 (update! global value dynamic-env)))))
      (else
       (case variable
	  ((0)
	   (lambda (dynamic-env) (set-car! dynamic-env
					   (value dynamic-env))))
	  ((1)
	   (lambda (dynamic-env) (set-car! (cdr dynamic-env)
					   (value dynamic-env))))
	  ((2)
	   (lambda (dynamic-env) (set-car! (cddr dynamic-env)
					   (value dynamic-env))))
	  ((3)
	   (lambda (dynamic-env) (set-car! (cdddr dynamic-env)
					   (value dynamic-env))))
	  (else
	   (lambda (dynamic-env)
	      (do ((i 0 (+fx i 1))
		   (env dynamic-env (cdr env)))
		    ((=fx i variable) (set-car! env
						(value dynamic-env))))))))))
	
;*---------------------------------------------------------------------*/
;*    comp-cnst ...                                                    */
;*---------------------------------------------------------------------*/
(define (comp-cnst cnst tail?)
   (lambda (dynamic-env) cnst))

;*---------------------------------------------------------------------*/
;*    comp-if ...                                                      */
;*---------------------------------------------------------------------*/
(define (comp-if test then sinon)
   (lambda (dynamic-env) (if (test dynamic-env)
			     (then dynamic-env)
			     (sinon dynamic-env))))

;*---------------------------------------------------------------------*/
;*    comp-begin ...                                                   */
;*---------------------------------------------------------------------*/
(define (comp-begin body env)
   (cond
      ((and (pair? body) (and (null? (cdr body))))
       ;; le cas degenere
       (let ((rest (comp (car body) env #t)))
	  (lambda (dynamic-env) (rest dynamic-env))))
      (else
       (let ((body (let loop ((rest body))
		      (cond
			 ((null? rest)
			  (errow "ewal" "Illegal form" body))
			 ((null? (cdr rest))
			  (cons (comp (car rest) env #t) '()))
			 (else
			  (cons (comp (car rest) env #f)
				(loop (cdr rest))))))))
	  (lambda (dynamic-env) (let _loop_ ((body body))
				   (if (null? (cdr body))
				       ((car body) dynamic-env)
				       (begin
					  ((car body) dynamic-env)
					  (_loop_ (cdr body))))))))))

;*---------------------------------------------------------------------*/
;*    init-the-global-environment! ...                                 */
;*---------------------------------------------------------------------*/
(define (linit-the-global-environment!)
   (if (pair? the-global-environment)
       'done
       ;; je ne peux pas utiliser de constante car quand cette fonction
       ;; sera appelle, je ne suis pas qu'elles soient initialisee.
       (set! the-global-environment (cons (cons #f #f) '()))))

;*---------------------------------------------------------------------*/
;*    comp-define ...                                                  */
;*    -------------------------------------------------------------    */
;*    On ne rajoute pas en tete car elle contient la definition de     */
;*    `the-module-environment'. On rajoute donc en deuxieme.           */
;*---------------------------------------------------------------------*/
(define (comp-define var val)
   (lambda (dynamic-env)
      (let ((cell (local-assq var the-global-environment)))
	 (if (pair? cell)
	     (begin
		(print "*** WARNING:bigloo:ewal\nredefinition of variable -- "
		       var)
		(update! cell val dynamic-env))
	     (begin
		(set-cdr! the-global-environment
			  (cons (car the-global-environment)
				(cdr the-global-environment)))
		(set-car! the-global-environment
			  (cons var (val dynamic-env)))
		var)))))

;*---------------------------------------------------------------------*/
;*    define-primitive! ...                                            */
;*    -------------------------------------------------------------    */
;*    Cette fonction est juste une forme abregee de la precedente, qui */
;*    construit le `(lambda () ...)' absent                            */
;*---------------------------------------------------------------------*/
(define (ldefine-primitive! var val)
   (set-cdr! the-global-environment
	     (cons (car the-global-environment)
		   (cdr the-global-environment)))
   (set-car! the-global-environment (cons var val)))

;*---------------------------------------------------------------------*/
;*      update! ...                                                    */
;*---------------------------------------------------------------------*/
(define (update! variable val dynamic-env)
   (set-cdr! variable (val dynamic-env))
   (car variable))

;*---------------------------------------------------------------------*/
;*    extend-env! ...                                                  */
;*---------------------------------------------------------------------*/
(define (extend-env! extend old-env)
   (let _loop_ ((extend extend))
      (cond
	 ((null? extend)
	  old-env)
	 ((not (pair? extend))
	  (cons extend old-env))
	 (else
	  (cons (car extend) (_loop_ (cdr extend)))))))

;*---------------------------------------------------------------------*/
;*    pair ...                                                         */
;*---------------------------------------------------------------------*/
(define (pair n l)
   (if (<fx n 0)
       (let loop ((n n)
		  (l l))
	  (cond
	     ((=fx -1 n)
	      #t)
	     ((not (pair? l))
	      #f)
	     (else
	      (loop (+fx 1 n) (cdr l)))))
       (let loop ((n n)
		  (l l))
	  (cond
	     ((=fx 0 n)
	      (null? l))
	     ((not (pair? l))
	      #f)
	     (else
	      (loop (-fx n 1) (cdr l)))))))
	      
;*---------------------------------------------------------------------*/
;*    comp-lambda ...                                                  */
;*---------------------------------------------------------------------*/
(define (comp-lambda formals body tail?)
   (cond
      ((null? formals)
       (lambda (dynamic-env)
	  (lambda ()
	     (body dynamic-env))))
      ((pair 1 formals)
       (lambda (dynamic-env)
	  (lambda (x)
	     (body (cons x dynamic-env)))))
      ((pair 2 formals)
       (lambda (dynamic-env)
	  (lambda (x y)
	     (body (cons x (cons y dynamic-env))))))
      ((pair 3 formals)
       (lambda (dynamic-env)
	  (lambda (x y z)
	     (body (cons x (cons y (cons z dynamic-env)))))))
      ((pair 4 formals)
       (lambda (dynamic-env)
	  (lambda (x y z t)
	     (body (cons x (cons y (cons z (cons z dynamic-env))))))))
      ((symbol? formals)
       (lambda (dynamic-env)
	  (lambda x
	     (body (cons x dynamic-env)))))
      ((pair -1 formals)
       (lambda (dynamic-env)
	  (lambda (x . y)
	     (body (cons x (cons y dynamic-env))))))
      ((pair -2 formals)
       (lambda (dynamic-env)
	  (lambda (x y . z)
	     (body (cons x (cons y (cons z dynamic-env)))))))
      ((pair -3 formals)
       (lambda (dynamic-env)
	  (lambda (x y z . t)
	     (body (cons x (cons y (cons z (cons z dynamic-env))))))))
      (else
       (lambda (dynamic-env)
	  (lambda x
	     (let ((new-env (let _loop_ ((actuals x)
					 (formals formals))
			       (cond
				  ((null? formals)
				   (if (not (null? actuals))
				       (errow "ewal"
					      "Too many arguments provided"
					      actuals)
				       dynamic-env))
				  ((null? actuals)
				   (errow "ewal"
					  "Too fee arguments provided"
					  formals))
				  ((not (pair? formals))
				   (cons actuals dynamic-env))
				  (else
				   (cons (car actuals)
					 (_loop_ (cdr actuals)
						 (cdr formals))))))))
		(body new-env)))))))

;*---------------------------------------------------------------------*/
;*    comp-global-application ...                                      */
;*---------------------------------------------------------------------*/
(define (comp-global-application proc actuals tail?)
   (case (length actuals)
      ((0)
       (lambda (dynamic-env) ((cdr proc))))
      ((1)
       (lambda (dynamic-env) ((cdr proc) ((car actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) ((cdr proc) ((car actuals) dynamic-env)
					 ((cadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) ((cdr proc) ((car actuals) dynamic-env)
					 ((cadr actuals) dynamic-env)
					 ((caddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) ((cdr proc) ((car actuals) dynamic-env)
					 ((cadr actuals) dynamic-env)
					 ((caddr actuals) dynamic-env)
					 ((cadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env)
	  (apply (cdr proc) (map (lambda (v) (v dynamic-env)) actuals))))))

;*---------------------------------------------------------------------*/
;*    comp-compd-application ...                                       */
;*---------------------------------------------------------------------*/
(define (comp-compd-application proc actuals tail?)
   (case (length actuals)
      ((0)
       (lambda (dynamic-env) (proc)))
      ((1)
       (lambda (dynamic-env) (proc ((car actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) (proc ((car actuals) dynamic-env)
				   ((cadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) (proc ((car actuals) dynamic-env)
				   ((cadr actuals) dynamic-env)
				   ((caddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) (proc ((car actuals) dynamic-env)
				   ((cadr actuals) dynamic-env)
				   ((caddr actuals) dynamic-env)
				   ((cadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env)
	  (apply proc (map (lambda (v) (v dynamic-env)) actuals))))))
   
;*---------------------------------------------------------------------*/
;*    comp-application ...                                             */
;*---------------------------------------------------------------------*/
(define (comp-application proc actuals tail?)
   (case (length actuals)
      ((0)
       (lambda (dynamic-env) ((proc dynamic-env))))
      ((1)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((car actuals) dynamic-env))))
      ((2)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((car actuals) dynamic-env)
			      ((cadr actuals) dynamic-env))))
      ((3)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((car actuals) dynamic-env)
			      ((cadr actuals) dynamic-env)
			      ((caddr actuals) dynamic-env))))
      ((4)
       (lambda (dynamic-env) ((proc dynamic-env)
			      ((car actuals) dynamic-env)
			      ((cadr actuals) dynamic-env)
			      ((caddr actuals) dynamic-env)
			      ((cadddr actuals) dynamic-env))))
      (else
       (lambda (dynamic-env)
	  (apply (proc dynamic-env) (map (lambda (v) (v dynamic-env))
					   actuals))))))

;*---------------------------------------------------------------------*/
;*    Les inits                                                        */
;*---------------------------------------------------------------------*/
(linit-the-global-environment!)

(ldefine-primitive! '+ (lambda (x y) (+fx x y)))
(ldefine-primitive! '- (lambda (x y) (-fx x y)))
(ldefine-primitive! '< (lambda (x y) (<fx x y)))
(ldefine-primitive! '>= (lambda (x y) (>=fx x y)))
(ldefine-primitive! 'eq? eq?)
(ldefine-primitive! 'car car)
(ldefine-primitive! 'cdr cdr)
(ldefine-primitive! 'cons cons)
(ldefine-primitive! 'null? null?)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (bench)
   (ewal '(tak 20 10 3)))

(define (run num)
   (ewal '(define tak (lambda (x y z)
			 (if (>= y x)
			     z
			     (tak (tak (- x 1) y z)
				  (tak (- y 1) z x)
				  (tak (- z 1) x y))))))

   (let ((res (repeat num (bench))))
      (print-res res 4)))

(define (do-bench num)
   (if (>fx num 0)
       (run num)))

(define (main argv)
   (let ((num (if (pair? (cdr argv))
		  (string->integer (cadr argv))
		  10)))
      (do-bench num)))
