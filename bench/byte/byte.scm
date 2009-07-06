(module beval (main main))
;*=====================================================================*/
;*    serrano/diffusion/article/sua/bench/prologue/prologue.bigloo     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun  1 11:05:12 1995                          */
;*    Last change :  Thu Jun  1 14:37:08 1995 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Le prologue de Bigloo1.7                                         */
;*=====================================================================*/

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
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (do-bench -1))
;*---------------------------------------------------------------------*/
;*    serrano/diffusion/article/sua/bench/beval/beval.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Feb 10 08:35:54 1993                          */
;*    Last change :  Thu Jun  1 16:21:06 1995 (serrano)                */
;*                                                                     */
;*    Un test d'evaluateur                                             */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Les environments ...                                             */
;*---------------------------------------------------------------------*/
(define the-global-environment '())

(define (errow a b c)
   (print "*** ERROW: " a #\Newline b " -- " c))

;*---------------------------------------------------------------------*/
;*    ewal ...                                                         */
;*    sexp x env --> sexp                                              */
;*---------------------------------------------------------------------*/
(define (ewal exp . env)
   (for-each (lambda (x)
		((compile-define (car x) (cdr x)) '()))
	     env)
   (meaning (compile exp '() #f) '()))

;*---------------------------------------------------------------------*/
;*    meaning ...                                                      */
;*---------------------------------------------------------------------*/
(define (meaning byte-code runtime-env)
   (case (car byte-code)
      ((-2) ;; null
       '())
      ((-3) ;; faux
       #f)
      ((-4) ;; vrai
       #t)
      ((-5) ;; 1
       1)
      ((-1) ;; les constantes
       (cdr byte-code))
      ((0) ;; la premiere variable de l'env
       (car runtime-env))
      ((1) ;; la deuxieme
       (cadr runtime-env))
      ((2) ;; la troisieme
       (caddr runtime-env))
      ((3) ;; la quatrieme
       (cadddr runtime-env))
      ((4) ;; une variable qui est loin dans l'environment
       (do ((i 0 (+fx i 1))
	    (env runtime-env (cdr env)))
	     ((=fx i (cdr byte-code)) (car env))))
      ((5) ;; une variable globale
       (cdr (cdr byte-code)))
      ((6) ;; les variables globales indefinies
       (let ((global (assq (vector-ref (cdr byte-code) 0)
			   the-global-environment)))
	  (if (not global)
	      (error "ewal"
		     "Unbound variable"
		     (vector-ref (cdr byte-code) 0))
	      (cdr global))))
      ((7) ;; l'affectation de la premiere variable de l'env
       (set-car! runtime-env (meaning (cdr byte-code) runtime-env)))
      ((8) ;; l'affectation de la deuxieme variable de l'env
       (set-car! (cdr runtime-env) (meaning (cdr byte-code) runtime-env)))
      ((9) ;; l'affectation de la troisieme variable de l'env
       (set-car! (cddr runtime-env) (meaning (cdr byte-code) runtime-env)))
      ((10) ;; l'affectation de la quatrieme variable de l'env
       (set-car! (cdddr runtime-env) (meaning (cdr byte-code) runtime-env)))
      ((11) ;; l'affectation de la nieme variable de l'env
       (do ((i 0 (+fx i 1))
	    (env runtime-env (cdr env)))
	     ((=fx i (cadr byte-code))
	      (set-car! env (meaning (cddr byte-code) runtime-env)))))
      ((12) ;; l'affectation des variables globales
       (my-update-global! (cadr byte-code) (cddr byte-code) runtime-env))
      ((13) ;; l'affectation des variables globales non definies
       (let ((global (assq (vector-ref (cadr byte-code) 0)
			   the-global-environment)))
	  (if (not global)
	      (error "ewal"
		     "Unbound variable"
		     (vector-ref (cadr byte-code) 0))
	      (my-update-global! global (cddr byte-code) runtime-env))))
      ((14) ;; if
       (if (meaning (cadr byte-code) runtime-env)
	   (meaning (caddr byte-code) runtime-env)
	   (meaning (cdddr byte-code) runtime-env)))
      ((15) ;; begin
       (let _loop_ ((body (cdr byte-code)))
	  (if (null? (cdr body))
	      (meaning (car body) runtime-env)
	      (begin
		 (meaning (car body) runtime-env)
		 (_loop_ (cdr body))))))
      ((16) ;; define
       (let ((var (cadr byte-code))
	     (val (cddr byte-code)))
	  (let ((cell (assq var the-global-environment)))
	     (if (pair? cell)
		 (begin
		    (print
		     "*** WARNING:bigloo:ewal\nredefinition of variable -- "
		     var)
		    (my-update-global! cell val runtime-env))
		 (begin
		    (set-cdr! the-global-environment
			      (cons (car the-global-environment)
				    (cdr the-global-environment)))
		    (set-car! the-global-environment
			      (cons var (meaning val runtime-env)))
		    var)))))
      ((17) ;; lambda-0
       (lambda ()
	  (meaning (cdr byte-code) runtime-env)))
      ((18) ;; lambda-1
       (lambda (x)
	  (meaning (cdr byte-code) (cons x runtime-env))))
      ((19) ;; lambda-2
       (lambda (x y)
	  (meaning (cdr byte-code) (cons x (cons y runtime-env)))))
      ((20) ;; lambda-3
       (lambda (x y z)
	  (meaning (cdr byte-code)
		   (cons x (cons y (cons z runtime-env))))))
      ((21) ;; lambda-4
       (lambda (x y z)
	  (meaning (cdr byte-code)
		   (cons x (cons y (cons z (cons z runtime-env)))))))
      ((22) ;; lambda--1
       (lambda x
	  (meaning (cdr byte-code)
		   (cons x runtime-env))))
      ((23) ;; lambda--2
       (lambda (x . y)
	  (meaning (cdr byte-code)
		   (cons x (cons y runtime-env)))))
      ((24) ;; lambda--3
       (lambda (x y . z)
	  (meaning (cdr byte-code)
		   (cons x (cons y (cons z runtime-env))))))
      ((25) ;; lambda--4
       (lambda (x y z . t)
	  (meaning (cdr byte-code)
		   (cons x (cons y (cons z (cons z runtime-env)))))))
      ((26) ;; lambda-n
       (lambda x
	  (let ((new-env (let _loop_ ((actuals x)
				      (formals (cadr byte-code)))
			    (cond
			       ((null? formals)
				(if (not (null? actuals))
				    (error "ewal"
					   "Too many arguments provided"
					   actuals)
				    runtime-env))
			       ((null? actuals)
				(error "ewal"
				       "Too fee arguments provided"
				       formals))
			       ((not (pair? formals))
				(cons actuals runtime-env))
			       (else
				(cons (car actuals)
				      (_loop_ (cdr actuals)
					      (cdr formals))))))))
	     (meaning (cddr byte-code) new-env))))
      ((27) ;; app-0
       ((cdr (cadr byte-code))))
      ((28) ;; app-1
       ((cdr (cadr byte-code)) (meaning (caddr byte-code) runtime-env)))
      ((29) ;; app-2
       ((cdr (cadr byte-code)) (meaning (caddr byte-code) runtime-env)
			       (meaning (cadddr byte-code) runtime-env)))
      ((30) ;; app-3
       ((cdr (cadr byte-code)) (meaning (caddr byte-code) runtime-env)
			       (meaning (cadddr byte-code) runtime-env)
			       (meaning (cadddr (cdr byte-code)) runtime-env)))
      ((31) ;; app-4
       ((cdr (cadr byte-code)) (meaning (caddr byte-code) runtime-env)
			       (meaning (cadddr byte-code) runtime-env)
			       (meaning (cadddr (cdr byte-code)) runtime-env)
			       (meaning (cadddr (cddr byte-code)) runtime-env)))
      ((32) ;; app-n
       (apply (cdr (cadr byte-code)) (map (lambda (v)
					     (meaning v runtime-env))
					  (cddr byte-code))))
      ((33) ;; app-0
       ((cadr byte-code)))
      ((34) ;; app-1
       ((cadr byte-code) (meaning (cddr byte-code) runtime-env)))
      ((35) ;; app-2
       ((cadr byte-code) (meaning (caddr byte-code) runtime-env)
			 (meaning (cadddr byte-code) runtime-env)))
      ((36) ;; app-3
       ((cadr byte-code) (meaning (caddr byte-code) runtime-env)
			 (meaning (cadddr byte-code) runtime-env)
			 (meaning (cadddr (cdr byte-code)) runtime-env)))
      ((37) ;; app-4
       ((cadr byte-code) (meaning (caddr byte-code) runtime-env)
			 (meaning (cadddr byte-code) runtime-env)
			 (meaning (cadddr (cdr byte-code)) runtime-env)
			 (meaning (cadddr (cddr byte-code)) runtime-env)))
      ((38) ;; app-n
       (apply (cadr byte-code) (map (lambda (v)
				       (meaning v runtime-env))
				    (cddr byte-code))))
      ((39) ;; app-0
       ((meaning (cadr byte-code) runtime-env)))
      ((40) ;; app-1
       ((meaning (cadr byte-code) runtime-env)
	(meaning (caddr byte-code) runtime-env)))
      ((41) ;; app-2
       ((meaning (cadr byte-code) runtime-env)
	(meaning (caddr byte-code) runtime-env)
	(meaning (cadddr byte-code) runtime-env)))
      ((42) ;; app-3
       ((meaning (cadr byte-code) runtime-env)
	(meaning (caddr byte-code) runtime-env)
	(meaning (cadddr byte-code) runtime-env)
	(meaning (cadddr (cdr byte-code)) runtime-env)))
      ((43) ;; app-4
       ((meaning (cadr byte-code) runtime-env)
	(meaning (caddr byte-code) runtime-env)
	(meaning (cadddr byte-code) runtime-env)
	(meaning (cadddr (cdr byte-code)) runtime-env)
	(meaning (cadddr (cddr byte-code)) runtime-env)))
      ((44) ;; app-n
       (apply (meaning (cadr byte-code) runtime-env)
	      (map (lambda (v)
		      (meaning v runtime-env))
		   (cddr byte-code))))))
       
;*---------------------------------------------------------------------*/
;*    compile ...                                                      */
;*    s-exp x env x { t, f } --> (lambda () ...)                       */
;*    -------------------------------------------------------------    */
;*    La phase d'expansion a genere une syntaxe correcte. On n'a donc  */
;*    plus du tout a la tester maintenant.                             */
;*---------------------------------------------------------------------*/
(define (compile exp env tail?)
   (cond
      ((not (pair? exp))
       (let ((atom exp))
	  (cond
	     ((symbol? atom)
	      (compile-ref (variable atom env) tail?))
	     (else
	      (compile-cnst atom tail?)))))
      ((eq? (car exp) 'quote)
       (let ((cnst (cadr exp)))
	  (compile-cnst cnst tail?)))
      ((eq? (car exp) 'if)
       (let ((si (cadr exp))
	     (alors (caddr exp))
	     (sinon (cadddr exp)))
	  (compile-if (compile si env #f)
		      (compile alors env tail?)
		      (compile sinon env tail?))))
      ((eq? (car exp) 'begin)
       (let ((rest (cdr exp)))
	  (compile-begin rest env)))
      ((eq? (car exp) 'define)
       (let ((var (cadr exp))
	     (val (caddr exp)))
	  (compile-define var (compile val '() #f))))
      ((eq? (car exp) 'set!)
       (let ((var (cadr exp))
	     (val (caddr exp)))
	  (compile-set (variable var env) (compile val env #f))))
      ((eq? (car exp) 'lambda)
       (let ((formals (cadr exp))
	     (body    (caddr exp)))
	  (compile-lambda formals
			  (compile body (my-extend-env formals env) #t)
			  tail?)))
      ((not (pair? (car exp)))
       (let ((fun (car exp))
	     (args (cdr exp)))
	  (let ((actuals (map (lambda (a) (compile a env #f)) args)))
	     (cond
		((symbol? fun)
		 (let ((proc (variable fun env)))
		    (cond
		       ((global? proc)
			(compile-global-application proc
						    actuals
						    tail?))
		       (else
			(compile-application (compile-ref proc #f)
					     actuals
					     tail?)))))
		((procedure? fun)
		 (compile-compiled-application fun actuals tail?))
		(else
		 (errow "ewal" "Not a procedure" fun))))))
      (else
       (let ((fun (car exp))
	     (args (cdr exp)))
	  (let ((actuals (map (lambda (a) (compile a env #f)) args))
		(proc    (compile fun env #f)))
	     (compile-application proc actuals tail?))))))

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
	  (let ((global (assq symbol the-global-environment)))
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
;*    compile-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (compile-ref variable tail?)
   (cond
      ((global? variable)
       `(5 . ,variable))
      ((dynamic? variable)
       `(6 . ,variable))
      (else
       (case variable
	  ((0 1 2 3)
	   `(,variable))
	  (else
	   `(4 . ,variable))))))

;*---------------------------------------------------------------------*/
;*    compile-set ...                                                  */
;*---------------------------------------------------------------------*/
(define (compile-set variable value)
   (cond
      ((global? variable)
       `(12 ,variable . ,value))
      ((dynamic? variable)
       `(13 ,variable . ,value))
      (else
       (case variable
	  ((0)
	   `(7 . ,value))
	  ((1)
	   `(8 . ,value))
	  ((2)
	   `(9 . ,value))
	  ((3)
	   `(10 . ,value))
	  (else
	   `(11 ,variable . ,value))))))
	
;*---------------------------------------------------------------------*/
;*    compile-cnst ...                                                 */
;*---------------------------------------------------------------------*/
(define (compile-cnst cnst tail?)
   (cond
      ((null? cnst)
       '(-2))
      ((and (boolean? cnst) (not cnst))
       '(-3))
      ((boolean? cnst)
       '(-4))
      ((eq? cnst 1)
       '(-5))
      (else
       `(-1 . ,cnst))))

;*---------------------------------------------------------------------*/
;*    compile-if ...                                                   */
;*---------------------------------------------------------------------*/
(define (compile-if test then sinon)
   `(14 ,test ,then . ,sinon))

;*---------------------------------------------------------------------*/
;*    compile-begin ...                                                */
;*---------------------------------------------------------------------*/
(define (compile-begin body env)
   (cond
      ((and (pair? body) (and (null? (cdr body))))
       ;; le cas degenere
       (compile (car body) env #t))
      (else
       (let ((body (let loop ((rest body))
		      (cond
			 ((null? rest)
			  (error "ewal" "Illegal form" body))
			 ((null? (cdr rest))
			  (cons (compile (car rest) env #t) '()))
			 (else
			  (cons (compile (car rest) env #f)
				(loop (cdr rest))))))))
	  `(15 ,@body)))))

;*---------------------------------------------------------------------*/
;*    linit-the-global-environment! ...                                */
;*---------------------------------------------------------------------*/
(define (linit-the-global-environment!)
   (if (pair? the-global-environment)
       'done
       ;; je ne peux pas utiliser de constante car quand cette fonction
       ;; sera appellee, je ne suis pas sur que le module soit deja
       ;; initialise.
       (set! the-global-environment (cons (cons #f #f) '()))))

;*---------------------------------------------------------------------*/
;*    compile-define ...                                               */
;*    -------------------------------------------------------------    */
;*    On ne rajoute pas en tete car elle contient la definition de     */
;*    `the-module-environment'. On rajoute donc en deuxieme.           */
;*---------------------------------------------------------------------*/
(define (compile-define var val)
   `(16 ,var . ,val))

;*---------------------------------------------------------------------*/
;*    ldefine-primitive! ...                                           */
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
;*      my-update-global! ...                                          */
;*---------------------------------------------------------------------*/
(define (my-update-global! variable val runtime-env)
   (set-cdr! variable (meaning val runtime-env))
   (car variable))

;*---------------------------------------------------------------------*/
;*    my-extend-env ...                                                */
;*---------------------------------------------------------------------*/
(define (my-extend-env extend old-env)
   (let _loop_ ((extend extend))
      (cond
	 ((null? extend)
	  old-env)
	 ((not (pair? extend))
	  (cons extend old-env))
	 (else
	  (cons (car extend) (_loop_ (cdr extend)))))))
   
;*---------------------------------------------------------------------*/
;*    compile-lambda ...                                               */
;*---------------------------------------------------------------------*/
(define (compile-lambda formals body tail?)
   (cond
      ((null? formals)
       `(17 . ,body))
      ((null? (cdr formals))
       `(18 . ,body))
      ((null? (cddr formals))
       `(19 . ,body))
      ((null? (cdddr formals))
       `(20 . ,body))
      ((null? (cddddr formals))
       `(21 . ,body))
      (else
       `(26 ,formals . ,body))))

;*---------------------------------------------------------------------*/
;*    compile-global-application ...                                   */
;*---------------------------------------------------------------------*/
(define (compile-global-application proc actuals tail?)
   (case (length actuals)
      ((0 1 2 3)
       `(,(+fx (length actuals) 27) ,proc ,@actuals))
      (else
       `(32 ,proc ,@actuals))))

;*---------------------------------------------------------------------*/
;*    compile-compiled-application ...                                 */
;*---------------------------------------------------------------------*/
(define (compile-compiled-application proc actuals tail?)
   (case (length actuals)
      ((0 1 2 3)
       `(,(+fx (length actuals) 33) ,proc ,@actuals))
      (else
       `(38 ,proc ,@actuals))))
       
;*---------------------------------------------------------------------*/
;*    compile-application ...                                          */
;*---------------------------------------------------------------------*/
(define (compile-application proc actuals tail?)
   (case (length actuals)
      ((0 1 2 3)
       `(,(+fx (length actuals) 39) ,proc ,@actuals))
      (else
       `(44 ,proc ,@actuals))))

(linit-the-global-environment!)
(ldefine-primitive! '+ (lambda (x y) (+fx x y)))
(ldefine-primitive! '- (lambda (x y) (-fx x y)))
(ldefine-primitive! '< (lambda (x y) (<fx x y)))
(ldefine-primitive! 'eq? eq?)
(ldefine-primitive! 'car car)
(ldefine-primitive! 'cdr cdr)
(ldefine-primitive! 'cons cons)
(ldefine-primitive! 'null? null?)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (bench)
   (ewal '(fib 25)))

(define (run num)
   (ewal '(define fib (lambda (x)
			 (if (< x 2)
			     1
			     (+ (fib (- x 1)) (fib (- x 2)))))))
   (let ((res (repeat num (bench))))
      (print-res res 121393)))

(define (do-bench num)
   (if (>fx num 0)
       (run num)))

(do-bench 5)
