;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expddefine.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan  4 17:14:30 1993                          */
;*    Last change :  Sun Aug 25 09:13:48 2019 (serrano)                */
;*    Copyright   :  2001-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Macro expansions of DEFINE and LAMBDA forms.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __expander_define
   
   (import  __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __object
	    __param
	    __thread
	    __reader
	    __dsssl
	    
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
	    __expand)
   
   (use     __type
	    __evenv
	    __evutils
	    __bit)

   (export  (eval-begin-expander ::procedure)
	    (expand-eval-lambda ::obj ::procedure)
	    (expand-eval-define ::obj ::procedure)
	    (expand-eval-define-inline ::obj ::procedure)
	    (expand-eval-define-generic ::obj ::procedure)
	    (expand-eval-define-method ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-args ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-args args e)
   (let loop ((args args))
      (cond
	 ((null? args)
	  '())
	 ((symbol? args)
	  args)
	 ((not (pair? args))
	  (expand-error "expand" "Illegal argument" args))
	 ((not (and (pair? (car args))
		    (pair? (cdr (car args)))
		    (null? (cddr (car args)))))
	  (cons (car args) (loop (cdr args))))
	 (else
	  (cons (list (car (car args)) (e (cadr (car args)) e))
		(loop (cdr args)))))))
       
;*---------------------------------------------------------------------*/
;*    eval-begin-expander ...                                          */
;*---------------------------------------------------------------------*/
(define (eval-begin-expander olde)
   (lambda (x e)
      (let ((res (match-case x
		    ((begin)
		     #unspecified)
		    ((begin . ?rest)
		     (if (not (list? rest))
			 (expand-error "begin" "Illegal `begin' form" x)
			 (lambda-defines (map (lambda (x) (olde x e)) rest))))
		    (else
		     (let ((nx (olde x e)))
			(match-case nx
			   ((begin)
			    #unspecified)
			   ((begin . ?rest)
			    (if (not (list? rest))
				(expand-error "begin" "Illegal `begin' form" x)
				(lambda-defines rest)))
			   (else
			    nx)))))))
	 (evepairify res x))))

;*---------------------------------------------------------------------*/
;*    expand-eval-lambda ...                                           */
;*---------------------------------------------------------------------*/
(define (expand-eval-lambda x e)
   (let ((res (match-case x
		 ((?- ?args . (and ?body (not ())))
		  (let* ((eargs (expand-args args e))
			 (ebody (expand-progn body))
			 (ne (eval-begin-expander e)))
		     `(lambda ,eargs
			 ,(%with-lexical (args->list eargs) ebody ne #f))))
		 (else
		  (expand-error "lambda" "Illegal form" x)))))
      (evepairify res x)))

;*---------------------------------------------------------------------*/
;*    expand-eval-define ...                                           */
;*    -------------------------------------------------------------    */
;*    on divise en deux sous:                                          */
;*       1- on define une lambda.                                      */
;*       2- on define une valeur (autre qu'un lambda).                 */
;*---------------------------------------------------------------------*/
(define (expand-eval-define x olde)
   (letrec ((newe (lambda (x e)
		     (match-case x
			((define . ?-)
			 (expand-eval-internal-define x e))
			(else
			 (olde x e))))))
       (expand-eval-external-define x newe)))

;*---------------------------------------------------------------------*/
;*    expand-eval-external-define ...                                  */
;*---------------------------------------------------------------------*/
(define (expand-eval-external-define x e)
   (let ((e (eval-begin-expander e)))
      (let* ((err (lambda () (expand-error "define" "Illegal form" x)))
	     (loc (get-source-location x))
	     (res (if (and (pair? x) (pair? (cdr x)) (pair? (cddr x)))
		      (let ((type (cadr x)))
			 (cond
			    ((and (pair? type) (symbol? (car type)))
			     `(define ,(car (parse-formal-ident (car type) loc))
				 (lambda ,(expand-args (cdr type) e)
				    ,(e (expand-progn (cddr x)) e))))
			    ((symbol? type)
			     `(define ,(car (parse-formal-ident type loc))
				 ,(e (expand-progn (cddr x)) e)))
			    (else
			     (err))))
		      (err))))
	 (evepairify res x))))

;*---------------------------------------------------------------------*/
;*    expand-eval-internal-define ...                                  */
;*---------------------------------------------------------------------*/
(define (expand-eval-internal-define x e)
   (match-case x
      ;; 1- a lambda definition
      ((or (?- (?name . ?args) . (and ?body (not ())))
	   (?- ?name (lambda ?args . (and ?body (not ())))))
       (let* ((loc (get-source-location x))
	      (eargs (expand-args args e))
	      (res `(define ,(car (parse-formal-ident name loc))
		       (lambda ,eargs
			  ,(%with-lexical
			    (args->list eargs) (expand-progn body) e #f)))))
	  (evepairify res x)))
      ;; 2- a variable definition
      ((?- ?name . (?value . ()))
       (let* ((loc (get-source-location x))
	      (res `(define ,(car (parse-formal-ident name loc)) ,(e value e))))
	  (evepairify res x)))
      ;; 3- an illegal define form
      (else
       (expand-error "define" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    lambda-defines ...                                               */
;*---------------------------------------------------------------------*/
(define (lambda-defines body::pair-nil)
   (let loop ((oldforms  body)
	      (newforms '())
	      (vars     '())
	      (sets     '()))
      (if (pair? oldforms)
	  (let* ((form (car oldforms))
		 (loc (or (get-source-location form)
			  (get-source-location oldforms))))
	     (cond ((or (not (pair? form))
			(not (eq? (car form) 'define)))
		    (loop (cdr oldforms)
			  (cons form newforms)
			  vars sets))
		   (else
		    (loop (cdr oldforms) newforms
			  (cons (cadr form) vars)
			  (cons `(set! ,(car (parse-formal-ident (cadr form) loc))
				       ,(caddr form))
				sets)))))
	  (if (not (null? vars))
	      `(let ,(map (lambda (v) (list v #unspecified)) vars)
		  ,(expand-progn (append (reverse sets) (reverse newforms))))
	      (expand-progn body)))))

;*---------------------------------------------------------------------*/
;*    expand-eval-define-inline ...                                    */
;*---------------------------------------------------------------------*/
(define (expand-eval-define-inline x e)
   (match-case x
      ((?- (?fun . ?formals) . (and ?body (not ())))
       (let* ((loc (get-source-location x))
	      (res `(define ,(car (parse-formal-ident fun loc))
		      ,(e `(lambda ,(expand-args formals e)
			      ,(expand-progn body)) e))))
	  (evepairify res x)))
      (else
       (expand-error "define-inline" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    map+ ...                                                         */
;*---------------------------------------------------------------------*/
(define (map+ f lst)
   (cond
      ((null? lst)
       '())
      ((not (pair? lst))
       (f lst))
      (else
       (cons (f (car lst)) (map+ f (cdr lst))))))

;*---------------------------------------------------------------------*/
;*    all? ...                                                         */
;*---------------------------------------------------------------------*/
(define (all? pred p)
   (let loop ((p p))
      (cond
	 ((null? p) #t)
	 ((pair? p) (when (pred (car p)) (loop (cdr p))))
	 ((pred p) #t)
	 (else #f))))
				
;*---------------------------------------------------------------------*/
;*    expand-eval-define-generic ...                                   */
;*---------------------------------------------------------------------*/
(define (expand-eval-define-generic x e)
   (match-case x
      ((?- (?fun ?f0 . ?formals) . ?body)
       (let* ((loc (get-source-location x))
	      (pf (parse-formal-ident fun loc))
	      (id (car pf))
	      (pa (map+ (lambda (i) (parse-formal-ident i loc))
		     (cons f0 formals)))
	      (def (gensym id))
	      (epa (expand-args pa e))
	      (va (and (not (null? formals))
		       (or (not (pair? formals))
			   (not (null? (cdr (last-pair formals)))))))
	      (met (gensym id))  
	      (met-body `(,met ,@(map+ (lambda (a)
					 (if (pair? a) (car a) a))
				      epa)))
	      (proc (cond
		      ((all? symbol? (cdr (cadr x)))
		       (let ((def-body `((generic-default ,id)
					 ,@(map+ (lambda (a)
						    (if (pair? a) (car a) a))
					      epa))))
			  `(lambda ,(cons f0 formals)
			      (let ((,def (lambda ()
					     ,(if va
						  (cons 'apply def-body)
						  def-body))))
				 (let ((,met (and (object? ,(caar pa))
						  (find-method ,(caar pa) ,id))))
				    (if (procedure? ,met)
					,(if va (cons 'apply met-body) met-body)
					(,def)))))))
		      ((and (list? formals) (memq #!optional formals))
		       (if (pair? (cdr (filter dsssl-named-constant? formals)))
			   (expand-error fun
			      "generics can only use one DSSSL keyword"
			      x)
			   (let* ((loc (get-source-location x))
				  (args (gensym 'args))
				  (names (dsssl-formals->names formals))
				  (unames (map (lambda (f) (car (parse-formal-ident f loc))) names)))
			      `(lambda (,f0 ,@formals)
				  (let ((,met (and (object? ,(caar pa))
						   (find-method ,(caar pa) ,id))))
				     (if (procedure? ,met)
					 (,met ,(caar pa) ,@unames)
					 ((generic-default ,id) ,(caar pa) ,@unames)))))))
		      ((and (list? formals) (any dsssl-named-constant? formals))
		       (let ((args (gensym 'args)))
			  `(lambda (,f0 . ,args)
			      (let ((,def (lambda ()
					     (apply (generic-default ,id) ,(caar pa) ,args))))
				 (let ((,met (and (object? ,(caar pa))
						  (find-method ,(caar pa) ,id))))
				    (if (procedure? ,met)
					(apply ,met ,(caar pa) ,args)
					(,def)))))))
		      (else
		       (expand-error fun
			  "Illegal formal arguments for generic function"
			  x)))))
	  (e `(begin
		 (define ,fun (procedure->generic ,proc))
		 (register-generic! ,id
		    (lambda ,(cons f0
				(if (memq #!optional formals)
				    (dsssl-formals->names formals)
				    formals))
		       ,(if (pair? body)
			    `(begin ,@body)
			    `(error ,(symbol->string (car pf))
				"No method for this object"
				',(car (car pa)))))
		    #f
		    ,(symbol->string id)))
	     e)))
      (else
       (expand-error "define-generic" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-eval-define-method ...                                    */
;*---------------------------------------------------------------------*/
(define (expand-eval-define-method x e)
   
   (define (get-arg a loc)
      (let ((r (parse-formal-ident a loc)))
	 (if (pair? r)
	     (car r)
	     r)))
   
   (define (get-args args loc)
      (cond
	 ((null? args)
	  '())
	 ((pair? args)
	  (cons (get-arg (car args) (or (get-source-location args) loc))
	     (get-args (cdr args) loc)))
	 (else
	  (list (get-arg args loc)))))
   
   (define (pair->list p)
      (cond
	 ((pair? p) (cons (car p) (pair->list (cdr p))))
	 ((null? p) '())
	 (else (list p))))
   
   (match-case x
      ((?- (?fun ?f0 . ?formals) . (and ?body (not ())))
       (let* ((loc (get-source-location x))
	      (pf (parse-formal-ident fun loc))
	      (p0 (parse-formal-ident f0 loc))
	      (rest (get-args formals loc))
	      (va (and (not (null? formals))
		       (or (not (pair? formals))
			   (not (null? (cdr (last-pair formals))))))))
	  (if (and (pair? p0) (symbol? (cdr p0)))
	      (cond
		 ((or (not (list? formals)) (not (any dsssl-named-constant? formals)))
		  (let* ((res `(generic-add-eval-method!
				  ,(car pf)
				  ,(cdr p0)
				  ,(e `(lambda ,(expand-args (cons f0 formals) e)
					  (define (call-next-method)
					     (let ((next (find-super-class-method
							    ,(car p0)
							    ,(car pf)
							    ,(cdr p0))))
						(if (procedure? next)
						    ,(if va
							 `(apply next ,(car p0) ,@rest)
							 `(next ,(car p0) ,@rest))
						    ,(if va
							 `(apply ,(car pf) ,(car p0) ,@rest)
							 `(,(car pf) ,(car p0) ,@rest)))))
					  ,@body) e)
				  ',f0)))
		     (evepairify res x)))
		 ((memq #!optional formals)
		  (let* ((tformals (dsssl-formals->scheme-typed-formals formals error #t))
			 (uformals (dsssl-formals->scheme-typed-formals formals error #f))
			 (names (dsssl-formals->names formals))
			 (res `(generic-add-eval-method!
				  ,(car pf)
				  ,(cdr p0)
				  ,(e `(lambda ,(expand-args (cons f0 names) e)
					  (define (call-next-method)
					     (let ((next (find-super-class-method ,(car p0)
							    ,(car pf)
							    ,(cdr p0))))
						(if (procedure? next)
						    (next ,(car p0) ,@names)
						    (apply ,(car pf) ,(car p0) ,@names))))
					  ,@body)
				      e)
				  ',f0)))
		     (evepairify res x)))
		 (else
		  ;;; plain dsssl method
		  (let* ((tformals (dsssl-formals->scheme-typed-formals formals error #t))
			 (uformals (dsssl-formals->scheme-typed-formals formals error #f))
			 (res `(generic-add-eval-method!
				  ,(car pf)
				  ,(cdr p0)
				  ,(e `(lambda ,(expand-args (cons f0 tformals) e)
					  (define (call-next-method)
					     (let ((next (find-super-class-method ,(car p0)
							    ,(car pf)
							    ,(cdr p0))))
						(if (procedure? next)
						    (apply next ,(car p0)
						       ,@(pair->list uformals))
						    (apply ,(car pf) ,(car p0)
						       ,@(pair->list uformals)))))
					  ,(make-dsssl-function-prelude fun formals
					      `(begin ,@body) error))
				      e)
				  ',f0)))
		     (evepairify res x))))
	      (expand-error "define-method" "Illegal form" x))))
      (else
       (expand-error "define-method" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    dsssl-formals->names ...                                         */
;*    -------------------------------------------------------------    */
;*    Returns the parameters names. Assumes a well formed prototype.   */
;*---------------------------------------------------------------------*/
(define (dsssl-formals->names formals)
   (filter-map (lambda (p)
		  (cond
		     ((symbol? p) p)
		     ((pair? p) (car p))
		     (else #f)))
      formals))
