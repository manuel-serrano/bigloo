;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/R5rs/syntax.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 17:24:01 2002                          */
;*    Last change :  Sun Nov 18 14:00:36 2012 (serrano)                */
;*    Copyright   :  2002-12 Dorai Sitaram, Manuel Serrano             */
;*    -------------------------------------------------------------    */
;*    The implementation of R5Rs macros.                               */
;*    -------------------------------------------------------------    */
;*    This code as be re-written from scratch on July 2010.            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r5_macro_4_3_syntax

   (import  __error
	    __object
	    __thread
	    __bexit)
   
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

	    __eval
	    __evenv
	    __macro)

   (import  __r4_output_6_10_3 __r4_numbers_6_5 __r4_ports_6_10_1 __r4_numbers_6_5_flonum_dtoa)

   (export  (install-syntax-expander ::symbol ::procedure)
	    (syntax-rules->expander ::symbol ::pair-nil ::pair-nil)
	    
	    (expand-define-syntax ::obj ::procedure)
	    (expand-letrec-syntax ::obj ::procedure)
	    (expand-let-syntax ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    global variables ...                                             */
;*---------------------------------------------------------------------*/
(define hygiene-mark (gensym '|hygiene.r5rs.mark|))
(define hygiene-prefix (symbol->string hygiene-mark))
(define hygiene-prefix-len (string-length hygiene-prefix))
   
(define syntaxes #f)
(define syntax-mutex (make-mutex))
(define syntax-expanders-mutex (make-mutex))

;*---------------------------------------------------------------------*/
;*    get-syntax-expander ...                                          */
;*---------------------------------------------------------------------*/
(define (get-syntax-expander f)
   (let* ((id (hygiene-value f))
	  (c (synchronize syntax-mutex (assq id syntaxes))))
      (when (pair? c)
	 (cdr c))))

;*---------------------------------------------------------------------*/
;*    install-syntax-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (install-syntax-expander keyword expander)
   (synchronize syntax-mutex
      (set! syntaxes (cons (cons keyword expander) syntaxes))))

;*---------------------------------------------------------------------*/
;*    init-syntax-expanders ...                                        */
;*---------------------------------------------------------------------*/
(define (init-syntax-expanders!)
   
   (define (define-syntax-expander id literals rules)
      (install-syntax-expander id (syntax-rules->expander id literals rules)))
   
   (synchronize syntax-expanders-mutex
      (unless syntaxes
	 (set! syntaxes '())
	 ;; quote
	 (install-syntax-expander 'quote (lambda (x e) x))
	 ;; cond
	 (define-syntax-expander
	    'cond '(else =>)
	    '(((cond (else result1 result2 ...))
	       (begin result1 result2 ...))
	      ((cond (test => result))
	       (let ((temp test))
		  (if temp (result temp))))
	      ((cond (test => result) clause1 clause2 ...)
	       (let ((temp test))
		  (if temp
		      (result temp)
		      (cond clause1 clause2 ...))))
	      ((cond (test)) test)
	      ((cond (test) clause1 clause2 ...)
	       (let ((temp test))
		  (if temp
		      temp
		      (cond clause1 clause2 ...))))
	      ((cond (test result1 result2 ...))
	       (if test (begin result1 result2 ...)))
	      ((cond (test result1 result2 ...)
		     clause1 clause2 ...)
	       (if test
		   (begin result1 result2 ...)
		   (cond clause1 clause2 ...)))))
	 ;; case
	 (define-syntax-expander
	    'case '(else)
	    '(((case (key ...)
		  clauses ...)
	       (let ((atom-key (key ...)))
		  (case atom-key clauses ...)))
	      ((case key
		  (else result1 result2 ...))
	       (begin result1 result2 ...))
	      ((case key
		  ((atoms ...) result1 result2 ...))
	       (if (memv key '(atoms ...))
		   (begin result1 result2 ...)))
	      ((case key
		  ((atoms ...) result1 result2 ...)
		  clause clauses ...)
	       (if (memv key '(atoms ...))
		   (begin result1 result2 ...)
		   (case key clause clauses ...)))))
	 ;; let
	 (define-syntax-expander
	    'let '()
	    '(((let ((name val) ...) body1 body2 ...)
	       ((lambda (name ...) body1 body2 ...)
		val ...))
	      ((let tag ((name val) ...) body1 body2 ...)
	       ((letrec ((tag (lambda (name ...)
				 body1 body2 ...)))
		   tag)
		val ...))))
	 ;; let*
	 (define-syntax-expander
	    'let* '()
	    '(((let* () body1 body2 ...)
	       (let () body1 body2 ...))
	      ((let* ((name1 val1) (name2 val2) ...)
		  body1 body2 ...)
	       (let ((name1 val1))
		  (let* ((name2 val2) ...)
		     body1 body2 ...)))))
	 ;; letrec
	 (define-syntax-expander
	    'letrec '()
	    '(((letrec ((var1 init1) ...) body ...)
	       (letrec "generate temp names"
		  (var1 ...)
		  ()
		  ((var1 init1) ...)
		  body ...))
	      ((letrec "generate temp names"
		  ()
		  (temp1 ...)
		  ((var1 init1) ...)
		  body ...)
	       (let ((var1 #unspecified) ...)
		  (let ((temp1 init1) ...)
		     (set! var1 temp1)
		     ...
		     body ...)))
	      ((letrec "generate temp names"
		  (x y ...)
		  (temp ...)
		  ((var1 init1) ...)
		  body ...)
	       (letrec "generate temp names"
		  (y ...)
		  (newtemp temp ...)
		  ((var1 init1) ...)
		  body ...))))
	 ;; do
	 (define-syntax-expander
	    'do '()
	    '(((do ((var init step ...) ...)
		   (test expr ...)
		   command ...)
	       (letrec
		     ((loop
			 (lambda (var ...)
			    (if test
				(begin
				   (if #f #f)
				   expr ...)
				(begin
				   command
				   ...
				   (loop (do "step" var step ...)
				      ...))))))
		  (loop init ...)))
	      ((do "step" x)
	       x)
	      ((do "step" x y)
	       y))))))

;*---------------------------------------------------------------------*/
;*    syntax-expand ...                                                */
;*---------------------------------------------------------------------*/
(define (syntax-expand x)
   (syntax-expander x syntax-expander))

;*---------------------------------------------------------------------*/
;*    syntax-expander ...                                              */
;*---------------------------------------------------------------------*/
(define (syntax-expander x e)
   (let ((e1 (cond
		((not (pair? x))
		 (lambda (x e) x))
		((get-syntax-expander (car x))
		 =>
		 (lambda (x) x))
		(else
		 (lambda (x e)
		    (let loop ((x x))
		       (cond
			  ((pair? x)
			   (cons (e (car x) e) (loop (cdr x))))
			  ((null? x)
			   '())
			  (else
			   (e x e)))))))))
      (let ((new (e1 x e)))
	 (if (and (pair? new) (not (epair? new)) (epair? x))
	     (econs (car new) (cdr new) (cer x))
	     new))))

;*---------------------------------------------------------------------*/
;*    expand-define-syntax ...                                         */
;*---------------------------------------------------------------------*/
(define (expand-define-syntax x e)
   (match-case x
      ((?- (and (? symbol?) ?macroname) (syntax-rules ?literals . ?rules))
       (let ((ex (syntax-rules->expander/init macroname literals rules)))
	  (install-syntax-expander macroname ex)
	  (install-expander macroname ex)
	  #unspecified))
      (else
       (error "define-syntax" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-letrec-syntax ...                                         */
;*---------------------------------------------------------------------*/
(define (expand-letrec-syntax x e)
   (match-case x
      ((?- ?bs . ?body)
       (let ((e1 (let loop ((bs bs))
		    (if (null? bs)
			e
			(match-case (car bs)
			   (((and (? symbol?) ?m) (syntax-rules ?ls . ?rules))
			    (let ((e3 (syntax-rules->expander/init m ls rules))
				  (e4 (loop (cdr bs))))
			       (lambda (x e2)
				  (if (and (pair? x) (hygiene-eq? m (car x)))
				      (e3 x e2)
				      (e4 x e2)))))
			   (else
			    (error "let-syntax" "Illegal bindings" bs)))))))
	  `(begin
	      ,@(map (lambda (x) (e1 (hygienize x '()) e1)) body))))
      (else
       (error "letrec-syntax" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-let-syntax ...                                            */
;*---------------------------------------------------------------------*/
(define (expand-let-syntax x e)
   (match-case x
      ((?- ?bs . ?body)
       (let ((e1 (let loop ((bs bs))
		    (if (null? bs)
			e
			(match-case (car bs)
			   (((and (? symbol?) ?m) (syntax-rules ?ls . ?rules))
			    (let ((e3 (syntax-rules->expander/init m ls rules))
				  (e4 (loop (cdr bs))))
			       (lambda (x e2)
				  (if (and (pair? x) (hygiene-eq? m (car x)))
				      (e3 x e)
				      (e4 x e2)))))
			   (else
			    (error "let-syntax" "Illegal bindings" bs)))))))
	  `(begin
	      ,@(map (lambda (x) (e1 (hygienize x '()) e1)) body))))
      (else
       (error "let-syntax" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    syntax-rules->expander/init ...                                  */
;*---------------------------------------------------------------------*/
(define (syntax-rules->expander/init keyword literals rules)
   (init-syntax-expanders!)
   (syntax-rules->expander keyword literals rules))

;*---------------------------------------------------------------------*/
;*    syntax-rules->expander ...                                       */
;*---------------------------------------------------------------------*/
(define (syntax-rules->expander keyword literals rules)
   (let ((k (cons keyword literals)))
      (if (list? rules)
	  (lambda (x e)
	     (let loop ((rules rules))
		(if (null? rules)
		    (error keyword "No matching clause" x)
		    (match-case (car rules)
		       ((?pattern ?template)
			(if (syntax-matches-pattern? keyword pattern x k)
			    (begin
			       (let* ((fs (syntax-get-frames pattern x k))
				      (t (syntax-expand-pattern template fs k))
				      (te (syntax-expand t)))
				  (e (hygienize te '()) e)))
			    (loop (cdr rules))))
		       (else
			(error keyword "Illegal clause" (car rules)))))))
	  (error keyword "Illegal declaration" rules))))

;*---------------------------------------------------------------------*/
;*    syntax-matches-pattern? ...                                      */
;*---------------------------------------------------------------------*/
(define (syntax-matches-pattern? keyword p e k)
   (cond
      ((ellipsis? p)
       (if (not (=fx (length p) 2))
	   (error keyword "Illegal ellipsis" p)
	   (and (list? e)
		(let ((p0 (car p)))
		   (every (lambda (ei)
			      (syntax-matches-pattern? keyword p0 ei k))
			  e)))))
      ((pair? p)
       (and (pair? e)
	    (syntax-matches-pattern? keyword (car p) (car e) k)
	    (syntax-matches-pattern? keyword (cdr p) (cdr e) k)))
      ((symbol? p)
       (if (memq p k) (hygiene-eq? e p) #t))
      (else
       (equal? p e))))

;*---------------------------------------------------------------------*/
;*    syntax-get-frames ...                                            */
;*---------------------------------------------------------------------*/
(define (syntax-get-frames p e k)
   (cond
      ((ellipsis? p)
       (let ((p0 (car p)))
	  (list (cons :ellipsis
		   (map (lambda (ei)
			   (syntax-get-frames p0 ei k))
			e)))))
      ((pair? p)
       (append (syntax-get-frames (car p) (car e) k)
	       (syntax-get-frames (cdr p) (cdr e) k)))
      ((symbol? p)
       (if (memq p k) '() (list (cons p (unhygienize e)))))
      (else
       '())))

;*---------------------------------------------------------------------*/
;*    syntax-expand-pattern ...                                        */
;*---------------------------------------------------------------------*/
(define (syntax-expand-pattern p env k)
   
   (define (syntax-expand-ellipsis p0 env k)
      (let* ((vars (get-ellipsis-variables p0 k))
	     (frames (get-ellipsis-frames vars env)))
	 (if (not (list? frames))
	     '()
	     (map (lambda (f)
		     (syntax-expand-pattern p0 (append f env) k))
		  frames))))
   
   (cond
      ((ellipsis? p)
       (append (syntax-expand-ellipsis (car p) env k)
	       (syntax-expand-pattern (cddr p) env k)))
      ((pair? p)
       (cons (syntax-expand-pattern (car p) env k)
	     (syntax-expand-pattern (cdr p) env k)))
      ((symbol? p)
       (if (memq p k)
	   p
	   (let ((x (assq p env)))
	      (if (pair? x)
		  (cdr x)
		  p))))
      (else
       p)))

;*---------------------------------------------------------------------*/
;*    get-ellipsis-frames ...                                          */
;*---------------------------------------------------------------------*/
(define (get-ellipsis-frames vars frames)
   (let loop ((vars vars)
	      (res '()))
      (if (null? vars)
	  res
	  (let ((v (car vars)))
	     (let ((f (any (lambda (f)
			      (when (eq? (car f) :ellipsis)
				 (let ((e (filter (lambda (e) (assq v e)) (cdr f))))
				    (when (pair? e) e))))
			   frames)))
		(if f
		    (let liip ((ovars (cdr vars))
			       (nvars '()))
		       (cond
			  ((null? ovars)
			   (if (null? res)
			       (loop nvars f)
			       (loop nvars (map append f res))))
			  ((any (lambda (e) (pair? (assq (car ovars) e))) f)
			   (liip (cdr ovars) nvars))
			  (else
			   (liip (cdr ovars) (cons (car ovars) nvars)))))
		    (loop (cdr vars) res)))))))
   
;*---------------------------------------------------------------------*/
;*    get-ellipsis-variables ...                                       */
;*---------------------------------------------------------------------*/
(define (get-ellipsis-variables p k)
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
;*    hygiene-symbol ...                                               */
;*---------------------------------------------------------------------*/
(define (hygiene-symbol x)
   (symbol-append hygiene-mark x))

;*---------------------------------------------------------------------*/
;*    hygiene-symbol? ...                                              */
;*---------------------------------------------------------------------*/
(define (hygiene-symbol? x)
   (let ((s (symbol->string x)))
      (substring-at? s hygiene-prefix 0)))
   
;*---------------------------------------------------------------------*/
;*    hygiene-value ...                                                */
;*---------------------------------------------------------------------*/
(define (hygiene-value x)
   (if (not (symbol? x))
       x
       (let ((s (symbol->string x)))
	  (if (substring-at? s hygiene-prefix 0)
	      (string->symbol
	       (substring s hygiene-prefix-len (string-length s)))
	      x))))

;*---------------------------------------------------------------------*/
;*    hygiene-eq? ...                                                  */
;*---------------------------------------------------------------------*/
(define (hygiene-eq? x id)
   (when (and (symbol? id) (symbol? x))
      (or (eq? x id)
	  (and (hygiene-symbol? x) (hygiene-eq? (hygiene-value x) id)))))

;*---------------------------------------------------------------------*/
;*    unhygienize ...                                                  */
;*---------------------------------------------------------------------*/
(define (unhygienize x)
   (cond
      ((symbol? x)
       (hygiene-symbol x))
      ((pair? x)
       (cons (unhygienize (car x)) (unhygienize (cdr x))))
      (else
       x)))

;*---------------------------------------------------------------------*/
;*    hygienize ...                                                    */
;*---------------------------------------------------------------------*/
(define (hygienize x env)
   (match-case x
      ((and ?var (? symbol?))
       (if (hygiene-symbol? x)
	   (hygiene-value x)
	   (let ((o (assq var env)))
	      (if (pair? o)
		  (cdr o)
		  var))))
      (((kwote quote) . ?rest)
       (cons 'quote (hygienize* rest env)))
      ((lambda ?vars . ?body)
       (let* ((nvars (genvars vars))
	      (nenv (append (map cons (flatten vars) (flatten nvars)) env)))
	  `(lambda ,nvars
	      ,@(hygienize* body nenv))))
      ((let ?bindings . ?body)
       (let* ((nvars (genvars (map car bindings)))
	      (nenv (append (map (lambda (b v)
				    (cons (car b) v))
				 bindings nvars)
			    env)))
	  `(let ,(map (lambda (b v)
			 (list v (hygienize (cadr b) env)))
		      bindings nvars)
	      ,@(hygienize* body nenv))))
      ((let (and ?loop (? symbol?)) ?bindings . ?body)
       (let* ((nloop (genvars loop))
	      (nvars (genvars (map car bindings)))
	      (nenv (cons (cons loop nloop)
			  (append (map (lambda (b v)
					  (cons (car b) v))
				       bindings nvars)
				  env))))
	  `(let ,nloop ,(map (lambda (b v)
				(list v (hygienize (cadr b) env)))
			     bindings nvars)
		,@(hygienize* body nenv))))
      ((let* ?bindings . ?body)
       (let loop ((bindings bindings)
		  (nbindings '())
		  (nenv env))
	  (if (null? bindings)
	      `(let* ,(reverse nbindings) ,@(hygienize* body nenv))
	      (let* ((var (caar bindings))
		     (val (cadar bindings))
		     (nvar (genvars var))
		     (nenv (cons (cons var nvar) env)))
		 (loop (cdr bindings)
		       (cons (list var (hygienize var env)) nbindings)
		       nenv)))))
      ((letrec ?bindings . ?body)
       (let* ((nvars (genvars (map car bindings)))
	      (nenv (append (map (lambda (b v)
				    (cons (car b) v))
				 bindings nvars)
			    env)))
	  `(letrec ,(map (lambda (b v)
			    (list v (hygienize (cadr b) nenv)))
			 bindings nvars)
	      ,@(hygienize* body nenv))))
      ((bind-exit (?var) . ?body)
       (let* ((nvar (genvars var))
	      (nenv (cons (cons var nvar) env)))
	  `(bind-exit (,nvar) ,@(hygienize* body nenv))))
      ((?- . ?-)
       (hygienize* x env))
      (else
       x)))

;*---------------------------------------------------------------------*/
;*    hygienize* ...                                                   */
;*---------------------------------------------------------------------*/
(define (hygienize* x env)
   (let loop ((x x))
      (cond
	 ((pair? x)
	  (cons (hygienize (car x) env) (loop (cdr x))))
	 ((null? x)
	  '())
	 (else
	  (hygienize x env)))))

;*---------------------------------------------------------------------*/
;*    flatten ...                                                      */
;*---------------------------------------------------------------------*/
(define (flatten l)
   (cond
      ((pair? l) (cons (car l) (flatten (cdr l))))
      ((null? l) l)
      (else (list l))))

;*---------------------------------------------------------------------*/
;*    genvars ...                                                      */
;*---------------------------------------------------------------------*/
(define (genvars l)
   (define (genname l)
      (match-case l
	 ((? symbol?)
	  (if (hygiene-symbol? l)
	      (hygiene-value l)
	      (gensym l)))
	 (else
	  (gensym))))
   (cond
      ((pair? l) (cons (genname (car l)) (genvars (cdr l))))
      ((null? l) l)
      (else (genname l))))
