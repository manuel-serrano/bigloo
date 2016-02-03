;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/if.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:42:10 1994                          */
;*    Last change :  Sat Jan 30 05:22:02 2016 (serrano)                */
;*    Copyright   :  1994-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The `if' macro expansion                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_if
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_misc
	    expand_eps)
   (export  (expand-if ::obj ::procedure)
	    (expand-or ::obj ::procedure)
	    (expand-and ::obj ::procedure)
	    (expand-not ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-or/bool ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-or/bool exp)
   (let ((res (let loop ((sor (cdr exp)))
		 (cond
		    ((null? sor)
		     #f)
		    ((not (pair? sor))
		     (error #f "Illegal form" exp))
		    ((null? (cdr sor))
		     (car sor))
		    (else
		     (let ((res `(if ,(car sor) #t ,(loop (cdr sor)))))
			(cond
			   ((epair? (car sor))
			    (econs (car res) (cdr res) (cer (car sor))))
			   (else
			    res))))))))
      (replace! exp res)))

;*---------------------------------------------------------------------*/
;*    expand-and/bool ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-and/bool exp)
   (let ((res (let loop ((sand (cdr exp)))
		 (cond
		    ((null? sand)
		     #t)
		    ((not (pair? sand))
		     (error #f "Illegal form" exp))
		    ((null? (cdr sand))
		     (car sand))
		    (else
		     (let ((res `(if ,(car sand) ,(loop (cdr sand)) #f)))
			(cond
			   ((epair? (car sand))
			    (econs (car res) (cdr res) (cer (car sand))))
			   (else
			    res))))))))
      (replace! exp res)))

;*---------------------------------------------------------------------*/
;*    expand-test ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-test x e)
   (letrec ((new-e (lambda (x e)
		      (match-case x
			 (((kwote or) . ?-)
			  (new-e (expand-or/bool x) e))
			 (((kwote and) . ?-)
			  (new-e (expand-and/bool x) e))
			 (else
			  (e x e))))))
      (if *nil*
	  (let ((res (new-e x e)))
	     (if (and (pair? x) (pair? res))
		 (replace! x res)
		 res))
	  (let ((aux (gensym 'test)))
	     `(let ((,aux ,(let ((res (new-e x e)))
			      ;; we have to enforce a pair creation
			      ;; otherwise we could be introducing a
			      ;; circular test list
			      (if (pair? res)
				  (cons (car res) (cdr res))
				  res))))
		 (if ,aux
		     (if (null? ,aux)
			 #f
			 #t)
		     #f))))))

;*---------------------------------------------------------------------*/
;*    expand-if ...                                                    */
;*    -------------------------------------------------------------    */
;*    `expand-if' is smart enough to be able to translated nested      */
;*    test in `case' construction.                                     */
;*---------------------------------------------------------------------*/
(define (expand-if x e)
   (match-case x
      ((?- #t ?alors ?sinon)
       (replace! x (e alors e)))
      ((?- #f ?alors ?sinon)
       (replace! x (e sinon e)))
      ((?- ?test ?alors ?sinon)
       (let ((new-test (replace! test (expand-if-with expand-test test e #t)))
	     (new-then (replace! alors (expand-if-with e alors e #t)))
	     (new-else (replace! sinon (expand-if-with e sinon e #f))))
	  (let ((res (let* ((exp0 `(if ,new-test ,new-then ,new-else))
			    (exp1 (epairify-rec exp0 x))
			    (exp (cons (car exp1) (cdr exp1))))
			(if (and *case-enabled?*
				 (not (pair? (assq 'case (lexical-stack)))))
			    (let ((new-exp (find-case-exp exp)))
			       (if new-exp
				   (let ((new-new-exp
					  (e (if->case! new-exp) e)))
				      (set-car! (car new-exp)
						(car new-new-exp))
				      (set-cdr! (car new-exp)
						(cdr new-new-exp))
				      exp)
				   exp))
			    exp))))
	     (replace! x res))
	  x))
      ((?- ?test ?alors)
       (let ((res `(if ,(expand-if-with expand-test test e #t)
		       ,(expand-if-with e alors e #t)
		       #f)))
	  (epairify-rec res x)))
      (else
       (error #f "Illegal `if' form" x))))

;*---------------------------------------------------------------------*/
;*    *case-enable?* ...                                               */
;*---------------------------------------------------------------------*/
(define *case-enabled?* #t)

;*---------------------------------------------------------------------*/
;*    expand-if-with ...                                               */
;*---------------------------------------------------------------------*/
(define (expand-if-with e1 x e2 case?)
   (let ((case-enabled? *case-enabled?*))
      (set! *case-enabled?* case?)
      (let ((res (e1 x e2)))
	 (set! *case-enabled?* case-enabled?)
	 (if (and (pair? x) (pair? res))
	     (replace! x res)
	     res))))

;*---------------------------------------------------------------------*/
;*    find-case-exp ...                                                */
;*    -------------------------------------------------------------    */
;*    On recherche un case possible dans une cascade de if. On         */
;*    descend recursivement dans les branches `sinon' jusqu'a          */
;*    qu'on en trouve une.                                             */
;*---------------------------------------------------------------------*/
(define (find-case-exp exp)
   (trace expand "is-case?: " exp " --> " (is-case? exp) #\Newline)
   (let ((is-case (is-case? exp)))
      (if is-case
	  is-case
	  (match-case exp
	     ((if ?- ?- ?sinon)
	      (find-case-exp sinon))
	     (else
	      #f)))))
   
;*---------------------------------------------------------------------*/
;*    is-case? ...                                                     */
;*    -------------------------------------------------------------    */
;*    Est-ce qu'une cascade de `if' peut-etre compile comme un         */
;*    case ?.                                                          */
;*    -------------------------------------------------------------    */
;*    Il faut pour cela que tous les tests soient de la forme          */
;*    `(eq? x k)', `(c-eq? x k)', `(eqv? x k)', `(eq? k x)',           */
;*    `(memq x k-list)' ou encore `(=fx x k)' et que k soit :          */
;*       . un entier                                                   */
;*       . un caracter                                                 */
;*       . une constante                                               */
;*       . un symbol                                                   */
;*       . un keyword                                                  */
;*---------------------------------------------------------------------*/
(define (is-case? exp)
   (let loop ((var        '())
	      (exp'       exp)
	      (nb-clauses 1))
      (match-case exp'
	 ((if ?test ?- ?sinon)
	  (match-case test
	     (((or c-eq? eq? =fx char=? eqv?) ?exp1 ?exp2)
	      (cond
		 ((is-a-valid-constant? exp1)
		  (cond
		     ((null? var)
		      (loop exp2 sinon (+fx nb-clauses 1)))
		     ((eq? var exp2)
		      (loop exp2 sinon (+fx nb-clauses 1)))
		     (else
		      (if (>fx nb-clauses 3)
			  (list exp var exp')
			  #f))))
		 ((is-a-valid-constant? exp2)
		  (cond
		     ((null? var)
		      (loop exp1 sinon (+fx nb-clauses 1)))
		     ((eq? var exp1)
		      (loop exp1 sinon (+fx nb-clauses 1)))
		     (else
		      (if (>fx nb-clauses 3)
			  (list exp var exp')
			  #f))))
		 (else
		  #f)))
	     ((memq ?new-var ((kwote quote) ?exp1))
	      (cond
		 ((and (pair? exp1)
		       (let loop ((exp1 exp1))
			  (cond
			     ((null? exp1)
			      #t)
			     ((is-a-valid-constant? (car exp1))
			      (loop (cdr exp1)))
			     (else
			      #f))))
		  (cond
		     ((null? var)
		      (loop new-var sinon (+fx nb-clauses 1)))
		     ((eq? new-var var)
		      (loop new-var sinon (+fx nb-clauses 1)))
		     (else
		      (if (>fx nb-clauses 3)
			  (list exp var exp')
			  #f))))
		 (else
		  (if (>fx nb-clauses 3)
		      (list exp var exp')
		      #f))))))
	 (else
	  (if (>fx nb-clauses 3)
	      (list exp var exp')
	      #f)))))

;*---------------------------------------------------------------------*/
;*    is-a-valid-constant? ...                                         */
;*    -------------------------------------------------------------    */
;*    Est-ce une constante qu'on peut mettre dans un `case' ?          */
;*---------------------------------------------------------------------*/
(define (is-a-valid-constant? cnst)
   (or (fixnum? cnst)
       (char? cnst)
       (cnst? cnst)
       (keyword? cnst)
       (and (pair? cnst)
	    (eq? (car cnst) 'quote)
	    (pair? (cdr cnst))
	    (symbol? (cadr cnst))
	    (null? (cddr cnst)))))

;*---------------------------------------------------------------------*/
;*    if->case! ...                                                    */
;*    -------------------------------------------------------------    */
;*    L'argument recu est une liste a trois elements. Le premier       */
;*    est l'expression a reecrire. Le second est la variable testee.   */
;*    La troisieme est l'expression a mettre dans le `else'.           */
;*---------------------------------------------------------------------*/
(define (if->case! exp-var-end)
   (trace expand "if->case: " exp-var-end #\Newline)
   (let* ((exp     (car exp-var-end))
	  (var     (cadr exp-var-end))
	  (end-exp (caddr exp-var-end))
	  (new-exp `(case ,var
		       ,@(let loop ((exp     exp)
				    (clauses '()))
			    (trace expand "   loop(if->case): " exp #\Newline
				   "          end-exp: " end-exp #\Newline
				   "          clauses: " clauses #\Newline)
			    (if (eq? exp end-exp)
				(reverse! (cons `(else ,end-exp) clauses))
				(match-case exp
				   ((if ?test ?alors ?sinon)
				    (loop sinon
					  (cons (make-clause var test alors)
						clauses)))))))))
      (trace expand "if->case: new-exp" new-exp #\Newline)
      (set-car! exp (car new-exp))
      (set-cdr! exp (cdr new-exp))
      (trace expand "new-exp: " exp #\Newline)
      exp))

;*---------------------------------------------------------------------*/
;*    make-clause ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-clause var test alors)
   (define (val->constant v)
      (if (and (pair? v) (eq? (car v) 'quote))
	  ;; get rid of the prefix quote construction
	  (cadr v)
	  v))
   (match-case test
      (((or eq? c-eq? eqv? =fx char=?) ?exp1 ?exp2)
       (if (eq? exp1 var)
	   `((,(val->constant exp2)) ,alors)
	   `((,(val->constant exp1)) ,alors)))
      ((memq ?- ((kwote quote) ?exp))
       `(,exp ,alors))))
		 
;*---------------------------------------------------------------------*/
;*    get-new-test-name ...                                            */
;*---------------------------------------------------------------------*/
(define (get-new-test-name string)
   (let ((symbol (gensym (string-append "_" string "test_"))))
      ;; the non user annotation is used for better bdb code production.
      ;; the non-user property is used by the compiler function
      ;; mark-symbol-non-user! (inside the ast_ident module).
      ;; @ref ../../comptime/Ast/ident.scm:mark-symbol-non-user!@
      (putprop! symbol 'non-user #t)
      symbol))

;*---------------------------------------------------------------------*/
;*    expand-or ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-or x e)
   (let ((res (let loop ((sor (cdr x)))
		 (cond
		    ((null? sor)
		     #f)
		    ((not (pair? sor))
		     (error 'or "Illegal form" x))
		    ((null? (cdr sor))
		     (car sor))
		    (else
		     (let* ((test (get-new-test-name "or"))
			    (res  `(let ((,test ,(car sor)))
				      (if ,test ,test ,(loop (cdr sor))))))
			(cond
			   ((epair? (car sor))
			    (econs (car res) (cdr res) (cer (car sor))))
			   (else
			    res))))))))
      (replace! x (e res e))))

;*---------------------------------------------------------------------*/
;*    expand-and ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-and x e)
   (let ((res (let loop ((sand (cdr x)))
		 (cond
		    ((null? sand)
		     #t)
		    ((not (pair? sand))
		     (error 'and "Illegal form" x))
		    ((null? (cdr sand))
		     (car sand))
		    (else
		     (let* ((test (get-new-test-name "and"))
			    (res  `(let ((,test ,(car sand)))
				      (if ,test ,(loop (cdr sand)) #f))))
			(cond
			   ((epair? (car sand))
			    (econs (car res) (cdr res) (cer (car sand))))
			   (else
			    res))))))))
      (replace! x (e res e))))

;*---------------------------------------------------------------------*/
;*    expand-not ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-not x e)
   (match-case x
      ((?- ?exp)
       (let ((res (e `(if ,exp #f #t) e)))
	  (replace! x res)))
      (else
       (error #f "Illegal `not' form" x))))
	      
	  


