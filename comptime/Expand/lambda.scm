;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/lambda.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 15:44:53 1994                          */
;*    Last change :  Mon Oct 18 08:37:41 2010 (serrano)                */
;*    Copyright   :  1994-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The lambda macro-expansion.                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_lambda
   (include "Tools/trace.sch")
   (import tools_args
	   tools_progn
	   tools_misc
	   tools_location
	   expand_eps
	   engine_param
	   (find-location tools_location))
   (export internal-definition?
	   (expand-args ::obj ::procedure)
	   (expand-lambda ::obj ::procedure)
	   (internal-begin-expander::procedure ::procedure)))

;*---------------------------------------------------------------------*/
;*    internal-definition? ...                                         */
;*---------------------------------------------------------------------*/
(define internal-definition? #f)

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
	  (error "expand" "Illegal argument" args))
	 ((not (and (pair? (car args))
		    (pair? (cdr (car args)))
		    (null? (cddr (car args)))))
	  (cons (car args) (loop (cdr args))))
	 (else
	  (cons (list (car (car args)) (e (cadr (car args)) e))
		(loop (cdr args)))))))
       
;*---------------------------------------------------------------------*/
;*    expand-lambda ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-lambda x e)
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let ((res (match-case x
		    ((?lam ?args . (and ?body (not ())))
		     (with-lexical (args*->args-list args)
				   '_
				   (find-location x)
				   (lambda ()
				      (let ((e (internal-begin-expander e)))
					 `(,lam ,(expand-args args e)
					     ,(e (expand-progn body) e))))))
		    (else
		     (error #f "Illegal `lambda' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))

;*---------------------------------------------------------------------*/
;*    internal-begin-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (internal-begin-expander old-expander)
   (lambda (expr expander)
      (match-case expr
	 ((begin)
	  #unspecified)
	 ((begin . ?rest)
	  (if (not (list? rest))
	      (error 'begin "Illegal `begin' form" expr)
	      (with-lexical
	       (begin-bindings rest)
	       '_
	       (find-location expr)
	       (lambda ()
		  (lambda-defines
		   (emap (lambda (x) (old-expander x old-expander)) rest))))))
	 (else
	  (let ((nexpr (old-expander expr old-expander)))
	     (match-case nexpr
		((begin)
		 #unspecified)
		((begin . ?rest)
		 (if (not (list? rest))
		     (error 'begin "Illegal `begin' form" expr)
		     (with-lexical
		      (begin-bindings rest)
		      '_
		      (find-location expr)
		      (lambda ()
			 (lambda-defines
			  (emap (lambda (x) (old-expander x old-expander)) rest))))))
		(else
		 nexpr)))))))

;*---------------------------------------------------------------------*/
;*    begin-bindings ...                                               */
;*---------------------------------------------------------------------*/
(define (begin-bindings body)
   (trace expand "let-bindings: " body
	  #\Newline)
   (let loop ((oldforms  body)
	      (vars     '()))
      (if (pair? oldforms)
	  (let ((form (car oldforms)))
	     (match-case form
		((define (?var . ?-) . ?-)
		 (loop (cdr oldforms) (cons var vars)))
		((define ?var . ?-)
		 (loop (cdr oldforms) (cons var vars)))
		(else
		 (loop (cdr oldforms) vars))))
	  vars)))

;*---------------------------------------------------------------------*/
;*    lambda-defines ...                                               */
;*---------------------------------------------------------------------*/
(define (lambda-defines body::pair-nil)
   (let loop ((oldforms  body)
	      (newforms '())
	      (vars     '())
	      (decls    '()))
      (if (pair? oldforms)
	  (let ((form (car oldforms)))
	     (match-case form
		((define ?var ?val)
		 (loop (cdr oldforms)
		       newforms
		       (cons var vars)
		       (cons `(,var ,val) decls)))
		((begin . ?body)
		 (loop (append body (cdr oldforms))
		       newforms
		       vars
		       decls))
		(else
		 (loop (cdr oldforms)
		       (cons form newforms)
		       vars
		       decls))))
	  (cond
	     ((not (null? vars))
	      `(letrec* ,(reverse! decls) ,(expand-progn (reverse newforms))))
	     (else
	      (expand-progn body))))))

