;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/let.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 19 08:29:58 1992                          */
;*    Last change :  Tue Dec 17 11:38:27 2013 (serrano)                */
;*    Copyright   :  1992-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Let expansions.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_let
   (include "Ast/node.sch"
	    "Tools/location.sch")
   (import   tools_progn
	     tools_args
	     tools_misc
	     expand_lambda
	     expand_eps
	     (find-location tools_location))
   (export   (expand-let*   ::obj ::procedure)
	     (expand-let    ::obj ::procedure)
	     (expand-letrec ::obj ::procedure)
	     (expand-labels ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-let* ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-let* x e)
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      (replace! x `(let () ,(e (expand-progn body) e))))
		     ((?- (and (? pair?) ?bindings) . (and ?body (not ())))
		      (let ((sl (econs 'let*
				       (cons* (cdr bindings) body)
				       (find-location (car bindings))))
			    (b (epairify (cons (car bindings) '()) bindings)))
			 (e `(let ,b ,sl) e)))
		     (else
		      (error #f "Illegal `let*' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))

;*---------------------------------------------------------------------*/
;*    expand-let ...                                                   */
;*---------------------------------------------------------------------*/
(define (expand-let x e)
   (define (expand-named-let e loop bindings body)
      (let* ((vars (map (lambda (x)
			   (match-case x
			      ((?- ?val)
			       (if (pair? val)
				   (cons #t (gensym))
				   (cons #f (cadr x))))
			      (else
			       (error #f "Illegal `named let' binding" x))))
			bindings))
	     (aux (filter-map (lambda (x y)
				 (and (car x) (cons (cdr x) (cdr y))))
			      vars bindings))
	     (rec `(letrec ((,loop (lambda ,(map car bindings) ,(expand-progn body))))
		      (,loop ,@(map cdr vars))))
	     (exp (if (pair? aux) `(let ,aux ,rec) rec)))
	 (e exp e)))
   (define (expand-simple-let e bindings body)
      (let ((vars (map (lambda (x)
			  (match-case x
			     ((?- ?val) (begin (set-car! (cdr x) (e val e)) x))
			     ((? symbol?) (list x #unspecified))
			     (else (error #f "Illegal `let' binding" x))))
		       bindings)))
	 `(let ,vars
	     ,(with-lexical (map car vars)
			    '_
			    (find-location x)
			    (lambda () (e (expand-progn body) e))))))
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e   (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      ;; we must left the construction (to uses with traces).
		      `(let () ,(e (expand-progn body) e)))
		     ((?- (and (? symbol?) ?l) (and (? list?) ?bdgs) . (and ?body (not ())))
		      ;; rewrite the form into a labels
		      (expand-named-let e l bdgs body))
		     ((?- (and (? list?) ?bdgs) . (and ?body (not ())))
		      ;; a regular lexical block
		      (expand-simple-let e bdgs body))
		     (else
		      (error #f "Illegal `let' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))

;*---------------------------------------------------------------------*/
;*    expand-letrec ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-letrec x e)
   
   (define (expand-rec e bdgs body)
      (let ((vars (map (lambda (x)
			  (match-case x
			     ((?- ?val) (begin (set-car! (cdr x) (e val e)) x))
			     ((? symbol?) (list x #unspecified))
			     (else (error #f "Illegal `letrec' binding" x))))
		     bdgs)))
	 (with-lexical (map car vars)
	    '_
	    (find-location x)
	    (lambda ()
	       `(,(car x) ,vars ,(e (expand-progn body) e))))))

   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      (set-car! x 'let)
		      (e x e))
		     ((?- (and (? list?) ?bdgs) . (and ?body (not ())))
		      (expand-rec e bdgs body))
		     (else
		      (error #f "Illegal `letrec' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))
	 
;*---------------------------------------------------------------------*/
;*      expand-labels ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-labels x e)
   (define (expd-lbls e bindings body)
      (let ((vars (map (lambda (x)
			  (match-case x
			     ((?id () . ?-)
			      id)
			     ((?id (? symbol?) . ?-)
			      id)
			     ((?id ((or (? dsssl-named-constant?) (? symbol?)) ...) . ?-)
			      id)
			     ((?id ((? symbol?) ... . (? symbol?)) . ?-)
			      id)
			     (else
			      (error #f "Illegal `labels' binding" x))))
		       bindings)))
	 (with-lexical
	  vars
	  '_
	  (find-location x)
	  (lambda ()
	     (let ((new (map (lambda (x)
				(match-case x
				   ((?name ?args . (and ?lbody (not ())))
				    (with-lexical
				     (args*->args-list args)
				     '_
				     (find-location x)
				     (lambda ()
					(let* ((body (e (expand-progn lbody) e))
					       (b `(,name ,args ,body)))
					   (epairify b x)))))
				   (else
				    (error #f "Illegal `labels' binding" x))))
			     bindings)))
		`(labels ,new ,(e (expand-progn body) e)))))))
   (let ((old-internal internal-definition?))
      (set! internal-definition? #t)
      (let* ((e   (internal-begin-expander e))
	     (res (match-case x
		     ((?- () . (and ?body (not ())))
		      (set-car! x 'let)
		      (e x e))
		     ((?- (and (? list?) ?bdgs) . (and ?body (not ())))
		      (expd-lbls e bdgs body))
		     (else
		      (error #f "Illegal `labels' form" x)))))
	 (set! internal-definition? old-internal)
	 (replace! x res))))

