;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/progn.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 11:08:04 1994                          */
;*    Last change :  Tue Dec 12 08:44:27 2006 (serrano)                */
;*    Copyright   :  1994-2006 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The code sequence normalization.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_progn
   (include "Tools/location.sch")
   (import  tools_misc
	    tools_error
	    tools_location)
   (export  (normalize-progn <expression>)
	    (normalize-begin <expression>)
	    (normalize-progn/loc <expression> <value>)
	    (progn-first-expression <expression>)
	    (progn-tail-expressions <expression>)
	    (emap f l)))

;*---------------------------------------------------------------------*/
;*    normalize-progn ...                                              */
;*    sexp --> sexp                                                    */
;*---------------------------------------------------------------------*/
(define (normalize-progn body*)
   (cond
      ((not (pair? body*))
       (internal-error "normalize-progn" "Illegal expression" body*))
      ((null? (cdr body*))
       (match-case (car body*)
	  ((begin)
	   #unspecified)
	  ((begin ?exps)
	   (car body*))
	  ((begin . ?exps)
	   (normalize-progn exps))
	  (else
	   (car body*))))
      (else
       (let ((sub (let loop ((body* (if (eq? (car body*) 'begin)
					(cdr body*)
					body*)))
		     (if (null? body*)
			 '()
			 (let ((expr (car body*)))
			    (if (and (pair? expr) (eq? (car expr) 'begin))
				(append (cdr expr) (loop (cdr body*)))
				(cond
				   ((and (eq? expr #unspecified)
					 (pair? (cdr body*)))
				    (loop (cdr body*)))
				   ((epair? expr)
				    (econs expr
					   (loop (cdr body*))
					   (cer expr)))
				   ((epair? body*)
				    (econs expr
					   (loop (cdr body*))
					   (cer body*)))
				   (else
				    (cons expr (loop (cdr body*)))))))))))
	  (if (epair? body*)
	      (econs 'begin sub (cer body*))
	      (cons 'begin sub))))))

;*---------------------------------------------------------------------*/
;*    normalize-begin ...                                              */
;*---------------------------------------------------------------------*/
(define (normalize-begin beg)
   (let ((body (cdr beg)))
      (cond
	 ((null? body)
	  #unspecified)
	 ((null? (cdr body))
	  (let ((e (car body)))
	     (match-case e
		((begin . ?rest)
		 (normalize-begin e))
		(else
		 e))))
	 (else
	  (let loop ((l body)
		     (res '()))
	     (cond
		((null? l)
		 (cond
		    ((null? res)
		     #unspecified)
		    ((null? (cdr res))
		     (car res))
		    (else
		     (epairify `(begin ,@(reverse! res)) beg))))
		((pair? l)
		 (let ((b (car l)))
		    (if (and (not (pair? b)) (not (symbol? b)) (pair? (cdr l)))
			(loop (cdr l) res)
			(loop (cdr l) (cons b res)))))
		(else
		 (internal-error 'normalize-begin "Illegal form" beg))))))))

;*---------------------------------------------------------------------*/
;*    normalize-progn/loc ...                                          */
;*---------------------------------------------------------------------*/
(define (normalize-progn/loc body* loc)
   (let ((nbody (expand-progn body*)))
      (cond
	 ((not loc)
	  nbody)
	 ((epair? nbody)
	  nbody)
	 ((pair? nbody)
	  (econs (car nbody) (cdr nbody) loc))
	 (else
	  (econs 'begin (list nbody) loc)))))

;*---------------------------------------------------------------------*/
;*    emap ...                                                         */
;*---------------------------------------------------------------------*/
(define (emap f l0)
   (let loop ((l l0))
      (cond
	 ((null? l)
	  '())
	 ((epair? l)
	  (econs (f (car l)) (loop (cdr l)) (cer l)))
	 ((pair? l)
	  (cons (f (car l)) (loop (cdr l))))
	 (else
	  (internal-error 'emap "Illegal parameter list" l0)))))

;*---------------------------------------------------------------------*/
;*    progn-first-expression ...                                       */
;*    -------------------------------------------------------------    */
;*    Search the first non BEGIN expression.                           */
;*---------------------------------------------------------------------*/
(define (progn-first-expression exp)
   (let loop ((exp exp))
      (match-case exp
	 ((begin)
	  #f)
	 ((begin ?first . ?-)
	  (loop first))
	 (else
	  exp))))

;*---------------------------------------------------------------------*/
;*    progn-tail-expressions ...                                       */
;*    -------------------------------------------------------------    */
;*    Search all but the first non BEGIN expression.                   */
;*---------------------------------------------------------------------*/
(define (progn-tail-expressions exp)
   (let loop ((exp exp))
      (match-case exp
	 ((begin)
	  '())
	 ((begin ?- . ?rest)
	  rest)
	 (else
	  '()))))
