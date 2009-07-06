;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkgcomp/src/Llib/srfi89.sch      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 11 17:23:50 2006                          */
;*    Last change :  Sun Feb 18 10:09:43 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    SRFI89 to DSSSL translation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    srfi89-args? ...                                                 */
;*---------------------------------------------------------------------*/
(define (srfi89-args? args)
   (let loop ((args args))
      (cond
	 ((not (pair? args))
	  #f)
	 ((symbol? (car args))
	  (loop (cdr args)))
	 ((pair? (car args))
	  #t))))

;*---------------------------------------------------------------------*/
;*    @srfi89->dsssl-proto ...                                         */
;*---------------------------------------------------------------------*/
(define (@srfi89->dsssl-proto args)
   (if (not (pair? args))
       args
       (let loop ((a args)
		  (reqs '())
		  (optionals '())
		  (names '()))
	  (cond
	     ((symbol? a)
	      (if (or (pair? optionals) (pair? names))
		  (error 'srfi89->dsssl "Not implemented yet" args)
		  args))
	     ((null? a)
	      (if (and (null? optionals) (null? names))
		  args
		  (let ((opts (if (pair? optionals)
				  (cons '#!optional (reverse! optionals))
				  '()))
			(names (if (pair? names)
				   (cons '#!key names)
				   '())))
		     (append (reverse! reqs) opts names))))
	     ((not (pair? a))
	      (error 'srfi89->dsssl "Illegal argument" a))
	     (else
	      (let ((a0 (car a)))
		 (cond
		    ((symbol? a0)
		     (loop (cdr a) (cons a0 reqs) optionals names))
		    ((srfi89-positional? a0)
		     (loop (cdr a) reqs (cons a0 optionals) names))
		    ((srfi89-named? a0)
		     (let ((d (srfi89-named->dsssl-named a0)))
			(loop (cdr a) reqs optionals (cons d names))))
		    (else
		     (error 'srfi89->dsssl "Illegal argument" a)))))))))

;*---------------------------------------------------------------------*/
;*    @srfi89-prelude ...                                              */
;*    -------------------------------------------------------------    */
;*    This function assumes that the arguments are correct.            */
;*---------------------------------------------------------------------*/
(define (@srfi89-prelude args body)
   (if (not (pair? args))
       (evepairify (cons 'begin body) body)
       (let loop ((a args)
		  (names '()))
	  (cond
	     ((symbol? a)
	      (evepairify (cons 'begin body) body))
	     ((null? a)
	      (if (null? names)
		  (evepairify (cons 'begin body) body)
		  (evepairify `(let ,names ,@body) body)))
	     (else
	      (let ((a0 (car a)))
		 (if (and (srfi89-named? a0)
			  (srfi89-named-require-binding? a0))
		     (loop (cdr a) (cons (srfi89-named->binding a0) names))
		     (loop (cdr a) names))))))))

;*---------------------------------------------------------------------*/
;*    srfi89-positional? ...                                           */
;*---------------------------------------------------------------------*/
(define (srfi89-positional? a)
   (match-case a
      (((? symbol?) ?-)
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    srfi89-named? ...                                                */
;*---------------------------------------------------------------------*/
(define (srfi89-named? a)
   (match-case a
      (((? keyword?) (? symbol?) ?-)
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    srfi89-named->dsssl-named ...                                    */
;*---------------------------------------------------------------------*/
(define (srfi89-named->dsssl-named arg)
   (match-case arg
      ((?key ?- ?val)
       (list (string->symbol (keyword->string key)) val))))

;*---------------------------------------------------------------------*/
;*    srfi89-named-require-binding? ...                                */
;*---------------------------------------------------------------------*/
(define (srfi89-named-require-binding? arg)
   (not (eq? (string->symbol (keyword->string (car arg))) (cadr arg))))

;*---------------------------------------------------------------------*/
;*    srfi89-named->binding ...                                        */
;*---------------------------------------------------------------------*/
(define (srfi89-named->binding arg)
   (match-case arg
      ((?key ?var ?-)
       (list var (string->symbol (keyword->string key))))))

;*---------------------------------------------------------------------*/
;*    @define-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (@define-expander x e)
   (match-case x
      ((?- (? symbol?) . ?-)
       (set-car! x 'define)
       (e x e))
      ((?- ?proto . ?body)
       (let* ((nproto (@srfi89->dsssl-proto proto))
	      (nbody (@srfi89-prelude proto body)))
	  (e (evepairify `(define ,nproto ,nbody) x) e)))
      (else
       (error '@define "Illegal form" x))))

