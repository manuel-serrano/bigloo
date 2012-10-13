;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/dsssl.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Apr  3 14:42:11 1997                          */
;*    Last change :  Sat Oct 13 07:37:57 2012 (serrano)                */
;*    Copyright   :  1997-2012 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Some dsssl goodies (see also runtime/Llib/dsssl.scm).            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_dsssl
   (import tools_error)
   (export (dsssl-prototype?::bool ::obj)
	   (dsssl-optional-only-prototype?::bool ::obj)
	   (dsssl-key-only-prototype?::bool ::obj)
	   (dsssl-check-prototype?::bool ::obj)
	   (dsssl-arity args ::bool)
	   (dsssl-defaulted-formal? obj)
	   (dsssl-default-formal obj)
	   (dsssl-find-first-formal obj)
	   (dsssl-args*->args-list obj)
	   (dsssl-formals::pair-nil ::pair-nil)
	   (dsssl-optionals::pair-nil ::obj)
	   (dsssl-keys::pair-nil ::obj)
	   (dsssl-key-args-sort::pair-nil ::pair-nil)
	   (dsssl-before-dsssl ::obj)))

;*---------------------------------------------------------------------*/
;*    dsssl-prototype? ...                                             */
;*    -------------------------------------------------------------    */
;*    Is a prototype a DSSSL prototype?                                */
;*---------------------------------------------------------------------*/
(define (dsssl-prototype? args)
   (let loop ((args args))
      (cond
	 ((null? args)
	  #f)
	 ((not (pair? args))
	  #f)
	 ((dsssl-named-constant? (car args))
	  #t)
	 (else
	  (loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    dsssl-only? ...                                                  */
;*---------------------------------------------------------------------*/
(define (dsssl-only? dsssl args)
   (let loop ((args args)
	      (r #f))
      (cond
	 ((null? args)
	  r)
	 ((not (pair? args))
	  r)
	 ((eq? (car args) dsssl)
	  (loop (cdr args) #t))
	 ((dsssl-named-constant? (car args))
	  #f)
	 (else
	  (loop (cdr args) r)))))

;*---------------------------------------------------------------------*/
;*    dsssl-optional-only-prototype? ...                               */
;*    -------------------------------------------------------------    */
;*    Is a prototype a DSSSL optional only prototype?                  */
;*---------------------------------------------------------------------*/
(define (dsssl-optional-only-prototype? args)
   (dsssl-only? #!optional args))

;*---------------------------------------------------------------------*/
;*    dsssl-key-only-prototype? ...                                    */
;*---------------------------------------------------------------------*/
(define (dsssl-key-only-prototype? args)
   (dsssl-only? #!key args))

;*---------------------------------------------------------------------*/
;*    dsssl-check-prototype? ...                                       */
;*    -------------------------------------------------------------    */
;*    Is a DSSSL prototype syntactically correct?                      */
;*    -------------------------------------------------------------    */
;*    This function accepts non DSSSL prototypes.                      */
;*---------------------------------------------------------------------*/
(define (dsssl-check-prototype? args)
   (let loop ((args args))
      (cond
	 ((null? args)
	  #t)
	 ((not (pair? args))
	  #f)
	 ((dsssl-named-constant? (car args))
	  (let liip ((args args))
	     (cond
		((null? args)
		 #t)
		((not (pair? args))
		 #f)
		((eq? #!rest (car args))
		 (if (or (null? (cdr args)) (not (symbol? (cadr args))))
		     #t
		     (loop (cddr args))))
		((dsssl-named-constant? (car args))
		 (loop (cdr args)))
		((symbol? (car args))
		 (loop (cdr args)))
		((and (pair? (car args))
		      (symbol? (car args))
		      (pair? (cdr args))
		      (null? (cddr args)))
		 (loop (cdr args)))
		(else
		 #f))))
	 (else
	  (loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    dsssl-arity ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function assumes a correct DSSSL declaration                */
;*    -------------------------------------------------------------    */
;*    DSSSL named constant is equivalent to a `.' expression.          */
;*    Thus:                                                            */
;*    (lambda (a1 a2 .. an-1 #!optional ...#!rest ...) ..) --> -n      */
;*    -------------------------------------------------------------    */
;*    In optimized mode (optim==#t), #!optional are not n-ary          */
;*    functions.                                                       */
;*---------------------------------------------------------------------*/
(define (dsssl-arity args optim)
   (assert (args) (dsssl-check-prototype? args))
   (let loop ((i 0)
	      (a args))
      (cond
	 ((null? a)
	  i)
	 ((symbol? (car a))
	  (loop (+fx i 1) (cdr a)))
	 ((and (memq (car a) '(#!optional #!key)) optim)
	  (if (any dsssl-named-constant? (cdr a))
	      (- (+fx i 1))
	      i))
	 (else
	  (- (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    dsssl-canonicalize-prototype ...                                 */
;*    -------------------------------------------------------------    */
;*    This function only accepts legal DSSSL protoypes.                */
;*---------------------------------------------------------------------*/
(define (dsssl-canonicalize-prototype args)
   (assert (args) (dsssl-check-prototype? args))
   (let loop ((args args)
	      (mode #f)
	      (res '()))
      (cond
	 ((null? args)
	  (reverse! res))
	 ((eq? (car args) #!rest)
	  (loop (cdr args) #f (cons (car args) res)))
	 ((or (eq? (car args) #!optional) (eq? (car args) #!key))
	  (loop (cdr args) 'opt (cons (car args) res)))
	 ((pair? (car args))
	  (loop (cdr args) mode (cons (car args) res)))
	 ((not mode)
	  (loop (cdr args) mode (cons (car args) res)))
	 (else
	  (loop (cdr args) mode (cons (list (car args) #f) res))))))

;*---------------------------------------------------------------------*/
;*    dsssl-defaulted-formal? ...                                      */
;*    -------------------------------------------------------------    */
;*    Is an expression a defaulted dsssl argument ?                    */
;*---------------------------------------------------------------------*/
(define (dsssl-defaulted-formal? obj)
   (match-case obj
      ((?- ?-)
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    dsssl-default-formal ...                                         */
;*    -------------------------------------------------------------    */
;*    The formal of a dsssl-defaulted-formal expression                */
;*---------------------------------------------------------------------*/
(define (dsssl-default-formal obj)
   (car obj))

;*---------------------------------------------------------------------*/
;*    dsssl-find-first-formal ...                                      */
;*---------------------------------------------------------------------*/
(define (dsssl-find-first-formal args)
   (cond
      ((null? args)
       #f)
      ((not (pair? args))
       (internal-error "dsssl-find-first-formal"
		       "Illegal dsssl formal list"
		       args))
      ((dsssl-named-constant? (car args))
       (dsssl-find-first-formal (cdr args)))
      ((dsssl-defaulted-formal? (car args))
       (dsssl-default-formal (car args)))
      ((not (symbol? (car args)))
       (internal-error "dsssl-find-first-formal"
		       "Illegal dsssl formal list"
		       args))
      (else
       (car args))))
       
;*---------------------------------------------------------------------*/
;*    dsssl-args*->args-list ...                                       */
;*    cons* --> list                                                   */
;*---------------------------------------------------------------------*/
(define (dsssl-args*->args-list exp)
   (cond
      ((null? exp)
       '())
      ((not (pair? exp))
       (list exp))
      ((dsssl-named-constant? (car exp))
       (let ((arg (dsssl-find-first-formal (cdr exp))))
	  (if arg
	      (list arg)
	      '())))
      ((dsssl-defaulted-formal? (car exp))
       (dsssl-args*->args-list (cdr exp)))
      (else
       (cons (car exp) (dsssl-args*->args-list (cdr exp))))))

;*---------------------------------------------------------------------*/
;*    dsssl-formals ...                                                */
;*---------------------------------------------------------------------*/
(define (dsssl-formals args)
   (let loop ((args args))
      (cond
	 ((not (pair? args))
	  '())
	 ((dsssl-named-constant? (car args))
	  args)
	 (else
	  (loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    dsssl-arguments ...                                              */
;*---------------------------------------------------------------------*/
(define (dsssl-arguments key args)
   (let loop ((args args))
      (cond
	 ((not (pair? args))
	  '())
	 ((eq? (car args) key)
	  (let liip ((args (cdr args))
		     (r '()))
	     (cond
		((null? args)
		 (reverse! r))
		((not (pair? args))
		 '())
		((dsssl-named-constant? (car args))
		 '())
		((pair? (car args))
		 (liip (cdr args) (cons (car args) r)))
		(else
		 (liip (cdr args) (cons (list (car args) #f) r))))))
	 ((dsssl-named-constant? (car args))
	  '())
	 (else
	  (loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    dsssl-optionals ...                                              */
;*---------------------------------------------------------------------*/
(define (dsssl-optionals args)
   (dsssl-arguments #!optional args))

;*---------------------------------------------------------------------*/
;*    dsssl-keys ...                                                   */
;*---------------------------------------------------------------------*/
(define (dsssl-keys args)
   (dsssl-key-args-sort (dsssl-arguments #!key args)))

;*---------------------------------------------------------------------*/
;*    dsssl-key-args-sort ...                                          */
;*---------------------------------------------------------------------*/
(define (dsssl-key-args-sort args)
   (sort args
	 (lambda (s1 s2)
	    (string<? (symbol->string (car s1))
		      (symbol->string (car s2))))))

;*---------------------------------------------------------------------*/
;*    dsssl-before-dsssl ...                                           */
;*    -------------------------------------------------------------    */
;*    This function is invoked iff we know that there is an            */
;*    #!optional argument.                                             */
;*---------------------------------------------------------------------*/
(define (dsssl-before-dsssl args)
   (let loop ((args args)
	      (res '()))
      (cond
	 ((dsssl-named-constant? (car args))
	  (reverse! res))
	 (else
	  (loop (cdr args) (cons (car args) res))))))
