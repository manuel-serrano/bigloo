;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/args.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 09:49:59 1994                          */
;*    Last change :  Fri Jul  8 08:52:13 2016 (serrano)                */
;*    Copyright   :  1994-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Tools function for managing parameters.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module tools_args
   (import tools_dsssl)
   (export (global-arity::long exp)
	   (local-arity::long exp)
	   (foreign-arity::long exp)
	   (dsssl-arity-zero?::bool ::int ::pair-nil)
	   (make-args-list args nil cons)
	   (args*->args-list exp)
	   (args-list->args* list arity)
	   (sound-arity?::bool arity args)
	   (make-n-proto n)
	   (+-arity arity <integer>)))

;*---------------------------------------------------------------------*/
;*    global-arity ...                                                 */
;*    args-list --> int                                                */
;*    -------------------------------------------------------------    */
;*    (lambda n ..)               --> -1                               */
;*    (lambda () ..)              -->  0                               */
;*    (lambda (a1 a2 .. an) ..)   -->  n                               */
;*    (lambda (a1 a2 .. . an) ..) --> -n                               */
;*---------------------------------------------------------------------*/
(define (global-arity args)
   (let loop ((i 0)
	      (a args))
      (cond
	 ((null? a)
	  i)
	 ((pair? a)
	  (if (dsssl-named-constant? (car a))
	      (dsssl-arity args #t)
	      (loop (+fx i 1) (cdr a))))
	 (else
	  (negfx (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    local-arity ...                                                  */
;*    args-list --> int                                                */
;*    -------------------------------------------------------------    */
;*    (lambda n ..)               --> -1                               */
;*    (lambda () ..)              -->  0                               */
;*    (lambda (a1 a2 .. an) ..)   -->  n                               */
;*    (lambda (a1 a2 .. . an) ..) --> -n                               */
;*---------------------------------------------------------------------*/
(define (local-arity args)
   (let loop ((i 0)
	      (a args))
      (cond
	 ((null? a)
	  i)
	 ((pair? a)
	  (if (dsssl-named-constant? (car a))
	      (dsssl-arity args #f)
	      (loop (+fx i 1) (cdr a))))
	 (else
	  (negfx (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    foreign-arity ...                                                */
;*    args-list --> int                                                */
;*    -------------------------------------------------------------    */
;*    (lambda n ..)               --> -1                               */
;*    (lambda () ..)              -->  0                               */
;*    (lambda (a1 a2 .. an) ..)   -->  n                               */
;*    (lambda (a1 a2 .. . an) ..) --> -n                               */
;*---------------------------------------------------------------------*/
(define (foreign-arity args)
   (let loop ((i 0)
	      (a args))
      (cond
	 ((null? a)
	  i)
	 ((pair? a)
	  (if (dsssl-named-constant? (car a))
	      (error 'foreign-arity
		     "DSSSL arguments not allowed in foreign"
		     args)
	      (loop (+fx i 1) (cdr a))))
	 (else
	  (negfx (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    dsssl-arity-zero? ...                                            */
;*---------------------------------------------------------------------*/
(define (dsssl-arity-zero? arity args)
   (and (<fx arity 0) (null? args)))

;*---------------------------------------------------------------------*/
;*    args*->args-list ...                                             */
;*    cons* --> list                                                   */
;*---------------------------------------------------------------------*/
(define (args*->args-list exp)
   (cond
      ((null? exp)
       '())
      ((not (pair? exp))
       (list exp))
      (else
       (cons (car exp)
             (args*->args-list (cdr exp))))))

;*---------------------------------------------------------------------*/
;*    args-list->args* ...                                             */
;*---------------------------------------------------------------------*/
(define (args-list->args* lst arity)
   (cond
      ((>=fx arity 0)
       lst)
      ((=fx arity -1)
       (car lst))
      (else
       (let loop ((lst  lst)
		  (arity arity))
	  (if (null? (cdr lst))
	      (if (< arity 0)
		  (car lst)
		  (list lst))
	      (cons (car lst) (loop (cdr lst) (+fx arity 1))))))))
   
;*---------------------------------------------------------------------*/
;*    sound-arity? ...                                                 */
;*---------------------------------------------------------------------*/
(define (sound-arity? arity args)
   (let ((len (length args)))
      (if (>=fx arity 0)
	  (=fx arity len)
	  (<=fx (negfx arity) (+fx len 1)))))

;*---------------------------------------------------------------------*/
;*    make-args-list ...                                               */
;*---------------------------------------------------------------------*/
(define (make-args-list args nil cons)
   (let loop ((args args))
      (if (null? args)
	  nil
	  `(,cons ,(car args) ,(loop (cdr args))))))

;*---------------------------------------------------------------------*/
;*    make-n-proto ...                                                 */
;*    -------------------------------------------------------------    */
;*    On construit un prototype a partir de l'arite.                   */
;*---------------------------------------------------------------------*/
(define (make-n-proto n)
   (define (make-args-name n)
      (string->symbol (string-append "A" (integer->string n))))
   (define (make-va-proto n count)
      (if (=fx n -1)
	  (make-args-name count)
	  (cons (make-args-name count)
		(make-va-proto (+fx n 1) (+fx count 1)))))
   (define (make-fx-proto n count)
      (if (=fx n 0)
	  '()
	  (cons (make-args-name count)
		(make-fx-proto (-fx n 1) (+fx count 1)))))
   (if (<fx n 0)
       (make-va-proto n 0)
       (make-fx-proto n 0)))


;*---------------------------------------------------------------------*/
;*    +-arity ...                                                      */
;*---------------------------------------------------------------------*/
(define (+-arity arity add)
   (if (>=fx arity 0)
       (+fx add arity)
       (-fx arity add)))

       
