;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/control.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 20 17:48:44 1995                          */
;*    Last change :  Tue Feb  6 08:29:46 2018 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.9. Control features (page 27, r4)                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_control_features_6_9
   
   (import  __error
	    __param
	    __evaluate)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __bignum
	    
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_strings_6_7
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r5_control_features_6_4
	    
	    __evenv)
   
   (extern  (macro c-procedure?::bool (::obj) "PROCEDUREP")
	    (call-cc::obj (::procedure) "call_cc")
	    
	    (macro push-before!::obj (::procedure) "PUSH_BEFORE")
	    (macro pop-before!::obj () "POP_BEFORE"))
   
   (java    (class foreign
	       (method static c-procedure?::bool (::obj) "PROCEDUREP")
	       ;(method static call-cc::obj (::procedure) "call_cc")
	       
	       (method static push-before!::obj (::procedure) "PUSH_BEFORE")
	       (method static pop-before!::obj () "POP_BEFORE")))
   
   (export  (inline procedure?::bool ::obj)
	    (apply ::procedure obj1 . args)
	    (map::pair-nil ::procedure . pair)
	    (map!::pair-nil ::procedure . pair)
	    (map-2::pair-nil ::procedure ::pair-nil)
	    (append-map::pair-nil ::procedure . pair)
	    (append-map!::pair-nil ::procedure . pair)
	    (filter-map::pair-nil ::procedure . pair)
	    (for-each ::procedure . pair)
	    (for-each-2 ::procedure ::pair-nil)
	    (filter::pair-nil ::procedure ::pair-nil)
	    (filter!::pair-nil ::procedure ::pair-nil)
	    (inline force promise)
	    (make-promise ::procedure)
	    (call/cc ::procedure)
	    (inline call-with-current-continuation ::procedure)
	    (dynamic-wind ::procedure ::procedure ::procedure))
   
   (pragma  (c-procedure? (predicate-of procedure) no-cfa-top nesting)
	    (procedure? side-effect-free no-cfa-top nesting)))

;*---------------------------------------------------------------------*/
;*    procedure? ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (procedure? obj)
   (c-procedure? obj))

;*---------------------------------------------------------------------*/
;*    apply ...                                                        */
;*---------------------------------------------------------------------*/
(define (apply proc args . opt)
   (let ((args (if (pair? opt)
		   (cons args (let loop ((opt opt))
				 (if (pair? (cdr opt))
				     (cons (car opt) (loop (cdr opt)))
				     (car opt))))
		   args)))
      (apply proc args)))

;*---------------------------------------------------------------------*/
;*    map-2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (map-2 f l)
   (let loop ((l   l)
	      (res '()))
      (if (null? l)
	  (reverse! res)
	  (loop (cdr l) (cons (f (car l)) res)))))

;*---------------------------------------------------------------------*/
;*    map ...                                                          */
;*---------------------------------------------------------------------*/
(define (map f . l)
   (cond
      ((null? l)
       '())
      ((null? (cdr l))
       (map-2 f (car l)))
      (else
       (let loop ((l l))
	  (if (null? (car l))
	      '()
	      (cons (apply f (map-2 car l))
		    (loop (map-2 cdr l))))))))

;*---------------------------------------------------------------------*/
;*    map-2! ...                                                       */
;*---------------------------------------------------------------------*/
(define (map-2! f l0)
   (let loop ((l l0))
      (if (null? l)
	  l0
	  (begin
	     (set-car! l (f (car l)))
	     (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    map! ...                                                         */
;*---------------------------------------------------------------------*/
(define (map! f . l)
   (cond
      ((null? l)
       '())
      ((null? (cdr l))
       (map-2! f (car l)))
      (else
       (let ((l0 (car l)))
	  (let loop ((l l))
	     (if (null? (car l))
		 l0
		 (begin
		    (set-car! (car l) (apply f (map-2 car l)))
		    (loop (map-2 cdr l)))))))))

;*---------------------------------------------------------------------*/
;*    append-map2 ...                                                  */
;*---------------------------------------------------------------------*/
(define (append-map2 f l)
   (let loop ((l l))
      (if (null? l)
	  '()
	  (append (f (car l)) (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    append-map ...                                                   */
;*---------------------------------------------------------------------*/
(define (append-map f . l)
   (cond
      ((null? l)
       '())
      ((null? (cdr l))
       (append-map2 f (car l)))
      (else
       (let loop ((l l))
	  (if (null? (car l))
	      '()
	      (append (apply f (map-2 car l)) (loop (map-2 cdr l))))))))

;*---------------------------------------------------------------------*/
;*    append-map2! ...                                                 */
;*---------------------------------------------------------------------*/
(define (append-map2! f l)
   (let loop ((l l))
      (if (null? l)
	  '()
	  (append! (f (car l)) (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    append-map! ...                                                  */
;*---------------------------------------------------------------------*/
(define (append-map! f . l)
   (cond
      ((null? l)
       '())
      ((null? (cdr l))
       (append-map2! f (car l)))
      (else
       (let loop ((l l))
	  (if (null? (car l))
	      '()
	      (append! (apply f (map-2 car l)) (loop (map-2 cdr l))))))))

;*---------------------------------------------------------------------*/
;*    filter-map-2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (filter-map-2 f l)
   (let loop ((l   l)
	      (res '()))
      (if (null? l)
	  (reverse! res)
	  (let ((hd (f (car l))))
	     (if hd
		 (loop (cdr l) (cons hd res))
		 (loop (cdr l) res))))))

;*---------------------------------------------------------------------*/
;*    filter-map ...                                                   */
;*---------------------------------------------------------------------*/
(define (filter-map f . l)
   (cond
      ((null? l)
       '())
      ((null? (cdr l))
       (filter-map-2 f (car l)))
      (else
       (let loop ((l l))
	  (if (null? (car l))
	      '()
	      (let ((hd (apply f (map-2 car l))))
		 (if hd
		     (cons hd (loop (map-2 cdr l)))
		     (loop (map-2 cdr l)))))))))

;*---------------------------------------------------------------------*/
;*    for-each-2 ...                                                   */
;*---------------------------------------------------------------------*/
(define (for-each-2 f l)
   (let loop ((l l))
      (if (null? l)
	  #unspecified
	  (begin
	     (f (car l))
	     (loop (cdr l))))))

;*---------------------------------------------------------------------*/
;*    for-each ...                                                     */
;*---------------------------------------------------------------------*/
(define (for-each f . l)
   (cond
      ((null? l)
       #unspecified)
      ((null? (cdr l))
       (for-each-2 f (car l)))
      (else
       (let loop ((l l))
	  (if (null? (car l))
	      #unspecified
	      (begin
		 (apply f (map-2 car l))
		 (loop (map-2 cdr l))))))))

;*---------------------------------------------------------------------*/
;*    filter ...                                                       */
;*---------------------------------------------------------------------*/
(define (filter pred l)
   (let ((hook (cons #f '())))
      (let loop ((l l)
		 (h hook))
	 (cond
	    ((null? l)
	     (cdr hook))
	    ((pred (car l))
	     (let ((nh (cons (car l) '())))
		(set-cdr! h nh)
		(loop (cdr l) nh)))
	    (else
	     (loop (cdr l) h))))))

;*---------------------------------------------------------------------*/
;*    filter! ...                                                      */
;*---------------------------------------------------------------------*/
(define (filter! pred lis)
   (let lp ((ans lis))
      (cond
	 ((null? ans)
	  ans)
	 ((not (pred (car ans)))
	  (lp (cdr ans)))
	 (else
	  (letrec ((scan-in (lambda (prev lis)
			       (if (pair? lis)
				   (if (pred (car lis))
				       (scan-in lis (cdr lis))
				       (scan-out prev (cdr lis))))))
		   (scan-out (lambda (prev lis)
				(let lp ((lis lis))
				   (if (pair? lis)
				       (if (pred (car lis))
					   (begin
					      (set-cdr! prev lis)
					      (scan-in lis (cdr lis)))
					   (lp (cdr lis)))
				       (set-cdr! prev lis))))))
	     (scan-in ans (cdr ans))
	     ans)))))

;*---------------------------------------------------------------------*/
;*    force ...                                                        */
;*---------------------------------------------------------------------*/
(define-inline (force promise)
   (promise))

;*---------------------------------------------------------------------*/
;*    make-promise ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-promise proc)
   (let ((result-ready? #f)
	 (result        #f))
      (lambda ()
	 (if result-ready?
	     result
	     (let ((x (proc)))
		(if result-ready?
		    result
		    (begin
		       (set! result-ready? #t)
		       (set! result x)
		       result))))))) 

;*---------------------------------------------------------------------*/
;*    call/cc ...                                                      */
;*---------------------------------------------------------------------*/
(define (call/cc proc)
   (call-cc (lambda (cont)
	       (let ((evc (get-evaluation-context)))
		  (proc (lambda vals
			   (set-evaluation-context! evc)
			   (if (and (pair? vals) (null? (cdr vals)))
			       (cont (car vals))
			       (begin
				  (%set-mvalues-number! -1)
				  (cont vals)))))))))

(cond-expand
   (bigloo-jvm
    (define (call-cc proc) (bind-exit (exit) (proc exit)))))

;*---------------------------------------------------------------------*/
;*    call-with-current-continuation ...                               */
;*---------------------------------------------------------------------*/
(define-inline (call-with-current-continuation proc)
   (call/cc proc))

;*---------------------------------------------------------------------*/
;*    dynamic-wind ...                                                 */
;*---------------------------------------------------------------------*/
(define (dynamic-wind before::procedure thunk::procedure after::procedure)
   (before)
   (let ()
      (push-before! before)
      (unwind-protect
	 (thunk)
	 (begin
	    (after)
	    (pop-before!)))))
