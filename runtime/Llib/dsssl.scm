;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/dsssl.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul  3 11:30:29 1997                          */
;*    Last change :  Sun Aug 25 09:08:52 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bigloo support for Dsssl (Iso/Iec 10179:1996)                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __dsssl
   
   (import  __error
	    __param
	    __bexit
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __bit
	    
	    __r4_output_6_10_3
	    __r4_ports_6_10_1
	    
	    __r4_control_features_6_9
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_characters_6_6
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_symbols_6_4
	    
	    __evenv)
   
   (export  (dsssl-named-constant?::bool ::obj)
	    (make-dsssl-function-prelude ::obj ::obj ::obj ::procedure)
	    (dsssl-get-key-arg ::obj ::keyword ::obj)
	    (dsssl-get-key-rest-arg ::obj ::pair-nil)
	    (dsssl-check-key-args! ::obj ::obj)
	    (dsssl-formals->scheme-formals ::obj ::procedure)
	    (dsssl-formals->scheme-typed-formals ::obj ::procedure ::bool)))
	    
;*---------------------------------------------------------------------*/
;*    dsssl-named-constant? ...                                        */
;*    -------------------------------------------------------------    */
;*    Is an object a dsssl named constant (#!optional, #!key or        */
;*    #!rest) ?                                                        */
;*---------------------------------------------------------------------*/
(define (dsssl-named-constant? obj)
   (and (cnst? obj) (memq obj '(#!rest #!optional #!key))))

;*---------------------------------------------------------------------*/
;*    make-dsssl-function-prelude ...                                  */
;*    -------------------------------------------------------------    */
;*    This function decodes a DSSSL formal parameter list and          */
;*    produce a header decoding the actual values.                     */
;*    -------------------------------------------------------------    */
;*    It implements a finite automata where each state is represented  */
;*    by a function.                                                   */
;*---------------------------------------------------------------------*/
(define (make-dsssl-function-prelude where formals body err)

   (define (scheme-state args)
      (cond
	 ((not (pair? args))
	  body)
	 ((and (not (symbol? (car args))) (not (pair? (car args))))
	  ;; either it is a DSSSL named constant or an error.
	  ;; (for bootstrapping ease, don't use CASE that uses the cnst index)
	  (cond 
	     ((eq? (car args) #!optional)
	      (enter-dsssl-state (cdr args) optional-state))
	     ((eq? (car args) #!rest)
	      (enter-dsssl-state (cdr args) rest-state))
	     ((eq? (car args) #!key)
	      (enter-dsssl-state (cdr args) no-rest-key-state))
	     (else
	      (err where "Illegal formal list.1" (cons (car args) formals)))))
	 (else
	  ;; regular Scheme formal, we simply skip
	  (scheme-state (cdr args)))))

   (define (enter-dsssl-state args next-state)
      (let loop ((as args))
	 (cond
	    ((not (pair? as))
	     (err where "Illegal formal list.2" (cons as formals)))
	    (else
	     (match-case (car as)
		((? symbol?)
		 (let ((dsssl-arg (gensym 'dsssl)))
		    `(let ((,dsssl-arg ,(car as)))
			,(next-state args dsssl-arg))))
		(((? symbol?) ?-)
		 (let ((dsssl-arg (gensym 'dsssl)))
		    `(let ((,dsssl-arg ,(car (car as))))
			,(next-state args dsssl-arg))))
		(else
		 (err where "Illegal formal list.3" (cons (car as) formals))))))))

   (define (optional-state args dsssl-arg)
      
      (define (get-keyword-arguments args)
	 (let loop ((args args))
	    (cond
	       ((not (pair? args))
		'())
	       ((eq? (car args) '#!key)
		(let loop ((args (cdr args))
			   (res '()))
		   (cond
		      ((or (not (pair? args))
			   (not (or (pair? (car args))
				    (symbol? (car args))))
			   (eq? (car args) '#!optional)
			   (eq? (car args) '#!rest))
		       res)
		      ((symbol? (car args))
		       (loop (cdr args)
			     (cons (symbol->keyword (car args)) res)))
		      (else
		       (loop (cdr args)
			     (cons (symbol->keyword (caar args)) res))))))
	       (else
		(loop (cdr args))))))

      (define keyword-arguments (get-keyword-arguments args))
		
      (define (one-optional-arg arg initializer rest)
	 (let ((tmp (gensym 'tmp)))
	    `(let ((,arg (if (if (null? ,dsssl-arg)
				 #t
				 (memq (car ,dsssl-arg) ',keyword-arguments))
			     ,initializer
			     (let ((,tmp (car ,dsssl-arg)))
				;; MS: 30 sep 2008
				;; Don't forget the explicit begin because
				;; the DSSSL code is no longer post
				;; macro-expanded by eval (for avoiding
				;; duplicated macro-expansion of all function
				;; definitions).
				(begin
				   (set! ,dsssl-arg (cdr ,dsssl-arg))
				   ,tmp)))))
		,(optional-args rest))))

      (define (optional-args args)
	 (cond
	    ((null? args)
	     body)
	    ((not (pair? args))
	     (err where "Illegal DSSSL formal list (#!optional)" formals))
	    ((and (not (symbol? (car args))) (not (pair? (car args))))
	     ;; either it is a DSSSL named constant or an error.
	     ;; (for bootstrapping ease, don't use CASE but COND)
	     (cond
		((eq? (car args) #!rest)
		 (rest-state (cdr args) dsssl-arg))
		((eq? (car args) #!key)
		 (no-rest-key-state (cdr args) dsssl-arg))
		(else
		 (err where "Illegal DSSSL formal list (#!optional)" formals))))
	    (else
	     ;; an optional DSSSL formal
	     (match-case (car args)
		(((and (? symbol?) ?arg) ?initializer)
		 (one-optional-arg arg initializer (cdr args)))
		((and (? symbol?) ?arg)
		 (one-optional-arg arg #f (cdr args)))
		(else
		 (err where "Illegal DSSSL formal list (#!optional)" formals))))))

      (optional-args args))

   (define (rest-state args dsssl-arg)
      (cond
	 ((not (pair? args))
	  (err where "Illegal DSSSL formal list (#!rest)" formals))
	 ((not (symbol? (car args)))
	  (err where "Illegal DSSSL formal list (#!rest)" formals))
	 (else
	  `(let ((,(car args) ,dsssl-arg))
	      ,(exit-rest-state (cdr args) dsssl-arg)))))
      
   (define (exit-rest-state args dsssl-arg)
      (cond
	 ((null? args)
	  body)
	 ((not (pair? args))
	  (err where "Illegal DSSSL formal list (#!rest)" formals))
	 ((eq? (car args) #!key)
	  (rest-key-state (cdr args) dsssl-arg))
	 (else
	  (err where "Illegal DSSSL formal list (#!rest)" formals))))

   (define (rest-key-state args dsssl-arg)
      
      (define (get-keyword-arguments args)
	 (map (lambda (x)
		 (cond
		    ((and (pair? x) (symbol? (car x)))
		     (symbol->keyword (car x)))
		    ((symbol? x)
		     (symbol->keyword x))
		    (else
		     (err where "Illegal #!keys parameters" formals))))
	    args))
      
      (cond
	 ((null? args)
	  (err where "Illegal DSSSL formal list (#!key)" formals))
	 (else
	  (let ((keys (get-keyword-arguments args)))
	     (if (null? keys)
		 (err where "Illegal DSSSL formal list (#!key)" formals)
		 (key-state args dsssl-arg '() #f))))))
   
   (define (no-rest-key-state args dsssl-arg)
      
      (define (get-keyword-arguments args)
	 (let loop ((args args)
		    (aux '()))
	    (cond
	       ((null? args)
		(reverse! aux))
	       ((eq? (car args) #!rest)
		(reverse! aux))
	       (else
		(match-case (car args)
		   ((and (? symbol?) ?arg)
		    (loop (cdr args) (cons (symbol->keyword arg) aux)))
		   (((and (? symbol?) ?arg) ?-)
		    (loop (cdr args) (cons (symbol->keyword arg) aux)))
		   (else
		    (err where "Illegal DSSSL formal list (#!key)" formals)))))))
      
      (cond
	 ((null? args)
	  (err where "Illegal DSSSL formal list (#!key)" formals))
	 (else
	  (let ((keys (get-keyword-arguments args)))
	     (if (null? keys)
		 (err where "Illegal DSSSL formal list (#!key)" formals)
		 (key-state args dsssl-arg '() #t))))))
   
   (define (key-state args dsssl-arg collected-keys allow-restp)
      
      (define (one-key-arg arg initializer collected-keys)
	 `(let ((,arg (dsssl-get-key-arg ,dsssl-arg
			 ,(symbol->keyword arg) ,initializer)))
	     ,(key-state (cdr args)
		 dsssl-arg
		 (cons (symbol->keyword arg) collected-keys)
		 allow-restp)))
      
      (define (rest-key-arg arg body)
	 `(let ((,arg (dsssl-get-key-rest-arg ,dsssl-arg ',collected-keys)))
	     ,body))
      
      (cond
	 ((null? args)
	  (if allow-restp
	      ;; no #!rest before the #!key, check that everything is bound
	      `(if (null? (dsssl-get-key-rest-arg ,dsssl-arg ',collected-keys))
		   ,body
		   (error "dsssl-get-key-arg"
		      (apply string-append "Illegal extra key arguments: "
			 (map (lambda (v)
				 (format "~a " v))
			    (dsssl-get-key-rest-arg ,dsssl-arg ',collected-keys)))
		      ,dsssl-arg))
	      ;; a #!rest was found before the #!key, accept everthing
	      body))
	 ((eq? (car args) #!rest)
	  (if (or (not allow-restp)
		  (null? (cdr args))
		  (not (symbol? (cadr args)))
		  (pair? (cddr args)))
	      (err where "Illegal DSSSL formal list (#!rest)" formals)
	      (rest-key-arg (cadr args) body)))
	 ((not (pair? args))
	  (err where "Illegal DSSSL formal list (#!key)" formals))
	 ((and (not (symbol? (car args))) (not (pair? (car args))))
	  (err where "Illegal DSSSL formal list (#!key)" formals))
	 (else
	  ;; an optional DSSSL formal
	  (match-case (car args)
	     (((and (? symbol?) ?arg) ?initializer)
	      (one-key-arg arg initializer collected-keys))
	     ((and (? symbol?) ?arg)
	      (one-key-arg arg #f collected-keys))
	     (else
	      (err where "Illegal DSSSL formal list (#!key)" formals))))))
   
   (scheme-state formals))
   
;*---------------------------------------------------------------------*/
;*    dsssl-check-key-args! ...                                        */
;*    -------------------------------------------------------------    */
;*    This function checks that dsssl args are, at runtime,            */
;*    correctly formed. That is, the dsssl-args variable must hold     */
;*    a serie of pairs where the first element is a keyword.           */
;*    Furthermore, if key-list is non-nil, we check that for each      */
;*    pair, if the key is present in key-list.                         */
;*---------------------------------------------------------------------*/
(define (dsssl-check-key-args! dsssl-args key-list)
   (if (null? key-list)
       (let loop ((args dsssl-args))
	  (cond
	     ((null? args)
	      dsssl-args)
	     ((or (not (pair? args))
		  (null? (cdr args))
		  (not (keyword? (car args))))
	      (error "dsssl formal parsing"
		     "Unexpected #!keys parameters"
		     args))
	     (else
	      (loop (cddr args)))))
       (let loop ((args dsssl-args)
		  (armed #f)
		  (opts '()))
	  (cond
	     ((null? args)
	      (reverse! opts))
	     ((or (not (pair? args))
		  (null? (cdr args))
		  (not (keyword? (car args)))
		  (not (memq (car args) key-list)))
	      (if (not armed)
		  (loop (cdr args) armed opts)
		  (loop (cdr args)
			#f
			(cons (car args) opts))))
	     (else
	      (loop (cddr args) #t opts))))))
   
;*---------------------------------------------------------------------*/
;*    dsssl-get-key-arg ...                                            */
;*    -------------------------------------------------------------    */
;*    dsssl args have already been tested. We know for sure that       */
;*    it is a serie of pairs where first elements are keywords.        */
;*---------------------------------------------------------------------*/
(define (dsssl-get-key-arg dsssl-args keyword initializer)
   (let loop ((args dsssl-args))
      (cond
	 ((not (pair? args))
	  (if (null? args)
	      initializer
	      (error "dsssl-get-key-arg"
		 "Illegal DSSSL arguments" dsssl-args)))
	 ((not (keyword? (car args)))
	  (loop (cdr args)))
	 ((eq? (car args) keyword)
	  (if (not (pair? (cdr args)))
	      (error "dsssl-get-key-arg"
		     "Keyword argument misses value"
		     (car args))
	      (cadr args)))
	 ((not (keyword? (car args)))
	  (error "dsssl-get-key-arg"
	     "Illegal keyword actual value"
	     (car args)))
	 (else
	  (if (not (pair? (cdr args)))
	      (error "dsssl-get-key-arg"
		     "Keyword argument misses value"
		     (car args))
	      (loop (cddr args)))))))
   
;*---------------------------------------------------------------------*/
;*    dsssl-get-key-rest-arg ...                                       */
;*---------------------------------------------------------------------*/
(define (dsssl-get-key-rest-arg dsssl-args keys)
   (let loop ((args dsssl-args))
      (cond
	 ((null? args)
	  '())
	 ((or (not (keyword? (car args)))
	      (null? (cdr args))
	      (not (memq (car args) keys)))
	  (cons (car args) (loop (cdr args))))
	 (else
	  (loop (cddr args))))))
   
;*---------------------------------------------------------------------*/
;*    id-sans-type ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function remove the type from an identifier. Thas is,       */
;*    provided the symbol `id::type', it returns `id'.                 */
;*---------------------------------------------------------------------*/
(define (id-sans-type::symbol id::symbol)
   (let* ((string (symbol->string id))
	  (len    (string-length string)))
      (let loop ((walker  0))
	 (cond
	    ((=fx walker len)
	     id)
	    ((and (char=? (string-ref string walker) #\:)
		  (<fx walker (-fx len 1))
		  (char=? (string-ref string (+fx walker 1)) #\:))
	     (string->symbol (substring string 0 walker)))
	    (else
	     (loop (+fx walker 1)))))))

;*---------------------------------------------------------------------*/
;*    dsssl-formals->scheme-formals ...                                */
;*---------------------------------------------------------------------*/
(define (dsssl-formals->scheme-formals formals err)
   (dsssl-formals->scheme-typed-formals formals err #f))

;*---------------------------------------------------------------------*/
;*    dsssl-formals->scheme-formals ...                                */
;*    -------------------------------------------------------------    */
;*    This function parses a formal arguments list and removes         */
;*    the DSSSL named constant in order to construct a regular Scheme  */
;*    formal parameter list.                                           */
;*      eg:   x y #!optional z #!rest r #!key k -> x y . z             */
;*    If the argument typed is true, fixed argument are left typed.    */
;*    Otherwise, they are untyped.                                     */
;*    -------------------------------------------------------------    */
;*    This function does not check the whole correctness of the        */
;*    formal parameters list. It only checks until the first           */
;*    DSSSL formal parameter is found.                                 */
;*---------------------------------------------------------------------*/
(define (dsssl-formals->scheme-typed-formals formals err typed)
   
   (define (dsssl-named-constant? obj)
      (memq obj '(#!optional #!rest #!key)))

   (define (dsssl-defaulted-formal? obj)
      (and (pair? obj)
	   (pair? (cdr obj))
	   (null? (cddr obj))))

   (define (dsssl-default-formal obj)
      (car obj)) 

   (let loop ((args  formals)
	      (dsssl #f))
      (cond
	 ((null? args)
	  '())
	 ((not (pair? args))
	  (cond
	     (dsssl
	      (err "Can't use both DSSSL named constant"
		   "and `.' notation"
		   formals))
	     ((not (symbol? args))
	      (err "Illegal formal parameter" "symbol expected" formals))
	     (else
	      (id-sans-type args))))
	 ((not (symbol? (car args)))
	  (cond
	     ((dsssl-named-constant? (car args))
	      (loop (cdr args) #t))
	     ((not dsssl)
	      (err "Illegal formal parameter" "symbol expected" formals))
	     ((dsssl-defaulted-formal? (car args))
	      (id-sans-type (dsssl-default-formal (car args))))
	     (else
	      (err "Illegal formal parameter"
		   "symbol or named constant expected"
		   formals))))
	 (dsssl
	  (id-sans-type (car args)))
	 (else
	  (cons (if typed (car args) (id-sans-type (car args)))
		(loop (cdr args) #f))))))

