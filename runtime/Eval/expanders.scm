;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/runtime/Eval/expanders.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 09:58:05 1994                          */
;*    Last change :  Sun Aug 25 09:13:12 2019 (serrano)                */
;*    Copyright   :  2002-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Expanders installation.                                          */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __install_expanders
   
   (import  __error
	    __macro
	    __expander_quote
	    __expander_let
	    __expander_bool
	    __expander_case
	    __expander_define
	    __expander_do
	    __expander_try
	    __expander_struct
	    __expander_record
	    __expander_srfi0
	    __expander_args
	    __expander_trace
	    __eval
	    __progn
	    __lalr_expand
	    __rgc_expand
	    __match_expand
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    __expand
	    __evmodule
	    __evobject

	    __r5_macro_4_3_syntax)
   
   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __os
	    __rgc
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r5_control_features_6_4
	    
	    __evenv)
   
   (export  (install-all-expanders!)))

;*---------------------------------------------------------------------*/
;*    expand-test ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-test x e)
   (if *nil*
       (e x e)
       `((lambda (test-aux-for-nil)
	    (if test-aux-for-nil
		(if (null? test-aux-for-nil)
		    #f
		    #t)
		#f))
	 ,(e x e)))) 

;*---------------------------------------------------------------------*/
;*    expand-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-error p m x)
   (let ((loc (when (epair? x) (cer x))))
      (if (and (pair? loc) (pair? (cdr loc)) (pair? (cddr loc)))
	  (error/location p m x (cadr loc) (caddr loc))
	  (error p m x))))

;*---------------------------------------------------------------------*/
;*    *expanders-installed-p* ...                                      */
;*---------------------------------------------------------------------*/
(define *expanders-installed-p* #f)

;*---------------------------------------------------------------------*/
;*    install-all-expanders! ...                                       */
;*---------------------------------------------------------------------*/
(define (install-all-expanders!)
   (unless *expanders-installed-p*
      (set! *expanders-installed-p* #t)
      (%install-all-expanders!)))

;*---------------------------------------------------------------------*/
;*    %install-all-expanders! ...                                      */
;*    -------------------------------------------------------------    */
;*    !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING !!!  */
;*    -------------------------------------------------------------    */
;*    It is mandatory to define the closure associated to expanders    */
;*    in *this* module in order to avoid module initialization         */
;*    order problems.                                                  */
;*---------------------------------------------------------------------*/
(define (%install-all-expanders!)
   
;*---------------------------------------------------------------------*/
;*    Expanders shared by the compiler and the interpreter             */
;*---------------------------------------------------------------------*/
   ;; quote
   (install-expander 'quote (lambda (x e) (expand-quote x e)))

   ;; @
   (install-expander '@ (lambda (x e)
			   (match-case x
			      ((@ (? symbol?) (? symbol?))
			       x)
			      (else
			       (expand-error "@" "Illegal form" x)))))
   ;; ->
   (install-expander '-> (lambda (x e)
			    (if (every symbol? x)
				(match-case x
				   ((?- ?v . ?rest)
				    (let ((nv (e v e)))
				       (match-case nv
					  ((-> . ?mv)
					   (evepairify `(-> ,@mv ,@rest) x))
					  (else
					   (set-car! (cdr x) nv)
					   x))))
				   (else
				    (expand-error "->" "Illegal form" x)))
				(expand-error "->" "Illegal form" x))))
   ;; quasiquote
   (install-expander 'quasiquote (lambda (x e) (e (quasiquotation 1 x) e)))
   
   ;; module
   (install-expander 'module (lambda (x e) x))
   
   ;; define-macro  
   (install-expander 'define-macro (lambda (x e)
				      (expand-define-macro x e)))
   
   ;; define-hygiene-macro  
   (install-expander 'define-hygiene-macro (lambda (x e)
					      (expand-define-hygiene-macro x e)))
   
   ;; define-expander
   (install-expander 'define-expander (lambda (x e)
					 (expand-define-expander x e)))
   
   ;; cond
   (install-expander 'cond (lambda (x e) (e (expand-cond x) e)))
   
   ;; do
   (install-expander 'do (lambda (x e) (expand-do x e)))
   
   ;; try
   (install-expander 'try (lambda (x e) (expand-try x e)))
   
   ;; match-case
   (install-expander 'match-case (lambda (x e) (e (expand-match-case x) e)))
   
   ;; match-lambda
   (install-expander 'match-lambda (lambda (x e)
				      (e (expand-match-lambda x) e)))
   
   ;; define-pattern
   (install-expander 'define-pattern (lambda (x e)
					(e (expand-define-pattern x) e)))
   
   ;; delay
   (install-expander 'delay (lambda (x e)
			       (match-case x
				  ((?- ?exp)
				   `(make-promise (lambda () ,(e exp e))))
				  (else
				   (expand-error "delay" "Illegal form" x)))))
   ;; regular-grammar
   (install-expander 'regular-grammar expand-regular-grammar)
   
   ;; string-case
   (install-expander 'string-case expand-string-case)
   
   ;; lalr-grammar
   (install-expander 'lalr-grammar expand-lalr-grammar)
   
   ;; begin
   (install-expander 'begin expand-begin)
   
   ;; failure
   (install-expander 'failure (lambda (x e)
				 (match-case x
				    ((?- ?proc ?msg ?obj)
				     `(failure ,(e proc e)
					       ,(e msg e)
					       ,(e obj e)))
				    (else
				     (expand-error "failure"
						   "Illegal `failure' form"
						   x)))))
   
   ;; receive
   (install-expander 'receive
		     (lambda (x e)
			(match-case x
			   ((?- ?vars ?call . ?exprs)
			    (e `(multiple-value-bind  ,vars ,call ,@exprs) e))
			   (else
			    (expand-error "receive"
					  "Illegal form"
					  x)))))
   
   ;; when
   (install-expander 'when
		     (lambda (x e)
			(match-case x
			   ((?- ?si . ?body)
			    (e `(if ,si
				    (begin ,@body)
				    #f)
			       e))
			   (else
			    (expand-error "when" "Illegal form" x)))))
   
   ;; unless
   (install-expander 'unless
		     (lambda (x e)
			(match-case x
			   ((?- ?si . ?body)
			    (e `(if ,si
				    #f
				    (begin ,@body))
			       e))
			   (else
			    (expand-error "unless" "Illegal form" x)))))
   ;; define-record-type
   (install-expander 'define-record-type expand-define-record-type)
   
   ;; args-parse
   (install-expander 'args-parse expand-args-parse)
   
   ;; tprint
   (install-expander 'tprint expand-tprint)
   
   ;; and-let*
   (install-expander 'and-let* expand-and-let*)
   
   ;; define-syntax
   (install-expander 'define-syntax expand-define-syntax)
   (install-expander 'letrec-syntax expand-letrec-syntax)
   (install-expander 'let-syntax expand-let-syntax)

;*---------------------------------------------------------------------*/
;*    Compiler macros                                                  */
;*---------------------------------------------------------------------*/
   ;; trace
   (install-compiler-expander 'when-trace (make-expand-when-trace 'compiler))
   (install-compiler-expander 'with-trace (make-expand-with-trace 'compiler))
   (install-compiler-expander 'trace-item (make-expand-trace-item 'compiler))

;*---------------------------------------------------------------------*/
;*    Interpreter macros                                               */
;*---------------------------------------------------------------------*/
   ;; #meta
   (install-eval-expander '|#meta|
      (lambda (x e)
	 (match-case x
	    ((?- (? list?) . ?body)
	     (evepairify
		`(begin ,@(map (lambda (x) (e x e)) body))
		x))
	    (else
	     (expand-error "#meta" "Illegal form" x)))))
   
   ;; bind-exit
   (install-eval-expander 'bind-exit
      (lambda (x e)
	 
	 (define (find-in-body k body)
	    (cond
	       ((eq? body k)
		#t)
	       ((pair? body)
		(unless (eq? (car body) 'quote)
		   (or (find-in-body k (car body))
		       (find-in-body k (cdr body)))))
	       (else #f)))
	 
	 (match-case x
	    ((?- (?exit) . (and ?body (not ())))
	     (evepairify
		(if (find-in-body exit body)
		    `(bind-exit (,exit)
			,(e (expand-progn body) e))
		    (e `(begin ,@body) e))
		x))
	    (else
	     (expand-error "bind-exit" "Illegal form" x)))))
   
   ;; unwind-protect
   (install-eval-expander 'unwind-protect
      (lambda (x e)
	 (match-case x
	    ((?- ?body . ?exp)
	     (evepairify
		`(unwind-protect
		    ,(e body e)
		    ,@(map (lambda (x) (e x e)) exp))
		x))
	    (else
	     (expand-error "unwind-protect" "Illegal form" x)))))
   
   ;; with-handler
   (install-eval-expander 'with-handler
      (lambda (x e)
	 (match-case x
	    ((?- ?handler . ?body)
	     (evepairify
		`(with-handler
		    ,(e handler e)
		    ,@(map (lambda (x) (e x e))
			 body))
		x))
	    (else
	     (expand-error "with-handler" "Illegal form" x)))))
   
   ;; multiple-value-bind
   (install-eval-expander 'multiple-value-bind
      (lambda (x e)
	 (match-case x
	    ((?- (and ??- (? (lambda (x) (every symbol? x))) ?vars)
		?producer . ?exprs)
	     (let* ((tmps (map gensym vars))
		    (tmps2 (map gensym vars))
		    (nx `(let (,@(map (lambda (v) `(,v #unspecified)) tmps))
			    (call-with-values
			       (lambda () ,producer)
			       (lambda ,tmps2
				  ,@(map (lambda (v t) `(set! ,v ,t))
				       tmps tmps2)))
			    (let (,@(map (lambda (v t) `(,v ,t))
				       vars tmps))
			       ,@exprs))))
		(evepairify (e nx e) x)))
	    (else
	     (expand-error "multiple-value-bind" "Illegal form" x)))))
   ;; if
   (install-eval-expander 'if expand-if)
   
   ;; lambda
   (install-eval-expander 'lambda expand-eval-lambda)
   
   ;; let
   (install-eval-expander 'let expand-eval-let)
   
   ;; let*
   (install-eval-expander 'let* expand-eval-let*)
   
   ;; letrec
   (install-eval-expander 'letrec expand-eval-letrec)
   
   ;; letrec
   (install-eval-expander 'letrec* expand-eval-letrec*)
   
   ;; labels
   (install-eval-expander 'labels expand-eval-labels)
   
   ;; define
   (install-eval-expander 'define expand-eval-define)
   
   ;; define-inline
   (install-eval-expander 'define-inline expand-eval-define-inline)
   
   ;; define-generic
   (install-eval-expander 'define-generic expand-eval-define-generic)
   
   ;; define-generic
   (install-eval-expander 'define-method expand-eval-define-method)
   
   ;; define-struct
   (install-eval-expander 'define-struct expand-eval-define-struct)
   
   ;; case
   (install-eval-expander 'case expand-eval-case)
   
   ;; cond-expand
   (install-eval-expander 'cond-expand expand-eval-cond-expand)
   
   ;; profile
   (install-eval-expander 'profile
      (lambda (x e)
	 (match-case x
	    ((?- (and (? symbol?) ?lbl) . ?exprs)
	     (let* ((la  `(lambda () ,@exprs))
		    (lam (if (epair? x)
			     (econs (car la)
				(cdr la)
				(cer x))
			     la))
		    (val (let ((sym (gensym 'value)))
			    sym))
		    (aux `(let ((,lbl ,lam))
			     (GC-profile-push
				,(symbol->string lbl)
				,lbl)
			     (let ((,val (,lbl)))
				(GC-profile-pop)
				,val)))
		    (res (if (epair? x)
			     (econs (car aux)
				(cdr aux)
				(cer x))
			     aux)))
		(e aux e)))
	    (else
	     (expand-error "profile" "Illegal form" x)))))
   
   ;; instantiate
   (install-eval-expander 'instantiate
      (lambda (x e)
	 (match-case x
	    ((?id . ?-)
	     (expand-error id "Unknown class" x)))))
   
   ;; with-access
   (install-eval-expander 'with-access
      (lambda (x e)
	 (match-case x
	    ((?id . ?-)
	     (expand-error id "Unknown class" x)))))

   ;; co-instantiate
   (install-eval-expander 'co-instantiate eval-co-instantiate-expander)
   
   ;; classes
   (let ((e (lambda (x e) (e (evmodule-static-class x) e))))
      (install-eval-expander 'define-class e)
      (install-eval-expander 'define-abstract-class e)
      (install-eval-expander 'define-final-class e))

   ;; trace
   (install-eval-expander 'when-trace (make-expand-when-trace 'eval))
   (install-eval-expander 'with-trace (make-expand-with-trace 'eval))
   (install-eval-expander 'trace-item (make-expand-trace-item 'eval)))

;*---------------------------------------------------------------------*/
;*    expand-if ...                                                    */
;*---------------------------------------------------------------------*/
(define (expand-if x e)
   
   (define (make-if test then else)
      (match-case test
	 (((kwote not) ?test)
	  `(if ,test ,else ,then))
	 (else
	  `(if ,test ,then ,else))))
   
   (match-case x
      ((if ?si ?alors ?sinon)
       (let ((nx (make-if (expand-test si e) (e alors e) (e sinon e))))
	  (evepairify-deep nx x)))
      ((if ?si ?alors)
       (let ((nx (make-if (expand-test si e) (e alors e) #f)))
	  (evepairify-deep nx x)))
      (else
       (expand-error "if" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-tprint ...                                                */
;*---------------------------------------------------------------------*/
(define (expand-tprint x e)
   (set-car! x '(@ tprint __r4_output_6_10_3))
   (e (if (epair? x)
	  (match-case (cer x)
	     ((at ?name ?pos)
	      (set-cdr! x
			(cons* '(current-error-port)
			       (relative-file-name name (pwd))
			       ","
			       (file-position->line pos name)
			       ":"
			       (cdr x)))
	      x)
	     (else
	      (set-cdr! x (cons '(current-error-port) (cdr x)))
	      x))
	  (begin
	     (set-cdr! x (cons '(current-error-port) (cdr x)))
	     x))
      e))

;*---------------------------------------------------------------------*/
;*    expand-begin ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-begin x e)
   (match-case x
      ((?- . ?body)
       (if (not (list? body))
	   (expand-error "begin" "Illegal form" x)
	   (expand-progn (map (lambda (x) (e x e)) body))))
      (else
       (expand-error "begin" "Illegal form" x))))

;*---------------------------------------------------------------------*/
;*    expand-and-let* ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-and-let* x e)
   (match-case x
      ((?- ?claws . ?body)
       (let* ((new-vars '()) (result (cons 'and '())) (growth-point result))
	  (define (andjoin! clause)
	     (let ((prev-point growth-point) (clause-cell (cons clause '())))
		(set-cdr! growth-point clause-cell)
		(set! growth-point clause-cell)))
	  (if (not (list? claws))
	      (expand-error #f "bindings must be a list" claws))
	  (for-each
	   (lambda (claw)
	      (cond
		 ((symbol? claw)
		  ;; BOUND-VARIABLE form
		  (andjoin! claw))
		 ((and (pair? claw) (null? (cdr claw)))
		  ;; (EXPRESSION) form
		  (andjoin! (car claw)))
		 ;; (VARIABLE EXPRESSION) form
		 ((and (pair? claw) (symbol? (car claw))
		       (pair? (cdr claw)) (null? (cddr claw)))
		  (let* ((var (car claw)) (var-cell (cons var '())))
		     (if (memq var new-vars)
			 (expand-error "let"
				       "duplicate variable in the bindings"
				       var))
		     (set! new-vars (cons var new-vars))
		     (set-cdr! growth-point `((let (,claw) (and . ,var-cell))))
		     (set! growth-point var-cell)))
		 (else
		  (expand-error "let" "ill-formed binding" claw))))
	   claws)
	  (if (not (null? body)) (andjoin! `(begin ,@body)))
	  (evepairify (e result e) x)))
      (else
       (expand-error "let" "Illegal `and-let*' form" x))))

;*---------------------------------------------------------------------*/
;*    Force the install of the expanders                               */
;*    -------------------------------------------------------------    */
;*    This initialization is required for module initialization order. */
;*    This module must be initialized before evprimop.scm (otherwise   */
;*    classes instantiate:: and with-access:: are not operational).    */
;*---------------------------------------------------------------------*/
(install-all-expanders!)
