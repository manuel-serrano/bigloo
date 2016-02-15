;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/eps.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Dec 28 14:56:58 1994                          */
;*    Last change :  Sun Feb 14 07:33:51 2016 (serrano)                */
;*    Copyright   :  1994-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The macro expanser inspired by:                                  */
;*    Expansion-Passing Style: Beyond Conventional Macro,              */
;*    Dybvig, Friedman & Haynes  -- ACM 1986 (LFP) page 143            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module expand_eps
   (include "Expand/expander.sch"
	    "Tools/trace.sch"
	    "Engine/pass.sch"
	    "Ast/unit.sch")
   (import  tools_speek
	    tools_error
	    tools_progn
	    tools_misc
	    type_type
	    ast_ident
	    ast_private
	    expand_expander
	    (find-location tools_location)
	    engine_param
	    tools_args)
   (export  (with-lexical ::obj ::obj loc ::procedure)
	    (lexical-stack)
	    (add-macro-definition! ::obj)
	    (add-macro-alias! ::symbol ::symbol)
	    (comptime-expand ::obj)
	    (comptime-expand/error ::obj)
	    (comptime-expand-cond-expand-only ::obj)
	    (compile-expand ::obj)
	    (expand-units ::obj)))

;*---------------------------------------------------------------------*/
;*    The user macro list.                                             */
;*---------------------------------------------------------------------*/
(define *macro* '())

;*---------------------------------------------------------------------*/
;*    add-macro! ...                                                   */
;*---------------------------------------------------------------------*/
(define (add-macro-definition! form)
   (set! *macro* (cons form *macro*)))

;*---------------------------------------------------------------------*/
;*    add-macro-alias! ...                                             */
;*---------------------------------------------------------------------*/
(define (add-macro-alias! sym1 sym2)
   (putprop! sym1 'macro-alias-key sym2))

;*---------------------------------------------------------------------*/
;*    *lexical-stack* ...                                              */
;*    -------------------------------------------------------------    */
;*    This global variable allow us to simulate a stack to know        */
;*    when a variable `over-define' a `O-macro'. It is not as          */
;*    beautiful as a extra parameter to `expand' but it is             */
;*    easier to implement.                                             */
;*---------------------------------------------------------------------*/
(define *lexical-stack* '())

;*---------------------------------------------------------------------*/
;*    lexical-stack ...                                                */
;*---------------------------------------------------------------------*/
(define (lexical-stack)
   *lexical-stack*)

;*---------------------------------------------------------------------*/
;*    with-lexical ...                                                 */
;*---------------------------------------------------------------------*/
(define (with-lexical new mark loc thunk)
   (let ((new-id (map (lambda (a) (fast-id-of-id a loc)) new))
	 (old-lexical-stack *lexical-stack*))
      (set! *lexical-stack*
	    (append (map (lambda (o) (cons o mark)) new-id) *lexical-stack*))
      (let ((res (bind-exit (escape)
		    (with-exception-handler
		       (lambda (e)
			  (if (isa? e &error)
			      (begin
				 (user-error-notify e 'expand)
				 (escape #unspecified))
			      (raise e)))
		       thunk))))
	 (set! *lexical-stack* old-lexical-stack)
	 res)))
    
;*---------------------------------------------------------------------*/
;*    expand-units ...                                                 */
;*    -------------------------------------------------------------    */
;*    We expand the user code and the produced codes.                  */
;*---------------------------------------------------------------------*/
(define (expand-units units)
   (pass-prelude "Expand")
   ;; We set all macros definitions seen in include files.
   (for-each (lambda (x)
		(compile-expand (comptime-expand x)))
      (reverse! *macro*))
   ;; imported inlined functions which are not coming from library
   ;; have to be expanded. It is not obliged to perform macro-expansion
   ;; on library functions because they have alredy been expanded.
   (define (handler e)
      (if (isa? e &error)
	  (user-error-notify e 'expand)
	  (raise e)))
   ;; we scan all units
   (for-each (lambda (unit)
		(if (procedure? (unit-sexp* unit))
		    ;; a freezed unit (such as the eval unit)
		    ;; cannot be macro expanser.
		    'nothing
		    (let loop ((src (unit-sexp* unit))
			       (res '()))
		       (if (null? src)
			   (unit-sexp*-set! unit (reverse! res))
			   (match-case (car src)
			      ((define-macro . ?-)
			       (with-exception-handler
				  (lambda (e)
				     (handler e)
				     ''())
				  (lambda ()
				     (comptime-expand (car src))))
			       (loop (cdr src) res))
			      ((define-expander . ?-)
			       (with-exception-handler
				  (lambda (e)
				     (handler e)
				     ''())
				  (lambda ()
				     (comptime-expand (car src))))
			       (loop (cdr src) res))
			      (else
			       (let* ((obody (car src))
				      (nbody (bind-exit (skip)
						(with-exception-handler
						   (lambda (e)
						      (handler e)
						      (if (isa? e &error)
							  (skip ''())))
						   (lambda ()
						      (comptime-expand obody))))))
				  (loop (cdr src) (cons nbody res)))))))))
      units)
   ;; in a second time, we apply compile (i.e., optim/debug macros).
   (when (or *optim-O-macro?*
	     (and (number? *compiler-debug*) (>= *compiler-debug* 1)))
      (for-each (lambda (unit)
		   (if (procedure? (unit-sexp* unit))
		       ;; a freezed unit (such as the eval unit)
		       ;; cannot be macro expanser.
		       'nothing
		       (let loop ((src (unit-sexp* unit))
				  (res '()))
			  (if (null? src)
			      (unit-sexp*-set! unit
				 (append (get-O-macro-toplevel) (reverse! res)))
			      (let* ((obody (car src))
				     (nbody (bind-exit (skip)
					       (with-exception-handler
						  (lambda (e)
						     (handler e)
						     (if (isa? e &error)
							 (skip ''())))
						  (lambda ()
						     (compile-expand obody))))))
				 (loop (cdr src) (cons nbody res)))))))
	 units))
   ;; we are done
   (pass-postlude units check-to-be-macros))
      
;*---------------------------------------------------------------------*/
;*    comptime-expand ...                                              */
;*---------------------------------------------------------------------*/
(define (comptime-expand x)
   (initial-expander x initial-expander))

;*---------------------------------------------------------------------*/
;*    comptime-expand/error ...                                        */
;*---------------------------------------------------------------------*/
(define (comptime-expand/error x)
   (bind-exit (escape)
      (with-exception-handler
	 (lambda (e)
	    (when (isa? e &error)
	       (user-error-notify e 'expand))
	    (user-error "module" "Illegal module clause" x #f)
	    (exit 1))
	 (lambda ()
	    (comptime-expand x)))))

;*---------------------------------------------------------------------*/
;*    comptime-expand-cond-expand-only ...                             */
;*---------------------------------------------------------------------*/
(define (comptime-expand-cond-expand-only x)

   (define (cond-expand-only-expander x e)
      (match-case x
	 ((cond-expand . ?-)
	  (initial-expander x e))
	 ((begin . ?rest)
	  (set-cdr! x (map! (lambda (x) (e x e)) rest))
	  x)
	 (else
	  x)))
   
   (bind-exit (escape)
      (with-exception-handler
	 (lambda (e)
	    (when (isa? e &error)
	       (user-error-notify e 'expand))
	    (user-error "module" "Illegal module clause" x #f)
	    (exit 1))
	 (lambda ()
	    (initial-expander x cond-expand-only-expander)))))

;*---------------------------------------------------------------------*/
;*    compile-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (compile-expand x)
   (compile-expander x compile-expander '()))

;*---------------------------------------------------------------------*/
;*    initial-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (initial-expander x e::procedure)
   (trace expand "initial-expander: " x #\Newline)
   (let ((e1 (cond
                ((symbol? x)
                 identifier-expander)
                ((null? x)
                 (error #f "Illegal form" '()))
                ((not (pair? x))
                 (lambda (x e) x))
                ((symbol? (car x))
                 (let ((id (fast-id-of-id (car x) (find-location x))))
		    (cond
		       ((pair? (assq id (lexical-stack)))
			application-expander)
		       ((get-compiler-expander id)
			=>
			(lambda (x) x))
		       (else
			application-expander))))
                (else
                 application-expander))))
      (let ((new (e1 x e)))
         (if (and (pair? new) (not (epair? new)) (epair? x))
             (econs (car new) (cdr new) (cer x))
             new))))

;*---------------------------------------------------------------------*/
;*    compile-expander ...                                             */
;*    -------------------------------------------------------------    */
;*    This expander is used in a second time, after the first          */
;*    traditional macro-expansion has been applied.                    */
;*---------------------------------------------------------------------*/
(define (compile-expander x e::procedure s::pair-nil)
   (trace expand "compile-expander: " x #\Newline)
   
   (define (id-of-id id)
      (fast-id-of-id id #f))
   
   (define (proto->frame p)
      (map! id-of-id (args*->args-list p)))
   
   (define (expand* x s)
      (map! (lambda (x) (expand x s)) x))
   
   (define (expand-lambda x s)
      (let ((frame (proto->frame (cadr x))))
	 (set-cdr! (cdr x) (expand* (cddr x) (append frame s)))
	 x))
   
   (define (expand x s)
      (match-case x
	 ((or () (atom ?-) (@ . ?-) (quote . ?-))
	  x)
	 ((or (begin . ?-) (if . ?-) (set! . ?-))
	  (set-cdr! x (expand* (cdr x) s))
	  x)
	 (((or define define-inline define-generic define-method) ?p . ?body)
	  (let ((frame (proto->frame p)))
	     (set-cdr! (cdr x) (expand* body (append frame s)))
	     x))
	 ((let (and ?loop (? symbol?)) ?bindings . ?body)
	  (let* ((id (id-of-id loop))
		 (frame (cons id (map (lambda (b)
					 (cond
					    ((and (pair? b) (symbol? (car b)))
					     (id-of-id (car b)))
					    ((symbol? b)
					     (id-of-id b))
					    (else
					     (error "let" "Illegal form" x))))
				      bindings)))
		 (ns (append frame s)))
	     (for-each (lambda (b)
			  (when (pair? b)
			     (set-cdr! b (expand* (cdr b) ns))))
		       bindings)
	     (set-cdr! (cddr x) (expand* body ns))
	     x))
	 (((or let let* letrec letrec*) ?bindings . ?body)
	  (let* ((frame  (map (lambda (b)
				 (cond
				    ((and (pair? b) (symbol? (car b)))
				     (id-of-id (car b)))
				    ((symbol? b)
				     (id-of-id b))
				    (else
				     (error (car x) "Illegal form" x))))
			      bindings))
		 (ns (append frame s)))
	     (for-each (lambda (b)
			  (when (pair? b)
			     (set-cdr! b (expand* (cdr b) ns))))
		       bindings)
	     (set-cdr! (cdr x) (expand* body ns))
	     x))
	 ((labels ?bindings . ?body)
	  (let* ((frame (map (lambda (b)
				(if (not (pair? b))
				    (error "labels" "Illegal form" x)
				    (id-of-id (car b))))
			     bindings))
		 (ns (append frame s)))
	     (for-each (lambda (b)
			  (let ((f (proto->frame (cadr b))))
			     (set-cdr! (cdr b)
				       (expand* (cddr b) (append f ns)))))
		       bindings)
	     (set-cdr! (cdr x) (expand* body ns))
	     x))
	 ((lambda . ?-)
	  (expand-lambda x s))
	 (((or pragma pragma/effect free-pragma free-pragma/effect) ?- . ?body)
	  (set-cdr! (cdr x) (expand* body s))
	  x)
	 ((failure . ?-)
	  (set-cdr! x (expand* (cdr x) s))
	  x)
	 ((case ?var . ?clauses)
	  (set-car! (cdr x) (expand var s))
	  (for-each (lambda (clause)
		       (when (pair? clause)
			  (set-cdr! clause (expand* (cdr clause) s))))
		    clauses)
	  x)
	 ((or (set-exit . ?-) (jump-exit . ?-))
	  (set-cdr! x (expand* (cdr x) s))
	  x)
	 ((apply ?fun ?arg)
	  (set-car! (cdr x) (expand fun s))
	  (set-car! (cddr x) (expand arg s))
	  x)
	 ((#unspecified)
	  x)
	 ((? private-sexp?)
	  (expand* x s))
	 (else
	  (if (and (symbol? (car x)) (eq? (id-of-id (car x)) 'lambda))
	      (expand-lambda x s)
	      (let ((nx (expand* x s)))
		 (if (symbol? (car nx))
		     (let* ((id (id-of-id (car nx))))
			(if (pair? (memq id s))
			    nx
			    (let ((b (or (find-O-expander id)
					 (find-G-expander id))))
			       (if b
				   (let* ((e (expander-expander b))
					  (e2 (lambda (x e)
						 (let ((nx (comptime-expand x)))
						    (compile-expander nx e s)))))
				      (e x e2))
				   nx))))
		     nx))))))
   (expand x s))

;*---------------------------------------------------------------------*/
;*    identifier-expander ...                                          */
;*---------------------------------------------------------------------*/
(define (identifier-expander id e)
   (if (pair? (assq id (lexical-stack)))
       id
       (let ((a (getprop id 'macro-alias-key)))
	  (if a
	      (e a e)
	      id))))

;*---------------------------------------------------------------------*/
;*    application-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (application-expander x e)
   (let loop ((x* x))
      (cond
	 ((pair? x*)
	  (set-car! x* (e (car x*) e))
	  (set-cdr! x* (loop (cdr x*)))
	  x*)
	 ((null? x*)
	  '())
	 (else
	  (error #f "Illegal application form" x)))))

       
