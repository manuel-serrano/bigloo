;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/sexp.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:05:39 1996                          */
;*    Last change :  Sun Dec 12 14:52:01 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We build an `ast node' from a `sexp'                             */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_sexp

   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    tools_progn
	    tools_location
	    tools_misc
	    tools_dsssl
	    type_type
	    type_env
	    type_cache
	    type_typeof
	    engine_param
	    backend_backend
	    ast_ident
	    ast_env
	    ast_var
	    ast_node
	    ast_build
	    ast_pragma
	    ast_labels
	    ast_let
	    ast_exit
	    ast_app
	    ast_apply
	    ast_private
	    effect_feffect)
   
   (export  (if-sym)
	    (top-level-sexp->node::node <sexp> ::obj)
	    (sexp->node::node ::obj <obj> ::obj ::symbol)
	    (sexp*->node::pair-nil ::pair-nil <obj> ::obj ::symbol)
	    (define-primop-ref->node::node ::global ::node)
	    (define-primop->node::node ::global)
	    (location->node::node ::global)
	    (error-sexp->node::node ::bstring ::obj ::obj)
	    (use-variable! ::variable ::obj ::symbol)
	    (make-anonymous-name::symbol loc . pref)))

;*---------------------------------------------------------------------*/
;*    top-level-sexp->node ...                                         */
;*---------------------------------------------------------------------*/
(define (top-level-sexp->node exp loc)
   (bind-exit (skip)
      (with-exception-handler
	 (lambda (e)
	    (exception-notify e)
	    (skip #unspecified))
	 (lambda ()
	    (sexp->node exp '() loc 'value)))))

;*---------------------------------------------------------------------*/
;*    *sites* ...                                                      */
;*---------------------------------------------------------------------*/
(define *sites* '(value apply app set!))

;*---------------------------------------------------------------------*/
;*    *if* ...                                                         */
;*---------------------------------------------------------------------*/
(define *if* (gensym 'if))

;*---------------------------------------------------------------------*/
;*    if-sym ...                                                       */
;*---------------------------------------------------------------------*/
(define (if-sym)
   *if*)

;*---------------------------------------------------------------------*/
;*    if-sym? ...                                                      */
;*---------------------------------------------------------------------*/
(define (if-sym? sym)
   (eq? sym *if*))

;*---------------------------------------------------------------------*/
;*    sexp->node ...                                                   */
;*    -------------------------------------------------------------    */
;*    `exp' is the expression to compile.                              */
;*    `stack' is the lexical environment                               */
;*    `loc' is the current file-position                               */
;*    `site' is a information on the place the sexp takes place        */
;*---------------------------------------------------------------------*/
(define (sexp->node exp stack loc site)
   [assert (site) (memq site *sites*)]
   (trace (ast 2) "sexp->node(" loc "): " (shape exp))
   (trace (ast 3) " site: " site)
   (trace (ast 2) #\Newline)
   (match-case exp
;*--- () --------------------------------------------------------------*/
      (()
       (error-sexp->node "Illegal `()' expression" exp loc))
;*--- node ------------------------------------------------------------*/
      ((? node?)
       (when (extern? exp)
	  (with-access::extern exp (expr* loc)
	     (map! (lambda (e) (sexp->node e stack loc 'value)) expr*)))
       (when (app? exp)
	  (with-access::app exp (args loc)
	     (map! (lambda (e) (sexp->node e stack loc 'value)) args)))
       exp)
;*--- atom ------------------------------------------------------------*/
      ((atom ?atom)
       (cond
	  ((not (symbol? atom))
	   (cond
	      ((or (local? atom) (global? atom))
	       (use-variable! atom loc site)
	       (if (and (not (eq? site 'app)) (fun? (variable-value atom)))
		   (instantiate::closure
		      (loc loc)
		      (type (variable-type atom))
		      (variable atom))
		   (instantiate::var
		      (loc loc)
		      (type (variable-type atom))
		      (variable atom))))
	      ((or (struct? atom)
		   (vector? atom)
		   (object? atom)
		   (procedure? atom))
	       (error-sexp->node "Illegal atom in s-expression" exp loc))  
	      (else
 	       (instantiate::atom
		  (loc loc)
		  (type (get-type-atom atom))
		  (value atom)))))
	  ((lookup atom stack)
	   (let* ((local (lookup atom stack))
		  (var   (if (and (not (eq? site 'app))
				  (fun? (local-value local)))
			     (instantiate::closure
				(loc loc)
				(type (local-type local))
				(variable local))
			     (instantiate::var
				(loc loc)
				(type (local-type local))
				(variable local)))))
	      (use-variable! local loc site)
	      var))
	  (else
	   (let ((global (find-global atom)))
	      (cond
		 ((not (global? global))
		  (error-sexp->node "Unbound variable" exp loc))
		 ((eq? (global-import global) 'eval)
		  (sexp->node `(eval ',atom) stack loc site))
		 (else
		  (use-variable! global loc site)
		  (if (and (not (eq? site 'app)) (fun? (global-value global)))
		      (if (and (cfun? (global-value global))
			       (not (backend-foreign-closure (the-backend)))
			       (not (eq? site 'apply)))
			  (error-sexp->node
			   (format "This backend (~a) does not support external first class value functions" (backend-name (the-backend)))
			   exp loc)
			  (instantiate::closure
			     (loc loc)
			     (type (global-type global))
			     (variable global)))
		      (instantiate::var 
			 (loc loc)
			 (type (global-type global))
			 (variable global)))))))))
;*--- application -----------------------------------------------------*/
      (((and (? symbol?)
	     (? (lambda (x)
		   (or (lookup x stack)
		       (let ((g (find-global x)))
			  (when g
			     (not (eq? (global-module g)
				       '__r4_control_features_6_9))))))))
	. ?rest)
       ;; apply is a special case. unless overriden it must be handled
       ;; as a special form by the compiler.
       (call->node exp stack loc site))
;*--- qualified global variable ---------------------------------------*/
      ((@ . ?-)
       (let ((loc (find-location/loc exp loc)))
	  (match-case exp
	     ((@ (and (? symbol?) ?name) (and (? symbol?) ?module))
	      (let ((global (find-global/module name module)))
		 (cond
		    ((not (global? global))
		     (error-sexp->node "Unbound variable" exp loc))
		    ((eq? (global-import global) 'eval)
		     (sexp->node `(eval ,atom) stack loc site))
		    (else
		     (use-variable! global loc site)
		     (if (and (not (eq? site 'app))
			      (fun? (global-value global)))
			 (instantiate::closure (loc loc)
					       (type (global-type global))
					       (variable global))
			 (instantiate::var (loc loc)
					   (type (global-type global))
					   (variable global)))))))
	     (else
	      (error-sexp->node "Illegal `@' expression" exp loc)))))
;*--- quote -----------------------------------------------------------*/
      ((quote . ?-)
       (match-case exp
          ((?- ?value)
	   (let ((loc (find-location/loc exp loc)))
	      (cond
		 ((null? value)
		  (instantiate::atom (loc loc)
				     (type *bnil*)
				     (value '())))
		 ((or (pair? value)
		      (vector? value)
		      (homogeneous-vector? value)
		      (struct? value)
		      (symbol? value)
		      (keyword? value))
		  (instantiate::kwote (loc loc)
				      (type *_*)
				      (value value)))
		 ((or (number? value)
		      (string? value)
		      (cnst? value)
		      ;; I won't put in the compiler that characters
		      ;; and boolean are constant so I explicitize
		      ;; these tests.
		      (char? value)
		      (boolean? value))
		  (sexp->node value stack loc site))
		 (else
		  (error-sexp->node "Illegal `quote' expression" exp loc)))))
          (else
	   (error-sexp->node "Illegal `quote' expression" exp loc))))
;*--- begin -----------------------------------------------------------*/
      ((begin . ?body)
       (let ((body (if (null? body)
		       (list #unspecified)
		       body)))
	  (let* ((loc    (find-location/loc exp loc))
		 (nodes  (sexp*->node body stack loc site)))
	     (instantiate::sequence (loc loc)
				    (type *_*)
				    (nodes nodes)))))
;*--- if --------------------------------------------------------------*/
      (((or if (? if-sym?)) . ?-)
       (match-case exp
          ((?- ?si ?alors ?sinon)
           (match-case si
              ((or (if ?si #f #t) ((kwote not) ?si))
	       (set-car! (cdr exp) si)
	       (set-car! (cddr exp) sinon)
	       (set-car! (cdddr exp) alors)
	       (sexp->node exp stack loc site))
              (else
	       (if (or (symbol? si) (atom? si))
		   (let* ((oloc    (find-location/loc exp loc))
			  (cdloc   (find-location/loc (cdr exp) loc))
			  (cddloc  (find-location/loc (cddr exp) loc))
			  (cdddloc (find-location/loc (cdddr exp) loc))
			  (l-si    (find-location/loc si cdloc))
			  (l-alors (find-location/loc alors cddloc))
			  (l-sinon (find-location/loc sinon cdddloc))
			  (si      (sexp->node si stack l-si 'value))
			  (alors   (sexp->node alors stack l-alors 'value))
			  (sinon   (sexp->node sinon stack l-sinon 'value))
			  (loc     (let ((loc (find-location/loc
					       exp
					       (node-loc si))))
				      (if (location? loc)
					  loc
					  oloc))))
		      (instantiate::conditional (loc loc)
						(type *_*)
						(test si)
						(true alors)
						(false sinon)))
		   (let ((test (mark-symbol-non-user! (gensym 'test))))
		      (replace! exp
				`(,(let-sym) ((,(symbol-append test '::bool) ,si))
				    ,(epairify-rec `(if ,test ,alors ,sinon)
						   exp)))
		      (sexp->node exp stack loc site))))))
          ((?- ?si ?alors)
           (set-cdr! (cddr exp) (list #unspecified))
           (sexp->node exp stack loc site))
          (else
	   (error-sexp->node "Illegal `if' form" exp loc))))
;*--- set! ------------------------------------------------------------*/
      ((set! . ?-)
       (match-case exp
          ((?- ?var ?val)
	   (let* ((loc (find-location/loc exp loc))
		  (cdloc (find-location/loc (cdr exp) loc))
		  (cddloc (find-location/loc (cddr exp) loc))
		  (val-loc (find-location/loc val cddloc))
		  (val (sexp->node val stack val-loc 'value)))
	      (let ((ast (sexp->node var stack cdloc 'set!)))
		 (if (var? ast)
		     (with-access::var ast (variable)
			(if (and (global? variable)
				 (global-read-only? variable))
			    (error-sexp->node "Read-only variable"
					      exp
					      cdloc)
			    (instantiate::setq
			       (loc   loc)
			       (type  *unspec*)
			       (var   ast)
			       (value val))))
		     (error-sexp->node "illegal `set!' expression"
				       exp
				       loc)))))
          (else
	   (error-sexp->node "Illegal `set!' form"
			     exp
			     (find-location/loc exp loc)))))
;*--- define ----------------------------------------------------------*/
      ((define . ?-)
       ;; define is very close to `set!' excepted that is it not
       ;; considered as a mutation of the defined variable.
       (match-case exp
          ((?- ?var ?val)
	   (let* ((loc (find-location/loc exp loc))
		  (cdloc (find-location/loc (cdr exp) loc))
		  (cddloc (find-location/loc (cddr exp) loc))
		  (val-loc (find-location/loc val cddloc))
		  (val (sexp->node val stack val-loc 'value)))
	      (let ((ast (sexp->node var stack cdloc 'value)))
		 (if (and (var? ast) (global? (var-variable ast)))
		     (begin
			(global-src-set! (var-variable ast) val)
			(instantiate::setq
			   (loc   loc)
			   (type  *unspec*)
			   (var   ast)
			   (value val)))
		     (error-sexp->node "illegal `define' expression"
				       exp
				       loc)))))
          (else
	   (error-sexp->node "Illegal `define' form"
			     exp
			     (find-location/loc exp loc)))))
;*--- a pattern to improve pattern-matching compilation ---------------*/
      ((((or let (? let-sym?) letrec labels (? labels-sym?)) ?- ?body) . ?args)
       (let* ((let-part (car exp))
	      (nexp `(,(car let-part) ,(cadr let-part) (,body ,@args))))
	  (sexp->node nexp stack loc site)))
;*--- let & letrec ----------------------------------------------------*/
      (((or let (? let-sym?) letrec letrec*) . ?-)
       (let->node exp stack loc 'value))
;*--- labels ----------------------------------------------------------*/
      (((or labels (? labels-sym?)) . ?-)
       (labels->node exp stack loc 'value))
;*--- the direct lambda applications (see match-case ...) -------------*/
      (((lambda ?vars . ?body) . ?args)
       (let ((loc (find-location/loc exp loc))
	     (nexp `(,(let-sym) ,(let loop ((vars vars)
					    (args args))
				    (cond
				       ((null? vars)
					(if (null? args)
					    '()
					    (user-error/location
					     loc
					     (shape (current-function))
					     "wrong number of argument"
					     exp)))
				       ((not (pair? vars))
					(list
					 (list
					  vars
					  (let liip ((args args))
					     (if (null? args)
						 ''()
						 `(cons ,(car args)
							,(liip (cdr args))))))))
				       ((dsssl-named-constant? (car vars))
					(let ((arg (dsssl-find-first-formal
						    (cdr vars))))
					   (if arg
					       (loop arg args)
					       (loop '() args))))
				       ((not (symbol? (car vars)))
					(user-error/location
					 loc
					 (shape (current-function))
					 "Illegal formal argument"
					 exp))
				       ((null? args)
					(user-error/location
					 loc
					 (shape (current-function))
					 "wrong number of argument"
					 exp))
				       (else
					(cons (list (car vars) (car args))
					      (loop (cdr vars) (cdr args))))))
				,(make-dsssl-function-prelude
				  (shape (current-function))
				  vars
				  (normalize-progn body)
				  (lambda (obj proc msg)
				     (user-error/location loc obj proc msg))))))
	  (let->node nexp stack loc site)))
;*--- lambda ----------------------------------------------------------*/
      ((lambda . ?-)
       (match-case exp
          ((?- ?args . ?body) 
           (let ((loc  (find-location/loc exp loc))
		 (fun  (make-anonymous-name loc)))
              (sexp->node `(,(labels-sym) ((,fun ,args ,(normalize-progn body))) ,fun)
			  stack
			  loc
			  site)))
          (else
	   (error-sexp->node "Illegal `lambda' form"
			     exp
			     (find-location/loc exp loc)))))
;*--- pragma ----------------------------------------------------------*/
      ((pragma . ?-)
       (pragma/type->node #f #f *unspec* exp stack loc site))
;*--- pragma/effect ---------------------------------------------------*/
      ((pragma/effect ?effect . ?rest)
       (pragma/type->node #f
			  (parse-effect effect)
			  *unspec* `(pragma ,@rest) stack loc site))
;*--- free-pragma -----------------------------------------------------*/
      ((free-pragma . ?-)
       (pragma/type->node #t #f *unspec* exp stack loc site))
;*--- pragma/effect ---------------------------------------------------*/
      ((free-pragma/effect ?effect . ?rest)
       (pragma/type->node #t
			  (parse-effect effect)
			  *unspec* `(pragma ,@rest) stack loc site))
;*--- failure ---------------------------------------------------------*/
      ((failure . ?-)
       (match-case exp
          ((?- ?proc ?msg ?obj)
	   (let* ((loc      (find-location/loc exp loc))
		  (cdloc    (find-location/loc (cdr exp) loc))
		  (cddloc   (find-location/loc (cddr exp) loc))
		  (cdddloc  (find-location/loc (cdddr exp) loc))
		  (loc-proc (find-location/loc proc cdloc))
		  (loc-msg  (find-location/loc msg cddloc))
		  (loc-obj  (find-location/loc obj cdddloc))
		  (proc     (sexp->node proc stack loc-proc 'value))
		  (msg      (sexp->node msg stack loc-msg 'value))
		  (obj      (sexp->node obj stack loc-obj 'value)))
	      (instantiate::fail (loc loc)
				 (type *magic*)
				 (proc proc)
				 (msg msg)
				 (obj obj))))
          (else
	   (error-sexp->node "Illegal `failure' form"
			     exp
			     (find-location/loc exp loc)))))
;*--- case ------------------------------------------------------------*/
      ((case . ?-)
       ;; former versions of the compiler used to make side effect
       ;; one the list that hold the clauses. This is a bad idea.
       ;; Because of macro expansion it could be that the very same
       ;; case form is seen twice by the compiler. If we compile case
       ;; by the means of side effects, the second time the compiler
       ;; will see the case form it will think of it as incorrect.
       (match-case exp
          ((?- ?test . ?clauses)
           (let* ((loc  (find-location/loc exp loc))
		  (cdloc (find-location/loc (cdr exp) loc))
		  (cddloc (find-location/loc (cdr exp) loc))
                  (test (sexp->node test
				    stack
				    (find-location/loc test cdloc)
				    'value)))
	      (let loop ((cls clauses)
			 (nclauses '()))
		 (if (null? cls)
		     (instantiate::select
			(loc loc)
			(type *_*)
			(test test)
			(item-type (get-type-atom (car (car (car clauses)))))
			(clauses (reverse! nclauses)))
		     (let* ((clause (car cls))
			    (body   (sexp->node (normalize-progn (cdr clause))
						stack
						(find-location/loc clause cddloc)
						'value))
			    (nclause (cons (car clause) body)))
			;; we check that it is not an illegal `else' clause
			(if (and (eq? (car clause) 'else)
				 (not (null? (cdr cls))))
			    (error-sexp->node "Illegal `case' form"
					      exp
					      (find-location/loc exp loc))
			    (loop (cdr cls)
				  (cons (epairify nclause clause)
					nclauses))))))))
          (else
	   (error-sexp->node "Illegal `case' form"
			     exp
			     (find-location/loc exp loc)))))
;*--- set-exit --------------------------------------------------------*/
      ((set-exit . ?-)
       (set-exit->node exp stack loc site))
;*--- jump-exit -------------------------------------------------------*/
      ((jump-exit . ?-)
       (jump-exit->node exp stack loc site))
;*--- apply -----------------------------------------------------------*/
      ((apply ?- ?-)
       (applycation->node exp stack loc site))
;*--- private ---------------------------------------------------------*/
      ((#unspecified)
       (error-sexp->node "Illegal `application' form"
			 exp
			 (find-location/loc exp loc)))
      ((? private-sexp?)
       (private->node exp stack loc site))
;*--- app -------------------------------------------------------------*/
      (else
       ;; this expression can be a function call or a typed pragma
       ;; form. We first check to see if it is a pragma. If it is not
       ;; we compile a function call. This check is required by the
       ;; form (pragma::??? ...) (because we can't add a branch in the
       ;; match-case to check the node `pragma::???').
       (call->node exp stack loc site))))

;*---------------------------------------------------------------------*/
;*    call->node ...                                                   */
;*---------------------------------------------------------------------*/
(define (call->node exp stack loc site)
   (let ((caller (car exp))
	 (loc    (find-location/loc exp loc)))
      (if (symbol? caller)
	  ;; it might be a typed special forms (such as pragma or lambda)
	  (let* ((pid  (parse-id caller loc))
		 (id   (car pid))
		 (type (cdr pid)))
	     (case id
		((pragma)
		 (pragma/type->node #f #f type exp stack loc site))
		((pragma/effect)
		 (if (not (pair? (cdr exp)))
		     (error-sexp->node "Illegal `pragma/effect' form"
				       exp
				       (find-location/loc exp loc))
		     (pragma/type->node #f (parse-effect (cadr exp))
					type `((car exp) ,@(cddr exp))
					stack loc site)))
		((free-pragma)
		 (pragma/type->node #t #f type exp stack loc site))
		((free-pragma/effect)
		 (if (not (pair? (cdr exp)))
		     (error-sexp->node "Illegal `free-pragma/effect' form"
				       exp
				       (find-location/loc exp loc))
		     (pragma/type->node #t (parse-effect (cadr exp))
					type `((car exp) ,@(cddr exp))
					stack loc site)))
		((lambda)
		 (match-case exp
		    ((?- ?args . ?body)
		     (let* ((loc  (find-location/loc exp loc))
			    (fun  (mark-symbol-non-user! (gensym 'lambda)))
			    (tfun (make-typed-ident fun (type-id type))))
			(sexp->node
			 `(,(labels-sym) ((,tfun ,args ,(normalize-progn body)))
			     ,fun)
			 stack
			 loc
			 site)))
		    (else
		     (error-sexp->node "Illegal `lambda' form" exp loc))))
		(else
		 (application->node exp stack loc site))))
	  (application->node exp stack loc site))))
 
;*---------------------------------------------------------------------*/
;*    sexp*->node ...                                                  */
;*---------------------------------------------------------------------*/
(define (sexp*->node::pair-nil exp*::pair-nil stack loc site)
   (let loop ((exps exp*)
	      (res  '())
	      (loc  loc))
      (cond
	 ((null? exps)
	  (error-sexp->node "Illegal empty sequence" exps loc))
	 ((null? (cdr exps))
	  (let ((loc (find-location/loc (car exps)
					(find-location/loc exps loc)))
		(nsite (if (eq? site 'app) 'value site)))
	     (reverse! (cons (sexp->node (car exps) stack loc nsite) res))))
	 (else
	  (let ((loc (find-location/loc (car exps)
					(find-location/loc exps loc))))
	     (loop (cdr exps)
		   (cons (sexp->node (car exps) stack loc 'value) res)
		   loc))))))

;*---------------------------------------------------------------------*/
;*    A 1-line cache lookup machinery                                  */
;*---------------------------------------------------------------------*/
(define *cache-name*  #f)
(define *cache-stack* #f)
(define *cache-res*   #f)

;*---------------------------------------------------------------------*/
;*    lookup ...                                                       */
;*---------------------------------------------------------------------*/
(define (lookup name stack)
   (if (and (eq? name *cache-name*)
	    (eq? stack *cache-stack*))
       *cache-res*
       (begin
	  (set! *cache-name* name)
	  (set! *cache-stack* stack)
	  (let loop ((stack stack))
	     (cond
		((null? stack)
		 (set! *cache-res* #f)
		 #f)
		((eq? (local-id (car stack)) name)
		 (set! *cache-res* (car stack))
		 (car stack))
		(else
		 (loop (cdr stack))))))))

;*---------------------------------------------------------------------*/
;*    use-variable! ...                                                */
;*---------------------------------------------------------------------*/
(define (use-variable! var::variable loc::obj site)
   [assert (site) (memq site *sites*)]
   (if (eq? site 'set!)
       (variable-access-set! var 'write))
   (let ((val (variable-value var)))
      (if (and (eq? site 'set!) (fun? val))
	  (error-sexp->node "Illegal mutation" (shape var) loc))))

;*---------------------------------------------------------------------*/
;*    error-sexp->node ...                                             */
;*---------------------------------------------------------------------*/
(define (error-sexp->node msg exp loc)
   (user-error/location loc
			(shape (current-function))
			msg
			exp
			(sexp->node ''() '() loc 'value)))

;*---------------------------------------------------------------------*/
;*    define-primop-ref->node ...                                      */
;*---------------------------------------------------------------------*/
(define (define-primop-ref->node global ref)
   (let ((fun (sexp->node '(@ define-primop-ref! __evenv) '() #f 'app)))
      (if (var? fun)
	  (instantiate::app (type *_*)
			    (fun fun)
			    (args (list (sexp->node `',(global-id global)
						    '()
						    #f
						    'value)
					ref)))
	  fun)))

;*---------------------------------------------------------------------*/
;*    define-primop->node ...                                          */
;*---------------------------------------------------------------------*/
(define (define-primop->node global)
   (use-variable! global #f 'value)
   (let ((fun (sexp->node '(@ define-primop! __evenv) '() #f 'app)))
      (if (var? fun)
	  (instantiate::app (type *_*)
			    (fun fun)
			    (args (list (sexp->node `',(global-id global)
						    '()
						    #f
						    'value)
					(sexp->node global
						    '()
						    #f
						    'value))))
	  fun)))
   
;*---------------------------------------------------------------------*/
;*    location->node ...                                               */
;*---------------------------------------------------------------------*/
(define (location->node global)
   (use-variable! global #f 'value)
   (let ((fun (sexp->node '(@ __evmeaning_address foreign) '() #f 'app)))
      (if (var? fun)
	  (let ((expr (if (backend-pragma-support (the-backend))
			  `(pragma::obj "($1)" ,global)
			  global)))
	     (instantiate::app (type *obj*)
			       (fun fun)
			       (args (list (sexp->node expr '() #f 'value)))))
	  fun)))
   
;*---------------------------------------------------------------------*/
;*    make-anonymous-name ...                                          */
;*---------------------------------------------------------------------*/
(define (make-anonymous-name::symbol loc . pref)
   (let ((pref (if (and (pair? pref) (string? (car pref)))
		   (car pref)
		   "anonymous")))
      (if (location? loc)
	  (let ((file (location-full-fname loc))
		(line (location-lnum loc))
		(base (symbol->string (gensym (string-append "<" pref ":")))))
	     (cond
		((or (and (>=fx *bdb-debug* 1)
			  (memq 'bdb (backend-debug-support (the-backend))))
		     (>=fx *compiler-debug* 1))
		 (let ((file (if (substring=? file "./" 2)
				 (substring file 2 (string-length file))
				 file)))
		    (string->symbol
		     (string-append base ":"
				    file ":" (number->string line)
				    ">"))))
		(else
		 (string->symbol (string-append base ">")))))
	  (symbol-append (gensym (string-append "<" pref "-")) '>))))
	  
