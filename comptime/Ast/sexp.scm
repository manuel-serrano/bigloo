;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Ast/sexp.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:05:39 1996                          */
;*    Last change :  Thu Jun 11 07:49:36 2020 (serrano)                */
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
	    type_misc
	    type_cache
	    object_class
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
	    ast_object
	    ast_dump
	    ast_ident
	    effect_feffect)
   
   (export  (if-sym)
	    (top-level-sexp->node::node <sexp> ::obj)
	    (sexp->node::node ::obj <obj> ::obj ::symbol)
	    (sexp*->node::pair-nil ::pair-nil <obj> ::obj ::symbol)
	    (define-primop-ref->node::node ::global ::node)
	    (define-primop-ref/src->node::node ::global ::node ::obj)
	    (define-primop->node::node ::global)
	    (location->node::node ::global)
	    (error-sexp->node::node ::bstring ::obj ::obj)
	    (use-variable! ::variable ::obj ::symbol)
	    (make-anonymous-name::symbol loc . pref)
	    (find-local ::symbol ::obj)))

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
(define *sites* '(value apply app set! test))

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
;*    proc-or-lambda? ...                                              */
;*---------------------------------------------------------------------*/
(define (proc-or-lambda? e)
   (match-case e
      ((? symbol?) #t)
      ((lambda ?- . ?-) #t)
      (else #f)))

;*---------------------------------------------------------------------*/
;*    sexp->node ...                                                   */
;*    -------------------------------------------------------------    */
;*    `exp' is the expression to compile.                              */
;*    `stack' is the lexical environment                               */
;*    `loc' is the current file-position                               */
;*    `site' is a information on the place the sexp takes place        */
;*---------------------------------------------------------------------*/
(define (sexp->node exp stack loc site)
   (assert (site) (memq site *sites*))
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
	  ((or (local? atom) (global? atom))
	   (variable->node atom loc site))
	  ((or (struct? atom)
	       (vector? atom) (homogeneous-vector? atom)
	       (object? atom)
	       (procedure? atom))
	   (error-sexp->node "Illegal atom in s-expression" exp loc))
	  ((not (symbol? atom))
	   (instantiate::literal
	      (loc loc)
	      (type (get-type-atom atom))
	      (value atom)))
	  ((find-local atom stack)
	   =>
	   (lambda (i) (variable->node i loc site)))
	  (else
	   (let ((global (find-global atom))
		 (loc (find-location/loc atom loc)))
	      (cond
		 ((not (global? global))
		  (error-sexp->node "Unbound variable" exp loc))
		 ((eq? (global-import global) 'eval)
		  (sexp->node `(eval ',atom) stack loc site))
		 (else
		  (variable->node global loc site)))))))
;*--- application -----------------------------------------------------*/
      (((and (? symbol?)
	     (? (lambda (x)
		   (or (find-local x stack)
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
	      (let ((global (find-global/module name module))
		    (loc (find-location/loc name loc)))
		 (cond
		    ((not (global? global))
		     (error-sexp->node "Unbound variable" exp loc))
		    ((eq? (global-import global) 'eval)
		     (sexp->node `(eval ,atom) stack loc site))
		    (else
		     (variable->node global loc site)))))
	     (else
	      (error-sexp->node "Illegal `@' expression" exp loc)))))
;*--- -> --------------------------------------------------------------*/
      ((-> . ?l)
       (if (and (pair? l) (pair? (cdr l)) (every symbol? l))
	   (field-ref->node l exp stack loc site)
	   (error-sexp->node "Illegal ->" exp loc)))
;*--- quote -----------------------------------------------------------*/
      ((quote . ?-)
       (match-case exp
          ((?- ?value)
	   (let ((loc (find-location/loc exp loc)))
	      (cond
		 ((null? value)
		  (instantiate::literal
		     (loc loc)
		     (type (strict-node-type (get-type-kwote value) *bnil*))
		     (value '())))
		 ((or (pair? value)
		      (vector? value)
		      (homogeneous-vector? value)
		      (struct? value)
		      (symbol? value)
		      (keyword? value))
		  (instantiate::kwote
		     (loc loc)
		     (type (strict-node-type (get-type-kwote value) *_*))
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
      ((begin)
       (sexp->node #unspecified stack (find-location/loc exp loc) site))
      ((begin . ?body)
       (let* ((loc (find-location/loc exp loc))
	      (nodes (sexp*->node body stack loc site)))
	  (instantiate::sequence
	     (loc loc)
	     (type *_*)
	     (nodes nodes))))
;*--- if --------------------------------------------------------------*/
      ((and ((or if (? if-sym?)) ?test #t #f)
	    (? (lambda (x) (eq? site 'test))))
       (sexp->node test stack loc site))
      (((or if (? if-sym?)) . ?-)
       (match-case exp
          ((?- ?si ?alors ?sinon)
	   (let ((nt (not-test si)))
	      (if nt
		  (begin
		     (set-car! (cdr exp) nt)
		     (set-car! (cddr exp) sinon)
		     (set-car! (cdddr exp) alors)
		     (sexp->node exp stack loc site))
		  (let* ((loc (find-location/loc exp loc))
			 (cdloc (find-location/loc (cdr exp) loc))
			 (cddloc (find-location/loc (cddr exp) loc))
			 (cdddloc (find-location/loc (cdddr exp) loc))
			 (l-si (find-location/loc si cdloc))
			 (l-alors (find-location/loc alors cddloc))
			 (l-sinon (find-location/loc sinon cdddloc))
			 (test (sexp->node si stack loc 'test)))
		     (cond
			((atom? test)
			 (with-access::atom test (value)
			    (if (not value)
				(sexp->node sinon stack l-sinon 'value)
				(sexp->node alors stack l-alors 'value))))
			((kwote? test)
			 (sexp->node alors stack l-alors 'value))
			((var? test)
			 (let ((alors (sexp->node alors stack l-alors 'value))
			       (sinon (sexp->node sinon stack l-sinon 'value)))
			    (instantiate::conditional
			       (loc loc)
			       (type *_*)
			       (test test)
			       (true alors)
			       (false sinon))))
			(else
			 (let* ((v (mark-symbol-non-user! (gensym 'test)))
				(var (make-typed-ident v 'bool))
				(nexp (epairify-rec `(if ,v ,alors ,sinon) exp)))
			    (replace! exp `(,(let-sym) ((,var ,si)) ,nexp))
			    (sexp->node exp stack loc site))))))))
	  ((?- ?si ?alors)
           (set-cdr! (cddr exp) (list #unspecified))
           (sexp->node exp stack loc site))
          (else
	   (error-sexp->node "Illegal `if' form" exp loc))))
;*--- set! ------------------------------------------------------------*/
      ((set! (-> . ?l) ?val)
       (if (and (pair? l) (pair? (cdr l)) (every symbol? l))
	   (field-set->node l val exp stack loc site)
	   (error-sexp->node "Illegal ->" exp loc)))
      ((set! . ?-)
       (match-case exp
          ((?- ?var ?val)
	   (let* ((loc (find-location/loc exp loc))
		  (cdloc (find-location/loc (cdr exp) loc))
		  (cddloc (find-location/loc (cddr exp) loc))
		  (val-loc (find-location/loc val cddloc))
		  (val (match-case val
			  ((lambda . ?-)
			   (lambda->node val stack val-loc 'value
				(symbol->string var)))	    
			  (else
			   (sexp->node val stack val-loc 'value)))))
	      (let ((ast (sexp->node var stack cdloc 'set!)))
		 (if (var? ast)
		     (with-access::var ast (variable)
			(if (and (global? variable)
				 (global-read-only? variable))
			    (error-sexp->node
			       "Read-only variable" exp cdloc)
			    (instantiate::setq
			       (loc loc)
			       (type *unspec*)
			       (var ast)
			       (value val))))
		     (error-sexp->node
			"illegal `set!' expression" exp loc)))))
          (else
	   (error-sexp->node
	      "Illegal `set!' form" exp (find-location/loc exp loc)))))
;*--- define ----------------------------------------------------------*/
      ((define . ?-)
       ;; define is very close to `set!' except that is it not
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
			   (loc loc)
			   (type *unspec*)
			   (var ast)
			   (value val)))
		     (error-sexp->node
			"illegal `define' expression" exp loc)))))
          (else
	   (error-sexp->node
	      "Illegal `define' form" exp (find-location/loc exp loc)))))
;*--- a pattern to improve pattern-matching compilation ---------------*/
      ((((or let (? let-sym?) letrec labels (? labels-sym?)) ?- ?body) . ?args)
       (let* ((let-part (car exp))
	      (nexp `(,(car let-part) ,(cadr let-part) (,body ,@args))))
	  (sexp->node nexp stack loc site)))
;*--- let & letrec ----------------------------------------------------*/
      (((or let (? let-sym?) letrec) ?bindings . ?expr)
       (when (and (pair? bindings) (null? (cdr bindings))
		  (and (pair? expr) (null? (cdr expr))))
	  (match-case exp
	     ((?- ((?var ?expr)) (if (and (? symbol?) ?id) ?then ?otherwise))
	      ;; (let ((v1 (not e1))) (if v1 then else))
	      ;;   =>
	      ;; (let ((v1 e1)) (if v1 else then))
	      (let ((nt (not-test expr)))
		 (when (and nt
			    (not (used-in? id otherwise))
			    (not (used-in? id then)))
		    (let ((binding (car bindings))
			  (body (caddr exp)))
		       (set-car! (cdr binding) nt)
		       (set-car! (cddr body) otherwise)
		       (set-car! (cdddr body) then)))))))
       (let->node exp stack loc 'value))
      ((letrec* . ?-)
       (letrec*->node exp stack loc 'value))
;*--- labels ----------------------------------------------------------*/
      (((or labels (? labels-sym?)) . ?-)
       (labels->node exp stack loc 'value))
;*--- the direct lambda applications (see match-case ...) -------------*/
      (((lambda (?var) (and ?body (?- . ?-) (? cast-sexp?))) (atom ?arg))
       (instantiate::cast
	  (loc loc)
	  (type (use-type! (cast-sexp-type body) loc))
	  (arg (sexp->node arg stack loc site))))
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
	  (let->node nexp stack loc 'value)))
;*--- the direct if applications --------------------------------------*/
      (((if ?test
	    (and (? proc-or-lambda?) ?proc1)
	    (and (? proc-or-lambda?) ?proc2))
	. (and (? (lambda (l) (every (lambda (l) (not (pair? l))) l))) ?args))
       (let ((nexp `(if ,test (,proc1 ,@args) (,proc2 ,@args))))
	  (sexp->node nexp stack (find-location/loc exp loc) site)))
      (((if ?test
	    (and (? proc-or-lambda?) ?proc1)
	    (and (? proc-or-lambda?) ?proc2))
	. (and (? list?) ?args))
       (let* ((tmps (map (lambda (_) (gensym)) args))
	      (loc (find-location/loc exp loc))
	      (nexp `(,(let-sym) ,(map list tmps args)
				 (if ,test (,proc1 ,@tmps) (,proc2 ,@tmps)))))
	  (let->node nexp stack (find-location/loc exp loc) site)))
      
;*--- lambda ----------------------------------------------------------*/
      ((lambda . ?-)
       (lambda->node exp stack loc site "anonymous"))
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
;*--- static-pragma ---------------------------------------------------*/
      ((static-pragma . ?-)
       (if (not (and (null? stack) (eq? site 'value)))
	   (error-sexp->node "Illegal `static-pragma' expression" exp loc)
	   (begin
	      (add-static-pragma!
		 (pragma/type->node #t #f *unspec* exp stack loc site))
	      (sexp->node #unspecified stack loc site))))
;*--- pragma/effect ---------------------------------------------------*/
      ((free-pragma/effect ?effect . ?rest)
       (pragma/type->node #t
	  (parse-effect effect)
	  *unspec* `(pragma ,@rest) stack loc site))
;*--- failure ---------------------------------------------------------*/
      ((failure . ?-)
       (match-case exp
          ((?- ?proc ?msg ?obj)
	   (let* ((loc (find-location/loc exp loc))
		  (cdloc (find-location/loc (cdr exp) loc))
		  (cddloc (find-location/loc (cddr exp) loc))
		  (cdddloc (find-location/loc (cdddr exp) loc))
		  (loc-proc (find-location/loc proc cdloc))
		  (loc-msg (find-location/loc msg cddloc))
		  (loc-obj (find-location/loc obj cdddloc))
		  (proc (sexp->node proc stack loc-proc 'value))
		  (msg (sexp->node msg stack loc-msg 'value))
		  (obj (sexp->node obj stack loc-obj 'value)))
	      (instantiate::fail
		 (loc loc)
		 (type *magic*)
		 (proc proc)
		 (msg msg)
		 (obj obj))))
          (else
	   (error-sexp->node
	      "Illegal `failure' form" exp (find-location/loc exp loc)))))
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
		     (instantiate::switch
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
			    (error-sexp->node
			       "Illegal `case' form" exp (find-location/loc exp loc))
			    (loop (cdr cls)
			       (cons (epairify nclause clause)
				  nclauses))))))))
          (else
	   (error-sexp->node
	      "Illegal `case' form" exp (find-location/loc exp loc)))))
;*--- set-exit --------------------------------------------------------*/
      ((set-exit . ?-)
       (set-exit->node exp stack loc site))
;*--- jump-exit -------------------------------------------------------*/
      ((jump-exit . ?-)
       (jump-exit->node exp stack loc site))
;*--- apply -----------------------------------------------------------*/
      ((apply ?- ?-)
       (applycation->node exp stack loc site))
;*--- synchronize -----------------------------------------------------*/
      ((synchronize ?mutex :prelock ?prelock . ?body)
       (synchronize->node exp mutex prelock body stack (find-location/loc exp loc) site))
      ((synchronize ?mutex . ?body)
       (synchronize->node exp mutex ''() body stack (find-location/loc exp loc) site))
;*--- private ---------------------------------------------------------*/
      ((#unspecified)
       (error-sexp->node
	  "Illegal `application' form" exp (find-location/loc exp loc)))
      ((? private-sexp?)
       (private-node exp stack loc site))
;*--- app -------------------------------------------------------------*/
      (else
       ;; this expression can be a function call or a typed pragma
       ;; form. We first check to see if it is a pragma. If it is not
       ;; we compile a function call. This check is required by the
       ;; form (pragma::??? ...) (because we can't add a branch in the
       ;; match-case to check the node `pragma::???').
       (call->node exp stack loc site))))

;*---------------------------------------------------------------------*/
;*    lambda->node ...                                                 */
;*---------------------------------------------------------------------*/
(define (lambda->node exp stack loc site prefname)
   (match-case exp
      ((?- ?args . ?body) 
       (let ((loc (find-location/loc exp loc))
	     (fun (make-anonymous-name loc prefname)))
	  (sexp->node `(,(labels-sym) ((,fun ,args ,(normalize-progn body))) ,fun)
	     stack
	     loc
	     site)))
      (else
       (error-sexp->node "Illegal `lambda' form"
	  exp
	  (find-location/loc exp loc)))))

;*---------------------------------------------------------------------*/
;*    variable->node ...                                               */
;*---------------------------------------------------------------------*/
(define (variable->node v loc site)
   (use-variable! v loc site)
   (if (and (not (eq? site 'app)) (fun? (variable-value v)))
       (if (and (global? v)
		(cfun? (global-value v))
		(not (backend-foreign-closure (the-backend)))
		(not (eq? site 'apply)))
	   (error-sexp->node
	      (format "Backend (~a) does not support external first class function" (backend-name (the-backend))) exp loc)
	   (instantiate::closure
	      (loc loc)
	      (type (strict-node-type *procedure* (variable-type v)))
	      (variable v)))
       (instantiate::var
	  (loc loc)
	  (type (strict-node-type *_* (variable-type v)))
	  (variable v))))
   
;*---------------------------------------------------------------------*/
;*    call->node ...                                                   */
;*---------------------------------------------------------------------*/
(define (call->node exp stack loc site)
   (let ((caller (car exp))
	 (loc (find-location/loc exp loc)))
      (if (symbol? caller)
	  ;; it might be a typed special forms (such as pragma or lambda)
	  (let* ((pid (parse-id caller loc))
		 (id (car pid))
		 (type (cdr pid)))
	     (case id
		((pragma)
		 (pragma/type->node #f #f type exp stack loc site))
		((pragma/effect)
		 (if (not (pair? (cdr exp)))
		     (error-sexp->node
			"Illegal pragma/effect"
			exp (find-location/loc exp loc))
		     (pragma/type->node #f (parse-effect (cadr exp))
			type `((car exp) ,@(cddr exp))
			stack loc site)))
		((free-pragma)
		 (pragma/type->node #t #f type exp stack loc site))
		((free-pragma/effect)
		 (if (not (pair? (cdr exp)))
		     (error-sexp->node
			"Illegal free-pragma/effect"
			exp (find-location/loc exp loc))
		     (pragma/type->node #t (parse-effect (cadr exp))
			type `((car exp) ,@(cddr exp))
			stack loc site)))
		((lambda)
		 (match-case exp
		    ((?- ?args . ?body)
		     (let* ((loc (find-location/loc exp loc))
			    (fun (mark-symbol-non-user! (gensym 'lambda)))
			    (tfun (if (bigloo-type? type)
				      (make-typed-ident fun (type-id type))
				      fun)))
			(sexp->node
			   `(,(labels-sym) ((,tfun ,args ,(normalize-progn body)))
					   ,fun)
			   stack
			   loc
			   site)))
		    (else
		     (error-sexp->node "Illegal lambda" exp loc))))
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
;*    find-local ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-local name stack)
   (if (and (eq? name *cache-name*) (eq? stack *cache-stack*))
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
   (assert (site) (memq site *sites*))
   (when (eq? site 'set!)
      (variable-access-set! var 'write))
   (let ((val (variable-value var)))
      (when (and (eq? site 'set!) (fun? val))
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
	  (instantiate::app
	     (type *_*)
	     (fun fun)
	     (args (list (sexp->node `',(global-id global) '() #f 'value) ref)))
	  fun)))

;*---------------------------------------------------------------------*/
;*    define-primop-ref/src->node ...                                  */
;*---------------------------------------------------------------------*/
(define (define-primop-ref/src->node global ref src)
   (if (not (epair? src))
       (define-primop-ref->node global ref)
       (match-case (cer src)
	  ((at ?fname ?loc)
	   (let ((fun (sexp->node '(@ define-primop-ref/loc! __evenv) '() #f 'app)))
	      (if (var? fun)
		  (instantiate::app
		     (type *_*)
		     (fun fun)
		     (args (list
			      (sexp->node `',(global-id global) '() #f 'value)
			      ref
			      (sexp->node fname '() #f 'value)
			      (sexp->node loc '() #f 'value))))
		  fun)))
	  (else
	   (define-primop-ref->node global ref)))))

;*---------------------------------------------------------------------*/
;*    define-primop->node ...                                          */
;*---------------------------------------------------------------------*/
(define (define-primop->node global)
   (use-variable! global #f 'value)
   (let ((fun (sexp->node '(@ define-primop! __evenv) '() #f 'app)))
      (if (var? fun)
	  (instantiate::app
	     (type *_*)
	     (fun fun)
	     (args (list (sexp->node `',(global-id global) '() #f 'value)
			 (sexp->node global '() #f 'value))))
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
	     (instantiate::app
		(type (strict-node-type *_* *obj*))
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
		(base (symbol->string (gensym (string-append "<@" pref ":")))))
	     (cond
		((string-contains pref "<@")
		 (symbol-append (string->symbol pref) (gensym 'anonymous)))
		((or (and (>=fx *bdb-debug* 1)
			  (memq 'bdb (backend-debug-support (the-backend))))
		     (>=fx *compiler-debug* 1))
		 (let ((file (if (substring=? file "./" 2)
				 (substring file 2 (string-length file))
				 file)))
		    (string->symbol
		     (string-append base ":"
				    (string-replace file #\. #\,)
				    ":" (number->string line)
				    "@>"))))
		(else
		 (string->symbol (string-replace (string-append base ">") #\. #\,)))))
	  (symbol-append (gensym (string-append "<@" pref "-")) '@>))))
	  
;*---------------------------------------------------------------------*/
;*    synchronize->node ...                                            */
;*---------------------------------------------------------------------*/
(define (synchronize->node exp mutex prelock body stack loc site)
   (if (symbol? mutex)
       (instantiate::sync
	  (loc loc)
	  (type *_*)
	  (mutex (sexp->node mutex stack loc 'value))
	  (prelock (sexp->node prelock stack loc 'value))
	  (body (sexp->node (normalize-progn body) stack loc site)))
       (let* ((v (mark-symbol-non-user! (gensym 'mutex)))
	      (var (make-typed-ident v 'mutex))
	      (nexp (epairify-rec `(synchronize ,v ,@body) exp)))
	  (replace! exp `(,(let-sym) ((,var ,mutex)) ,nexp))
	  (sexp->node exp stack loc site))))
	     
;*---------------------------------------------------------------------*/
;*    not-test ...                                                     */
;*    -------------------------------------------------------------    */
;*    When the test is a negation, returns a new inverted tests.       */
;*    Returns #f otherwise.                                            */
;*---------------------------------------------------------------------*/
(define (not-test test)   
   (match-case test
      ((if ?ntest #f #t)
       ntest)
      (((kwote not) ?ntest)
       ntest)
      ((let ?bindings ?expr)
       (let ((nt (not-test expr)))
	  (when nt
	     (epairify-rec `(let ,bindings ,nt) test))))
      (else
       #f)))
   
;*---------------------------------------------------------------------*/
;*    used-in? ...                                                     */
;*    -------------------------------------------------------------    */
;*    A rough approximation of "does the variable ID used in EXPR?".   */
;*---------------------------------------------------------------------*/
(define (used-in? id expr)
   (cond
      ((eq? id expr) #t)
      ((not (pair? expr)) #f)
      ((eq? (car expr) 'quote) #f)
      ((not (list? expr)) #f)
      ((find (lambda (e) (used-in? id e)) expr) #t)
      (else #f)))
