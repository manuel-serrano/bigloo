;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Ast/sexp.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 15:05:39 1996                          */
;*    Last change :  Mon Oct 20 17:14:05 2025 (serrano)                */
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
	    (top-level-sexp->node::node <sexp> ::obj ::obj)
	    (sexp->node::node ::obj <obj> ::obj ::symbol ::obj)
	    (sexp*->node::pair-nil ::pair-nil <obj> ::obj ::symbol ::obj)
	    (define-primop-ref->node::node ::global ::node ::obj)
	    (define-primop-ref/src->node::node ::global ::node ::obj ::obj)
	    (define-primop->node::node ::global ::obj)
	    (location->node::node ::global ::obj)
	    (error-sexp->node::node ::bstring ::obj ::obj ::obj)
	    (use-variable! ::variable ::obj ::symbol)
	    (make-anonymous-name::symbol loc . pref)
	    (find-local ::symbol ::obj)))

;*---------------------------------------------------------------------*/
;*    top-level-sexp->node ...                                         */
;*---------------------------------------------------------------------*/
(define (top-level-sexp->node exp loc genv)
   (bind-exit (skip)
      (with-exception-handler
	 (lambda (e)
	    (exception-notify e)
	    (skip #unspecified))
	 (lambda ()
	    (sexp->node exp '() loc 'value genv)))))

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
(define (sexp->node exp stack loc site genv)
   (assert (site) (memq site *sites*))
   (trace (ast 2) "sexp->node(" loc "): " (shape exp))
   (trace (ast 3) " site: " site)
   (trace (ast 2) #\Newline)
   (match-case exp
;*--- () --------------------------------------------------------------*/
      (()
       (error-sexp->node "Illegal `()' expression" exp loc genv))
;*--- node ------------------------------------------------------------*/
      ((? node?)
       (when (extern? exp)
	  (with-access::extern exp (expr* loc)
	     (map! (lambda (e) (sexp->node e stack loc 'value genv)) expr*)))
       (when (app? exp)
	  (with-access::app exp (args loc)
	     (map! (lambda (e) (sexp->node e stack loc 'value genv)) args)))
       exp)
;*--- atom ------------------------------------------------------------*/
      ((atom ?atom)
       (cond
	  ((or (local? atom) (global? atom))
	   (variable->node atom loc site genv))
	  ((or (struct? atom)
	       (vector? atom) (homogeneous-vector? atom)
	       (object? atom)
	       (procedure? atom))
	   (error-sexp->node "Illegal atom in s-expression" exp loc genv))
	  ((not (symbol? atom))
	   (instantiate::literal
	      (loc loc)
	      (type (get-type-atom atom))
	      (value atom)))
	  ((find-local atom stack)
	   =>
	   (lambda (i) (variable->node i loc site genv)))
	  (else
	   (let ((global (find-global (get-genv) atom))
		 (loc (find-location/loc atom loc)))
	      (cond
		 ((not (global? global))
		  (trace (ast 2) "*** UNBOUND VARIALBLE " exp " " loc)
		  (error-sexp->node "Unbound variable" exp loc genv))
		 ((eq? (global-import global) 'eval)
		  (sexp->node `(eval ',atom) stack loc site genv))
		 (else
		  (variable->node global loc site genv)))))))
;*--- application -----------------------------------------------------*/
      (((and (? symbol?)
	     (? (lambda (x)
		   (or (find-local x stack)
		       (let ((g (find-global (get-genv) x)))
			  (when g
			     (not (eq? (global-module g)
				     '__r4_control_features_6_9))))))))
	. ?rest)
       ;; apply is a special case. unless overriden it must be handled
       ;; as a special form by the compiler.
       (call->node exp stack loc site genv))
;*--- qualified global variable ---------------------------------------*/
      ((@ . ?-)
       (let ((loc (find-location/loc exp loc)))
	  (match-case exp
	     ((@ (and (? symbol?) ?name) (and (? symbol?) ?module))
	      (let ((global (find-global/module (get-genv) name module))
		    (loc (find-location/loc name loc)))
		 (cond
		    ((not (global? global))
		     (error-sexp->node "Unbound global variable" exp loc genv))
		    ((eq? (global-import global) 'eval)
		     (sexp->node `(eval ,atom) stack loc site genv))
		    (else
		     (variable->node global loc site genv)))))
	     (else
	      (error-sexp->node "Illegal `@' expression" exp loc genv)))))
;*--- -> --------------------------------------------------------------*/
      ((-> . ?l)
       (if (and (pair? l) (pair? (cdr l)) (every symbol? l))
	   (field-ref->node l exp stack loc site genv)
	   (error-sexp->node "Illegal ->" exp loc genv)))
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
		  (sexp->node value stack loc site genv))
		 (else
		  (error-sexp->node "Illegal `quote' expression" exp loc genv)))))
          (else
	   (error-sexp->node "Illegal `quote' expression" exp loc genv))))
;*--- begin -----------------------------------------------------------*/
      ((begin)
       (sexp->node #unspecified stack (find-location/loc exp loc) site genv))
      ((begin . ?body)
       (let* ((loc (find-location/loc exp loc))
	      (nodes (sexp*->node body stack loc site genv)))
	  (instantiate::sequence
	     (loc loc)
	     (type *_*)
	     (nodes nodes))))
;*--- if --------------------------------------------------------------*/
      ((and ((or if (? if-sym?)) ?test #t #f)
	    (? (lambda (x) (eq? site 'test))))
       (sexp->node test stack loc site genv))
      (((or if (? if-sym?)) . ?-)
       (match-case exp
          ((?- ?si ?alors ?sinon)
	   (let ((nt (not-test si)))
	      (if nt
		  (begin
		     (set-car! (cdr exp) nt)
		     (set-car! (cddr exp) sinon)
		     (set-car! (cdddr exp) alors)
		     (sexp->node exp stack loc site genv))
		  (let* ((loc (find-location/loc exp loc))
			 (cdloc (find-location/loc (cdr exp) loc))
			 (cddloc (find-location/loc (cddr exp) loc))
			 (cdddloc (find-location/loc (cdddr exp) loc))
			 (l-si (find-location/loc si cdloc))
			 (l-alors (find-location/loc alors cddloc))
			 (l-sinon (find-location/loc sinon cdddloc))
			 (test (sexp->node si stack loc 'test genv)))
		     (cond
			((atom? test)
			 (with-access::atom test (value)
			    (if (not value)
				(sexp->node sinon stack l-sinon 'value genv)
				(sexp->node alors stack l-alors 'value genv))))
			((kwote? test)
			 (sexp->node alors stack l-alors 'value genv))
			((test-nestable? test)
			 (let ((alors (sexp->node alors stack l-alors 'value genv))
			       (sinon (sexp->node sinon stack l-sinon 'value genv)))
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
			    (sexp->node exp stack loc site genv))))))))
	  ((?- ?si ?alors)
           (set-cdr! (cddr exp) (list #unspecified))
           (sexp->node exp stack loc site genv))
          (else
	   (error-sexp->node "Illegal `if' form" exp loc genv))))
;*--- set! ------------------------------------------------------------*/
      ((set! (-> . ?l) ?val)
       (if (and (pair? l) (pair? (cdr l)) (every symbol? l))
	   (field-set->node l val exp stack loc site genv)
	   (error-sexp->node "Illegal ->" exp loc genv)))
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
				(symbol->string var) genv))	    
			  (else
			   (sexp->node val stack val-loc 'value genv)))))
	      (let ((ast (sexp->node var stack cdloc 'set! genv)))
		 (if (var? ast)
		     (with-access::var ast (variable)
			(if (and (global? variable)
				 (global-read-only? variable))
			    (error-sexp->node
			       "Read-only variable" exp cdloc genv)
			    (instantiate::setq
			       (loc loc)
			       (type *unspec*)
			       (var ast)
			       (value val))))
		     (error-sexp->node
			"illegal `set!' expression" exp loc genv)))))
          (else
	   (error-sexp->node
	      "Illegal `set!' form" exp (find-location/loc exp loc) genv))))
;*--- define ----------------------------------------------------------*/
      ((define . ?-)
       ;; define is very similar to `set!' except that is it not
       ;; considered as a mutation of the defined variable.
       (match-case exp
          ((?- ?var ?val)
	   (let* ((loc (find-location/loc exp loc))
		  (cdloc (find-location/loc (cdr exp) loc))
		  (cddloc (find-location/loc (cddr exp) loc))
		  (val-loc (find-location/loc val cddloc))
		  (val (sexp->node val stack val-loc 'value genv)))
	      (let ((ast (sexp->node var stack cdloc 'value genv)))
		 (if (and (var? ast) (global? (var-variable ast)))
		     (begin
			(global-src-set! (var-variable ast) val)
			(instantiate::setq
			   (loc loc)
			   (type *unspec*)
			   (var ast)
			   (value val)))
		     (error-sexp->node
			"illegal `define' expression" exp loc genv)))))
          (else
	   (error-sexp->node
	      "Illegal `define' form" exp (find-location/loc exp loc) genv))))
;*--- let & letrec ----------------------------------------------------*/
      (($let ?bindings . ?expr)
       (let ((exp (let->node exp stack loc 'value genv)))
	  (if (isa? exp let-var)
	      (with-access::let-var exp (removable?)
		 (set! removable? #f)
		 exp)
	      (error-sexp->node
		 "illegal `$let' expression, does not produce a let" exp loc genv))))
;*--- a pattern to improve pattern-matching compilation ---------------*/
      ((((or let (? let-sym?) letrec labels (? labels-sym?)) ?- ?body) . ?args)
       (let* ((let-part (car exp))
	      (nexp `(,(car let-part) ,(cadr let-part) (,body ,@args))))
	  (sexp->node nexp stack loc site genv)))
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
       (let->node exp stack loc 'value genv))
      ((letrec* . ?-)
       (letrec*->node exp stack loc 'value genv))
;*--- labels ----------------------------------------------------------*/
      (((or labels (? labels-sym?)) . ?-)
       (labels->node exp stack loc 'value genv))
;*--- the direct lambda applications (see match-case ...) -------------*/
      (((lambda (?var) (and ?body (?- . ?-) (? cast-sexp?))) (atom ?arg))
       (instantiate::cast
	  (loc loc)
	  (type (use-type! (cast-sexp-type body) loc))
	  (arg (sexp->node arg stack loc site genv))))
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
	  (let->node nexp stack loc 'value genv)))
;*--- the direct if applications --------------------------------------*/
      (((if ?test
	    (and (? proc-or-lambda?) ?proc1)
	    (and (? proc-or-lambda?) ?proc2))
	. (and (? (lambda (l) (every (lambda (l) (not (pair? l))) l))) ?args))
       (let ((nexp `(if ,test (,proc1 ,@args) (,proc2 ,@args))))
	  (sexp->node nexp stack (find-location/loc exp loc) site genv)))
      (((if ?test
	    (and (? proc-or-lambda?) ?proc1)
	    (and (? proc-or-lambda?) ?proc2))
	. (and (? list?) ?args))
       (let* ((tmps (map (lambda (_) (gensym)) args))
	      (loc (find-location/loc exp loc))
	      (nexp `(,(let-sym) ,(map list tmps args)
				 (if ,test (,proc1 ,@tmps) (,proc2 ,@tmps)))))
	  (let->node nexp stack (find-location/loc exp loc) site genv)))
;*--- lambda ----------------------------------------------------------*/
      ((lambda . ?-)
       (lambda->node exp stack loc site "L" genv))
;*--- pragma ----------------------------------------------------------*/
      ((pragma . ?-)
       (pragma/type->node #f #f *unspec* exp stack loc site genv))
;*--- pragma/effect ---------------------------------------------------*/
      ((pragma/effect ?effect . ?rest)
       (pragma/type->node #f
	  (parse-effect effect)
	  *unspec* `(pragma ,@rest) stack loc site genv))
;*--- free-pragma -----------------------------------------------------*/
      ((free-pragma . ?-)
       (pragma/type->node #t #f *unspec* exp stack loc site genv))
;*--- static-pragma ---------------------------------------------------*/
      ((static-pragma . ?-)
       (if (not (and (null? stack) (eq? site 'value)))
	   (error-sexp->node "Illegal `static-pragma' expression" exp loc genv)
	   (begin
	      (add-static-pragma!
		 (pragma/type->node #t #f *unspec* exp stack loc site genv))
	      (sexp->node #unspecified stack loc site genv))))
;*--- pragma/effect ---------------------------------------------------*/
      ((free-pragma/effect ?effect . ?rest)
       (pragma/type->node #t
	  (parse-effect effect)
	  *unspec* `(pragma ,@rest) stack loc site genv))
;*--- cast-null -------------------------------------------------------*/
      ((cast-null ?type)
       (instantiate::cast-null
	  (c-format "")
	  (loc (find-location/loc exp loc))
	  (type (find-type type))))
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
		  (proc (sexp->node proc stack loc-proc 'value genv))
		  (msg (sexp->node msg stack loc-msg 'value genv))
		  (obj (sexp->node obj stack loc-obj 'value genv)))
	      (instantiate::fail
		 (loc loc)
		 (type *magic*)
		 (proc proc)
		 (msg msg)
		 (obj obj))))
          (else
	   (error-sexp->node
	      "Illegal `failure' form" exp (find-location/loc exp loc) genv))))
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
			   'value genv)))
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
				       'value genv))
			    (nclause (cons (car clause) body)))
			;; we check that it is not an illegal `else' clause
			(if (and (eq? (car clause) 'else)
				 (not (null? (cdr cls))))
			    (error-sexp->node
			       "Illegal `case' form" exp (find-location/loc exp loc) genv)
			    (loop (cdr cls)
			       (cons (epairify nclause clause)
				  nclauses))))))))
          (else
	   (error-sexp->node
	      "Illegal `case' form" exp (find-location/loc exp loc) genv))))
;*--- set-exit --------------------------------------------------------*/
      ((set-exit . ?-)
       (set-exit->node exp stack loc site genv))
;*--- jump-exit -------------------------------------------------------*/
      ((jump-exit . ?-)
       (jump-exit->node exp stack loc site genv))
;*--- apply -----------------------------------------------------------*/
      ((apply ?- ?-)
       (applycation->node exp stack loc site genv))
;*--- synchronize -----------------------------------------------------*/
      ((synchronize ?mutex :prelock ?prelock . ?body)
       (synchronize->node exp mutex prelock body stack (find-location/loc exp loc) site genv))
      ((synchronize ?mutex . ?body)
       (synchronize->node exp mutex ''() body stack (find-location/loc exp loc) site genv))
;*--- private ---------------------------------------------------------*/
      ((#unspecified)
       (error-sexp->node
	  "Illegal `application' form" exp (find-location/loc exp loc) genv))
      ((? private-sexp?)
       (private-node exp stack loc site genv))
;*--- app -------------------------------------------------------------*/
      (else
       ;; this expression can be a function call or a typed pragma
       ;; form. We first check to see if it is a pragma. If it is not
       ;; we compile a function call. This check is required by the
       ;; form (pragma::??? ...) (because we can't add a branch in the
       ;; match-case to check the node `pragma::???').
       (call->node exp stack loc site genv))))

;*---------------------------------------------------------------------*/
;*    lambda->node ...                                                 */
;*---------------------------------------------------------------------*/
(define (lambda->node exp stack loc site prefname genv)
   (match-case exp
      ((?- ?args . ?body) 
       (let ((loc (find-location/loc exp loc))
	     (fun (make-anonymous-name loc prefname)))
	  (sexp->node `(,(labels-sym) ((,fun ,args ,(normalize-progn body))) ,fun)
	     stack
	     loc
	     site genv)))
      (else
       (error-sexp->node "Illegal `lambda' form"
	  exp
	  (find-location/loc exp loc) genv))))

;*---------------------------------------------------------------------*/
;*    variable->node ...                                               */
;*---------------------------------------------------------------------*/
(define (variable->node v loc site genv)
   (use-variable! v loc site)
   (if (and (not (eq? site 'app)) (fun? (variable-value v)))
       (if (and (global? v)
		(cfun? (global-value v))
		(not (backend-foreign-closure (the-backend)))
		(not (eq? site 'apply)))
	   (error-sexp->node
	      (format "Backend (~a) does not support external first class function" (backend-name (the-backend))) exp loc genv)
	   (instantiate::closure
	      (loc loc)
	      (type (strict-node-type *procedure* (variable-type v)))
	      (variable v)))
       (instantiate::ref
	  (loc loc)
	  (type (strict-node-type *_* (variable-type v)))
	  (variable v))))
   
;*---------------------------------------------------------------------*/
;*    call->node ...                                                   */
;*---------------------------------------------------------------------*/
(define (call->node exp stack loc site genv)
   (let ((caller (car exp))
	 (loc (find-location/loc exp loc)))
      (if (symbol? caller)
	  ;; it might be a typed special forms (such as pragma or lambda)
	  (let* ((pid (parse-id caller loc))
		 (id (car pid))
		 (type (cdr pid)))
	     (case id
		((pragma)
		 (pragma/type->node #f #f type exp stack loc site genv))
		((pragma/effect)
		 (if (not (pair? (cdr exp)))
		     (error-sexp->node
			"Illegal pragma/effect"
			exp (find-location/loc exp loc) genv)
		     (pragma/type->node #f (parse-effect (cadr exp))
			type `((car exp) ,@(cddr exp))
			stack loc site genv)))
		((free-pragma)
		 (pragma/type->node #t #f type exp stack loc site genv))
		((free-pragma/effect)
		 (if (not (pair? (cdr exp)))
		     (error-sexp->node
			"Illegal free-pragma/effect"
			exp (find-location/loc exp loc) genv)
		     (pragma/type->node #t (parse-effect (cadr exp))
			type `((car exp) ,@(cddr exp))
			stack loc site genv)))
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
			   site genv)))
		    (else
		     (error-sexp->node "Illegal lambda" exp loc genv))))
		(else
		 (application->node exp stack loc site genv))))
	  (application->node exp stack loc site genv))))
 
;*---------------------------------------------------------------------*/
;*    sexp*->node ...                                                  */
;*---------------------------------------------------------------------*/
(define (sexp*->node::pair-nil exp*::pair-nil stack loc site genv)
   (let loop ((exps exp*)
	      (res  '())
	      (loc  loc))
      (cond
	 ((null? exps)
	  (error-sexp->node "Illegal empty sequence" exps loc genv))
	 ((null? (cdr exps))
	  (let ((loc (find-location/loc (car exps)
					(find-location/loc exps loc)))
		(nsite (if (eq? site 'app) 'value site)))
	     (reverse! (cons (sexp->node (car exps) stack loc nsite genv) res))))
	 (else
	  (let ((loc (find-location/loc (car exps)
					(find-location/loc exps loc))))
	     (loop (cdr exps)
		   (cons (sexp->node (car exps) stack loc 'value genv) res)
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
	 (error-sexp->node "Illegal mutation" (shape var) loc (get-genv)))))

;*---------------------------------------------------------------------*/
;*    error-sexp->node ...                                             */
;*---------------------------------------------------------------------*/
(define (error-sexp->node msg exp loc genv)
   (user-error/location loc
      (shape (current-function))
      msg
      exp
      (sexp->node ''() '() loc 'value genv)))

;*---------------------------------------------------------------------*/
;*    define-primop-ref->node ...                                      */
;*---------------------------------------------------------------------*/
(define (define-primop-ref->node global ref genv)
   (let ((fun (sexp->node '(@ define-primop-ref! __evenv) '() #f 'app genv)))
      (if (var? fun)
	  (instantiate::app
	     (type *_*)
	     (fun fun)
	     (args (list (sexp->node `',(global-id global) '() #f 'value genv) ref)))
	  fun)))

;*---------------------------------------------------------------------*/
;*    define-primop-ref/src->node ...                                  */
;*---------------------------------------------------------------------*/
(define (define-primop-ref/src->node global ref src genv)
   (if (not (epair? src))
       (define-primop-ref->node global ref genv)
       (match-case (cer src)
	  ((at ?fname ?loc)
	   (let ((fun (sexp->node '(@ define-primop-ref/loc! __evenv) '() #f 'app genv)))
	      (if (var? fun)
		  (instantiate::app
		     (type *_*)
		     (fun fun)
		     (args (list
			      (sexp->node `',(global-id global) '() #f 'value genv)
			      ref
			      (sexp->node fname '() #f 'value genv)
			      (sexp->node loc '() #f 'value genv))))
		  fun)))
	  (else
	   (define-primop-ref->node global ref genv)))))

;*---------------------------------------------------------------------*/
;*    define-primop->node ...                                          */
;*---------------------------------------------------------------------*/
(define (define-primop->node global genv)
   (use-variable! global #f 'value)
   (let ((fun (sexp->node '(@ define-primop! __evenv) '() #f 'app genv)))
      (if (var? fun)
	  (instantiate::app
	     (type *_*)
	     (fun fun)
	     (args (list (sexp->node `',(global-id global) '() #f 'value genv)
			 (sexp->node global '() #f 'value genv))))
	  fun)))
   
;*---------------------------------------------------------------------*/
;*    location->node ...                                               */
;*---------------------------------------------------------------------*/
(define (location->node global genv)
   (use-variable! global #f 'value)
   (let ((fun (sexp->node '(@ __evmeaning_address foreign) '() #f 'app genv)))
      (if (var? fun)
	  (let ((expr (if (backend-pragma-support (the-backend))
			  `(pragma::obj "($1)" ,global)
			  global)))
	     (instantiate::app
		(type (strict-node-type *_* *obj*))
		(fun fun)
		(args (list (sexp->node expr '() #f 'value genv)))))
	  fun)))
   
;*---------------------------------------------------------------------*/
;*    make-anonymous-name ...                                          */
;*---------------------------------------------------------------------*/
(define (make-anonymous-name::symbol loc . pref)
   (let ((pref (if (and (pair? pref) (string? (car pref)))
		   (car pref)
		   "L")))
      (if (location? loc)
	  (let ((file (location-fname loc))
		(line (location-lnum loc))
		(base (symbol->string (gensym (string-append "<@" pref ":")))))
	     (cond
		((string-contains pref "<@")
		 (symbol-append (string->symbol pref) (gensym 'L)))
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
(define (synchronize->node exp mutex prelock body stack loc site genv)
   (if (symbol? mutex)
       (instantiate::sync
	  (loc loc)
	  (type *_*)
	  (mutex (sexp->node mutex stack loc 'value genv))
	  (prelock (sexp->node prelock stack loc 'value genv))
	  (body (sexp->node (normalize-progn body) stack loc site genv)))
       (let* ((v (mark-symbol-non-user! (gensym 'mutex)))
	      (var (make-typed-ident v 'mutex))
	      (nexp (epairify-rec `(synchronize ,v ,@body) exp)))
	  (replace! exp `(,(let-sym) ((,var ,mutex)) ,nexp))
	  (sexp->node exp stack loc site genv))))
	     
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

;*---------------------------------------------------------------------*/
;*    test-nestable? ...                                               */
;*    -------------------------------------------------------------    */
;*    Returns #t iff expr does not need to be bound to a temporary     */
;*    inside a condition expression.                                   */
;*---------------------------------------------------------------------*/
(define (test-nestable? expr)
   (or (var? expr)
       (when (app? expr)
	  (let ((var (var-variable (app-fun expr))))
	     (when (global? var)
		(and (memq 'nesting (global-pragma var))
		     (memq 'args-safe (global-pragma var))))))))
