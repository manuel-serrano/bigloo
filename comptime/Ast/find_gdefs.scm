;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/find_gdefs.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun  3 11:21:26 1996                          */
;*    Last change :  Mon Nov 30 09:08:17 2015 (serrano)                */
;*    -------------------------------------------------------------    */
;*    This module implements a function which travers an entire        */
;*    unit in order to find the global declared variable and their     */
;*    mutation property.                                               */
;*    -------------------------------------------------------------    */
;*    This module uses property list (under a gensymed symbol). The    */
;*    property is cleared after the pass.                              */
;*    -------------------------------------------------------------    */
;*    furthermore, in order to help aliases compilation. When a        */
;*    global variable is bound to a function, we return the arity      */
;*    of the function (see eta-expanse@ast_unit).                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_find-gdefs

   (include "Tools/trace.sch"
	    "Ast/node.sch")
   
   (import  ast_ident
	    tools_shape
	    tools_error
	    tools_dsssl
	    tools_args
	    engine_param
	    module_module
	    (find-location tools_location))
   
   (export  (to-be-define! ::global)
	    (check-to-be-define)
	    (find-global-defs sexp*)))

;*---------------------------------------------------------------------*/
;*    def ...                                                          */
;*---------------------------------------------------------------------*/
(define-struct def id access arity)
   
;*---------------------------------------------------------------------*/
;*    *to-be-define* ...                                               */
;*---------------------------------------------------------------------*/
(define *to-be-define* '())

;*---------------------------------------------------------------------*/
;*    to-be-define! ...                                                */
;*---------------------------------------------------------------------*/
(define (to-be-define! global)
   (set! *to-be-define* (cons global *to-be-define*)))

;*---------------------------------------------------------------------*/
;*    check-to-be-define                                               */
;*    -------------------------------------------------------------    */
;*    While doing this conversion we erase the property list of        */
;*    global identifier.                                               */
;*---------------------------------------------------------------------*/
(define (check-to-be-define)
   (for-each (lambda (global)
		(let ((def (getprop (global-id global) *gdef-key*)))
		   (if (not (def? def))
		       (user-error (shape global)
				   "Can't find global definition"
				   (shape (global-src global))
				   '()))))
	     *to-be-define*)
   ;; we remove all the property list
   (for-each (lambda (id) (remprop! id *gdef-key*)) *all-defined-id*)
   (set! *all-defined-id* '())
   ;; we are done with the `*to-be-define*' list then we restet it.
   (set! *to-be-define* '()))
 
;*---------------------------------------------------------------------*/
;*    *gdef-key* ...                                                   */
;*    -------------------------------------------------------------    */
;*    This variable holds the gensymed symbol for the implementation   */
;*    with property list of the simili global environment used in      */
;*    this passed.                                                     */
;*---------------------------------------------------------------------*/
(define *gdef-key* (gensym 'find-gdefs))

;*---------------------------------------------------------------------*/
;*    *gdefs-list* ...                                                 */
;*---------------------------------------------------------------------*/
(define *gdefs-list*     '())
(define *all-defined-id* '())

;*---------------------------------------------------------------------*/
;*    find-global-def ...                                              */
;*---------------------------------------------------------------------*/
(define (find-global-def id)
   (getprop id *gdef-key*))

;*---------------------------------------------------------------------*/
;*    bind-global-def! ...                                             */
;*---------------------------------------------------------------------*/
(define (bind-global-def! id arity)
   (let ((def (def id 'read arity)))
      (set! *gdefs-list* (cons def *gdefs-list*))
      (set! *all-defined-id* (cons id *all-defined-id*))
      (putprop! id *gdef-key* def)))

;*---------------------------------------------------------------------*/
;*    defs->list ...                                                   */
;*    -------------------------------------------------------------    */
;*    we convert the def list in a list of pairs (the car is the id    */
;*    of the global been defined and the cdr is its mutation property).*/
;*---------------------------------------------------------------------*/
(define (defs->list)
   (let loop ((defs *gdefs-list*)
	      (res  '()))
      (if (null? defs)
	  (begin
	     (set! *gdefs-list* '())
	     res)
	  (let ((def (car defs)))
	     (loop (cdr defs)
		   (cons (cons (def-id def) (cons (def-access def)
						  (def-arity def)))
			 res))))))

;*---------------------------------------------------------------------*/
;*    find-global-defs ...                                             */
;*    -------------------------------------------------------------    */
;*    We scan the entire source expression in order to find            */
;*    all the global variable declarations and their mutations. We     */
;*    use two passes, the first one does not traverse the expression   */
;*    sources while the second does (to find mutations).               */
;*    -------------------------------------------------------------    */
;*    This function check for multiple definition and for unbound      */
;*    variables (only variables which have been declared and not       */
;*    defined).                                                        */
;*---------------------------------------------------------------------*/
(define (find-global-defs sexp*)
   ;; we reset the global variables
   (set! *gdefs-list* '())
   ;; what to do when seing a global definition...
   (define (define-global var arity exp)
      (match-case var
	 ((or (@ ?pre-id ?-) ?pre-id)
	  (let* ((id (id-of-id pre-id (find-location exp)))
		 (old-def (find-global-def id)))
	     (if (def? old-def)
		 (user-error var "Illegal duplicated definition" exp '())
		 (bind-global-def! id arity))))
	 (else
	  (internal-error "find-globald-defs" "Illegal define form" var))))
   ;; the generic scanning function
   (define (scan sexp* action-define action-body)
      (let loop ((sexp* sexp*))
	 (if (not (pair? sexp*))
	     'done 
	     (let ((sexp (car sexp*)))
		(match-case sexp
		   ((begin . ?nsexp*)
		    (loop nsexp*)
		    (loop (cdr sexp*)))
		   ((or (define (?var . ?args) . ?exp)
			(define-inline (?var . ?args) . ?exp)
			(define-generic (?var . ?args) . ?exp))
		    (action-define var (global-arity args) sexp)
		    (action-body args exp sexp)
		    (loop (cdr sexp*)))
		   ((define-method (?var . ?args) . ?exp)
		    (action-body args exp sexp)
		    (loop (cdr sexp*)))
		   ((define ?var . ?exp)
		    (action-define var #f sexp)
		    (action-body '() exp sexp)
		    (loop (cdr sexp*)))
		   (else
		    (action-body '() (list sexp) sexp)
		    (loop (cdr sexp*))))))))
   ;; the first pass where we defines global variables
   (scan sexp*
	 define-global
	 (lambda (args exp def) 'nothing))
   ;; the second pass where we traverse top level expressions
   (scan sexp*
	 (lambda (x y exp)
	    'nothing)
	 (lambda (args exp def)
	    (find-mutations! exp (push-args args '() (find-location def)))))
   ;; we return the list of the global variables and their mutations
   (defs->list))

;*---------------------------------------------------------------------*/
;*    find-mutations! ...                                              */
;*---------------------------------------------------------------------*/
(define (find-mutations! exp* stack)
   (for-each (lambda (exp) (find-1-mutations! exp stack)) exp*))

;*---------------------------------------------------------------------*/
;*    find-1-mutations! ...                                            */
;*---------------------------------------------------------------------*/
(define (find-1-mutations! exp stack)
   (match-case exp
      ((atom ?atom)
       'done)
      ((quote ?-)
       'done)
      ((pragma ?-)
       'done)
      ((free-pragma ?-)
       'done)
      ((assert ?- ?f- . ?body)
       (find-mutations! body stack))
      ((begin . ?body)
       (find-mutations! body stack))
      ((set! (and (? symbol?) ?id) . ?val)
       (find-mutations! val stack)
       (if (not (memq id stack))
	   (let ((def (find-global-def id)))
	      (if (def? def)
		  (def-access-set! def 'write)))))
      ((set! (@ (and (? symbol?) ?id) (and (? symbol?) ?module)) . ?val)
       (find-mutations! val stack)
       (if (eq? module *module*)
	   (let ((def (find-global-def id)))
	      (if (def? def)
		  (def-access-set! def 'write)))))
      ((let ?bindings . ?body)
       (let ((new-stack (let loop ((stack    stack)
				   (bindings bindings))
			   (cond
			      ((null? bindings)
			       stack)
			      ((not (pair? bindings))
			       (user-error "let" "Illegal bindings" exp '()))
			      ((not (pair? (car bindings)))
			       (loop (cons (id-of-id (car bindings)
						     (find-location exp))
					   stack)
				     (cdr bindings)))
			      (else
			       (loop (cons (id-of-id (car (car bindings))
						     (find-location exp))
					   stack)
				     (cdr bindings)))))))
	  (find-mutations! body new-stack)
	  (when (pair? bindings)
	     (for-each (lambda (b) (find-mutations! (cdr b) stack)) bindings))))
      ((letrec ?bindings . ?body)
       (let ((new-stack (let loop ((stack stack)
				   (bindings bindings))
			   (cond
			      ((null? bindings)
			       stack)
			      ((not (pair? bindings))
			       (user-error "letrec" "Illegal bindings" exp '()))
			      ((not (pair? (car bindings)))
			       (loop (cons (id-of-id (car bindings)
						     (find-location exp))
					   stack)
				     (cdr bindings)))
			      (else
			       (loop (cons (id-of-id (car (car bindings))
						     (find-location exp))
					   stack)
				     (cdr bindings)))))))
	  (find-mutations! body new-stack)
	  (when (pair? bindings)
	     (for-each (lambda (b) (find-mutations! (cdr b) new-stack))
		       bindings))))
      ((labels ?bindings . ?body)
       (let* ((loc (find-location exp))
	      (new-stack (push-args (map car bindings) stack loc)))
	  (find-mutations! body new-stack)
	  (when (pair? bindings)
	     (for-each (lambda (b)
			  (find-mutations! (cddr b)
					   (push-args (cadr b) new-stack loc)))
		       bindings))))
      ((lambda ?args . ?body)
       (find-mutations! body (push-args args stack (find-location exp))))
      ((bind-exit ?exit . ?body)
       (find-mutations! body (cons (id-of-id exit (find-location exp)) stack)))
      ((apply ?proc ?arg)
       (find-1-mutations! proc stack)
       (find-1-mutations! arg stack))
      ((case ?val ?clauses)
       (find-mutations! val stack)
       (for-each (lambda (c) (find-mutations! (cdr c) stack)) clauses))
      ((if . ?body)
       (find-mutations! body stack))
      ((define ?name . ?body)
       (find-mutations! body (cons name stack)))
      ((define-inline ?name . ?body)
       (user-error name "Illegal `define-inline' form" body '()))
      ((define-generic ?name . ?body)
       (user-error name "Illegal `define-generic' form" body '()))
      ((define-method (?- . ?args) . ?body)
       (find-mutations! body (push-args args stack (find-location exp))))
      (else
       (let ((caller (car exp))
	     (loc    (find-location exp)))
	  (if (symbol? caller)
	      ;; it might be a typed special forms (such as pragma or lambda)
	      (let* ((pid  (parse-id caller loc))
		     (id   (car pid))
		     (type (cdr pid)))
		 (case id
		    ((pragma)
		     'done)
		    ((free-pragma)
		     'done)
		    ((lambda)
		     (match-case exp
			((?- ?args . ?body)
			 (find-mutations! body (push-args args stack loc)))
			(else
			 (user-error "lambda"
				     "Illegal `lambda' form"
				     exp
				     '()))))
		    (else
		     (find-mutations! exp stack))))
	      (find-mutations! exp stack))))))

;*---------------------------------------------------------------------*/
;*    push-args ...                                                    */
;*    -------------------------------------------------------------    */
;*    We push args and we parse a formal list (according to dsssl      */
;*    formal arguments).					       */
;*---------------------------------------------------------------------*/
(define (push-args expr0 list loc)
   (let loop ((expr  expr0)
	      (list  list)
	      (dsssl #f))
      (cond
	 ((null? expr)
	  list)
	 ((not (pair? expr))
	  (cond
	     (dsssl
	      (user-error/location loc
				   'lambda
				   "Can't use both DSSSL named constant, and `.' notation"
				   expr))
	     ((not (symbol? expr))
	      (user-error/location loc
				   'lambda
				   "Illegal formal parameter, symbol expected"
				   expr))
	     (else
	      (cons (id-of-id expr loc) list))))
	 ((not (symbol? (car expr)))
	  (cond
	     ((dsssl-named-constant? (car expr))
	      (loop (cdr expr) list #t))
	     ((not dsssl)
	      (user-error/location (or (find-location expr) loc)
				   'lambda
				   "Illegal formal parameter, symbol expected"
				   expr))
	     ((dsssl-defaulted-formal? (car expr))
	      (loop (cdr expr)
		    (cons (id-of-id (dsssl-default-formal (car expr)) loc)
			  list)
		    #t))
	     (else
	      (user-error/location (or (find-location expr) loc)
				   'lambda
				   "Illegal formal parameter, symbol or named constant expected"
				   expr))))
	 (else
	  (loop (cdr expr)
		(cons (id-of-id (car expr) loc) list)
		dsssl)))))

