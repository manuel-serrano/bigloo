;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/app.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jun 21 09:34:48 1996                          */
;*    Last change :  Wed May 31 10:30:01 2017 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The application compilation                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_app
   (include "Ast/node.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_location
	    tools_shape
	    type_cache
	    tvector_tvector
 	    ast_sexp
	    ast_ident
	    ast_local
	    ast_let
	    ast_private
	    tools_dsssl
	    expand_eps)
   (import type_typeof)
   (export  (application->node::node ::obj ::obj ::obj ::symbol)
	    (make-app-node::node stack loc site var::var args)
	    (correct-arity-app?::bool ::variable ::obj)))

;*---------------------------------------------------------------------*/
;*    correct-arity-app? ...                                           */
;*    -------------------------------------------------------------    */
;*    We check functional application arity in order to print, as      */
;*    soon as possible, user errors.                                   */
;*---------------------------------------------------------------------*/
(define (correct-arity-app? var::variable args)
   (let* ((fun     (variable-value var))
	  (nb-args (length args))
	  (arity   (fun-arity fun)))
      (cond
	 ((=fx arity -1)
	  #t)
	 ((>=fx arity 0)
	  (= arity nb-args))
	 (else
	  (<=fx (-fx (negfx arity) (+fx nb-args 1)) 0)))))

;*---------------------------------------------------------------------*/
;*    clean-user-node! ...                                             */
;*    -------------------------------------------------------------    */
;*    This function walk thru a let-var node to unset the user         */
;*    property of local variables.                                     */
;*---------------------------------------------------------------------*/
(define (clean-user-node! node)
   (let loop ((walk node))
      (if (let-var? walk)
	  (begin
	     (for-each (lambda (binding)
			  (local-user?-set! (car binding) #f))
		       (let-var-bindings walk))
	     (loop (let-var-body walk)))
	  node)))

;*---------------------------------------------------------------------*/
;*    application->node ...                                            */
;*    -------------------------------------------------------------    */
;*    Each parameters which is not a variable _is_ forced to be bound  */
;*    to a variable.                                                   */
;*---------------------------------------------------------------------*/
(define (application->node exp stack loc site)
   
   (define (atomic? exp)
      (match-case exp
	 ((atom ?-)
	  #t)
	 ((? symbol?)
	  #t)
	 ((? keyword?)
	  #t)
	 (((kwote quote) . ?-)
	  #t)
	 ((@ (? symbol?) (? symbol?))
	  #t)
	 (else
	  #f)))

   (define (all-subexp-symbol? exp)
      (let loop ((exp exp))
	 (cond
	    ((null? exp)
	     #t)
	    ((atomic? (car exp))
	     (loop (cdr exp)))
	    (else
	     (and (or (atom? (car exp)) (var? (car exp)))
		  (loop (cdr exp)))))))

   (define (wrong-number-of-arguments exp loc fun args)
      (let* ((var     (var-variable fun))
	     (fun     (variable-value var))
	     (nb-args (length args))
	     (arity   (cond
			 ((fun? fun)
			  (fun-arity fun))
			 (else
			  -1)))
	     (expect  (cond
			 ((>=fx arity 0)
			  (cond
			     ((sfun-optional? fun)
			      (string-append "["
				 (number->string arity)
				 ".."
				 (number->string
				    (+ arity
				       (length (sfun-optionals fun))))
				 "] args expected, "))
			     ((sfun-key? fun)
			      (string-append "["
				 (number->string arity)
				 ".."
				 (number->string
				    (+ arity
				       (* 2 (length (sfun-keys fun)))))
				 "] args expected, "))
			     (else
			      (string-append (number->string arity)
				 " arg(s) expected, "))))
			 (else
			  (string-append (number->string (negfx (+fx arity 1)))
			     " or more arg(s) expected, "))))
	     (provide (string-append (number->string (length args)) " provided")))
	 (error-sexp->node
	    (string-append "Illegal application: " expect provide)
	    (shape exp)
	    loc)))
   
   (let* ((loc (find-location/loc exp loc))
	  (err-nb *nb-error-on-pass*)
	  (debugstamp (gensym))
	  (fun (sexp->node (car exp) stack loc 'app))
	  (fun-err? (>fx *nb-error-on-pass* err-nb)))
      (if (and (all-subexp-symbol? exp) (var? fun))
	  (let* ((args  (cdr exp))
		 (delta (check-user-app fun args)))
	     (cond
		((not (var? fun))
		 (sexp->node ''() stack loc 'value))
		((=fx delta 0)
		 (make-app-node stack loc site fun args))
		(else
		 (wrong-number-of-arguments exp loc fun args))))
	  (let loop ((old-args (cdr exp))
		     (new-args '())
		     (bindings '()))
	     (cond
		((null? old-args)
		 (let ((old-fun      (car exp))
		       (make-the-app (lambda (fun)
					(if (pair? bindings)
					    `(,(let-sym) ,(reverse! bindings)
						(,fun ,@(reverse! new-args)))
					    `(,fun ,@(reverse! new-args))))))
		    (if (var? fun)
			(begin
			   (let ((node (sexp->node (make-the-app fun)
						   stack
						   loc
						   site)))
			      (clean-user-node! node)))
			(let* ((new-fun (mark-symbol-non-user! (gensym 'fun)))
			       (lexp `(,(let-sym) ((,new-fun ,(if fun-err?
								  '(@ list __r4_pairs_and_lists_6_3)
							   fun)))
					 ,(make-the-app new-fun)))
			       (node (sexp->node lexp stack loc site)))
			   (clean-user-node! node)))))
		((atomic? (car old-args))
		 (loop (cdr old-args)
		       (if (epair? old-args)
			   (econs (car old-args) new-args (cer old-args))
			   (cons (car old-args) new-args))
		       bindings))
		(else
		 (let ((new-arg (mark-symbol-non-user! (gensym 'arg))))
		    (loop (cdr old-args)
			  (if (epair? old-args)
			      (econs new-arg new-args (cer old-args))
			      (cons new-arg new-args))
			  (cons (list new-arg (car old-args)) bindings)))))))))

;*---------------------------------------------------------------------*/
;*    check-user-app ...                                               */
;*    -------------------------------------------------------------    */
;*    We check functional application arity in order to print, as      */
;*    soon as possible, user errors.                                   */
;*    -------------------------------------------------------------    */
;*    This function returns an integer denoting the delta between      */
;*    expected and provided arguments.                                 */
;*---------------------------------------------------------------------*/
(define (check-user-app fun args)
   (if (not (var? fun))
       ;; we may have found an error while compiling the
       ;; function and have compiled '() instead. In order
       ;; to not print several messages for the same error
       ;; we skip the arity one here.
       0
       (let* ((var (var-variable fun))
	      (fun (variable-value var))
	      (nb-args (length args))
	      (arity (cond
			((fun? fun)
			 (fun-arity fun))
			(else
			 -1))))
	  (cond
	     ((=fx arity -1)
	      0)
	     ((>=fx arity 0)
	      (cond
		 ((sfun-optional? fun)
		  (cond
		     ((< nb-args arity)
		      (-fx arity nb-args))
		     ((> nb-args (+fx arity (length (sfun-optionals fun))))
		      (-fx (+fx arity (length (sfun-optionals fun))) nb-args))
		     (else
		      0)))
		 ((sfun-key? fun)
		  (let ((kl (length (sfun-keys fun))))
		     (cond
			((< nb-args arity)
			 (-fx arity nb-args))
			((> nb-args (+fx arity (*fx kl 2)))
			 (-fx (+fx arity (*fx kl 2)) nb-args))
			(else
			 0))))
		 (else
		  (-fx arity nb-args))))
	     (else
	      (if (<=fx (-fx (negfx arity) (+fx nb-args 1)) 0)
		  0
		  1))))))

;*---------------------------------------------------------------------*/
;*    make-app-node ...                                                */
;*---------------------------------------------------------------------*/
(define (make-app-node stack loc site var args)
   (let ((fun (variable-value (var-variable var))))
      (cond
	 ((and (sfun? fun) (pair? (sfun-optionals fun)))
	  (let ((args (let loop ((args args)
				 (res  '()))
			 (if (null? args)
			     (reverse! res)
			     (let ((a   (car args))
				   (loc (find-location/loc args loc)))
				(loop (cdr args)
				      (cons (sexp->node a stack loc 'value)
					    res)))))))
	     (make-optionals-app-node stack loc site var args)))
	 ((and (sfun? fun) (pair? (sfun-keys fun)))
	  (let ((args (let loop ((args args)
				 (res  '()))
			 (if (null? args)
			     (reverse! res)
			     (let ((a   (car args))
				   (loc (find-location/loc args loc)))
				(loop (cdr args)
				      (cons (sexp->node a stack loc 'value)
					    res)))))))
	     (make-keys-app-node stack loc site var args)))
	 ((or (not (fun? fun)) (>=fx (fun-arity fun) 0) (cfun? fun))
	  (let ((args (let loop ((args args)
				 (res  '()))
			 (if (null? args)
			     (reverse! res)
			     (let ((a   (car args))
				   (loc (find-location/loc args loc)))
				(loop (cdr args)
				      (cons (sexp->node a stack loc 'value)
					    res)))))))
	     (make-fx-app-node loc var args)))
	 (else
	  (make-va-app-node (fun-arity fun) stack loc var args)))))

;*---------------------------------------------------------------------*/
;*    make-optionals-app-node ...                                      */
;*---------------------------------------------------------------------*/
(define (make-optionals-app-node stack loc site var actuals)
   (let ((v (var-variable var))
	 (len (length actuals)))
      (with-access::sfun (variable-value v) (arity args optionals args-name)
	 (if (=fx len (+fx arity (length optionals)))
	     ;; all optional arguments are provided
	     (instantiate::app
		(loc loc)
		(type (strict-node-type *_* (variable-type v)))
		(fun  (if (closure? var) (duplicate::var var) var))
		(args actuals))
	     ;; compile the optionals arguments in a left-to-right env
	     (let* ((reqs (take args-name arity))
		    (provs (map car (take optionals (-fx len arity))))
		    (opts (list-tail optionals (-fx len arity)))
		    (exp `(,(let-sym) ,(map list reqs (take actuals arity))
			     (let* (,@(map list provs (drop actuals arity))
				      ,@opts)
				(,var ,@reqs
				      ,@(map (lambda (i)
						(fast-id-of-id i loc))
					     provs)
				      ,@(map (lambda (o)
						(fast-id-of-id (car o) loc))
					     opts))))))
		(sexp->node (compile-expand (comptime-expand exp))
			    '() loc site))))))

;*---------------------------------------------------------------------*/
;*    make-keys-app-node ...                                           */
;*    -------------------------------------------------------------    */
;*    This function assumes a correct arity.                           */
;*---------------------------------------------------------------------*/
(define (make-keys-app-node stack loc site var args)
   (let ((v (var-variable var)))
      (with-access::sfun (variable-value v) (arity keys (formals args))
	 (let ((collected (let loop ((l (list-tail args arity)))
			     (cond
				((null? l)
				 '())
				((pair? (cdr l))
				 (cons (list (car l) (cadr l))
				       (loop (cddr l))))
				(else
				 (user-error/location
				  loc (variable-id v)
				  "Illegal keyword application"
				  (shape (car l))))))))
	    (define (funcall)
	       (let ((f (gensym (variable-id v))))
		  ;; introduce a funcall on purpose for using the
		  ;; var-args entry point without heap allocating
		  ;; it may be safer to use
		  ;; `(,(let-sym) ((f #unspecified)) (set! f ,v) (,f @args))
		  (let ((exp `(,(let-sym) ((,f ,v)) (,f ,@args))))
		     (sexp->node exp stack loc site))))
	    (define (unique vals)
	       (let loop ((vs vals)
			  (nvals '()))
		  (cond
		     ((null? vs)
		      (reverse! nvals))
		     ((null? (cdr vs))
		      (reverse! (cons (car vs) nvals)))
		     ((eq? (caar vs) (car (cadr vs)))
		      (user-warning/location
		       loc (variable-id v)
		       "Illegal duplicated key" (caar vs))
		      (loop (cdr vs) nvals))
		     (else
		      (loop (cdr vs) (cons (car vs) nvals))))))
	    (define (body keys vals stack)
	       ;; prepare the keyword arguments
	       (let loop ((keys keys)
			  (vals vals)
			  (env '())
			  (stack stack))
		  (cond
		     ((null? keys)
		      (instantiate::app
			 (loc loc)
			 (type (strict-node-type *_* (variable-type v)))
			 (fun var)
			 (args (append
				(take args arity)
				(map (lambda (v)
					(sexp->node v stack loc site))
				     (reverse! env))))))
		     ((null? vals)
		      (let* ((it (parse-id (caar keys) loc))
			     (var (make-user-local-svar (car it) (cdr it)))
			     (val (sexp->node (cadar keys) stack loc site))
			     (nenv (cons var env))
			     (nstack (cons var stack))
			     (body (loop (cdr keys) '() nenv nstack)))
			 (instantiate::let-var
			    (loc loc)
			    (type (strict-node-type *_* (variable-type v)))
			    (bindings (list (cons var val)))
			    (body body))))
		     ((eq? (fast-id-of-id (caar keys) loc) (caar vals))
		      (let* ((it (parse-id (caar vals) loc))
			     (var (make-user-local-svar (car it) (cdr it)))
			     (val (sexp->node (cadar vals) stack loc site))
			     (nenv (cons var env))
			     (nstack (cons var stack))
			     (body (loop (cdr keys) (cdr vals) nenv nstack)))
			 (instantiate::let-var
			    (loc loc)
			    (type (strict-node-type *_* (variable-type v)))
			    (bindings (list (cons var val)))
			    (body body))))
		     (else
		      (let* ((it (parse-id (caar keys) loc))
			     (var (make-user-local-svar (car it) (cdr it)))
			     (val (sexp->node (cadar keys) stack loc site))
			     (nenv (cons var env))
			     (nstack (cons var stack))
			     (body (loop (cdr keys) vals nenv nstack)))
			 (instantiate::let-var
			    (loc loc)
			    (type (strict-node-type *_* (variable-type v)))
			    (bindings (list (cons var val)))
			    (body body)))))))
	    (if (any (lambda (v)
			 (let ((a (car v)))
			    (not (and (atom? a) (keyword? (atom-value a))))))
		      collected)
		(funcall)
		(let ((vals (unique
			     (dsssl-key-args-sort
			      (map (lambda (v)
				      (list (keyword->symbol
					     (atom-value (car v)))
					    (cadr v)))
				   collected)))))
		   ;; check that all keys are correct
		   (let* ((fkeys (map (lambda (k)
					 (fast-id-of-id (car k) loc))
				      keys))
			  (err (filter (lambda (v)
					  (not (memq (car v) fkeys)))
				       vals)))
		      (when (pair? err)
			 (user-error/location
			  loc (variable-id v)
			  "Illegal keyword(s) argument(s)" (map car err))))
		   (body keys vals stack)))))))

;*---------------------------------------------------------------------*/
;*    make-fx-app-node ...                                             */
;*    -------------------------------------------------------------    */
;*    This function produces nodes that represent function calls in    */
;*    the AST. If the called function is special (mainly because it is */
;*    an operator), this function emits an ad-hoc node. Otherwise, it  */
;*    emits an APP node if the function is constant and a FUNCALL      */
;*    node otherwise.                                                  */
;*---------------------------------------------------------------------*/
(define (make-fx-app-node loc var args)
   (let ((v (var-variable var)))
      (cond
	 ((and (cfun? (variable-value v)) (special-cfun? v))
	  ;; this is a special C function call, such as a vector-ref,
	  ;; a vector-set! or a vector creation
	  (make-special-app-node loc var args))
	 ((fun? (variable-value v))
	  ;; this is a regular direct call
	  (let ((call (instantiate::app
			 (loc loc)
			 (type (strict-node-type *_* (variable-type v)))
			 (fun  (if (closure? var) (duplicate::var var) var))
			 (args args))))
	     (if (eq? (variable-type v) *void*)
		 ;; the call is cast into obj
		 (let ((unspec (instantiate::literal
				  (type (strict-node-type (get-type-atom #unspecified) *obj*))
				  (value #unspecified))))
		    (instantiate::sequence
		       (type (strict-node-type *_* *obj*))
		       (nodes (list call unspec))))
		 call)))
	 (else
	  ;; this is a computed function call (a call to an unkknown function)
	  (instantiate::funcall
	     (loc loc)
	     (type *_*)
	     (fun  var)
	     (args (cons (duplicate::var var) args)))))))

;*---------------------------------------------------------------------*/
;*    make-va-app-node ...                                             */
;*---------------------------------------------------------------------*/
(define (make-va-app-node arity stack loc var args)
   (define (make-args-list args)
      (if (null? args)
	  ''()
	  `((@ $cons foreign) ,(car args) ,(make-args-list (cdr args)))))
   (let loop ((old-args args)
	      (arity    arity)
	      (f-args   '()))
      (if (=fx arity -1)
	  (let* ((l-arg (mark-symbol-non-user! (gensym 'list)))
		 (l-exp  `(,(let-sym) ((,l-arg ,(make-args-list old-args)))
				      ,l-arg))
		 (l-node (sexp->node l-exp stack loc 'value))
		 (l-var (let-var-body l-node))
		 (app (make-fx-app-node
		       loc var (reverse! (cons l-var f-args)))))
	     (with-access::let-var l-node (body type)
		(set! body app)
		(set! type (strict-node-type *_* type)))
	     (clean-user-node! l-node))
	  (loop (cdr old-args)
		(+fx arity 1)
		(cons (sexp->node (car old-args) stack loc 'value) f-args)))))

;*---------------------------------------------------------------------*/
;*    special-cfun? ...                                                */
;*---------------------------------------------------------------------*/
(define (special-cfun? global)
   (memq (global-id global)
	 '(c-eq?
	   $vector?
	   $vector-length
	   $vector-ref $vector-ref-ur
	   $vector-set! $vector-set-ur!
	   $create-vector)))

;*---------------------------------------------------------------------*/
;*    make-special-app-node ...                                        */
;*    -------------------------------------------------------------    */
;*    We don't have to take care of arity checks because they have     */
;*    already been done.                                               */
;*---------------------------------------------------------------------*/
(define (make-special-app-node loc var::var args)
   (let* ((variable (var-variable var))
	  (gname (global-name variable)))
      (case (global-id variable)
	 ((c-eq?)
	  ;; As for $VECTOR-SET in order to let the cfa specialize the
	  ;; vector it is required to erase the type of the third argument
	  (when (and (var? (car args)) (not (closure? (car args))))
	     (node-type-set! (car args) *_*))
	  (when (and (var? (cadr args)) (not (closure? (cadr args))))
	     (node-type-set! (cadr args) *_*))
	  (instantiate::app
	     (loc loc)
	     (type (strict-node-type *_* (variable-type variable)))
	     (fun  var)
	     (args args)))
	 (($vector?)
	  ;; As for $VECTOR-SET in order to let the cfa specialize the
	  ;; vector it is required to erase the type of the argument
	  (node-type-set! (car args) *_*)
	  (instantiate::app
	     (loc loc)
	     (type (strict-node-type *_* (variable-type variable)))
	     (fun  var)
	     (args args)))
	 (($vector-length)
	  (let* ((vtype (car (cfun-args-type (global-value variable))))
		 (ftype (if (tvec? vtype)
			    (tvec-item-type vtype)
			    *_*)))
	     (instantiate::vlength
		(loc loc)
		(type (global-type variable))
		(c-format (string-append gname "($1)"))
		(expr* args)
		(ftype ftype)
		(vtype vtype))))
	 (($vector-ref $vector-ref-ur)
	  (instantiate::vref
	     (loc loc)
	     (type (strict-node-type *_* (global-type variable)))
	     (c-format (string-append gname "($1,$2)"))
	     (expr* args)
	     (unsafe (string-suffix? "-ur" (symbol->string! (global-id variable))))
	     (vtype (car (cfun-args-type (global-value variable))))
	     (ftype (global-type variable))
	     (otype (cadr (cfun-args-type (global-value variable))))))
	 (($vector-set! $vector-set-ur!)
	  ;; in order to let the cfa specialize the vector it is
	  ;; required to erase the type of the third argument
	  (node-type-set! (caddr args) *_*)
	  (instantiate::vset!
	     (loc loc)
	     (type (strict-node-type *unspec* (global-type variable)))
	     (c-format (string-append gname "($1,$2,$3)"))
	     (expr* args)
	     (unsafe (eq? (global-id variable) '$vector-set-ur!))
	     (vtype (car (cfun-args-type (global-value variable))))
	     (otype (cadr (cfun-args-type (global-value variable))))
	     (ftype (caddr (cfun-args-type (global-value variable))))))
	 (($create-vector)
	  (let* ((stack-alloc (fun-stack-allocator (variable-value variable)))
		 (heap-format (string-append gname "($1)"))
		 (stack-format (if (global? stack-alloc)
				   (string-append (global-name stack-alloc)
						  "($1)")
				   heap-format)))
	     (instantiate::valloc
		(loc loc)
		(type (strict-node-type *_* (global-type variable)))
		(ftype (strict-node-type *_* *obj*))
		(c-format heap-format)
		(otype (car (cfun-args-type (global-value variable))))
		(expr* args))))
	 (else
	  (error "make-special-app-node" "Illegal application" (shape var))))))
