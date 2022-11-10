;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Flop/walk.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Wed Nov  9 11:53:27 2022 (serrano)                */
;*    Copyright   :  2010-22 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Optimize flonum operations by propagation ::real type in         */
;*    expressions. This optimization replaces generic operations with  */
;*    flonum operations and pushes the new types to the surrounding    */
;*    expression. Eg.                                                  */
;*    (2+ a (2+ 1.0 c)) => (+fl (->flonum a) (+fl 1.0 (->flonum c)))   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module flop_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    effect_effect
	    engine_param
	    backend_backend)
   (export  (flop-walk! globals)
	    (init-flop-cache!)))

;*---------------------------------------------------------------------*/
;*    flop-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (flop-walk! globals)
   (pass-prelude "Flop" init-flop-cache!) 
   (when *optim-specialize-flonum?*
      (for-each flop-fun! globals))
   (pass-postlude globals clear-flop-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *flops* '())
(define *flonum?* #f)
(define *$flonum?* #f)
(define *fixnum?* #f)
(define *c-fixnum?* #f)
(define *toflonum* #f)

;*---------------------------------------------------------------------*/
;*    init-flop-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (init-flop-cache!)
   (unless (pair? *flops*)
      (set! *$flonum?* (find-global '$flonum? 'foreign))
      (set! *flonum?* (find-global 'flonum? '__r4_numbers_6_5_flonum))
      (set! *c-fixnum?* (find-global 'c-fixnum? 'foreign))
      (set! *fixnum?* (find-global 'fixnum? '__r4_numbers_6_5_fixnum))
      (set! *toflonum* (find-global 'number->flonum '__r4_numbers_6_5))
      (set! *flops*
	 (map (lambda (op)
		 (cons (find-global (symbol-append '|2| op) '__r4_numbers_6_5)
		    (find-global (symbol-append op 'fl) '__r4_numbers_6_5_flonum)))
	    '(+ - / *))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-flop-cache! ...                                            */
;*---------------------------------------------------------------------*/
(define (clear-flop-cache!)
   (set! *flonum?* #f)
   (set! *$flonum?* #f)
   (set! *fixnum?* #f)
   (set! *c-fixnum?* #f)
   (set! *flops* '()))

;*---------------------------------------------------------------------*/
;*    flop-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (flop-fun! var)
   (enter-function (variable-id var))
   (let ((fun (variable-value var)))
      (sfun-body-set! fun (flop! (sfun-body fun)))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    flop! ...                                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (flop! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    flop! ::let-var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (flop! node::let-var)
   
   (define (find-flop expr)
      (when (isa? expr app)
	 (let ((v (var-variable (app-fun expr))))
	    (let ((c (assq v *flops*)))
	       (when c (cdr c))))))
   
   (define (flop-simple-letvar? bindings body)
      ;; true iff:
      ;;   - there are one binding
      ;;   - it is a flonum call
      ;;   - the body is a call to a generic binary arithmetic op
      (and (=fx (length bindings) 1)
	   (app? body)
	   (find-flop (car (app-args body)))
	   (flonum? (cdar bindings))))

   (define (reducible-cond node)
      (with-access::let-var node (body bindings type)
	 ;; check first if node is
	 ;; (let ((b::bool literal)) (if b true false))
	 (when (and (pair? bindings)
		    (null? (cdr bindings))
		    (eq? (variable-type (caar bindings)) *bool*)
		    (isa? (cdar bindings) literal)
		    (isa? body conditional))
	    (with-access::conditional body (test true false)
	       (when (isa? test ref)
		  (with-access::ref test (variable)
		     (when (eq? variable (caar bindings))
			;; yes, reduce
			(with-access::literal (cdar bindings) (value)
			   (if value
			       true
			       false)))))))))
   
   (with-access::let-var node (body bindings type)
      (for-each (lambda (binding)
		   (set-cdr! binding (flop! (cdr binding)))
		   (when (eq? (variable-access (car binding)) 'read)
		      (when (eq? (node-var-type (cdr binding)) *real*)
			 (variable-type-set! (car binding) *real*))))
	 bindings)
      (set! body (flop! body))
      (cond
	 ((eq? (node-var-type body) *real*)
	  (set! type (node-var-type body))
	  node)
	 ((or (is-boolean? body #t) (is-boolean? body #f))
	  (if (every (lambda (b) (not (side-effect? (cdr b)))) bindings)
	      body
	      node))
	 ((reducible-cond node)
	  =>
	  (lambda (reduction)
	     reduction))
	 (else
	  node))))

;*---------------------------------------------------------------------*/
;*    flop! ::app ...                                                  */
;*---------------------------------------------------------------------*/
(define-walk-method (flop! node::app)
   
   (define (patch! op node fun args)
      (node-type-set! node *real*)
      ;; patch the function
      (with-access::var fun (variable)
	 (set! variable (cdr op)))
      ;; patch the second argument
      (flop! (car args))
      (unless (eq? (node-var-type (car args)) *real*)
	 (set-car! args
	    (instantiate::app
	       (type *real*)
	       (fun (instantiate::ref
		       (type *real*)
		       (variable *toflonum*)))
	       (args (list (car args))))))
      node)

   (define (cond->bool args bool)
      (if (side-effect? (car args))
	  (instantiate::sequence
	     (type *bool*)
	     (nodes (list (car args)
		       (instantiate::literal
			  (type *bool*)
			  (value bool)))))
	  (instantiate::literal
	     (type *bool*)
	     (value bool))))
   
   (with-access::app node (fun args type)
      (when (not (eq? type (variable-type (var-variable fun))))
	 (set! type (variable-type (var-variable fun))))
      (let ((op (assq (var-variable fun) *flops*)))
	 (cond
	    ((isflonum? node)
	     (call-default-walker)
	     (cond
		((eq? (node-var-type (car args)) *real*)
		 (cond->bool args #t))
		((and (not (eq? (node-var-type (car args)) *_*))
		      (not (eq? (node-var-type (car args)) *obj*)))
		 (cond->bool args #f))
		(else
		 node)))
	    ((isfixnum? node)
	     (call-default-walker)
	     (cond
		((eq? (node-var-type (car args)) *real*) (cond->bool args #f))
		(else node)))
	    ((not op)
	     (call-default-walker))
	    ((eq? (node-var-type (car args)) *real*)
	     (patch! op node fun (cdr args)))
	    ((eq? (node-var-type (cadr args)) *real*)
	     (patch! op node fun args))
	    (else
	     (call-default-walker))))))

;*---------------------------------------------------------------------*/
;*    flop! ::conditional ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (flop! node::conditional)
   (call-default-walker)
   (with-access::conditional node (true false type)
      (cond
	 ((and (is-true? true) (is-true? false))
	  (instantiate::literal
	     (type *bool*)
	     (value #t)))
	 ((and (is-false? true) (is-false? false))
	  (instantiate::literal
	     (type *bool*)
	     (value #f)))
	 ((eq? (node-var-type true) (node-var-type false))
	  (set! type (node-var-type true))
	  node)
	 (else
	  node))))

;*---------------------------------------------------------------------*/
;*    is-boolean? ...                                                  */
;*---------------------------------------------------------------------*/
(define (is-boolean? node val)
   (when (isa? node literal)
      (with-access::literal node (value)
	 (eq? value val))))
   
(define (is-true? node)
   (is-boolean? node #t))
   
(define (is-false? node)
   (is-boolean? node #f))
   
;*---------------------------------------------------------------------*/
;*    isnumber? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is EXPR a number test?                                           */
;*---------------------------------------------------------------------*/
(define (isnumber? expr::node pred $pred)
   
   (define (number-app? expr::app pred $pred)
      (let ((v (var-variable (app-fun expr))))
	 (or (eq? v pred) (eq? v $pred))))
   
   (define (number-letvar? expr::let-var pred $pred)
      (with-access::let-var expr (bindings body)
	 (when (=fx (length bindings) 1)
	    (when (number-app? (cdar bindings) pred $pred)
	       (when (conditional? body)
		  (with-access::conditional body (test)
		     (when (var? test)
			(with-access::var test (variable)
			   (eq? variable (caar bindings))))))))))
   
   (cond
      ((app? expr) (number-app? expr pred $pred))
      ((let-var? expr) (number-letvar? expr pred $pred))
      (else #f)))

(define (isflonum? expr::node) (isnumber? expr *flonum?* *$flonum?*))
(define (isfixnum? expr::node) (isnumber? expr *fixnum?* *c-fixnum?*))

;*---------------------------------------------------------------------*/
;*    node-var-type ...                                                */
;*---------------------------------------------------------------------*/
(define-generic (node-var-type node::node)
   (node-type node))

;*---------------------------------------------------------------------*/
;*    node-var-type ::ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-var-type node::ref)
   (with-access::ref node (variable)
      (variable-type variable)))
