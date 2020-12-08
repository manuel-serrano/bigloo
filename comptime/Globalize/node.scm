;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/node.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 27 14:12:58 1995                          */
;*    Last change :  Wed May 31 10:38:06 2017 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We transforme the ast in order to fix the free variables, to     */
;*    remove the useless local functions (globalized or integrated     */
;*    ones) and to remove `fun' constructions.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_node
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    type_cache
	    type_typeof
	    ast_var
	    ast_node
	    globalize_ginfo
	    ast_sexp
	    ast_env
	    ast_local
	    globalize_free
	    globalize_local->global
	    tools_error)
   (export  (node-globalize!::node ::node ::variable <variablexvariable>*)))

;*---------------------------------------------------------------------*/
;*    node-globalize! ...                                              */
;*    -------------------------------------------------------------    */
;*    This function makes many transformation on the Node *and*        */
;*    returns a free variables list.                                   */
;*---------------------------------------------------------------------*/
(define (node-globalize! node integrator what/by*)
   (trace (globalize 5) "node-globalize!=" (shape node) #\Newline)
   ;; for each celled variable, we declare a new local
   ;; variable
   (let* ((fun (variable-value integrator))
	  (celled (celled-bindings (sfun-args fun)))
	  (what/by* (append celled what/by*)))
      ;; we set alpha-fast slot 
      (for-each (lambda (w-b)
		   (local-fast-alpha-set! (car w-b) (cdr w-b)))
		what/by*)
      (let ((res (cell-formals celled (glo! node integrator))))
	 ;; we remove alpha-fast slots
	 (for-each (lambda (w-b)
		      (local-fast-alpha-set! (car w-b) #unspecified))
		   what/by*)
	 res)))

;*---------------------------------------------------------------------*/
;*    celled-bindings ...                                              */
;*---------------------------------------------------------------------*/
(define (celled-bindings formals)
   (let loop ((celled   '())
	      (formals  formals))
      (cond
	 ((null? formals)
	  celled)
	 ((not (celled? (car formals)))
	  (loop celled (cdr formals)))
	 (else
	  (let* ((ovar (car formals))
		 (nvar (cell-variable ovar)))
	     (loop (cons (cons ovar nvar) celled) (cdr formals)))))))

;*---------------------------------------------------------------------*/
;*    cell-variable ...                                                */
;*---------------------------------------------------------------------*/
(define (cell-variable::local local::local)
   (let ((var (make-local-svar (local-id local) *cell*)))
      (local-access-set! var 'cell-globalize)
      (local-user?-set! var (local-user? local))
      (widen!::svar/Ginfo (local-value var)
	 (celled? #t)
	 (kaptured? #t))
      var))
   
;*---------------------------------------------------------------------*/
;*    cell-formals ...                                                 */
;*---------------------------------------------------------------------*/
(define (cell-formals celled body)
   (if (null? celled)
       body
       (let ((loc (node-loc body)))
	  (instantiate::let-var
	     (loc loc)
	     (body body)
	     (type (node-type body))
	     (bindings (map (lambda (o-n)
			       (cons (cdr o-n)
				  (a-make-cell
				     (instantiate::var
					(type (variable-type (car o-n)))
					(loc loc)
					(variable (car o-n)))
				     (car o-n))))
			  celled))))))

;*---------------------------------------------------------------------*/
;*    a-make-cell ...                                                  */
;*---------------------------------------------------------------------*/
(define (a-make-cell::make-box node::node variable::variable)
   (with-access::node node (loc)
      (local-access-set! variable 'cell-globalize)
      (svar/Ginfo-celled?-set! (variable-value variable) #t)
      (instantiate::make-box
	 (type *cell*)
	 (vtype (get-bigloo-defined-type (variable-type variable)))
	 (loc loc)
	 (value node)
	 (stackable (svar/Ginfo-stackable (variable-value variable))))))
   
;*---------------------------------------------------------------------*/
;*    celled? ...                                                      */
;*---------------------------------------------------------------------*/
(define (celled?::bool variable::variable)
   (and (local? variable)
	(svar/Ginfo? (variable-value variable))
	(or (svar/Ginfo-celled? (variable-value variable))
	    (and (memq (variable-access variable) '(write cell-globalize))
		 (svar/Ginfo-kaptured? (variable-value variable))))))

;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-generic (glo!::node node::node integrator::variable))

;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-method (glo! node::atom integrator)
   node)
 
;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-method (glo! node::kwote integrator)
   node)

;*---------------------------------------------------------------------*/
;*    glo! ...                                                         */
;*---------------------------------------------------------------------*/
(define-method (glo! node::var integrator)
   (let* ((variable (var-variable node))
	  (vtype (variable-type variable)))
      (let loop ((variable variable))
	 (let ((alpha (variable-fast-alpha variable)))
	    (cond
	       ((local? alpha)
		(var-variable-set! node alpha)
		(node-type-set! node (get-bigloo-defined-type vtype))
		(loop alpha))
	       ((global? variable)
		node)
	       ((celled? variable)
		;; (local-access-set! variable 'cell-globalize)
		(let ((vtype (get-bigloo-defined-type vtype))
		      (ntype (get-bigloo-defined-type (node-type node))))
		   ;; (node-type-set! node (get-bigloo-defined-type vtype))
		   (node-type-set! node *cell*)
		   (instantiate::box-ref
		      (loc (node-loc node))
		      ;; (vtype (get-bigloo-defined-type vtype))
		      ;; (type (get-bigloo-defined-type (node-type node)))
		      (vtype vtype)
		      (type ntype)
		      (var node))))
	       (else
		node))))))

;*---------------------------------------------------------------------*/
;*    glo! ::closure ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::closure integrator)
   (with-access::closure node (loc variable)
      (glo! (instantiate::var
	       (loc loc)
	       (type *procedure*)
	       (variable (the-closure variable loc)))
	    integrator)))

;*---------------------------------------------------------------------*/
;*    glo! ::sequence ...                                              */
;*---------------------------------------------------------------------*/
(define-method (glo! node::sequence integrator)
   (with-access::sequence node (nodes)
      (glo*! nodes integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::sync ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::sync integrator)
   (with-access::sync node (body mutex prelock)
      (set! mutex (glo! mutex integrator))
      (set! prelock (glo! prelock integrator))
      (set! body (glo! body integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::app ...                                                   */
;*---------------------------------------------------------------------*/
(define-method (glo! node::app integrator)
   (with-access::app node (loc args type)
      (let* ((fun  (var-variable (app-fun node)))
	     (info (variable-value fun)))
	 ;; we change the called function if globalized. We have
	 ;; to take care that for a call to a local globalized
	 ;; function there are two cases. First, the function
	 ;; is escaping, then we still calls the local entry
	 ;; point (which is integrated in the global function).
	 ;; Second, the function is not escaping then we have
	 ;; to translate a recursive call to a call to the
	 ;; globalized function.
	 (if (and (local? fun)
		  (or (not (eq? fun integrator))
		      (not (local/Ginfo-escape? fun)))
		  (sfun/Ginfo-G? info))
	     (app-fun-set! node
			   (instantiate::var
			      (loc loc)
			      (type type)
			      (variable (the-global fun)))))
	 ;; we globalize the actuals before adding new one
	 ;; otherwise, we could produce illegal `cell-ref'
	 (let liip ((nodes args))
	    (if (null? nodes)
		'done
		(begin
		   (set-car! nodes (glo! (car nodes) integrator))
		   (liip (cdr nodes)))))
	 (cond
	    ((or (global? fun)
		 (not (sfun/Ginfo-G? info))
		 (and (eq? fun integrator)
		      (local/Ginfo-escape? fun)))
	     'done)
	    ((local/Ginfo-escape? fun)
	     ;; this is a direct call to an escaping call,
	     ;; we add its environement if it is a local function ...
	     (set! args (cons (glo! (instantiate::var
				       (loc loc)
				       (type (variable-type (the-closure fun loc)))
				       (variable (the-closure fun loc)))
				    integrator)
			      args)))
	    (else
	     ;; this is a call to globalized but non escaping
	     ;; function. We add its kaptured variables
	     (let loop ((new-actuals args)
			(kaptured    (sfun/Ginfo-kaptured info)))
		(if (null? kaptured)
		    (set! args new-actuals)
		    (let* ((kap   (car kaptured))
			   (alpha (local-fast-alpha kap))
			   (var   (if (local? alpha) alpha kap)))
		       (loop (cons (instantiate::var
				      (loc loc)
				      (type (variable-type var))
				      (variable var))
				   new-actuals)
			     (cdr kaptured)))))))
	 node)))
	  
;*---------------------------------------------------------------------*/
;*    glo! ::app-ly ...                                                */
;*---------------------------------------------------------------------*/
(define-method (glo! node::app-ly integrator)
   (with-access::app-ly node (fun arg)
      (set! fun (glo! fun integrator))
      (set! arg (glo! arg integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::funcall ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::funcall integrator)
   (with-access::funcall node (fun args)
      (set! fun (glo! fun integrator))
      (glo*! args integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::extern ...                                                */
;*---------------------------------------------------------------------*/
(define-method (glo! node::extern integrator)
   (with-access::extern node (expr*)
      (glo*! expr* integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::cast ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::cast integrator)
   (with-access::cast node (arg)
      (glo! arg integrator)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::setq ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::setq integrator)
   (with-access::setq node (value)
      (set! value (glo! value integrator))
      (let* ((var (var-variable (setq-var node)))
	     (vtype (variable-type var)))
	 (let loop ((var   var)
		    (alpha (variable-fast-alpha var)))
	    (if (local? alpha)
		(begin
		   (var-variable-set! (setq-var node) alpha)
		   (loop alpha (variable-fast-alpha alpha)))
		(let ((var (var-variable (setq-var node))))
		   (if (and (local? var) (celled? var))
		       (let ((a-var (make-local-svar 'aux *obj*))
			     (loc   (node-loc node)))
			  (local-access-set! var 'cell-globalize)
			  (local-user?-set! a-var (local-user? var))
			  (node-type-set! (setq-var node) *obj*)
			  (widen!::svar/Ginfo (local-value a-var)
			     (kaptured? #f))
			  (instantiate::let-var
			     (loc loc)
			     (type *unspec*)
			     (bindings (list (cons a-var value)))
			     (body (instantiate::box-set!
				      (loc loc)
				      (type *unspec*)
				      (vtype (get-bigloo-defined-type vtype))
				      (var (setq-var node))
				      (value (instantiate::var
						(loc loc)
						(type (variable-type a-var))
						(variable a-var)))))))
		       node)))))))

;*---------------------------------------------------------------------*/
;*    glo! ::conditional ...                                           */
;*---------------------------------------------------------------------*/
(define-method (glo! node::conditional integrator)
   (with-access::conditional node (test true false)
      (set! test (glo! test integrator))
      (set! true (glo! true integrator))
      (set! false (glo! false integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::fail ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (glo! node::fail integrator)
   (with-access::fail node (proc msg obj)
      (set! proc (glo! proc integrator))
      (set! msg (glo! msg integrator))
      (set! obj (glo! obj integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::switch ...                                                */
;*---------------------------------------------------------------------*/
(define-method (glo! node::switch integrator)
   (with-access::switch node (clauses test)
      (set! test (glo! test integrator))
      (for-each (lambda (clause)
		   (set-cdr! clause (glo! (cdr clause) integrator)))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::let-fun ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::let-fun integrator)
   (with-access::let-fun node (body locals)
      (set! body (glo! body integrator))
      (let liip ((obindings locals)
		 (nbindings '())
		 (ebindings '()))
	 (if (not (null? obindings))
	     (trace (globalize 4)
		    "glo!(let-fun): " (shape (car obindings))
		    ", integrator=" (shape integrator)
		    ", escape=" (local/Ginfo-escape? (car obindings))
		    ", eq=" (eq? integrator (car obindings))
		    #\Newline))
	 (cond
	    ((null? obindings)
	     (set! locals nbindings)
	     (if (null? ebindings) 
		 node
		 (make-escaping-bindings ebindings node integrator)))
	    ((and (not (eq? (car obindings) integrator))
		  (local/Ginfo-escape? (car obindings)))
	     (liip (cdr obindings)
		   nbindings
		   (cons (car obindings) ebindings)))
	    ((eq? (car obindings) integrator)
	     ;; MS: 3 oct 2006
	     (internal-error 'glo!
			     "Shoud not be here (because of the integration"
			     (shape (car obindings)))
	     (liip (cdr obindings)
		   nbindings
		   ebindings))
	    (else
	     (let ((local (car obindings)))
		(globalize-local-fun! local integrator)
		(liip (cdr obindings)
		      (cons local nbindings)
		      ebindings)))))))

;*---------------------------------------------------------------------*/
;*    glo! ::let-var ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::let-var integrator)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (set-cdr! binding (glo! val integrator))
		      (when (celled? var)
			 (let ((nvar (cell-variable var)))
			    (local-fast-alpha-set! var nvar)
			    (set-cdr! binding
			       (a-make-cell (cdr binding) var))))))
	 bindings)
      (set! body (glo! body integrator))
      (for-each (lambda (binding)
		   (let ((var (car binding)))
		      (when (celled? var)
			 (let ((nvar (cell-variable var)))
			    (set-car! binding (variable-fast-alpha var))
			    (local-fast-alpha-set! var #unspecified)))))
	 bindings)
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::set-ex-it ...                                             */
;*---------------------------------------------------------------------*/
(define-method (glo! node::set-ex-it integrator)
   (with-access::set-ex-it node (var body)
      (let ((hdlg (sexit-handler (local-value (var-variable var)))))
	 (trace (globalize 2)
		"# glo!   :" (shape node) #\Newline
		"# handler: " (shape hdlg)
		#\Newline)
	 (when (sfun/Ginfo-G? (local-value hdlg))
	    (sexit-detached?-set! (local-value (var-variable var)) #t))
	 (set! body (glo! body integrator))
	 node)))

;*---------------------------------------------------------------------*/
;*    glo! ::jump-ex-it ...                                            */
;*---------------------------------------------------------------------*/
(define-method (glo! node::jump-ex-it integrator)
   (with-access::jump-ex-it node (exit value)
      (set! exit (glo! exit integrator))
      (set! value (glo! value integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::make-box ...                                              */
;*---------------------------------------------------------------------*/
(define-method (glo! node::make-box integrator)
   (with-access::make-box node (value)
      (set! value (glo! value integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::box-ref ...                                               */
;*---------------------------------------------------------------------*/
(define-method (glo! node::box-ref integrator)
   (with-access::box-ref node (var)
      (set! var (glo! var integrator))
      node))

;*---------------------------------------------------------------------*/
;*    glo! ::box-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-method (glo! node::box-set! integrator)
   (with-access::box-set! node (var value)
      (set! var (glo! var integrator))
      (set! value (glo! value integrator))
      node))
	    
;*---------------------------------------------------------------------*/
;*    glo*! ...                                                        */
;*---------------------------------------------------------------------*/
(define (glo*! node* integrator)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (glo! (car node*) integrator))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    make-escaping-bindings ...                                       */
;*---------------------------------------------------------------------*/
(define (make-escaping-bindings ebindings node::node integrator::variable)
   (trace (globalize 2)
	  "make-escaping-bindings: " (shape node) #\Newline
	  "             ebindings: " (shape ebindings) #\Newline)
   (let loop ((ebindings ebindings)
	      (bindings  '())
	      (sets      '()))
      (if (null? ebindings)
	  (instantiate::let-var
	     (loc (node-loc node))
	     (type (node-type node))
	     (bindings bindings)
	     (body (if (null? sets)
		       node
		       (instantiate::sequence
			  (loc (node-loc node))
			  (type (node-type node))
			  (nodes (append sets (list node)))))))
	  (let* ((local (car ebindings))
		 (new   (the-closure local (node-loc node)))
		 (nsets (make-sets new
				   (node-loc node)
				   (sfun/Ginfo-kaptured (local-value local))
				   integrator)))
	     (loop (cdr ebindings)
		   (cons (cons new (a-make-procedure local))
			 bindings)
		   (if (null? nsets)
		       sets
		       (cons nsets sets)))))))

;*---------------------------------------------------------------------*/
;*    globalize-local-fun! ...                                         */
;*---------------------------------------------------------------------*/
(define (globalize-local-fun! local::local integrator::variable)
   (trace (globalize 4) "globalize-local-fun: local=" (shape local)
	  " integrator=" (shape integrator) #\Newline)
   (let* ((fun   (local-value local))
	  (obody (sfun-body fun)))
      (if (eq? local integrator)
	  (sfun-body-set! fun (glo! obody integrator))
	  (let ((celled (celled-bindings (sfun-args fun))))
	     (for-each (lambda (w-b)
			  (local-fast-alpha-set! (car w-b) (cdr w-b)))
		       celled)
	     (let* ((nbody1 (glo! obody integrator))
		    (nbody2 (cell-formals celled nbody1)))
		(for-each (lambda (w-b)
			     (local-fast-alpha-set! (car w-b) #unspecified))
			  celled)
		(sfun-body-set! fun nbody2))))))

;*---------------------------------------------------------------------*/
;*    a-make-procedure ...                                             */
;*---------------------------------------------------------------------*/
(define (a-make-procedure local::local)
   (let* ((fun (local-value local))
	  (arity (sfun-arity fun))
	  (kaptured (sfun/Ginfo-kaptured (local-value local)))
	  (loc #unspecified)
	  (make-p (if (<fx arity 0) 'make-va-procedure 'make-fx-procedure))
	  (v (find-global/module make-p 'foreign)))
      (instantiate::app
	 (loc loc)
	 (type (variable-type v))
	 (stackable (when (eq? (sfun-stackable fun) #t)
		       (let ((gv (global-value v)))
			  (when (fun? gv)
			     (fun-stack-allocator gv)))))
	 (fun (instantiate::var
		 (loc loc)
		 (type (variable-type v))
		 (variable v)))
	 (args (list (instantiate::var
			(loc loc)
			(type (variable-type (the-global local)))
			(variable (the-global local)))
		  (instantiate::literal
		     (loc loc)
		     (type (get-type-atom arity))
		     (value arity))
		  (instantiate::literal
		     (loc loc)
		     (type (get-type-atom 1))
		     (value (length kaptured))))))))

;*---------------------------------------------------------------------*/
;*    make-sets ...                                                    */
;*---------------------------------------------------------------------*/
(define (make-sets new loc kaptured integrator)
   (if (null? kaptured)
       '()
       (let loop ((kaptured kaptured)
		  (indice   0)
		  (sets     '()))
	  (if (null? kaptured)
	      (instantiate::sequence
		 (loc loc)
		 (type (node-type (car sets)))
		 (nodes (reverse! sets)))
	      (loop (cdr kaptured)
		    (+fx indice 1)
		    (cons (a-procedure-set! loc
					    new
					    indice
					    (car kaptured)
					    integrator)
			  sets))))))

;*---------------------------------------------------------------------*/
;*    a-procedure-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (a-procedure-set! loc new indice kaptured integrator)
   (define (alpha-convert var)
      (let ((alpha (variable-fast-alpha var)))
	 (if (local? alpha)
	     (alpha-convert alpha)
	     var)))
   (let ((vf (find-global/module 'procedure-set! 'foreign))
	 (va (alpha-convert kaptured)))
      (instantiate::app
	 (loc loc)
	 (type (variable-type vf))
	 (fun (instantiate::var
		 (loc loc)
		 (type (variable-type vf))
		 (variable vf)))
	 (args (list (instantiate::var
			(loc loc)
			(type (variable-type new))
			(variable new))
		     (instantiate::literal
			(loc loc)
			(type (get-type-atom indice))
			(value indice))
		     (let ()
			(instantiate::var
			   (loc loc)
			   (type (variable-type va))
			   (variable va))))))))
						       
							     
			    
