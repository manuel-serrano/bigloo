;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/alphatize.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  6 11:09:14 1995                          */
;*    Last change :  Tue Sep  7 09:05:36 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The substitution tools module                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_alphatize
   (include "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_error
	    type_cache
	    ast_sexp
	    ast_local
	    ast_apply
	    ast_app)
   (export  (alphatize::node what* by* loc ::node)))

;*---------------------------------------------------------------------*/
;*    alphatize ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function differs from SUBSTITUTE, because:                  */
;*      - it operates only on variables                                */
;*      - it allocates new nodes (i.e. it does not operate on place).  */
;*    -------------------------------------------------------------    */
;*    Alphatize can replace a variable by a variable                   */
;*    construction but nothing else.                                   */
;*---------------------------------------------------------------------*/
(define (alphatize what* by* loc node)
   ;; we set alpha-fnode slot and the type of the new variable
   (for-each (lambda (what by)
		(variable-fast-alpha-set! what by))
	     what*
	     by*)
   (set! *location* loc)
   (let ((res (do-alphatize node)))
      ;; we remove alpha-fast slots
      (for-each (lambda (what)
		   (variable-fast-alpha-set! what #unspecified))
		what*)
      res))

;*---------------------------------------------------------------------*/
;*    *location* ...                                                   */
;*---------------------------------------------------------------------*/
(define *location* #f)

;*---------------------------------------------------------------------*/
;*    get-inline-location ...                                          */
;*    -------------------------------------------------------------    */
;*    This function controls how location are propagate inside inline  */
;*    body. The default behaviour is to propagate the location of the  */
;*    caller. To change this, simply inverse the order of the tests.   */
;*---------------------------------------------------------------------*/
(define (get-inline-location node)
   (cond
      ((location? *location*)
       *location*)
      ((location? (node-loc node))
       (node-loc node))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    do-alphatize ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (do-alphatize::node node::node))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::atom ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::atom)
   (duplicate::atom node
      (loc (get-inline-location node))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::var)
   (let* ((var   (var-variable node))
	  (alpha (variable-fast-alpha var)))
      (cond
	 ((eq? alpha #unspecified)
	  (use-variable! var (node-loc node) 'value)
	  (duplicate::var node (loc (get-inline-location node))))
	 ((variable? alpha)
	  (use-variable! alpha (node-loc node) 'value)
	  (if (fun? (variable-value alpha))
	      (instantiate::closure
		 (loc (get-inline-location node))
		 (type (variable-type alpha))
		 (variable alpha))
	      (duplicate::var node
		 (loc (get-inline-location node))
		 (variable alpha))))
	 ((atom? alpha)
	  (duplicate::atom alpha))
	 ((kwote? alpha)
	  (duplicate::kwote alpha))
	 (else
	  (internal-error "alphatize"
			  "Illegal alphatization (var)"
			  (cons (shape node) (shape alpha)))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::closure ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::closure)
   (let* ((var   (var-variable node))
	  (alpha (variable-fast-alpha var)))
      (cond
	 ((eq? alpha #unspecified)
	  (use-variable! var (node-loc node) 'value)
	  (duplicate::closure node (loc (get-inline-location node))))
	 ((variable? alpha)
	  (use-variable! alpha (node-loc node) 'value)
	  (duplicate::closure node
	     (loc (get-inline-location node))
	     (variable alpha)))
	 (else
	  (internal-error "alphatize"
			  "Illegal alphatization (closure)"
			  (shape node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::kwote)
   (duplicate::kwote node (loc (get-inline-location node))))
       
;*---------------------------------------------------------------------*/
;*    do-alphatize ::sequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::sequence)
   (duplicate::sequence node
      (loc (get-inline-location node))
      (nodes (map do-alphatize (sequence-nodes node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::app)
   ;; we have to enforce here a variable and not a closure (that
   ;; why the duplicate::var of the fun field).
   (duplicate::app node
      (loc (get-inline-location node))
      (fun (let ((var (do-alphatize (app-fun node))))
	      (if (closure? var)
		  (duplicate::var var)
		  var)))
      (args (map do-alphatize (app-args node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::app-ly)
   (let ((fun (do-alphatize (app-ly-fun node)))
	 (arg (do-alphatize (app-ly-arg node))))
      (if (and (closure? fun)
	       (not (global-optional? (var-variable fun)))
	       (not (global-key? (var-variable fun))))
	  (known-app-ly->node '()
			      (get-inline-location node)
			      (duplicate::var fun)
			      arg
			      'value)
	  (duplicate::app-ly node
	     (loc (get-inline-location node))
	     (fun fun)
	     (arg arg)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::funcall ...                                       */
;*    -------------------------------------------------------------    */
;*    When transforming a funcall into an app node we have to remove   */
;*    the extra argument which hold the closure.                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::funcall)
   (let ((fun  (do-alphatize (funcall-fun node)))
	 (args (map do-alphatize (funcall-args node))))
      (if (closure? fun)
	  (sexp->node `(,(duplicate::var fun) ,@(cdr args))
		      '() (node-loc node) 'app)
	  (duplicate::funcall node
	     (loc (get-inline-location node))
	     (fun fun)
	     (args args)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::pragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::pragma)
   (duplicate::pragma node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (pragma-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::getfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::getfield)
   (duplicate::getfield node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (getfield-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::setfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::setfield)
   (duplicate::setfield node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (setfield-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::new ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::new)
   (duplicate::new node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (new-expr* node))) ))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::vlength ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::vlength)
   (duplicate::vlength node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (vlength-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::vref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::vref)
   (duplicate::vref node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (vref-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::vset! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::vset!)
   (duplicate::vset! node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (vset!-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::valloc ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::valloc)
   (duplicate::valloc node
      (loc (get-inline-location node))
      (expr* (map do-alphatize (valloc-expr* node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::isa ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::isa)
   (duplicate::isa node
      (expr* (list (do-alphatize (car (isa-expr* node)))))
      (loc (get-inline-location node))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::cast)
   (duplicate::cast node
      (loc (get-inline-location node))
      (arg (do-alphatize (cast-arg node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::setq)
   (let* ((v     (setq-var node))
	  (var   (var-variable v))
	  (alpha (variable-fast-alpha var)))
      (cond
	 ((eq? alpha #unspecified)
	  (use-variable! var (node-loc node) 'set!)
	  (duplicate::setq node
	     (loc (get-inline-location node))
	     (var (duplicate::var v (loc (get-inline-location node))))
	     (value (do-alphatize (setq-value node)))))
	 ((variable? alpha)
	  (use-variable! alpha (node-loc node) 'set!)
	  (duplicate::setq node
	     (loc (get-inline-location node))
	     (var (duplicate::var v
		     (loc (get-inline-location node))
		     (variable alpha)))
	     (value (do-alphatize (setq-value node)))))
	 (else
	  (internal-error "alphatize"
			  "Illegal alphatization (setq)"
			  (shape node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::conditional)
   (duplicate::conditional node
      (loc (get-inline-location node))
      (test (do-alphatize (conditional-test node)))
      (true (do-alphatize (conditional-true node)))
      (false (do-alphatize (conditional-false node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::fail)
   (duplicate::fail node
      (loc (get-inline-location node))
      (proc (do-alphatize (fail-proc node)))
      (msg  (do-alphatize (fail-msg node)))
      (obj  (do-alphatize (fail-obj node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::select ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::select)
   (duplicate::select node
      (loc (get-inline-location node))
      (test (do-alphatize (select-test node)))
      (clauses (map (lambda (clause)
		       (cons (car clause)
			     (do-alphatize (cdr clause))))
		    (select-clauses node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::make-box)
   (duplicate::make-box node
      (loc (get-inline-location node))
      (value (do-alphatize (make-box-value node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::box-ref)
   (duplicate::box-ref node
      (loc (get-inline-location node))
      (var (do-alphatize (box-ref-var node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::box-set!)
   (duplicate::box-set! node
      (loc (get-inline-location node))
      (var (do-alphatize (box-set!-var node)))
      (value (do-alphatize (box-set!-value node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::let-fun)
   (let* ((old-locals (let-fun-locals node))
	  (new-locals (map (lambda (l)
			      (make-local-sfun (local-id l)
					       (local-type l)
					       (local-value l)))
			   old-locals)))
      (for-each (lambda (old new)
		   (let* ((old-sfun (local-value old))
			  (old-args (sfun-args old-sfun))
			  (new-args (map (lambda (l)
					    (make-local-svar (local-id l)
							     (local-type l)))
					 old-args))
			  (old-body (sfun-body old-sfun))
			  (new-body (alphatize (append old-locals old-args)
					       (append new-locals new-args)
					       (get-inline-location node)
					       old-body))
			  (new-sfun (duplicate::sfun old-sfun
				       (args new-args)
				       (body new-body))))
		      (local-user?-set! new (local-user? old))
		      (local-value-set! new new-sfun)))
		old-locals
		new-locals)
      (duplicate::let-fun node
	 (loc (get-inline-location node))
	 (locals new-locals)
	 (body (alphatize old-locals
			  new-locals
			  (get-inline-location node)
			  (let-fun-body node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::let-var)
   (let* ((old-locals   (map car (let-var-bindings node)))
	  (new-locals   (map (lambda (l)
				;; we can't use duplicate for locals because
				;; all local variables must be allocated
				;; using the `make-local-svar' form
				;; (for the key attribution).
				(let ((var (make-local-svar (local-id l)
							    (local-type l))))
				   (local-user?-set! var (local-user? l))
				   (local-access-set! var (local-access l))
				   var))
			     old-locals))
	  (new-bindings (map (lambda (binding new-local)
				(cons new-local (do-alphatize (cdr binding))))
			     (let-var-bindings node)
			     new-locals)))
      (duplicate::let-var node
	 (loc (get-inline-location node))
	 (bindings new-bindings)
	 (body (alphatize old-locals
			  new-locals
			  (get-inline-location node)
			  (let-var-body node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::set-ex-it)
   (let* ((old-var    (var-variable (set-ex-it-var node)))
	  (old-exit   (local-value old-var))
	  (old-hdlg   (sexit-handler old-exit))
	  (alpha-hdlg (variable-fast-alpha old-hdlg))
	  (new-var    (make-local-sexit (local-id old-var)
					(local-type old-var)
					(duplicate::sexit old-exit
					   (handler alpha-hdlg))))
								 
	  (old-body   (set-ex-it-body node)))
      (local-user?-set! new-var (local-user? old-var))
      (duplicate::set-ex-it node
	 (loc (get-inline-location node))
	 (var (duplicate::var (set-ex-it-var node)
		 (loc (get-inline-location node))
		 (variable new-var)))
	 (body (alphatize (list old-var)
			  (list new-var)
			  (get-inline-location node)
			  old-body)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::jump-ex-it)
   (duplicate::jump-ex-it node
      (loc (get-inline-location node))
      (exit (do-alphatize (jump-ex-it-exit node)))
      (value (do-alphatize (jump-ex-it-value node)))))

