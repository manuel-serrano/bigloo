;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/alphatize.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  6 11:09:14 1995                          */
;*    Last change :  Wed Jun  7 18:06:31 2017 (serrano)                */
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
	    ast_app
	    ast_dump)
   (static  (wide-class retblock/alpha::retblock
	       alpha::retblock))
   (export  (alphatize::node what* by* loc ::node)
	    (alphatize-sans-closure::node what* by* loc ::node ::variable)))

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
   (let ((res (do-alphatize node loc)))
      ;; we remove alpha-fast slots
      (for-each (lambda (what)
		   (variable-fast-alpha-set! what #unspecified))
		what*)
      res))

;*---------------------------------------------------------------------*/
;*    *no-alphatize-closure* ...                                       */
;*---------------------------------------------------------------------*/
(define *no-alphatize-closure* #f)

;*---------------------------------------------------------------------*/
;*    alphatize-sans-closure ...                                       */
;*---------------------------------------------------------------------*/
(define (alphatize-sans-closure what* by* loc node closure)
   (set! *no-alphatize-closure* closure)
   (unwind-protect
      (alphatize what* by* loc node)
      (set! *no-alphatize-closure* #f)))

;*---------------------------------------------------------------------*/
;*    get-location ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function selects which location to use, four different      */
;*    cases:                                                           */
;*                                                                     */
;*      1- if there is no current loc, use the one of the node         */
;*      2- if the node contains no location, use the current one       */
;*      3- if the location of the node refers to another file,         */
;*         use the default one.                                        */
;*      4- otherwise, use the location of the node.                    */
;*---------------------------------------------------------------------*/
(define (get-location node loc)
   (cond
      ((not (location? (node-loc node)))
       loc)
      ((not (location? loc))
       (node-loc node))
      ((not (string=? (location-fname (node-loc node)) (location-fname loc)))
       loc)
      (else
       (node-loc node))))

;*---------------------------------------------------------------------*/
;*    do-alphatize* ...                                                */
;*---------------------------------------------------------------------*/
(define (do-alphatize* nodes::pair-nil loc)
   (map (lambda (node) (do-alphatize node loc)) nodes))

;*---------------------------------------------------------------------*/
;*    do-alphatize ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (do-alphatize::node node::node loc))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::literal ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::literal loc)
   (duplicate::literal node
      (loc (get-location node loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::patch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::patch loc)
   (with-access::patch node (ref value)
      (duplicate::patch node
	 (ref (do-alphatize ref loc))
	 (value (do-alphatize value loc))
	 (loc (get-location node loc)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::var ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::var loc)
   (let* ((var   (var-variable node))
	  (alpha (variable-fast-alpha var)))
      (cond
	 ((eq? alpha #unspecified)
	  (use-variable! var (node-loc node) 'value)
	  (duplicate::var node (loc (get-location node loc))))
	 ((variable? alpha)
	  (use-variable! alpha (node-loc node) 'value)
	  (if (fun? (variable-value alpha))
	      (instantiate::closure
		 (loc (get-location node loc))
		 (type (strict-node-type *procedure* (variable-type alpha)))
		 (variable alpha))
	      (duplicate::var node
		 (loc (get-location node loc))
		 (variable alpha))))
	 ((literal? alpha)
	  (duplicate::literal alpha))
	 ((patch? alpha)
	  (with-access::patch alpha (ref)
	     (duplicate::patch alpha
		(ref (do-alphatize ref loc)))))
	 ((kwote? alpha)
	  (duplicate::kwote alpha))
	 (else
	  (internal-error "alphatize"
			  "Illegal alphatization (var)"
			  (cons (shape node) (shape alpha)))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::closure ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::closure loc)
   (let ((var (var-variable node)))
      (if (eq? var *no-alphatize-closure*)
	  node
	  (let ((alpha (variable-fast-alpha var)))
	     (cond
		((eq? alpha #unspecified)
		 (use-variable! var (node-loc node) 'value)
		 (duplicate::closure node (loc (get-location node loc))))
		((variable? alpha)
		 (use-variable! alpha (node-loc node) 'value)
		 (duplicate::closure node
		    (loc (get-location node loc))
		    (variable alpha)))
		(else
		 (internal-error "alphatize"
		    "Illegal alphatization (closure)"
		    (shape node))))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::kwote ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::kwote loc)
   (duplicate::kwote node (loc (get-location node loc))))
       
;*---------------------------------------------------------------------*/
;*    do-alphatize ::sequence ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::sequence loc)
   (duplicate::sequence node
      (loc (get-location node loc))
      (nodes (do-alphatize* (sequence-nodes node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::sync ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::sync loc)
   (duplicate::sync node
      (loc (get-location node loc))
      (mutex (do-alphatize (sync-mutex node) loc))
      (prelock (do-alphatize (sync-prelock node) loc))
      (body (do-alphatize (sync-body node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::app ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::app loc)
   ;; we have to enforce here a variable and not a closure (that
   ;; why the duplicate::var of the fun field).
   (duplicate::app node
      (loc (get-location node loc))
      (fun (let ((var (do-alphatize (app-fun node) loc)))
	      (if (closure? var)
		  (duplicate::var var)
		  var)))
      (args (do-alphatize* (app-args node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::app-ly ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::app-ly loc)
   (let ((fun (do-alphatize (app-ly-fun node) loc))
	 (arg (do-alphatize (app-ly-arg node) loc)))
      (if (and (closure? fun)
	       (not (global-optional? (var-variable fun)))
	       (not (global-key? (var-variable fun))))
	  (known-app-ly->node '()
			      (get-location node loc)
			      (duplicate::var fun)
			      arg
			      'value)
	  (duplicate::app-ly node
	     (loc (get-location node loc))
	     (fun fun)
	     (arg arg)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::funcall ...                                       */
;*    -------------------------------------------------------------    */
;*    When transforming a funcall into an app node we have to remove   */
;*    the extra argument which hold the closure.                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::funcall loc)
   (let ((fun  (do-alphatize (funcall-fun node) loc))
	 (args (do-alphatize* (funcall-args node) loc)))
      (if (closure? fun)
	  (sexp->node `(,(duplicate::var fun) ,@(cdr args))
	     '() (node-loc node) 'app)
	  (duplicate::funcall node
	     (loc (get-location node loc))
	     (fun fun)
	     (args args)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::pragma ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::pragma loc)
   (duplicate::pragma node
      (loc (get-location node loc))
      (expr* (do-alphatize* (pragma-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::cast-null ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::cast-null loc)
   (duplicate::cast-null node
      (loc (get-location node loc))
      (expr* (do-alphatize* (cast-null-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::getfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::getfield loc)
   (duplicate::getfield node
      (loc (get-location node loc))
      (expr* (do-alphatize* (getfield-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::setfield ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::setfield loc)
   (duplicate::setfield node
      (loc (get-location node loc))
      (expr* (do-alphatize* (setfield-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::new ...                                           */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::new loc)
   (duplicate::new node
      (loc (get-location node loc))
      (expr* (do-alphatize* (new-expr* node) loc)) ))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::vlength ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::vlength loc)
   (duplicate::vlength node
      (loc (get-location node loc))
      (expr* (do-alphatize* (vlength-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::vref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::vref loc)
   (duplicate::vref node
      (loc (get-location node loc))
      (expr* (do-alphatize* (vref-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::vset! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::vset! loc)
   (duplicate::vset! node
      (loc (get-location node loc))
      (expr* (do-alphatize* (vset!-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::valloc ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::valloc loc)
   (duplicate::valloc node
      (loc (get-location node loc))
      (expr* (do-alphatize* (valloc-expr* node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::instanceof ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::instanceof loc)
   (duplicate::instanceof node
      (expr* (list (do-alphatize (car (instanceof-expr* node)) loc)))
      (loc (get-location node loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::cast ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::cast loc)
   (duplicate::cast node
      (loc (get-location node loc))
      (arg (do-alphatize (cast-arg node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::setq ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::setq loc)
   (let* ((v     (setq-var node))
	  (var   (var-variable v))
	  (alpha (variable-fast-alpha var)))
      (cond
	 ((eq? alpha #unspecified)
	  (use-variable! var (node-loc node) 'set!)
	  (duplicate::setq node
	     (loc (get-location node loc))
	     (var (duplicate::var v (loc (get-location node loc))))
	     (value (do-alphatize (setq-value node) loc))))
	 ((variable? alpha)
	  (use-variable! alpha (node-loc node) 'set!)
	  (duplicate::setq node
	     (loc (get-location node loc))
	     (var (duplicate::var v
		     (loc (get-location node loc))
		     (variable alpha)))
	     (value (do-alphatize (setq-value node) loc))))
	 (else
	  (internal-error "alphatize"
			  "Illegal alphatization (setq)"
			  (shape node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::conditional ...                                   */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::conditional loc)
   (duplicate::conditional node
      (loc (get-location node loc))
      (test (do-alphatize (conditional-test node) loc))
      (true (do-alphatize (conditional-true node) loc))
      (false (do-alphatize (conditional-false node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::fail ...                                          */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::fail loc)
   (duplicate::fail node
      (loc (get-location node loc))
      (proc (do-alphatize (fail-proc node) loc))
      (msg  (do-alphatize (fail-msg node) loc))
      (obj  (do-alphatize (fail-obj node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::switch ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::switch loc)
   (duplicate::switch node
      (loc (get-location node loc))
      (test (do-alphatize (switch-test node) loc))
      (clauses (map (lambda (clause)
		       (cons (car clause)
			     (do-alphatize (cdr clause) loc)))
		    (switch-clauses node)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::make-box ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::make-box loc)
   (duplicate::make-box node
      (loc (get-location node loc))
      (value (do-alphatize (make-box-value node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::box-ref ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::box-ref loc)
   (duplicate::box-ref node
      (loc (get-location node loc))
      (var (do-alphatize (box-ref-var node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::box-set! ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::box-set! loc)
   (duplicate::box-set! node
      (loc (get-location node loc))
      (var (do-alphatize (box-set!-var node) loc))
      (value (do-alphatize (box-set!-value node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::let-fun ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::let-fun loc)
   (let* ((old-locals (let-fun-locals node))
	  (new-locals (map (lambda (l)
			      (let ((v (make-local-sfun (local-id l)
					  (local-type l)
					  (local-value l))))
				 (variable-occurrence-set! v
				    (variable-occurrence l))
				 v))
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
				       (get-location node loc)
				       old-body))
			  (new-sfun (duplicate::sfun old-sfun
				       (args new-args)
				       (body new-body))))
		      (local-user?-set! new (local-user? old))
		      (local-value-set! new new-sfun)))
	 old-locals
	 new-locals)
      (duplicate::let-fun node
	 (loc (get-location node loc))
	 (locals new-locals)
	 (body (alphatize old-locals
		  new-locals
		  (get-location node loc)
		  (let-fun-body node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::let-var ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::let-var loc)
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
				(cons new-local (do-alphatize (cdr binding) loc)))
			     (let-var-bindings node)
			     new-locals)))
      (duplicate::let-var node
	 (loc (get-location node loc))
	 (bindings new-bindings)
	 (body (alphatize old-locals
			  new-locals
			  (get-location node loc)
			  (let-var-body node))))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::set-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::set-ex-it loc)
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
	 (loc (get-location node loc))
	 (var (duplicate::var (set-ex-it-var node)
		 (loc (get-location node loc))
		 (variable new-var)))
	 (body (alphatize (list old-var)
			  (list new-var)
			  (get-location node loc)
			  old-body)))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::jump-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::jump-ex-it loc)
   (duplicate::jump-ex-it node
      (loc (get-location node loc))
      (exit (do-alphatize (jump-ex-it-exit node) loc))
      (value (do-alphatize (jump-ex-it-value node) loc))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::retblock ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::retblock loc)
   (with-access::retblock node (body)
      (let ((res (duplicate::retblock node)))
	 (widen!::retblock/alpha node
	    (alpha res))
	 (let ((nbody (do-alphatize body loc)))
	    (shrink! node)
	    (with-access::retblock res (body)
	       (set! body nbody))
	    res))))

;*---------------------------------------------------------------------*/
;*    do-alphatize ::return ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-alphatize node::return loc)
   (with-access::return node (value block loc)
      (if (isa? block retblock/alpha)
	  (with-access::retblock/alpha block (alpha)
	     (duplicate::return node
		(block alpha)
		(value (do-alphatize value loc))))
	  (duplicate::return node
	     (block block)
	     (value (do-alphatize value loc))))))
