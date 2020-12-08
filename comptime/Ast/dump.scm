;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/dump.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 31 07:26:21 1994                          */
;*    Last change :  Fri Jun  9 10:13:14 2017 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The ast->sexp translator                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_dump
   (include "Tvector/tvector.sch"
	    "Ast/node.sch")
   (import  tools_shape
	    tools_args
	    tools_misc
	    tools_location
	    engine_param
	    ast_ident
	    type_typeof
	    type_cache
	    effect_effect)
   (export  (generic node->sexp ::node)))

;*---------------------------------------------------------------------*/
;*    node->sexp-hook ...                                              */
;*---------------------------------------------------------------------*/
(define (node->sexp-hook node)
   node)

;*---------------------------------------------------------------------*/
;*    node->sexp ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node->sexp node::node))

;*---------------------------------------------------------------------*/
;*    node->sexp ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::atom)
   (node->sexp-hook node)
   (if (null? (atom-value node))
       (location-shape (node-loc node) ''())
       (location-shape (node-loc node)
		       (if *type-shape?*
			   `#(,(atom-value node)
			      ,(shape (get-type node #f))
			      ,(shape (node-type node)))
			   (atom-value node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::var)
   (node->sexp-hook node)
   (with-access::var node (type variable)
      (let* ((vshape (shape variable))
	     (tvshape (if (and *type-shape?*
			       (not (sfun? (variable-value variable)))
			       (not (eq? type (variable-type variable))))
			  (string->symbol (format "~a[::~a]" vshape (shape type)))
			  vshape))
	     (tvshape (if *access-shape?*
			  (string->symbol
			     (format "~a{~a}" tvshape (variable-access variable)))
			  tvshape)))
	 (location-shape (node-loc node) tvshape))))
		     
;*---------------------------------------------------------------------*/
;*    node->sexp ::closure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::closure)
   (node->sexp-hook node)
   (location-shape (node-loc node)
      `(,(shape-typed-node 'closure (node-type node))
	,(shape (closure-variable node)))))
 
;*---------------------------------------------------------------------*/
;*    node->sexp ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::kwote)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   (let ((value (kwote-value node)))
		      (if (a-tvector? value)
			  `(quote ,(a-tvector (shape (a-tvector-type value))
					      (a-tvector-vector value)))
			  `(quote ,value)))))
       
;*---------------------------------------------------------------------*/
;*    node->sexp ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::sequence)
   (with-access::sequence node (meta)
      (node->sexp-hook node)
      (let ((sym (shape-typed-node
		    (if (sequence-unsafe node) 'unsafe 'begin) (node-type node))))
	 (location-shape (node-loc node)
	    `(,sym ,@(if (pair? meta) (list meta) '())
		,@(map node->sexp (sequence-nodes node)))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::sync)
   (node->sexp-hook node)
   (let ((sym (shape-typed-node 'synchronize (node-type node))))
      (location-shape (node-loc node)
	 `(,sym ,(node->sexp (sync-mutex node))
	     :prelock ,(node->sexp (sync-prelock node))
	     ,(node->sexp (sync-body node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::app)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   (cond
		      (*type-shape?*
		       `(,(node->sexp (app-fun node))
			 ,@(if *access-shape?*
			       (list
				(list "side-effect:" (side-effect? node)))
			       '())
			 ,(list "type:" (shape (node-type node)))
			 ,@(map node->sexp (app-args node))))
		      (*access-shape?*
		       `(,(node->sexp (app-fun node))
			 ,(list "side-effect:" (side-effect? node))
			 ,@(map node->sexp (app-args node))))
		      (else
		       `(,(node->sexp (app-fun node))
			 ,@(map node->sexp (app-args node)))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::app-ly)
   (node->sexp-hook node)
   (let ((top (shape-typed-node 'apply (shape (node-type node)))))
      (location-shape (node-loc node)
	 `(,top ,(node->sexp (app-ly-fun node))
	     ,(node->sexp (app-ly-arg node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::funcall)
   (node->sexp-hook node)
   (let* ((op (case (funcall-strength node)
		 ((light) 'funcall-l)
		 ((elight) 'funcall-el)
		 (else 'funcall)))
	  (top (shape-typed-node op (get-type node #f))))
      (location-shape (node-loc node)
	 `(,top
	     ,(node->sexp (funcall-fun node))
	     ,@(map node->sexp (funcall-args node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::pragma ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::pragma)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   (let ((p (if (pragma-side-effect node)
				'pragma
				'free-pragma)))
		      `(,(shape-typed-node p (get-type node #f))
			,(pragma-format node)
			,@(map node->sexp (pragma-expr* node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::getfield ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::getfield)
   (node->sexp-hook node)
   (with-access::getfield node (fname ftype otype expr*)
      (location-shape (node-loc node)
	 `(,(shape-typed-node 'getfield (get-type node #f))
	   (,fname ,(type-id ftype))
	   ,(type-id otype) ,(node->sexp (car expr*))))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::setfield ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::setfield)
   (node->sexp-hook node)
   (with-access::setfield node (fname ftype otype expr*)
      (location-shape (node-loc node)
	 `(setfield (,fname ,(type-id ftype))
	     ,(type-id otype) ,@(map node->sexp expr*)))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::new ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::new)
   (node->sexp-hook node)
   (with-access::new node (expr* type)
      (location-shape (node-loc node)
	 `(,(shape-typed-node 'new type)
	   (,(type-id type)) ,@(map node->sexp expr*)))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::vlength ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::vlength)
   (node->sexp-hook node)
   (with-access::vlength node (type expr* ftype)
      (location-shape (node-loc node)
	 `(,(shape-typed-node 'vlength type)
	   ("ftype:" ,(shape ftype))
	   ,(node->sexp (car expr*))))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::vref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::vref)
   (node->sexp-hook node)
   (with-access::vref node (expr* ftype unsafe)
      (let ((id (if unsafe 'vref-ur 'vref)))
	 (if *type-shape?*
	     `(,(string->symbol
		 (string-append (symbol->string id)
				"::"
				(shape (get-type node #f))
				"[::" (shape ftype) "]"))
	       ,@(map node->sexp expr*))
	     `(,id ,@(map node->sexp expr*))))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::vset! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::vset!)
   (node->sexp-hook node)
   (with-access::vset! node (expr* ftype unsafe)
      (let ((id (shape-typed-node (if unsafe 'vset-ur! 'vset!) (get-type node #f))))
	 (if *type-shape?*
	     `(,id ,(vector (shape ftype)) ,@(map node->sexp expr*))
	     `(,id ,@(map node->sexp expr*))))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::valloc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::valloc)
   (node->sexp-hook node)
   (with-access::valloc node (expr* type ftype)
      `(,(string->symbol
	  (string-append "valloc::"
			 (shape (get-type node #f))
			 "[::" (shape ftype) "]"))
	,@(map node->sexp expr*))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::instanceof ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::instanceof)
   (node->sexp-hook node)
   (with-access::instanceof node (class expr*)
      `(isa? ,(node->sexp (car expr*)) ,(type-id class))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::cast-null ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::cast-null)
   (node->sexp-hook node)
   (with-access::cast-null node (type)
      `(cast-null:: ,(type-id type))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::cast)
   (node->sexp-hook node)
   (with-access::cast node (type arg)
      `(,(make-typed-ident 'cast (type-id type)) ,(node->sexp arg))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::setq)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(set! ,(node->sexp (setq-var node))
			  ,(node->sexp (setq-value node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::conditional) 
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(,(shape-typed-node 'if (node-type node))
		     ,(node->sexp (conditional-test node))
		     ,(node->sexp (conditional-true node))
		     ,(node->sexp (conditional-false node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::fail)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(failure ,(node->sexp (fail-proc node))
			     ,(node->sexp (fail-msg node))
			     ,(node->sexp (fail-obj node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::switch)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(,(shape-typed-node 'case (node-type node))
		     ,(node->sexp (switch-test node))
		     ,@(map (lambda (clause)
			       `(,(car clause) ,(node->sexp (cdr clause))))
			    (switch-clauses node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::let-fun)
   (node->sexp-hook node)
   (let ((sym (shape-typed-node 'labels (node-type node))))
      (location-shape (node-loc node)
	 `(,sym ,(map (lambda (fun)
			 (let ((f (local-value fun)))
			    `(,(shape fun)
			      ,@(with-access::sfun f (stackable)
				   (if (boolean? stackable)
				       `(:stackable ,stackable)
				       '()))
			      ,(args-list->args*
				  (map shape (sfun-args f))
				  (sfun-arity f))
			      ,(node->sexp (sfun-body f)))))
		    (let-fun-locals node))
	     ,(node->sexp (let-fun-body node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::let-var)
   (node->sexp-hook node)
   (let ((sym (shape-typed-node 'let (node-type node))))
      (location-shape (node-loc node)
	 `(,sym
	     ,@(if *access-shape?*
		   (list
		      (list "side-effect:" (side-effect? node)))
		   '())
	     ,(map (lambda (b)
			 `(,(shape (car b)) ,(node->sexp (cdr b))))
		    (let-var-bindings node))
	     ,(node->sexp (let-var-body node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::set-ex-it)
   (node->sexp-hook node)
   `(,(shape-typed-node 'set-exit (node-type node))
     ,(node->sexp (set-ex-it-var node))
     ,(node->sexp (set-ex-it-body node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::jump-ex-it)
   (node->sexp-hook node)
   `(,(shape-typed-node 'jump-exit (node-type node))
     ,(node->sexp (jump-ex-it-exit node))
     ,(node->sexp (jump-ex-it-value node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::retblock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::retblock)
   (node->sexp-hook node)
   `(,(shape-typed-node 'retblock (node-type node))
     ,(node->sexp (retblock-body node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::return)
   (node->sexp-hook node)
   `(,(shape-typed-node 'return (node-type node))
     ,(node->sexp (return-value node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::make-box)
   (node->sexp-hook node)
   `(,(shape-typed-node 'make-box (node-type node))
     ,(node->sexp (make-box-value node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::box-ref)
   (node->sexp-hook node)
   `(,(shape-typed-node 'box-ref (node-type node))
     ,(node->sexp (box-ref-var node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::box-set!)
   (node->sexp-hook node)
   `(box-set! ,(node->sexp (box-set!-var node))
	      ,(node->sexp (box-set!-value node))))
		  
;*---------------------------------------------------------------------*/
;*    shape-typed-node ...                                             */
;*---------------------------------------------------------------------*/
(define (shape-typed-node id type)
   (if *typenode-shape?*
       (string->symbol (string-append (symbol->string id) "::" (shape type)))
       id))

;*---------------------------------------------------------------------*/
;*    node->sexp ::patch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::patch)
   (with-access::patch node (ref value type)
      `(,(shape-typed-node 'patch type) ,(node->sexp ref) ,(node->sexp value))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::genpatchid ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::genpatchid)
   (with-access::genpatchid node (index)
      `(genpatchid ,index)))
