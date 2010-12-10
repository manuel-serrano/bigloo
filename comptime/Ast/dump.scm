;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/dump.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec 31 07:26:21 1994                          */
;*    Last change :  Sun Nov 28 06:05:58 2010 (serrano)                */
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
			   (vector (atom-value node)
				   (shape (get-type node))
				   (shape (node-type node)))
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
			  (string->symbol
			   (string-append (symbol->string vshape)
					  "[::" (shape type) "]"))
			  vshape)))
	 (location-shape (node-loc node) tvshape))))
		     
;*---------------------------------------------------------------------*/
;*    node->sexp ::closure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::closure)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(function ,(shape (closure-variable node)))))
 
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
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(begin
		       ,@(map node->sexp (sequence-nodes node)))))

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
				(vector 'side-effect: (side-effect? node)))
			       '())
			 ,(vector (shape (get-type node))
				  (shape (variable-type
					  (var-variable (app-fun node)))))
			 ,@(map node->sexp (app-args node))))
		      (*access-shape?*
		       `(,(node->sexp (app-fun node))
			 ,(vector 'side-effect: (side-effect? node))
			 ,@(map node->sexp (app-args node))))
		      (else
		       `(,(node->sexp (app-fun node))
			 ,@(map node->sexp (app-args node)))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::app-ly)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(apply ,(node->sexp (app-ly-fun node))
			   ,(node->sexp (app-ly-arg node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::funcall)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(,(case (funcall-strength node)
			 ((light)
			  'funcall-l)
			 ((elight)
			  'funcall-el)
			 (else
			  'funcall))
		     ,(node->sexp (funcall-fun node))
		     ,@(map node->sexp (funcall-args node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::pragma ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::pragma)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   (let ((p (if (pragma-side-effect? node)
				'pragma
				'free-pragma)))
		      `(,(if *type-shape?*
			     (make-typed-ident p (type-id (get-type node)))
			     p)
			,(pragma-format node)
			,@(map node->sexp (pragma-expr* node))))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::getfield ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::getfield)
   (node->sexp-hook node)
   (with-access::getfield node (fname ftype otype expr*)
      `(getfield (,fname ,(type-id ftype))
		 ,(type-id otype) ,(node->sexp (car expr*)))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::setfield ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::setfield)
   (node->sexp-hook node)
   (with-access::setfield node (fname ftype otype expr*)
      `(setfield (,fname ,(type-id ftype))
		 ,(type-id otype) ,@(map node->sexp expr*))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::new ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::new)
   (node->sexp-hook node)
   (with-access::new node (expr* type)
      `(new ,(type-id type) ,@(map node->sexp expr*))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::vlength ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::vlength)
   (node->sexp-hook node)
   (with-access::vlength node (expr*)
      `(vlength ,(node->sexp (car expr*)))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::vref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::vref)
   (node->sexp-hook node)
   (with-access::vref node (expr* ftype unsafe)
      (let ((id (if unsafe 'vref-ur 'vref)))
	 (if *type-shape?*
	     `(,id ,(vector (shape (get-type node)) (shape ftype))
		 ,@(map node->sexp expr*))
	     `(,id ,@(map node->sexp expr*))))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::vset! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::vset!)
   (node->sexp-hook node)
   (with-access::vset! node (expr* ftype unsafe)
      (let ((id (if unsafe 'vset-ur! 'vset!)))
	 (if *type-shape?*
	     `(,id ,(vector (shape (get-type node)) (shape ftype))
		     ,@(map node->sexp expr*))
	     `(,id ,@(map node->sexp expr*))))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::valloc ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::valloc)
   (node->sexp-hook node)
   (with-access::valloc node (expr*)
      `(vnew ,@(map node->sexp expr*))))
   
;*---------------------------------------------------------------------*/
;*    node->sexp ::isa ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::isa)
   (node->sexp-hook node)
   (with-access::isa node (class expr*)
      `(isa? ,(type-id class) ,(node->sexp (car expr*)))))
   
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
		   `(if ,(node->sexp (conditional-test node))
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
;*    node->sexp ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::select)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(case ,(node->sexp (select-test node))
		       ,@(map (lambda (clause)
				 `(,(car clause) ,(node->sexp (cdr clause))))
			      (select-clauses node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::let-fun)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(labels ,(map (lambda (fun)
				     `(,(shape fun)
				       ,(args-list->args*
					 (map shape
					      (sfun-args (local-value fun)))
					 (sfun-arity (local-value fun)))
				       ,(node->sexp
					 (sfun-body (local-value fun)))))
				  (let-fun-locals node))
		       ,(node->sexp (let-fun-body node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::let-var)
   (node->sexp-hook node)
   (location-shape (node-loc node)
		   `(let ,(map (lambda (b)
				  `(,(shape (car b)) ,(node->sexp (cdr b))))
			       (let-var-bindings node))
		       ,(node->sexp (let-var-body node)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::set-ex-it)
   (node->sexp-hook node)
   `(set-exit ,(node->sexp (set-ex-it-var node))
	      ,(node->sexp (set-ex-it-body node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::jump-ex-it)
   (node->sexp-hook node)
   `(jump-exit ,(node->sexp (jump-ex-it-exit node))
	       ,(node->sexp (jump-ex-it-value node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::make-box)
   (node->sexp-hook node)
   `(make-box ,(node->sexp (make-box-value node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::box-ref)
   (node->sexp-hook node)
   `(box-ref ,(node->sexp (box-ref-var node))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::box-set!)
   (node->sexp-hook node)
   `(box-set! ,(node->sexp (box-set!-var node))
	      ,(node->sexp (box-set!-value node))))
		  
	

     
   
