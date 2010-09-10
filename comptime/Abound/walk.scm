;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Abound/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Fri Sep 10 17:22:27 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Introduce array bound checking                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module abound_walk
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
	    backend_backend)
   (export  (abound-walk! globals)))

;*---------------------------------------------------------------------*/
;*    abound-walk! ...                                                 */
;*    -------------------------------------------------------------    */
;*    When the compiler is running in unsafe-range mode, this function */
;*    is not even called.                                              */
;*---------------------------------------------------------------------*/
(define (abound-walk! globals)
   (pass-prelude "Abound" init-cache!) 
   (for-each abound-fun! globals)
   (pass-postlude globals clear-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *string-ref* #unspecified)
(define *string-set!* #unspecified)
(define *struct-ref* #unspecified)
(define *struct-set!* #unspecified)

;*---------------------------------------------------------------------*/
;*    init-cache! ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-cache!)
   (set! *string-ref* (find-global '$string-ref 'foreign))
   (set! *string-set!* (find-global '$string-set! 'foreign))
   (set! *struct-ref* (find-global '$struct-ref 'foreign))
   (set! *struct-set!* (find-global '$struct-set! 'foreign))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-cache! ...                                                 */
;*---------------------------------------------------------------------*/
(define (clear-cache!)
   (set! *string-ref* #f))

;*---------------------------------------------------------------------*/
;*    abound-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define (abound-fun! global)
   (enter-function (global-id global))
   (let ((fun (variable-value global)))
      (sfun-body-set! fun (abound-node (sfun-body fun)))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    abound-node ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (abound-node::node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::sequence)
   (abound-node*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::app)
   (abound-node*! (app-args node))
   ;; check if we are calling string-ref/set!, or struct-ref/set!
   (with-access::app node (fun args loc)
      (let ((v (var-variable fun)))
	 (cond
	    ((or (eq? v *string-ref*) (eq? v *string-set!*))
	     (let* ((s (mark-symbol-non-user! (gensym 's)))
		    (i (mark-symbol-non-user! (gensym 'i)))
		    (l (mark-symbol-non-user! (gensym 'l)))
		    (lname (when (location? loc) (location-full-fname loc)))
		    (lpos (when (location? loc) (location-pos loc)))
		    (name (if (eq? v *string-ref*) "string-ref" "string-set!"))
		    (types (cfun-args-type (variable-value v))))
		(top-level-sexp->node
		 `(let ((,(make-typed-ident s (type-id (car types))) ,(car args))
			(,(make-typed-ident i (type-id (cadr types))) ,(cadr args)))
		     (let ((,(make-typed-ident l (type-id (cadr types)))
			    ($string-length ,s)))
			(if ($string-bound-check? ,i ,l)
			    ,(duplicate::app node
				(args (cons* s i (cddr args))))
			    (failure
			     ((@ index-out-of-bounds-error __error)
			      ,lname ,lpos ,name ,i ,s)
			     #f #f))))
		 loc)))
	    (else
	     node)))))
 
;*---------------------------------------------------------------------*/
;*    abound-node ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (abound-node fun))
      (set! arg (abound-node arg))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (abound-node fun))
      (abound-node*! args)
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::extern)
   (abound-node*! (extern-expr* node))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::vref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::vref)
   (call-next-method)
   (with-access::vref node (expr* loc ftype otype vtype unsafe)
      (if unsafe
	  node
	  (let ((v (mark-symbol-non-user! (gensym 'v)))
		(i (mark-symbol-non-user! (gensym 'i)))
		(l (mark-symbol-non-user! (gensym 'l)))
		(lname (when (location? loc) (location-full-fname loc)))
		(lpos (when (location? loc) (location-pos loc))))
	     (top-level-sexp->node
	      `(let ((,(make-typed-ident v (type-id vtype))
		       ,(car expr*))
		      (,(make-typed-ident i (type-id otype))
		       ,(cadr expr*)))
		  (let ((,(make-typed-ident l (type-id otype))
			 ,(if (eq? vtype *vector*)
			      `($vector-length ,v)
			      `($tvector-length ,v))))
		     (if ($vector-bound-check? ,i ,l)
			 ,(duplicate::vref node
			     (expr* (list v i)))
			 (failure
			  ((@ index-out-of-bounds-error __error)
			   ,lname ,lpos "vector-ref" ,(cadr expr*) ,(car expr*))
			  #f #f))))
	      loc)))))

;*---------------------------------------------------------------------*/
;*    abound-node ::vset! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::vset!)
   (call-next-method)
   (with-access::vset! node (expr* loc ftype otype vtype unsafe)
      (if unsafe
	  node
	  (let ((v (mark-symbol-non-user! (gensym 'v)))
		(i (mark-symbol-non-user! (gensym 'i)))
		(l (mark-symbol-non-user! (gensym 'l)))
		(lname (when (location? loc) (location-full-fname loc)))
		(lpos (when (location? loc) (location-pos loc))))
	     (top-level-sexp->node
	      `(let ((,(make-typed-ident v (type-id vtype)) ,(car expr*))
		     (,(make-typed-ident i (type-id otype)) ,(cadr expr*)))
		  (let ((,(make-typed-ident l (type-id otype))
			 ,(if (eq? vtype *vector*)
			      `($vector-length ,v)
			      `($tvector-length ,v))))
		     (if ($vector-bound-check? ,i ,l)
			 ,(duplicate::vset! node
			     (expr* (list v i (caddr expr*))))
			 (failure
			  ((@ index-out-of-bounds-error __error)
			   ,lname ,lpos "vector-ref" ,(cadr expr*) ,(car expr*))
			  #f #f))))
	      loc)))))

;*---------------------------------------------------------------------*/
;*    abound-node ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::cast)
   (abound-node (cast-arg node))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::setq)
   (setq-value-set! node (abound-node (setq-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::conditional)
   (with-access::conditional node (test true false)
       (set! test (abound-node test))
       (set! true (abound-node true))
       (set! false (abound-node false))
       node))

;*---------------------------------------------------------------------*/
;*    abound-node ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::fail)
   (with-access::fail node (proc msg obj)
      (set! proc (abound-node proc))
      (set! msg (abound-node msg))
      (set! obj (abound-node obj))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::select ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::select)
   (with-access::select node (clauses test)
      (set! test (abound-node test))
      (for-each (lambda (clause)
		   (set-cdr! clause (abound-node (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (v) (abound-fun! v)) locals)
      (set! body (abound-node body))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (abound-node (cdr binding))))
		bindings)
      (set! body (abound-node body))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::set-ex-it)
   (set-ex-it-body-set! node (abound-node (set-ex-it-body node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (abound-node exit)) 
      (set! value (abound-node value))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::make-box)
   (make-box-value-set! node (abound-node (make-box-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::box-ref)
   (box-ref-var-set! node (abound-node (box-ref-var node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (abound-node var))
      (set! value (abound-node value))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (abound-node*! node*)
   (when (pair? node*)
      (set-car! node* (abound-node (car node*)))
      (abound-node*! (cdr node*))))
   
