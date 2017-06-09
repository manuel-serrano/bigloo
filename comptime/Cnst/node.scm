;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cnst/node.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  6 14:08:40 1995                          */
;*    Last change :  Wed Jun  7 15:36:12 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The constant compilation (of the kwoted forms and                */
;*    `make-??-procedure' calls).                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cnst_node
   (include "Tools/trace.sch"
	    "Tvector/tvector.sch"
	    "Cnst/node.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    ast_var
	    ast_node
	    ast_env
	    cnst_cache
	    cnst_alloc)
   (export  (generic cnst!::node ::node)
	    (generic get-node-atom-value node::node))
   (static  (wide-class local/bvalue::local
	       (binding-value::node read-only))))

;*---------------------------------------------------------------------*/
;*    cnst! ...                                                        */
;*---------------------------------------------------------------------*/
(define-generic (cnst!::node node::node))
   
;*---------------------------------------------------------------------*/
;*    cnst! ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::literal)
   (with-access::literal node (value loc)
      (cond
	 ((keyword? value) (cnst-alloc-keyword value loc))
	 ((ucs2-string? value) (cnst-alloc-ucs2-string value loc))
	 ((string? value) (cnst-alloc-string value loc))
	 ((bignum? value) (or (cnst-alloc-bignum value loc) node))
	 (else node))))

;*---------------------------------------------------------------------*/
;*    cnst! ::patch ...                                                */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::patch)
   (with-access::patch node (value)
      (cnst! value)
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::kwote)
   (with-access::kwote node (value loc type)
      (cond
	 ((symbol? value)
	  (cnst-alloc-symbol value loc))
	 ((keyword? value)
	  (cnst-alloc-keyword value loc))
	 ((pair? value)
	  (cnst-alloc-list value loc))
	 ((vector? value)
	  (cnst-alloc-vector value loc))
	 ((homogeneous-vector? value)
	  (cnst-alloc-homogenous-vector value loc))
	 ((string? value)
	  (cnst-alloc-string value loc))
	 ((ucs2-string? value)
	  (cnst-alloc-ucs2-string value loc))
	 ((a-tvector? value)
	  (cnst-alloc-tvector value loc))
	 ((or (char? value)
	      (fixnum? value)
	      (boolean? value)
	      (real? value)
	      (cnst? value)
	      (elong? value)
	      (llong? value)
	      (int8? value)
	      (uint8? value)
	      (int16? value)
	      (uint16? value)
	      (int32? value)
	      (uint32? value)
	      (int64? value)
	      (uint64? value))
	  (instantiate::literal
	     (loc loc)
	     (type (strict-node-type (get-type-atom value) type))
	     (value value)))
	 ((bignum? value)
	  (or (cnst-alloc-bignum value loc)
	      (instantiate::literal
		 (loc loc)
		 (type (strict-node-type (get-type-atom value) type))
		 (value value))))
	 ((struct? value)
	  (cnst-alloc-struct value loc))
	 (else
	  (internal-error "cnst-quote" "Illegal expression" (shape node))))))

;*---------------------------------------------------------------------*/
;*    cnst! ...                                                        */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::var)
   node)

;*---------------------------------------------------------------------*/
;*    cnst! ::closure ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::closure)
   (internal-error "cnst!" "Unexepected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    cnst! ::sequence ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::sequence)
   (with-access::sequence node (nodes)
      (cnst*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::sync ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::sync)
   (with-access::sync node (body mutex prelock)
      (set! mutex (cnst! mutex))
      (set! prelock (cnst! prelock))
      (set! body (cnst! body))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::extern ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::extern)
   (with-access::extern node (expr*)
      (cnst*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::cast ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::cast)
   (with-access::cast node (arg)
      (cnst! arg)
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::setq ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::setq)
   (with-access::setq node (value)
      (set! value (cnst! value))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::conditional ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::conditional)
   (with-access::conditional node (test true false)
      (set! test (cnst! test))
      (set! true (cnst! true))
      (set! false (cnst! false))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::fail ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::fail)
   (with-access::fail node (proc msg obj)
      (set! proc (cnst! proc))
      (set! msg (cnst! msg))
      (set! obj (cnst! obj))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::switch ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::switch)
   (with-access::switch node (clauses test)
      (set! test (cnst! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (cnst! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::let-fun ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (cnst! (sfun-body fun)))))
		locals)
      (set! body (cnst! body))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::let-var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::let-var)
   (with-access::let-var node (body bindings)
       (for-each (lambda (binding)
		    (let ((var (car binding)))
		       (set-cdr! binding (cnst! (cdr binding)))
		       (if (eq? (local-access var) 'read)
			   (widen!::local/bvalue var
			      (binding-value (cdr binding))))))
		 bindings)
      (set! body (cnst! body))
      node))
 
;*---------------------------------------------------------------------*/
;*    cnst! ::set-ex-it ...                                            */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (cnst! body))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::jump-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (cnst! exit))
      (set! value (cnst! value))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::make-box ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::make-box)
   (with-access::make-box node (value)
      (set! value (cnst! value))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::box-ref ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (cnst! var))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::box-set! ...                                             */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (cnst! var))
      (set! value (cnst! value))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::app-ly ...                                               */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (cnst! fun))
      (set! arg (cnst! arg))
      node))

;*---------------------------------------------------------------------*/
;*    cnst! ::funcall ...                                              */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (cnst! fun))
      (cnst*! args)
      node))
		
;*---------------------------------------------------------------------*/
;*    cnst*! ...                                                       */
;*---------------------------------------------------------------------*/
(define (cnst*! nodes)
   (let loop ((hook nodes))
      (if (null? hook)
	  'done
	  (begin
	     (set-car! hook (cnst! (car hook)))
	     (loop (cdr hook))))))

;*---------------------------------------------------------------------*/
;*    cnst! ::app ...                                                  */
;*---------------------------------------------------------------------*/
(define-method (cnst! node::app)
   (with-access::app node (args loc)
      (cnst*! args)
      ;; then we make special cases depending on the called function
      (if (null? args)
	  node
	  (let* ((fun (var-variable (app-fun node)))
		 (actual (car args))
		 (actual-value (get-node-atom-value actual)))
	     (cond
		((eq? fun *string->bstring*)
		 (if (string? actual-value)
		     (let ((r (cnst-alloc-string actual-value loc)))
			(trace cnst "string->bstring: " (shape r) #\Newline)
			r)
		     node))
		((eq? fun *bool->bbool*)
		 (if (boolean? actual-value)
		     (if actual-value
			 (instantiate::var
			    (loc loc)
			    (type *bbool*)
			    (variable *btrue*))
			 (instantiate::var
			    (loc loc)
			    (type *bbool*)
			    (variable *bfalse*)))
		     node))
		((or (eq? fun *make-fx-procedure*)
		     (eq? fun *make-va-procedure*))
		 (let ((size-value (get-node-atom-value (caddr args))))
		    (if (and (fixnum? size-value) (=fx size-value 0))
			(cnst-alloc-procedure node loc)
			node)))
		((eq? fun *make-l-procedure*)
		 (let ((size-value (get-node-atom-value (cadr args))))
		    (if (and (fixnum? size-value) (=fx size-value 0))
			(cnst-alloc-l-procedure node loc)
			node)))
		((eq? fun *double->real*)
		 (if (real? actual-value)
		     (cnst-alloc-real actual-value loc)
		     node))
		((eq? fun *elong->belong*)
		 (if (elong? actual-value)
		     (cnst-alloc-elong actual-value loc)
		     node))
		((eq? fun *llong->bllong*)
		 (if (llong? actual-value)
		     (cnst-alloc-llong actual-value loc)
		     node))
		((eq? fun *int32->bint32*)
		 (if (int32? actual-value)
		     (cnst-alloc-int32 actual-value loc)
		     node))
		((eq? fun *uint32->buint32*)
		 (if (uint32? actual-value)
		     (cnst-alloc-uint32 actual-value loc)
		     node))
		((eq? fun *int64->bint64*)
		 (if (int64? actual-value)
		     (cnst-alloc-int64 actual-value loc)
		     node))
		((eq? fun *uint64->buint64*)
		 (if (uint64? actual-value)
		     (cnst-alloc-uint64 actual-value loc)
		     node))
		(else
		 node))))))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ...                                          */
;*    -------------------------------------------------------------    */
;*    To return false, we just return a non-atomic value.              */
;*---------------------------------------------------------------------*/
(define-generic (get-node-atom-value node::node)
   '(no-atom-value))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ::atom ...                                   */
;*---------------------------------------------------------------------*/
(define-method (get-node-atom-value node::atom)
   (atom-value node))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ::var ...                                    */
;*---------------------------------------------------------------------*/
(define-method (get-node-atom-value node::var)
   (let ((v (var-variable node)))
      (if (local/bvalue? v)
	  (get-node-atom-value (local/bvalue-binding-value v))
	  '(no-atom-value))))

;*---------------------------------------------------------------------*/
;*    get-node-atom-value ::app ...                                    */
;*---------------------------------------------------------------------*/
(define-method (get-node-atom-value node::app)
   (with-access::app node (args)
      (let ((fun (var-variable (app-fun node))))
	 (cond
	    ((eq? fun *long->int*)
	     (if (and (pair? args) (null? (cdr args)))
		 (get-node-atom-value (car args))
		 '(no-atom-value)))
	    (else
	     '(no-atom-value))))))
