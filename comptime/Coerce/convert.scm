;*=====================================================================*/
;*    serrano/bigloo/5.0a/comptime/Coerce/convert.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 10:19:33 1995                          */
;*    Last change :  Fri Feb  6 15:12:57 2026 (serrano)                */
;*    Copyright   :  1995-2026 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The convertion. The coercion and type checks are generated       */
;*    inside this module.                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module coerce_convert
   (include "Tools/trace.sch"
	    "Tools/location.sch"
	    "Type/coercer.sch")
   (import  engine_param
	    backend_backend
	    tools_shape
	    tools_location
	    tools_error
	    tools_misc
	    type_type
	    type_cache
	    type_coercion
	    type_env
	    type_typeof
	    ast_sexp
	    ast_env
	    ast_var
	    ast_local
	    ast_node
	    ast_ident
	    ast_lvtype
	    ast_dump
	    ast_private
	    object_class
	    coerce_coerce
	    effect_spread)
    (export *notify-type-test*
	    (convert!::node ::node ::type ::type ::bool)
	    (runtime-type-error loc ti ::node)
	    (get-stack-check)))

;*---------------------------------------------------------------------*/
;*    *check* ...                                                      */
;*---------------------------------------------------------------------*/
(define *check* 0)

;*---------------------------------------------------------------------*/
;*    notification ...                                                 */
;*---------------------------------------------------------------------*/
(define notified-locations '())
(define *notify-type-test* #t)
(define *notify-counter* 0)

;*---------------------------------------------------------------------*/
;*    notify-type-test ...                                             */
;*---------------------------------------------------------------------*/
(define (notify-type-test from to loc)
   (when *notify-type-test*
      (let ((st (bigloo-trace-stack-depth)))
	 (bigloo-trace-stack-depth-set! 0)
	 (cond
	    ((not loc)
	     (set! *notify-counter* (+fx 1 *notify-counter*))
	     (warning
		(format " ~a. Type test inserted \"~a\" -> \"~a\""
		   *notify-counter* (shape from) (shape to))))
	    ((not (member loc notified-locations))
	     (set! *notify-counter* (+fx 1 *notify-counter*))
	     (set! notified-locations (cons loc notified-locations))
	     (warning/location (location-full-fname loc)
		(location-pos loc)
		(format " ~a. Type test inserted \"~a\" -> \"~a\""
		   *notify-counter* (shape from) (shape to)))))
	 (bigloo-trace-stack-depth-set! st))))
      
;*---------------------------------------------------------------------*/
;*    increment-stat-check! ...                                        */
;*---------------------------------------------------------------------*/
(define (increment-stat-check! from to loc)
   (set! *check* (+fx 1 *check*))
   (when *warning-types* (notify-type-test from to loc)))

;*---------------------------------------------------------------------*/
;*    get-stack-check ...                                              */
;*---------------------------------------------------------------------*/
(define (get-stack-check)
   *check*)

;*---------------------------------------------------------------------*/
;*    type-error/location ...                                          */
;*---------------------------------------------------------------------*/
(define (type-error/location loc function from to)
   (with-trace 'convert "type-error/location"
      (trace-item "loc=" loc)
      (trace-item "function=" function)
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))
      (user-error/location loc function "Type error"
	 (bigloo-type-error-msg "" (shape to) (shape from)))))

;*---------------------------------------------------------------------*/
;*    type-warning/location ...                                        */
;*---------------------------------------------------------------------*/
(define (type-warning/location loc function from to)
   (with-trace 'convert "type-warning/location"
      (trace-item "loc=" loc)
      (trace-item "function=" function)
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))
      (when *warning-type-error*
	 (user-warning/location loc function "Type error"
	    (bigloo-type-error-msg "" (shape to) (shape from))))))

;*---------------------------------------------------------------------*/
;*    runtime-type-error/id ...                                        */
;*---------------------------------------------------------------------*/
(define (runtime-type-error/id loc ti id::symbol)
   (with-trace 'convert "runtime-type-error/id"
      (trace-item "id=" (shape id))
      (let ((fname (when (location? loc) (location-full-fname loc)))
	    (pos (when (location? loc) (location-pos loc))))
	 `(failure
	     ((@ type-error __error) ,fname ,pos
				     ,(symbol->string (current-function))
				     ,(symbol->string ti)
				     ,id)
	     #f #f))))

;*---------------------------------------------------------------------*/
;*    runtime-type-error ...                                           */
;*---------------------------------------------------------------------*/
(define (runtime-type-error loc ti value::node)
   (with-trace 'convert "runtime-type-error"
      (trace-item "ti=" (shape ti))
      (trace-item "value=" (shape value))

      (define (get-value value)
	 (cond
	    ((cast? value)
	     (get-value (cast-arg value)))
	    ((var? value)
	     (coerce! (duplicate::ref value) #unspecified *obj* #f))
	    (else
	     (coerce! value #unspecified *obj* #f))))
      
      (let* ((aux (gensym 'err))
	     (uvalue (get-value value))
	     (res (top-level-sexp->node
		     `(let ((,(mark-symbol-non-user! (symbol-append aux '::obj))
			     #unspecified))
			 ,(runtime-type-error/id loc ti aux))
		     loc (get-genv))))
	 (coerce! res #unspecified (node-type value) #f)
	 (set-cdr! (car (let-var-bindings res)) (cast-obj-if-needed uvalue))
	 res)))

;*---------------------------------------------------------------------*/
;*    convert-error ...                                                */
;*    -------------------------------------------------------------    */
;*    When we find a type error and the wanted type is a Bigloo        */
;*    type we emit a warning and compile an error. Otherwise, we       */
;*    stop the compilation.                                            */
;*---------------------------------------------------------------------*/
(define (convert-error from to loc node safe)
   (with-trace 'convert "convert-error"
      (trace-item "node=" (shape node))
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))
      (trace-item "loc=" loc)
      (cond
	 ((not safe)
	  (type-warning/location loc (current-function) from to)
	  (instantiate::cast-null
	     (loc loc)
	     (c-format "")
	     (type to)))
	 ((and (not (eq? to *obj*)) (sub-type? to *obj*))
	  (let ((node (runtime-type-error loc (type-id to) node)))
	     (type-warning/location loc (current-function) from to)
	     (lvtype-node! node)
	     (instantiate::sequence
		(type to)
		(nodes (list node
			  (instantiate::cast-null
			     (c-format "")
			     (type to)))))))
	 ((tclass? to)
	  (let ((node (runtime-type-error loc (type-id to) node)))
	     (type-warning/location loc (current-function) from to)
	     (lvtype-node! node)
	     (instantiate::sequence
		(type to)
		(nodes (list node
			  (instantiate::cast-null
			     (c-format "")
			     (type to)))))))
	 ((not *warning-type-error*)
	  (cond
	     ((or (eq? to *int*) (eq? to *long*)
		  (eq? to *elong*)
		  (eq? to *llong*)
		  (eq? to *int8*)
		  (eq? to *uint8*)
		  (eq? to *int16*)
		  (eq? to *uint16*)
		  (eq? to *int32*)
		  (eq? to *uint32*)
		  (eq? to *int64*)
		  (eq? to *uint64*))
	      (let ((node (runtime-type-error loc (type-id to) node)))
		 (type-warning/location loc (current-function) from to)
		 (lvtype-node! node)
		 (instantiate::sequence
		    (type to)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::literal (type to) (value 0)))))))
	     ((eq? to *bool*)
	      (let ((node (runtime-type-error loc (type-id to) node)))
		 (type-warning/location loc (current-function) from to)
		 (lvtype-node! node)
		 (instantiate::sequence
		    (type to)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::literal (type to) (value 0)))))))
	     ((eq? to *real*)
	      (let ((node (runtime-type-error loc (type-id to) node)))
		 (type-warning/location loc (current-function) from to)
		 (lvtype-node! node)
		 (instantiate::sequence
		    (type to)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::literal (type to) (value 0.0)))))))
	     ((eq? to *char*)
	      (let ((node (runtime-type-error loc (type-id to) node)))
		 (type-warning/location loc (current-function) from to)
		 (lvtype-node! node)
		 (instantiate::sequence
		    (type to)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::literal (type to) (value #a000)))))))
	     ((eq? to *schar*)
	      (let ((node (runtime-type-error loc (type-id to) node)))
		 (type-warning/location loc (current-function) from to)
		 (lvtype-node! node)
		 (instantiate::sequence
		    (type *char*)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::literal (type *char*) (value #a000)))))))
	     ((eq? to *string*)
	      (let ((node (runtime-type-error loc (type-id to) node)))
		 (type-warning/location loc (current-function) from to)
		 (lvtype-node! node)
		 (instantiate::sequence
		    (type to)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::literal (type to) (value "")))))))
	     (else
	      (type-error/location loc (current-function) from to))))
	 (else
	  (type-error/location loc (current-function) from to)))))

;*---------------------------------------------------------------------*/
;*    convert! ...                                                     */
;*    -------------------------------------------------------------    */
;*    If the parameter `safe' is set to false it means that type       */
;*    convertion in between Bigloo objects must not be checked.        */
;*---------------------------------------------------------------------*/
(define (convert! node from to safe)
   (with-trace 'convert "convert!"
      (trace-item "node=" (shape node))
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))
      (trace-item "safe=" safe)
      (if (eq? from to)
	  node
	  (let ((to (get-aliased-type to))
		(fro (get-aliased-type from)))
	     (cond
		((eq? fro to)
		 node)
		((type-magic? fro)
		 (instantiate::sequence
		    (type to)
		    (nodes (list (coerce! node #unspecified from #f)
			      (instantiate::cast-null
				 (loc (node-loc node))
				 (type to)
				 (c-format (format "/* magic to ~a */" (shape to))))))))
		(else
		 (let ((coercer (find-coercer fro to))
		       (loc (node-loc node)))
		    (if (not (coercer? coercer))
			;; There is no convertion between these types. 
			;; Thus, it is a type error.
			(convert-error fro to loc node safe)
			(let loop ((checks (coercer-check-op coercer))
				   (coerces (coercer-coerce-op coercer))
				   (node node))
			   (cond
			      ((null? checks)
			       (if (null? coerces)
				   node
				   (internal-error "Illegal conversion"
				      (shape from)
				      (shape to))))
			      ((null? coerces)
			       (internal-error "Illegal conversion"
				  (shape from)
				  (shape to)))
			      (else
			       (loop (cdr checks)
				  (cdr coerces)
				  (make-one-conversion (cdar checks)
				     (cdar coerces)
				     (caar checks)
				     (caar coerces)
				     node
				     safe)))))))))))))

;*---------------------------------------------------------------------*/
;*    make-one-conversion ...                                          */
;*    -------------------------------------------------------------    */
;*    from A0 we build an new node like:                               */
;* 	(let ((v A0))                                                  */
;* 	   (if (check? v)                                              */
;* 	       (coerce v)                                              */
;* 	       (type-error)))                                          */
;*---------------------------------------------------------------------*/
(define (make-one-conversion from to checkop coerceop node safe)
   (if (or (eq? checkop #t) (not safe))
       (do-convert coerceop node from to)
       (if (tclass? from)
	   (make-one-class-conversion from to checkop coerceop node)
	   (make-one-type-conversion from to checkop coerceop node))))

;*---------------------------------------------------------------------*/
;*    skip-let-var ...                                                 */
;*---------------------------------------------------------------------*/
(define (skip-let-var node)
   (if (let-var? node)
       (skip-let-var (let-var-body node))
       node))

;*---------------------------------------------------------------------*/
;*    make-one-type-conversion ...                                     */
;*---------------------------------------------------------------------*/
(define (make-one-type-conversion from to check-op coerce-op node)
   (with-trace 'convert "make-one-type-conversion"
      (trace-item "node=" (shape node))
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))
      (let* ((aux (mark-symbol-non-user! (gensym 'aux)))
	     (loc (node-loc node))
	     (lnode (top-level-sexp->node
		       ;; we coerce all checked object into `obj' because
		       ;; all the predicate are only defined under this
		       ;; type and sometimes `super-class' object' have to
		       ;; be checked and they are not of obj type (but of
		       ;; a compatible type).
		       `(let ((,(mark-symbol-non-user!
				   (make-typed-ident aux (type-id from)))
			       #unspecified))
			   (if (,check-op ,aux)
			       ,aux
			       ,(runtime-type-error/id loc (type-id to) aux)))
		       loc (get-genv))))
	 (increment-stat-check! from to loc)
	 (spread-side-effect! lnode)
	 (let* ((var (car (car (let-var-bindings lnode))))
		(ref (instantiate::ref
			(loc loc)
			(type (strict-node-type to from))
			(variable var)))
		(coerce-app (do-convert coerce-op ref from to))
		(condn (skip-let-var lnode)))
	    ;; we set the local variable type
	    (local-type-set! var from)
	    ;; and the local variable value
	    (set-cdr! (car (let-var-bindings lnode)) node)
	    (node-type-set! node from)
	    ;; FIXME
	    (node-type-set! lnode to)
	    (node-type-set! condn to)
	    (conditional-true-set! condn coerce-app)
	    (conditional-false-set! condn
	       (coerce! (conditional-false condn) #unspecified to #f))
	    (lvtype-node! lnode)
	    lnode))))

;*---------------------------------------------------------------------*/
;*    cast-obj-if-needed ...                                           */
;*---------------------------------------------------------------------*/
(define (cast-obj-if-needed node)
   (if (or (eq? (get-static-type node) *obj*)
	   (not (backend-strict-type-cast (the-backend))))
       node
       (instantiate::cast
	  (loc (node-loc node))
	  (type *obj*)
	  (arg node))))

;*---------------------------------------------------------------------*/
;*    make-one-class-conversion ...                                    */
;*---------------------------------------------------------------------*/
(define (make-one-class-conversion from to check-op coerce-op node)

   (define (ref-check from to check-op node::ref)
      (let* ((loc (node-loc node))
	     (tmp (gensym 'tmp))
	     (ttmp (mark-symbol-non-user! (symbol-append tmp '::obj)))
	     (lnode (top-level-sexp->node
		       `(let ((,ttmp #unspecified))
			   (if (,check-op ,tmp)
			       ,node
			       ,(runtime-type-error loc (type-id to)
				   (duplicate::ref node))))
		       loc (get-genv))))
	 (increment-stat-check! from to loc)
	 (spread-side-effect! lnode)
	 (with-access::ref node ((var variable))
	    (let ((condn (skip-let-var lnode))
		  (binding (car (let-var-bindings lnode))))
	       ;; set the value of the temp variable
	       (set-cdr! binding (cast-obj-if-needed node))
	       (values lnode condn (duplicate::ref node))))))

   (define (node-check from to check-op node::node)
      (let* ((loc (node-loc node))
	     (o (make-local-svar (gensym 'o) from))
	     (ref (instantiate::ref
		     (loc loc)
		     (variable o)
		     (type from))))
	 (multiple-value-bind (lnode cnode rnode)
	    (ref-check from to check-op ref)
	    (values (instantiate::let-var
		       (loc loc)
		       (type to)
		       (bindings (list (cons o node)))
		       (body lnode))
	       cnode
	       rnode))))

   (with-trace 'convert "make-one-class-concversion"
      (trace-item "node=" (shape node))
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))

      (if (and (tclass? to) (type-subclass? from to))
	  (do-convert coerce-op node from to)
	  (multiple-value-bind (lnode cnode rnode)
	     (if (isa? node ref)
		 (ref-check from to check-op node)
		 (node-check from to check-op node))
	     (let ((capp (do-convert coerce-op rnode from to))
		   (fnode (coerce! (conditional-false cnode) #unspecified to #f)))
		(with-access::conditional cnode (true false type)
		   (set! type to)
		   (set! true capp)
		   (set! false fnode)
		   (lvtype-node! lnode)
		   lnode))))))

;*---------------------------------------------------------------------*/
;*    do-convert ...                                                   */
;*---------------------------------------------------------------------*/
(define (do-convert coerce-op node from::type to::type)
   (with-trace 'convert "do-convert"
      (trace-item "node=" (shape node))
      (trace-item "from=" (shape from))
      (trace-item "to=" (shape to))
      
      (let ((loc (node-loc node)))
	 (if (eq? coerce-op #t)
	     (if (is-subtype? from to)
		 node
		 (instantiate::cast
		    (loc loc)
		    (type to)
		    (arg node)))
	     (let ((nnode (top-level-sexp->node
			     `(,coerce-op ,node) loc (get-genv))))
		;; we have to mark that the node has been converted and is
		;; now of the correct type...
		(lvtype-node! nnode)
		(spread-side-effect! nnode)
		;; explicit args type cast when the backend demands it
		(when (backend-strict-type-cast (the-backend))
		   (when (isa? nnode app)
		      (with-access::app nnode (fun args)
			 (when (isa? fun ref)
			    (let* ((proc (variable-value (var-variable fun)))
				   (ta (node-type (car args)))
				   (tf (cond
					  ((sfun? proc)
					   (car (sfun-args proc)))
					  ((cfun? proc)
					   (car (cfun-args-type proc)))
					  (else
					   *obj*))))
			       (unless (eq? ta tf)
				  (set-car! args
				     (instantiate::cast
					(loc loc)
					(type tf)
					(arg (car args))))))))))
		nnode)))))

