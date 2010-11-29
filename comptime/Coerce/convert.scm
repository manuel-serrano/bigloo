;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Coerce/convert.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jan 19 10:19:33 1995                          */
;*    Last change :  Mon Nov 29 09:08:25 2010 (serrano)                */
;*    Copyright   :  1995-2010 Manuel Serrano, see LICENSE file        */
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
	    ast_sexp
	    ast_var
	    ast_node
	    ast_ident
	    ast_lvtype
	    object_class
	    coerce_coerce
	    effect_spread)
    (export (convert!::node ::node ::type ::type ::bool)
	    (runtime-type-error loc ti value)))

;*---------------------------------------------------------------------*/
;*    type-error/location ...                                          */
;*---------------------------------------------------------------------*/
(define (type-error/location loc function from to)
   (user-error/location loc
			function
			"Type error"
			(bigloo-type-error-msg
			 ""
			 (shape to)
			 (shape from))))

;*---------------------------------------------------------------------*/
;*    type-warning/location ...                                        */
;*---------------------------------------------------------------------*/
(define (type-warning/location loc function from to)
   (user-warning/location loc
			  function
			  "Type error"
			  (bigloo-type-error-msg
			   ""
			   (shape to)
			   (shape from))))

;*---------------------------------------------------------------------*/
;*    runtime-type-error ...                                           */
;*---------------------------------------------------------------------*/
(define (runtime-type-error loc ti value)
   (trace coerce "runtime-type-error: " (shape ti) "  " (shape value) #\Newline)
   (define (runtime-type-error/id id)
      (trace coerce "   runtime-type-error/id: " (shape id) #\Newline)
      (let ((fname (when (location? loc) (location-full-fname loc)))
	    (pos (when (location? loc) (location-pos loc))))
	 `(failure
	   ((@ type-error __error) ,fname ,pos
				   ,(symbol->string (current-function))
				   ,(symbol->string ti)
				   ,id)
	   #f #f)))
   (define (runtime-type-error/node)
      (trace coerce "   runtime-type-error/node: " #\Newline)
      (let* ((aux (gensym 'aux))
	     (res (top-level-sexp->node
		   `(let ((,(mark-symbol-non-user! (symbol-append aux '::obj))
			   #unspecified))
		       ,(runtime-type-error/id aux))
		   loc)))
	 (set-cdr! (car (let-var-bindings res)) value)
	 res))
   (if (node? value)
       (runtime-type-error/node)
       (runtime-type-error/id value)))

;*---------------------------------------------------------------------*/
;*    convert-error ...                                                */
;*    -------------------------------------------------------------    */
;*    When we find a type error and the wanted type is a Bigloo        */
;*    type we emit a warning and compile an error. Otherwise, we       */
;*    stop the compilation.                                            */
;*---------------------------------------------------------------------*/
(define (convert-error from to loc node)
   (trace coerce "convert-error: " (shape from) " " (shape to) " " (shape node)
	  #\Newline)
   (if (and (not (eq? to *obj*)) (sub-type? to *obj*))
       (begin
	  (type-warning/location loc (current-function) from to)
	  (coerce! (runtime-type-error loc (type-id to) node)
		   #unspecified
		   from
		   #f))
       (type-error/location loc (current-function) from to)))

;*---------------------------------------------------------------------*/
;*    convert! ...                                                     */
;*    -------------------------------------------------------------    */
;*    If the parameter `safe' is set to false it means that type       */
;*    convertion in between Bigloo object much not be checked.         */
;*---------------------------------------------------------------------*/
(define (convert! node from to safe)
   (trace coerce "convert: " (shape from) " -> " (shape to) " : " (shape node)
	  #\Newline)
   (if (eq? from to)
       node
       (let ((to (get-aliased-type to))
	     (from (get-aliased-type from)))
	  (if (or (eq? from to) (type-magic? from))
	      node
	      (let ((coercer (find-coercer from to))
		    (loc (node-loc node)))
		 (if (not (coercer? coercer))
		     ;; There is no convertion between these types. 
		     ;; Thus, it is a type error.
		     (convert-error from to loc node)
		     (let ((checks  (coercer-check-op coercer))
			   (coerces (coercer-coerce-op coercer)))
			(trace (coerce 2)
			       "   checks : " checks #\Newline
			       "   coerces: " coerces #\Newline)
			(let loop ((checks  checks)
				   (coerces coerces)
				   (node    node))
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
				     (make-one-conversion (car checks)
							  from
							  to
							  (car checks)
							  (car coerces)
							  node
							  safe))))))))))))

;*---------------------------------------------------------------------*/
;*    make-one-conversion ...                                          */
;*    -------------------------------------------------------------    */
;*    from A0 we build an new node like:                               */
;* 	(let ((v A0))                                                  */
;* 	   (if (check? v)                                              */
;* 	       (coerce v)                                              */
;* 	       (type-error)))                                          */
;*---------------------------------------------------------------------*/
(define (make-one-conversion id-from from to checkop coerceop node safe)
   (if (or (null? checkop) (not safe))
       (do-convert coerceop node from)
       (if (tclass? from)
	   (make-one-class-conversion id-from from to checkop coerceop node)
	   (make-one-type-conversion id-from from to checkop coerceop node))))

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
(define (make-one-type-conversion id-from from to check-op coerce-op node)
   (trace (coerce 2) "make-one-type-conversion: " (shape node) " ("
	  (shape from) " -> " (shape to) ")" #\Newline)
   (let* ((aux   (mark-symbol-non-user! (gensym 'aux)))
	  (loc   (node-loc node))
	  (lnode (top-level-sexp->node
		  ;; we coerce all checked object into `obj' because
		  ;; all the predicate are only defined under this
		  ;; type and sometime `super-class' object' have to
		  ;; be checked and they are not of obj type (but of
		  ;; a compatible type).
		  `(let ((,(mark-symbol-non-user!
			    (make-typed-ident aux (type-id from)))
			  #unspecified))
		      (if (,check-op ,aux)
			  ,aux
			  ,(runtime-type-error loc (type-id to) aux)))
		  loc)))
      (lvtype-node! lnode)
      (spread-side-effect! lnode)
      (let* ((var        (car (car (let-var-bindings lnode))))
	     (coerce-app (do-convert coerce-op
				     (instantiate::var
					(loc loc)
					(type from)
					(variable var))
				     from))
	     (condn      (skip-let-var lnode)))
	 ;; we set the local variable type
	 (local-type-set! var from)
	 ;; and the local variable value
	 (set-cdr! (car (let-var-bindings lnode)) node)
	 (conditional-true-set! condn coerce-app)
	 (conditional-false-set! condn
				 (coerce! (conditional-false condn)
					  #unspecified
					  from
					  #f))
	 lnode)))

;*---------------------------------------------------------------------*/
;*    make-one-class-conversion ...                                    */
;*---------------------------------------------------------------------*/
(define (make-one-class-conversion id-from from to check-op coerce-op node)
   (if (and (tclass? to) (type-subclass? from to))
       (do-convert coerce-op node from)
       (let* ((aux   (gensym 'aux))
	      (aux2  (gensym 'aux2))
	      (loc   (node-loc node))
	      (lnode (top-level-sexp->node
		      `(let ((,(mark-symbol-non-user!
				(symbol-append aux '::obj)) #unspecified))
			  (let ((,(mark-symbol-non-user!
				   (symbol-append aux2 '::obj)) #unspecified))
			     (if (,check-op ,aux2)
				 ,aux
				 ,(runtime-type-error loc (type-id to) aux))))
		      loc)))
	  (lvtype-node! lnode)
	  (spread-side-effect! lnode)
	  (let* ((var        (car (car (let-var-bindings lnode))))
		 (coerce-app (do-convert coerce-op
					 (instantiate::var
					    (loc loc)
					    (type from)
					    (variable var))
					 from))
		 (condn      (skip-let-var lnode)))
	     ;; we set the local variable type
	     (local-type-set! var from)
	     ;; and the local variable value
	     (set-cdr! (car (let-var-bindings lnode)) node)
	     (let ((binding2 (car (let-var-bindings (let-var-body lnode)))))
		(set-cdr! binding2 (instantiate::cast
				      (loc loc)
				      (type *obj*)
				      (arg (instantiate::var 
					      (loc loc)
					      (type from)
					      (variable var))))))
	     (conditional-true-set! condn coerce-app)
	     (conditional-false-set! condn
				     (coerce! (conditional-false condn)
					      #unspecified
					      from
					      #f))
	     lnode))))

;*---------------------------------------------------------------------*/
;*    do-convert ...                                                   */
;*---------------------------------------------------------------------*/
(define (do-convert coerce-op node from::type)
   (trace coerce "do-convert: " (shape coerce-op) " " (shape node)
	  #\Newline)
   (if (null? coerce-op)
       node
       (let* ((nnode (top-level-sexp->node `(,coerce-op #unspecified)
					   (node-loc node))))
	  (trace coerce
		 "   app : " (shape nnode) #\Newline
		 "   type: " (shape (node-type nnode)) #\Newline
		 "   node: " (shape node) #\Newline
		 "   type: " (shape (node-type node)) #\Newline
		 "   from: " (shape from) #\Newline)
	  ;; we have to mark that the node has been converted and is
	  ;; now of the correct type...
 	  (node-type-set! node from)
	  (spread-side-effect! nnode)
	  ;; we apply the conversion
	  (cond
	     ((app? nnode)
	      (app-args-set! nnode (list node))
	      nnode)
	     ((let-var? nnode)
	      (let ((bdgs (let-var-bindings nnode)))
		 (if (or (null? bdgs) (not (null? (cdr bdgs))))
		     (internal-error "do-convert"
				     "Illegal converter"
				     (shape coerce-op))
		     (begin
			(local-type-set! (car (car bdgs)) from)
			(set-cdr! (car bdgs) node)
			nnode))))
	     (else
	      (internal-error "do-convert"
			      "Illegal converter"
			      (shape coerce-op)))))))









