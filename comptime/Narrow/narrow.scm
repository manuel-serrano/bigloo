;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/narrow.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 11 19:18:58 2013                          */
;*    Last change :  Fri Apr 21 18:41:06 2017 (serrano)                */
;*    Copyright   :  2013-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Narrow functions body                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module narrow_narrow
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    ast_walk
	    ast_alphatize
	    ast_local
	    ast_occur
	    module_module
	    engine_param
	    narrow_types
	    narrow_defuse
	    narrow_set)
   (export  (narrow-function! ::variable)))

;*---------------------------------------------------------------------*/
;*    shape ::local/narrow ...                                         */
;*---------------------------------------------------------------------*/
(define-method (shape node::local/narrow)
   (string->symbol (format "!~a" (call-next-method))))

;*---------------------------------------------------------------------*/
;*    narrow-function! ...                                             */
;*    -------------------------------------------------------------    */
;*    Narrow the scope of local variables (i.e., variables bound       */
;*    by an inner let form).                                           */
;*---------------------------------------------------------------------*/
(define (narrow-function! v::variable)
   (with-trace 1 (variable-id v)
      (with-access::variable v (value)
	 (defuse-sfun! value)
	 (with-access::sfun value (body args)
	    (set! body (narrow-scope! body args))))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::node ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow-scope! n::node locals::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    kill-used ...                                                    */
;*---------------------------------------------------------------------*/
(define (kill-used::pair-nil locals::pair-nil node-or-nodes)
   
   (define (kill-used-in-node n)
      (multiple-value-bind (def use)
	 (defuse n)
	 (set! locals (disjonction locals use))
	 (set! locals (disjonction locals def))))

   (when (pair? locals)
      (cond
	 ((null? node-or-nodes)
	  #unspecified)
	 ((pair? node-or-nodes)
	  (for-each kill-used-in-node node-or-nodes))
	 (else
	  (kill-used-in-node node-or-nodes))))

   locals)

;*---------------------------------------------------------------------*/
;*    kill-closure ...                                                 */
;*---------------------------------------------------------------------*/
(define (kill-closure::pair-nil locals::pair-nil nodes::pair-nil)
   (with-trace 4 "kill-closure"
      (trace-item "locals=" (shape locals))
      (when (pair? locals)
	 (for-each (lambda (n)
		      (with-trace 4 "kill-closure, node"
			 (trace-item "node=" (shape n))
			 (trace-item "locals in closure="
			    (shape (node-locals-in-closure* n locals)))
			 (when (pair? locals)
			    (set! locals
			       (disjonction locals
				  (node-locals-in-closure* n locals))))))
	    nodes))
      locals))

;*---------------------------------------------------------------------*/
;*    node-locals-in-closure* ::node ...                               */
;*---------------------------------------------------------------------*/
(define-walk-method (node-locals-in-closure* n::node locals::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-locals-in-closure* ::let-fun ...                            */
;*---------------------------------------------------------------------*/
(define-walk-method (node-locals-in-closure* n::let-fun locals::pair-nil)
   (if (pair? (node-closure* n '()))
       locals
       '()))

;*---------------------------------------------------------------------*/
;*    node-closure* ::node ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (node-closure* n::node closures::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-closure* ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-closure* n::let-fun closures::pair-nil)
   (with-access::let-fun n (locals body)
      (let ((closures (append locals closures)))
	 (append (node-closure* body closures)
	    (append-map (lambda (f)
			   (with-access::local f (value)
			      (with-access::sfun value (body)
				 (node-closure* body closures))))
	       locals)))))

;*---------------------------------------------------------------------*/
;*    node-closure* ::closure ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (node-closure* n::closure closures::pair-nil)
   (with-access::closure n (variable)
      (if (memq variable closures)
	  (list variable)
	  '())))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow-scope! n::sequence locals)
   
   (define (defassign? n::node locals::pair-nil)
      (when (isa? n setq)
	 (with-trace 3 "defassign?"
	    (trace-item "n=" (shape n))
	    (trace-item "locals=" (shape locals))
	    (with-access::setq n (var value)
	       (with-access::var var ((v variable))
		  (and (isa? v local/narrow)
		       (memq v (kill-used locals value))))))))
   
   (define (copy-local variable)
      (with-access::variable variable (value id type)
	 (if (isa? value sfun)
	     (make-local-sfun id type value)
	     (make-local-svar id type))))
   
   (define (setq->let nodes type locals)
      (with-access::setq (car nodes) (var value (stype type) loc)
	 (with-access::var var (variable)
	    (let* ((newvar (copy-local variable))
		   (newval (narrow-scope! value locals))
		   (seq (instantiate::sequence
			   (type type)
			   (nodes (narrow-scope*!
				     (cdr nodes) type
				     (kill-used locals value))))))
	       (instantiate::let-var
		  (loc loc)
		  (type type)
		  (bindings (list (cons newvar newval)))
		  (body (alphatize (list variable) (list newvar) loc seq)))))))
   
   (define (narrow-scope*! ns type locals)
      (let loop ((nodes ns)
		 (locals locals))
	 (cond
	    ((null? nodes)
	     ns)
	    ((defassign? (car nodes) locals)
	     (with-trace 1 "setq->let"
		(trace-item "n=" (shape (car nodes)))
		(set-car! nodes (setq->let nodes type locals))
		(set-cdr! nodes '())
		ns))
	    (else 
	     (multiple-value-bind (def use)
		(defuse (car nodes))
		(let ((defs (kill-used locals (cdr nodes))))
		   (with-trace 3 "narrow-scope! ::sequence, node"
		      (trace-item "n=" (shape (car nodes)))
		      (trace-item "locals=" (shape locals))
		      (trace-item "def(n)=" (shape def))
		      (trace-item "defs=" (shape defs))
		      (set-car! nodes (narrow-scope! (car nodes) defs)))
		   (loop (cdr nodes) (kill-used locals (car nodes)))))))))
   
   (with-trace 2 "narrow-scope! ::sequence"
      (trace-item "n=" (shape n))
      (trace-item "locals=" (shape locals))
      (with-access::sequence n (nodes type)
	 (narrow-scope*! nodes type
	    (kill-closure locals nodes))))
   n)

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::conditional ...                                  */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::conditional locals::pair-nil)
   (with-access::conditional n (test true false)
      (multiple-value-bind (deftest usetest)
	 (defuse test)
	 (let ((defs (kill-used locals (list true false))))
	    (set! test (narrow-scope! test defs))
	    (let ((locals (kill-used locals test)))
	       (set! true (narrow-scope! true (kill-used locals (list false))))
	       (set! false (narrow-scope! false (kill-used locals (list true))))))))
   n)

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::switch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::switch locals::pair-nil)
   (with-access::switch n (test clauses)
      (multiple-value-bind (deftest usetest)
	 (defuse test)
	 (let ((defs (kill-used locals (map cdr clauses))))
	    (set! test (narrow-scope! test defs))
	    (let ((locals (kill-used locals test)))
	       (for-each (lambda (clause)
			    (set-cdr! clause
			       (narrow-scope! (cdr clause) locals)))
		  clauses)))))
   n)

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::sync ...                                         */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::sync locals::pair-nil)
   (with-access::sync n (mutex prelock body)
      (multiple-value-bind (defprelock useprelock)
	 (defuse prelock)
	 (multiple-value-bind (defmutex usemutex)
	    (defuse mutex)
	    (multiple-value-bind (defbody usebody)
	       (defuse body)
	       (let ((pdefs (kill-used locals (list mutex body))))
		  (set! prelock (narrow-scope! prelock pdefs))
		  (let* ((locals (kill-used locals prelock))
			 (mdefs (kill-used locals (list body))))
		     (set! mutex (narrow-scope! mutex mdefs))
		     (let ((locals (kill-used locals mutex)))
			(set! body (narrow-scope! body locals)))))))))
   n)

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::let-var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::let-var locals::pair-nil)
   (with-trace 2 "narrow-scope! ::let-var"
      (trace-item "n=" (shape n))
      (trace-item "locals=" (shape locals))
      (with-access::let-var n (bindings body)
	 (with-trace 4 "narrow-scope! ::let-var, binding.1"
	    (for-each (lambda (b)
			 (trace-item "b=" (shape b))
			 (multiple-value-bind (def use)
			    (defuse (cdr b))
			    (let ((bdefs (kill-used locals
					    (cons body
					       (filter-map (lambda (b2)
							      (unless (eq? b b2)
								 (cdr b2)))
						  bindings)))))
			       (set-cdr! b (narrow-scope! (cdr b) bdefs)))))
	       bindings))
	 (let ((locals (append (map car bindings) locals)))
	    (with-trace 4 "narrow-scope! ::let-var, binding.2"
	       (for-each (lambda (b)
			    (trace-item "b=" (shape b))
			    (multiple-value-bind (def use)
			       (defuse (cdr b))
			       (trace-item "def=" (shape def))
			       (trace-item "use=" (shape use)))
			    (set! locals (kill-used locals (cdr b))))
		  bindings))
	    (with-trace 2 "narrow-scope! ::let-var, body"
	       (trace-item "n=" (shape body))
	       (trace-item "locals=" (shape locals))
	       (set! body (narrow-scope! body locals))))))
   n)

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::let-fun locals::pair-nil)
   
   (define (fun-body fun)
      (with-access::local fun (value)
	 (with-access::sfun value (body)
	    body)))

   (define (narrow-fun! fun body funs locals)
      (with-trace 2 "narrow-fun!"
	 (trace-item "fun=" (shape fun))
	 (trace-item "locals=" (shape locals))
	 (let ((others (filter-map (lambda (fun2)
				      (unless (eq? fun fun2)
					 (fun-body fun2)))
			  funs)))
	    (with-access::local fun (value)
	       (with-access::sfun value ((fbody body) args)
		  (multiple-value-bind (def use)
		     (defuse fbody)
		     (let* ((locals (append args locals))
			    (fdef (kill-used locals (cons body others))))
			(set! fbody (narrow-scope! fbody fdef)))
		     (kill-used locals (list fbody))))))))

   (with-trace 2 "narrow-scope! ::let-fun"
      (trace-item "n=" (shape n))
      (trace-item "locals=" (shape locals))
      (with-access::let-fun n (body (funs locals))
	 (let ((locals (if (pair? (node-closure* n '()))
			   (kill-used locals
			      (map (lambda (f)
				      (with-access::local f (value)
					 (with-access::sfun value (body)
					    body)))
				 funs))
			   locals)))
	    (let ((blocals locals))
	       (for-each (lambda (fun)
			    (set! blocals (narrow-fun! fun body funs blocals)))
		  funs)
	       (set! body (narrow-scope! body blocals)))))
      n))
