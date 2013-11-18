;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Narrow/narrow.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Nov 11 19:18:58 2013                          */
;*    Last change :  Sun Nov 17 17:22:39 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
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
	    narrow_defuse)
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
   (with-access::variable v (value)
      (with-access::sfun value (body)
	 (set! body (narrow! body)))))

;*---------------------------------------------------------------------*/
;*    mark-captured-local ::node ...                                   */
;*    -------------------------------------------------------------    */
;*    Mark local captured local variables as non eligible for          */
;*    optimization.                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (mark-captured-local n::node binder::int)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    narrow! ::node ...                                               */
;*    -------------------------------------------------------------    */
;*    Narrow the body (excluding marked variables).                    */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow! n::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    narrow! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow! n::let-var)
   (with-trace 1 "narrow"
      (trace-item "n=" (shape n))
      (with-access::let-var n (bindings body)
	 (let ((locals (map (lambda (b)
			       (let ((v (car b)))
				  (widen!::local/narrow v)))
			  bindings)))
	    (set! body (narrow-scope! body locals 0)))
	 (call-default-walker))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::node ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow-scope! n::node locals::pair-nil binder::int)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::var locals binder::int)
   (with-access::var n (variable)
      (when (isa? variable local/narrow)
	 (with-access::local/narrow variable ((b binder))
	    (when (>fx binder b)
	       (shrink! variable))))
      n))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-walk-method (narrow-scope! n::sequence locals binder::int)
   
   (define (defassign? s::node locals::pair-nil uses::pair-nil)
      (when (isa? s setq)
	 (with-access::setq s (var)
	    (with-access::var var (variable)
	       (when (and (isa? variable local/narrow) (memq variable locals))
		  (with-access::local/narrow variable ((b binder))
		     (or (>=fx b binder)
			 (not (any (lambda (use) (memq variable use))
				 uses)))))))))

   (define (copy-local variable)
      (with-access::variable variable (value id type)
	 (if (isa? value sfun)
	     (make-local-sfun id type value)
	     (make-local-svar id type))))
   
   (define (setq->let nodes type locals)
      (with-access::setq (car nodes) (var value (stype type) loc)
	 (with-access::var var (variable)
	    (let* ((newv (copy-local variable))
		   (bind (cons newv value)))
	       (widen!::local/narrow newv
		  (binder binder))
	       (shrink! variable)
	       (if (null? (cdr nodes))
		   (instantiate::let-var
		      (loc loc)
		      (type type)
		      (bindings (list bind))
		      (body (instantiate::atom
			       (value #unspecified)
			       (type stype))))
		   (let ((seq (instantiate::sequence
				 (type type)
				 (nodes (cdr nodes)))))
		      (instantiate::let-var
			 (loc loc)
			 (type type)
			 (bindings (list bind))
			 (body (narrow-scope!
				  (alphatize (list variable)
				     (list newv)
				     loc seq)
				  (remq variable locals)
				  binder)))))))))
   
   (define (dead-locals locals uses)
      (filter (lambda (l)
		 (every (lambda (s) (not (memq l s))) uses))
	 locals))

   (with-trace 1 "narrow-scope!"
      (trace-item "n=" (shape n))
      (trace-item "locals=" (shape locals))
      (if (null? locals)
	  n
	  (with-access::sequence n (nodes type)
	     (let ((uses '()))
		(for-each (lambda (node)
			     (multiple-value-bind (def use)
				(defuse-locals node locals)
				(set! uses (cons use uses))))
		   nodes)
		(let loop ((nodes nodes)
			   (locals locals)
			   (uses (reverse! uses)))
		   (when (pair? nodes)
		      (trace-item "node=" (shape (car nodes)))
		      (trace-item "  locals=" (shape locals)))
		   (cond
		      ((null? nodes)
		       n)
		      ((defassign? (car nodes) locals uses)
		       (trace-item "new let=" (shape (car nodes)))
		       (tprint "SETQ->LET: " (shape (car nodes)))
		       (set-car! nodes (setq->let nodes type locals))
		       (set-cdr! nodes '())
		       n)
		      (else
		       (let ((deads (dead-locals locals uses)))
			  (trace-item "  deads=" (shape deads))
			  (trace-item "  current-node=" (shape (car nodes)))
			  ;; mark that the locals are no longer used after
			  ;; (car nodes) and then can be considered as
			  ;; first defined in (car nodes)
			  (for-each (lambda (l)
				       (when (isa? l local/narrow)
					  (with-access::local/narrow l ((b binder))
					     (set! b -1)
					     (set! b (+fx binder 1)))))
			     deads)
			  (set-car! nodes
			     (narrow-scope! (car nodes) deads binder))
			  (loop (cdr nodes)
			     locals
			     (cdr uses)))))))))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::conditional ...                                  */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::conditional locals::pair-nil binder::int)
   
   (define (kill-locals locals use)
      (filter (lambda (l) (not (memq l use))) locals))
   
   (with-access::conditional n (test true false)
      (multiple-value-bind (deftrue usetrue)
	 (defuse-locals true locals)
	 (let ((locals-sans (kill-locals locals usetrue)))
	    (multiple-value-bind (deffalse usefalse)
	       (defuse-locals false locals)
	       (let ((locals-sans (kill-locals locals-sans usefalse)))
		  (set! test (narrow-scope! test locals-sans binder))
		  (set! true (narrow-scope! true locals binder))
		  (set! false (narrow-scope! false locals binder))
		  n))))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::select ...                                       */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::select locals::pair-nil binder::int)
   
   (define (kill-locals locals use)
      (filter (lambda (l) (not (memq l use))) locals))
   
   (with-access::select n (test clauses)
      (let ((locals-sans locals))
	 (for-each (lambda (clause)
		      (multiple-value-bind (def use)
			 (defuse-locals (cdr clause) locals)
			 (set! locals-sans (kill-locals locals-sans use))
			 (set-cdr! clause
			    (narrow-scope! (cdr clause) locals binder))))
	    clauses)
	 (set! test (narrow-scope! test locals-sans binder))
	 n)))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::sync ...                                         */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::sync locals::pair-nil binder::int)
   
   (define (kill-locals locals use)
      (filter (lambda (l) (not (memq l use))) locals))
   
   (with-access::sync n (mutex prelock body)
      (multiple-value-bind (defnodes usenodes)
	 (defuse-locals n locals)
	 (let ((locals-sans-nodes (kill-locals locals usenodes)))
	    (multiple-value-bind (defmutex usemutex)
	       (defuse-locals mutex locals-sans-nodes)
	       (let ((locals-sans-mutex (kill-locals locals-sans-nodes usemutex)))
		  (set! prelock (narrow-scope! prelock locals-sans-mutex binder))
		  (set! mutex (narrow-scope! mutex locals-sans-nodes binder))
		  (set! body (narrow-scope! body locals binder))
		  n))))))

;*---------------------------------------------------------------------*/
;*    narrow-scope! ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::let-fun locals::pair-nil binder::int)

   (define (mark-variable! l)
      (when (isa? l local/narrow)
	 (with-access::local/narrow l (%count2)
	    (set! %count2 (+fx 1 %count2)))))

   (define (reset-variable! l)
      (when (isa? l local/narrow)
	 (with-access::local/narrow l (%count2)
	    (set! %count2 0))))
   
   (define (kill-locals locals)
      (filter (lambda (l)
		 (when (isa? l local/narrow)
		    (with-access::local/narrow l (%count2)
		       (<=fx %count2 1))))
	 locals))
   
   (with-access::let-fun n (body (funs locals))
      (multiple-value-bind (def use)
	 (defuse-locals body locals)
	 (for-each mark-variable! def)
	 (for-each mark-variable! use)
	 (for-each (lambda (fun)
		      (with-access::local fun (id value)
			 (with-access::sfun value (body)
			    (multiple-value-bind (deffun usefun)
			       (defuse-locals body locals)
			       (for-each mark-variable! deffun)
			       (for-each (lambda (l)
					    (when (isa? l local/narrow)
					       (shrink! l)))
				  usefun)))))
	    funs)
	 (let ((nlocals (kill-locals locals)))
	    (for-each reset-variable! locals)
	    (for-each (lambda (fun)
			 (with-access::local fun (value)
			    (with-access::sfun value (body)
			       (set! body
				  (narrow-scope! body nlocals (+fx 1 binder))))))
	       funs)
	    (set! body (narrow-scope! body nlocals binder))
	    n))))
		      
;*---------------------------------------------------------------------*/
;*    narrow-scope! ::let-var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (narrow-scope! n::let-var locals::pair-nil binder::int)
   
   (define (kill-locals locals use)
      (filter (lambda (l) (not (memq l use))) locals))

   (with-access::let-var n (bindings body)
      (multiple-value-bind (defbody usebody)
	 (defuse-locals body locals)
	 (let ((locals-sans (kill-locals locals usebody)))
	    (for-each (lambda (b)
			 (set-cdr! b
			    (narrow-scope! (cdr b) locals-sans binder)))
	       bindings)
	    (set! body (narrow-scope! body locals binder))
	    n))))
