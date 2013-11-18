;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tlift/walk.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov  6 18:02:26 2013                          */
;*    Last change :  Sun Nov 17 18:01:08 2013 (serrano)                */
;*    Copyright   :  2013 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Type tlift. Start from the leaves (i.e., function calls) and */
;*    propagate the type constraints up function parameters.           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tlift_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_typeof
	    type_cache
	    type_env
	    type_misc
	    ast_var
	    ast_node
	    ast_env
	    ast_walk
	    ast_alphatize
	    ast_local
	    ast_occur
	    module_module
	    engine_param
	    tlift_types
	    tlift_capture)
   (export  (tlift-walk! ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    tlift-walk! ...                                                  */
;*---------------------------------------------------------------------*/
(define (tlift-walk! globals)
   (pass-prelude "Type lifting")
   (for-each type-lift-function! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    type-lift-function! ...                                          */
;*---------------------------------------------------------------------*/
(define (type-lift-function! v::variable)
   (mark-captured! v)
   (with-access::variable v (value)
      (with-access::sfun value (body)
	 (type-lift body '()))))

;*---------------------------------------------------------------------*/
;*    unify-variable-type! ...                                         */
;*---------------------------------------------------------------------*/
(define (unify-variable-type! n::node atype::type v::var)
   (with-access::var v (variable)
      (unless (or (eq? atype *obj*) (eq? atype *_*))
	 (when (isa? variable local/tlift)
	    (with-access::local/tlift variable (ltype)
	       (cond
		  ((eq? ltype *_*)
		   (set! ltype atype))
		  ((type-less-specific? ltype atype)
		   (set! ltype atype))
		  ((not (eq? ltype atype))
		   (with-access::node n (loc)
		      (user-warning/location loc
			 "type lifting"
			 "Potential type error"
			 (bigloo-type-error-msg
			    ""
			    (shape atype)
			    (shape ltype)))))))))))

;*---------------------------------------------------------------------*/
;*    local-of-expr* ...                                               */
;*---------------------------------------------------------------------*/
(define-walk-method (local-of-expr* n::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    local-of-expr* ::var ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (local-of-expr* n::var)
   (with-access::var n (variable)
      (if (isa? variable local/tlift)
	  (list variable)
	  '())))

;*---------------------------------------------------------------------*/
;*    type-lift ::node ...                                             */
;*    -------------------------------------------------------------    */
;*    This function bubbles up type constraints it finds on leaves     */
;*    (i.e., function calls, let bindings, ...).                       */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::node env::pair-nil)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    type-lift ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::var env::pair-nil)
   (with-access::var n (variable type)
      (when (and (isa? variable local/tlift) (eq? type *_*))
	 (with-access::local/tlift variable (ltype)
	    (set! type ltype)))))

;*---------------------------------------------------------------------*/
;*    type-lift ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::app env::pair-nil)

   (define (unify-arguments-type! fun::fun formals::pair-nil)
      (cond
	 ((isa? fun sfun)
	  (with-access::sfun fun (args)
	     (for-each (lambda (a f)
			  (cond
			     ((isa? a local)
			      (with-access::local a (type)
				 (unify-variable-type! n type f)))
			     ((isa? a type)
			      (unify-variable-type! n a f))))
		args formals)))
	 ((isa? fun cfun)
	  (with-access::cfun fun (args-type)
	     (for-each (lambda (a f) (unify-variable-type! n a f))
		args-type formals)))
	 (else
	  '())))

   (call-default-walker)
   (with-access::app n (fun args)
      (with-access::var fun (variable)
	 (with-access::variable variable (value)
	    (unify-arguments-type! value args)))))
   
;*---------------------------------------------------------------------*/
;*    type-lift ::vref ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::vref env::pair-nil)
   (with-access::vref n (vtype otype expr*)
      (let ((arg (car expr*))
	    (index (cadr expr*)))
	 (when (isa? arg var)
	    (unify-variable-type! n vtype arg))
	 (when (isa? index var)
	    (unify-variable-type! n otype index)))))
	 
;*---------------------------------------------------------------------*/
;*    type-lift ::vset! ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::vset! env::pair-nil)
   (with-access::vset! n (vtype otype expr*)
      (call-default-walker)
      (let ((arg (car expr*))
	    (index (cadr expr*)))
	 (when (isa? arg var)
	    (unify-variable-type! n otype arg))
	 (when (isa? index var)
	    (unify-variable-type! n otype index)))))

;*---------------------------------------------------------------------*/
;*    type-lift ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::conditional env::pair-nil)
   (with-access::conditional n (test true false)
      (type-lift true env)
      (type-lift false env)
      (type-lift test env)
      (tprint "COND: " (shape test))
      (for-each (lambda (v)
		   ;; local-of-expr* is a multiset so we have to test
		   ;; before shrinking
		   (tprint "COND v=" (shape v))
		   (when (isa? v local/tlift)
		      (tprint "*** SHRINKING: " (shape v))
		      (shrink! v)))
	 (local-of-expr* test))))
	 
;*---------------------------------------------------------------------*/
;*    type-lift ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (type-lift n::let-var env::pair-nil)
   (with-access::let-var n (body bindings)
      (call-default-walker)
      (tprint "LET-VAR " (shape n))
      (for-each (lambda (b)
		   (let ((v (car b))
			 (val (cdr b)))
		      (tprint "v=" (shape v) " " (typeof v))
		      (when (isa? v local/tlift)
			 (with-access::local/tlift v (type ltype)
			    (when (eq? type *_*)
			       (tprint "SET TYPE: " (shape v)
				  " " (shape ltype))
			       (set! type ltype))
			    (when (isa? val var)
			       (with-access::var val (type)
				  (when (eq? type *_*)
				     (set! type ltype)))
			       (unify-variable-type! n ltype val))))))
	 bindings)))
