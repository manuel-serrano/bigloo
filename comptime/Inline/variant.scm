;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Inline/variant.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 10 10:19:51 1996                          */
;*    Last change :  Mon Nov 14 17:05:49 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The variant/invariant property computations.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_variant
   (include "Tools/trace.sch"
	    "Inline/variant.sch")
   (import  type_type
	    tools_shape
	    ast_var
	    ast_node)
   (export  (wide-class local/variant::local variant::bool)
	    (invariant-args ::node ::variable ::obj)
	    (variant-args ::variable)
	    (shrink-args! ::variable)
	    (substitutions ::variable actuals var-args)
	    (remove-invariant-args!::app ::app)))

;*---------------------------------------------------------------------*/
;*    invariant-args ...                                               */
;*    -------------------------------------------------------------    */
;*    This function computes the list of invariant parameters of a     */
;*    function and it marks (widen) all the formal with a variant      */
;*    property.                                                        */
;*---------------------------------------------------------------------*/
(define (invariant-args node var::variable calls)
   (let* ((fun  (variable-value var))
	  (args (sfun-args fun))
	  (vals (app-args node)))
      ;; first, we remove all the formals which are written or
      ;; bound to a written variable
      (for-each (lambda (local val)
		   (if (and (eq? (local-access local) 'read)
			    (or (atom? val)
				(kwote? val)
				(and (var? val)
				     (eq? (variable-access (var-variable val))
					  'read))))
		       (widen!::local/variant local (variant #f))
		       (widen!::local/variant local (variant #t))))
		args vals)
      ;; then, we scan alls the calls to find the matches
      (for-each (lambda (app::app)
		   (let loop ((actuals (app-args app))
			      (args    args))
		      (cond
			 ((null? args)
			  'done)
			 ((null? actuals)
			  (for-each (lambda (local)
				       (local/variant-variant-set! local #t))
				    args))
			 ((and (var? (car actuals))
			       (eq? (var-variable (car actuals)) (car args)))
			  (loop (cdr actuals) (cdr args)))
			 (else
			  (local/variant-variant-set! (car args) #t)
			  (loop (cdr actuals) (cdr args))))))
		calls)
      ;; we compute the results now
      (let loop ((args      args)
		 (invariant '()))
	 (if (null? args)
	     (reverse! invariant)
	     (loop (cdr args)
		   (if (local/variant-variant (car args))
		       invariant
		       (cons (car args) invariant)))))))

;*---------------------------------------------------------------------*/
;*    variant-args ...                                                 */
;*    -------------------------------------------------------------    */
;*    The list of the variant arguments.                               */
;*---------------------------------------------------------------------*/
(define (variant-args var::variable)
   (let* ((fun  (variable-value var))
	  (args (sfun-args fun)))
      (let loop ((args    args)
		 (variant '()))
	 (if (null? args)
	     (reverse! variant)
	     (loop (cdr args)
		   (if (local/variant-variant (car args))
		       (cons (car args) variant)
		       variant))))))

;*---------------------------------------------------------------------*/
;*    substitutions ...                                                */
;*---------------------------------------------------------------------*/
(define (substitutions var::variable actuals var-args)
   (let* ((fun      (variable-value var))
	  (all-args (sfun-args fun)))
      (let loop ((actuals       actuals)
		 (all-args     all-args)
		 (var-args      var-args)
		 (substitutions '()))
	 (cond
	    ((null? all-args)
	     (reverse! substitutions))
	    ((local/variant-variant (car all-args))
	     (loop (cdr actuals)
		   (cdr all-args)
		   (cdr var-args)
		   (cons (car var-args) substitutions)))
	    (else
	     (loop (cdr actuals)
		   (cdr all-args)
		   var-args
		   (cons (if (var? (car actuals))
			     (var-variable (car actuals))
			     (car actuals))
			 substitutions)))))))

;*---------------------------------------------------------------------*/
;*    shrink-args! ...                                                 */
;*---------------------------------------------------------------------*/
(define (shrink-args! var::variable)
   (let* ((fun  (variable-value var))
	  (args (sfun-args fun)))
      (for-each (lambda (a) (if (wide-object? a) (shrink! a))) args)))

;*---------------------------------------------------------------------*/
;*    remove-invariant-args! ...                                       */
;*    -------------------------------------------------------------    */
;*    This function removes from a call the invariant arguments. It    */
;*    uses the variant property computed in the `invariant-args'       */
;*    function. it does not make side-effect on the list itself (i.e.  */
;*    it allocates new lists) because the orignal list is saved some-  */
;*    where and restored after a recursive inlining.                   */
;*---------------------------------------------------------------------*/
(define (remove-invariant-args!::app app::app)
   (with-access::app app (args fun)
      (let loop ((formals  (sfun-args (variable-value (var-variable fun))))
		 (old-args args)
		 (new-args '()))
	 (cond
	    ((null? old-args)
	     (set! args (reverse! new-args))
	     app)
	    ((and (local/variant? (car formals))
		  (not (local/variant-variant (car formals))))
	     (loop (cdr formals)
		   (cdr old-args)
		   new-args))
	    (else
	     (loop (cdr formals)
		   (cdr old-args)
		   (cons (car old-args) new-args)))))))
	     
      
      



