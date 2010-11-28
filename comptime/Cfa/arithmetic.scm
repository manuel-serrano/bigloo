;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/arithmetic.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 20 09:48:45 2000                          */
;*    Last change :  Sun Nov 28 08:33:55 2010 (serrano)                */
;*    Copyright   :  2000-10 Manuel Serrano, see LICENSE file          */
;*    -------------------------------------------------------------    */
;*    This module implements a refined estimate computations for       */
;*    generic operator. The key idea is that, if we call a function    */
;*    like +, if we don't approximate + as a special function, the     */
;*    parameters to + will be forced to be OBJ and thus, all the       */
;*    optimization will be disabled.                                   */
;*    -------------------------------------------------------------    */
;*    The key idea is that for each arithmetic function (+, -, neg,    */
;*    etc.), if the two arguments are fixnum then the result is        */
;*    fixnum, if the two arguments are flonum then the result is       */
;*    a flonum otherwise, it is an obj.                                */
;*    -------------------------------------------------------------    */
;*    This works in collaboration with the CFA_SPECIALIZE module. This */
;*    last one is in charge of translating generic operator to         */
;*    specific operator in the annotated source code.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module cfa_arithmetic
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_shape
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_loose
	    cfa_iterate
	    cfa_cfa
	    cfa_setup
	    cfa_approx
	    cfa_tvector)
   (export  (cleanup-arithmetic-nodes!)))

;*---------------------------------------------------------------------*/
;*    cleanup-arithmetic-nodes! ...                                    */
;*    -------------------------------------------------------------    */
;*    For each arithmetic nodes that has not been allocated specific   */
;*    arithmetic types, we allocate generic types.                     */
;*---------------------------------------------------------------------*/
(define (cleanup-arithmetic-nodes!)
   (define (cleanup-type t)
      (cond
	 ((type? t)
	  (if (eq? t *_*)
	      *obj*
	      t))
	 ((local? t)
	  (if (eq? (local-type t) *_*)
	      (local-type-set!
	       t
	       *obj*))
	  t)
	 (else
	  t)))
   (for-each (lambda (node)
		(with-access::arithmetic-app node (fun args)
		   (let* ((f (var-variable fun))
			  (val (variable-value f)))
		      ;; we have first to unspecialize the function call
		      (if (eq? (variable-type f) *_*)
			  (variable-type-set! f *obj*))
		      (if (eq? (node-type node) *_*)
			  (node-type-set! node *obj*))
		      (cond
			 ((sfun? val)
			  (sfun-args-set! val
					  (map cleanup-type (sfun-args val))))
			 ((cfun? val)
			  (let loop ((l (cfun-args-type val)))
			     (if (pair? l)
				 (begin
				    (set-car! l (cleanup-type (car l)))
				    (loop (cdr l))))))))))
	     *arithmetic-nodes*))

;*---------------------------------------------------------------------*/
;*    *arithmetic-nodes* ...                                           */
;*---------------------------------------------------------------------*/
(define *arithmetic-nodes* '())

;*---------------------------------------------------------------------*/
;*    node-setup! ::pre-arithmetic-app ...                             */
;*---------------------------------------------------------------------*/
(define-method (node-setup! node::pre-arithmetic-app)
   (define (unspecified-type l)
      (cond
	 ((type? l)
	  *_*)
	 ((local? l)
	  (local-type-set! l *_*)
	  l)
	 (else
	  l)))
   (with-access::pre-arithmetic-app node (fun args spec-types)
      (let* ((f (var-variable fun))
	     (val (variable-value f)))
	 ;; we have first to unspecialize the function call
	 (if (eq? (variable-type f) *obj*)
	     ;; if the node is a predicate (such as <), the return type,
	     ;; is bool and there is no need to introduce type inference
	     (begin
		(variable-type-set! f *_*)
		(node-type-set! node *_*)))
	 (cond
	  ((sfun? val)
	   (sfun-args-set! val
			   (map unspecified-type (sfun-args val))))
	  ((cfun? val)
	   (let loop ((l (cfun-args-type val)))
	      (if (pair? l)
		  (set-car! l (unspecified-type (car l)))
		  (loop (cdr l)))))
	  (else
	   (error "node-setup!" "Illegal arithmetic node" node)))
	 (set! *arithmetic-nodes* (cons node *arithmetic-nodes*))
	 ;; then, we can setup the all thing
	 (node-setup*! args)
	 (let* ((spec-types spec-types)
		(node (shrink! node)))
	    (widen!::arithmetic-app node
	       (spec-types spec-types)
	       (approx (if (eq? (variable-type f) *_*)
			   (make-empty-approx)
			   (make-type-approx (variable-type f)))))))))

;*---------------------------------------------------------------------*/
;*    cfa! ::arithmetic-app ...                                        */
;*---------------------------------------------------------------------*/
(define-method (cfa!::approx node::arithmetic-app)
   (define (normalize-type type)
      ;; normalize arithmethic types
      (if (eq? type *int*)
	  *long*
	  type))
   (define (find-first-specialized-type args-approx)
      ;; find the first non _ typed arguments
      (let loop ((args args-approx))
	 (if (null? args)
	     #f
	     (let ((t (normalize-type (approx-type (car args)))))
		(if (eq? t *_*)
		    (loop (cdr args))
		    t)))))
   (define (types-compatible? args-approx type spec-types)
      ;; check if all the arguments are either untyped yet or
      ;; all int or real
      (or (not type)
	  (and (memq type spec-types)
	       (let loop ((args args-approx))
		  (if (null? args)
		      #t
		      (let ((t (normalize-type (approx-type (car args)))))
			 (if (or (eq? t type) (eq? t *_*))
			     (loop (cdr args))
			     #f)))))))
   (define (all-types-specialized? args-approx spec-types)
      ;; are all the approximated type already specialized?
      (let loop ((args args-approx))
	 (cond
	    ((null? args)
	     #t)
	    ((memq (normalize-type (approx-type (car args))) spec-types)
	     (loop (cdr args)))
	    (else
	     #f))))
   (with-access::arithmetic-app node (fun args approx spec-types)
      (trace (cfa 4) ">>> arithmetic: " (shape node) " "
	     (shape approx) " currently: " (shape approx) #\Newline)
      ;; we process all the argument to the function call
      (let ((args-approx (map cfa! args)))
	 ;; find the type
	 (let ((ty (find-first-specialized-type args-approx)))
	    (cond
	       ((types-compatible? args-approx ty spec-types)
		;; ok, we just return as it is...
		(if (all-types-specialized? args-approx spec-types)
		    ;; we, all types are specialized, we specialize
		    ;; the result
		    (if (eq? (approx-type approx) *_*)
			;; this was an unallocated type
			(begin
			   (approx-set-type! approx ty)
			   (continue-cfa! 'arithmetic-app)))))
	       ((unless (eq? (global-id (var-variable fun)) 'c-eq?))
		(for-each (lambda (a) (loose! a 'all)) args-approx)
		(approx-set-type! approx *obj*)))))
      ;; we are done
      approx))




