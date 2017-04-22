;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/arithmetic.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 20 09:48:45 2000                          */
;*    Last change :  Fri Apr 21 18:44:21 2017 (serrano)                */
;*    Copyright   :  2000-17 Manuel Serrano, see LICENSE file          */
;*    -------------------------------------------------------------    */
;*    This module implements a refined estimate computations for       */
;*    generic operator. The key idea is that, if we call a function    */
;*    like + and if we don't approximate + as a special function, the  */
;*    parameters to + will be forced to be OBJ and thus, all the       */
;*    optimization will be disabled.                                   */
;*    -------------------------------------------------------------    */
;*    The key idea is that for each arithmetic function (+, -, neg,    */
;*    etc.), if the two arguments are fixnum then the result is        */
;*    fixnum, if the two arguments are flonum then the result is       */
;*    a flonum. Otherwise, it is an obj.                               */
;*    -------------------------------------------------------------    */
;*    This works in collaboration with the CFA_SPECIALIZE module. This */
;*    last one is in charge of translating generic operators into      */
;*    specific operators in the annotated source code.                 */
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
   (export  (cleanup-arithmetic-nodes! globals)))

;*---------------------------------------------------------------------*/
;*    cleanup-arithmetic-nodes! ...                                    */
;*    -------------------------------------------------------------    */
;*    For each arithmetic nodes that has not been allocated specific   */
;*    arithmetic types, we allocate generic types.                     */
;*---------------------------------------------------------------------*/
(define (cleanup-arithmetic-nodes! globals)
   (define (cleanup-type t)
      (cond
	 ((type? t)
	  (if (eq? t *_*) *obj* t))
	 ((local? t)
	  (when (eq? (local-type t) *_*)
	     (local-type-set! t *obj*))
	  t)
	 (else
	  t)))
   (for-each (lambda (node)
		(with-access::arithmetic-app node (fun args)
		   (let* ((f (var-variable fun))
			  (val (variable-value f)))
		      ;; we have first to unspecialize the function call
		      (when (eq? (variable-type f) *_*)
			 (variable-type-set! f *obj*))
		      (when (eq? (node-type node) *_*)
			 (node-type-set! node *obj*))
		      (cond
			 ((sfun? val)
			  (sfun-args-set! val (map cleanup-type (sfun-args val))))
			 ((cfun? val)
			  (let loop ((l (cfun-args-type val)))
			     (if (pair? l)
				 (begin
				    (set-car! l (cleanup-type (car l)))
				    (loop (cdr l))))))))))
	     *arithmetic-nodes*)
   ;; patch all the bodies to take into account the new computed types
   (for-each cleanup-fun! globals)
   globals)

;*---------------------------------------------------------------------*/
;*    cleanup-fun! ...                                                 */
;*---------------------------------------------------------------------*/
(define (cleanup-fun! var)
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (cleanup-node! body)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::node ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (cleanup-node! node::node)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::sequence ...                                     */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::sequence)
   (with-access::sequence node (nodes type)
      (when (pair? nodes)
	 (for-each cleanup-node! nodes)
	 (when (eq? type *_*)
	    (set! type (node-type (car (last-pair nodes))))))))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::sync ...                                         */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::sync)
   (with-access::sync node (body mutex prelock type)
      (cleanup-node! mutex)
      (cleanup-node! prelock)
      (cleanup-node! body)
      (when (eq? type *_*)
	 (set! type (node-type body)))))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::setq ...                                         */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::setq)
   (with-access::setq node (value)
      (cleanup-node! value)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::conditional ...                                  */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::conditional)
   (with-access::conditional node (type test true false)
      (cleanup-node! test)
      (cleanup-node! true)
      (cleanup-node! false)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::fail ...                                         */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::fail)
   (with-access::fail node (type proc msg obj)
      (cleanup-node! proc)
      (cleanup-node! msg)
      (cleanup-node! obj)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::switch ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::switch)
   (with-access::switch node (clauses test)
      (cleanup-node! test)
      (for-each (lambda (clause) (cleanup-node! (cdr clause))) clauses)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::let-fun ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::let-fun)
   (with-access::let-fun node (type locals body)
      (for-each cleanup-fun! locals)
      (cleanup-node! body)
      (when (eq? type *_*)
	 (set! type (node-type body)))))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::let-var ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::let-var)
   (with-access::let-var node (type bindings body)
      (for-each (lambda (b) (cleanup-node! (cdr b))) bindings)
      (cleanup-node! body)
      (when (eq? type *_*)
	 (set! type (node-type body)))))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::set-ex-it ...                                    */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::set-ex-it)
   (with-access::set-ex-it node (type var body)
      (cleanup-node! var)
      (cleanup-node! body)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::jump-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (cleanup-node! exit)
      (cleanup-node! value)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::make-box ...                                     */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::make-box)
   (with-access::make-box node (value)
      (cleanup-node! value)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::box-ref ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::box-ref)
   (with-access::box-ref node (var)
      (cleanup-node! var)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::box-set! ...                                     */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::box-set!)
   (with-access::box-set! node (var value)
      (cleanup-node! var)
      (cleanup-node! value)))

;*---------------------------------------------------------------------*/
;*    cleanup-node! ::app-ly ...                                       */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::app-ly)
   (with-access::app-ly node (fun arg)
      (cleanup-node! fun)
      (cleanup-node! arg)))
      
;*---------------------------------------------------------------------*/
;*    cleanup-node! ::funcall ...                                      */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::funcall)
   (with-access::funcall node (fun args)
      (cleanup-node! fun)
      (for-each cleanup-node! args)))
      
;*---------------------------------------------------------------------*/
;*    cleanup-node! ::app ...                                          */
;*---------------------------------------------------------------------*/
(define-method (cleanup-node! node::app)
   (with-access::app node (fun args)
      (cleanup-node! fun)
      (for-each cleanup-node! args)))

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
	 (when (eq? (variable-type f) *obj*)
	    ;; if the node is a predicate (such as <), the return type,
	    ;; is bool and there is no need to introduce type inference
	    (variable-type-set! f *_*)
	    (node-type-set! node *_*))
	 (cond
	  ((sfun? val)
	   (sfun-args-set! val (map unspecified-type (sfun-args val))))
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
	     (shape approx) "\n    currently: " (shape approx) #\Newline)
      ;; we process all the argument to the function call
      (let ((args-approx (map cfa! args)))
	 ;; find the type
	 (let ((ty (find-first-specialized-type args-approx)))
	    (trace (cfa 5)
		   "      args-approx=" (shape args-approx) #\Newline
		   "      ty=" (shape ty) #\Newline
		   "      spec-types=" (map shape spec-types) #\Newline)
	    (cond
	       ((types-compatible? args-approx ty spec-types)
		;; ok, we just return as it is...
		(trace (cfa 5) "      TYPE COMPATIBLE\n")
		(when (and (all-types-specialized? args-approx spec-types)
			   (eq? (approx-type approx) *_*))
		   ;; all types are specialized, we specialize
		   ;; the result
		   (approx-set-type! approx ty)
		   (continue-cfa! 'arithmetic-app)))
	       ((unless (eq? (global-id (var-variable fun)) 'c-eq?))
		(for-each (lambda (a) (loose! a 'all)) args-approx)
		(approx-set-type! approx *obj*)))))
      ;; we are done
      (trace (cfa 4) "<<< arithmetic: " (shape approx) #\Newline)
      approx))
