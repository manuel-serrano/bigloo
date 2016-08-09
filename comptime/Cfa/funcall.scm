;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/funcall.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun 25 07:47:42 1996                          */
;*    Last change :  Tue Aug  9 08:56:03 2016 (serrano)                */
;*    Copyright   :  1996-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The funcall management.                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_funcall
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    tools_args
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cfa_info
	    cfa_info2
	    cfa_cfa
	    cfa_loose
	    cfa_approx
	    cfa_app
	    cfa_procedure))

;*---------------------------------------------------------------------*/
;*    cfa! ...                                                         */
;*    -------------------------------------------------------------    */
;*    First, we compute the possible function called. Then for         */
;*    each of them, we compute the result of evaluating its            */
;*    body with the actual approximations.                             */
;*    -------------------------------------------------------------    */
;*    In this function, we always ignore the first argument which      */
;*    is the closure itself. Instead, it uses the approximation        */
;*    computed by the Cfa.                                             */
;*---------------------------------------------------------------------*/
(define-method (cfa! node::funcall/Cinfo)
   (with-access::funcall/Cinfo node (approx fun args)
      (trace (cfa 2) ">>>  funcall: " (shape node) " approx=" (shape approx)
	 #\Newline)
      (let* ((fapprox (cfa! fun))
	     (aapprox (map cfa! (cdr args))))
	 (trace (cfa 2)
	    "      fun: " (shape fapprox) #\Newline
	    "     args: " (shape aapprox) #\Newline)
	 ;; check for a possible type error
	 (let ((fun-type (approx-type fapprox)))
	    (unless (or (eq? fun-type *_*)
			(eq? fun-type *obj*)
			(eq? fun-type *procedure*))
	       (funcall-type-error node fun-type)))
	 ;; check the type...
	 (when (or (not (eq? (approx-type fapprox) *procedure*))
		   (approx-top? fapprox))
	    (approx-set-type! approx *obj*))
	 ;; compute the approximations
	 (cond
	    ((or (approx-top? fapprox)
		 (not (eq? (approx-type fapprox) *procedure*)))
	     (for-each (lambda (approx) (loose! approx 'all)) aapprox)
	     (for-each-approx-alloc
		(lambda (alloc)
		   (if (make-procedure-app? alloc)
		       (let ((eapprox (make-procedure-app-approx alloc)))
			  (union-approx!
			     approx
			     (funcall! alloc (cons eapprox aapprox) node)))
		       (make-empty-approx)))
		fapprox)
	     (approx-set-top! approx))
	    ((empty-approx-alloc? fapprox)
	     ;; this might occur when the the functions potentially invoked
	     ;; are obtained from a foreign function pragma-ed with
	     ;; no-cfa-top
	     (approx-set-type! approx *obj*))
	    (else
	     (for-each-approx-alloc
		(lambda (alloc)
		   (if (make-procedure-app? alloc)
		       (let ((eapprox (make-procedure-app-approx alloc)))
			  (union-approx!
			     approx
			     (funcall! alloc (cons eapprox aapprox) node)))
		       (make-empty-approx)))
		fapprox)))
	 ;; If approx is *obj* it must be propagated in all the procedures
	 ;; which from now on must return a bigloo type. In consequence,
	 ;; for each closure, we find the most possible specific Bigloo type
   	 (when (and *optim-cfa-unbox-closure-args*
		    (bigloo-type? (approx-type approx))
		    (not (eq? (approx-type approx) *_*)))
	    (for-each-approx-alloc
	       (lambda (a)
		  (when (make-procedure-app? a)
		     (set-procedure-approx-polymorphic! a)))
	       fapprox))
	 (trace (cfa 2) "<<<  funcall: " (shape node) "-> approx=" (shape approx)
	    #\Newline)
	 approx)))

;*---------------------------------------------------------------------*/
;*    optional-correct-arity? ...                                      */
;*---------------------------------------------------------------------*/
(define (optional-correct-arity? fun args-approx)
   ;; add one for the closure itself
   (let* ((mi (+fx 1 (fun-arity fun)))
	  (ma (+fx mi (length (sfun-optionals fun))))
	  (l (length args-approx)))
      (and (>=fx l mi) (<=fx l ma))))

;*---------------------------------------------------------------------*/
;*    key-correct-arity? ...                                           */
;*---------------------------------------------------------------------*/
(define (key-correct-arity? fun args-approx)
   ;; add one for the closure itself
   (let* ((mi (+fx 1 (fun-arity fun)))
	  (ma (+fx mi (*fx 2 (length (sfun-keys fun)))))
	  (l (length args-approx)))
      (and (>=fx l mi) (<=fx l ma))))

;*---------------------------------------------------------------------*/
;*    cfa-correct-arity? ...                                           */
;*---------------------------------------------------------------------*/
(define (cfa-correct-arity? fun args-approx)
   (let ((g (sfun-the-closure-global fun)))
      (cond
	 ((global-optional? g)
	  (optional-correct-arity? (global-value g) args-approx))
	 ((global-key? g)
	  (key-correct-arity? (global-value g) args-approx))
	 (else
	  (sound-arity? (fun-arity fun) args-approx)))))

;*---------------------------------------------------------------------*/
;*    funcall! ...                                                     */
;*---------------------------------------------------------------------*/
(define (funcall!::approx alloc::make-procedure-app args-approx node)
   (let* ((callee (car (make-procedure-app-args alloc)))
	  (v      (var-variable callee))
	  (fun    (variable-value v))
	  (arity  (fun-arity fun)))
      (trace (cfa 3) " funcall!: " (shape callee) " (" (typeof fun)
	     ") arity: " arity " "
	     (shape args-approx) #\Newline)
      (cond 
	 ((not (cfa-correct-arity? fun args-approx))
	  ;; arity error
	  (with-access::make-procedure-app alloc (X-T?)
	     ;; mark that this function cannot be optimized
	     (set! X-T? #f))
	  (funcall-arity-error node v arity args-approx))
	 ((or (global-optional? (sfun-the-closure-global fun))
	      (global-key? (sfun-the-closure-global fun)))
	  ;; optional/key function, we loose everything
	  (trace (cfa 3) "  funcall! opt/key app!: " #\Newline)
	  (for-each (lambda (approx) (loose! approx 'all)) args-approx)
	  (make-type-approx *obj*))
	 ((>=fx arity 0)
	  ;; fix arity call
	  (trace (cfa 3) "  funcall! fx app!: " #\Newline)
	  (app! fun callee args-approx))
	 (else
	  (trace (cfa 3) "  funcall! va app!: " #\Newline)
	  ;; va arity call
	  (let loop ((old-args-approx args-approx)
		     (new-args-approx '())
		     (arity           arity))
	     (if (=fx arity -1)
		 (begin
		    (for-each (lambda (approx) (loose! approx 'all))
			      old-args-approx)
		    (app! fun
			  callee
			  (reverse! (cons (funcall/Cinfo-va-approx node)
					  new-args-approx))))
		 (loop (cdr old-args-approx)
		       (cons (car old-args-approx) new-args-approx)
		       (+fx arity 1))))))))

;*---------------------------------------------------------------------*/
;*    funcall-type-error ...                                           */
;*---------------------------------------------------------------------*/
(define (funcall-type-error node type)
   (trace (cfa 3) " *** type error *** " (shape node) #\Newline)
   (with-access::funcall/Cinfo node (type-error-noticed? loc)
      (unless type-error-noticed?
	 (set! type-error-noticed? #t)
	 (user-warning/location loc
	    "cfa"
	    "Possible funcall type error"
	    (shape type)))))

;*---------------------------------------------------------------------*/
;*    funcall-arity-error ...                                          */
;*---------------------------------------------------------------------*/
(define (funcall-arity-error node v arity args-approx)
   (with-access::funcall/Cinfo node (arity-error-noticed? loc)
      (if (not arity-error-noticed?)
	  (let ((len-prov (-fx (length args-approx) 1)))
	     (set! arity-error-noticed? #t)
	     (user-warning/location loc
				    ;; we duplicate to avoid the printing
				    ;; of the approximation informations.
				    (shape (cond
					      ((local? v)
					       (duplicate::local v))
					      ((global? v)
 					       (duplicate::global v))
					      (else
					       v)))
				    "Possible funcall arity error"
				    (string-append
				     (integer->string (-fx arity 1))
				     " arg(s) expected, "
				     (integer->string len-prov)
				     " provided")))))
   (trace (cfa 3) " *** arity error *** " (shape node) #\Newline)
   (make-empty-approx))
     
			  
