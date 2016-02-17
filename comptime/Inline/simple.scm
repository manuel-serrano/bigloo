;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Inline/simple.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jun 17 14:01:30 1996                          */
;*    Last change :  Wed Feb 17 09:13:06 2016 (serrano)                */
;*    Copyright   :  1996-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inlining of simple functions (non recursive functions).      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_simple
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  engine_param
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    ast_local
	    tools_shape
	    tools_speek
	    tools_error
	    inline_inline
	    inline_size
	    module_module
	    ast_alphatize
	    ast_sexp
	    effect_effect
	    effect_spread)
   (export  (inline-app-simple::node ::node ::long ::obj ::bstring)))

;*---------------------------------------------------------------------*/
;*    inline-app-simple ...                                            */
;*    -------------------------------------------------------------    */
;*    Thanks to normalization of the ast building, all actuals         */
;*    values are placed into variables. Hence, we don't have to        */
;*    build a let construction. We just make an alpha-conversion.      */
;*---------------------------------------------------------------------*/
(define (inline-app-simple node kfactor stack msg)
   (trace (inline inline+ 2) "inline-app-simple: " (shape node) #\Newline)
   (let* ((callee (var-variable (app-fun node)))
	  (sfun (variable-value callee))
	  (formals (sfun-args sfun))
	  (actuals (app-args node))
	  (reductors (map (lambda (f a)
			     (cond
				((and (closure? a)
				      (eq? (local-access f) 'read)
				      (or (eq? (local-type f) *procedure*)
					  (eq? (local-type f) *_*)
					  (eq? (local-type f) *obj*)))
				 ;; We are given a closure and
				 ;; the formal binding is only read.
				 ;; Hence we do not use intermediate
				 ;; variable
				 (closure-variable a))
				((and (eq? (local-access f) 'read)
				      (or (eq? (local-type f) *obj*)
					  (eq? (local-type f) *_*))
				      (atom? a))
				 a)
				((and (eq? (local-access f) 'read)
				      (or (eq? (local-type f) *obj*)
					  (eq? (local-type f) *_*))
				      (var? a)
				      (eq? (variable-access (var-variable a))
					 'read))
				 (var-variable a))
				((and (eq? (local-access f) 'read)
				      (and (or (eq? (local-type f) *int*)
					       (eq? (local-type f) *long*))
					   (atom? a)
					   (fixnum? (atom-value a))))
				 a)
				(else
				 (clone-local
				    f
				    (duplicate::svar (local-value f))))))
			formals
			actuals))
	  (bindings (let loop ((reductors reductors)
			       (actuals   actuals)
			       (res       '()))
		       (cond
			  ((null? actuals)
			   (reverse! res))
			  ((and (closure? (car actuals))
				(eq? (car reductors)
				   (closure-variable (car actuals))))
			   (loop (cdr reductors)
			      (cdr actuals)
			      res))
			  ((eq? (car reductors) (car actuals))
			   (loop (cdr reductors)
			      (cdr actuals)
			      res))
			  ((and (var? (car actuals))
				(eq? (car reductors) (var-variable (car actuals))))
			   (loop (cdr reductors)
			      (cdr actuals)
			      res))
			  (else
			   (loop (cdr reductors)
			      (cdr actuals)
			      (cons (cons (car reductors)
				       (car actuals))
				 res))))))
	  (body (if (isfun? sfun)
		    (isfun-original-body sfun)
		    (sfun-body sfun)))
	  (arity (sfun-arity sfun))
	  (new-kfactor (*inlining-reduce-kfactor* kfactor))
	  (loc (node-loc node))
	  (type (variable-type callee)))
      (with-access::variable callee (occurrence)
	 (set! occurrence (-fx occurrence 1)))
      ;; some verbing small ...
      (unless (memq (sfun-class sfun) '(sifun sgfun))
	 (verbose 3 "         "
	    (shape callee) " --> " (current-function) " (" msg #\) #\Newline))
      ;; we compute the new body
      (trace (inline inline+ 3) "J'alphatize: "
	 (shape formals) " " (shape reductors)
	 #\Newline
	 "        sur le body: " (shape body) #\Newline)
      (let* ((alpha-body (alphatize formals reductors loc body)))
	 ;; we spread side effect for incoming inlines (such as
	 ;; null? which is translated into $null?).
	 (spread-side-effect! alpha-body)
	 (let* ((inode (inline-node alpha-body new-kfactor (cons callee stack)))
		(ibody (instantiate::let-var
			  (loc loc)
			  (type (strict-node-type
				   (node-type inode) type))
			  (side-effect (side-effect? alpha-body))
			  (bindings bindings)
			  (body inode))))
	    (for-each (lambda (reductor formal)
			 (when (local? reductor)
			    (local-user?-set! reductor (local-user? formal))))
	       reductors
	       formals)
	    ;; if the result type of the inlined function is not *_* or
	    ;; *obj* we use a local variable in order to ensure that the
	    ;; type checking of the result will be implemented even after
	    ;; inlining. This fixes a bug of the version 1.9b
 	    (if (or (eq? type *_*)
		    (eq? type *obj*)
		    (eq? type (node-type alpha-body)))
		ibody
		(let ((var (make-local-svar (gensym 'res) type)))
		   (instantiate::let-var
		      (loc loc)
		      (type type)
		      (side-effect (side-effect? ibody))
		      (bindings (list (cons var ibody)))
		      (body (instantiate::var
			       (loc loc)
			       (type type)
			       (variable var))))))))))

