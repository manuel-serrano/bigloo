;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Fxop/walk.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Thu Jul 27 13:08:26 2017 (serrano)                */
;*    Copyright   :  2010-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Optimize tagged binary operators by avoid useless tagging        */
;*    untagging operations. Typically, replaces:                       */
;*       (let ((z1::long (bint->long e1))                              */
;*             (z2::long e2))                                          */
;*          (long->bint (+ z1 z2)))                                    */
;*    with                                                             */
;*        ($addfx e1 (long->bint e2))                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module fxop_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    ast_walk
	    engine_param
	    backend_backend)
   (export  (fxop-walk! globals)
	    (init-fxop-cache!)))

;*---------------------------------------------------------------------*/
;*    fxop-walk! ...                                                   */
;*---------------------------------------------------------------------*/
(define (fxop-walk! globals)
   (pass-prelude "Fxop" init-fxop-cache!) 
   (for-each fxop-fun! globals)
   (pass-postlude globals clear-fxop-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *long->bint* #unspecified)
(define *bint->long* #unspecified)
(define *fxops* '())

;*---------------------------------------------------------------------*/
;*    init-fxop-cache! ...                                             */
;*---------------------------------------------------------------------*/
(define (init-fxop-cache!)
   (unless (pair? *fxops*)
      (set! *bint->long* (find-global '$bint->long 'foreign))
      (set! *long->bint* (find-global '$long->bint 'foreign))
      (set! *fxops*
	 (map (lambda (ut)
		 (cons (find-global (car ut) 'foreign)
		    (find-global (cadr ut) 'foreign)))
	    '((c-+fx $addfx)
	      (c--fx $subfx)
	      (c-<fx $ltfx)
	      (c-<=fx $lefx)
	      ($c->fx $c-gtfx)
	      ($c->=fx $c-gefx)
	      ($c-=fx $c-egfx)))))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-fxop-cache! ...                                            */
;*---------------------------------------------------------------------*/
(define (clear-fxop-cache!)
   (set! *long->bint* #f)
   (set! *bint->long* #f)
   (set! *fxops* '()))

;*---------------------------------------------------------------------*/
;*    fxop-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (fxop-fun! var)
   (enter-function (variable-id var))
   (let ((fun (variable-value var)))
      (sfun-body-set! fun (fxop! (sfun-body fun)))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    fxop! ...                                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (fxop! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    fxop! ::let-var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (fxop! node::let-var)
   
   (define (bint->long? expr)
      (when (isa? expr app)
	 (let ((v (var-variable (app-fun expr))))
	    (eq? v *bint->long*))))
   
   (define (long->bint? expr)
      (when (isa? expr app)
	 (let ((v (var-variable (app-fun expr))))
	    (eq? v *long->bint*))))
   
   (define (find-fxop expr)
      (when (isa? expr app)
	 (let ((v (var-variable (app-fun expr))))
	    (let ((c (assq v *fxops*)))
	       (when c (cdr c))))))
   
   (define (fxop-double-int-letvar? bindings body)
      ;; true iff:
      ;;   - there are two arguments
      ;;   - at least one is a conversion bint->long
      ;;   - the return is a known fxop converted to bint
      (and (=fx (length bindings) 2)
	   (or (or (bint->long? (cdar bindings))
		   (eq? (variable-type (caar bindings)) *bint*)
		   (eq? (variable-type (caar bindings)) *long*))
	       (or (bint->long? (cdadr bindings))
		   (eq? (variable-type (caadr bindings)) *bint*)
		   (eq? (variable-type (caadr bindings)) *long*)))
	   (isa? body app)
	   (long->bint? body)
	   (find-fxop (car (app-args body)))))

   (define (fxop-simple-int-letvar? bindings body)
      ;; true iff:
      ;;   - there is only one variable
      ;;   - at least one is a conversion bint->long
      ;;   - the return is a known fxop converted to bint
      (and (=fx (length bindings) 1)
	   (bint->long? (cdar bindings))
	   (isa? body app)
	   (long->bint? body)
	   (find-fxop (car (app-args body)))))

   (define (fxop-bool-letvar? bindings body)
      ;; true iff:
      ;;   - there are two arguments
      ;;   - both are conversions bint->long
      ;;   - the operator is known
      (and (app? body)
	   (=fx (length bindings) 2)
	   (and (bint->long? (cdar bindings))
		(bint->long? (cdadr bindings)))
	   (find-fxop (car (app-args body)))))

   (define (bint::node expr::node)
      (with-access::node expr (type)
	 (cond
	    ((eq? type *bint*)
	     expr)
	    ((bint->long? expr)
	     (car (app-args expr)))
	    (else
	     (instantiate::app
		(type *bint*)
		(fun (instantiate::var
			(variable *long->bint*)
			(type *bint*)))
		(args (list expr)))))))
		
   (define (tag-double-int-fxop op node)
      (with-access::let-var node (loc type bindings)
	 (instantiate::app
	    (type *bint*)
	    (fun (instantiate::var
		    (variable op)
		    (type *bint*)))
	    (args (list
		     (bint (cdar bindings))
		     (bint (cdadr bindings)))))))

   (define (tag-simple-int-fxop op node)
      (with-access::let-var node (loc type bindings)
	 (let ((call (car (app-args (let-var-body node)))))
	    (with-access::app call (fun args)
	       (instantiate::app
		  (type *bint*)
		  (fun (instantiate::var
			  (variable op)
			  (type *bint*)))
		  (args (list
			   (if (and (var? (car args))
				    (eq? (var-variable (car args))
				       (caar bindings)))
			       (bint (cdar bindings))
			       (bint (car args)))
			   (if (and (var? (cadr args))
				    (eq? (var-variable (cadr args))
				       (caar bindings)))
			       (bint (cdar bindings))
			       (bint (cadr args))))))))))

   (define (tag-bool-fxop op node)
      (with-access::let-var node (loc type bindings)
	 (instantiate::app
	    (type *bool*)
	    (fun (instantiate::var
		    (variable op)
		    (type (variable-type op))))
	    (args (list
		     (bint (cdar bindings))
		     (bint (cdadr bindings)))))))

   (with-access::let-var node (bindings body type)
      (cond
	 ((and (eq? type *bint*) (fxop-double-int-letvar? bindings body))
	  =>
	  (lambda (op)
	     (tag-double-int-fxop op node)))
	 ((and (eq? type *bint*) (fxop-simple-int-letvar? bindings body))
	  =>
	  (lambda (op)
	     (tag-simple-int-fxop op node)))
	 ((and (eq? type *bool*) (fxop-bool-letvar? bindings body))
	  =>
	  (lambda (op)
	     (tag-bool-fxop op node)))
	 (else
	  (call-default-walker)))))
