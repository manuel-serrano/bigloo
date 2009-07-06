;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Callcc/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 28 10:50:15 1995                          */
;*    Last change :  Sat Jan 31 13:47:30 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    When compiling for call/cc we put all written local variables    */
;*    in cells.                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module callcc_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch")
   (import  tools_error
	    tools_shape
	    type_cache
	    ast_local
	    (*compiler-debug* engine_param))
   (static  (wide-class local/cell::local))
   (export  (callcc-walk! globals)))
 
;*---------------------------------------------------------------------*/
;*    callcc-walk! ...                                                 */
;*---------------------------------------------------------------------*/
(define (callcc-walk! globals)
   (pass-prelude "Callcc")
   ;; -g3 and -call/cc are incompatible
   (if (>=fx *compiler-debug* 2)
       (set! *compiler-debug* 1))
   (for-each callcc-fun! globals)
   (pass-postlude globals))

;*---------------------------------------------------------------------*/
;*    callcc-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define (callcc-fun! var)
   (let* ((fun    (variable-value var))
	  (body   (sfun-body fun))
	  (celled (celled-bindings (sfun-args fun))))
      ;; we set alpha-fast slot
      (for-each (lambda (w.b)
		   (local-fast-alpha-set! (car w.b) (cdr w.b)))
		celled)
      (sfun-body-set! fun (cell-formals celled (callcc! body)))
      ;; we remove alpha-fast slots
      (for-each (lambda (w.b)
		   (local-fast-alpha-set! (car w.b) #unspecified))
		celled)))

;*---------------------------------------------------------------------*/
;*    celled-bindings ...                                              */
;*---------------------------------------------------------------------*/
(define (celled-bindings formals)
   (let loop ((celled   '())
	      (formals  formals))
      (cond
	 ((null? formals)
	  celled)
	 ((not (celled? (car formals)))
	  (loop celled (cdr formals)))
	 (else
	  (let* ((var (make-local-svar (local-id (car formals)) *obj*))
		 (o.n (cons (car formals) var)))
	     (widen!::local/cell var)
	     (loop (cons o.n celled) (cdr formals)))))))

;*---------------------------------------------------------------------*/
;*    cell-formals ...                                                 */
;*---------------------------------------------------------------------*/
(define (cell-formals::node celled body::node)
   (if (null? celled)
       body
       (let ((loc (node-loc body)))
	  (instantiate::let-var
	     (loc loc)
	     (body body)
	     (type *_*)
	     (bindings (map (lambda (o.n)
			       (cons (cdr o.n)
				     (a-make-cell (instantiate::var
						     (type *_*)
						     (loc loc)
						     (variable (car o.n)))
						  (car o.n))))
			    celled))))))

;*---------------------------------------------------------------------*/
;*    a-make-cell ...                                                  */
;*---------------------------------------------------------------------*/
(define (a-make-cell::make-box node::node variable::variable)
   (with-access::node node (loc)
      (local-access-set! variable 'cell-callcc)
      (widen!::local/cell variable)
      (instantiate::make-box (type *_*) (loc loc) (value node))))
   
;*---------------------------------------------------------------------*/
;*    celled? ...                                                      */
;*---------------------------------------------------------------------*/
(define (celled?::bool var::variable)
   (or (local/cell? var)
       (and (eq? (local-access var) 'write)
	    (or (eq? (local-type var) *_*)
		(bigloo-type? (local-type var))))))

;*---------------------------------------------------------------------*/
;*    callcc! ...                                                      */
;*---------------------------------------------------------------------*/
(define-generic (callcc!::node node::node))

;*---------------------------------------------------------------------*/
;*    callcc! ::atom ...                                               */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::kwote ...                                              */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::var ...                                                */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::var)
   (let ((var (var-variable node)))
      (let loop ((var   var)
		 (alpha (variable-fast-alpha var)))
	 (if (local? alpha)
	     (begin
		(var-variable-set! node alpha)
		(loop alpha (variable-fast-alpha alpha)))
	     (cond
		((local? alpha)
		 (var-variable-set! node alpha)
		 (callcc! node))
		((global? var)
		 node)
		((not (celled? var))
		 node)
		(else
		 (instantiate::box-ref
		    (type *_*)
		    (loc (node-loc node))
		    (var node))))))))

;*---------------------------------------------------------------------*/
;*    callcc! ::sequence ...                                           */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::sequence)
   (callcc*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::app ...                                                */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::app)
   (with-access::app node (args)
      (callcc*! args)
      node))
 
;*---------------------------------------------------------------------*/
;*    callcc! ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (callcc! fun))
      (set! arg (callcc! arg))
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::funcall ...                                            */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (callcc! fun))
      (callcc*! args)
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::extern ...                                             */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::extern)
   (callcc*! (extern-expr* node))
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::cast ...                                               */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::cast)
   (callcc! (cast-arg node))
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::setq ...                                               */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::setq)
   (setq-value-set! node (callcc! (setq-value node)))
   (let ((var (var-variable (setq-var node))))
      (let loop ((var   var)
		 (alpha (variable-fast-alpha var)))
	 (if (local? alpha)
	     (begin
		(var-variable-set! (setq-var node) alpha)
		(loop alpha (variable-fast-alpha alpha)))
	     (cond
		((global? var)
		 node)
		((not (celled? var))
		 node)
		(else
		 (let ((a-var (make-local-svar 'aux *obj*))
		       (loc   (node-loc node)))
		    (instantiate::let-var
		       (type     *_*)
		       (loc      loc)
		       (bindings (list (cons a-var (setq-value node))))
		       (body     (instantiate::box-set!
				    (type *_*)
				    (loc loc)
				    (var (setq-var node))
				    (value (instantiate::var
					      (type *_*)
					      (loc loc)
					      (variable a-var)))))))))))))

;*---------------------------------------------------------------------*/
;*    callcc! ::conditional ...                                        */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (callcc! test))
       (set! true (callcc! true))
       (set! false (callcc! false))
       node))

;*---------------------------------------------------------------------*/
;*    callcc! ::fail ...                                               */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::fail)
   (with-access::fail node (proc msg obj)
      (set! proc (callcc! proc))
      (set! msg (callcc! msg))
      (set! obj (callcc! obj))
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::select ...                                             */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::select)
   (with-access::select node (clauses test)
      (set! test (callcc! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (callcc! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::let-fun ...                                            */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each callcc-fun! locals)
      (set! body (callcc! body))
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::let-var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::let-var)
   (with-access::let-var node (body bindings)
      (set! body (callcc! body))
      (for-each (lambda (binding)
		   (let ((var (car binding))
			 (val (cdr binding)))
		      (set-cdr! binding (callcc! val))
		      (if (celled? var)
			  (begin
			     (local-type-set! var *obj*)
			     (set-cdr! binding
				       (a-make-cell (cdr binding) var))))))
		bindings)
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::set-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::set-ex-it)
   (set-ex-it-body-set! node (callcc! (set-ex-it-body node)))
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::jump-ex-it ...                                         */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (callcc! exit)) 
      (set! value (callcc! value))
      node))

;*---------------------------------------------------------------------*/
;*    callcc! ::make-box ...                                           */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::make-box)
   (make-box-value-set! node (callcc! (make-box-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::box-ref ...                                            */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::box-ref)
   node)

;*---------------------------------------------------------------------*/
;*    callcc! ::box-set! ...                                           */
;*---------------------------------------------------------------------*/
(define-method (callcc! node::box-set!)
   (with-access::box-set! node (value)
      (set! value (callcc! value))
      node))

;*---------------------------------------------------------------------*/
;*    callcc*! ...                                                     */
;*---------------------------------------------------------------------*/
(define (callcc*! node*)
   (if (null? node*)
       'done
       (begin
	  (set-car! node* (callcc! (car node*)))
	  (callcc*! (cdr node*)))))
   
   




