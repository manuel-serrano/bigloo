;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Reduce/typec.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Fri Jan  5 19:08:29 2018 (serrano)                */
;*    Copyright   :  1995-2018 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reduction of type checks.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_typec
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    type_misc
	    type_coercion
	    coerce_coerce
	    effect_effect
	    ast_var
	    ast_node
	    ast_lvtype
	    ast_env
	    ast_dump
	    object_class)
   (export  (reduce-type-check! globals::pair-nil)))

;*---------------------------------------------------------------------*/
;*    reduce-type-check! ...                                           */
;*---------------------------------------------------------------------*/
(define (reduce-type-check! globals)
   (verbose 2 #"      type check             ")
   (set! *pair?* (find-global/module '$pair? 'foreign))
   (set! *null?* (find-global/module '$null? 'foreign))
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun))) 
		   (sfun-body-set! fun (node-typec! node))
		   #unspecified))
	     globals)
   (verbose 2 "(removed: " *type-checks-removed*
	    ", remaining: " *type-checks-remaining* #")\n")
   (set! *pair?* #f)
   (set! *null?* #f)
   globals)

;*---------------------------------------------------------------------*/
;*    *pair?* ...                                                      */
;*---------------------------------------------------------------------*/
(define *pair?* #f)
(define *null?* #f)

;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *type-checks-remaining* 0)
(define *type-checks-removed*   0)

;*---------------------------------------------------------------------*/
;*    node-typec! ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (node-typec!::node node::node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::atom ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::kwote ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::var ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::var)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::closure ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::closure)
   node)

;*---------------------------------------------------------------------*/
;*    node-typec! ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::sequence)
   (with-access::sequence node (nodes)
      (node-typec*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::sync)
   (with-access::sync node (body mutex prelock)
      (set! mutex (node-typec! mutex))
      (set! prelock (node-typec! prelock))
      (set! body (node-typec! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (node-typec! fun))
      (set! arg (node-typec! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (node-typec! fun))
      (node-typec*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::extern)
   (with-access::extern node (expr* type)
      (node-typec*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::cast)
   (with-access::cast node (arg)
      (node-typec! arg)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::setq)
   (with-access::setq node (var value)
      (set! value (node-typec! value))
      (set! var (node-typec! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::conditional)
   (with-access::conditional node (test true false)
      (set! test (node-typec! test))
      (set! true (node-typec! true))
      (set! false (node-typec! false))
      (when (pair-of-pair-nil? test)
	 ;; an expression
	 ;;     (if (pair? x) e1 e2)
	 ;; is replaced with
	 ;;     (if (null? x) e2 e1)
	 ;; if the compiler knows that x is ::pair-nil because testing null?
	 ;; is faster than testing pair?
	 (set-null-test! test)
	 (let ((otrue true))
	    (set! true false)
	    (set! false otrue)
	    (trace (reduce 3) "typec: inverting pair?/null? test: "
		   (shape node)
		   #\Newline)))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::fail)
   (with-access::fail node (type proc msg obj)
      (set! proc (node-typec! proc))
      (set! msg (node-typec! msg))
      (set! obj (node-typec! obj))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::switch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::switch)
   (with-access::switch node (clauses test)
      (set! test (node-typec! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (node-typec! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (node-typec! (sfun-body fun)))))
		locals)
      (set! body (node-typec! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (node-typec! (cdr binding))))
		bindings)
      (set! body (node-typec! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (node-typec! body))
      (set! var (node-typec! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (node-typec! exit))
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::retblock ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::retblock)
   (with-access::retblock node (body)
      (set! body (node-typec! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::return ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::return)
   (with-access::return node (value)
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::make-box)
   (with-access::make-box node (value)
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (node-typec! var))
      (set! value (node-typec! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec! ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (node-typec! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-typec*! ...                                                 */
;*---------------------------------------------------------------------*/
(define (node-typec*! node*)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (node-typec! (car node*)))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    node-typec! ::app ...                                            */
;*    -------------------------------------------------------------    */
;*    The subtype relationship between nil, pair, epair and pair-nil   */
;*    is hard coded into that function.                                */
;*    @label pair-nil subtyping@                                       */
;*    @ref ../../runtime/Llib/type.scm:pair-nil subtyping@             */
;*---------------------------------------------------------------------*/
(define-method (node-typec! node::app)

   (define (type-check-disjoint? typec typea)
      (cond
	 ((eq? typec typea)
	  #f)
	 ((eq? typea *_*)
	  #f)
	 ((and (bigloo-type? typec) (not (bigloo-type? typea)))
	  (not (find-coercer typea typec)))
	 ((or (type-less-specific? typec typea)
	      (type-less-specific? typea typec))
	  #f)
	 ((or (find-coercer typec typea) (find-coercer typea typec))
	  #f)
	 ((and (foreign-type? typec) (foreign-type? typea))
	  #f)
	 ((and (eq? typec *foreign*) (or (jclass? typea) (foreign-type? typea)))
	  #f)
	 (else
	  #t)))
   
   (define (check-type node typec typea)
      (with-access::app node (loc)
	 (let ((type  (get-type node #f)))
	    (cond
	       ((type-less-specific? typec typea)
		(set! *type-checks-removed* (+fx 1 *type-checks-removed*))
		(trace (reduce 3) "typec: reducing: "
		   (shape node) " => #t" #\Newline)
		(let ((node (coerce! (instantiate::literal
					(loc loc)
					(type type)
					(value #t))
			       #unspecified
			       type
			       #f)))
		   node))
	       ((type-check-disjoint? typec typea)
		(set! *type-checks-removed* (+fx 1 *type-checks-removed*))
		(trace (reduce 3) "typec: reducing: "
		   (shape node) " => #f (typec="
		   (shape typec) " " (foreign-type? typec)
		   " typea=" (shape typea) " " (jclass? typea) ")"
		   #\Newline)
		(let ((node (coerce! (instantiate::literal
					(loc loc)
					(type type)
					(value #f))
			       #unspecified
			       type
			       #f)))
		   node))
	       (else
		(set! *type-checks-remaining* (+fx 1 *type-checks-remaining*))
		node)))))
   
   (with-access::app node (fun args)
      (node-typec*! args)
      (let* ((var   (var-variable fun))
	     (typec (fun-predicate-of (variable-value var))))
	 (cond
	    ((and (pair? args)
		  (null? (cdr args))
		  (type? typec)
		  (not (side-effect? (car args))))
	     (check-type node typec (get-type (car args) #f)))
	    ((isa-of node)
	     =>
	     (lambda (typec)
		(check-type node typec (get-type (car args) #f))))
	    (else
	     node)))))

;*---------------------------------------------------------------------*/
;*    pair-of-pair-nil? ...                                            */
;*---------------------------------------------------------------------*/
(define (pair-of-pair-nil? node)
   (when (app? node)
      (with-access::app node (fun args loc)
	 (and (eq? (var-variable fun) *pair?*)
	      (pair? args)
	      (null? (cdr args))
	      (eq? (get-type (car args) #f) *pair-nil*)))))

;*---------------------------------------------------------------------*/
;*    set-null-test! ...                                               */
;*---------------------------------------------------------------------*/
(define (set-null-test! node::app)
   (with-access::app node (fun)
      (with-access::var fun (variable)
	 (set! variable *null?*))))
