;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/cse.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Thu Dec  5 09:16:04 2002 (serrano)                */
;*    Copyright   :  1995-2002 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reduction of type checks.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_cse
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    coerce_coerce
	    effect_effect
	    ast_var
	    ast_node
	    ast_lvtype
	    reduce_same)
   (export  (reduce-cse! globals)
	    (generic node-cse! ::node ::obj)))

;*---------------------------------------------------------------------*/
;*    reduce-cse! ...                                                  */
;*---------------------------------------------------------------------*/
(define (reduce-cse! globals)
   (verbose 2 #"      cse                    ")
   (set! *cse-removed* 0)
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun)))
		   (sfun-body-set! fun (multiple-value-bind (_ node)
					   (node-cse! node '())
					   node))
		   #unspecified))
	     globals)
   (verbose 2 "(removed : " *cse-removed* #\) #\newline)
   globals)

;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *cse-removed* 0)

;*---------------------------------------------------------------------*/
;*    node-cse! ...                                                    */
;*    -------------------------------------------------------------    */
;*    Stack is a stack of variable aliases which is initially empty.   */
;*---------------------------------------------------------------------*/
(define-generic (node-cse! node::node stack))

;*---------------------------------------------------------------------*/
;*    node-cse! ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::atom stack)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-cse! ::kwote ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::kwote stack)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-cse! ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::var stack)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-cse! ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::closure stack)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-cse! ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::sequence stack)
   (with-access::sequence node (nodes)
      (values (node-cse*! nodes stack) node)))

;*---------------------------------------------------------------------*/
;*    node-cse! ::app-ly ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::app-ly stack)
   (with-access::app-ly node (fun arg)
      (multiple-value-bind (reset nfun)
	 (node-cse! fun stack)
	 (set! fun nfun)
	 (multiple-value-bind (reset' narg)
	    (node-cse! arg (if reset '() stack))
	    (set! arg narg)
	    (values (or reset reset') node)))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::funcall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::funcall stack)
   (with-access::funcall node (args)
      (let ((reset' (node-cse*! args stack)))
	 (values reset' node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::extern ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::extern stack)
   (with-access::extern node (expr*)
      (let ((reset' (node-cse*! expr* stack)))
	 (values reset' node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::cast stack)
   (with-access::cast node (arg)
      (multiple-value-bind (reset narg)
	 (node-cse! arg stack)
	 (set! arg narg)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::setq stack)
   (with-access::setq node (var value)
      (multiple-value-bind (reset nvalue)
	 (node-cse! value stack)
	 (set! value nvalue)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::conditional stack)
   (with-access::conditional node (test true false)
      (multiple-value-bind (reset ntest)
	 (node-cse! test stack)
	 (set! test ntest)
	 (let ((stack' (if reset '() stack)))
	    (multiple-value-bind (reset' ntrue)
	       (node-cse! true stack')
	       (set! true ntrue)
	       (multiple-value-bind (reset'' nfalse)
		  (node-cse! false stack')
		  (set! false nfalse)
		  (values (or reset reset' reset'') node)))))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::fail stack)
   (with-access::fail node (proc msg obj)
      (multiple-value-bind (reset nproc)
	 (node-cse! proc stack)
	 (set! proc nproc)
	 (let ((stack' (if reset '() stack)))
	    (multiple-value-bind (reset' nmsg)
	       (node-cse! msg stack')
	       (set! msg nmsg)
	       (let ((stack'' (if reset' '() stack')))
		  (multiple-value-bind (reset'' nobj)
		     (node-cse! obj stack'')
		     (set! obj nobj)
		     (values (or reset reset' reset'') node))))))))
   
;*---------------------------------------------------------------------*/
;*    node-cse! ::select ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::select stack)
   (with-access::select node (clauses test)
      (multiple-value-bind (reset ntest)
	 (node-cse! test stack)
	 (set! test ntest)
	 (let ((stack' (if reset '() stack)))
	    (let loop ((clauses clauses)
		       (reset   reset))
	       (if (null? clauses)
		   (values reset node)
		   (let ((clause (car clauses)))
		      (multiple-value-bind (reset' nclause)
			 (node-cse! (cdr clause) stack)
			 (set-cdr! clause nclause)
			 (loop (cdr clauses) (or reset reset'))))))))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::let-fun stack)
   (with-access::let-fun node (body locals)
      (multiple-value-bind (reset nbody)
	 (node-cse! body stack)
	 (set! body nbody)
	 (let loop ((locals locals)
		    (reset  reset))
	    (if (null? locals)
		(values reset node)
		(let* ((local (car locals))
		       (sfun  (local-value local)))
		   (multiple-value-bind (reset' nbody)
		      (node-cse! (sfun-body sfun) '())
		      (sfun-body-set! sfun nbody)
		      (loop (cdr locals) (or reset reset')))))))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::set-ex-it stack)
   (with-access::set-ex-it node (var body)
      (multiple-value-bind (reset nbody)
	 (node-cse! body '())
	 (set! body nbody)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::jump-ex-it stack)
   (with-access::jump-ex-it node (exit value)
      (multiple-value-bind (reset nexit)
	 (node-cse! exit stack)
	 (set! exit nexit)
	 (let ((stack' (if reset '() stack)))
	    (multiple-value-bind (reset' nvalue)
	       (node-cse! value stack')
	       (set! value nvalue)
	       (values (or reset reset') node))))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::make-box stack)
   (with-access::make-box node (value)
      (multiple-value-bind (reset nvalue)
	 (node-cse! value stack)
	 (set! value nvalue)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::box-set! stack)
   (with-access::box-set! node (var value)
      (multiple-value-bind (reset nvalue)
	 (node-cse! value stack)
	 (set! value nvalue)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::box-ref stack)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-cse! ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::app stack)
   (with-access::app node (fun args loc type)
      (let ((reset (node-cse*! args stack)))
	 (if (or reset (side-effect? node))
	     (begin
		(trace (reduce 2) "***cse: side-effect?: " (shape node)
		       #\Newline)
		(values #t node))
	     (let ((previous (find-stack node stack)))
		(cond
		   ((variable? previous)
		    (set! *cse-removed* (+fx 1 *cse-removed*))
		    (trace (reduce 2) "***cse: " (shape node) #\Newline)
		    (values stack (instantiate::var
				     (loc loc)
				     (type type)
				     (variable previous))))
		   (else
		    (values #f node))))))))

;*---------------------------------------------------------------------*/
;*    node-cse! ::let-var ...                                          */
;*    -------------------------------------------------------------    */
;*    Putting a `set!' in the right part of a let-binding makes        */
;*    the cse to loose everything it has in its stack.                 */
;*---------------------------------------------------------------------*/
(define-method (node-cse! node::let-var stack)
   (with-access::let-var node (body bindings loc type)
      (let loop ((bindings bindings)
		 (reset    #f)
		 (extend   '()))
	 (if (null? bindings)
	     (let ((stack' (if reset '() (append extend stack))))
		(multiple-value-bind (reset' nbody)
		   (node-cse! body stack')
		   (set! body nbody)
		   (cond
		      ((or reset reset')
		       (values #t node))
		      ((side-effect? node)
		       (values #f node))
		      (else
		       (let ((previous (find-stack node stack)))
			  (cond
			     ((variable? previous)
			      (set! *cse-removed* (+fx 1 *cse-removed*))
			      (trace (reduce 2) "***cse: " (shape node)
				     #\Newline)
			      (values stack' (instantiate::var
						(loc loc)
						(type type)
						(variable previous))))
			     (else
			      (values #f node))))))))
	     (let ((binding (car bindings)))
		(let ((var (car binding))
		      (val (cdr binding)))
		   (multiple-value-bind (reset' nval)
		      (node-cse! val stack)
		      (set-cdr! binding nval)
		      (cond
			 ((or reset reset')
			  (loop (cdr bindings)
				#t
				'()))
			 ((not (eq? (local-access var) 'read))
			  (loop (cdr bindings)
				#f
				extend))
			 ((or (app? val) (let-var? val))
			  (loop (cdr bindings)
				#f
				(cons (call val var) extend)))
			 (else
			  (loop (cdr bindings)
				#f
				extend))))))))))

;*---------------------------------------------------------------------*/
;*    node-cse*! ...                                                   */
;*---------------------------------------------------------------------*/
(define (node-cse*! node* stack)
   (let loop ((node*  node*)
	      (reset  #f)
	      (stack  stack))
      (cond
	 ((null? node*)
	  reset)
	 ((null? (cdr node*))
	  (multiple-value-bind (reset' node)
	     (node-cse! (car node*) stack)
	     (set-car! node* node)
	     (or reset reset')))
	 (else
	  (multiple-value-bind (reset' node)
	     (node-cse! (car node*) stack)
	     (set-car! node* node)
	     (if (or reset reset')
		 (loop (cdr node*) #t '())
		 (loop (cdr node*) #f stack)))))))

;*---------------------------------------------------------------------*/
;*    call ...                                                         */
;*---------------------------------------------------------------------*/
(define-struct call
   ;; the call itself
   node
   ;; the associated variable
   variable)

;*---------------------------------------------------------------------*/
;*    find-stack ...                                                   */
;*---------------------------------------------------------------------*/
(define (find-stack node::node stack)
   (trace (reduce 2) "***cse:find-stack: " (shape node) #\Newline)
   (let loop ((stack stack))
      (cond
	 ((null? stack)
	  #f)
	 ((same-node? node (call-node (car stack)) '())
	  (call-variable (car stack)))
	 (else
	  (loop (cdr stack))))))
	     
