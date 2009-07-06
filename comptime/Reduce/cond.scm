;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/cond.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Tue Sep 21 21:53:23 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The conditional reduction                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_cond
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    ast_var
	    ast_node)
   (export  (reduce-conditional! globals)))

;*---------------------------------------------------------------------*/
;*    reduce-conditional! ...                                          */
;*---------------------------------------------------------------------*/
(define (reduce-conditional! globals)
   (verbose 2 #"      conditional expression ")
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun))) 
		   (sfun-body-set! fun (node-cond! node))
		   #unspecified))
	     globals)
   (verbose 2 "(reduced : " *cond-reduced* #\) #\newline)
   globals)

;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *cond-reduced* 0)

;*---------------------------------------------------------------------*/
;*    node-cond! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node-cond!::node node::node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    node-cond! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    node-cond! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::var)
   node)

;*---------------------------------------------------------------------*/
;*    node-cond! ::closure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::closure)
   node)

;*---------------------------------------------------------------------*/
;*    node-cond! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::sequence)
   (with-access::sequence node (nodes)
      (node-cond*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::app)
   (with-access::app node (args)
      (node-cond*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (node-cond! fun))
      (set! arg (node-cond! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (node-cond! fun))
      (node-cond*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::extern)
   (with-access::extern node (expr*)
      (node-cond*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::cast)
   (with-access::cast node (arg)
      (node-cond! arg)
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::setq)
   (with-access::setq node (var value)
      (set! value (node-cond! value))
      (set! var (node-cond! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (node-cond! test))
       (set! true (node-cond! true))
       (set! false (node-cond! false))
       (if (atom? test)
	   (begin
	      (set! *cond-reduced* (+fx 1 *cond-reduced*))
	      (trace (reduce 2) "Je reduis le cond: " (shape node) #\Newline)
	      (if (atom-value test)
		  true
		  false))
	   node)))

;*---------------------------------------------------------------------*/
;*    node-cond! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::fail)
   (with-access::fail node (type proc msg obj)
      (set! proc (node-cond! proc))
      (set! msg (node-cond! msg))
      (set! obj (node-cond! obj))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::select)
   (with-access::select node (clauses test)
      (set! test (node-cond! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (node-cond! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (node-cond! (sfun-body fun)))))
		locals)
      (set! body (node-cond! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (node-cond! (cdr binding))))
		bindings)
      (set! body (node-cond! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (node-cond! body))
      (set! var (node-cond! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (node-cond! exit))
      (set! value (node-cond! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::make-box)
   (with-access::make-box node (value)
      (set! value (node-cond! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (node-cond! var))
      (set! value (node-cond! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-cond! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (node-cond! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-cond*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-cond*! node*)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (node-cond! (car node*)))
	     (loop (cdr node*))))))

