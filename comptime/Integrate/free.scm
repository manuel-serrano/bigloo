;*=====================================================================*/
;*    serrano/prgm/project/bigloo2.3/comptime/Integrate/free.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 27 14:20:15 1995                          */
;*    Last change :  Thu Jul 13 11:24:48 2000 (serrano)                */
;*    Copyright   :  1995-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The search of free variables.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_free
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    ast_var
	    ast_local
	    ast_node
	    ast_sexp
	    ast_glo-def
	    integrate_info
	    integrate_node
	    engine_param)
   (export  (get-free-vars ::node  ::local)
	    (free-from     local** ::local)))

;*---------------------------------------------------------------------*/
;*    *round* ...                                                      */
;*---------------------------------------------------------------------*/
(define *round* 0)

;*---------------------------------------------------------------------*/
;*    mark-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define (mark-variable! local::local)
   (let ((info (local-value local)))
      (cond
	 ((svar/Iinfo? info)
	  (svar/Iinfo-f-mark-set! info *round*))
	 ((sexit/Iinfo? info)
	  (sexit/Iinfo-f-mark-set! info *round*)))))

;*---------------------------------------------------------------------*/
;*    bind-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define (bind-variable! local::local integrator::local)
   (let ((finfo (local-value integrator)))
      (sfun/Iinfo-bound-set! finfo (cons local (sfun/Iinfo-bound finfo)))
      (mark-variable! local)))

;*---------------------------------------------------------------------*/
;*    free-variable? ...                                               */
;*---------------------------------------------------------------------*/
(define (free-variable? local)
   (let ((info (local-value local)))
      (cond
	 ((svar/Iinfo? info)
	  (not (eq? (svar/Iinfo-f-mark info) *round*)))
	 ((sexit/Iinfo? info)
	  (not (eq? (sexit/Iinfo-f-mark info) *round*)))
	 (else
	  (error "free-variable?"
		 "Unknown variable type"
		 (cons local (shape local)))))))

;*---------------------------------------------------------------------*/
;*    get-free-vars ...                                                */
;*    -------------------------------------------------------------    */
;*    We don't need a stack, we just have to mark all bound and free   */
;*    variables.                                                       */
;*---------------------------------------------------------------------*/
(define (get-free-vars node::node integrator::local)
   (trace (integrate 2) "get-free-vars [" (shape integrator) "] : "
	  #\Newline)
   (let ((free (sfun/Iinfo-free (local-value integrator))))
      (if (or (null? free) (pair? free))
	  free
	  (let ((free (internal-get-free-vars! node integrator)))
	     (sfun/Iinfo-free-set! (local-value integrator) free)
	     free))))

;*---------------------------------------------------------------------*/
;*    *integrator* ...                                                 */
;*---------------------------------------------------------------------*/
(define *integrator* #unspecified)

;*---------------------------------------------------------------------*/
;*    internal-get-free-vars! ...                                      */
;*    -------------------------------------------------------------    */
;*    This function, makes a side effect on integrator because,        */
;*    it maintains `bound' variables list.                             */
;*---------------------------------------------------------------------*/
(define (internal-get-free-vars! node::node integrator::local)
   (set! *round* (+fx *round* 1))
   ;; we mark integrator and its formals
   (set! *integrator* integrator)
   (for-each (lambda (l) (bind-variable! l integrator))
	     (sfun-args (local-value integrator)))
   ;; we can now walk across the body
   (node-free node '()))

;*---------------------------------------------------------------------*/
;*    node-free ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (node-free node::node free)
   free)

;*---------------------------------------------------------------------*/
;*    node-free ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-free node::var free)
   (with-access::var node (variable)
      (cond
	 ((global? variable)
	  free)
	 ((free-variable? variable)
	  (mark-variable! variable)
	  (cons variable free))
	 (else
	  free))))

;*---------------------------------------------------------------------*/
;*    node-free ::closure ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-free node::closure free)
   (internal-error "node-free" "Unexepected `closure' node" (shape node)))

;*---------------------------------------------------------------------*/
;*    node-free ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-free node::sequence free)
   (with-access::sequence node (nodes)
      (node-free* nodes free)))

;*---------------------------------------------------------------------*/
;*    node-free ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-free node::app free)
   (with-access::app node (fun args)
      (node-free* args free)))
 
;*---------------------------------------------------------------------*/
;*    node-free ::app-ly ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-free node::app-ly free)
   (with-access::app-ly node (fun arg)
      (node-free fun (node-free arg free))))

;*---------------------------------------------------------------------*/
;*    node-free ::funcall ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-free node::funcall free)
   (with-access::funcall node (fun args)
      (node-free fun (node-free* args free))))

;*---------------------------------------------------------------------*/
;*    node-free ::extern ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-free node::extern free)
   (with-access::extern node (expr*)
      (node-free* expr* free)))

;*---------------------------------------------------------------------*/
;*    node-free ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-free node::cast free)
   (with-access::cast node (arg)
      (node-free arg free)))

;*---------------------------------------------------------------------*/
;*    node-free ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-free node::setq free)
   (with-access::setq node (var value)
      (node-free var (node-free value free))))

;*---------------------------------------------------------------------*/
;*    node-free ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-free node::conditional free)
   (with-access::conditional node (test true false)
      (node-free test (node-free true (node-free false free)))))

;*---------------------------------------------------------------------*/
;*    node-free ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-free node::fail free)
   (with-access::fail node (proc msg obj)
      (node-free proc (node-free msg (node-free obj free)))))

;*---------------------------------------------------------------------*/
;*    node-free ::select ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-free node::select free)
   (with-access::select node (clauses test)
      (let loop ((clauses clauses)
		 (free    free))
	 (if (null? clauses)
	     (node-free test free)
	     (loop (cdr clauses) (node-free (cdr (car clauses)) free))))))

;*---------------------------------------------------------------------*/
;*    node-free ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-free node::let-fun free)
   (with-access::let-fun node (body)
      ;; we mark all locals functions
      (let liip ((locals (let-fun-locals node))
		 (free   free))
	 (if (null? locals)
	     (node-free body free)
	     (let* ((local (car locals))
		    (fun   (local-value local)))
		(for-each (lambda (l) (bind-variable! l *integrator*))
			  (sfun-args fun))
		(liip (cdr locals)
		      (node-free (sfun-body fun) free)))))))

;*---------------------------------------------------------------------*/
;*    node-free ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-free node::let-var free)
   (with-access::let-var node (body bindings)
      (let loop ((bindings bindings)
		 (free     free))
	 (if (null? bindings)
	     (node-free body free)
	     (begin
		(bind-variable! (car (car bindings)) *integrator*)
		(loop (cdr bindings)
		      (node-free (cdr (car bindings)) free)))))))

;*---------------------------------------------------------------------*/
;*    node-free ::set-ex-it ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-free node::set-ex-it free)
   (with-access::set-ex-it node (var body)
      (bind-variable! (var-variable var) *integrator*)
      (node-free body free)))

;*---------------------------------------------------------------------*/
;*    node-free ::jump-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-free node::jump-ex-it free)
   (with-access::jump-ex-it node (exit value)
      (node-free exit (node-free value free))))

;*---------------------------------------------------------------------*/
;*    node-free ::make-box ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-free node::make-box free)
   (with-access::make-box node (value)
      (node-free value free)))

;*---------------------------------------------------------------------*/
;*    node-free ::box-ref ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-free node::box-ref free)
   (with-access::box-ref node (var)
      (node-free var free)))

;*---------------------------------------------------------------------*/
;*    node-free ::box-set! ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-free node::box-set! free)
   (with-access::box-set! node (var value)
      (node-free var (node-free value free))))

;*---------------------------------------------------------------------*/
;*    node-free* ...                                                   */
;*---------------------------------------------------------------------*/
(define (node-free* node* free)
   (let loop ((node* node*)
	      (free  free))
      (if (null? node*)
	  free
	  (loop (cdr node*)
		(node-free (car node*) free)))))
		    
;*---------------------------------------------------------------------*/
;*    free-from ...                                                    */
;*---------------------------------------------------------------------*/
(define (free-from sets integrator)
   (set! *round* (+fx *round* 1))
   ;; we re-mark all bound variables.
   (let ((finfo (local-value integrator)))
      (trace (integrate 2)
	     "   bound(" (shape integrator) ") : "
	     (shape (sfun/Iinfo-bound finfo))
	     #\Newline
	     "   sets: " (shape sets)
	     #\Newline)
      (for-each mark-variable! (sfun/Iinfo-bound finfo)))
   ;; then, we scan sets
   (map (lambda (set)
	   (let loop ((set set)
		      (res '()))
	      (cond
		 ((null? set)
		  res)
		 ((free-variable? (car set))
		  (loop (cdr set) (cons (car set) res)))
		 (else
		  (loop (cdr set) res)))))
	sets))
   
