;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/free.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan 27 14:20:15 1995                          */
;*    Last change :  Wed May 31 10:38:56 2017 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The search of free variables.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_free
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    type_type
	    type_cache
	    ast_var
	    ast_local
	    ast_node
	    ast_sexp
	    ast_glo-def
	    ast_env
	    globalize_ginfo
	    globalize_node
	    engine_param
	    globalize_global-closure)
   (export  (get-free-vars      ::node     ::local)
	    (free-from          <local>**  ::local)
	    (the-closure        ::variable <loc>)
	    (the-global-closure ::global   <loc>)))

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
	 ((svar/Ginfo? info)
	  (svar/Ginfo-free-mark-set! info *round*))
	 ((sfun/Ginfo? info)
	  (sfun/Ginfo-free-mark-set! info *round*))
	 ((sexit/Ginfo? info)
	  (sexit/Ginfo-free-mark-set! info *round*)))))

;*---------------------------------------------------------------------*/
;*    bind-variable! ...                                               */
;*---------------------------------------------------------------------*/
(define (bind-variable! local::local integrator::local)
   (let ((finfo (local-value integrator)))
      (sfun/Ginfo-bound-set! finfo (cons local (sfun/Ginfo-bound finfo)))
      (mark-variable! local)))

;*---------------------------------------------------------------------*/
;*    free-variable? ...                                               */
;*---------------------------------------------------------------------*/
(define (free-variable? local)
   (when (local? local)
      (let ((info (local-value local)))
	 (cond
	    ((svar/Ginfo? info)
	     (not (eq? (svar/Ginfo-free-mark info) *round*)))
	    ((sfun/Ginfo? info)
	     (not (eq? (sfun/Ginfo-free-mark info) *round*)))
	    ((sexit/Ginfo? info)
	     (not (eq? (sexit/Ginfo-free-mark info) *round*)))
	    (else
	     (error "free-variable?"
		"Unknown variable type"
		(cons local (shape local))))))))
       
;*---------------------------------------------------------------------*/
;*    get-free-vars ...                                                */
;*    -------------------------------------------------------------    */
;*    We don't need a stack, we just have to mark all bound and free   */
;*    variables.                                                       */
;*---------------------------------------------------------------------*/
(define (get-free-vars node::node integrator::local)
   (trace (globalize 5) "get-free-vars [" (shape integrator) "] : "
	  #\Newline)
   (let ((free (sfun/Ginfo-free (local-value integrator))))
      (if (or (null? free) (pair? free))
	  free
	  (let ((free (internal-get-free-vars! node integrator)))
	     (sfun/Ginfo-free-set! (local-value integrator) free)
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
   (trace (globalize 5) "internal-get-free-vars [" (shape integrator)
      "] : node=" (shape node) #\Newline)
   (set! *round* (+fx *round* 1))
   ;; we mark integrator and its formals
   (set! *integrator* integrator)
   (bind-variable! integrator integrator)
   (bind-variable! (the-closure integrator #f) integrator)
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
      (trace (globalize 5) "node-free ::var var=" (shape node)
	 " global=" (global? variable) " free="
	 (free-variable? variable) #\Newline)
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
   (with-access::closure node (variable)
      (let ((var (the-closure variable #f)))
	 (cond
	    ((global? var)
	     free)
	    ((free-variable? var)
	     (mark-variable! var)
	     (cons var free))
	    (else
	     free)))))

;*---------------------------------------------------------------------*/
;*    node-free ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-free node::sequence free)
   (with-access::sequence node (nodes)
      (node-free* nodes free)))

;*---------------------------------------------------------------------*/
;*    node-free ::sync ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-free node::sync free)
   (with-access::sync node (body mutex prelock)
      (node-free body (node-free prelock (node-free mutex free)))))

;*---------------------------------------------------------------------*/
;*    node-free ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-free node::app free)
   (with-access::app node (fun args)
      (let ((free (let ((var (var-variable (app-fun node))))
		     (cond
			((global? var)
			 free)
			((and (local/Ginfo-escape? var) (free-variable? var))
			 (mark-variable! var)
			 (cons (the-closure var (node-loc node)) free))
			(else
			 free)))))
	 (node-free* args free))))
 
;*---------------------------------------------------------------------*/
;*    node-free ::app-ly ...                                           */
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
;*    node-free ::switch ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-free node::switch free)
   (with-access::switch node (clauses test)
      (let loop ((clauses clauses)
		 (free    free))
	 (if (null? clauses)
	     (node-free test free)
	     (loop (cdr clauses) (node-free (cdr (car clauses)) free))))))

;*---------------------------------------------------------------------*/
;*    node-free ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-free node::let-fun free)
   (with-access::let-fun node (body locals)
      (for-each (lambda (f)
		   ;; we mark as bound the function
		   (bind-variable! f *integrator*)
		   ;; and the closure if there is one.
		   (if (local/Ginfo-escape? f)
		       (bind-variable! (the-local-closure f (node-loc node))
				       *integrator*)))
		locals)
      (let liip ((lcls locals)
		 (free free))
	 (if (null? lcls)
	     (node-free body free)
	     (let* ((local (car lcls))
		    (fun   (local-value local)))
		(for-each (lambda (l)
			     (bind-variable! l *integrator*))
			  (sfun-args fun))
		(liip (cdr lcls)
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
;*    the-closure ...                                                  */
;*---------------------------------------------------------------------*/
(define (the-closure variable::variable loc)
   (if (global? variable)
       (the-global-closure variable loc)
       (the-local-closure variable loc)))

;*---------------------------------------------------------------------*/
;*    the-global-closure ...                                           */
;*---------------------------------------------------------------------*/
(define (the-global-closure global::global loc)

   (define (global-alias-closure g)
      (with-access::global g (alias id module)
	 (when alias
	    (let ((ag (find-global alias module)))
	       (when (global? ag)
		  (the-global-closure ag loc))))))
   
   (let ((closure (fun-the-closure (global-value global))))
      (cond
	 ((global? closure)
	  closure)
	 ((global-alias-closure global)
	  =>
	  (lambda (g)
	     (fun-the-closure-set! (global-value global) g)
	     g))
	 (else
	  (let* ((gloclo   (make-global-closure global))
		 (arity    (fun-arity (global-value global)))
		 (make-clo (if (<fx arity 0)
			       'make-va-procedure
			       'make-fx-procedure))
		 (node     (sexp->node `(,make-clo
					 ,(instantiate::var
					     (loc loc)
					     (type *_*)
					     (variable gloclo))
					 ,(instantiate::literal
					     (loc loc)
					     (type *_*)
					     (value arity))
					 ,(instantiate::literal
					     (loc loc)
					     (type *_*)
					     (value 0)))
				       '()
				       loc
				       'value))
		 (closure  (def-global-scnst! (symbol-append
						 (if (global-alias global)
						     (global-alias global)
						     (global-id global))
						 '-env::procedure)
			      (global-module global)
			      node
			      (if (sfun? (global-value global))
				  (case (sfun-class (global-value global))
				     ((sgfun)
				      'sgfun)
				     (else
				      'sfun))
				  'sfun)
			      loc)))	     
	     (global-library-set! closure (global-library global))
	     (global-import-set! closure (global-import global))
	     (fun-the-closure-set! (global-value global) closure)
	     ;; this next setting is require by the Cfa pass
	     ;; (file Cfa/closure.scm) when light and extra-light
	     ;; closures are built.
	     (fun-the-closure-set! (global-value gloclo) closure)
	     (trace (globalize 5) "J'ai pluge pour la globale: "
		    (shape closure) " l'ast: "
		    (shape (scnst-node (global-value closure)))
		    #\Newline)
	     closure)))))

;*---------------------------------------------------------------------*/
;*    the-local-closure ...                                            */
;*---------------------------------------------------------------------*/
(define (the-local-closure local loc)
   (let ((info (local-value local)))
      (if (local? (sfun/Ginfo-the-closure info))
	  (sfun/Ginfo-the-closure info)
	  (let ((closure (make-local-svar (local-id local) *procedure*)))
	     (local-user?-set! closure (local-user? local))
	     (widen!::svar/Ginfo (local-value closure))
	     (widen!::local/Ginfo closure)
	     (sfun/Ginfo-the-closure-set! info closure)
	     closure))))
      
;*---------------------------------------------------------------------*/
;*    free-from ...                                                    */
;*---------------------------------------------------------------------*/
(define (free-from sets integrator)
   (set! *round* (+fx *round* 1))
   ;; we re-mark all bound variables.
   (let ((finfo (local-value integrator)))
      (trace (globalize 5)
	     "   bound(" (shape integrator) ") : "
	     (shape (sfun/Ginfo-bound finfo))
	     #\Newline
	     "   sets: " (shape sets)
	     #\Newline)
      (for-each mark-variable! (sfun/Ginfo-bound finfo)))
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
   
