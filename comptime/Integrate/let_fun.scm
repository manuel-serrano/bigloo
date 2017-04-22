;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Integrate/let_fun.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 09:38:46 1995                          */
;*    Last change :  Fri Apr 21 18:43:59 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    This module implements a function which remove displaced         */
;*    local functions and which adds the integrated ones.              */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_let-fun
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    ast_var
	    ast_node
	    integrate_info)
   (export  (displace-let-fun! <variable>)))

;*---------------------------------------------------------------------*/
;*    *stamp* ...                                                      */
;*---------------------------------------------------------------------*/
(define *stamp* 0)

;*---------------------------------------------------------------------*/
;*    bind-fun! ...                                                    */
;*---------------------------------------------------------------------*/
(define (bind-fun! var)
   (trace (integrate 4) "bind-fun!(" (shape var) ", " *stamp* ")" #\Newline)
   (if (local? var)
       (sfun/Iinfo-istamp-set! (local-value var) *stamp*)))

;*---------------------------------------------------------------------*/
;*    free-fun? ...                                                    */
;*---------------------------------------------------------------------*/
(define (free-fun? local)
   (trace (integrate 4) "free-fun?(" (shape local) ", "
	  (sfun/Iinfo-istamp (local-value local))
	  ") *stamp*=" *stamp* #\Newline)
   (not (eq? (sfun/Iinfo-istamp (local-value local)) *stamp*)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun! ...                                            */
;*---------------------------------------------------------------------*/
(define (displace-let-fun! var)
   (trace (integrate 2)
	  "   displace-let-fun!: " (shape var) #\Newline
	  "                 Led: " (shape (sfun/Iinfo-Led
					   (variable-value var)))
	  #\Newline)
   (set! *stamp* (+fx 1 *stamp*))
   (displace-let-fun-node! (sfun-body (variable-value var)) (list var))
   ;; we scan all local functions in order to remove from additional
   ;; functions the ones that are already integrated in var. In the
   ;; following example we don't integrate f3.
   ;;   (define (foo x)
   ;;      (labels ((f1 (a) (labels ((f2 (b) (f2 b)))
   ;;                          (f2 a))))
   ;;         (labels ((f3 (c) (f3 (f3 (f3 (f1 c))))))
   ;;            (f3 4))))
   (let ((Led (sfun/Iinfo-Led (variable-value var))))
      (for-each (lambda (l)
		   (trace (integrate 4)
			  "  is free-fun?( " (shape l) " ): " (free-fun? l)
			  #\Newline)
		   (when (free-fun? l)
		       ;; we _absolutly can't_ mark function as seen
		       ;; otherwise all this computation will be wrong
		       ;; but we can test if the function is nested.
		       ;; If the function is nested, the predicate `free-fun?'
		       ;; will be false.
		       (displace-let-fun-node! (sfun-body (variable-value l))
					       (list var))))
		Led)
      (let loop ((Led   Led)
		 (added '()))
	 (cond
	    ((null? Led)
	     ;; we set the new body.
	     (let ((old-body (sfun-body (variable-value var))))
		(if (pair? added)
		    (let ((new-body (instantiate::let-fun
				       (loc (node-loc old-body))
				       (type (get-type old-body #f))
				       (locals added)
				       (body old-body))))
		       (trace (integrate 3)
			      (shape var) ", new body:\n"
			      (shape new-body) "\n")
		       (trace (integrate 2)
			      "    j'ajoute les fonctions: " (shape added)
			      #\Newline)
		       (sfun-body-set! (variable-value var) new-body)
		       new-body)
		    (begin
		       (trace (integrate 3)
			      (shape var) ", old body:\n"
			      (shape old-body) "\n")
		       old-body))))
	    ((free-fun? (car Led))
	     ;; we have to add this local function.
	     (loop (cdr Led) (cons (car Led) added)))
	    (else
	     ;; this function is already defined in var
	     (loop (cdr Led) added))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ...                                       */
;*    -------------------------------------------------------------    */
;*    This function only modifies the list of the `let-fun'            */
;*    constructions. So, we don't need to perform mutation             */
;*    during all the pass. We just realize side-effects when           */
;*    managing a `let-fun' node.                                       */
;*---------------------------------------------------------------------*/
(define-generic (displace-let-fun-node! node::node hosts::pair))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::atom ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::atom hosts)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::kwote ...                               */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::kwote hosts)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::var ...                                 */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::var hosts)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::closure ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::closure hosts)
   (internal-error "displace-let-fun-node" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::sequence ...                            */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::sequence hosts)
   (with-access::sequence node (nodes)
      (for-each (lambda (node) (displace-let-fun-node! node hosts)) nodes)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::sync ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::sync hosts)
   (with-access::sync node (body mutex prelock)
      (displace-let-fun-node! mutex hosts)
      (displace-let-fun-node! prelock hosts)
      (displace-let-fun-node! body hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::app ...                                 */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::app hosts)
   (let liip ((args (app-args node)))
      (if (null? args)
	  #unspecified
	  (begin
	     (displace-let-fun-node! (car args) hosts)
	     (liip (cdr args))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::app-ly ...                              */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::app-ly hosts)
   (with-access::app-ly node (fun arg)
      (displace-let-fun-node! fun hosts)
      (displace-let-fun-node! arg hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::funcall ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::funcall hosts)
   (with-access::funcall node (fun args)
      (let liip ((asts args))
	 (if (null? asts)
	     (displace-let-fun-node! fun hosts)
	     (begin
		(displace-let-fun-node! (car asts) hosts)
		(liip (cdr asts)))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::extern ...                              */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::extern hosts)
   (with-access::extern node (expr*)
      (for-each (lambda (node) (displace-let-fun-node! node hosts)) expr*)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::cast ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::cast hosts)
   (with-access::cast node (arg)
      (displace-let-fun-node! arg hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::setq ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::setq hosts)
   (with-access::setq node (var value)
      (displace-let-fun-node! value hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::conditional ...                         */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::conditional hosts)
   (with-access::conditional node (test true false)
      (displace-let-fun-node! test hosts)
      (displace-let-fun-node! true hosts)
      (displace-let-fun-node! false hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::fail ...                                */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::fail hosts)
   (with-access::fail node (proc msg obj)
      (displace-let-fun-node! proc hosts)
      (displace-let-fun-node! msg hosts)
      (displace-let-fun-node! obj hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::switch ...                              */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::switch hosts)
   (with-access::switch node (test)
      (let liip ((clauses (switch-clauses node)))
	 (if (null? clauses)
	     (displace-let-fun-node! test hosts)
	     (begin
		(displace-let-fun-node! (cdr (car clauses)) hosts)
		(liip (cdr clauses)))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::let-fun ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::let-fun hosts)
   (trace (integrate 5)
	  "displace-let-fun-node(let-fun): " (shape node) #\Newline)
   (with-access::let-fun node (body)
      (let liip ((old (let-fun-locals node))
		 (new '()))
	 (if (not (null? old))
	     (trace (integrate 5)
		    "display-let-fun-node!(let-fun), "
		    (shape (car old)) ":"
		    "\n  host: " (shape host)
		    "\n  Iinfo-L: "
		    (shape (sfun/Iinfo-L (local-value (car old))))
		    #\Newline))
	 (cond
	    ((null? old)
	     (let-fun-locals-set! node new)
	     (displace-let-fun-node! body hosts))
	    ((memq (sfun/Iinfo-L (local-value (car old))) hosts)
	     (let ((l (car old)))
		(bind-fun! l)
		(displace-let-fun-node! (sfun-body (local-value l))
					(cons l hosts))
		(liip (cdr old) (cons l new))))
	    (else
	     (liip (cdr old) new))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::let-var ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::let-var hosts)
   (with-access::let-var node (body)
      (let liip ((bindings (let-var-bindings node)))
	 (if (null? bindings)
	     (displace-let-fun-node! body hosts)
	     (let* ((binding (car bindings))
		    (var (car binding))
		    (val (cdr binding)))
		(displace-let-fun-node! val hosts)
		(liip (cdr bindings)))))))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::set-ex-it ...                           */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::set-ex-it hosts)
   (with-access::set-ex-it node (body)
      (displace-let-fun-node! body hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::jump-ex-it ...                          */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::jump-ex-it hosts)
   (with-access::jump-ex-it node (exit value)
      (displace-let-fun-node! exit hosts)
      (displace-let-fun-node! value hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::make-box ...                            */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::make-box hosts)
   (with-access::make-box node (value)
      (displace-let-fun-node! value hosts)))

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::box-set! ...                            */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::box-set! hosts)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    displace-let-fun-node! ::box-ref ...                             */
;*---------------------------------------------------------------------*/
(define-method (displace-let-fun-node! node::box-ref hosts)
   #unspecified)

