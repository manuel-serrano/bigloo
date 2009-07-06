;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/copy.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Tue Oct  5 13:56:23 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reduction of type checks.                                    */
;*=====================================================================*/
    
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_copy
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
	    type_typeof
	    type_misc
	    coerce_coerce
	    effect_effect
	    ast_var
	    ast_node
	    ast_alphatize
	    ast_lvtype)
   (export  (reduce-copy! globals)))

;*---------------------------------------------------------------------*/
;*    reduce-copy! ...                                                 */
;*---------------------------------------------------------------------*/
(define (reduce-copy! globals)
   (verbose 2 #"      copy propagation       ")
   (set! *copy-removed* 0)
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun))) 
		   (sfun-body-set! fun (node-copy! node))
		   #unspecified))
	     globals)
   (verbose 2 "(removed : " *copy-removed* #\) #\newline)
   globals)


;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *copy-removed* 0)

;*---------------------------------------------------------------------*/
;*    node-copy! ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (node-copy!::node node::node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    node-copy! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::kwote)
   node)

;*---------------------------------------------------------------------*/
;*    node-copy! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::var)
   (let ((v (var-variable node)))
      (let ((falpha (variable-fast-alpha v)))
	 (if (not (eq? falpha #unspecified))
	     (alphatize '() '() (node-loc node) (node-copy! falpha))
	     node))))

;*---------------------------------------------------------------------*/
;*    node-copy! ::closure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::closure)
   node)

;*---------------------------------------------------------------------*/
;*    node-copy! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::sequence)
   (with-access::sequence node (nodes)
      (node-copy*! nodes)
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (node-copy! fun))
      (set! arg (node-copy! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (node-copy! fun))
      (node-copy*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::extern ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::extern)
   (with-access::extern node (expr*)
      (node-copy*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::cast)
   (with-access::cast node (arg)
      (set! arg (node-copy! arg))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::setq)
   (with-access::setq node (var value)
      (set! value (node-copy! value))
      (set! var (node-copy! var))
      node))
 
;*---------------------------------------------------------------------*/
;*    node-copy! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::conditional)
   (with-access::conditional node (test true false)
       (set! test (node-copy! test))
       (set! true (node-copy! true))
       (set! false (node-copy! false))
       node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::fail)
   (with-access::fail node (type proc msg obj)
      (set! proc (node-copy! proc))
      (set! msg (node-copy! msg))
      (set! obj (node-copy! obj))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::select ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::select)
   (with-access::select node (clauses test)
      (set! test (node-copy! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (node-copy! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (local)
		   (let ((fun (local-value local)))
		      (sfun-body-set! fun (node-copy! (sfun-body fun)))))
		locals)
      (set! body (node-copy! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::let-var)
   (with-access::let-var node (body bindings removable?)
      (let loop ((obindings bindings)
		 (nbindings '()))
	 (if (null? obindings)
	     (if (and (null? nbindings) removable?)
		 (node-copy! body)
		 (begin
		    (set! bindings nbindings)
		    (set! body (node-copy! body))
		    node))
	     (let* ((binding (car obindings))
		    (var     (car binding))
		    (val     (node-copy! (cdr binding))))
		(set-cdr! binding val)
		;; we test a bit more than exact copy propagation
		;; because we check if val is also an atom.
		(trace (reduce 3)
		       "copy-propagation: var:" (shape var) " ["
		       (variable-access var)
		       "]  val: " (shape val) " "
		       (if (var? val) (variable-access (var-variable val)) "?")
		       "]" #\Newline)
		(if (and (eq? (variable-access var) 'read)
			 (or (atom? val)
			     (and (var? val)
				  (eq? (variable-access (var-variable val))
				       'read)
				  (type-less-specific? 
				   (variable-type var)
				   (variable-type (var-variable val))))))
 		    (begin
		       ;; we propagate the copy
		       (trace (reduce 3) "copy: reducing: "
			      (shape val) " -> " (shape var) #\Newline)
		       (set! *copy-removed* (+fx *copy-removed* 1))
		       (variable-fast-alpha-set! var val)
		       (loop (cdr obindings) nbindings))
		    (loop (cdr obindings) (cons binding nbindings))))))))
 
;*---------------------------------------------------------------------*/
;*    node-copy! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (node-copy! body))
      (set! var (node-copy! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (node-copy! exit))
      (set! value (node-copy! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::make-box)
   (with-access::make-box node (value)
      (set! value (node-copy! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (node-copy! var))
      (set! value (node-copy! value))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (node-copy! var))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::app)
   (with-access::app node (fun args)
      (node-copy*! args)
      node))

;*---------------------------------------------------------------------*/
;*    node-copy*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-copy*! node*)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (node-copy! (car node*)))
	     (loop (cdr node*))))))

