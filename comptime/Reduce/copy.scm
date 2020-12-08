;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Reduce/copy.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Wed Dec 25 19:20:51 2019 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The reduction of type checks.                                    */
;*=====================================================================*/
    
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_copy
   (include "Tools/trace.sch")
   (import  engine_param
	    tools_shape
	    tools_speek
	    tools_error
	    type_type
	    type_cache
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
   (verbose 2 "(removed: " *copy-removed* #\) #\newline)
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
;*    node-copy! ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::sync)
   (with-access::sync node (body mutex prelock)
      (set! mutex (node-copy! mutex))
      (set! prelock (node-copy! prelock))
      (set! body (node-copy! body))
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
;*    node-copy! ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::switch)
   (with-access::switch node (clauses test)
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
;*    copyable? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is an expression "copyable" in the copy optimization?            */
;*---------------------------------------------------------------------*/
(define-generic (copyable? node v)
   #f)

;*---------------------------------------------------------------------*/
;*    copyable? ::atom ...                                             */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::atom v)
   #t)

;*---------------------------------------------------------------------*/
;*    copyable? ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::var v)
   (eq? (variable-access (var-variable node)) 'read))

;*---------------------------------------------------------------------*/
;*    copyable? ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::sequence v)
   (with-access::sequence node (nodes)
      (every (lambda (n) (copyable? n v)) nodes)))

;*---------------------------------------------------------------------*/
;*    copyable? ::vlength ...                                          */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::vlength v)
   (with-access::vlength node (expr*)
      (every (lambda (n) (copyable? n v)) expr*)))

;*---------------------------------------------------------------------*/
;*    copyable? ::cast ...                                             */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::cast v)
   (with-access::cast node (arg)
      (copyable? arg v)))

;*---------------------------------------------------------------------*/
;*    copyable? ::cast-null ...                                        */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::cast-null v)
   (with-access::cast-null node (expr*)
      (every (lambda (n) (copyable? n v)) expr*)))

;*---------------------------------------------------------------------*/
;*    copyable? ::instanceof ...                                       */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::instanceof v)
   (with-access::instanceof node (expr*)
      (every (lambda (n) (copyable? n v)) expr*)))

;*---------------------------------------------------------------------*/
;*    copyable? ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::conditional v)
   (with-access::conditional node (test true false)
      (when (<=fx (variable-occurrence v) 1)
	 (and (copyable? test v) (copyable? true v) (copyable? false v)))))

;*---------------------------------------------------------------------*/
;*    copyable? ::fail ...                                             */
;*---------------------------------------------------------------------*/
(define-method (copyable? node::fail v)
   (with-access::fail node (proc msg obj)
      (and (copyable? proc v) (copyable? msg v) (copyable? obj v))))

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
		    (var (car binding))
		    (val (node-copy! (cdr binding))))
		(set-cdr! binding val)
		;; we test a bit more than exact copy propagation
		;; because we check if val is also an atom.
		(trace (reduce reduce+ 3)
		   "copy-propagation: var:" (shape var) " ["
		   (variable-access var)
		   "] val=" (shape val) " "
		   (if (var? val) (variable-access (var-variable val)) "?")
		   "] copyable=" (copyable? val var) " type="
		   (cell-type-less-specific?
		      (node-type val)
		      (variable-type var))
		   " tval=" (shape (node-type val))
		   " tvar=" (shape (variable-type var))
		   #\Newline)
		(if (and (eq? (variable-access var) 'read)
			 (copyable? val var)
			 (cell-type-less-specific?
			    (node-type val)
			    (variable-type var)))
 		    (begin
		       ;; we propagate the copy
		       (trace (reduce reduce+ 3) "copy: reducing: "
			  (shape val) " -> " (shape var) #\Newline)
		       (set! *copy-removed* (+fx *copy-removed* 1))
		       (variable-fast-alpha-set! var
			  (if (eq? (node-type val) (variable-type var))
			      val
			      (instantiate::cast
				 (loc (node-loc val))
				 (type (variable-type var))
				 (arg val))))
		       (loop (cdr obindings) nbindings))
		    (loop (cdr obindings) (cons binding nbindings))))))))

;*---------------------------------------------------------------------*/
;*    cell-type-less-specific? ...                                     */
;*---------------------------------------------------------------------*/
(define (cell-type-less-specific? ty1 ty2)
   (or (type-less-specific? ty1 ty2)
       (and (eq? ty2 *obj*) (eq? ty1 *cell*))))

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
;*    node-copy! ::retblock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::retblock)
   (with-access::retblock node (body)
      (set! body (node-copy! body))
      node))

;*---------------------------------------------------------------------*/
;*    node-copy! ::return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::return)
   (with-access::return node (value)
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
      (set! value (node-copy! value))
      (let ((cp (node-copy! var)))
	 (if (isa? cp cast)
	     (with-access::cast cp (arg)
		(set! var arg)
		(set! arg node)
		cp)
	     (begin
		(set! var cp)
		node)))))

;*---------------------------------------------------------------------*/
;*    node-copy! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-copy! node::box-ref)
   (with-access::box-ref node (var)
      (let ((cp (node-copy! var)))
	 (if (isa? cp cast)
	     (with-access::cast cp (arg)
		(set! var arg)
		(set! arg node)
		cp)
	     (begin
		(set! var cp)
		node)))))

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

