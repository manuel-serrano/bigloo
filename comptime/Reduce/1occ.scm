;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Reduce/1occ.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 10:29:17 1995                          */
;*    Last change :  Sun Jul 23 08:46:11 2017 (serrano)                */
;*    Copyright   :  1995-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The removal of the local variables appearing just once.          */
;*    The only goal of this pass is to prune the ast.                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module reduce_1occ
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
	    ast_lvtype
	    ast_occur
	    ast_dump)
   (export  (reduce-1occ! globals)))

;*---------------------------------------------------------------------*/
;*    reduce-1occ! ...                                                 */
;*---------------------------------------------------------------------*/
(define (reduce-1occ! globals)
   (verbose 2 #"      single occurrence")
   ;; we have to recompute the occurrences because both `copy-propagation
   ;; and `cse' have changed the number of occurrence (in two directions).
   (occur-var globals)
   ;; then we start the 1-occurrence reduction. 
   (set! *variable-removed* 0)
   (for-each (lambda (global)
		(let* ((fun  (global-value global))
		       (node (sfun-body fun)))
		   (sfun-body-set! fun (multiple-value-bind (_ node)
					  (node-1occ! node '())
					  node))
		   #unspecified))
	     globals)
   (verbose 2 "      (removed: " *variable-removed* #\) #\newline)
   globals)

;*---------------------------------------------------------------------*/
;*    Statitics ...                                                    */
;*---------------------------------------------------------------------*/
(define *variable-removed* 0)

;*---------------------------------------------------------------------*/
;*    node-1occ! ...                                                   */
;*    -------------------------------------------------------------    */
;*    1-exp* is a list of expressions bound to a 1 occurrence read     */
;*    variable. This list is reset when a call to a function           */
;*    performing a side effect is encountered.                         */
;*---------------------------------------------------------------------*/
(define-generic (node-1occ! node::node 1-exp*))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::atom ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::atom 1-exp*)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::kwote ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::kwote 1-exp*)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::var 1-exp*)
   (let ((v (var-variable node)))
      (let ((falpha (assq v 1-exp*)))
	 (if (pair? falpha)
	     (begin
		(variable-occurrence-set! v (-fx (variable-occurrence v) 1))
		(set! *variable-removed* (+fx *variable-removed* 1))
		(node-1occ! (cdr falpha) 1-exp*))
	     (values #f node)))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::closure ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::closure 1-exp*)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::sequence ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::sequence 1-exp*)
   (with-access::sequence node (nodes)
      (values (node-1occ*! nodes 1-exp*) node)))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::sync ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::sync 1-exp*)
   (with-access::sync node (body mutex prelock)
      (multiple-value-bind (resetm nmutex)
	 (node-1occ! mutex 1-exp*)
	 (set! mutex nmutex)
	 (multiple-value-bind (resetp nprelock)
	    (node-1occ! prelock 1-exp*)
	    (set! prelock nprelock)
	    (values (or (node-1occ! body 1-exp*) resetm resetp) node)))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::app-ly ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::app-ly 1-exp*)
   (with-access::app-ly node (fun arg)
      (multiple-value-bind (reset nfun)
	 (node-1occ! fun 1-exp*)
	 (set! fun nfun)
	 (multiple-value-bind (reset' narg)
	    (node-1occ! arg (if reset '() 1-exp*))
	    (set! arg narg)
	    (values (or reset reset') node)))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::funcall ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::funcall 1-exp*)
   (with-access::funcall node (args)
      (let ((reset' (node-1occ*! args 1-exp*)))
	 (values reset' node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::extern ...                                          */
;*    -------------------------------------------------------------    */
;*    We don't remove single occurrences for extern constructions      */
;*    because it may introduce typing pbm.                             */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::extern 1-exp*)
   (values (side-effect? node) node))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::private ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::private 1-exp*)
   (with-access::private node (expr*)
      (values (node-1occ*! expr* 1-exp*) node)))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::cast ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::cast 1-exp*)
   (with-access::cast node (arg)
      (multiple-value-bind (reset narg)
	 (node-1occ! arg 1-exp*)
	 (set! arg narg)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::setq ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::setq 1-exp*)
   (with-access::setq node (var value)
      (multiple-value-bind (reset nvalue)
	 (node-1occ! value 1-exp*)
	 (set! value nvalue)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::conditional ...                                     */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::conditional 1-exp*)
   (with-access::conditional node (test true false)
      (multiple-value-bind (reset ntest)
	 (node-1occ! test 1-exp*)
	 (set! test ntest)
	 (let ((1-exp*' (if reset '() 1-exp*)))
	    (multiple-value-bind (reset' ntrue)
	       (node-1occ! true 1-exp*')
	       (set! true ntrue)
	       (multiple-value-bind (reset'' nfalse)
		  (node-1occ! false 1-exp*')
		  (set! false nfalse)
		  (values (or reset reset' reset'') node)))))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::fail ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::fail 1-exp*)
   (with-access::fail node (proc msg obj)
      (multiple-value-bind (reset nproc)
	 (node-1occ! proc 1-exp*)
	 (set! proc nproc)
	 (let ((1-exp*' (if reset '() 1-exp*)))
	    (multiple-value-bind (reset' nmsg)
	       (node-1occ! msg 1-exp*')
	       (set! msg nmsg)
	       (let ((1-exp*'' (if reset' '() 1-exp*')))
		  (multiple-value-bind (reset'' nobj)
		     (node-1occ! obj 1-exp*'')
		     (set! obj nobj)
		     (values (or reset reset' reset'') node))))))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::switch ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::switch 1-exp*)
   (with-access::switch node (clauses test)
      (multiple-value-bind (reset ntest)
	 (node-1occ! test 1-exp*)
	 (set! test ntest)
	 (let ((1-exp*' (if reset '() 1-exp*)))
	    (let loop ((clauses clauses)
		       (reset   reset))
	       (if (null? clauses)
		   (values reset node)
		   (let ((clause (car clauses)))
		      (multiple-value-bind (reset' nclause)
			 (node-1occ! (cdr clause) 1-exp*)
			 (set-cdr! clause nclause)
			 (loop (cdr clauses) (or reset reset'))))))))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::let-fun ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::let-fun 1-exp*)
   (with-access::let-fun node (body locals)
      (multiple-value-bind (reset nbody)
	 (node-1occ! body 1-exp*)
	 (set! body nbody)
	 (let loop ((locals locals)
		    (reset  reset))
	    (if (null? locals)
		(values reset node)
		(let* ((local (car locals))
		       (sfun  (local-value local)))
		   (multiple-value-bind (reset' nbody)
		      (node-1occ! (sfun-body sfun) '())
		      (sfun-body-set! sfun nbody)
		      (loop (cdr locals) (or reset reset')))))))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::let-var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::let-var 1-exp*)
   (with-access::let-var node (body bindings loc type removable?)
      ;; we first treat the very special case
      ;;    (let ((var expr)) (if var ... ...))
      ;; this special case is important in order to get good
      ;; compile-type error messages
      (if (and (pair? bindings)
	       (null? (cdr bindings))
	       (let ((var (caar bindings)))
		  (and (=fx (local-occurrence var) 1)
		       (eq? (variable-type var) *bool*)
		       (conditional? body)
		       (with-access::conditional body (test)
			  (and (var? test)
			       (eq? (var-variable test) var))))))
	  ;; yes
	  (with-access::conditional body (test)
	     (set! test (cdar bindings))
	     (set! bindings '())
	     (node-1occ! body 1-exp*))
	  (node-1occ-let-var! node 1-exp*))))

;*---------------------------------------------------------------------*/
;*    node-1occ-let-var! ...                                           */
;*---------------------------------------------------------------------*/
(define (node-1occ-let-var! node::let-var 1-exp*)
   (with-access::let-var node (body bindings loc type removable?)
      (let loop ((obindings bindings)
		 (reset #f)
		 (extend '()))
	 (if (null? obindings)
	     (if (or reset (side-effect? body))
		 (multiple-value-bind (reset' nbody)
		    (node-1occ! body '())
		    (set! body nbody)
		    (values #t node))
		 (let ((1-exp*' (append extend 1-exp*)))
		    (multiple-value-bind (reset' nbody)
		       (node-1occ! body 1-exp*')
		       (set! body nbody)
		       ;; we computed the new body, we now have to remove
		       ;; bindings which have been inlined
		       (let loop ((obindings bindings)
				  (nbindings '()))
			  (cond
			     ((null? obindings)
			      (if (and removable? (null? nbindings))
				  (begin
				     (trace (reduce 3) "***1occ: remove bindings: "
					(shape body)
					#\Newline)
				     (values reset' body))
				  (begin
				     (set! bindings (reverse! nbindings))
				     (values reset' node))))
			     ((let ((var (car (car obindings)))
				    (val (cdr (car obindings))))
				 (and (=fx (local-occurrence var) 0)
				      (not (side-effect? val))))
			      (trace (reduce 3) "***1occ: remove: "
				 (shape (car obindings))
				 #\Newline)
			      (loop (cdr obindings) nbindings))
			     (else
			      (loop (cdr obindings)
				 (cons (car obindings) nbindings))))))))
	     (let ((binding (car obindings)))
		(let ((var (car binding))
		      (val (cdr binding)))
		   (multiple-value-bind (reset' nval)
		      (node-1occ! val 1-exp*)
		      (set-cdr! binding nval)
		      (cond
			 ((or reset reset')
			  (loop (cdr obindings) #t '()))
			 ((not (eq? (local-access var) 'read))
			  (loop (cdr obindings) #f extend))
			 ((and (=fx (local-occurrence var) 1)
			       (eq? (local-access var) 'read)
			       (not (side-effect? val))
			       (type-less-specific? (local-type var)
				  (get-type val #f)))
			  (trace (reduce 3) "***1occ: apply: "
			     (shape var) " " (shape val) #\Newline)
			  (loop (cdr obindings) #f (cons binding extend)))
			 (else
			  (loop (cdr obindings) #f extend))))))))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::set-ex-it ...                                       */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::set-ex-it 1-exp*)
   (with-access::set-ex-it node (var body)
      (multiple-value-bind (reset nbody)
	 (node-1occ! body 1-exp*)
	 (set! body nbody)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::jump-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::jump-ex-it 1-exp*)
   (with-access::jump-ex-it node (exit value)
      (multiple-value-bind (reset nexit)
	 (node-1occ! exit 1-exp*)
	 (set! exit nexit)
	 (let ((1-exp*' (if reset '() 1-exp*)))
	    (multiple-value-bind (reset' nvalue)
	       (node-1occ! value 1-exp*')
	       (set! value nvalue)
	       (values (or reset reset') node))))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::retblock ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::retblock 1-exp*)
   (with-access::retblock node (body)
      (multiple-value-bind (reset nbody)
	 (node-1occ! body 1-exp*)
	 (set! body nbody)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::return ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::return 1-exp*)
   (with-access::return node (value)
      (multiple-value-bind (reset nvalue)
	 (node-1occ! value 1-exp*)
	 (set! value nvalue)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::make-box ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::make-box 1-exp*)
   (with-access::make-box node (value)
      (multiple-value-bind (reset nvalue)
	 (node-1occ! value 1-exp*)
	 (set! value nvalue)
	 (values reset node))))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::box-set! ...                                        */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::box-set! 1-exp*)
   (with-access::box-set! node (var value)
      (multiple-value-bind (reset nvalue)
	 (node-1occ! value 1-exp*)
	 (set! value nvalue)
	 (values reset node))))
 
;*---------------------------------------------------------------------*/
;*    node-1occ! ::box-ref ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::box-ref 1-exp*)
   (values #f node))

;*---------------------------------------------------------------------*/
;*    node-1occ! ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-1occ! node::app 1-exp*)
   (with-access::app node (fun args loc type)
      (let ((reset (node-1occ*! args 1-exp*)))
	 (if (or reset (side-effect? node))
	     (begin
		(trace (reduce 2)
		       "***1occ: side-effect?: " (shape node) #\Newline)
		(values #t node))
	     (values #f node)))))

;*---------------------------------------------------------------------*/
;*    node-1occ*! ...                                                  */
;*---------------------------------------------------------------------*/
(define (node-1occ*! node* 1-exp*)
   (let loop ((node*  node*)
	      (reset  #f)
	      (1-exp* 1-exp*))
      (cond
	 ((null? node*)
	  reset)
	 ((null? (cdr node*))
	  (multiple-value-bind (reset' node)
	     (node-1occ! (car node*) 1-exp*)
	     (set-car! node* node)
	     (or reset reset')))
	 (else
	  (multiple-value-bind (reset' node)
	     (node-1occ! (car node*) 1-exp*)
	     (set-car! node* node)
	     (if (or reset reset')
		 (loop (cdr node*) #t '())
		 (loop (cdr node*) #f 1-exp*)))))))
