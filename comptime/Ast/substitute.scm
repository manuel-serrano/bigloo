;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Ast/substitute.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  6 11:09:14 1995                          */
;*    Last change :  Tue Feb 26 13:54:27 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The substitution tools module                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_substitute
   (include "Tools/trace.sch")
   (import  type_type
	    type_cache
	    ast_var
	    ast_node
	    tools_shape
	    tools_error
	    tools_shape
	    ast_apply
	    ast_app
	    ast_sexp)
   (export  (substitute!::node what* by* ::node site)))

;*---------------------------------------------------------------------*/
;*    substitute! ...                                                  */
;*    -------------------------------------------------------------    */
;*    Substitute can replace a variable by a variable or an atom       */
;*    (including `fun') construction but nothing else.                 */
;*---------------------------------------------------------------------*/
(define (substitute! what* by* node site)
   (assert (site) (memq site '(value apply app set!)))
   ;; we set alpha-fnode slot 
   (for-each (lambda (what by)
		(assert (by) (variable? by))
		(variable-fast-alpha-set! what by))
	     what*
	     by*)
   (let ((res (do-substitute! node site)))
      ;; we remove alpha-fast slots
      (for-each (lambda (what)
		   (variable-fast-alpha-set! what #unspecified))
		what*)
      res))
  
;*---------------------------------------------------------------------*/
;*    do-substitute! ...                                               */
;*---------------------------------------------------------------------*/
(define-generic (do-substitute!::node node::node site))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::atom ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::atom site)
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::var ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::var site)
   (let* ((var   (var-variable node))
	  (alpha (variable-fast-alpha var)))
      (let loop ((alpha alpha))
	 (cond
	    ((eq? alpha #unspecified)
	     node)
	    ((var? alpha)
	     (loop (var-variable alpha)))
	    ((variable? alpha) 
	     (use-variable! alpha (node-loc node) site) 
	     (if (and (fun? (variable-value alpha)) (not (eq? site 'app)))
		 (instantiate::closure
		    (loc (node-loc node))
		    (type (strict-node-type *procedure* (node-type node)))
		    (variable alpha))
		 (instantiate::var
		    (loc (node-loc node))
		    (type (node-type node))
		    (variable alpha))))
	    ((atom? alpha)
	     alpha)
	    (else
	     (internal-error "duplicate" "Illegal substitution" (shape node)))))))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::kwote ...                                       */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::kwote site)
   node)
       
;*---------------------------------------------------------------------*/
;*    do-substitute! ::sequence ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::sequence site)
   (do-substitute*! (sequence-nodes node) site)
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::sync ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::sync site)
   (sync-mutex-set! node (do-substitute! (sync-mutex node) site))
   (sync-prelock-set! node (do-substitute! (sync-prelock node) site))
   (sync-body-set! node (do-substitute! (sync-body node) site))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::app site)
   (app-fun-set! node (do-substitute! (app-fun node) 'app))
   (do-substitute*! (app-args node) 'value)
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::app-ly ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::app-ly site)
   (with-access::app-ly node (arg fun loc)
      (let ((nfun (do-substitute! fun 'apply))
	    (narg (do-substitute! arg 'value)))
	 (if (and (closure? nfun)
		  (not (global-optional? (var-variable nfun)))
		  (not (global-key? (var-variable nfun))))
	     (known-app-ly->node '() loc (duplicate::var nfun) narg site)
	     (begin
		(set! fun nfun)
		(set! arg narg)
		node)))))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::funcall ...                                     */
;*    -------------------------------------------------------------    */
;*    When transforming a funcall into an app node we have to remove   */
;*    the extra argument which hold the closure.                       */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::funcall site)
   (with-access::funcall node (args fun loc)
      (let ((nfun  (do-substitute! fun 'value))
	    (nargs (map (lambda (a) (do-substitute! a 'value)) args)))
	 (if (or (closure? nfun)
		 (and (var? nfun)
		      (fun? (variable-value (var-variable nfun)))))
	     (if (correct-arity-app? (var-variable nfun)
				     (cdr nargs))
		 (make-app-node '() loc 'funcall nfun (cdr nargs))
		 (user-error/location
		    loc
		    "Illegal application" "wrong number of argument(s)"
		    (shape node)))
	     (begin
		(set! fun nfun)
		(set! args nargs)
		node)))))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::extern ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::extern site)
   (do-substitute*! (extern-expr* node) site)
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::cast ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::cast site)
   (cast-arg-set! node (do-substitute! (cast-arg node) site))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::setq ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::setq site)
   (with-access::setq node (var value)
      (set! var (do-substitute! var 'set!))
      (set! value (do-substitute! value site))
      node))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::conditional ...                                 */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::conditional site)
   (with-access::conditional node (test true false)
      (set! test (do-substitute! test 'value))
      (set! true (do-substitute! true site))
      (set! false (do-substitute! false site))
      node))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::fail ...                                        */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::fail site)
   (with-access::fail node (proc msg obj)
      (set! proc (do-substitute! proc 'value))
      (set! msg (do-substitute! msg 'value))
      (set! obj (do-substitute! obj 'value))
      node))

;*---------------------------------------------------------------------*/
;*    do-substitute! ::switch ...                                      */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::switch site)
   (switch-test-set! node (do-substitute! (switch-test node) 'value))
   (for-each (lambda (clause)
		(set-cdr! clause (do-substitute! (cdr clause) site)))
	     (switch-clauses node))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::let-fun ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::let-fun site)
   (for-each (lambda (local)
		(let ((fun (local-value local)))
		   (sfun-body-set! fun (do-substitute! (sfun-body fun)
						       'value))))
	     (let-fun-locals node))
   (let-fun-body-set! node (do-substitute! (let-fun-body node) site))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::let-var ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::let-var site)
   (for-each (lambda (binding)
		(set-cdr! binding (do-substitute! (cdr binding) 'value)))
	     (let-var-bindings node))
   (let-var-body-set! node (do-substitute! (let-var-body node) site))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::set-ex-it ...                                   */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::set-ex-it site)
   (set-ex-it-body-set! node (do-substitute! (set-ex-it-body node) site))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::jump-ex-it ...                                  */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::jump-ex-it site)
   (jump-ex-it-exit-set! node (do-substitute! (jump-ex-it-exit node) 'app))
   (jump-ex-it-value-set! node (do-substitute! (jump-ex-it-value node) 'value))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::make-box ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::make-box site)
   (make-box-value-set! node (do-substitute! (make-box-value node) 'value))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::box-ref ...                                     */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::box-ref site)
   (box-ref-var-set! node (do-substitute! (box-ref-var node) 'value))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute! ::box-set! ...                                    */
;*---------------------------------------------------------------------*/
(define-method (do-substitute! node::box-set! site)
   (box-set!-var-set! node (do-substitute! (box-set!-var node) 'value))
   (box-set!-value-set! node (do-substitute! (box-set!-value node) 'value))
   node)

;*---------------------------------------------------------------------*/
;*    do-substitute*! ...                                              */
;*---------------------------------------------------------------------*/
(define (do-substitute*! node* site)
   (cond
      ((null? node*)
       'done)
      ((null? (cdr node*))
       (set-car! node* (do-substitute! (car node*) site))
       'done)
      (else
       (set-car! node* (do-substitute! (car node*) 'value))
       (do-substitute*! (cdr node*) site))))
   
