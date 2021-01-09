;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Stackable/walk.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul 12 08:33:01 2020                          */
;*    Last change :  Sun Jul 12 08:33:02 2020 (serrano)                */
;*    Copyright   :  2020-21 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Mark "stackable" expressions, that is expression that can        */
;*    possibly be stack allocated.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module stackable_walk
   (include "Engine/pass.sch"
	    "Tools/trace.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_speek
	    type_type
	    type_typeof
	    type_cache
	    type_misc
	    type_env
	    object_class
	    ast_var
	    ast_node
	    ast_env
	    ast_dump
	    ast_walk
	    ast_occur
	    module_module
	    engine_param)
   (static  (wide-class app/depth::app
	       (depth::long read-only))
	    (wide-class local/depth::local
	       (depth::long read-only)
	       (owner::obj read-only)))
   (export  (stackable-walk! globals)))

;*---------------------------------------------------------------------*/
;*    shape ::local/depth ...                                          */
;*---------------------------------------------------------------------*/
(define-method (shape var::local/depth)
   (with-access::local/depth var (depth)
      (string->symbol (format "[~a]~a" depth (call-next-method)))))

;*---------------------------------------------------------------------*/
;*    node->sexp ::app ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node->sexp node::app/depth)
   (with-access::app/depth node (depth)
      (let ((n (call-next-method)))
	 (if (symbol? (car n))
	     (cons (symbol-append (string->symbol (format "[~a]" depth)) (car n))
		(cdr n))
	     n))))

;*---------------------------------------------------------------------*/
;*    stackable-walk! ...                                              */
;*---------------------------------------------------------------------*/
(define (stackable-walk! globals)
   (if *optim-stackable?*
       (begin
	  (pass-prelude "Stackable")
	  (for-each global-lift-let! globals)
	  (for-each global-depth-let! globals)
	  (for-each global-init-stackable! globals)
	  (let loop ((ctx (cons #f '())))
	     (unless (car ctx)
		(set-car! ctx #t)
		(set-cdr! ctx '())
		(for-each (lambda (g) (var-stackable g ctx)) globals)
		(loop ctx)))
	  (pass-postlude globals))
       globals))

;*---------------------------------------------------------------------*/
;*    import? ...                                                      */
;*---------------------------------------------------------------------*/
(define (import? var::variable)
   (and (global? var) (not (eq? (global-module var) *module*))))

;*---------------------------------------------------------------------*/
;*    var-stackable ...                                                */
;*---------------------------------------------------------------------*/
(define (var-stackable var::variable ctx::pair)
   (when (and (not (import? var)) (not (memq var (cdr ctx))))
      ;; a local function not scanned during this iteration yet
      (set-cdr! ctx (cons var (cdr ctx)))
      (with-access::variable var (value)
	 (with-access::sfun value (body)
	    (if (isa? var global)
		(stackable body #t 0 ctx)
		(with-access::local/depth var (depth)
		   (stackable body #t depth ctx)))))))

;*---------------------------------------------------------------------*/
;*    max-depth ...                                                    */
;*---------------------------------------------------------------------*/
(define (max-depth) 100000)

;*---------------------------------------------------------------------*/
;*    stackable ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::node escp::bool depth::long ctx::pair)
   (set! escp #t)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    stackable ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::var escp depth ctx::pair)
   (with-access::var node (variable loc)
      (when (isa? variable local)
	 (unless (isa? variable local/depth)
	    (tprint "PAS BON: " (shape node) " " loc))
	 (with-access::local/depth variable (val-stackable (vdepth depth))
	    (when (or escp (<fx depth vdepth))
	       (escape! variable ctx))))))

;*---------------------------------------------------------------------*/
;*    stackable ::setq ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::setq escp depth ctx::pair)
   (with-access::setq node (var value)
      (with-access::var var (variable)
	 (if (isa? variable global)
	     (stackable value #t -1 ctx)
	     (with-access::local/depth variable ((vdepth depth))
		(stackable value escp vdepth ctx))))))

;*---------------------------------------------------------------------*/
;*    stackable ::conditional ...                                      */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::conditional escp depth ctx::pair)
   (with-access::conditional node (test true false)
      (stackable test #f depth ctx)
      (stackable true escp depth ctx)
      (stackable false escp depth ctx)))

;*---------------------------------------------------------------------*/
;*    stackable ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::let-var escp depth ctx::pair)
   (with-access::let-var node (bindings body)
      (for-each (lambda (bind)
		   (let ((var (car bind))
			 (val (cdr bind)))
		      (with-access::local/depth var (val-noescape depth)
			 (stackable val (not val-noescape) depth ctx))))
	 bindings)
      (stackable body escp depth ctx)))

;*---------------------------------------------------------------------*/
;*    stackable ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::let-fun escp depth ctx::pair)
   (with-access::let-fun node (locals body)
      (for-each (lambda (local)
		   (with-access::local/depth local (depth value)
		      (with-access::sfun value (body)
			 (stackable body #f depth ctx))))
	 locals)
      (stackable body escp depth ctx)))

;*---------------------------------------------------------------------*/
;*    stackable ::app ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::app escp depth ctx::pair)
   
   (define (all-stackable? args)
      ;; Compute an over approximation of the stackable property of all
      ;; the call arguments. This is needed for args-retescape annotations.
      ;; args-retescape is currently not used because it needs more
      ;; than purely local reasoning. Let's consider the following:
      ;;  (define x 0)
      ;;  (define (gee a b c)
      ;;     (let ((lll (cons b (cons 222 '())))
      ;;           (o (cons 111 333)))
      ;;        (set-car! o lll) [1]
      ;;        (set! x o)       [2]
      ;;        (bar a lll)))
      ;; in line [1] lll must be declared as escaping and the
      ;; call unstackable because at line [2] o is found escaping.
      ;; The current implementation cannot know is an expression
      ;; is escaping
      (every (lambda (a)
		(cond
		   ((isa? a app)
		    (with-access::app a (stackable) stackable))
		   ((isa? a make-box)
		    (with-access::make-box a (stackable) stackable))
		   ((isa? a var)
		    (with-access::var a (variable)
		       (when (isa? variable local)
			  (with-access::local variable (val-noescape)
			     val-noescape))))
		   ((or (isa? a atom) (isa? a kwote))
		    #t)
		   (else
		    #f)))
	 args))
      
   (with-access::app node ((callee fun) args (stkp stackable))
      (with-access::app/depth node ((adepth depth))
	 (when (or escp (<fx depth adepth))
	    (when (and #f stkp)
	       (tprint "! " (node->sexp node)
		  " escp=" escp " D=" depth " AD=" adepth))
	    (escape! node ctx)))
      (let* ((v (var-variable callee))
	     (f (variable-value v)))
	 (cond
	    ((isa? v local)
	     ;; local loops cannot stack allocate
	     (for-each (lambda (a) (stackable a #t (max-depth) ctx)) args))
	    ((and (isa? f sfun) (not (import? v)))
	     (var-stackable v ctx)
	     (with-access::sfun f ((parameters args))
		(for-each (lambda (p a)
			     (with-access::local p (val-noescape depth)
				(stackable a (not val-noescape) (max-depth) ctx)))
		   parameters args)))
	    ((isa? f fun)
	     (with-access::fun f (args-noescape args-retescape)
		(cond
		   ((eq? args-noescape '*)
		    (for-each (lambda (a) (stackable a #f (max-depth) ctx)) args))
		   ((and (eq? args-retescape '*) stkp (all-stackable? args))
		    (for-each (lambda (a) (stackable a #f depth ctx)) args))
		   ((or (pair? args-noescape) (pair? args-retescape))
		    (let loop ((i 0)
			       (args args))
		       (when (pair? args)
			  (cond
			     ((memq i args-noescape)
			      (stackable (car args) #f (max-depth) ctx))
			     ((and (memq i args-retescape) stkp)
			      (stackable (car args) (not stkp) depth ctx))
			     (else
			      (stackable (car args) #t depth ctx)))
			  (loop (+fx i 1) (cdr args)))))
		   (else
		    (for-each (lambda (a) (stackable a #t (max-depth) ctx)) args)))))
	    (else
	     (for-each (lambda (a) (stackable a #t (max-depth) ctx)) args))))))

;*---------------------------------------------------------------------*/
;*    stackable ::funcall ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::funcall escp depth ctx::pair)
   (set! escp #t)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    stackable ::sequence ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (stackable node::sequence escp depth ctx::pair)
   (with-access::sequence node (nodes)
      (when (pair? nodes)
	 (let loop ((nodes nodes))
	    (if (null? (cdr nodes))
		(stackable (car nodes) escp depth ctx)
		(begin
		   (stackable (car nodes) #f (max-depth) ctx)
		   (loop (cdr nodes))))))))
   
;*---------------------------------------------------------------------*/
;*    escape! ...                                                      */
;*    -------------------------------------------------------------    */
;*    mark that an expression escapes, i.e., the variables escape      */
;*    and the function calls are unstackable.                          */
;*---------------------------------------------------------------------*/
(define-generic (escape! node ctx::pair)
   (tprint "ESCAPE! should not be here " (shape node))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    escape! ::global ...                                             */
;*---------------------------------------------------------------------*/
(define-method (escape! var::global ctx)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    escape! ::local ...                                              */
;*---------------------------------------------------------------------*/
(define-method (escape! var::local ctx)
   (with-access::local var (val-noescape)
      (when (eq? val-noescape #t)
	 (set-car! ctx #f)
	 (set! val-noescape #f))))

;*---------------------------------------------------------------------*/
;*    escape! ::node ...                                               */
;*---------------------------------------------------------------------*/
(define-method (escape! node::node ctx)
   (node-escape node ctx))

;*---------------------------------------------------------------------*/
;*    escape! ::app ...                                                */
;*---------------------------------------------------------------------*/
(define-method (escape! node::app ctx)
   (with-access::app node (args stackable)
      (when stackable
	 (set-car! ctx #f)
	 (set! stackable #f))))
   
;*---------------------------------------------------------------------*/
;*    escape! ::make-box ...                                           */
;*---------------------------------------------------------------------*/
(define-method (escape! node::make-box ctx)
   (with-access::make-box node (value stackable)
      (when stackable
	 (set-car! ctx #f)
	 (set! stackable #f)
	 (escape! value ctx))))

;*---------------------------------------------------------------------*/
;*    node-escape ::node ...                                           */
;*---------------------------------------------------------------------*/
(define-walk-method (node-escape node::node ctx)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    node-escape ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-walk-method (node-escape node::app ctx)
   (escape! node ctx))

;*---------------------------------------------------------------------*/
;*    node-escape ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-walk-method (node-escape node::make-box ctx)
   (escape! node ctx))

;*---------------------------------------------------------------------*/
;*    global-lift-let! ...                                             */
;*    -------------------------------------------------------------    */
;*    Apply the following transformation:                              */
;*                                                                     */
;*      (let (... (l (let ((x ...)) body)) ...)                        */
;*    =>                                                               */
;*      (let ((x ...)) (let (... (l body) ...) ...)                    */
;*---------------------------------------------------------------------*/
(define (global-lift-let! var::variable)
   (with-access::variable var (value)
      (with-access::sfun value (body)
	 (set! body (lift-let! body)))))

;*---------------------------------------------------------------------*/
;*    lift-let! ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (lift-let! node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    lift-let! ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (lift-let! node::let-var)

   (define (liftable? node)
      ;; as the purpose of the lift is simply to move stackable
      ;; variables in compatible lexical blocks, only those let-var
      ;; blocks that might potentially lift stack variables are
      ;; considered
      (cond
	 ((isa? node let-var)
	  (with-access::let-var node (bindings body)
	     (and (pair? bindings)
		  (null? (cdr bindings))
		  (liftable? (cdar bindings))
		  (liftable? body))))
	 ((isa? node var)
	  (local? (var-variable node)))
	 ((isa? node app)
	  (with-access::app node (fun)
	     (let ((v (var-variable fun)))
		(when (isa? v global)
		   (pair? (fun-stack-allocator (global-value v)))))))
	 (else
	  (isa? node make-box))))
      
   (define (lift-binding! binding)
      (if (and (isa? (cdr binding) let-var) (liftable? (cdr binding)))
	  (with-access::let-var (cdr binding) (bindings body loc)
	     (verbose 3 "      lifting binding "
		(variable-id (car binding)) "<-"
		(map (lambda (b) (variable-id (car b))) bindings)
		" " loc "\n")
	     (set-cdr! binding body)
	     bindings)
	  '()))
   
   (with-access::let-var node (bindings body)
      (let ((lbindings (append-map lift-binding! bindings)))
	 (if (pair? lbindings)
	     (lift-let!
		(duplicate::let-var node
		   (bindings lbindings)
		   (body node)))
	     (call-default-walker)))))
	     
;*---------------------------------------------------------------------*/
;*    global-depth-let! ...                                            */
;*    -------------------------------------------------------------    */
;*    Compute let local-depth property.                                */
;*---------------------------------------------------------------------*/
(define (global-depth-let! var::variable)
   (with-access::variable var (value)
      (with-access::sfun value (body args)
	 (for-each (lambda (a)
		      (widen!::local/depth a
			 (owner var)
			 (depth 0)))
	    args)
	 (depth-let body 0 var))))
   
;*---------------------------------------------------------------------*/
;*    depth-let ::node ...                                             */
;*---------------------------------------------------------------------*/
(define-walk-method (depth-let node::node depth fun)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    depth-let ::var ...                                              */
;*---------------------------------------------------------------------*/
(define-walk-method (depth-let node::var depth fun)
   (with-access::var node (variable loc)
      (when (isa? variable local/depth)
	 (with-access::local/depth variable (owner val-noescape)
	    (unless (eq? owner fun)
	       (set! val-noescape #f))))))

;*---------------------------------------------------------------------*/
;*    depth-let ::let-var ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (depth-let node::let-var depth fun)
   (let ((depth (+fx depth 1)))
      (with-access::let-var node (bindings body)
	 (for-each (lambda (binding)
		      (widen!::local/depth (car binding)
			 (owner fun)
			 (depth depth))
		      (depth-let (cdr binding) depth fun))
	    bindings)
	 (depth-let body depth fun))))

;*---------------------------------------------------------------------*/
;*    depth-let ::let-fun ...                                          */
;*---------------------------------------------------------------------*/
(define-walk-method (depth-let node::let-fun depth fun)
   (let ((depth (+fx depth 1)))
      (with-access::let-fun node (locals body)
	 (for-each (lambda (local)
		      (with-access::local local (value)
			 (widen!::local/depth local
			    (owner fun)
			    (depth depth))
			 (with-access::sfun value (body args)
			    (for-each (lambda (a)
					 (widen!::local/depth a
					    (owner local)
					    (depth (+fx depth 1))))
			       args)
			    (depth-let body (+fx depth 1) local))))
	    locals)
	 (depth-let body 1 fun))))

;*---------------------------------------------------------------------*/
;*    depth-let ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (depth-let node::app depth fun)
   (call-default-walker)
   (widen!::app/depth node (depth depth)))

;*---------------------------------------------------------------------*/
;*    set-ex-it ...                                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (depth-let node::set-ex-it depth fun)
   (call-default-walker)
   (with-access::set-ex-it node (var body)
      (with-access::var var (variable)
	 (with-access::local variable (val-noescape)
	    (set! val-noescape #f))
	 (widen!::local/depth variable
	    (owner fun)
	    (depth (+fx depth 1))))
      (depth-let body (+fx depth 1) fun)))

;*---------------------------------------------------------------------*/
;*    global-init-stackable! ...                                       */
;*---------------------------------------------------------------------*/
(define (global-init-stackable! var::variable)
   (with-access::variable var (value)
      (with-access::sfun value (body)
	 (init-stackable body))))
   
;*---------------------------------------------------------------------*/
;*    init-stackable ::node ...                                        */
;*---------------------------------------------------------------------*/
(define-walk-method (init-stackable node::node)
   (call-default-walker))

;*---------------------------------------------------------------------*/
;*    init-stackable ::app ...                                         */
;*---------------------------------------------------------------------*/
(define-walk-method (init-stackable node::app)
   (call-default-walker)
   (with-access::app node (stackable)
      (set! stackable #t)))

;*---------------------------------------------------------------------*/
;*    init-stackable ::make-box ...                                    */
;*---------------------------------------------------------------------*/
(define-walk-method (init-stackable node::make-box)
   (call-default-walker)
   (with-access::make-box node (stackable)
      (set! stackable #t)))
