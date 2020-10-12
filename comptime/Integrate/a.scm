;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Integrate/a.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 14 10:52:56 1995                          */
;*    Last change :  Wed Dec 25 18:29:57 2019 (serrano)                */
;*    Copyright   :  1995-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The computation of the A relation.                               */
;*    -------------------------------------------------------------    */
;*    Celled variables are considered as only read and are treated     */
;*    accordingly.                                                     */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module integrate_a
   (include "Tools/trace.sch")
   (import  tools_shape
	    tools_error
	    type_type
	    engine_param
	    ast_var
	    ast_node
	    ast_dump
	    integrate_info
	    return_walk
	    type_cache
	    type_typeof)
   (export  (A ::global ::node)
	    *phi*))

;*---------------------------------------------------------------------*/
;*    *phi* ...                                                        */
;*---------------------------------------------------------------------*/
(define *phi* #unspecified)

;*---------------------------------------------------------------------*/
;*    A ...                                                            */
;*    -------------------------------------------------------------    */
;*    Compute the A property (see Seniak's thesis) and for             */
;*    each function, compute its free variables set.                   */
;*---------------------------------------------------------------------*/
(define (A global node)
   (when *optim-return-goto?* (init-return-cache!))
   ;; the setups
   (set! *phi*  (list global))
   (set! *kont* 0)
   (initialize-fun! global global)
   ;; we start the A computation
   (let ((A (node-A node global (cons 'tail (global-type global)) '())))
      (trace-A A "Before tail-coercion")
      (let ((A' (tail-coercion A global)))
	 (trace-A A' "After tail-coercion")
	 A')))

;*---------------------------------------------------------------------*/
;*    initialize-fun! ...                                              */
;*---------------------------------------------------------------------*/
(define (initialize-fun! fun::variable owner::variable)
   (widen!::sfun/Iinfo (variable-value fun)
      (owner owner)
      (G? (global? fun)))
   (for-each (lambda (x)
		(widen!::svar/Iinfo (local-value x)))
	     (sfun-args (variable-value fun))))

;*---------------------------------------------------------------------*/
;*    *kont* ...                                                       */
;*---------------------------------------------------------------------*/
(define *kont* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-new-kont ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-new-kont)
   (set! *kont* (+fx 1 *kont*))
   *kont*)

;*---------------------------------------------------------------------*/
;*    trace-A ...                                                      */
;*---------------------------------------------------------------------*/
(define (trace-A A msg)
   (let ((p (with-output-to-string
	       (lambda ()
		  (for-each (lambda (a) 
			       (print "A( " (shape (car a)) ", "
				  (shape (cadr a)) ", "
				  (shape (caddr a)) " )"))
		     A)))))
      (trace (integrate 2)
	 "- - - - - - - - - - - - - - - - " msg
	 #\Newline
	 "PHI: " (shape *phi*) #\newline
	 p
	 "- - - - - - - - - - - - - - - - "
	 #\Newline)))

;*---------------------------------------------------------------------*/
;*    tail-type-compatible? ...                                        */
;*    -------------------------------------------------------------    */
;*    Are two types compatible with respect to the tail recursion      */
;*    property?                                                        */
;*---------------------------------------------------------------------*/
(define (tail-type-compatible? t1 t2)
   (or (eq? t1 t2)
       (and (eq? t1 *int*) (eq? t2 *long*))
       (and (eq? t1 *long*) (eq? t2 *int*))
       ;; MS: 30jul2011, I have added that new test but I'm not fully
       ;; sure it's correct. I will have to keep an eye on that.
       (and (bigloo-type? t1) (bigloo-type? t2))))
       
;*---------------------------------------------------------------------*/
;*    tail-coercion ...                                                */
;*    -------------------------------------------------------------    */
;*    This function checks the tail calls contexts. In particular,     */
;*    it detects local functions that are always called in tail        */
;*    positions but from different type contexts. These peculiar       */
;*    local functions are globalized.                                  */
;*---------------------------------------------------------------------*/
(define (tail-coercion A global)
   ;; detect the functions called from different type contexts
   (for-each (lambda (a)
		(match-case a
		   ((?- ?callee (?- . ?type))
		    (let ((fun (variable-value callee)))
		       (with-access::sfun/Iinfo fun (tail-coercion body)
			  (cond
			     ((not tail-coercion)
			      #unspecified)
			     ((not (type? type))
			      #unspecified)
			     ((or (eq? type *obj*) (eq? type *magic*))
			      #unspecified)
			     ((eq? tail-coercion #unspecified)
			      (set! tail-coercion type))
			     ((or (eq? type *pair-nil*) (eq? type *magic*))
			      #unspecified)
			     ((or (and (eq? type *pair-nil*)
				       (eq? tail-coercion *pair*))
				  (and (eq? type *pair*)
				       (eq? tail-coercion *pair-nil*)))
			      #unspecified)
			     ((not (tail-type-compatible? tail-coercion type))
			      (if (local? callee)
				  (user-warning/location
				     (node-loc body)
				     (shape callee)
				     "Globalized because used in two different type contexts"
				     
				     (format "~a/~a" (shape tail-coercion) (shape type))))
			      (set! tail-coercion #f))))))))
      A)
   ;; cleanup the A set according to the first traversal
   (map (lambda (a)
	   (match-case a
	      ((?caller ?callee (tail . ?type))
	       (let ((fun (variable-value callee)))
		  (with-access::sfun/Iinfo fun (tail-coercion)
		     (list caller
			callee
			(if (not tail-coercion) (get-new-kont) 'tail)))))
	      ((?caller ?callee (?kont . ?type))
	       (list caller callee kont))
	      (else
	       a)))
      A))

;*---------------------------------------------------------------------*/
;*    node-A ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (node-A node::node host::variable k::obj A))

;*---------------------------------------------------------------------*/
;*    node-A ::atom ...                                                */
;*---------------------------------------------------------------------*/
(define-method (node-A node::atom host k A)
   A)

;*---------------------------------------------------------------------*/
;*    node-A ::kwote ...                                               */
;*---------------------------------------------------------------------*/
(define-method (node-A node::kwote host k A)
   A)

;*---------------------------------------------------------------------*/
;*    node-A ::var ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (node-A node::var host k A)
   (if *optim-return-goto?*
       (with-access::var node (variable)
	  (let ((val (variable-value variable)))
	     (if (isa? val svar/Iinfo)
		 (with-access::svar/Iinfo val (xhdl)
		    (if xhdl
			(let ((nk (cons (get-new-kont) (get-type node #f))))
			   (cons `(,host ,xhdl ,nk) A))
			A))
		 A)))
       A))

;*---------------------------------------------------------------------*/
;*    node-A ::closure ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-A node::closure host k A)
   (internal-error "node-A" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    node-A ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-A node::sequence host k A)
   (with-access::sequence node (nodes) 
      (if (null? nodes)
	  A
	  (let liip ((nds nodes)
		     (A A))
	     (if (null? (cdr nds))
		 (node-A (car nds) host k A)
		 (liip (cdr nds)
		    (node-A (car nds)
		       host
		       (cons (get-new-kont) (get-type (car nds) #f))
		       A)))))))

;*---------------------------------------------------------------------*/
;*    node-A ::sync ...                                                */
;*---------------------------------------------------------------------*/
(define-method (node-A node::sync host k A)
   (with-access::sync node (body mutex prelock)
      (node-A body host k
	 (node-A prelock host (cons (get-new-kont) *obj*)
	    (node-A mutex host (cons (get-new-kont) *mutex*) A)))))

;*---------------------------------------------------------------------*/
;*    node-A ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (node-A node::app host k A)
   
   (define (set-exit-call node::node)
      (when *optim-return-goto?*
	 (with-access::app node (fun args)
	    (with-access::var fun (variable)
	       (when (is-unwind-until? variable)
		  (when (isa? (car args) var)
		     (with-access::var (car args) (variable)
			(let ((val (variable-value variable)))
			   (when (isa? val svar/Iinfo)
			      (with-access::svar/Iinfo val (xhdl)
				 xhdl))))))))))
   
   (define (is-exit-handler? callee)
      (when *optim-return-goto?*
	 (when (isa? (variable-value callee) sfun/Iinfo)
	    (with-access::sfun/Iinfo (variable-value callee) (xhdl?)
	       xhdl?))))
   
   (with-access::app node (fun)
      (let ((callee (var-variable fun)))
	 (cond
	    ((set-exit-call node)
	     =>
	     (lambda (callee) A))
	    (else
	     (let liip ((args (app-args node))
			(A A))
		(cond
		   ((null? args)
		    (cond
		       ((not (local? callee))
			A)
		       (else
			(cons `(,host ,callee ,k) A))))
		   (else
		    (liip (cdr args)
		       (node-A (car args)
			  host
			  (cons (get-new-kont) (get-type (car args) #f))
			  A))))))))))

;*---------------------------------------------------------------------*/
;*    node-A ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-A node::app-ly host k A)
   (with-access::app-ly node (fun arg)
      (node-A fun
	 host
	 (cons (get-new-kont) (get-type fun #f))
	 (node-A arg host (cons (get-new-kont) (get-type arg #f)) A))))

;*---------------------------------------------------------------------*/
;*    node-A ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-A node::funcall host k A)
   (with-access::funcall node (fun args)
      (node-A fun
	 host
	 (cons (get-new-kont) (get-type fun #f))
	 (let liip ((args args)
		    (A A))
	    (if (null? args)
		A
		(liip (cdr args)
		   (node-A (car args)
		      host (cons (get-new-kont) (get-type (car args) #f)) A)))))))

;*---------------------------------------------------------------------*/
;*    node-A ::extern ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-A node::extern host k A)
   (with-access::extern node (expr*)
      (let liip ((asts expr*)
		 (A A))
	 (if (null? asts)
	     A
	     (liip (cdr asts)
		(node-A (car asts)
		   host (cons (get-new-kont) (get-type (car asts) #f)) A))))))

;*---------------------------------------------------------------------*/
;*    node-A ::cast ...                                                */
;*---------------------------------------------------------------------*/
(define-method (node-A node::cast host k A)
   (with-access::cast node (arg)
      (node-A arg host (cons (get-new-kont) (get-type arg #f)) A)))

;*---------------------------------------------------------------------*/
;*    node-A ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (node-A node::setq host k A)
   (with-access::setq node (value)
      (node-A value host (cons (get-new-kont) (get-type value #f)) A)))

;*---------------------------------------------------------------------*/
;*    node-A ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (node-A node::conditional host k A)
   (with-access::conditional node (test true false)
      (let ((A (node-A test host (cons (get-new-kont) *bool*) A)))
	 (node-A true host k (node-A false host k A)))))

;*---------------------------------------------------------------------*/
;*    node-A ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (node-A node::fail host k A)
   (with-access::fail node (proc msg obj)
      (node-A proc
	 host
	 (cons (get-new-kont) proc)
	 (node-A msg
	    host
	    (cons (get-new-kont) (get-type msg #f))
	    (node-A obj
	       host (cons (get-new-kont) (get-type obj #f)) A)))))

;*---------------------------------------------------------------------*/
;*    node-A ::switch ...                                              */
;*---------------------------------------------------------------------*/
(define-method (node-A node::switch host k A)
   (with-access::switch node (test item-type)
      (let liip ((clauses (switch-clauses node))
		 (A (node-A test
		       host (cons (get-new-kont) item-type) A)))
	 (if (null? clauses)
	     A
	     (liip (cdr clauses)
		(node-A (cdr (car clauses)) host k A))))))

;*---------------------------------------------------------------------*/
;*    node-A ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-A node::let-fun host k A)

   (define (set-exit? node)
      (with-access::let-fun node (body locals)
	 (when (and (pair? locals) (null? (cdr locals)))
	    (let* ((var (car locals))
		   (fun (local-value var)))
	       (when (isa? (sfun-body fun) set-ex-it)
		  (when (isa? body app)
		     (with-access::app body (fun)
			(with-access::var fun (variable)
			   (eq? variable var)))))))))

   (define (mark-set-exit! node)
      (with-access::let-fun node (locals loc)
	 (with-access::sfun/Iinfo (local-value (car locals)) (forceG? xhdl?)
	    ;; MS 11oct2020: I'm not so sure that it is correct
	    ;; not to force globalization if not tail
	    (set! forceG?
	       (or (not *optim-return-goto?*) (not (eq? (car k) 'tail))))
	    (set! forceG?
	       (not *optim-return-goto?*))
	    (set! xhdl? #t))))
   
   (with-access::let-fun node (body)
      ;; we initialize all the local definitions
      (for-each (lambda (f)
		   (initialize-fun! f host)
		   (set! *phi* (cons f *phi*)))
	 (let-fun-locals node))
      ;; now, we scan the locals definitions and the body
      (let liip ((locals (let-fun-locals node))
		 (A A))
	 (if (null? locals)
	     (begin
		(when (set-exit? node) (mark-set-exit! node))
		(node-A body host k A))
	     (liip (cdr locals)
		(node-A (sfun-body (local-value (car locals)))
		   (car locals)
		   (cons 'tail (local-type (car locals)))
		   A))))))

;*---------------------------------------------------------------------*/
;*    node-A ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-A node::let-var host k A)
   
   (define (is-get-exitd-top-app? node)
      (when *optim-return-goto?*
	 (when (isa? node app)
	    (with-access::app node (fun args)
	       (with-access::var fun (variable)
		  (is-get-exitd-top? variable))))))
      
   (with-access::let-var node (body)
      (let liip ((bindings (let-var-bindings node))
		 (A A))
	 (if (null? bindings)
	     (node-A body host k A)
	     (let* ((binding (car bindings))
		    (var (car binding))
		    (val (cdr binding)))
		(if (is-get-exitd-top-app? val)
		    (widen!::svar/Iinfo (local-value var)
		       (xhdl host))
		    (widen!::svar/Iinfo (local-value var)))
		(liip (cdr bindings)
		   (node-A val
		      host (cons (get-new-kont) (local-type var)) A)))))))
 
;*---------------------------------------------------------------------*/
;*    node-A ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (node-A node::set-ex-it host k A)
   (with-access::set-ex-it node (var body)
      (let* ((exit (var-variable var))
	     (hdlg (sexit-handler (local-value exit))))
	 (widen!::sexit/Iinfo (local-value exit))
	 (when (and (not *optim-return-goto?*)
		    (not (sexit-detached? (local-value exit))))
	    (with-access::sfun/Iinfo (local-value hdlg) (forceG?)
	       (set! forceG? #t))))
      (node-A body host k A)))

;*---------------------------------------------------------------------*/
;*    node-A ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (node-A node::jump-ex-it host k A)
   (with-access::jump-ex-it node (exit value)
      (node-A exit
	 host
	 (cons (get-new-kont) (get-type exit #f))
	 (node-A value host (cons (get-new-kont) (get-type value #f)) A))))

;*---------------------------------------------------------------------*/
;*    node-A ::make-box ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-A node::make-box host k A)
   (with-access::make-box node (value)
      (node-A value host (cons (get-new-kont) (get-type value #f)) A)))

;*---------------------------------------------------------------------*/
;*    node-A ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (node-A node::box-set! host k A)
   (with-access::box-set! node (var value)
      (node-A value host (cons (get-new-kont) (get-type value #f)) A)))

;*---------------------------------------------------------------------*/
;*    node-A ::box-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (node-A node::box-ref host k A)
   A)

		
	    
