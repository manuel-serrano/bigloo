;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/let.scm                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  1 11:37:29 1995                          */
;*    Last change :  Mon Oct 18 08:35:59 2010 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The `let->ast' translator                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_let
   (include "Ast/node.sch"
	    "Tools/trace.sch"
	    "Tools/location.sch")
   (import  type_cache
	    tools_progn
	    tools_shape
	    tools_location
	    engine_param
	    ast_ident
	    ast_sexp
	    ast_local
	    ast_substitute
	    ast_occur
	    ast_remove
	    backend_backend)
   (export  (let-sym? ::obj)
	    (let-sym::symbol)
	    (let->node::node <sexp> <stack> ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    *let* ...                                                        */
;*---------------------------------------------------------------------*/
(define *let* (gensym 'let))

;*---------------------------------------------------------------------*/
;*    let-sym ...                                                      */
;*---------------------------------------------------------------------*/
(define (let-sym)
   *let*)

;*---------------------------------------------------------------------*/
;*    let-sym? ...                                                     */
;*---------------------------------------------------------------------*/
(define (let-sym? sym)
   (eq? sym *let*))

;*---------------------------------------------------------------------*/
;*    let->node ...                                                    */
;*---------------------------------------------------------------------*/
(define (let->node exp stack oloc site)
   (trace (ast 3)
	  "*** LET *******: " exp #\Newline
	  "            loc: " (find-location/loc exp #f) #\Newline
	  "        old-loc: " oloc #\Newline
	  "           body: " (match-case exp
				 ((?- ?- . ?body)
				  (find-location/loc body #f))
				 (else
				  '???))
	  #\Newline)
  (match-case exp
      ((?- () . ?body)
       ;; we don't remove explicit user let.
       (let* ((nloc (find-location/loc exp oloc))
	      (bloc (if (pair? body)
			(find-location/loc (car body) nloc)
			nloc))
	      (body (sexp->node (normalize-progn body) stack bloc site)))
	  (trace (ast 3)
		 "make-empty-let: " (shape exp) #\Newline
		 "bloc: " bloc #\Newline
		 "nloc: " nloc #\Newline)
	  (instantiate::let-var
	     (loc        nloc)
	     (type       (node-type body))
	     (bindings   '())
	     (body       body)
	     (removable? (backend-remove-empty-let (the-backend))))))
      ((?- ?bindings . ?-)
       (if (or (not (or (pair? bindings) (null? bindings)))
	       (let loop ((bindings bindings))
		  (if (null? bindings)
		      #f
		      (let ((binding (car bindings)))
			 (match-case binding
			    ((?- ?-)
			     (loop (cdr bindings)))
			    (else
			     #t))))))
	   (error-sexp->node (string-append "Illegal "
					    (symbol->string (car exp))
					    "' form")
			     exp
			     (find-location/loc exp oloc))
	   (make-smart-generic-let (car exp)
				   (make-generic-let exp stack oloc site)
				   site)))
      (else
       (error-sexp->node (string-append "Illegal "
					(symbol->string (car exp))
					"' form")
			 exp
			 (find-location/loc exp oloc)))))

;*---------------------------------------------------------------------*/
;*    make-generic-let ...                                             */
;*---------------------------------------------------------------------*/
(define (make-generic-let exp stack oloc site)
   (let* ((bindings   (cadr exp))
	  (loc        (find-location/loc exp oloc))
	  (bloc       (if (pair? (cddr exp))
			  (find-location/loc (caddr exp) #f)
			  #f))
	  (bloc-exp   (if (pair? (cddr exp))
			  (caddr exp)
			  #f))
	  (body       (normalize-progn (cddr exp)))
	  (loc-bis    (find-location/loc body loc))
	  (nloc       (if (location? bloc)
			  bloc
			  loc))
	  (frame      (map (lambda (binding)
			      (let* ((var.id (parse-id (car binding) nloc))
				     (id     (car var.id))
				     (type   (cdr var.id)))
				 (if (user-symbol? id)
				     (make-user-local-svar id type)
				     (make-local-svar id type))))
			   bindings))
	  (new-stack  (append frame stack)))
      (trace (ast 3)
	     "make-generic-let: " (shape exp) #\Newline
	     "loc: " loc #\Newline
	     "bloc: " bloc "[exp: " bloc-exp "]" #\Newline
	     "loc-bis: " loc-bis #\Newline
	     "nloc: " nloc #\Newline)
      (let* ((body     (sexp->node body new-stack nloc 'value))
	     (bstack   (if (or (eq? (car exp) 'let) (let-sym? (car exp)))
			   stack
			   new-stack))
	     (bindings (map (lambda (binding var)
			       (cons var
				     (sexp->node
				      (normalize-progn (cdr binding))
				      bstack
				      (find-location/loc binding nloc)
				      'value)))
			    bindings
			    frame))
	     (loc      (let ((loc (find-location/loc
				   exp 
				   (if (pair? bindings)
				       (node-loc (cdr (car bindings)))
				       (node-loc body)))))
			  (if (location? loc)
			      loc
			      oloc)))
	     (node (instantiate::let-var
		      (loc loc)
		      (type *_*)
		      (bindings bindings)
		      (body body))))
	 (occur-node! node)
	 (node-remove! node)
	 node)))

;*---------------------------------------------------------------------*/
;*    make-smart-generic-let ...                                       */
;*    -------------------------------------------------------------    */
;*    We patch bindings which concerns a function and where the        */
;*    variable is never mutated. These bindings are put all together   */
;*    in a labels form.                                                */
;*    -------------------------------------------------------------    */
;*    We try to apply the following transformation:                    */
;*    (let (... (f (labels ((aux args body)) aux)) ...) ...)           */
;*       -->                                                           */
;*    (labels ((f args body)) (let (...) ...))                         */
;*---------------------------------------------------------------------*/
(define (make-smart-generic-let let/letrec node-let site)
   (let loop ((bindings (let-var-bindings node-let))
	      (fun      '())
	      (value    '()))
      (if (null? bindings)
	  (begin
	     (trace (ast 3)
		    "make-smart-generic-let: " (shape node-let) #\Newline
		    "    fun: " (length fun) #\Newline
		    "    values: " (length value) #\Newline)
	     (cond
		((null? fun)
		 (let ((vars (map car (let-var-bindings node-let))))
		    (let-or-letrec let/letrec node-let vars)))
		((null? value)
		 (let->labels fun (let-var-body node-let) site))
		(else
		 (let ((vars (map car (let-var-bindings node-let))))
		    ;; first we ajust let-var bindings
		    (let-var-bindings-set! node-let (reverse! value))
		    ;; then, we send the let form to the `let-or-letrec'
		    ;; function
		    (let ((nlet (let-or-letrec let/letrec node-let vars)))
		       (let-var-body-set! nlet (let->labels fun
							    (let-var-body nlet)
							    site))
		       nlet)))))
	  (let* ((binding (car bindings))
		 (var     (car binding))
		 (sexp    (cdr binding)))
	     (if (let-fun? sexp)
		 (let* ((locals (let-fun-locals sexp))
			(body   (let-fun-body   sexp)))
		    (if (or (null? locals) (not (null? (cdr locals))))
			;; several functions are introduced by the let-fun
			;; construction or, the body of the construction
			;; include several forms. We skip ...
			(loop (cdr bindings)
			      fun
			      (cons (car bindings) value))
			(if (var? body)
			    (let ((res (var-variable body))
				  (aux (car locals)))
			       (if (or (not (eq? res aux))
				       ;; the result of the labels
				       ;; construction is not the
				       ;; introduced variable.
				       (eq? (local-access var) 'write)
				       (not (or
					     (eq? (local-type var) *procedure*)
					     (eq? (local-type var) *_*)
					     (eq? (local-type var) *obj*))))
				   ;; the variable is mutated
				   ;; we skip
				   (loop (cdr bindings)
					 fun
					 (cons (car bindings) value))
				   ;; yes, we have found one
				   (loop (cdr bindings)
					 (cons (car bindings) fun)
					 value)))
			    (loop (cdr bindings)
				  fun
				  (cons (car bindings) value)))))
		 (loop (cdr bindings)
		       fun
		       (cons (car bindings) value)))))))
	      
;*---------------------------------------------------------------------*/
;*    let-or-letrec ...                                                */
;*    -------------------------------------------------------------    */
;*    Let differ from letrec in the sens that in a letrec form all     */
;*    bindings must be introduces by the unspecified value and         */
;*    it must exists an initialization stage which initialize all      */
;*    introduced local variables. This means that in a letrec form     */
;*    all variables have to be bound to unspecified then, they have    */
;*    to be mutated to their correct values.                           */
;*---------------------------------------------------------------------*/
(define (let-or-letrec let/letrec node-let vars)
   
   (define (safe-rec-val? val)
      (or (atom? val) (closure? val) (kwote? val)
	  (and (sequence? val) (every? safe-rec-val? (sequence-nodes val)))))

   (define (safe-rec-val-optim? val vars::pair-nil)
      (or (safe-rec-val? val)
	  (cond
	     ((var? val)
	      (not (memq (var-variable val) vars)))
	     ((sequence? val)
	      (safe-rec-val-optim? (sequence-nodes val) vars))
	     ((app? val)
	      (with-access::app val (fun args)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? args vars))))
	     ((pair? val)
	      (every? (lambda (v) (safe-rec-val-optim? v vars)) val))
	     ((app-ly? val)
	      (with-access::app-ly val (fun arg)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? arg vars))))
	     ((funcall? val)
	      (with-access::funcall val (fun args)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? args vars))))
	     ((extern? val)
	      (with-access::extern val (expr*)
		 (safe-rec-val-optim? expr* vars)))
	     ((conditional? val)
	      (with-access::conditional val (test true false)
		 (and (safe-rec-val-optim? test vars)
		      (safe-rec-val-optim? true vars)
		      (safe-rec-val-optim? false vars))))
	     ((setq? val)
	      (with-access::setq val (var value)
		 (and (safe-rec-val-optim? var vars)
		      (safe-rec-val-optim? value vars))))
	     ((fail? val)
	      (with-access::fail val (proc msg obj)
		 (and (safe-rec-val-optim? proc vars)
		      (safe-rec-val-optim? msg vars)
		      (safe-rec-val-optim? obj vars))))
	     ((select? val)
	      (with-access::select val (test clauses)
		 (and (safe-rec-val-optim? test vars)
		      (every? (lambda (clause)
				 (safe-rec-val-optim? (cdr clause) vars))
			      clauses))))
	     ((let-fun? val)
	      (with-access::let-fun val (body locals)
		 (and (safe-rec-val-optim? body vars)
		      (every? (lambda (f)
				 (safe-rec-val-optim?
				  (sfun-body (local-value f)) vars))
			      locals))))
	     ((let-var? val)
	      (with-access::let-var val (body bindings)
		 (and (safe-rec-val-optim? body vars)
		      (every? (lambda (binding)
				 (safe-rec-val-optim? (cdr binding) vars))
			      bindings))))
	     ((set-ex-it? val)
	      (with-access::set-ex-it val (var body)
		 (and (safe-rec-val-optim? var vars)
		      (safe-rec-val-optim? body vars))))
	     ((jump-ex-it? val)
	      (with-access::jump-ex-it val (exit value)
		 (and (safe-rec-val-optim? exit vars)
		      (safe-rec-val-optim? value vars))))
	     ((make-box? val)
	      (with-access::make-box val (value)
		 (safe-rec-val-optim? value vars)))
	     ((box-ref? val)
	      (with-access::box-ref val (var)
		 (safe-rec-val-optim? var vars)))
	     ((box-set!? val)
	      (with-access::box-set! val (var value)
		 (and (safe-rec-val-optim? var vars)
		      (safe-rec-val-optim? value vars))))
	     (else
	      #f))))

   (define (safe-let? node)
      (with-access::let-var node (bindings)
	 (every? (lambda (b) (safe-rec-val? (cdr b))) bindings)))
   
   (define (safe-let-optim? node)
      (with-access::let-var node (bindings)
	 (every? (lambda (b)
		    (when (eq? (variable-access (car b)) 'read)
		       (safe-rec-val-optim? (cdr b) vars)))
		 bindings)))

   (cond
      ((or (eq? let/letrec 'let) (let-sym? let/letrec))
       node-let)
      ((safe-let? node-let)
       node-let)
      ((and (>=fx *optim* 1) (not *call/cc?*) (safe-let-optim? node-let))
       node-let)
      ((eq? let/letrec 'letrec*)
       (let* ((bindings (let-var-bindings node-let))
	      (body     (let-var-body node-let))
	      (seq      (if (sequence? body)
			    body
			    (instantiate::sequence
			       (loc (node-loc body))
			       (type  *_*)
			       (nodes (list body))))))
	  (let-var-body-set! node-let seq)
	  (let loop ((bindings  bindings)
		     (nsequence '()))
	     (if (null? bindings)
		 (begin
		    (let-var-body-set! node-let
				       (instantiate::sequence
					  (loc   (node-loc seq))
					  (type  *_*)
					  (nodes (append (reverse! nsequence)
							 (sequence-nodes seq)))))
		    node-let)
		 (let* ((binding (car bindings))
			(var     (car binding))
			(val     (cdr binding))
			(loc     (node-loc val)))
		    (let ((init (instantiate::setq
				   (loc loc)
				   (type *unspec*)
				   (var (instantiate::var
					   (type *_*)
					   (loc loc)
					   (variable var)))
				   (value val))))
		       (use-variable! var loc 'set!)
		       (set-cdr! binding
				 (sexp->node #unspecified '() loc 'value))
		       (loop (cdr bindings)
			     (cons init nsequence))))))))
      (else
       (let* ((bindings (let-var-bindings node-let))
	      (body     (let-var-body node-let))
	      (seq      (if (sequence? body)
			    body
			    (instantiate::sequence
			       (loc (node-loc body))
			       (type  *_*)
			       (nodes (list body))))))
	  (let-var-body-set! node-let seq)
	  (let loop ((bindings  bindings)
		     (nbindings '())
		     (nsequence (sequence-nodes seq)))
	     (if (null? bindings)
		 (let* ((seq (instantiate::sequence
				(loc (node-loc seq))
				(type  *_*)
				(nodes nsequence)))
			(letb (instantiate::let-var
				 (loc (node-loc node-let))
				 (type (node-type node-let))
				 (bindings nbindings)
				 (body seq)
				 (removable? #t))))
		    (let-var-body-set! node-let letb)
		    node-let)
		 (let* ((binding (car bindings))
			(var     (car binding))
			(val     (cdr binding))
			(loc     (node-loc val))
			(nvar    (make-local-svar (gensym) *_*)))
		    (let ((init (instantiate::setq
				   (loc loc)
				   (type *unspec*)
				   (var (instantiate::var
					   (type *_*)
					   (loc loc)
					   (variable var)))
				   (value (instantiate::var
					     (type *_*)
					     (loc loc)
					     (variable nvar))))))
		       (use-variable! var loc 'set!)
		       (use-variable! nvar loc 'set!)
		       (set-cdr! binding
				 (sexp->node #unspecified '() loc 'value))
		       (loop (cdr bindings)
			     (cons (cons nvar val) nbindings)
			     (cons init nsequence))))))))))
 
;*---------------------------------------------------------------------*/
;*    let->labels ...                                                  */
;*    -------------------------------------------------------------    */
;*    This function creates a `labels' construction for variables      */
;*    introduced in a `let' form which are never mutated and bound     */
;*    to function.                                                     */
;*---------------------------------------------------------------------*/
(define (let->labels value-bindings node site)
   ;; we compute (allocate) the list of new functions
   (let ((old-funs (map car value-bindings))
	 (new-funs (map (lambda (binding)
			   (let* ((ovar (car binding))
				  (val  (cdr binding))
				  (aux  (car (let-fun-locals val)))
				  (id   (if (local-user? ovar)
					    (local-id ovar)
					    (local-id aux)))
				  (new  (make-local-svar id (local-type aux))))
			      (local-value-set! new (local-value aux))
			      (local-user?-set! new (or (local-user? aux)
							(local-user? ovar)))
			      (local-name-set! new (local-name aux))
			      new))
			value-bindings)))
      (let loop ((vbindings value-bindings)
		 (nvars     new-funs))
	 (if (null? vbindings)
	     ;; the call to substitute! has only the goal to introduce
	     ;; the `closure' constructions
	     (let* ((body (substitute! old-funs new-funs node site))
		    (funs (reverse! new-funs))
		    (loc  (if (and (pair? funs)
				   (sfun? (local-value (car funs))))
			      (node-loc (sfun-body (local-value (car funs))))
			      (node-loc node))))
		(instantiate::let-fun
		   (loc    loc)
		   (type   (node-type node))
		   (locals funs)
		   (body   body)))
	     ;; we style have to alpha-convert the body of `var'
	     (let* ((binding (car vbindings))
		    (nvar    (car nvars))
		    (sfun    (local-value nvar))
		    (body    (sfun-body sfun))
		    (val     (cdr binding))
		    (aux     (car (let-fun-locals val))))
		(sfun-body-set! sfun
				(substitute! (cons aux old-funs)
					     (cons nvar new-funs)
					     body
					     'value))
		;; ok, it is finished, we loop now.
		(loop (cdr vbindings)
		      (cdr nvars)))))))
