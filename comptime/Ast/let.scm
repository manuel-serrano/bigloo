;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Ast/let.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  1 11:37:29 1995                          */
;*    Last change :  Sat Jan 11 10:10:36 2020 (serrano)                */
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
	    tools_misc
	    engine_param
	    ast_ident
	    ast_sexp
	    ast_local
	    ast_substitute
	    ast_occur
	    ast_remove
	    ast_dump
	    object_class
	    backend_backend)
   (export  (let-sym? ::obj)
	    (let-sym::symbol)
	    (let->node::node <sexp> <stack> ::obj ::symbol)
	    (letrec*->node::node <sexp> <stack> ::obj ::symbol)))

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
      "*** LET *******: " (shape exp) #\Newline
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
	      (body (sexp->node (normalize-progn body) stack bloc 'value)))
	  (trace (ast 3)
	     "make-empty-let: " (shape exp) #\Newline
	     "bloc: " bloc #\Newline
	     "nloc: " nloc #\Newline)
	  (instantiate::let-var
	     (loc nloc)
	     (type (strict-node-type *_* (node-type body)))
	     (bindings '())
	     (body body)
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
	   (error-sexp->node
	      (string-append "Illegal `" (symbol->string (car exp)) "' form")
	      exp
	      (find-location/loc exp oloc))
	   (make-smart-generic-let
	      (car exp)
	      (make-generic-let exp stack oloc site)
	      site)))
      (else
       (error-sexp->node
	  (string-append "Illegal `" (symbol->string (car exp)) "' form")
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
			      (let* ((var-id (parse-id (car binding) nloc))
				     (id (car var-id))
				     (type (cdr var-id)))
				 (if (user-symbol? id)
				     (make-user-local-svar id type)
				     (make-local-svar id type))))
			   bindings))
	  (new-stack  (append frame stack)))
      (trace (ast 3)
	     "make-generic-let: " (shape exp) #\Newline
	     "loc: " loc #\Newline
	     "bloc: " bloc " [exp: " (shape bloc-exp) "]" #\Newline
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
;*    We patch the bindings that concern a function and where the      */
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
		    (let* ((nlet (let-or-letrec let/letrec node-let vars))
			   (nbody (let->labels fun (let-var-body nlet) site)))
		       (with-access::let-var nlet (body type)
			  (set! body nbody)
			  (set! type (strict-node-type *_* type)))
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
				   ;; the variable is mutated, skip it
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
;*    Let differs from letrec in the sense that in a letrec form all   */
;*    bindings must be introduced by the unspecified value and         */
;*    it must exists an initialization stage which initializes all     */
;*    introduced local variables. This means that in a letrec form     */
;*    all variables have to be bound to unspecified then, they have    */
;*    to be mutated to their correct values.                           */
;*---------------------------------------------------------------------*/
(define (let-or-letrec let/letrec node-let vars)
   
   (define (safe-rec-val? val)
      (or (atom? val) (closure? val) (kwote? val)
	  (and (sequence? val) (every safe-rec-val? (sequence-nodes val)))))

   (define (safe-rec-val-optim? val vars::pair-nil)
      (or (safe-rec-val? val)
	  (cond
	     ((null? val)
	      #t)
	     ((atom? val)
	      #t)
	     ((var? val)
	      (not (memq (var-variable val) vars)))
	     ((sequence? val)
	      (safe-rec-val-optim? (sequence-nodes val) vars))
	     ((app? val)
	      (with-access::app val (fun args)
		 (and (safe-rec-val-optim? fun vars)
		      (safe-rec-val-optim? args vars))))
	     ((pair? val)
	      (every (lambda (v) (safe-rec-val-optim? v vars)) val))
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
		 (every (lambda (e)
			   (safe-rec-val-optim? e vars))
		    expr*)))
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
	     ((switch? val)
	      (with-access::switch val (test clauses)
		 (and (safe-rec-val-optim? test vars)
		      (every (lambda (clause)
				(safe-rec-val-optim? (cdr clause) vars))
			 clauses))))
	     ((let-fun? val)
	      (with-access::let-fun val (body locals)
		 (and (safe-rec-val-optim? body vars)
		      (every (lambda (f)
				(safe-rec-val-optim?
				   (sfun-body (local-value f)) vars))
			 locals))))
	     ((let-var? val)
	      (with-access::let-var val (body bindings)
		 (and (safe-rec-val-optim? body vars)
		      (every (lambda (binding)
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
	 (every (lambda (b) (safe-rec-val? (cdr b))) bindings)))
   
   (define (safe-let-optim? node)
      (with-access::let-var node (bindings)
	 (cond
	    ((null? bindings)
	     #t)
	    ((null? (cdr bindings))
	     (safe-rec-val-optim? (cdar bindings) vars))
	    (else
	     (every (lambda (b)
		       (when (eq? (variable-access (car b)) 'read)
			  (safe-rec-val-optim? (cdr b) vars)))
		bindings)))))

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
			       (type *_*)
			       (nodes (list body))))))
	  (let-var-body-set! node-let seq)
	  (let loop ((bindings  bindings)
		     (nsequence '()))
	     (if (null? bindings)
		 (begin
		    (let-var-body-set!
		     node-let
		     (instantiate::sequence
			(loc (node-loc seq))
			(type *_*)
			(nodes (append (reverse! nsequence)
				       (sequence-nodes seq)))))
		    node-let)
		 (let* ((binding (car bindings))
			(var (car binding))
			(val (cdr binding))
			(loc (node-loc val)))
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
			       (type *_*)
			       (nodes (list body))))))
	  (let-var-body-set! node-let seq)
	  (let loop ((bindings  bindings)
		     (nbindings '())
		     (nsequence (sequence-nodes seq)))
	     (if (null? bindings)
		 (let* ((typ (if (pair? nsequence)
				 (node-type (car (last-pair nsequence)))
				 *unspec*))
			(seq (instantiate::sequence
				(loc (node-loc seq))
				(type *_*)
				(nodes nsequence)))
			(letb (instantiate::let-var
				 (loc (node-loc node-let))
				 (type (strict-node-type *_* (node-type node-let)))
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
;*    to functions.                                                    */
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
		   (loc loc)
		   (type (node-type node))
		   (locals funs)
		   (body body)))
	     ;; we style have to alpha-convert the body of `var'
	     (let* ((binding (car vbindings))
		    (nvar (car nvars))
		    (sfun (local-value nvar))
		    (body (sfun-body sfun))
		    (val (cdr binding))
		    (aux (car (let-fun-locals val))))
		(sfun-body-set! sfun
				(substitute! (cons aux old-funs)
					     (cons nvar new-funs)
					     body
					     'value))
		;; ok, it is finished, we loop now.
		(loop (cdr vbindings) (cdr nvars)))))))

;*---------------------------------------------------------------------*/
;*    letrec*->node ...                                                */
;*    -------------------------------------------------------------    */
;*    Decompose a letrec* in a let* and a labels, i.e.,                */
;*      (LETREC* ((f1 (lambda (x) ...))                                */
;*    	          (f2 (lambda (y) ...))                                */
;*    	          (v1 i1)                                              */
;*    	          (v2 i2)                                              */
;*    	          (f3 (lambda (y) ...))                                */
;*    	          (v3 i3)                                              */
;*    	          ...)                                                 */
;*         body)                                                       */
;*    ==>                                                              */
;*      (LET* ((v1 i1)                                                 */
;*    	       (v2 i2)                                                 */
;*    	       (v3 i1))                                                */
;*         (LETREC ((f1 (lambda (x) ...))                              */
;*    	            (f2 (lambda (x) ...))                              */
;*    	            (f3 (lambda (x) ...)))                             */
;*    	body))                                                         */
;*---------------------------------------------------------------------*/
(define (letrec*->node sexp stack loc site)

   (define (binding->ebinding b v vars)
      (make-ebinding b v vars))

   (define (make-ebinding b v vars)
      ;; ebinding: < binding, variable, free vars, set vars >
      (list b v
	 (free-vars (cadr b) v vars '())
	 (set-vars (cadr b) v vars '())))

   (define (ebinding-binding b) (car b))
   (define (ebinding-value b) (cadr (ebinding-binding b)))
   (define (ebinding-var b) (cadr b))
   (define (ebinding-frees b) (caddr b))
   (define (ebinding-setvs b) (cadddr b))

   (define (mutable-in? b ebindings::pair-nil)
      ;; is b mutated in any of the ebindings
      (let ((var (ebinding-var b)))
	 (find (lambda (eb)
		  (memq var (ebinding-setvs eb)))
	    ebindings)))

   (define (immutable-in? b ebindings::pair-nil)
      (not (mutable-in? b ebindings)))

   (define (used-in? b ebindings::pair-nil)
      (let ((var (ebinding-var b)))
	 (find (lambda (eb)
		  (memq var (ebinding-frees eb)))
	    ebindings)))

   (define (letstar ebindings body)
      (cond
	 ((null? ebindings)
	  body)
	 ((used-in? (car ebindings) (list (car ebindings)))
	  `(letrec (,(ebinding-binding (car ebindings)))
	      ,(letstar (cdr ebindings) body)))
	 (else
	  `(let (,(ebinding-binding (car ebindings)))
	      ,(letstar (cdr ebindings) body)))))

   (define (letstarcollapse expr)
      (let loop ((expr expr)
		 (bindings '()))
	 (match-case expr
	    ((let ((and ?binding (?- ?val))) ?subexpr)
	     (cond
		((or (constant? val) (function? val))
		 (loop subexpr (cons binding bindings)))
		((null? bindings)
		 `(let (,binding) ,(letcollapse subexpr)))
		(else
		 `(let ,(reverse bindings)
		     (let (,binding) ,(letcollapse subexpr))))))
	    (else
	     (if (pair? bindings)
		 `(let ,(reverse bindings) ,expr)
		 expr)))))

   (define (letreccollapse expr)
      (let loop ((expr expr)
		 (bindings '()))
	 (match-case expr
	    ((letrec ((and ?binding (?- ?val))) ?subexpr)
	     (cond
		((function? val)
		 (loop subexpr (cons binding bindings)))
		((null? bindings)
		 `(letrec (,binding) ,(letcollapse subexpr)))
		(else
		 `(letrec ,(reverse bindings)
		     (letrec (,binding) ,(letcollapse subexpr))))))
	    (else
	     (if (pair? bindings)
		 `(letrec ,(reverse bindings) ,expr)
		 expr)))))
   
   (define (labelscollapse expr)
      (tprint "labels collapse val=" expr)
      expr)
   
   (define (letcollapse expr)
      (match-case expr
	 ((let . ?-) (letstarcollapse expr))
	 ((letrec . ?-) (letreccollapse expr))
	 ((labels . ?-) (labelscollapse expr))
	 (else expr)))

   (define (letrecursive ebindings body)
      (cond
	 ((null? ebindings)
	  body)
	 ((used-in? (car ebindings) ebindings)
	  `(letrec ,(map ebinding-binding ebindings)
	      ,body))
	 (else
	  `(let (,(ebinding-binding (car ebindings)))
	      ,(letrecursive (cdr ebindings) body)))))
   
   (define (split-head-letrec ebindings body split::procedure kont)
      (cond
	 ((null? ebindings)
	  (sexp->node body stack loc site))
	 (else
	  (multiple-value-bind (rec-bindings rec*-bindings)
	     (split ebindings)
	     (trace-item "rec="
		(map (lambda (x) (shape (ebinding-var x))) rec-bindings))
	     (trace-item "rec*="
		(map (lambda (x) (shape (ebinding-var x))) rec*-bindings))
	     (if (pair? rec-bindings)
		 (sexp->node
		    (letcollapse
		       (letrecursive rec-bindings
			  `(letrec* ,(map ebinding-binding rec*-bindings)
			      ,body)))
		    stack loc site)
		 (kont ebindings body))))))

   (define (split-head-let* ebindings body split::procedure kont)
      (cond
	 ((null? ebindings)
	  (sexp->node body stack loc site))
	 (else
	  (multiple-value-bind (let-bindings rec*-bindings)
	     (split ebindings)
	     (trace-item "let="
		(map (lambda (x) (shape (ebinding-var x))) let-bindings))
	     (trace-item "rec*="
		(map (lambda (x) (shape (ebinding-var x))) rec*-bindings))
	     (if (pair? let-bindings)
		 (sexp->node
		    (letcollapse
		       (letstar let-bindings
			  `(letrec* ,(map ebinding-binding rec*-bindings)
			      ,body)))
		    stack loc site)
		 (kont ebindings body))))))

   (define (split-tail-letrec ebindings body split::procedure kont)
      (cond
	 ((null? ebindings)
	  (sexp->node body stack loc site))
	 (else
	  (multiple-value-bind (rec*-bindings tail-bindings)
	     (split ebindings)
	     (trace-item "rec*="
		(map (lambda (x) (shape (ebinding-var x))) rec*-bindings))
	     (trace-item "tail="
		(map (lambda (x) (shape (ebinding-var x))) tail-bindings))
	     (if (pair? tail-bindings)
		 (sexp->node
		    `(letrec* ,(map ebinding-binding rec*-bindings)
			(letrec* ,(map ebinding-binding tail-bindings)
			   ,body))
		    stack loc site)
		 (kont ebindings body))))))

   (define (split-tail-let* ebindings body split::procedure kont)
      (cond
	 ((null? ebindings)
	  (sexp->node body stack loc site))
	 (else
	  (multiple-value-bind (rec*-bindings tail-bindings)
	     (split ebindings)
	     (trace-item "rec*="
		(map (lambda (x) (shape (ebinding-var x))) rec*-bindings))
	     (trace-item "tail="
		(map (lambda (x) (shape (ebinding-var x))) tail-bindings))
	     (cond
		((null? tail-bindings)
		 (kont ebindings body))
		((pair? rec*-bindings)
		 (sexp->node
		    `(letrec* ,(map ebinding-binding rec*-bindings)
			,(letstar tail-bindings body))
		    stack loc site))
		(else
		 (sexp->node
		    (letstar tail-bindings body)
		    stack loc site)))))))
   
   (define (type-undefined type)
      (if (tclass? type)
	  (let ((v (tclass-holder type)))
	     `(class-nil (@ ,(global-id v) ,(global-module v))))
	  (case (type-id type)
	     ((obj _) #unspecified)
	     ((int8) #s8:0)
	     ((uint8) #u8:0)
	     ((int16) #s16:0)
	     ((uint16) #u16:0)
	     ((int32) #s32:0)
	     ((uint32) #u32:0)
	     ((int64) #s64:0)
	     ((uint64) #u64:0)
	     ((double real) 0.0)
	     ((int long) 0)
	     ((bool) #f)
	     ((procedure) 'list)
	     ((cell) `(make-cell #f))
	     ((char) #a000)
	     (else (error "type-undefined" "cannot undefined type" (type-id type))))))

   (define (untyped-id id)
      (fast-id-of-id id #f))
   
   (define (append* pair lst)
      (cond
	 ((null? pair)
	  lst)
	 ((pair? pair)
	  (cons (untyped-id (car pair)) (append* (cdr pair) lst)))
	 (else
	  (cons (untyped-id pair) lst))))
	  
   (define (free-vars sexp v vars env)
      ;; compute an over-approximation of all the
      ;; free vars appearing in sexp
      (let loop ((sexp sexp)
		 (res '())
		 (env env))
	 (match-case sexp
	    ((? symbol?)
	     (cond
		((not (memq sexp vars)) res)
		((memq sexp env) res)
		((memq sexp res) res)
		(else (cons sexp res))))
	    ((not (? pair?))
	     res)
	    (((kwote quote) ?-) res)
	    ((let ((?v ?val)) ?body)
	     (loop body (loop val res env) (cons (untyped-id v) env)))
	    ((let* ((?v ?val)) ?body)
	     (loop body (loop val res env) (cons (untyped-id v) env)))
	    ((letrec ((?v ?val)) ?body)
	     (let ((nenv (cons (untyped-id v) env)))
		(loop body (loop val res nenv) nenv)))
	    ((letrec* ((?v ?val)) ?body)
	     (let ((nenv (cons (untyped-id v) env)))
		(loop body (loop val res nenv) nenv)))
	    ((labels ((?v ?vars ?fun)) ?body)
	     (let ((fenv (cons* (untyped-id v) (append* vars env))))
		(loop body (loop fun res fenv) (cons v env))))
	    (else
	     (loop (car sexp) (loop (cdr sexp) res env) env)))))

   (define (set-vars sexp v vars env)
      ;; compute an over-approximation of all the
      ;; free vars assigned in sexp
      (let loop ((sexp sexp)
		 (res '())
		 (env env))
	 (match-case sexp
	    ((not (? pair?))
	     res)
	    (((kwote quote) ?-) res)
	    ((set! (and ?var (? symbol?)) ?val)
	     (cond
		((or (eq? sexp v) (not (memq var vars))) (loop var res env))
		((memq var res) (loop val res env))
		((memq var env) (loop val res env))
		(else (loop val (cons var res) env))))
	    (((kwote quote) ?-) res)
	    ((let ((?v ?val)) ?body)
	     (loop body (loop val res env) (cons (untyped-id v) env)))
	    ((let* ((?v ?val)) ?body)
	     (loop body (loop val res env) (cons (untyped-id v) env)))
	    ((letrec ((?v ?val)) ?body)
	     (let ((nenv (cons (untyped-id v) env)))
		(loop body (loop val res nenv) nenv)))
	    ((letrec* ((?v ?val)) ?body)
	     (let ((nenv (cons (untyped-id v) env)))
		(loop body (loop val res nenv) nenv)))
	    ((labels ((?v ?vars ?fun)) ?body)
	     (let ((fenv (cons* (untyped-id v) (append* vars env))))
		(loop body (loop fun res fenv) (cons v env))))
	    (else
	     (loop (car sexp) (loop (cdr sexp) res env) env)))))
   
   (define (stage8 ebindings body)
      ;; a true letrec*
      (with-trace 'letrec* "letrec*/stage8"
	 (trace-item "bindings="
	    (map (lambda (b) (shape (ebinding-var b))) ebindings))
	 (let ((sexp `(let ,(map (lambda (b)
				    (let ((ty (type-of-id (caar b) (find-location (car b)))))
				       (list (caar b) (type-undefined ty))))
			       ebindings)
			 ,@(map (lambda (b)
				   `(set! ,(fast-id-of-id (caar b) loc)
				       ,(cadr (car b))))
			      ebindings)
			 ,body)))
	    (sexp->node sexp stack loc site))))

   (define (stage7 ebindings body)
      ;; if the last binding is not a function and if it is not typed,
      ;; bind it to unspecified at the beginning of the letrec*
      (with-trace 'letrec* "letrec*/stage7"
	 (let* ((last (car (last-pair ebindings)))
		(t (type-of-id (car (ebinding-binding last))
		      (find-location (ebinding-binding last)))))
	    (if (and (memq (type-id t) '(_ obj))
		     (not (function? (ebinding-value last))))
		(begin
		   (trace-item "last=" (ebinding-var last))
		   (sexp->node
		      `(let ((,(ebinding-var last) #unspecified))
			  (letrec* ,(reverse
				       (cdr (map ebinding-binding
					       (reverse ebindings))))
			     (set! ,(ebinding-var last) ,(ebinding-value last))
			     ,body))
		      stack loc site))
		(begin
		   (trace-item "no last t=" (type-id t))
		   (stage8 ebindings body))))))

   (define (stage6 ebindings body)
      ;; move downward tail bindings that never appear in head bindings

      (define (split ebindings)
	 (with-trace 'letrec* "letrec*/stage6/split"
	    (let loop ((ebindings (reverse ebindings))
		       (rec*-bindings '())
		       (let-bindings '()))
	       (cond
		  ((null? ebindings)
		   (values rec*-bindings let-bindings))
		  ((immutable-in? (car ebindings) (cdr ebindings))
		   (cond
		      ((function? (ebinding-value (car ebindings)))
		       (loop (cdr ebindings)
			  (cons (car ebindings) rec*-bindings)
			  let-bindings))
		      ((and (not (used-in? (car ebindings) rec*-bindings))
			    (not (used-in? (car ebindings) (cdr ebindings))))
		       (loop (cdr ebindings)
			  rec*-bindings
			  (cons (car ebindings) let-bindings)))
		      (else
		       (trace-item "abort.1=" (ebinding-var (car ebindings)))
		       (values (append (reverse ebindings) rec*-bindings)
			  let-bindings))))
		  (else
		   ;; one of the variables introduced in the next bindings
		   ;; appears free in the value of the current binding
		   (trace-item "abort.2=" (ebinding-var (car ebindings)))
		   (values (append (reverse ebindings) rec*-bindings)
		      let-bindings))))))
      
      (with-trace 'letrec* "letrec*/stage6"
	 (split-tail-letrec ebindings body split stage7)))

   (define (stage5 ebindings body)
      ;; split the values and the functions
      
      (define (split ebindings)
	 ;; try to partition the bindings putting variables first
	 ;; and functions second
	 (with-trace 'letrec* "letrec*/stage5/split"
	    (let loop ((l ebindings)
		       (vbindings '())
		       (fbindings '()))
	       (cond
		  ((null? l)
		   ;; check that all the fbindings are free in the vbindings
		   (let ((funs (map cadr fbindings))
			 (vars (map cadr vbindings)))
		      (let ((frees (append-map ebinding-frees vbindings))
			    (sets (append-map ebinding-setvs fbindings)))
			 (if (any (lambda (b) (memq (ebinding-var b) frees))
				fbindings)
			     (values '() ebindings)
			     (values (reverse! vbindings)
				(reverse! fbindings))))))
		  ((function? (ebinding-value (car l)) #t)
		   (loop (cdr l) vbindings (cons (car l) fbindings)))
		  (else
		   (loop (cdr l) (cons (car l) vbindings) fbindings))))))
      
      (with-trace 'letrec* "letrec*/stage5"
	 (cond
	    ((null? ebindings)
	     (sexp->node body stack loc site))
	    (else
	     (multiple-value-bind (vbindings fbindings)
		;; split values/functions
		(split ebindings)
		(trace-item "vals="
		   (map (lambda (x) (shape (ebinding-var x))) vbindings))
		(trace-item "funs="
		   (map (lambda (x) (shape (ebinding-var x))) fbindings))
		(if (and (pair? vbindings) (pair? fbindings))
		    (sexp->node
		       `(letrec* ,(map ebinding-binding vbindings)
			   (letrec* ,(map ebinding-binding fbindings)
			      ,body))
		       stack loc site)
		    ;; true letrec*
		    (stage6 ebindings body)))))))

   (define (stage4 ebindings body)
      ;; collect all first variables that do not use any of the following
      ;; variables

      (define (split ebindings)
	 (with-trace 'letrec* "letrec*/stage4/split"
	    (let loop ((ebindings ebindings)
		       (let*-bindings '()))
	       (cond
		  ((null? ebindings)
		   (values (reverse! let*-bindings) '()))
		  ((any (lambda (eb)
			   (memq (ebinding-var eb)
			      (ebinding-frees (car ebindings))))
		      (cdr ebindings))
		   ;; one of the variables introduced in the next bindings
		   ;; appears free in the value of the current binding
		   (trace-item "abort=" (ebinding-var (car ebindings)))
		   (values (reverse! let*-bindings) ebindings))
		  (else
		   (loop (cdr ebindings) (cons (car ebindings) let*-bindings)))))))
      (with-trace 'letrec* "letrec*/stage4"
	 (split-head-let* ebindings body split stage5)))

   (define (stage3 ebindings body)
      ;; collect all first immutable functions and move ahead those
      ;; that do not refer to any locally introduced variables
      
      (define (split ebindings)
	 (with-trace 'letrec* "letrec*/stage3/split"
	    (let loop ((ebindings ebindings)
		       (fun-bindings '())
		       (rec*-bindings '()))
	       (cond
		  ((null? ebindings)
		   (values (reverse! fun-bindings) (reverse! rec*-bindings)))
		  ((and (function? (ebinding-value (car ebindings)))
			(not (find (lambda (eb)
				      (memq (ebinding-var (car ebindings))
					 (ebinding-setvs eb)))
				ebindings)))
		   ;; an immutable function
		   (if (null? (ebinding-frees (car ebindings)))
		       (loop (cdr ebindings)
			  (cons (car ebindings) fun-bindings)
			  rec*-bindings)
		       (loop (cdr ebindings)
			  fun-bindings
			  (cons (car ebindings) rec*-bindings))))
		  (else
		   (trace-item "abort=" (ebinding-var (car ebindings)))
		   (values (reverse! fun-bindings)
		      (append (reverse! rec*-bindings) ebindings)))))))
      
      (with-trace 'letrec* "letrec*/stage3"
	 (split-head-letrec ebindings body split stage4)))

   (define (stage2 ebindings body)
      ;; collect all first bound immutable values to move them up front
      
      (define (split ebindings)
	 (with-trace 'letrec* "letrec*/stage2/split"
	    (let loop ((ebindings ebindings)
		       (val-bindings '())
		       (rec*-bindings '()))
	       (cond
		  ((null? ebindings)
		   (values (reverse! val-bindings) (reverse! rec*-bindings)))
		  ((and (function? (ebinding-value (car ebindings)))
			(immutable-in? (car ebindings) (cdr ebindings)))
		   (loop (cdr ebindings)
		      val-bindings
		      (cons (car ebindings) rec*-bindings)))
		  ((and (not (function? (ebinding-value (car ebindings))))
			(not (find (lambda (var)
				      (not (find (lambda (eb)
						    (eq? (ebinding-var eb) var))
					      (cdr ebindings))))
				(ebinding-frees (car ebindings))))
			(immutable-in? (car ebindings) rec*-bindings))
		   ;; this is variable that do not use any of the following
		   ;; bindings, move it up front
		   (loop (cdr ebindings)
		      (cons (car ebindings) val-bindings)
		      rec*-bindings))
		  (else
		   (trace-item "abort=" (ebinding-var (car ebindings)))
		   (values (reverse! val-bindings)
		      (append (reverse! rec*-bindings) ebindings)))))))
      
      (with-trace 'letrec* "letrec*/stage2"
	 (split-head-letrec ebindings body split stage3)))

   (define (stage1 ebindings body)
      ;; collect all the last independant values and move them downward
      ;; into a new let block

      (define (split ebindings)
	 (with-trace 'letrec* "letrec*/stage1/split"
	    (let loop ((ebindings (reverse ebindings))
		       (let*-bindings '()))
	       (cond
		  ((null? ebindings)
		   (values '() let*-bindings))
		  ((and (every (lambda (var)
				  (find (lambda (eb)
					   (eq? var (ebinding-var eb)))
				     (cdr ebindings)))
			   (ebinding-frees (car ebindings)))
			;; all the ebinding free variables are defined before
			;; and the binding is never used in a previous binding
			(not (used-in? (car ebindings) (cdr ebindings))))
		   (loop (cdr ebindings) (cons (car ebindings) let*-bindings)))
		  (else
		   ;; one of the variables introduced in the next bindings
		   ;; appears free in the value of the current binding
		   (trace-item "abort=" (ebinding-var (car ebindings)))
		   (values (reverse ebindings) let*-bindings))))))
      
      (with-trace 'letrec* "letrec*/stage1"
	 (split-tail-let* ebindings body split stage2)))

   (define (stage0 ebindings body)
      ;; collect all immutable variables bound to literals
      
      (define (split ebindings)
	 (let loop ((ebindings ebindings)
		    (rec-bindings '())
		    (rec*-bindings '()))
	    (cond
	       ((null? ebindings)
		(values (reverse! rec-bindings) (reverse! rec*-bindings)))
	       ((and (not (side-effect? (ebinding-value (car ebindings))))
		     (immutable-in? (car ebindings) rec*-bindings))
		;; this is literal is bound to an immutable variable
		(loop (cdr ebindings)
		   (cons (car ebindings) rec-bindings)
		   rec*-bindings))
	       (else
		(loop (cdr ebindings)
		   rec-bindings
		   (cons (car ebindings) rec*-bindings))))))
      
      (with-trace 'letrec* "letrec*/stage0"
	 (split-head-letrec ebindings body split stage1)))

   (define (decompose-letrec* bindings body)
      ;; for each binding, extract the variable name and the set
      ;; of scoped free variables used in the expression
      (let* ((vars (map (lambda (b) (fast-id-of-id (car b) loc)) bindings))
	     (ebindings (map (lambda (b v)
				(binding->ebinding b v vars))
			   bindings vars)))
	 (trace-item "bindings="
	    (map (lambda (b) (shape (ebinding-var b)))
	       ebindings))
	 (for-each (lambda (b)
		      (trace-item "  " (shape (ebinding-var b))
			 " free-vars=" (ebinding-frees b)
			 " set-vars=" (ebinding-setvs b)))
	    ebindings)
	 (stage0 ebindings (epairify-propagate-loc `(begin ,@body) loc))))
   
   (define (letrec*->letrec sexp stack loc site)
      (set-car! sexp 'letrec)
      (sexp->node sexp stack loc site))
   
   (define (lambda? exp)
      (match-case exp
	 ((?- (lambda ?- . ?-))
	  #t)
	 ((?- (labels ((?sym . ?-)) (and ?var (? symbol?))))
	  (eq? (fast-id-of-id sym #f) var))
	 ((?- ((and ?sym (? symbol?)) ?- . ?-))
	  (eq? (fast-id-of-id sym #f) 'lambda))
	 (else
	  #f)))

   (with-trace 'letrec* "letrec*"
      (match-case sexp
	 ((letrec* () . ?body)
	  ;; not a real letrec*
	  (sexp->node
	     (epairify-propagate-loc `(begin ,@body) loc)
	     stack loc site))
	 ((letrec* (and ?bindings (?binding)) . ?body)
	  (sexp->node
	     (epairify-propagate-loc `(letrec ,bindings ,@body) loc)
	     stack loc site))
	 ((letrec* (and (? list?) ?bindings) . ?body)
	  (if (every lambda? bindings)
	      ;; a plain letrec
	      (letrec*->letrec sexp stack loc site)
	      ;; a true letrec*
	      (decompose-letrec* bindings body)))
	 (else
	  (error-sexp->node (string-append "Illegal 'letrec*' form")
	     exp (find-location/loc exp loc))))))

;*---------------------------------------------------------------------*/
;*    function? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Does exp contain a lambda definition (possibly nested)           */
;*---------------------------------------------------------------------*/
(define (function? exp #!optional directp)

   (define (any* pred lst)
      (when (pair? lst)
	 (or (pred (car lst)) (any* pred (cdr lst)))))

   (match-case exp
      ((quote . ?-)
       #f)
      ((labels ((?tid . ?-)) ?id)
       (eq? (fast-id-of-id tid #f) id))
      ((?var . (and ?args ??-))
       (or (eq? var 'lambda)
	   (and (symbol? var) (eq? (fast-id-of-id var #f) 'lambda))
	   (function? var)
	   (and (not directp) (any* function? args))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    constant? ...                                                    */
;*---------------------------------------------------------------------*/
(define (constant? val)
   (or (atom? val) (kwote? val) (null? val) (number? val)
       (cnst? val) (string? val) (char? val) (boolean? val)
       (number? val)))

;*---------------------------------------------------------------------*/
;*    side-effect? ...                                                 */
;*---------------------------------------------------------------------*/
(define (side-effect? val)
   
   (define (op? val)
      (memq val '(+ - *
		  +fx -fx *fx
		  +elong -elong *elong
		  +llong -llong *llong
		  +s8 -s8 *s8
		  +u8 -u8 *u8
		  +s16 -s16 *s16
		  +u16 -u16 *u16
		  +s32 -s32 *s32
		  +u32 -u32 *u32
		  +s64 -s64 *s64
		  +u64 -u64 *u64
		  +f32 -f32 *f32
		  +f64 -f64 *f64
		  
		  > >= < <= =
		  >fx >=fx <fx <=fx =fx
		  >elong >=elong <elong <=elong =elong
		  >llong >=llong <llong <=llong =llong
		  >s8 >=s8 <s8 <=s8 =s8
		  >u8 >=u8 <u8 <=u8 =u8
		  >s16 >=s16 <s16 <=s16 =s16
		  >u16 >=u16 <u16 <=u16 =u16
		  >s32 >=s32 <s32 <=s32 =s32
		  >u32 >=u32 <u32 <=u32 =u32
		  >s64 >=s64 <s64 <=s64 =s64
		  >u64 >=u64 <u64 <=u64 =u64
		  >f32 >=f32 <f32 <=f32 =f32
		  >f64 >=f64 <f64 <=f64 =f64
		  
		  eq? equal?
		  
		  bit-lsh bit-rsh bit-ursh bit-not bit-xor
		  bit-lshelong bit-rshelong bit-urshelong bit-notelong bit-xorelong
		  bit-lshs8 bit-rshs8 bit-urshs8 bit-nots8 bit-xors8
		  bit-lshu8 bit-rshu8 bit-urshu8 bit-notu8 bit-xoru8
		  bit-lshs16 bit-rshs16 bit-urshs16 bit-nots16 bit-xors16
		  bit-lshu16 bit-rshu16 bit-urshu16 bit-notu16 bit-xoru16
		  bit-lshs32 bit-rshs32 bit-urshs32 bit-nots32 bit-xors32
		  bit-lshu32 bit-rshu32 bit-urshu32 bit-notu32 bit-xoru32
		  bit-lshs64 bit-rshs64 bit-urshs64 bit-nots64 bit-xors64
		  bit-lshu64 bit-rshu64 bit-urshu64 bit-notu64 bit-xoru64)))
   
   (let loop ((val val))
      (match-case val
	 ((? constant?) #f)
	 (((kwote quote) . ?-) #f)
	 (((? op?) ?e1 ?e2) (or (loop e1) (loop e2)))
	 (((kwote not) ?e) (loop e))
	 ((if ?test ?then ?else) (or (loop test) (loop then) (loop else)))
	 (else #t))))
