;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Inline/recursion.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun 19 13:40:47 1996                          */
;*    Last change :  Tue Feb 26 10:59:18 2019 (serrano)                */
;*    Copyright   :  1996-2019 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inlining of recursive functions.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module inline_recursion
   (include "Tools/trace.sch"
	    "Tools/verbose.sch")
   (import  engine_param
	    type_type
	    ast_var
	    ast_node
	    ast_local
	    ast_alphatize
	    ast_sexp
	    module_module
	    tools_speek
	    tools_shape
	    tools_error
	    inline_inline
	    inline_simple
	    inline_variant
	    inline_loop
	    inline_app
	    reduce_cse)
   (export  (is-recursive?::bool ::variable)
	    (inline-app-recursive::node ::node ::long ::obj)))
 
;*---------------------------------------------------------------------*/
;*    inline-app-recursive ...                                         */
;*---------------------------------------------------------------------*/
(define (inline-app-recursive node kfactor stack)
   (let* ((fun (app-fun node))
	  (callee (var-variable fun)))
      (cond
	 ((not (tailrec? callee))
	  node)
	 ((memq callee stack)
	  ;; when we are in the definition of the function, we don't
	  ;; do anything otherwise, we could possibly not be able
	  ;; to inline call to this function (because its body should
	  ;; have growth...)
	  (inline-app-simple node kfactor stack "simple"))
	 (else
	  ;; ok, we do a smart inlining.
	  (trace (inline inline+ 2)
	     "inline-app-recursive: " (shape node) #\Newline)
	  (inline-app-labels node kfactor stack)))))

;*---------------------------------------------------------------------*/
;*    tailrec? ...                                                     */
;*---------------------------------------------------------------------*/
(define (tailrec? var::variable)
   (when (isfun? (variable-value var))
      (with-access::isfun (variable-value var) (tailrec)
	 tailrec)))

;*---------------------------------------------------------------------*/
;*    inline-app-labels ...                                            */
;*    -------------------------------------------------------------    */
;*    We create a local function to inline this call.                  */
;*---------------------------------------------------------------------*/
(define (inline-app-labels node kfactor stack)
   (let* ((variable (var-variable (app-fun node)))
	  (local (make-local-sfun (variable-id variable)
		    (variable-type variable)
		    (variable-value variable)))
	  (old-sfun (variable-value variable))
	  (old-body (if (isfun? old-sfun)
			(isfun-original-body old-sfun)
			(sfun-body old-sfun)))
	  (rec-calls (find-recursive-calls old-body variable #t (make-cell #t)))
	  (old-args (sfun-args old-sfun))
	  (inv-args (invariant-args node variable rec-calls))
	  (var-args (variant-args variable))
	  (new-args (map (lambda (l)
			    (clone-local l (duplicate::svar (local-value l))))
		       var-args))
	  (substitute (substitutions variable (app-args node) new-args))
	  (svg-calls-args (map (lambda (app) (app-args app)) rec-calls))
	  (remove! (for-each remove-invariant-args! rec-calls))
	  (iloc (and (global? variable)
		     (not (eq? (global-module variable) *module*))
		     (node-loc node)))
	  (new-body (if (null? inv-args)
			;; full substitution of the procedure and its arguments
			(alphatize (cons variable old-args)
			   (cons local substitute)
			   iloc
			   old-body)
			;; The arity of the inlined function differs from
			;; the one of the initial function so we cannot replace
			;; the old closure with the new one.
			(alphatize-sans-closure (cons variable old-args)
			   (cons local substitute)
			   iloc
			   old-body
			   variable)))
	  (restore! (for-each (lambda (app args)
				 (app-args-set! app args))
		       rec-calls
		       svg-calls-args))
	  (new-sfun (duplicate::sfun old-sfun
		       (args new-args)
		       (class 'sfun)
		       (body new-body)))
	  (new-kfactor (*inlining-reduce-kfactor* kfactor)))
      ;; we mark the formal parameter as compiler parameters
      (for-each (lambda (new old)
		   (local-user?-set! new (local-user? old)))
		new-args
		var-args)
      ;; we set the new sfun
      (local-user?-set! local (cond
				 ((global? variable)
				  (global-user? variable))
				 ((local? variable)
				  (local-user? variable))
				 (else
				  #f)))
      (local-value-set! local new-sfun)
      ;; some traces
      (trace (inline inline+ 3)
	     "   new-fun  : " (shape local) #\Newline
	     "   user?    : " (local-user? local) #\Newline
	     "   rec-calls: " (shape rec-calls) #\Newline
	     "   inv-args : " (shape inv-args) #\Newline
	     "   var-args : " (shape var-args) #\Newline)
      ;; some small verbing ...
      (if (not (memq (sfun-class old-sfun) '(sifun sgfun)))
	  (verbose 3 "         "
		   (shape variable) " --> " (current-function)
		   " (recursive)"
		   #\Newline))
      ;; now (and only now), we can inline the new local body
      (trace (inline inline+ 3)
	     "Je reinline: " (shape local) " "
	     (shape (sfun-body (local-value local)))
	     #\Newline)
      (inline-sfun! local new-kfactor stack)
      (let ((new-call (remove-invariant-args! node))
	    (call-size (+fx 1 (length (app-args node)))))
	 (if (and *optim-unroll-loop?*
		  (is-loop? variable)
		  ;; we disable (because we don't know how to do it
		  ;; easily) loop unrolling of functions nesting inner loops.
		  (not (inner-loop? variable))
		  (inline-app? local new-kfactor call-size stack))
	     (unroll-call variable node local new-kfactor new-call stack)
	     (plain-call variable node local new-call stack)))))

;*---------------------------------------------------------------------*/
;*    plain-call ...                                                   */
;*---------------------------------------------------------------------*/
(define (plain-call variable node local new-call stack)
   (trace (inline 3)
      "    plain: " (shape node) #\Newline
      " new-call: " (shape new-call) #\Newline
      "    local: " (shape local) #\Newline
      " variable: " (shape variable) #\Newline)
   ;; we shrink the formals because we won't 
   ;; need anymore variant/invariant property.
   (shrink-args! variable)
   ;; the new node ...
   (let ((iloc (and (global? variable)
		    (not (eq? (global-module variable) *module*))
		    (node-loc node))))
      (instantiate::let-fun
	 (loc (node-loc node))
	 (type (node-type node))
	 (locals (list local))
	 (body (alphatize (list variable) (list local) iloc new-call)))))


;*---------------------------------------------------------------------*/
;*    unroll-call ...                                                  */
;*---------------------------------------------------------------------*/
(define (unroll-call variable node local kfactor call stack)
   (trace (inline 3)
	  "unrolling: " (shape node) #\Newline
	  "     call: " (shape call) #\Newline
	  "    local: " (shape local) #\Newline
	  " variable: " (shape variable) #\Newline)
   (let* ((iloc        (and (global? variable)
			    (not (eq? (global-module variable) *module*))
			    (node-loc call)))
	  (new-call    (alphatize (list variable) (list local) iloc call))
	  (old-body    (sfun-body (variable-value variable)))
	  (new-body    (inline-app-simple new-call
					  kfactor
					  (cons local stack)
					  "unrolling")))
      (trace (inline 4)
	     "  old-body: " (shape old-body) #\Newline
	     "  new-body: " (shape new-body) #\Newline)
      ;; we shrink the formals because we won't 
      ;; need anymore variant/invariant property.
      (shrink-args! variable)
      ;; and we nest the local function
      (multiple-value-bind (_ node)
	 (node-cse! (nest-loop! new-body
				local
				(lambda (node)
				   (instantiate::let-fun
				      (loc (node-loc node))
				      (type (node-type node))
				      (body node)
				      (locals (list local)))))
		    '())
	 node)))

;*---------------------------------------------------------------------*/
;*    is-recursive? ...                                                */
;*---------------------------------------------------------------------*/
(define (is-recursive?::bool var::variable)
   (trace (inline 2) "is-recursive?: " (shape var) "..." #\Newline)
   (let ((sfun (variable-value var)))
      (if (not (isfun? sfun))
	  (let* ((trec (make-cell #t))
		 (calls (find-recursive-calls (sfun-body sfun) var #t trec)))
	     (widen!::isfun sfun
		(tailrec (cell-ref trec))
		(original-body (sfun-body sfun))
		(recursive-calls calls))
	     (pair? calls))
	  (cond
	     ((pair? (isfun-recursive-calls sfun))
	      #t)
	     ((null? (isfun-recursive-calls sfun))
	      #f)
	     (else
	      (let* ((trec (make-cell #t))
		     (calls (find-recursive-calls
			       (isfun-original-body sfun) var #t trec)))
		 (isfun-tailrec-set! sfun (cell-ref trec))
		 (isfun-recursive-calls-set! sfun calls)
		 (pair? calls)))))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (find-recursive-calls node::node var::variable tail tres)
   '())

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::sequence ...                              */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::sequence var tail tres)
   (find-recursive-calls* (sequence-nodes node) var tail tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::sync ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::sync var tail tres)
   (append (find-recursive-calls (sync-mutex node) var #f tres)
      (find-recursive-calls (sync-prelock node) var #f tres)
      (find-recursive-calls (sync-body node) var #f tres)))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::app ...                                   */
;*    -------------------------------------------------------------    */
;*    There is no need to inlining the arguments because all the       */
;*    arguments are variables.                                         */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::app var tail tres)
   (with-access::app node (fun args)
      (let ((args-calls (find-recursive-calls* args var tail tres)))
	 (if (and (var? fun) (eq? (var-variable fun) var))
	     (begin
		(unless tail (cell-set! tres #f))
		(cons node args-calls))
	     args-calls))))
 
;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::app-ly ...                                */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::app-ly var tail tres)
   (with-access::app-ly node (fun arg)
      (append (find-recursive-calls fun var #f tres)
	 (find-recursive-calls arg var #f tres))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::funcall ...                               */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::funcall var tail tres)
   (with-access::funcall node (fun args)
      (append (find-recursive-calls fun var #f tres)
	      (find-recursive-calls* args var #f tres))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::extern ...                                */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::extern var tail tres)
   (find-recursive-calls* (extern-expr* node) var #f tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::cast ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::cast var tail tres)
   (find-recursive-calls (cast-arg node) var #f tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::setq ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::setq var tail tres)
   (find-recursive-calls (setq-value node) var #f tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::conditional ...                           */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::conditional var tail tres)
   (with-access::conditional node (test true false)
       (append (find-recursive-calls test var #f tres)
	       (find-recursive-calls true var tail tres)
	       (find-recursive-calls false var tail tres))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::fail ...                                  */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::fail var tail tres)
   (with-access::fail node (proc msg obj)
      (append (find-recursive-calls proc var #f tres)
	      (find-recursive-calls msg var #f tres)
	      (find-recursive-calls obj var #f tres))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::switch ...                                */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::switch var tail tres)
   (let loop ((clauses (switch-clauses node))
	      (calls (find-recursive-calls (switch-test node) var #f tres)))
      (if (null? clauses)
	  calls
	  (loop (cdr clauses)
	     (append (find-recursive-calls (cdr (car clauses)) var tail tres)
		calls)))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::let-fun ...                               */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::let-fun var tail tres)
   (let loop ((locals (let-fun-locals node))
	      (calls (find-recursive-calls (let-fun-body node) var tail tres)))
      (if (null? locals)
	  calls
	  (loop (cdr locals)
	     (append calls
		(find-recursive-calls
		   (sfun-body (local-value (car locals)))
		   var #t tres))))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::let-var ...                               */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::let-var var tail tres)
   (let loop ((bindings (let-var-bindings node))
	      (calls (find-recursive-calls (let-var-body node) var tail tres)))
      (if (null? bindings)
	  calls
	  (loop (cdr bindings)
	     (append calls
		(find-recursive-calls (cdr (car bindings)) var #f tres))))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::set-ex-it ...                             */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::set-ex-it var tail tres)
   (find-recursive-calls (set-ex-it-body node) var #f tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::jump-ex-it ...                            */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::jump-ex-it v tail tres)
   (with-access::jump-ex-it node (exit value)
      (append (find-recursive-calls exit v #f tres)
	 (find-recursive-calls value v #f tres))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::make-box ...                              */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::make-box var tail tres)
   (find-recursive-calls (make-box-value node) var #f tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::box-ref ...                               */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::box-ref var tail tres)
   (find-recursive-calls (box-ref-var node) var #f tres))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls ::box-set! ...                              */
;*---------------------------------------------------------------------*/
(define-method (find-recursive-calls node::box-set! v tail tres)
   (with-access::box-set! node (var value)
      (append (find-recursive-calls var v #f tres)
	 (find-recursive-calls value v #f tres))))

;*---------------------------------------------------------------------*/
;*    find-recursive-calls* ...                                        */
;*---------------------------------------------------------------------*/
(define (find-recursive-calls* node* var tail tres)
   (let loop ((node* node*)
	      (calls '()))
      (cond
	 ((null? node*)
	  calls)
	 ((null? (cdr node*))
	  (append (find-recursive-calls (car node*) var tail tres) calls))
	 (else
	  (loop (cdr node*)
	     (append (find-recursive-calls (car node*) var #f tres) calls))))))
   

