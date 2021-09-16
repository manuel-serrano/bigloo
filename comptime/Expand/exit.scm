;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Expand/exit.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Apr 21 15:03:35 1995                          */
;*    Last change :  Thu Sep 16 14:03:04 2021 (serrano)                */
;*    Copyright   :  1995-2021 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The macro expansion of the `exit' machinery.                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_exit
   (include "Expand/expander.sch"
	    "Tools/trace.sch"
	    "Tools/location.sch")
   (import  tools_args
	    tools_speek
	    tools_misc
	    expand_expander
	    expand_eps
	    expand_lambda
	    engine_param
	    type_type
	    ast_ident
	    backend_backend
	    tools_location)
   (export  (expand-jump-exit      ::obj ::procedure)
	    (expand-set-exit       ::obj ::procedure)
	    (expand-bind-exit      ::obj ::procedure)
	    (expand-unwind-protect ::obj ::procedure)
	    (expand-with-handler   ::obj ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-jump-exit ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-jump-exit x e)
   (match-case x
      ((?- ?exit . ?value)
       (let ((new `(jump-exit ,(e exit e) ,(e (expand-progn value) e))))
	  (replace! x new)))
      (else
       (error #f "Illegal 'jump-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-set-exit ...                                              */
;*---------------------------------------------------------------------*/
(define (expand-set-exit x e)
   (match-case x
      ((?- (?exit) ?body :onexit ?onexit)
       (let ((new `(set-exit (,exit) ,(e body e) :onexit ,(e onexit e))))
	  (replace! x new)))
      ((?- (?exit) ?body)
       (let ((new `(set-exit (,exit) ,(e body e))))
	  (replace! x new)))
      (else
       (error #f "Illegal `set-exit' form" x))))
	  
;*---------------------------------------------------------------------*/
;*    expand-bind-exit ...                                             */
;*---------------------------------------------------------------------*/
(define (expand-bind-exit x e)

   (define (find-in-body k body)
      (cond
	 ((eq? body k)
	  #t)
	 ((pair? body)
	  (unless (eq? (car body) 'quote)
	     (or (find-in-body k (car body)) (find-in-body k (cdr body)))))
	 (else #f)))

   (define (tailrec*? exit body)
      (let ((ydob (reverse body)))
	 (unless (any (lambda (b) (find-in-body exit b)) (cdr ydob))
	    (tailrec? exit (car ydob)))))
   
   (define (tailrec? exit body)
      (match-case body
	 (((? (lambda (f) (eq? f exit))) ?arg)
	  (not (find-in-body exit arg)))
	 ((begin . (and (? list?) ?body))
	  (tailrec*? exit body))
	 ((if ?test ?then ?otherwise)
	  (and (not (find-in-body exit test))
	       (tailrec? exit then)
	       (tailrec? exit otherwise)))
	 (((or let let* letrec letrec*) (and (? list?) ?bindings) . ?body)
	  (unless (any (lambda (b) (find-in-body exit b)) bindings)
	     (tailrec*? exit body)))
	 ((bind-exit (?xit) . ?body)
	  (unless (eq? xit exit)
	     (tailrec*? exit body)))
	 (((kwote quote) . ?-)
	  #t)
	 ((? (lambda (id) (eq? id exit)))
	  #f)
	 ((? (lambda (v) (not (pair? v))))
	  #t)
	 (else
	  #f)))

   (define (return! exit body)
      (match-case body
	 (((? (lambda (f) (eq? f exit))) ?arg)
	  (set-car! body 'begin)
	  body)
	 ((begin . ?exprs)
	  (return! exit (car (last-pair exprs)))
	  body)
	 ((if ?test ?then ?otherwise)
	  (return! exit then)
	  (return! exit otherwise)
	  body)
	 (((or let let* letrec letrec*) ?- . ?exprs)
	  (return! exit (car (last-pair exprs)))
	  body)
	 ((bind-exit ?- . ?exprs)
	  (return! exit (car (last-pair exprs)))
	  body)
	 (else
	  body)))

   (define (trace?)
      (and (>fx (bigloo-compiler-debug) 0)
	   (backend-trace-support (the-backend))))
   
   (define (add-trace tracesp body)
      (if (trace?)
	  (let ((tmp (mark-symbol-non-user! (gensym 'tmp))))
	     `(let ((,tracesp ($get-trace-stacksp)))
		 (let ((,tmp ,body))
		    ($set-trace-stacksp ,tracesp)
		    ,tmp)))
	  body))

   (define (default-expansion exit body)
      (let ((an-exit (mark-symbol-non-user! (gensym 'an_exit)))
	    (an-exitd (mark-symbol-non-user! (gensym 'an_exitd)))
	    (val (mark-symbol-non-user! (gensym 'val)))
	    (res (mark-symbol-non-user! (gensym 'res)))
	    (env (mark-symbol-non-user! (gensym 'env)))
	    (tracesp (mark-symbol-non-user! (gensym 'tracesp))))
	 (let ((new `(set-exit (,an-exit)
			,(add-trace tracesp
			    `(let ((,env ($current-dynamic-env)))
				($env-push-exit! ,env ,an-exit 1)
				(let ((,an-exitd ($env-get-exitd-top ,env)))
				   (labels ((,exit (,val)
					       ((@ unwind-stack-until! __bexit)
						,an-exitd
						#f
						,val
						#f
						,(when (trace?) tracesp))))
				      (let ((,res (begin ,@body)))
					 ($env-pop-exit! ,env)
					 ,res))))))))
	    (replace! x new))))

   (define (env-expansion env exit body)
      (let ((an-exit (mark-symbol-non-user! (gensym 'an_exit)))
	    (an-exitd (mark-symbol-non-user! (gensym 'an_exitd)))
	    (val (mark-symbol-non-user! (gensym 'val)))
	    (res (mark-symbol-non-user! (gensym 'res)))
	    (tracesp (mark-symbol-non-user! (gensym 'tracesp))))
	 (let ((new (add-trace tracesp
		       `(set-exit (,an-exit)
			   (begin
			      ($env-push-exit! ,env ,an-exit 1)
			      (let ((,exit ($env-get-exitd-top ,env)))
				 (let ((,res ,body))
				    ($env-pop-exit! ,env)
				    ,res)))))))
	    (replace! x new))))

   (define (env-exit-expansion env exit body onexit)
      (let ((an-exit (mark-symbol-non-user! (gensym 'an_exit)))
	    (an-exitd (mark-symbol-non-user! (gensym 'an_exitd)))
	    (val (mark-symbol-non-user! (gensym 'val)))
	    (res (mark-symbol-non-user! (gensym 'res)))
	    (tracesp (mark-symbol-non-user! (gensym 'tracesp))))
	 (let ((new (add-trace tracesp
		       `(set-exit (,an-exit)
			   (begin
			      ($env-push-exit! ,env ,an-exit 1)
			      (let ((,exit ($env-get-exitd-top ,env)))
				 (let ((,res ,body))
				    ($env-pop-exit! ,env)
				    ,res)))
			   :onexit ,onexit))))
	    (replace! x new)
	    x)))
   
   (match-case x
      ((?- (?exit) (?exit ?expr))
       (e expr e))
      ((?- (?exit) . ?exprs)
       ;; force the macro expansion before optimizing bind-exit
       (let ((old-internal internal-definition?))
	  (set! internal-definition? #t)
	  (let* ((e (internal-begin-expander e))
		 (nexprs (e `(begin ,@exprs) e)))
	     (set-car! (cddr x) nexprs)
	     (set-cdr! (cddr x) '()))
	  (set! internal-definition? old-internal))
       (match-case x
	  ((?- (?exit)
	      ((and (or let let* letrec letrec*) ?letk)
	       (and (? list?) ?bindings)
	       (and ?body (?exit ?expr))))
	   (if (every (lambda (b)
			 (or (symbol? b)
			     (and (list? b)
				  (not (find-in-body exit (car b))))))
		  bindings)
	       (replace! x `(,letk ,bindings ,expr))
	       (default-expansion exit body)))
	  ((?- (?exit) . ?body)
	   (if (not (find-in-body exit body))
	       (replace! x (e `(begin ,@body) e))
	       (let ((head (take body (- (length body) 1)))
		     (tail (car (last-pair body))))
		  (if (and (not (find-in-body exit head))
			   (tailrec? exit tail))
		      (replace! x `(begin ,@head ,(return! exit tail)))
		      (default-expansion exit body)))))
	  (else
	   (error #f "Illegal `bind-exit' form" x))))
      ((?- :env ?env (?exit) ?body)
       ;; only used by with-handler
       (let ((old-internal internal-definition?))
	  (set! internal-definition? #t)
	  (let* ((e (internal-begin-expander e))
		 (ebody (e body e)))
	     (set! internal-definition? old-internal)
	     (env-expansion env exit ebody))))
      ((?- :env ?env (?exit) ?body ?onexit)
       ;; only used by with-handler
       (let ((old-internal internal-definition?))
	  (set! internal-definition? #t)
	  (let* ((e (internal-begin-expander e))
		 (ebody (e body e))
		 (eonexit (e onexit e)))
	     (set! internal-definition? old-internal)
	     (env-exit-expansion env exit ebody eonexit))))
      (else
       (error #f "Illegal `bind-exit' form" x))))

;*---------------------------------------------------------------------*/
;*    expand-unwind-protect ...                                        */
;*---------------------------------------------------------------------*/
(define (expand-unwind-protect x e)

   (define (trace?)
      (and (>fx (bigloo-compiler-debug) 0)
	   (backend-trace-support (the-backend))))
   
   (define (add-trace tracesp body)
      (if (trace?)
	  (let ((tmp (mark-symbol-non-user! (gensym 'tmp))))
	     `(let ((,tracesp ($get-trace-stacksp)))
		 (let ((,tmp ,body))
		    ($set-trace-stacksp ,tracesp)
		    ,tmp)))
	  body))

   (define (new-expander exp cleanup)
      (let ((exitd (mark-symbol-non-user! (gensym 'exitd)))
	    (protect (mark-symbol-non-user! (gensym 'protect)))
	    (tmp (mark-symbol-non-user! (gensym 'tmp)))
	    (tracesp (mark-symbol-non-user! (gensym 'tracesp))))
	 (let ((new (add-trace tracesp
		       `(let ((,exitd ($get-exitd-top))
			      (,protect (lambda () ,@cleanup)))
			   ((@ exitd-push-protect! __bexit) ,exitd ,protect)
			   (let ((,tmp ,exp))
			      ((@ exitd-pop-protect! __bexit) ,exitd)
			      ,@(if (trace?)
				    `(($set-trace-stacksp ,tracesp))
				    '())
			      (,protect)
			      ,tmp)))))
	    (replace! x (e new e)))))

   (define (error-handler-expander exp ohs)
      (let ((exitd (mark-symbol-non-user! (gensym 'exitd)))
	    (tmp (mark-symbol-non-user! (gensym 'tmp)))
	    (protect (mark-symbol-non-user! (gensym 'tmp))))
	 (let ((new `(let* ((,exitd ($get-exitd-top))
			    (,protect (cons ,ohs ($exitd-protect ,exitd))))
			((@ exitd-protect-set! __bexit) ,exitd ,protect)
			(let ((,tmp ,exp))
			   ((@ exitd-pop-protect! __bexit) ,exitd)
			   ($set-error-handler! ,ohs)
			   ,tmp))))
	    (replace! x (e new e)))))

   (define (env-error-handler-expander exp env ohs)
      (let ((exitd (mark-symbol-non-user! (gensym 'exitd)))
	    (tmp (mark-symbol-non-user! (gensym 'tmp)))
	    (protect (mark-symbol-non-user! (gensym 'tmp))))
	 (let ((new `(let* ((,exitd ($env-get-exitd-top ,env))
			    (,protect (cons ,ohs ($exitd-protect ,exitd))))
			((@ exitd-protect-set! __bexit) ,exitd ,protect)
			(let ((,tmp ,exp))
			   ((@ exitd-pop-protect! __bexit) ,exitd)
			   ($env-set-error-handler! ,env ,ohs)
			   ,tmp))))
	    (replace! x (e new e)))))

   (match-case x
      ((?- ?exp ($set-error-handler! (and (? symbol?) ?ohs)))
       (error-handler-expander exp ohs))
      ((?- ?exp ($env-set-error-handler! (and (? symbol?) ?env) (and (? symbol?) ?ohs)))
       (env-error-handler-expander exp env ohs))
      ((?- ?exp . (and (? pair?) ?cleanup))
       (new-expander exp cleanup))
      (else
       (error #f "Illegal `unwind-protect' form" x))))
			  
;*---------------------------------------------------------------------*/
;*    expand-with-handler ...                                          */
;*---------------------------------------------------------------------*/
(define (expand-with-handler x e)
   
  (define (expand handler body)
     (let ((ohs (gensym 'ohs))
	   (hds (gensym 'hds))
	   (err (gensym 'err))
	   (cell (gensym 'cell))
	   (escape (gensym 'escape))
	   (hdl (gensym 'handler))
	   (ehdl (gensym 'errhandler))
	   (val (gensym 'val))
	   (env (gensym 'env)))
	(e `(let ((,hdl ,handler)
		  (,env ($current-dynamic-env)))
	       (let ((,cell ($make-stack-cell #unspecified)))
		  (let ((,val (bind-exit :env ,env (,escape)
				 (let ((,ohs ($env-get-error-handler ,env)))
				    (let ((,hds ($acons ,escape ,cell)))
				       ($env-set-error-handler! ,env ,hds)
				       (unwind-protect
					  (begin ,@body)
					  ($env-set-error-handler! ,env ,ohs)))))))
		     (if (eq? ,val ,cell)
			 (begin
			    (sigsetmask 0)
			    (,hdl (cell-ref ,val)))
			 ,val))))
	   e)))

   (match-case x
      ((?- ?handler . ?body)
       (replace! x (expand handler body)))
      (else
       (error #f "Illegal `with-handler' form" x))))

   
			  
       
