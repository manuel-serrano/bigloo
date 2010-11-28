;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/closure.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 27 11:35:13 1996                          */
;*    Last change :  Sun Nov 28 08:45:41 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The closure optimization described in:                           */
;*                                                                     */
;*    @InProceedings{ serrano:sac95,                                   */
;*      author        = {Serrano, M.},                                 */
;*      title         = {{C}ontrol {F}low {A}nalysis: a {F}unctional   */
;*                       {L}anguages {C}ompilation {P}aradigm},        */
;*      year          = 1995,                                          */
;*      month         = feb,                                           */
;*      booktitle     = {10th {S}ymposium on {A}pplied {C}omputing},   */
;*      address       = {Nashville, Tennessee, USA},                   */
;*    }                                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_closure
   (include "Tools/trace.sch")
   (import  engine_param
	    type_type
	    type_cache
	    tools_shape
	    tools_speek
	    ast_var
	    ast_node
	    ast_env
	    cfa_info
	    cfa_info2
	    cfa_cfa
	    cfa_approx
	    cfa_set
	    cfa_ltype)
   (export  (closure-optimization! ::pair-nil)
	    (closure-optimization?)
	    (add-procedure-ref!  ::node)
	    (add-make-procedure! ::node)
	    (add-funcall! ::node)
	    (approx-procedure-el?::bool ::approx)
	    (approx-procedure-el1?::bool ::approx)))

;*---------------------------------------------------------------------*/
;*    closure-optimization? ...                                        */
;*---------------------------------------------------------------------*/
(define (closure-optimization?)
   (>=fx *optim* 2))

;*---------------------------------------------------------------------*/
;*    closure-optimization! ...                                        */
;*---------------------------------------------------------------------*/
(define (closure-optimization! globals)
   (when (closure-optimization?)
      (verbose 1 "   . Light closures" #\newline)
      (trace cfa
	     "--------------------------------------"
	     #\Newline "closure-optimization! :" #\Newline
	     (shape *make-procedure-list*)
	     #\Newline)
      ;; we set the local cache
      (start-cache)
      ;; first, we set the X et T fields for each closures.
      (for-each
       (lambda (app)
	  (with-access::make-procedure-app app (lost-stamp X T args X-T?)
	     (let ((fun    (var-variable (car args)))
		   (lost?  (>fx lost-stamp -1))
		   (size   (get-node-atom-value (caddr args))))
		(trace (cfa 3) "light?: " (shape app) " ... ")
		(if (not (or lost?
			     (not X-T?)
			     (<fx (sfun-arity (global-value fun)) 0)
			     (not (fixnum? size))
			     ;; procedure with no free variable
			     ;; cannot be light neither extra-light
			     ;; because they are already optimized
			     ;; by the constant compilation.
			     (=fx size 0)))
		    (begin
		       (trace (cfa 3) " may be X and T." #\Newline)
		       (set! X #t)
		       (set! T #t))
		    (begin
		       (trace (cfa 3) " can't be X nor T because: "
			      #\Newline
			      "        lost?: " lost? #\Newline
			      "        X-T?: " X-T? #\Newline
			      "        size : " size #\Newline
			      "        arity: " (sfun-arity
						 (global-value fun))
			      #\Newline)))))) 
       *make-procedure-list*)
      ;; we compute the X property ...
      (X! *funcall-list*)
      ;; and the T one
      (T-fix-point! *funcall-list*)
      ;; we print the result
      (show-X-T *make-procedure-list*)
      ;; then, we have to scan, all funcall and procedure-ref
      ;; and procedure-set!, procedure? in order to change them according
      ;; to procedure's classifications and free variables types.
      (light-closure! globals)
      ;; we don't need the cache anymore
      (stop-cache)))

;*---------------------------------------------------------------------*/
;*    X! ...                                                           */
;*    -------------------------------------------------------------    */
;*    If type checks are omitted, a funcall which can apply            */
;*    procedure or other types, do not prevent optimization of         */
;*    the called functions.                                            */
;*---------------------------------------------------------------------*/
(define (X! funcall-list)
   (trace (cfa 3) "X! ... " #\Newline)
   (for-each (lambda (app::funcall)
		(trace (cfa 3) "funcall: " (shape app) " ... ")
		(let* ((fun    (funcall-fun app))
		       (approx (cfa! fun))
		       (alloc  (approx-allocs approx))
		       (type   (approx-type approx))
		       (top?   (approx-top? approx)))
		   (cond
		      (top?
		       (trace (cfa 3) "nok (top?)")
		       (for-each-approx-alloc
			(lambda (alloc)
			   (if (make-procedure-app? alloc)
			       (begin
				  (trace (cfa 3) #"nok: " (shape alloc)
					 #\Newline)
				  (make-procedure-app-T-set! alloc #f)
				  (make-procedure-app-X-set! alloc #f))))
			approx))
		      ((=fx (set-length alloc) 0)
		       (trace (cfa 3) #"ok.\n")
		       'ok)
		      ((and (=fx (set-length alloc) 1)
			    (or *unsafe-type* (eq? type *procedure*)))
		       (trace (cfa 3) #"ok.\n")
		       'ok)
		      (else
		       ;; several function can be applied, the
		       ;; closure does not satisfy X.
		       (for-each-approx-alloc
			(lambda (alloc)
			   (if (make-procedure-app? alloc)
			       (begin
				  (trace (cfa 3) #"nok: " (shape alloc)
					 #\Newline)
				  (make-procedure-app-X-set! alloc #f))))
			approx)))))
	     funcall-list))
   
;*---------------------------------------------------------------------*/
;*    T-fix-point! ...                                                 */
;*    -------------------------------------------------------------    */
;*    The computation of T require a fix point under all the funcall.  */
;*    -------------------------------------------------------------    */
;*    If type checks are omitted, a funcall which can apply            */
;*    procedure or other types, do not prevent optimization of         */
;*    the called functions.                                            */
;*---------------------------------------------------------------------*/
(define (T-fix-point! funcall-list)
   (trace cfa "T-fix-point! ... " #\Newline)
   (let loop ((continue? #t))
      (if (not continue?)
	  #unspecified
	  (let ((continue? #f))
	     (for-each
	      (lambda (app)
		 (trace (cfa 3) "funcall: " (shape app) #\Newline)
		 (let* ((fun     (funcall-fun app))
			(approx  (cfa! fun))
			(alloc   (set->list (approx-allocs approx)))
			(type    (approx-type approx))
			(T-init? (or (approx-top? approx)
				     (not (or (eq? type *procedure*)
					      *unsafe-type*)))))
		    (let loop ((one-non-T? T-init?)
			       (allocs     alloc))
		       (trace (cfa 3)
			      "      one-non-T?: " one-non-T? #\newline
			      "          allocs: " (shape allocs) #\Newline)
		       (cond
			  ((null? allocs)
			   'done)
			  (one-non-T?
			   (for-each
			    (lambda (alloc)
			       (if (make-procedure-app? alloc)
				   (with-access::make-procedure-app alloc (T)
				      (if T
					  (begin
					     (set! T #f)
					     (set! continue? #t))))))
			    alloc))
			  ((make-procedure-app? (car allocs))
			   (with-access::make-procedure-app (car allocs) (T)
			      (if T
				  (loop one-non-T? (cdr allocs))
				  (loop #t allocs))))
			  (else
			   (if *unsafe-type*
			       (loop one-non-T? (cdr allocs))
			       (loop #t allocs)))))))
	      funcall-list)
	     (loop continue?)))))

;*---------------------------------------------------------------------*/
;*    make-procedure->function ...                                     */
;*---------------------------------------------------------------------*/
(define (make-procedure->function app::make-procedure-app)
   (with-access::make-procedure-app app (args)
      (var-variable (car args))))

;*---------------------------------------------------------------------*/
;*    light-closure! ...                                               */
;*---------------------------------------------------------------------*/
(define (light-closure! globals)
   (light-funcall!)
   (light-access!)
   (light-make-procedure!)
   ;; CARE: *saw* (fomerly invoked only with *saw*)
   (light-type! globals))

;*---------------------------------------------------------------------*/
;*    light-make-procedure! ...                                        */
;*    -------------------------------------------------------------    */
;*    Globalize pass (in file Globalize/free.scm) has set the class    */
;*    field for global procedure to 'sprocedure. We must remove this   */
;*    for optimized procedure otherwise the Cgen pass will crash in    */
;*    the sprocedure prototypes emmission.                             */
;*---------------------------------------------------------------------*/
(define (light-make-procedure!)
   (define (make-elight-procedure-app app)
      (with-access::make-procedure-app app (fun args)
	 (let* ((size (get-node-atom-value (caddr args)))
		(ffun (var-variable (car args)))
		(sfun (variable-value ffun)))
	    (cond
	       ((=fx size 1)
		(if (and (global? ffun)
			 (global? (sfun-the-closure sfun))
			 (scnst?  (global-value (sfun-the-closure sfun))))
		    (scnst-class-set! (global-value (sfun-the-closure sfun))
				      'selfun))
		(var-variable-set! fun *make-el-procedure-1*))
	       (else
		(var-variable-set! fun *make-el-procedure*))))
	 (set! args (cddr args))
	 app))
   (define (make-light-procedure-app app)
      (with-access::make-procedure-app app (fun args)
	 (let* ((size (get-node-atom-value (caddr args)))
		(ffun (var-variable (car args)))
		(sfun (variable-value ffun)))
	    (if (and (global? ffun)
		     (global? (sfun-the-closure sfun))
		     (scnst? (global-value (sfun-the-closure sfun))))
		(scnst-class-set! (global-value (sfun-the-closure sfun))
				  'sefun))
	    (var-variable-set! fun *make-l-procedure*)
	    (set-cdr! args (cddr args))
	    app)))
   ;; we change the procedure allocation sites
   (for-each (lambda (app)
		(with-access::make-procedure-app app (X T)
		   (cond
		      (X (make-elight-procedure-app app))
		      (T (make-light-procedure-app app)))))
	     *make-procedure-list*))

;*---------------------------------------------------------------------*/
;*    light-funcall! ...                                               */
;*---------------------------------------------------------------------*/
(define (light-funcall!)
   (for-each (lambda (app::funcall)
		(trace (cfa 2) "light-funcall!: " (shape app) " ... ")
		(let* ((fun        (funcall-fun app))
		       (approx     (cfa! fun))
		       (alloc-list (set->list (approx-allocs approx))))
		   (if (or (not (pair? alloc-list))
			   (not (make-procedure-app? (car alloc-list))))
		       (begin
			  (trace (cfa 2) "heavy1" #\Newline)
			  'nothing-to-do)
		       (let ((alloc::app (car alloc-list)))
			  (with-access::make-procedure-app alloc (X T args)
			     (cond
				(X
				 (trace (cfa 2) "extra-light" #\Newline)
				 (funcall-fun-set!
				  app
				  (duplicate::var (car args)))
				 (funcall-strength-set! app 'elight))
				(T
				 (trace (cfa 2) "light" #\Newline)
				 (funcall-strength-set! app 'light))
				(else
				 (trace (cfa 2) "heavy2" #\Newline)
				 'nothing-to-do)))))))
	     *funcall-list*))

;*---------------------------------------------------------------------*/
;*    light-access! ...                                                */
;*---------------------------------------------------------------------*/
(define (light-access!)
   (for-each
    (lambda (app)
       (let* ((args       (app-args app))
	      (approx     (cfa! (car (app-args app))))
	      (alloc-list (set->list (approx-allocs approx)))
	      (fun        (app-fun app))
	      (vfun       (var-variable fun)))
	  (if (or (not (pair? alloc-list))
		  (not (make-procedure-app? (car alloc-list))))
	      'nothing-to-do
	      (let ((alloc::app (car alloc-list)))
		 (with-access::make-procedure-app alloc (X T args)
		    (cond
		       (X
			(if (=fx (get-node-atom-value (caddr args)) 1)
			    (if (eq? vfun *procedure-ref*)
				(var-variable-set! fun *procedure-1-el-ref*)
				(var-variable-set! fun *procedure-1-el-set!*))
			    (if (eq? vfun *procedure-ref*)
				(var-variable-set! fun *procedure-el-ref*)
				(var-variable-set! fun *procedure-el-set!*))))
		       (T
			(if (eq? vfun *procedure-ref*)
			    (var-variable-set! fun *procedure-l-ref*)
			    (var-variable-set! fun *procedure-l-set!*)))))))))
    *procedure-ref-list*))

;*---------------------------------------------------------------------*/
;*    show-X-T ...                                                     */
;*---------------------------------------------------------------------*/
(define (show-X-T allocs)
   (define (show prop l)
      (if (pair? l)
	  (begin
	     (verbose 2 "     " prop ": " (shape (car l)) #\Newline)
	     (for-each (lambda (x)
			  (verbose 2 "        " (shape x) #\newline))
		       (cdr l)))))
   (let loop ((Xp    '())
	      (Tp    '())
	      (allocs allocs))
      (if (null? allocs)
	  (begin
	     (show 'X Xp)
	     (show 'T Tp)
	     #unspecified)
	  (with-access::make-procedure-app (car allocs) (args T X)
	     (cond
		(X
		 (loop (cons (var-variable (car args)) Xp) Tp (cdr allocs)))
		(T
		 (loop Xp (cons (var-variable (car args)) Tp) (cdr allocs)))
		(else
		 (loop Xp Tp (cdr allocs))))))))
		       
;*---------------------------------------------------------------------*/
;*    lists for the closure optimization                               */
;*---------------------------------------------------------------------*/
(define *funcall-list*        '())
(define *make-procedure-list* '())
(define *procedure-ref-list*  '())

;*---------------------------------------------------------------------*/
;*    add-funcall! ...                                                 */
;*---------------------------------------------------------------------*/
(define (add-funcall! ast)
   (if (closure-optimization?)
       (set! *funcall-list* (cons ast *funcall-list*))))

;*---------------------------------------------------------------------*/
;*    add-make-procedure! ...                                          */
;*---------------------------------------------------------------------*/
(define (add-make-procedure! ast)
   (if (closure-optimization?)
       (set! *make-procedure-list* (cons ast *make-procedure-list*))))

;*---------------------------------------------------------------------*/
;*    add-procedure-ref! ...                                           */
;*---------------------------------------------------------------------*/
(define (add-procedure-ref! ast)
   (if (closure-optimization?)
       (set! *procedure-ref-list* (cons ast *procedure-ref-list*))))

;*---------------------------------------------------------------------*/
;*    approx-procedure-el? ...                                         */
;*---------------------------------------------------------------------*/
(define (approx-procedure-el? approx)
   (let ((type (approx-type approx))
	 (alloc-list (set->list (approx-allocs approx))))
      (and (eq? type *procedure*)
	   (pair? alloc-list)
	   (null? (cdr alloc-list))
	   (make-procedure-app? (car alloc-list))
	   (make-procedure-app-X (car alloc-list)))))

;*---------------------------------------------------------------------*/
;*    approx-procedure-el1? ...                                        */
;*---------------------------------------------------------------------*/
(define (approx-procedure-el1? approx)
   (let ((alloc-list (set->list (approx-allocs approx))))
      (with-access::make-procedure-app (car alloc-list) (fun)
	 (eq? (var-variable fun) *make-el-procedure-1*))))
   
;*---------------------------------------------------------------------*/
;*    A small cache                                                    */
;*---------------------------------------------------------------------*/
(define *procedure-ref*       #f)
(define *procedure-set!*      #f)
(define *procedure-l-ref*     #f)
(define *procedure-l-set!*    #f)
(define *procedure-el-ref*    #f)
(define *procedure-el-set!*   #f)
(define *procedure-1-el-ref*  #f)
(define *procedure-1-el-set!* #f)
(define *make-el-procedure*   #f)
(define *make-el-procedure-1* #f)
(define *make-l-procedure*    #f)

;*---------------------------------------------------------------------*/
;*    start-cache ...                                                  */
;*---------------------------------------------------------------------*/
(define (start-cache)
   (set! *procedure-ref* (get-global/module 'procedure-ref 'foreign))
   (set! *procedure-set!* (get-global/module 'procedure-set! 'foreign))
   (set! *procedure-l-ref* (get-global/module 'procedure-l-ref 'foreign))
   (set! *procedure-l-set!* (get-global/module 'procedure-l-set! 'foreign))
   (set! *procedure-el-ref* (get-global/module 'procedure-el-ref 'foreign))
   (set! *procedure-el-set!* (get-global/module 'procedure-el-set! 'foreign))
   (set! *procedure-1-el-ref* (get-global/module 'procedure-1-el-ref 'foreign))
   (set! *procedure-1-el-set!* (get-global/module 'procedure-1-el-set! 'foreign))
   (set! *make-el-procedure* (get-global/module 'make-el-procedure 'foreign))
   (set! *make-el-procedure-1* (get-global/module 'make-el-procedure-1 'foreign))
   (set! *make-l-procedure* (get-global/module 'make-l-procedure 'foreign)))

;*---------------------------------------------------------------------*/
;*    stop-cache ...                                                   */
;*---------------------------------------------------------------------*/
(define (stop-cache)
   (set! *procedure-ref* #f)
   (set! *procedure-set!* #f)
   (set! *procedure-l-ref* #f)
   (set! *procedure-l-set!* #f)
   (set! *procedure-el-ref* #f)
   (set! *procedure-el-set!* #f)
   (set! *procedure-1-el-ref* #f)
   (set! *procedure-1-el-set!* #f)
   (set! *make-el-procedure* #f)
   (set! *make-el-procedure-1* #f)
   (set! *make-l-procedure* #f))
   
