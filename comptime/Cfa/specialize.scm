;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cfa/specialize.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Fri Apr 11 13:18:21 1997                          */
;*    Last change :  Sat Dec  9 08:53:46 2017 (serrano)                */
;*    Copyright   :  1997-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    This module implements an optimization asked by John Gerard      */
;*    Malecki <johnm@vlibs.com>. What it does is, for each generic     */
;*    operation (e.g. +, max, ...) if a specialized operation exists,  */
;*    regarding Cfa type informations, the generic operation is        */
;*    replaced by the specific one.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cfa_specialize
   (include "Tools/trace.sch"
	    "Cfa/specialize.sch")
   (import  engine_param
	    type_type
	    type_cache
	    type_typeof
	    tools_shape
	    tools_speek
	    tools_error
	    backend_backend
	    ast_var
	    ast_node
	    ast_env
	    inline_inline
	    inline_walk)
   (static  (wide-class specialized-global::global
	       fix))
   (export  (specialize! globals)
	    (arithmetic-operator? ::global)
	    (arithmetic-spec-types ::global)))

;*---------------------------------------------------------------------*/
;*    specialize! ...                                                  */
;*---------------------------------------------------------------------*/
(define (specialize! globals)
   (when (specialize-optimization?)
      (trace (cfa 4) "============= specialize arithmetic ===============\n")
      (for-each install-specialize! (get-specializations))
      (patch-tree! globals)
      (show-specialize)
      (uninstall-specializes!))
   globals)

;*---------------------------------------------------------------------*/
;*    arithmetic-operator? ...                                         */
;*---------------------------------------------------------------------*/
(define (arithmetic-operator? global)
   (with-access::global global (id module)
      (let ((cell (assq id (get-specializations))))
	 (and (pair? cell) (eq? (cadr (cadr cell)) module)))))
	 
;*---------------------------------------------------------------------*/
;*    arithmetic-spec-types ...                                        */
;*    -------------------------------------------------------------    */
;*    In order to find the types an arithmetic operator is specialized */
;*    for, we inspect which modules defines its specialized versions.  */
;*---------------------------------------------------------------------*/
(define (arithmetic-spec-types global)
   (with-access::global global (id module)
      (let ((cell (assq id (get-specializations))))
	 (if (not (pair? cell))
	     '()
	     (let loop ((spec (cddr cell))
			(types '()))
		(cond
		   ((null? spec)
		    types)
		   ((and *optim-cfa-fixnum-arithmetic?*
			 (eq? (cadr (car spec)) '__r4_numbers_6_5_fixnum))
		    (loop (cdr spec) (cons *long* types)))
		   ((and *optim-cfa-flonum-arithmetic?*
			 (eq? (cadr (car spec)) '__r4_numbers_6_5_flonum))
		    (loop (cdr spec) (cons *real* types)))
		   (else
		    (loop (cdr spec) types))))))))

;*---------------------------------------------------------------------*/
;*    get-specializations ...                                          */
;*---------------------------------------------------------------------*/
(define (get-specializations)
   (cond
      (*specializations*
       ;;; already computed
       *specializations*)
      (*arithmetic-overflow*
       ;;; compilation in a safe fixnum arithmetic mode
       (set! *specializations* *specializations-sans-overflow*)
       *specializations*)
      (else
       ;;; don't check fixnum overflow, deploy more aggressive optimizations
       (set! *specializations*
	  (append *specializations-overflow* *specializations-sans-overflow*))
       *specializations*)))

;*---------------------------------------------------------------------*/
;*    *specializations* ...                                            */
;*---------------------------------------------------------------------*/
(define *specializations* #f)

;*---------------------------------------------------------------------*/
;*    *specializations-sans-overflow* ...                              */
;*---------------------------------------------------------------------*/
(define *specializations-sans-overflow*
   '((2+ (2+ __r4_numbers_6_5)
	(+fl __r4_numbers_6_5_flonum))
     (2- (2- __r4_numbers_6_5)
	(-fl __r4_numbers_6_5_flonum))
     (2* (2* __r4_numbers_6_5)
	(*fl __r4_numbers_6_5_flonum))
     (2/ (2/ __r4_numbers_6_5)
	(/fl __r4_numbers_6_5_flonum))
     (abs (abs __r4_numbers_6_5)
	(absfl __r4_numbers_6_5_flonum))
     (c-eq? (c-eq? foreign)
	(=fx __r4_numbers_6_5_fixnum)
	(=fl __r4_numbers_6_5_flonum))
     (2= (2= __r4_numbers_6_5)
	(=fx __r4_numbers_6_5_fixnum)
	(=fl __r4_numbers_6_5_flonum))
     (2< (2< __r4_numbers_6_5)
	(<fx __r4_numbers_6_5_fixnum)
	(<fl __r4_numbers_6_5_flonum))
     (2> (2> __r4_numbers_6_5)
	(>fx __r4_numbers_6_5_fixnum)
	(>fl __r4_numbers_6_5_flonum))
     (2<= (2<= __r4_numbers_6_5)
	(<=fx __r4_numbers_6_5_fixnum)
	(<=fl __r4_numbers_6_5_flonum))
     (2>= (2>= __r4_numbers_6_5)
	(>=fx __r4_numbers_6_5_fixnum)
	(>=fl __r4_numbers_6_5_flonum))
     (zero? (zero? __r4_numbers_6_5)
	(zerofx? __r4_numbers_6_5_fixnum)
	(zerofl? __r4_numbers_6_5_flonum))
     (positive? (positive? __r4_numbers_6_5)
	(positivefx? __r4_numbers_6_5_fixnum)
	(positivefl? __r4_numbers_6_5_flonum))
     (negative? (negative? __r4_numbers_6_5)
	(negativefx? __r4_numbers_6_5_fixnum)
	(negativefl? __r4_numbers_6_5_flonum))
     (log (log __r4_numbers_6_5)
	(logfl __r4_numbers_6_5_flonum))
     (exp (exp __r4_numbers_6_5)
	(expfl __r4_numbers_6_5_flonum))
     (sin (sin __r4_numbers_6_5)
	(sinfl __r4_numbers_6_5_flonum))
     (cos (cos __r4_numbers_6_5)
	(cosfl __r4_numbers_6_5_flonum))
     (tan (tan __r4_numbers_6_5)
	(tanfl __r4_numbers_6_5_flonum))
     (atan (atan __r4_numbers_6_5)
	(atanfl __r4_numbers_6_5_flonum))
     (asin (asin __r4_numbers_6_5)
	(asinfl __r4_numbers_6_5_flonum))
     (acos (acos __r4_numbers_6_5)
	(acosfl __r4_numbers_6_5_flonum))
     (sqrt (sqrt __r4_numbers_6_5)
	(sqrtfl __r4_numbers_6_5_flonum))))

(define *specializations-overflow*
   '((2+ (2+ __r4_numbers_6_5)
	(+fl __r4_numbers_6_5_flonum)
	(+fx __r4_numbers_6_5_fixnum))
     (2- (2- __r4_numbers_6_5)
	(-fl __r4_numbers_6_5_flonum)
	(-fx __r4_numbers_6_5_fixnum))
     (2* (2* __r4_numbers_6_5)
	(*fl __r4_numbers_6_5_flonum)
	(*fx __r4_numbers_6_5_fixnum))
     (abs (abs __r4_numbers_6_5)
	(absfl __r4_numbers_6_5_flonum)
	(absfx __r4_numbers_6_5_fixnum))))

(define *c-eq?* #unspecified)

;*---------------------------------------------------------------------*/
;*    specialize-optimization? ...                                     */
;*---------------------------------------------------------------------*/
(define (specialize-optimization?)
   (and (>=fx *optim* 1) (not *lib-mode*)))

;*---------------------------------------------------------------------*/
;*    show-specialize ...                                              */
;*---------------------------------------------------------------------*/
(define (show-specialize)
   (verbose 1 "   . Specializing" #\newline)
   (for-each (lambda (type-num)
		(if (>fx (cdr type-num) 0)
		    (verbose 2 "        (-> " (shape (car type-num))
			     ": " (cdr type-num) #")\n")))
	     *specialized-types*)
   (for-each (lambda (type-num)
		(if (>fx (cdr type-num) 0)
		    (verbose 2 "        (eq? " (shape (car type-num))
			     ": " (cdr type-num) #")\n")))
	     *specialized-eq-types*))

;*---------------------------------------------------------------------*/
;*    *specialize* ...                                                 */
;*---------------------------------------------------------------------*/
(define *specialize* '())
(define *specialized-types* '())
(define *specialized-eq-types* '())
(define *boxed-eq?* #unspecified)

;*---------------------------------------------------------------------*/
;*    install-specialized-type! ...                                    */
;*---------------------------------------------------------------------*/
(define (install-specialized-type! type)
   (unless (pair? (assq type *specialized-types*))
      (set! *specialized-types* (cons (cons type 0) *specialized-types*))
      (set! *boxed-eq?* (find-global 'c-boxed-eq? 'foreign))
      (set! *c-eq?* (find-global 'c-eq? 'foreign))))

;*---------------------------------------------------------------------*/
;*    add-specialized-type! ...                                        */
;*---------------------------------------------------------------------*/
(define (add-specialized-type! type)
   (let ((cell (assq type *specialized-types*)))
      (if (not (pair? cell))
	  (internal-error
	   "add-specialized-type!" "Unspecialized type" (shape type))
	  (set-cdr! cell (+fx 1 (cdr cell))))))

;*---------------------------------------------------------------------*/
;*    add-specialized-eq-type! ...                                     */
;*---------------------------------------------------------------------*/
(define (add-specialized-eq-type! type)
   (let ((cell (assq type *specialized-eq-types*)))
      (if (not (pair? cell))
	  (set! *specialized-eq-types*
	     (cons (cons type 1) *specialized-eq-types*))
	  (set-cdr! cell (+fx 1 (cdr cell))))))

;*---------------------------------------------------------------------*/
;*    install-specialize! ...                                          */
;*---------------------------------------------------------------------*/
(define (install-specialize! spec)
   (let* ((gen (cadr spec))
	  (fixes (cddr spec))
	  (gen-id (car gen))
	  (gen-mod (cadr gen))
	  (generic (find-global/module gen-id gen-mod)))
      (if (global? generic)
	  (let loop ((fixes fixes)
		     (res '()))
	     (if (null? fixes)
		 (if (specialized-global? generic)
		     (with-access::specialized-global generic (fix)
			(set! fix (append fix res)))
		     (begin
			(widen!::specialized-global generic
			   (fix res))
			(set! *specialize* (cons generic *specialize*))))
		 (let* ((fix (car fixes))
			(id (car fix))
			(mod (cadr fix))
			(global (find-global/module id mod)))
		    (if (global? global)
			(let ((val (global-value global)))
			   (cond
			      ((and (sfun? val) (pair? (sfun-args val)))
			       (let ((type (let ((v (car (sfun-args val))))
					      (cond
						 ((type? v)
						  v)
						 ((local? v)
						  (local-type v))
						 (else
						  (internal-error
						   "install-specialize"
						   "Illegal argument"
						   (global-id global)))))))
				  (install-specialized-type! type)
				  (loop (cdr fixes)
					(cons (cons type global) res))))
			      ((and (cfun? val) (pair? (cfun-args-type val)))
			       (let ((type (car (cfun-args-type val))))
				  (install-specialized-type! type)
				  (loop (cdr fixes)
					(cons (cons type global) res))))))
			(begin
			   (warning "install-specialize!"
				    "Can't find global"
				    " -- "
				    (cons id mod))
			   (loop (cdr fixes) res))))))
	  (warning "install-specialize!"
		   "Can't find global"
		   " -- "
		   (cons gen-id gen-mod)))))

;*---------------------------------------------------------------------*/
;*    uninstall-specializes! ...                                       */
;*---------------------------------------------------------------------*/
(define (uninstall-specializes!)
   (set! *specialized-types* '())
   (for-each (lambda (o) (shrink! o)) *specialize*)
   (set! *specialize* '()))

;*---------------------------------------------------------------------*/
;*    patch-tree! ...                                                  */
;*---------------------------------------------------------------------*/
(define (patch-tree! globals)
   (for-each patch-fun! globals))

;*---------------------------------------------------------------------*/
;*    patch-fun! ...                                                   */
;*---------------------------------------------------------------------*/
(define (patch-fun! variable)
   (let ((fun (variable-value variable)))
      (sfun-body-set! fun (patch! (sfun-body fun)))))

;*---------------------------------------------------------------------*/
;*    patch! ...                                                       */
;*---------------------------------------------------------------------*/
(define-generic (patch! node::node))

;*---------------------------------------------------------------------*/
;*    patch! ::atom ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::atom)
   node)

;*---------------------------------------------------------------------*/
;*    patch! ::kwote ...                                               */
;*---------------------------------------------------------------------*/
(define-method (patch! node::kwote)
   node)
		    
;*---------------------------------------------------------------------*/
;*    patch! ::var ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (patch! node::var)
   node)
 
;*---------------------------------------------------------------------*/
;*    patch! ::closure ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::closure)
   (internal-error "patch!" "Unexpected closure" (shape node)))

;*---------------------------------------------------------------------*/
;*    patch! ::sequence ...                                            */
;*---------------------------------------------------------------------*/
(define-method (patch! node::sequence)
   (with-access::sequence node (type nodes)
      (patch*! nodes)
      (when (eq? type *obj*)
	 (set! type *_*)
	 (set! type (get-type node #f)))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::sync ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::sync)
   (with-access::sync node (type body mutex prelock)
      (set! mutex (patch! mutex))
      (set! prelock (patch! prelock))
      (set! body (patch! body))
      (when (eq? type *obj*)
	 (set! type *_*)
	 (set! type (get-type node #f)))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::app-ly ...                                              */
;*---------------------------------------------------------------------*/
(define-method (patch! node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (patch! fun))
      (set! arg (patch! arg))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::funcall ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (patch! fun))
      (patch*! args)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::extern ...                                              */
;*---------------------------------------------------------------------*/
(define-method (patch! node::extern)
   (with-access::extern node (expr* type)
      (patch*! expr*)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::cast ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::cast)
   (with-access::cast node (arg)
      (patch! arg)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::setq ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::setq)
   (with-access::setq node (var value)
      (set! value (patch! value))
      (set! var (patch! var))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::conditional ...                                         */
;*---------------------------------------------------------------------*/
(define-method (patch! node::conditional)
   (with-access::conditional node (type test true false)
       (set! test (patch! test))
       (set! true (patch! true))
       (set! false (patch! false))
       (when (eq? type *obj*)
	  (set! type *_*)
	  (set! type (get-type node #f)))
       node))

;*---------------------------------------------------------------------*/
;*    patch! ::fail ...                                                */
;*---------------------------------------------------------------------*/
(define-method (patch! node::fail)
   (with-access::fail node (proc msg obj)
      (set! proc (patch! proc))
      (set! msg (patch! msg))
      (set! obj (patch! obj))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::switch ...                                              */
;*---------------------------------------------------------------------*/
(define-method (patch! node::switch)
   (with-access::switch node (clauses test)
      (set! test (patch! test))
      (for-each (lambda (clause)
		   (set-cdr! clause (patch! (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::let-fun ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::let-fun)
   (with-access::let-fun node (type body locals)
      (for-each patch-fun! locals)
      (set! body (patch! body))
      (when (eq? type *obj*)
	 (set! type *_*)
	 (set! type (get-type node #f)))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::let-var ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::let-var)
   (with-access::let-var node (type body bindings)
      (for-each (lambda (binding)
		   (let ((val (cdr binding)))
		      (set-cdr! binding (patch! val))))
		bindings)
      (set! body (patch! body))
       (when (eq? type *obj*)
	  (set! type *_*)
	  (set! type (get-type node #f)))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::set-ex-it ...                                           */
;*---------------------------------------------------------------------*/
(define-method (patch! node::set-ex-it)
   (with-access::set-ex-it node (var body)
      (set! body (patch! body))
      (patch! var)
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::jump-ex-it ...                                          */
;*---------------------------------------------------------------------*/
(define-method (patch! node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (patch! exit))
      (set! value (patch! value))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::make-box ...                                            */
;*---------------------------------------------------------------------*/
(define-method (patch! node::make-box)
   (with-access::make-box node (value)
      (set! value (patch! value))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::box-set! ...                                            */
;*---------------------------------------------------------------------*/
(define-method (patch! node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (patch! var))
      (set! value (patch! value))
      node))

;*---------------------------------------------------------------------*/
;*    patch! ::box-ref ...                                             */
;*---------------------------------------------------------------------*/
(define-method (patch! node::box-ref)
   (with-access::box-ref node (var)
      (set! var (patch! var))
      node))

;*---------------------------------------------------------------------*/
;*    patch*! ...                                                      */
;*---------------------------------------------------------------------*/
(define (patch*! node*)
   (let loop ((node* node*))
      (if (null? node*)
	  'done
	  (begin
	     (set-car! node* (patch! (car node*)))
	     (loop (cdr node*))))))

;*---------------------------------------------------------------------*/
;*    patch! ::app ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (patch! node::app)
   (with-access::app node (fun args)
      (patch*! args)
      (set! fun (patch! fun))
      (let ((v (var-variable fun)))
	 (cond
	    ((specialized-global? v)
	     (specialize-app! node))
	    (else
	     node)))))

;*---------------------------------------------------------------------*/
;*    specialize-app! ...                                              */
;*---------------------------------------------------------------------*/
(define (specialize-app!::app node::app)
   
   (define (normalize-get-type val)
      (let ((ty (get-type val #f)))
	 (if (eq? ty *int*)
	     *long*
	     ty)))

   (with-access::app node (fun args)
      (if (null? args)
	  node
	  ;; we check if the type of all the arguments
	  ;; is either fixnum or flonum
	  (let* ((type (normalize-get-type (car args)))
		 (glo (var-variable fun))
		 (spec (assq type (specialized-global-fix glo))))
	     (if (pair? spec)
		 (let loop ((args (cdr args)))
		    (cond
		       ((null? args)
			(add-specialized-type! type)
			(var-variable-set! fun (cdr spec))
			(let ((nt (variable-type (var-variable fun))))
			   (node-type-set! node (strict-node-type nt type)))
			node)
		       ((eq? (normalize-get-type (car args)) type)
			(loop (cdr args)))
		       (else
			;; sorry, it fails
			(specialize-eq? node))))
		 (specialize-eq? node))))))

;*---------------------------------------------------------------------*/
;*    specialize-eq? ...                                               */
;*    -------------------------------------------------------------    */
;*    For the JVM back-end it is important to use a kind of non        */
;*    fully-polymorph definition of EQ? as soon as one of the          */
;*    argument is known not to be an integer.                          */
;*---------------------------------------------------------------------*/
(define (specialize-eq? node)
   (with-access::app node (fun args)
      (define (boxed-eq! node type)
	 (when (global? *boxed-eq?*)
	    (var-variable-set! fun *boxed-eq?*)
	    (add-specialized-eq-type! type)))
      ;; arguments type have been erase, we have to restore them now
      (if (and (eq? (var-variable fun) *c-eq?*)
	       (backend-typed-eq (the-backend)))
	  ;; here we are...
	  (let ((t1 (get-type (car args) #f))
		(t2 (get-type (cadr args) #f)))
	     (cond
		((and (eq? t1 *obj*) (eq? t2 *obj*))
		 #unspecified)
		((eq? t1 *bint*)
		 (if (eq? t2 *bint*)
		     ;; we specialize for an boxed integers eq?
		     (boxed-eq! node *bint*)))
		((eq? t2 *bint*)
		 #unspecified)
		((eq? t1 *breal*)
		 (if (eq? t2 *breal*)
		     ;; we specialize for an boxed reals eq?
		     (boxed-eq! node *breal*)))
		((eq? t2 *breal*)
		 #unspecified)
		((eq? t2 *bint*)
		 #unspecified)
		((eq? t1 *bchar*)
		 (if (eq? t2 *bchar*)
		     ;; we specialize for an boxed chars eq?
		     (boxed-eq! node *bchar*)))
		((eq? t2 *bchar*)
		 #unspecified)
		((and (bigloo-type? t1) (bigloo-type? t2))
		 ;; we specialize with a boxed eq?
		 (boxed-eq! node (if (eq? t1 *obj*) t2 t1)))
		(else
		 #unspecified))
	     node)
	  node)))
<
