;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Abound/walk.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep  7 05:11:17 2010                          */
;*    Last change :  Fri Apr 21 18:41:24 2017 (serrano)                */
;*    Copyright   :  2010-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Introduce array bound checks                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module abound_walk
   (include "Engine/pass.sch"
	    "Ast/node.sch"
	    "Tools/location.sch")
   (import  tools_error
	    tools_shape
	    tools_location
	    type_cache
	    ast_ident
	    ast_local
	    ast_env
	    ast_sexp
	    ast_private
	    ast_lvtype
	    ast_dump
	    backend_backend)
   (export  (abound-walk! globals)))

;*---------------------------------------------------------------------*/
;*    abound-walk! ...                                                 */
;*    -------------------------------------------------------------    */
;*    When the compiler is running in unsafe-range mode, this function */
;*    is not even called.                                              */
;*---------------------------------------------------------------------*/
(define (abound-walk! globals)
   (pass-prelude "Abound" init-cache!) 
   (for-each abound-fun! globals)
   (pass-postlude globals clear-cache!))

;*---------------------------------------------------------------------*/
;*    cache ...                                                        */
;*---------------------------------------------------------------------*/
(define *string-ref* #unspecified)
(define *string-set!* #unspecified)
(define *struct-ref* #unspecified)
(define *struct-set!* #unspecified)
(define *s8vector-ref* #unspecified)
(define *u8vector-ref* #unspecified)
(define *s16vector-ref* #unspecified)
(define *u16vector-ref* #unspecified)
(define *s32vector-ref* #unspecified)
(define *u32vector-ref* #unspecified)
(define *s64vector-ref* #unspecified)
(define *u64vector-ref* #unspecified)
(define *f32vector-ref* #unspecified)
(define *f64vector-ref* #unspecified)
(define *s8vector-set!* #unspecified)
(define *u8vector-set!* #unspecified)
(define *s16vector-set!* #unspecified)
(define *u16vector-set!* #unspecified)
(define *s32vector-set!* #unspecified)
(define *u32vector-set!* #unspecified)
(define *s64vector-set!* #unspecified)
(define *u64vector-set!* #unspecified)
(define *f32vector-set!* #unspecified)
(define *f64vector-set!* #unspecified)

;*---------------------------------------------------------------------*/
;*    init-cache! ...                                                  */
;*---------------------------------------------------------------------*/
(define (init-cache!)
   (set! *string-ref* (find-global '$string-ref 'foreign))
   (set! *string-set!* (find-global '$string-set! 'foreign))
   (set! *struct-ref* (find-global '$struct-ref 'foreign))
   (set! *struct-set!* (find-global '$struct-set! 'foreign))
   (set! *s8vector-ref* (find-global '$s8vector-ref 'foreign))
   (set! *u8vector-ref* (find-global '$u8vector-ref 'foreign))
   (set! *s16vector-ref* (find-global '$s16vector-ref 'foreign))
   (set! *u16vector-ref* (find-global '$u16vector-ref 'foreign))
   (set! *s32vector-ref* (find-global '$s32vector-ref 'foreign))
   (set! *u32vector-ref* (find-global '$u32vector-ref 'foreign))
   (set! *s64vector-ref* (find-global '$s64vector-ref 'foreign))
   (set! *u64vector-ref* (find-global '$u64vector-ref 'foreign))
   (set! *f32vector-ref* (find-global '$f32vector-ref 'foreign))
   (set! *f64vector-ref* (find-global '$f64vector-ref 'foreign))
   (set! *s8vector-set!* (find-global '$s8vector-set! 'foreign))
   (set! *u8vector-set!* (find-global '$u8vector-set! 'foreign))
   (set! *s16vector-set!* (find-global '$s16vector-set! 'foreign))
   (set! *u16vector-set!* (find-global '$u16vector-set! 'foreign))
   (set! *s32vector-set!* (find-global '$s32vector-set! 'foreign))
   (set! *u32vector-set!* (find-global '$u32vector-set! 'foreign))
   (set! *s64vector-set!* (find-global '$s64vector-set! 'foreign))
   (set! *u64vector-set!* (find-global '$u64vector-set! 'foreign))
   (set! *f32vector-set!* (find-global '$f32vector-set! 'foreign))
   (set! *f64vector-set!* (find-global '$f64vector-set! 'foreign))
   #unspecified)

;*---------------------------------------------------------------------*/
;*    clear-cache! ...                                                 */
;*---------------------------------------------------------------------*/
(define (clear-cache!)
   (set! *string-ref* #f))

;*---------------------------------------------------------------------*/
;*    abound-fun! ...                                                  */
;*---------------------------------------------------------------------*/
(define (abound-fun! var)
   (enter-function (variable-id var))
   (let ((fun (variable-value var)))
      (sfun-body-set! fun (abound-node (sfun-body fun)))
      (leave-function)
      var))

;*---------------------------------------------------------------------*/
;*    abound-node ...                                                  */
;*---------------------------------------------------------------------*/
(define-generic (abound-node::node node::node)
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::sequence ...                                       */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::sequence)
   (abound-node*! (sequence-nodes node))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::sync ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::sync)
   (with-access::sync node (mutex body)
      (set! mutex (abound-node mutex))
      (set! body (abound-node body))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::app ...                                            */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::app)

   (define (abound-string-access node)
      (with-access::app node (fun args loc)
	 (let* ((s (mark-symbol-non-user! (gensym 's)))
		(i (mark-symbol-non-user! (gensym 'i)))
		(l (mark-symbol-non-user! (gensym 'l)))
		(lname (when (location? loc) (location-full-fname loc)))
		(lpos (when (location? loc) (location-pos loc)))
		(v (var-variable fun))
		(name (if (eq? v *string-ref*) "string-ref" "string-set!"))
		(types (cfun-args-type (variable-value v))))
	    (top-level-sexp->node
	     `(let ((,(make-typed-ident s (type-id (car types))) ,(car args))
		    (,(make-typed-ident i (type-id (cadr types))) ,(cadr args)))
		 (let ((,(make-typed-ident l (type-id (cadr types)))
			($string-length ,s)))
		    (if ($string-bound-check? ,i ,l)
			,(duplicate::app node
			    (args (cons* s i (cddr args))))
			(failure
			 ((@ index-out-of-bounds-error __error)
			  ,lname ,lpos ,name ,s ,l ,i)
			 #f #f))))
	     loc))))

   (abound-node*! (app-args node))
   ;; check if we are calling string-ref/set!, or struct-ref/set!
   (with-access::app node (fun)
      (let ((v (var-variable fun)))
	 (cond
	    ((or (eq? v *string-ref*) (eq? v *string-set!*))
	     (let ((node (abound-string-access node)))
		(lvtype-node! node)
		node))
	    ((or (eq? v *s8vector-ref*)
		 (eq? v *s16vector-ref*)
		 (eq? v *s32vector-ref*)
		 (eq? v *s64vector-ref*)
		 (eq? v *u8vector-ref*)
		 (eq? v *u16vector-ref*)
		 (eq? v *u32vector-ref*)
		 (eq? v *u64vector-ref*)
		 (eq? v *f32vector-ref*)
		 (eq? v *f64vector-ref*))
	     (lvtype-node
		(with-access::app node (fun args loc)
		   (array-ref node (car args) (cadr args) loc
		      (global-type v)
		      (cadr (cfun-args-type (global-value v)))
		      (car (cfun-args-type (global-value v)))
		      (lambda (node v i)
			 (duplicate::app node
			    (args (list v i))))))))
	    ((or (eq? v *s8vector-set!*)
		 (eq? v *s16vector-set!*)
		 (eq? v *s32vector-set!*)
		 (eq? v *s64vector-set!*)
		 (eq? v *u8vector-set!*)
		 (eq? v *u16vector-set!*)
		 (eq? v *u32vector-set!*)
		 (eq? v *u64vector-set!*)
		 (eq? v *f32vector-set!*)
		 (eq? v *f64vector-set!*))
	     (lvtype-node
		(with-access::app node (fun args loc)
		   (array-set! node (car args) (cadr args) loc
		      (caddr (cfun-args-type (global-value v)))
		      (cadr (cfun-args-type (global-value v)))
		      (car (cfun-args-type (global-value v)))
		      (lambda (node vec i)
			 (with-access::app node (args)
			    (duplicate::app node
			       (args (list vec i (caddr args))))))))))
	    (else
	     node)))))

;*---------------------------------------------------------------------*/
;*    abound-node ::app-ly ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::app-ly)
   (with-access::app-ly node (fun arg)
      (set! fun (abound-node fun))
      (set! arg (abound-node arg))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::funcall ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::funcall)
   (with-access::funcall node (fun args)
      (set! fun (abound-node fun))
      (abound-node*! args)
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::extern ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::extern)
   (abound-node*! (extern-expr* node))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::vref ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::vref)

   (define (abound-vref node)
      (with-access::vref node (expr* loc ftype otype vtype)
	 (array-ref node (car expr*) (cadr expr*) loc ftype otype vtype 
	    (lambda (node v i) (duplicate::vref node (expr* (list v i)))))))

   (call-next-method)
   (with-access::vref node (unsafe)
      (if unsafe
	  node
	  (let ((node (abound-vref node)))
	     (lvtype-node! node)
	     node))))

;*---------------------------------------------------------------------*/
;*    array-ref ...                                                    */
;*---------------------------------------------------------------------*/
(define (array-ref node vec off loc ftype otype vtype dup)
   (let ((v (mark-symbol-non-user! (gensym 'v)))
	 (i (mark-symbol-non-user! (gensym 'i)))
	 (l (mark-symbol-non-user! (gensym 'l)))
	 (lname (when (location? loc) (location-full-fname loc)))
	 (lpos (when (location? loc) (location-pos loc))))
      (top-level-sexp->node
	 `(let ((,(make-typed-ident v (type-id vtype)) ,vec)
		(,(make-typed-ident i (type-id otype)) ,off))
	     (let ((,(make-typed-ident l (type-id otype))
		    ,(cond
			((eq? vtype *vector*)
			 `($vector-length ,v))
			((memq vtype *hvectors*)
			 `($hvector-length ,v))
			(else
			 `($tvector-length ,v)))))
		(if ($vector-bound-check? ,i ,l)
		    ,(dup node v i)
		    (failure
		       ((@ index-out-of-bounds-error __error)
			,lname ,lpos
			,(string-append
			    (symbol->string! (type-id vtype)) "-ref")
			,v ,l ,i)
		       #f #f))))
	 loc)))

;*---------------------------------------------------------------------*/
;*    abound-node ::vset! ...                                          */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::vset!)

   (define (abound-vset node)
      (with-access::vset! node (expr* loc ftype otype vtype)
	 (array-set! node (car expr*) (cadr expr*) loc ftype otype vtype
	    (lambda (node v i)
	       (with-access::vset! node (expr*)
		  (duplicate::vset! node
		     (expr* (list v i (caddr expr*)))))))))

   (call-next-method)
   (with-access::vset! node (unsafe)
      (if unsafe
	  node
	  (let ((node (abound-vset node)))
	     (lvtype-node! node)
	     node))))

;*---------------------------------------------------------------------*/
;*    array-set! ...                                                   */
;*---------------------------------------------------------------------*/
(define (array-set! node vec off loc ftype otype vtype dup)
   (let ((v (mark-symbol-non-user! (gensym 'v)))
	 (i (mark-symbol-non-user! (gensym 'i)))
	 (l (mark-symbol-non-user! (gensym 'l)))
	 (lname (when (location? loc) (location-full-fname loc)))
	 (lpos (when (location? loc) (location-pos loc))))
      (top-level-sexp->node
	 `(let ((,(make-typed-ident v (type-id vtype)) ,vec)
		(,(make-typed-ident i (type-id otype)) ,off))
	     (let ((,(make-typed-ident l (type-id otype))
		    ,(cond
			((eq? vtype *vector*)
			 `($vector-length ,v))
			((memq vtype *hvectors*)
			 `($hvector-length ,v))
			(else
			 `($tvector-length ,v)))))
		(if ($vector-bound-check? ,i ,l)
		    ,(dup node v i)
		    (failure
		       ((@ index-out-of-bounds-error __error)
			,lname ,lpos
			,(string-append
			    (symbol->string! (type-id vtype)) "-set!")
			,v ,l ,i)
		       #f #f))))
	 loc)))
   
;*---------------------------------------------------------------------*/
;*    abound-node ::cast ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::cast)
   (abound-node (cast-arg node))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::setq ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::setq)
   (setq-value-set! node (abound-node (setq-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::conditional ...                                    */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::conditional)
   (with-access::conditional node (test true false)
       (set! test (abound-node test))
       (set! true (abound-node true))
       (set! false (abound-node false))
       node))

;*---------------------------------------------------------------------*/
;*    abound-node ::fail ...                                           */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::fail)
   (with-access::fail node (proc msg obj)
      (set! proc (abound-node proc))
      (set! msg (abound-node msg))
      (set! obj (abound-node obj))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::switch ...                                         */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::switch)
   (with-access::switch node (clauses test)
      (set! test (abound-node test))
      (for-each (lambda (clause)
		   (set-cdr! clause (abound-node (cdr clause))))
		clauses)
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::let-fun ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::let-fun)
   (with-access::let-fun node (body locals)
      (for-each (lambda (v) (abound-fun! v)) locals)
      (set! body (abound-node body))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::let-var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::let-var)
   (with-access::let-var node (body bindings)
      (for-each (lambda (binding)
		   (set-cdr! binding (abound-node (cdr binding))))
		bindings)
      (set! body (abound-node body))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::set-ex-it ...                                      */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::set-ex-it)
   (set-ex-it-body-set! node (abound-node (set-ex-it-body node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::jump-ex-it ...                                     */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::jump-ex-it)
   (with-access::jump-ex-it node (exit value)
      (set! exit (abound-node exit))
      (set! value (abound-node value))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node ::make-box ...                                       */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::make-box)
   (make-box-value-set! node (abound-node (make-box-value node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::box-ref ...                                        */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::box-ref)
   (box-ref-var-set! node (abound-node (box-ref-var node)))
   node)

;*---------------------------------------------------------------------*/
;*    abound-node ::box-set! ...                                       */
;*---------------------------------------------------------------------*/
(define-method (abound-node node::box-set!)
   (with-access::box-set! node (var value)
      (set! var (abound-node var))
      (set! value (abound-node value))
      node))

;*---------------------------------------------------------------------*/
;*    abound-node*! ...                                                */
;*---------------------------------------------------------------------*/
(define (abound-node*! node*)
   (when (pair? node*)
      (set-car! node* (abound-node (car node*)))
      (abound-node*! (cdr node*))))
   
