;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/Ast/check_type.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 28 17:38:10 2000                          */
;*    Last change :  Thu Jan 24 10:04:47 2019 (serrano)                */
;*    Copyright   :  2000-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This module implements a simple self debug module. It reports on */
;*    nodes that are inconsitently typed.                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module ...                                                   */
;*---------------------------------------------------------------------*/
(module ast_check-type
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   (import  ast_env
	    ast_var
	    ast_node
	    ast_dump
	    engine_param
	    tools_error
	    tools_shape
	    type_type
	    type_cache
	    type_coercion
	    type_typeof
	    tvector_tvector
	    object_class
	    module_module
	    module_include
	    module_class)
   (export  (check-type ::bstring ::pair-nil ::bool ::bool)
	    (%check-node-type ::bstring ::node ::bool ::bool)))

;*---------------------------------------------------------------------*/
;*    %check-node-type ...                                             */
;*---------------------------------------------------------------------*/
(define (%check-node-type fun::bstring node full correctness)
   (set! *check-type-pass* fun)
   (set! *check-full* full)
   (set! *check-correctness* correctness)
   (check-node-type node))

;*---------------------------------------------------------------------*/
;*    check-type ...                                                   */
;*---------------------------------------------------------------------*/
(define (check-type pass::bstring globals full correctness)
   (when *compiler-type-debug?*
      (set! *check-type-pass* pass)
      (set! *check-full* full)
      (set! *check-correctness* correctness)
      (for-each check-type-fun globals)
      (pass-postlude globals)))

;*---------------------------------------------------------------------*/
;*    *check-type-pass* ...                                            */
;*---------------------------------------------------------------------*/
(define *check-type-pass* #f)
(define *check-full* #f)
(define *check-correctness* #f)

;*---------------------------------------------------------------------*/
;*    check-type-fun ...                                               */
;*---------------------------------------------------------------------*/
(define (check-type-fun var)
   (let* ((fun (variable-value var))
	  (body (sfun-body fun)))
      (check-node-type body)))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err node t1 t2)
   (user-error/location (node-loc node)
			(format "check-node-type (~a)" *check-type-pass*)
			(format "Inconsistent type [~a], \"~a\" expected, \"~a\" provided" (typeof node) (shape t2) (shape t1))
			(shape node)))

;*---------------------------------------------------------------------*/
;*    err-no-type ...                                                  */
;*---------------------------------------------------------------------*/
(define (err-no-type node)
   (user-error/location (node-loc node)
			(format "check-node-type (~a)" *check-type-pass*)
			"Untyped node"
			(shape node)))

;*---------------------------------------------------------------------*/
;*    warn ...                                                         */
;*---------------------------------------------------------------------*/
(define (warn node)
   (user-warning/location (node-loc node)
			  "check-node-type: "
			  (format "Inconsistent \"~a\" type" (typeof node))
			  (shape node)))

;*---------------------------------------------------------------------*/
;*    check-node-type ::node ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (check-node-type node::node)
   (when (and *check-full* (eq? (node-type node) *_*))
      (err-no-type node)))

;*---------------------------------------------------------------------*/
;*    check-node-type ::var ...                                        */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::var)
   (with-access::var node (type variable loc)
      (with-access::variable variable ((vtype type) id)
	 (unless (sfun? (variable-value variable))
	    (unless (or (subtype? type vtype)
			(and (eq? type *obj*) (bigloo-type? vtype))
			(and (tclass? vtype) (subtype? vtype type)))
	       (tprint "ERR: " (shape node) " loc=" loc
		  " type=" (shape type) " vtype=" (shape vtype)
		  " eq=" (eq? type vtype) " sub=" (subtype? type vtype)
		  " check-full=" *check-full*)
	       (err node type vtype))
	    (when (and (eq? vtype *_*)
		       (global? variable)
		       (not (eq? (global-import variable) 'static)))
	       (err-no-type node))))))
      
;*---------------------------------------------------------------------*/
;*    check-node-type ::atom ...                                       */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::atom)
   (with-access::atom node (value type)
      (unless (atom-subtype? type (get-type-atom value))
	 (err node type (get-type-atom value)))))
   
;*---------------------------------------------------------------------*/
;*    check-node-type ::kwote ...                                      */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::kwote)
   (with-access::kwote node (value type)
      (unless (eqtype? type (get-type-kwote value))
	 (err node type (get-type-kwote value)))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::closure ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::closure)
   (with-access::closure node (type)
      (unless (eq? type *procedure*)
	 (err node type *procedure*))))
	       
;*---------------------------------------------------------------------*/
;*    check-node-type ::sequence ...                                   */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::sequence)
   (with-access::sequence node (nodes type)
      (if (pair? nodes)
	  (begin
	     (for-each check-node-type nodes)
	     (unless (subtype? (node-type (car (last-pair nodes))) type)
		(err node (node-type (car (last-pair nodes))) type)))
	  (unless (eqtype? type *unspec*)
	     (err node type *unspec*)))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::sync ...                                       */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::sync)
   (with-access::sync node (mutex prelock body type)
      (check-node-type mutex)
      (check-node-type prelock)
      (check-node-type body)))

;*---------------------------------------------------------------------*/
;*    check-node-type ::setq ...                                       */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::setq)
   (with-access::setq node (type var value)
      (check-node-type value)
      (check-node-type var)
      (unless (eq? type *unspec*)
	 (err node type *unspec*))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::conditional ...                                */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::conditional)
   (with-access::conditional node (test true false)
      (check-node-type test)
      (check-node-type true)
      (check-node-type false)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    check-node-type ::fail ...                                       */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::fail)
   (with-access::fail node (type proc msg obj)
      (check-node-type proc)
      (check-node-type msg)
      (check-node-type obj)
      (unless (eq? type *magic*)
	 (err node type *magic*))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::switch ...                                     */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::switch)
   (with-access::switch node (clauses test type)
      (check-node-type test)
      (let loop ((clauses clauses))
	 (if (null? clauses)
	     (call-next-method)
	     (let ((clause (car clauses)))
		(check-node-type (cdr clause))
		(let ((ntype (node-type (cdr clause))))
		   (if (subtype? ntype type)
		       (loop (cdr clauses))
		       (err (cdr clause) ntype type))))))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::let-fun ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::let-fun)
   (with-access::let-fun node (type locals body)
      (for-each check-type-fun locals)
      (check-node-type body)
      (unless (subtype? (node-type body) type)
	 (err node type (node-type body)))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::let-var ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::let-var)
   (with-access::let-var node (type bindings body)
      (for-each (lambda (b) (check-node-type (cdr b))) bindings)
      (check-node-type body)
      (unless (subtype? (node-type body) type)
	 (err node type (node-type body)))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::set-ex-it ...                                  */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::set-ex-it)
   (with-access::set-ex-it node (type var body)
      (check-node-type var)
      (check-node-type body)
      (unless (eqtype? type (get-type node #f))
	 (err node type (node-type body)))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::jump-ex-it ...                                 */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::jump-ex-it)
   (with-access::jump-ex-it node (type exit value)
      (check-node-type exit)
      (check-node-type value)
      (unless (eq? type *unspec*)
	 (err node type *unspec*))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::make-box ...                                   */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::make-box)
   (with-access::make-box node (type value)
      (check-node-type value)
      (unless (eq? type *cell*)
	 (err node type *cell*))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::box-ref ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::box-ref)
   (with-access::box-ref node (type var)
      (check-node-type var)
      (unless (eqtype? *cell* (node-type var))
	 (err node type (variable-type (var-variable var))))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::box-set! ...                                   */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::box-set!)
   (with-access::box-set! node (type var value)
      (check-node-type var)
      (check-node-type value)
      (unless (eq? type *unspec*)
	 (err node type *unspec*))))

;*---------------------------------------------------------------------*/
;*    check-node-type ::app-ly ...                                     */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::app-ly)
   (with-access::app-ly node (type fun arg)
      (check-node-type fun)
      (check-node-type arg)
      (call-next-method)))

;*---------------------------------------------------------------------*/
;*    check-node-type ::funcall ...                                    */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::funcall)
   (with-access::funcall node (type fun args strength)
      (unless (eq? strength 'elight)
	 (check-node-type fun))
      (for-each check-node-type args)
      (call-next-method)))
      
;*---------------------------------------------------------------------*/
;*    check-node-type ::app ...                                        */
;*---------------------------------------------------------------------*/
(define-method (check-node-type node::app)
   (with-access::app node (type fun args)
      (for-each check-node-type args)
      (unless (subtype? type (variable-type (var-variable fun)))
	 (err node type (variable-type (var-variable fun))))))

;*---------------------------------------------------------------------*/
;*    eqtype? ...                                                      */
;*---------------------------------------------------------------------*/
(define (eqtype? t1 t2)
   (or (or (eq? t1 t2) (eq? t1 *magic*) (eq? t2 *magic*))
       (and (eq? t1 *_*) (not *check-full*))))

;*---------------------------------------------------------------------*/
;*    subtype? ...                                                     */
;*    -------------------------------------------------------------    */
;*    Is t1 a subtype of t2?                                           */
;*---------------------------------------------------------------------*/
(define (subtype? t1 t2)
   (cond
      ((eq? t1 t2)
       #t)
      ((or (eq? t2 *_*) (eq? t1 *_*))
       (not *check-full*))
      ((or (not *check-correctness*) *unsafe-type*)
       #t)
      ((or (eq? t1 *magic*) (eq? t2 *magic*))
       #t)
      ((eq? t2 *pair-nil*)
       ;; pair-nil subtyping, e.g., pair-nil < pair
       (or (eq? t1 *pair*) (eq? t1 *epair*) (eq? t1 *bnil*)))
      ((and (eq? t2 *pair*) (eq? *epair* t1))
       #t)
      ((and (tclass? t1) (tclass? t2))
       ;; bigloo class subtyping, e.g., point3d < point
       (type-subclass? t1 t2))
      ((eq? t2 *obj*)
       ;; bigloo subtyping, e.g., bstring < obj
       #t)
      ((and (eq? t2 *foreign*) (coercer-exists? t1 t2))
       ;; foreign type subtyping, XXX < foreign
       #t)
      ((and (eq? t1 *foreign*) (coercer-exists? t1 t2))
       ;; foreign type subtyping, XXX < foreign
       #t)
      ((and (eq? (type-id t1) 'tvector) (tvec? t2))
       #t)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    atom-subtype? ...                                                */
;*---------------------------------------------------------------------*/
(define (atom-subtype? t1 t2)
   (cond
      ((eq? t1 *_*)
       #t)
      ((eq? t1 t2)
       #t)
      ((or (eq? t1 *long*) (eq? t1 *int*))
       (or (eq? t2 *long*) (eq? t2 *int*)))
      (else
       #f)))
