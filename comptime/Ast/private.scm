;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/private.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 13 14:11:36 2000                          */
;*    Last change :  Sat May  6 08:50:12 2017 (serrano)                */
;*    Copyright   :  2000-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Private constructino of the AST.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_private
   (import engine_param
	   ast_node
	   type_type
	   type_cache
	   type_env
	   tools_shape
	   ast_sexp
	   ast_var)
   (export (private-node ::pair ::obj ::obj ::symbol)
	   (private-stamp::symbol)
	   (private-sexp?::bool ::pair)
	   (cast-sexp?::bool ::pair)
	   (cast-sexp-type::symbol ::pair)
	   (make-private-sexp::pair ::symbol ::symbol . objs)
	   (expand-meta ::pair ::procedure)))

;*---------------------------------------------------------------------*/
;*    *private-stamp* ...                                              */
;*    -------------------------------------------------------------    */
;*    This is a constant. It cannot be changed without bootstrapping   */
;*    because the heap refers to it.                                   */
;*---------------------------------------------------------------------*/
(define *private-stamp* '___bgl_private_stamp_mark)

;*---------------------------------------------------------------------*/
;*    private-stamp ...                                                */
;*---------------------------------------------------------------------*/
(define (private-stamp)
   *private-stamp*)

;*---------------------------------------------------------------------*/
;*    private-sexp? ...                                                */
;*---------------------------------------------------------------------*/
(define (private-sexp?::bool sexp)
   (eq? (car sexp) *private-stamp*))

;*---------------------------------------------------------------------*/
;*    cast-sexp? ...                                                   */
;*---------------------------------------------------------------------*/
(define (cast-sexp?::bool sexp)
   (when (private-sexp? sexp)
      (match-case sexp
	 ((?- cast ?- ?-) #t)
	 (else #f))))

;*---------------------------------------------------------------------*/
;*    cast-sexp-type ...                                               */
;*---------------------------------------------------------------------*/
(define (cast-sexp-type sexp)
   (match-case sexp
      ((?- cast ?type ?-) type)
      (else
       (error "cast-sexp-type" "Illegal cast sexp" sexp))))

;*---------------------------------------------------------------------*/
;*    private-node ...                                                 */
;*---------------------------------------------------------------------*/
(define (private-node sexp::pair stack loc site)
   (define (bigloodemangle f)
      (if (bigloo-mangled? f)
	  (bigloo-demangle f)
	  f ))
   (match-case sexp
      ((?- getfield ?ftype ?otype ?field-name ?c-fmt ?obj)
       (let ((tid (symbol-append otype '-
				 (string->symbol
				  (bigloodemangle field-name))))
	     (ftype (use-type! ftype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::getfield
	     (loc loc)
	     (type ftype)
	     (otype otype)
	     (fname field-name)
	     (ftype ftype)
	     (side-effect #f)
	     (expr* (list (sexp->node obj stack loc 'value)))
	     (effect (instantiate::feffect
			(read (list tid))))
	     (c-format c-fmt))))
      ((?- setfield ?ftype ?otype ?field-name ?c-fmt . ?rest)
       (let ((tid (symbol-append otype '-
				 (string->symbol
				  (bigloodemangle field-name))))
	     (otype (use-type! otype loc))
	     (ftype (use-type! ftype loc)))
	  (instantiate::setfield
	     (loc loc)
	     (type *obj*)
	     (otype otype)
	     (fname field-name)
	     (ftype ftype)
	     (side-effect #t)
	     (expr* (sexp*->node rest stack loc 'value))
	     (effect (instantiate::feffect
			(write (list tid))))
	     (c-format c-fmt))))
      ((?- new ?type)
       (instantiate::new
	  (loc loc)
	  (type (use-type! type loc))
	  (side-effect #t)
	  (c-format "")))
      ((?- new ?type (quote ?args-type) . ?rest)
       (if (null? rest)
	   ;; not an external class
	   (instantiate::new
	      (loc loc)
	      (type (use-type! type loc))
	      (args-type (map (lambda (t) (use-type! t loc)) args-type))
	      (side-effect #t)
	      (c-format ""))
	   (instantiate::new
	      (loc loc)
	      (type (use-type! type loc))
	      (args-type (map (lambda (t) (use-type! t loc)) args-type))
	      (expr* (if (null? rest)
			 '()
			 (sexp*->node rest stack loc 'value)))
	      (side-effect #t)
	      (c-format ""))))
      ((?- cast ?type ?exp)
       (instantiate::cast
	  (loc loc)
	  (type (use-type! type loc))
	  (arg (sexp->node exp stack loc site))))
      ((?- cast-null ?type)
       (instantiate::cast-null
	  (loc loc)
	  (type (use-type! type loc))
	  (c-format "")))
      ((?- (or instanceof isa) ?type ?exp)
       ;; isa to be removed
       (instantiate::instanceof
	  (loc loc)
	  (type *bool*)
	  (class (use-type! type loc))
	  (expr* (list (sexp->node exp stack loc site)))
	  (effect (instantiate::feffect))
	  (c-format "")))
      ((?- vlength ?vtype ?ftype ?otype (and (? string?) ?c-fmt) ?exp)
       (let ((vtype (use-type! vtype loc))
	     (otype (use-type! otype loc))
	     (ftype (use-type! ftype loc)))
	  (instantiate::vlength
	     (loc loc)
	     (type otype)
	     (vtype vtype)
	     (ftype ftype)
	     (c-format c-fmt)
	     (expr* (list (sexp->node exp stack loc 'value)))
	     (effect (instantiate::feffect)))))
      ((?- (or vref vref-ur) ?vtype ?ftype ?otype (and (? string?) ?c-fmt) . ?rest)
       (let ((ftype (use-type! ftype loc))
	     (vtype (use-type! vtype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::vref
	     (loc loc)
	     (type ftype)
	     (ftype ftype)
	     (otype otype)
	     (vtype vtype)
	     (c-format c-fmt)
	     (expr* (sexp*->node rest stack loc 'value))
	     (unsafe (eq? (cadr sexp) 'vref-ur))
	     (effect (instantiate::feffect
			(read (list (type-id ftype))))))))
      ((?- (or vset! vset-ur!) ?vtype ?ftype ?otype (and (? string?) ?c-fmt) . ?rest)
       (let ((ftype (use-type! ftype loc))
	     (vtype (use-type! vtype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::vset!
	     (loc loc)
	     (type *unspec*)
	     (ftype ftype)
	     (otype otype)
	     (vtype vtype)
	     (c-format c-fmt)
	     (expr* (sexp*->node rest stack loc 'value))
	     (unsafe (eq? (cadr sexp) 'vset-ur!))
	     (effect (instantiate::feffect
			(write (list (type-id ftype))))))))
      ((?- valloc ?vtype ?ftype ?otype
	   (and (? string?) ?c-heap-fmt)
	   (and (? string?) ?c-stack-fmt)
	   (and (? boolean?) ?stack?) . ?rest)
       (let ((ftype (use-type! ftype loc))
	     (vtype (use-type! vtype loc))
	     (otype (use-type! otype loc)))
	  (instantiate::valloc
	     (loc loc)
	     (type vtype)
	     (ftype ftype)
	     (otype otype)
	     (c-format c-heap-fmt)
	     (expr* (sexp*->node rest stack loc 'value)))))
      ((?- unsafe ?type ?exp)
       (instantiate::sequence
	  (loc loc)
	  (type (use-type! type loc))
	  (unsafe #t)
	  (nodes (list (sexp->node exp stack loc 'value)))))
      ((?- meta ?type ?kwds ?body)
       (instantiate::sequence
	  (loc loc)
	  (type (use-type! type loc))
	  (meta kwds)
	  (nodes (sexp*->node body stack loc 'value))))
      (else
       (error "private-node"
	      "Illegal private kind"
	      (if (pair? (cdr sexp)) (cadr sexp) sexp)))))

;*---------------------------------------------------------------------*/
;*    make-private-sexp ...                                            */
;*    -------------------------------------------------------------    */
;*    Build an private sexp. That is a sexp that can be processed by   */
;*    SEXP->NODE but that is not accessible from the user source code. */
;*    -------------------------------------------------------------    */
;*    the private stamp can't be gensymed because it has to traverse   */
;*    heap files.                                                      */
;*    -------------------------------------------------------------    */
;*    Private sexps block the macro-expansion (i.e., their arguments   */
;*    are never macro-expanded).                                       */
;*---------------------------------------------------------------------*/
(define (make-private-sexp::pair kind::symbol type-id::symbol . objs)
   (assert (kind) (memq kind '(getfield setfield new cast cast-null
			       ;; isa to be removed
			       instanceof isa
			       vlength vref vset! valloc unsafe
			       vref-ur vset-ur! meta)))
   (cons* *private-stamp* kind type-id objs))

;*---------------------------------------------------------------------*/
;*    expand-meta ...                                                  */
;*---------------------------------------------------------------------*/
(define (expand-meta x e)
   (match-case x
      ((?- (and (? list?) ?keywords) . ?body)
       (make-private-sexp 'meta '_ keywords (map (lambda (x) (e x e)) body)))
      (else
       (error "meta" "bad syntax" x))))
       
