;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 10:23:30 2011                          */
;*    Last change :  Sat Nov  5 07:13:05 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    dot notation for object access                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module ast_object

   (include "Tools/trace.sch"
	    "Tools/location.sch")
   
   (import  tools_error
	    tools_shape
	    tools_progn
	    tools_location
	    tools_misc
	    tools_dsssl
	    type_type
	    type_env
	    type_cache
	    type_typeof
	    object_class
	    object_slots
	    object_tools
	    engine_param
	    backend_backend
	    ast_ident
	    ast_env
	    ast_var
	    ast_node
	    ast_build
	    ast_pragma
	    ast_labels
	    ast_let
	    ast_exit
	    ast_app
	    ast_apply
	    ast_private
	    effect_feffect
	    ast_sexp)
   
   (export (field-access?::bool ::symbol ::obj)
	   (field-ref->node::node ::symbol stack ::obj ::symbol)
	   (field-set->node::node ::symbol ::obj stack ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    bigloo-mark ...                                                  */
;*---------------------------------------------------------------------*/
(define bigloo-mark '__bigloo__)

;*---------------------------------------------------------------------*/
;*    field-access? ...                                                */
;*---------------------------------------------------------------------*/
(define (field-access? s stack)
   (let* ((s (symbol->string! s))
	  (i (string-index s #\.)))
      (when i
	 (let ((n (string->symbol (substring s 0 i))))
	    (cond
	       ((eq? n bigloo-mark) #t)
	       ((eq? (identifier-syntax) 'bigloo) #t)
	       ((eq? (identifier-syntax) 'r5rs) #f)
	       (else (or (find-local n stack) (global? (find-global n)))))))))

;*---------------------------------------------------------------------*/
;*    field-ref->node ...                                              */
;*---------------------------------------------------------------------*/
(define (field-ref->node exp stack loc site)
   (let* ((l (map! string->symbol (string-split (symbol->string! exp) ".")))
	  (l (if (eq? (car l) bigloo-mark) (cdr l) l))
	  (var (sexp->node (car l) stack loc site)))
      (with-access::variable (var-variable var) (type)
	 (let loop ((node var)
		    (klass type)
		    (slots (cdr l)))
	    (cond
	       ((null? slots)
		node)
	       ((not (or (tclass? klass) (jclass? klass) (wclass? klass)))
		(error-sexp->node "Static type not a class" exp loc))
	       (else
		(let ((slot (find-class-slot klass (car slots))))
		   (if (not slot)
		       (error-sexp->node
			  (format "Class \"~a\" has not field \"~a\""
			     (type-id klass) (car slots))
			  exp loc)
		       (let ((node (make-field-ref klass slot node
				      stack loc site)))
			  (loop node (slot-type slot) (cdr slots)))))))))))

;*---------------------------------------------------------------------*/
;*    field-set->node ...                                              */
;*---------------------------------------------------------------------*/
(define (field-set->node exp val stack loc site)
   (let* ((l (map! string->symbol (string-split (symbol->string! exp) ".")))
	  (l (if (eq? (car l) bigloo-mark) (cdr l) l))
	  (var (sexp->node (car l) stack loc site))
	  (val (sexp->node val stack loc site)))
      (if (not (var? var))
	  (error-sexp->node "Unbound variable" exp loc)
	  (with-access::variable (var-variable var) (type)
	     (let loop ((node var)
			(klass type)
			(slots (cdr l)))
		(if (not (or (tclass? klass) (jclass? klass) (wclass? klass)))
		    (error-sexp->node "Static type not a class" exp loc)
		    (let ((slot (find-class-slot klass (car slots))))
		       (cond
			  ((not slot)
			   (error-sexp->node
			      (format "Class \"~a\" has not field \"~a\""
				 (type-id klass) (car slots))
			      exp loc))
			  ((null? (cdr slots))
			   (if (slot-read-only? slot)
			       (error-sexp->node
				  (format "Field read-only \"~a\"" (car slots))
				  exp loc)
			       (make-field-set! klass slot node val
				  stack loc site)))
			  (else
			   (let ((node (make-field-ref klass slot node
					  stack loc site)))
			      (loop node (slot-type slot) (cdr slots))))))))))))

;*---------------------------------------------------------------------*/
;*    make-field-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (make-field-ref type slot obj stack loc site)
   (let ((priv (make-class-ref type slot obj)))
      (private-node priv stack loc site)))

;*---------------------------------------------------------------------*/
;*    make-field-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (make-field-set! type slot obj val stack loc site)
   (let ((priv (make-class-set! type slot obj val)))
      (private-node priv stack loc site)))

