;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 10:23:30 2011                          */
;*    Last change :  Wed Nov 16 07:25:33 2011 (serrano)                */
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
   
   (export (field-access::pair ::symbol ::symbol)
	   (field-ref->node::node ::obj stack ::obj ::symbol)
	   (field-set->node::node ::obj ::obj stack ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    __bigloo__ ...                                                   */
;*---------------------------------------------------------------------*/
(define __bigloo__
   (string->symbol "#!bigloo"))

;*---------------------------------------------------------------------*/
;*    field-access ...                                                 */
;*---------------------------------------------------------------------*/
(define (field-access var field)
   `(-> ,__bigloo__ ,var ,field))

;*---------------------------------------------------------------------*/
;*    field-ref->node ...                                              */
;*---------------------------------------------------------------------*/
(define (field-ref->node exp stack loc site)
   (let* ((l (if (symbol? exp)
		 (map! string->symbol (string-split (symbol->string! exp) "."))
		 exp))
	  (l2 (if (eq? (car l) __bigloo__) (cdr l) l))
	  (var (sexp->node (car l2) stack loc site)))
      (with-access::variable (var-variable var) (type)
	 (let loop ((node var)
		    (klass type)
		    (slots (cdr l2)))
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
		       (let ((node (make-field-ref slot node stack loc site)))
			  (loop node (slot-type slot) (cdr slots)))))))))))

;*---------------------------------------------------------------------*/
;*    field-set->node ...                                              */
;*---------------------------------------------------------------------*/
(define (field-set->node exp val stack loc site)
   (let* ((l (if (symbol? exp)
		 (map! string->symbol (string-split (symbol->string! exp) "."))
		 exp))
	  (l2 (if (eq? (car l) __bigloo__) (cdr l) l))
	  (var (sexp->node (car l2) stack loc site))
	  (val (sexp->node val stack loc site)))
      (if (not (var? var))
	  (error-sexp->node "Unbound variable" exp loc)
	  (with-access::variable (var-variable var) (type)
	     (let loop ((node var)
			(klass type)
			(slots (cdr l2)))
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
			   (if (and (slot-read-only? slot) (eq? l l2))
			       (error-sexp->node
				  (format "Field read-only \"~a\"" (car slots))
				  exp loc)
			       (make-field-set! slot node val stack loc site)))
			  (else
			   (let ((node (make-field-ref slot node stack loc site)))
			      (loop node (slot-type slot) (cdr slots))))))))))))

;*---------------------------------------------------------------------*/
;*    make-field-ref ...                                               */
;*---------------------------------------------------------------------*/
(define (make-field-ref slot obj stack loc site)
   (if (slot-getter slot)
       (let* ((vnum (slot-virtual-num slot))
	      (exp `((@ call-virtual-getter __object) ,obj ,vnum)))
	  (sexp->node exp stack loc site))
       (let ((priv (make-class-ref (slot-class-owner slot) slot obj)))
	  ;; instead of (SLOT-CLASS-OWNER SLOT), the class used to find
	  ;; the slot (see find-class-lot in FIELD-REF->NODE) was used here
	  (private-node priv stack loc site))))

;*---------------------------------------------------------------------*/
;*    make-field-set! ...                                              */
;*---------------------------------------------------------------------*/
(define (make-field-set! slot obj val stack loc site)
   (cond
      ((slot-setter slot)
       (let* ((vnum (slot-virtual-num slot))
	      (exp `((@ call-virtual-setter __object) ,obj ,vnum ,val)))
	  (sexp->node exp stack loc site)))
      (else
       (let ((priv (make-class-set! (slot-class-owner slot) slot obj val)))
	  ;; see MAKE-FIELD-REF for the remark about (SLOT-CLASS-OWNER SLOT)
	  (private-node priv stack loc site)))))

