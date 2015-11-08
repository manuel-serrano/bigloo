;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Ast/object.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov  3 10:23:30 2011                          */
;*    Last change :  Sat Nov  7 10:04:40 2015 (serrano)                */
;*    Copyright   :  2011-15 Manuel Serrano                            */
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
   
   (export (field-access::pair ::symbol ::symbol #!optional write-allow)
	   (field-ref->node::node ::obj ::pair stack ::obj ::symbol)
	   (field-set->node::node ::obj ::obj ::pair stack ::obj ::symbol)))

;*---------------------------------------------------------------------*/
;*    __bigloo__ ...                                                   */
;*---------------------------------------------------------------------*/
(define __bigloo__
   (string->symbol "#!bigloo"))
(define __bigloo_wallow__
   (string->symbol "#!bigloo_wallow"))

;*---------------------------------------------------------------------*/
;*    field-access ...                                                 */
;*---------------------------------------------------------------------*/
(define (field-access var field #!optional write-allow)
   `(-> ,(if write-allow __bigloo_wallow__ __bigloo__) ,var ,field))

;*---------------------------------------------------------------------*/
;*    source->field ...                                                */
;*---------------------------------------------------------------------*/
(define (source->field l)
   (if (or (eq? (car l) __bigloo__) (eq? (car l) __bigloo_wallow__))
       (cdr l)
       l))

;*---------------------------------------------------------------------*/
;*    field-ref->node ...                                              */
;*---------------------------------------------------------------------*/
(define (field-ref->node l exp stack loc site)
   (let* ((l2 (source->field l))
	  (var (sexp->node (car l2) stack loc site)))
      (cond
	 ((var? var)
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
			      (loop node (slot-type slot) (cdr slots))))))))))
	 ((= *nb-error-on-pass* 0)
	  (error-sexp->node "Unbound variable" exp loc))
	 (else
	  var))))

;*---------------------------------------------------------------------*/
;*    field-set->node ...                                              */
;*---------------------------------------------------------------------*/
(define (field-set->node l val exp stack loc site)
   (let* ((l2 (source->field l))
	  (var (sexp->node (car l2) stack loc site))
	  (val (sexp->node val stack loc site)))
      (cond
	 ((var? var)
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
			      (format "Class \"~a\" has no field \"~a\""
				 (type-id klass) (car slots))
			      exp loc))
			  ((null? (cdr slots))
			   (if (and (slot-read-only? slot)
				    (not (eq? (car l) __bigloo_wallow__)))
			       (error-sexp->node
				  (format "Field read-only \"~a\"" (cadr l2))
				  `(set! ,(car l2) ,(cadr l2)) loc)
			       (make-field-set! slot node val stack loc site)))
			  (else
			   (let ((node (make-field-ref slot node stack loc site)))
			      (loop node (slot-type slot) (cdr slots))))))))))
	 ((= *nb-error-on-pass* 0)
	  (error-sexp->node "Unbound variable" exp loc))
	 (else
	  var))))

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
