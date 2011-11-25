;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/getter.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Fri Nov 25 06:43:12 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Generation of class accessors                                    */
;*    -------------------------------------------------------------    */
;*    In this module we cannot use consume-module-clause! because      */
;*    the importation are already done.                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_getter
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    expand_eps
	    object_class
	    object_slots
	    object_tools
	    module_module
	    module_impuse
	    engine_param
	    tools_shape)
   (export  (gen-java-class-slots-access! ::jclass ::pair-nil ::pair)))
   
;*---------------------------------------------------------------------*/
;*    gen-java-class-slots-access! ...                                 */
;*---------------------------------------------------------------------*/
(define (gen-java-class-slots-access! jclass slots src-def)
   (with-access::jclass jclass (id)
      (define (gen-java-class-slot-access slot)
	 (let ((ref (slot-direct-ref id jclass slot #f src-def)))
	    (if (slot-read-only? slot)
		ref
		(append ref (slot-direct-set! id jclass slot #f src-def)))))
      (apply append (map gen-java-class-slot-access slots))))

;*---------------------------------------------------------------------*/
;*    slot-direct-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (slot-direct-ref class-id class slot widening src-def)
   (with-access::slot slot (id type src class-owner)
      (let* ((slot-ref-id  (symbol-append class-id '- id))
	     (slot-ref-id slot-ref-id)
	     (slot-ref-tid (make-typed-ident slot-ref-id (type-id type)))
	     (obj (mark-symbol-non-user! (gensym 'obj)))
	     (tid (make-typed-formal (type-id class))))
	 (if (eq? class-id (type-id class-owner))
	     ;; if the slot is defined in this class, we create the
	     ;; function that accesses it
	     ;; @label slot-direct-ref@
	     (begin
		(produce-module-clause!
		   `(static (inline ,slot-ref-tid ,tid)))
		(produce-module-clause!
		   `(pragma (,slot-ref-id side-effect-free no-cfa-top
			       (effect (read (,slot-ref-id))))))
		(list
		   (epairify*
		      `(define-inline (,slot-ref-tid ,(symbol-append obj tid))
			  ,(make-class-ref class slot obj))
		      src
		      src-def)))
	     ;; otherwise we define an alias pointing to the real slot
	     (let ((slot-ref-oid (symbol-append (type-id class-owner) '- id)))
		(add-macro-alias! slot-ref-id slot-ref-oid)
		'())))))

;*---------------------------------------------------------------------*/
;*    slot-direct-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (slot-direct-set! class-id class slot widening src-def)
   (let* ((slot-ref-id   (symbol-append class-id '- (slot-id slot)))
	  (slot-set!-id  (symbol-append slot-ref-id '-set!))
	  (slot-set!-tid (symbol-append slot-set!-id '::obj))
	  (tid           (make-typed-formal (type-id class)))
	  (v-id          (mark-symbol-non-user! (gensym 'val)))
	  (obj           (mark-symbol-non-user! (gensym 'obj)))
	  (v-tid         (make-typed-ident v-id (type-id (slot-type slot))))
	  (class-owner   (slot-class-owner slot)))
      (if (eq? class-id (type-id class-owner))
	  ;; see @ref getter.scm:slot-direct-ref@ for this test
	  (begin
	     (produce-module-clause!
	      `(static (inline ,slot-set!-tid ,tid ,v-tid)))
	     (produce-module-clause!
	      `(pragma (,slot-set!-id (effect (write (,slot-ref-id))))))
	     (list
	      (epairify* `(define-inline (,slot-set!-tid
					  ,(symbol-append obj tid) ,v-tid)
			     ,(make-class-set! class slot obj v-id))
			 (if (slot? slot)
			     (slot-src slot)
			     slot)
			 src-def)))
	  (let ((slot-set!-oid (symbol-append (type-id class-owner)
					      '- (slot-id slot)
					      '-set!)))
	     (add-macro-alias! slot-set!-id slot-set!-oid)
	     '()))))      


