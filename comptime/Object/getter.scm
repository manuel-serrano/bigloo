;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/getter.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Thu Nov 18 11:56:54 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
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
	    object_struct
	    object_slots
	    object_tools
	    module_module
	    module_impuse
	    engine_param
	    tools_shape)
   (export  (gen-class-slots-access! ::tclass type widening ::pair)
	    (gen-java-class-slots-access! ::jclass ::pair-nil ::pair)))
   
;*---------------------------------------------------------------------*/
;*    gen-class-slots-access! ...                                      */
;*    -------------------------------------------------------------    */
;*    The list of slots is the full list. That is, inheritated slots   */
;*    are included in the list. The counter VIRTUAL-SLOT-NUM, counts   */
;*    the virtual slots. This counter is used to create the getter     */
;*    and the setter for virtual slots. The getter will be a call      */
;*    to the getter registered in the class of the instance, located   */
;*    at position VIRTUAL-SLOT-NUM.                                    */
;*    -------------------------------------------------------------    */
;*    This function returns three values:                              */
;*      - the list of user fields                                      */
;*      - the virtual getters                                          */
;*      - the virtual setters                                          */
;*    -------------------------------------------------------------    */
;*    The argument VNUM is the initial value for the virtual           */
;*    numbering. VNUM is different of 0 in case of wide virtual slots  */
;*    of if the super class already introduces virtual slots.          */
;*---------------------------------------------------------------------*/
(define (gen-class-slots-access! class type widening src)
   (trace (ast 2) "make-class-slots-access!: " (shape class) #\Newline)
   ;; for wide classes this function is be called twice. First, type will be #f
   ;; (so the slots are in the class objects, as for plain class). Second,
   ;; it will build the accessors for the plain part of the wide-class,
   ;; using the super class (i.e. the type argument)
   (let loop ((slots (tclass-slots (if (tclass? type) type class)))
	      (res '())
	      (virtuals '()))
      (if (null? slots)
	  (values (reverse! res) (reverse! virtuals))
	  (let ((slot (car slots)))
	     (multiple-value-bind (ref vref)
		(slot-ref class type slot widening src)
		(if (slot-read-only? slot)
		    (loop (cdr slots)
			  (append ref res)
			  (if vref
			      (cons (list (slot-virtual-num slot) vref #f)
				    virtuals)
			      virtuals))
		    (multiple-value-bind (set vset)
		       (slot-set! class type slot widening src)
		       (loop (cdr slots)
			     (append ref set res)
			     (if vref
				 (cons (list (slot-virtual-num slot)
					     vref
					     vset)
				       virtuals)
				 virtuals)))))))))

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
;*    slot-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (slot-ref class::tclass type slot widening src-def)
   (let ((class-id (tclass-id class)))
      (cond
	 ((slot-indexed slot)
	  (values
	   (slot-indexed-ref class-id type slot widening src-def)
	   #f))
	 ((slot-virtual? slot)
	  (multiple-value-bind (user-def class-def)
	     (slot-virtual-ref class type slot widening src-def)
	     (if class-def
		 (values user-def class-def)
		 (values user-def #f))))
	 (else
	  (values
	   (slot-direct-ref class-id type slot widening src-def)
	   #f)))))

;*---------------------------------------------------------------------*/
;*    first-virtual-slot? ...                                          */
;*    -------------------------------------------------------------    */
;*    A virtual slot is the first if:                                  */
;*      - the class has no super class                                 */
;*      - the super class a less slot the number of the current slot   */
;*---------------------------------------------------------------------*/
(define (first-virtual-slot? slot class)
   (let ((vnum  (slot-virtual-num slot))
	 (super (tclass-its-super class)))
      (or (not (tclass? super))
	  (<=fx (tclass-virtual-slots-number super) vnum))))

;*---------------------------------------------------------------------*/
;*    slot-virtual-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (slot-virtual-ref class::tclass type slot widening src-def)
   (define (make-slot-ref-body user-getter class-id vnum)
      (if (first-virtual-slot? slot class)
	  ;; this is the first virtual slot, no CALL-NEXT-SLOT
	  user-getter
	  ;; this is not the first slot thus we define a CALL-NEXT-SLOT
	  (let ((gobj (mark-symbol-non-user! (gensym 'obj))))
	     `(lambda (,gobj)
		 (let ((call-next-slot
			(lambda ()
			   (call-next-virtual-getter ,class-id
						     ,gobj
						     ,vnum))))
		    (,user-getter ,gobj))))))
   (let* ((class-id     (tclass-id class))
	  (class-owner  (slot-class-owner slot))
	  (slot-ref-id  (symbol-append class-id '- (slot-id slot)))
	  (slot-ref-tid (make-typed-ident slot-ref-id
					  (type-id (slot-type slot))))
	  (tid          (make-typed-formal (type-id type)))
	  (holder       (tclass-holder type))
	  (obj          (mark-symbol-non-user! (gensym 'obj)))
	  (vnum         (slot-virtual-num slot))
	  (user-getter  (slot-getter slot))
	  (getter       (make-slot-ref-body user-getter class-id vnum)))
      (if (eq? class class-owner)
	  ;; see @ref getter.scm:slot-direct-ref@ for this test
	  (begin
	     (produce-module-clause!
	      `(static (inline ,slot-ref-tid ,tid)))
	     (produce-module-clause!
	      `(pragma (,slot-ref-id side-effect-free no-cfa-top
				     (effect (read (,slot-ref-id))))))
	     (values
	      (list
	       (epairify*
		`(define-inline (,slot-ref-tid ,(symbol-append obj tid))
		    ((@ call-virtual-getter __object) ,obj ,vnum))
		(if (slot? slot)
		    (slot-src slot)
		    slot)
		src-def))
	      (if (eq? class (slot-class-owner slot))
		  getter
		  #f)))
	  (let ((slot-ref-oid (symbol-append (type-id class-owner)
					     '- (slot-id slot))))
	     (add-macro-alias! slot-ref-id slot-ref-oid)
	     (values '() #f)))))

;*---------------------------------------------------------------------*/
;*    slot-direct-ref ...                                              */
;*---------------------------------------------------------------------*/
(define (slot-direct-ref class-id class slot widening src-def)
   (with-access::slot slot (id type src class-owner)
      (let* ((slot-ref-id  (symbol-append class-id '- id))
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
 		      ,(make-pragma-direct-ref/widening class slot
							obj widening))
		  src
		  src-def)))
	     ;; otherwise we define a alias pointing to the real slot
	     (let ((slot-ref-oid (symbol-append (type-id class-owner)
						'- id)))
		(add-macro-alias! slot-ref-id slot-ref-oid)
		'())))))

;*---------------------------------------------------------------------*/
;*    slot-indexed-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (slot-indexed-ref class-id type slot widening src-def)
   (define (indexed-ref-unsafe sid id tid)
      `(define-inline (,tid ,(make-typed-ident 'o class-id) i::long)
	  ,(make-pragma-indexed-ref/widening type slot 'o 'i widening)))
   (define (indexed-ref-safe sid id tid)
      `(define (,tid ,(make-typed-ident 'o class-id) i::long)
	  (if (>=fx i 0)
	      (if (<fx i (,(symbol-append class-id '- sid '-len) o))
		  ,(make-pragma-indexed-ref/widening type slot 'o 'i widening)
		  (error ',id "Index out of bound" i))
	      (error ',id "Index out of bound" i))))
   (with-access::slot slot (id type src)
      (let* ((slot-ref-id (symbol-append class-id '- id '-ref))
	     (slot-ref-tid (make-typed-ident slot-ref-id (type-id type)))
	     (tid (make-typed-formal class-id)))
	 (produce-module-clause!
	  `(pragma (,slot-ref-id side-effect-free no-cfa-top
				 (effect (write (,slot-ref-id))))))
	 (cond
	    ((not *unsafe-range*)
	     (produce-module-clause!
	      `(static (,slot-ref-tid ,tid ::long)))
	     (list
	      (epairify* (indexed-ref-safe id slot-ref-id slot-ref-tid)
			 src
			 src-def)))
	    (else
	     (produce-module-clause!
	      `(static (inline ,slot-ref-tid ,tid ::long)))
	     (list
	      (epairify* (indexed-ref-unsafe id slot-ref-id slot-ref-tid)
			 src
			 src-def)))))))
      
;*---------------------------------------------------------------------*/
;*    slot-set! ...                                                    */
;*---------------------------------------------------------------------*/
(define (slot-set! class type slot widening src-def)
   (let ((class-id (tclass-id class)))
      (cond
	 ((slot-indexed slot)
	  (values
	   (slot-indexed-set! class-id type slot widening src-def)
	   #f))
	 ((slot-virtual? slot)
	  (multiple-value-bind (user-def class-def)
	     (slot-virtual-set! class type slot widening src-def)
	     (values user-def class-def)))
	 (else
	  (values
	   (slot-direct-set! class-id type slot widening src-def)
	   #f)))))

;*---------------------------------------------------------------------*/
;*    slot-virtual-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (slot-virtual-set! class::tclass type slot widening src)
   (define (make-slot-set-body user-setter class-id vnum)
      (if (first-virtual-slot? slot class)
	  ;; this is the first virtual slot, no CALL-NEXT-SLOT
	  user-setter
	  ;; this is not the first slot thus we define a CALL-NEXT-SLOT
	  (let ((sobj (mark-symbol-non-user! (gensym 'sobj)))
		(sval (mark-symbol-non-user! (gensym 'sval))))
	     `(lambda (,sobj ,sval)
		 (let ((call-next-slot
			(lambda ()
			   (call-next-virtual-setter ,class-id
						     ,sobj
						     ,vnum
						     ,sval))))
		    (,user-setter ,sobj ,sval))))))
   (let* ((class-id      (tclass-id class))
	  (class-owner   (slot-class-owner slot))
	  (slot-ref-id   (symbol-append class-id '- (slot-id slot)))
	  (slot-set!-id  (symbol-append slot-ref-id '-set!))
	  (slot-set!-tid (symbol-append slot-set!-id '::obj))
	  (tid           (make-typed-formal (type-id type)))
	  (holder        (tclass-holder type))
	  (v-id          (mark-symbol-non-user! (gensym 'val)))
	  (obj           (mark-symbol-non-user! (gensym 'obj)))
	  (v-tid         (make-typed-ident v-id (type-id (slot-type slot))))
	  (vnum          (slot-virtual-num slot))
	  (user-setter   (slot-setter slot))
	  (setter        (make-slot-set-body user-setter class-id vnum)))
      (if (eq? class class-owner)
	  ;; see @ref getter.scm:slot-direct-ref@ for this test
	  (begin
	     (produce-module-clause!
	      `(static (inline ,slot-set!-tid ,tid ,v-tid)))
	     (produce-module-clause!
	      `(pragma (,slot-set!-id (effect (write (,slot-ref-id))))))
	     (values
	      (list
	       (epairify*
		`(define-inline (,slot-set!-tid ,(symbol-append obj tid)
						,v-tid)
		    ((@ call-virtual-setter __object) ,obj ,vnum ,v-id))
		(if (slot? slot)
		    (slot-src slot)
		    slot)
		src))
	      (if (eq? class (slot-class-owner slot))
		  setter
		  #f)))
	  (let ((slot-set!-oid (symbol-append (type-id class-owner)
					      '- (slot-id slot)
					      '-set!)))
	     (add-macro-alias! slot-set!-id slot-set!-oid)
	     '()))))

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
			     ,(make-pragma-direct-set!/widening class
								slot
								obj
								v-id
								widening))
			 (if (slot? slot)
			     (slot-src slot)
			     slot)
			 src-def)))
	  (let ((slot-set!-oid (symbol-append (type-id class-owner)
					      '- (slot-id slot)
					      '-set!)))
	     (add-macro-alias! slot-set!-id slot-set!-oid)
	     '()))))

;*---------------------------------------------------------------------*/
;*    slot-indexed-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (slot-indexed-set! class-id class slot widening src-def)
   (define (indexed-set!-unsafe class-id sid! stid! vid vtid)
      `(define-inline (,stid! ,(make-typed-ident 'o class-id) i::long ,vtid)
	  ,(make-pragma-indexed-set!/widening class slot 'o vid 'i widening)))
   (define (indexed-set!-safe id class-id sid! stid! vid vtid)
      `(define (,stid! ,(make-typed-ident 'o class-id) i::long ,vtid)
	  (if (>=fx i 0)
	      (if (<fx i (,(symbol-append class-id '- id '-len) o))
		  ,(make-pragma-indexed-set!/widening class slot 'o vid 'i widening)
		  (error ',sid! "Index out of bound" i))
	      (error ',sid! "Index out of bound" i))))
   (with-access::slot slot (id type src)
      (let* ((slot-set!-id (symbol-append class-id '- id '-set!))
	     (slot-set!-tid (symbol-append slot-set!-id '::obj))
	     (class-id (type-id class))
	     (tid (make-typed-formal class-id))
	     (vid (gensym 'val))
	     (vtid (make-typed-ident vid (type-id type))))
	 (produce-module-clause!
	  `(pragma (,slot-set!-id (effect (write (,slot-set!-id))))))
	 (cond
	    ((not *unsafe-range*)
	     (produce-module-clause!
	      `(static (,slot-set!-tid ,tid ::long ,vtid)))
	     (list
	      (epairify*
	       (indexed-set!-safe id class-id slot-set!-id slot-set!-tid vid vtid)
	       src
	       src-def)))
	    (else
	     (produce-module-clause!
	      `(static (inline ,slot-set!-tid ,tid ::long ,vtid)))
	     (list
	      (epairify*
	       (indexed-set!-unsafe class-id slot-set!-id slot-set!-tid vid vtid)
	       src
	       src-def)))))))
      


