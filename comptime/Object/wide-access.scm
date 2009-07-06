;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/wide-access.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Thu Nov  6 04:16:29 2008 (serrano)                */
;*    Copyright   :  1996-2008 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the class accessors                                      */
;*    -------------------------------------------------------------    */
;*    In this module we cannot use consume-module-clause! because      */
;*    the importation are already done.                                */
;*    -------------------------------------------------------------    */
;*    This constructors does not require any importation information   */
;*    since all accessors are always static.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_wide-access
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    tools_shape
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    ast_private
	    object_class
	    object_struct
	    object_slots
	    object_tools
	    object_getter
	    object_creator
	    object_predicate
	    object_coercion
	    module_module
	    module_impuse
	    engine_param)
   (export  (gen-wide-class-accessors! ::pair ::tclass ::obj ::symbol)
	    (import-wide-class-accessors! ::pair ::tclass ::obj ::symbol)
	    (heap-wide-class-accessors! ::tclass)))

;*---------------------------------------------------------------------*/
;*    gen-wide-class-accessors! ...                                    */
;*---------------------------------------------------------------------*/
(define (gen-wide-class-accessors! class-def class src-def import)
   (trace (ast 2) "make-wide-class-accessors!: " src-def #\Newline)
   (if (correct-wide-class? class src-def)
       (with-access::tclass class (id its-super)
	  ;; we store inside the class structure some information about
	  ;; its slots
	  (set-wide-class-slots! class-def src-def class)
	  ;; we install the coercion between the new-class and obj
	  ;; and the class and all its super classes.
	  (gen-class-coercions! class)
	  ;; we produces the user access function
	  (let ((creats (gen-wide-class-creators class src-def import))
		(structs (gen-wide-class<->struct class src-def))
		(predicate (gen-class-pred! class src-def import)))
	     (multiple-value-bind (fields virtuals)
		(gen-class-slots-access! class class class src-def)
		(multiple-value-bind (super-fields super-virtuals)
		   (gen-class-slots-access! class its-super #f src-def)
		   (values `(,@predicate ,@structs ,@creats
					 ,@super-fields ,@fields)
			   (append super-virtuals virtuals))))))
       (values '() '())))

;*---------------------------------------------------------------------*/
;*    import-wide-class-accessors! ...                                 */
;*---------------------------------------------------------------------*/
(define (import-wide-class-accessors! class-def class src-def module)
   (trace (ast 2) "impport-wide-class-accessors!: " src-def #\Newline)
   (if (correct-wide-class? class src-def)
       (with-access::tclass class (id its-super)
	  ;; we store inside the class structure some information about
	  ;; its slots
	  (set-wide-class-slots! class-def src-def class)
	  ;; we install the coercion between the new-class and obj
	  ;; and the class and all its super classes.
	  (gen-class-coercions! class)
	  ;; we produces the user access function
	  (let ((creats (import-wide-class-creators class src-def module))
		(predicate (import-class-pred! class src-def module)))
	     (multiple-value-bind (fields virtuals)
		(gen-class-slots-access! class class class src-def)
		(multiple-value-bind (super-fields super-virtuals)
		   (gen-class-slots-access! class its-super #f src-def)
		   (values `(,@predicate ,@creats ,@super-fields ,@fields)
			   (append super-virtuals virtuals))))))
       (values '() '())))

;*---------------------------------------------------------------------*/
;*    heap-wide-class-accessors! ...                                   */
;*---------------------------------------------------------------------*/
(define (heap-wide-class-accessors! class)
   (let ((src-def '(no def found)))
      (with-access::tclass class (id its-super)
	 ;; we install the coercion between the new-class and obj
	 ;; and the class and all its super classes.
	 ;; we produces the user access functions
	 (multiple-value-bind (fields virtuals)
	    (gen-class-slots-access! class class class src-def)
	    (multiple-value-bind (super-fields super-virtuals)
	       (gen-class-slots-access! class its-super #f src-def)
	       (values (append super-fields fields)
		       (append super-virtuals virtuals)))))))
   
;*---------------------------------------------------------------------*/
;*    set-wide-class-slots! ...                                        */
;*    -------------------------------------------------------------    */
;*    Parse the class slots and store inside the class structure       */
;*    how many slots are virtual.                                      */
;*---------------------------------------------------------------------*/
(define (set-wide-class-slots! class-def src-def class)
   (let* ((super (let ((super (tclass-its-super class)))
		    (if (eq? super class)
			#f
			super)))
	  (super-vnum (if (tclass? super)
			  (tclass-virtual-slots-number super)
			  0))
	  (slots (cddr class-def)))
      ;; we store inside the class the result of the parsing
      ;; we pre-parse the class slot and we return the data structure
      ;; describing the result of this parsing
      (let* ((cslots (make-class-slots class slots #f super-vnum src-def))
	     (local-vnum (get-local-virtual-slots-number class cslots)))
	 ;; we set the number of virtual slots for the current class
	 (tclass-virtual-slots-number-set! class local-vnum)
	 ;; and we store the slots
	 (tclass-slots-set! class cslots))))

;*---------------------------------------------------------------------*/
;*    correct-wide-class? ...                                          */
;*    -------------------------------------------------------------    */
;*    This function checks that the super class is conform to the      */
;*    class. That is, the class is a wide class and the super          */
;*    class is final.                                                  */
;*---------------------------------------------------------------------*/
(define (correct-wide-class? class src-def)
   (let* ((super      (tclass-its-super class))
	  (class-id   (tclass-id class)))
	  ;; Now that the class is defined we check the super (is it or
	  ;; not a class).
      (cond
	 ((and (type? super) (not (tclass? super)))
	  (user-error (type-id super)
		      (string-append "super of `"
				     (symbol->string class-id)
				     "' is not a class")
		      src-def
		      type))
	 ((not (wide-class? class))
	      ;; internal error because wide classes must be processed
	      ;; by make-class-accesses
	  (internal-error "correct-wide-class?"
			  "Should not be able to see a plain class here"
			  src-def))
	 ((wide-class? super)
	      ;; no one can inherite of a wide class
	  (user-error (type-id super)
		      (string-append "super of `"
				     (symbol->string class-id)
				     "' is a wide class")
		      src-def
		      type))
	 ((not (final-class? super))
	      ;; wide class can only inherit of final classes
	  (user-error (type-id super)
		      (string-append "super of wide class `"
				     (symbol->string class-id)
				     "' is not a final class")
		      src-def
		      type))
	 ((final-class? class)
	      ;; a class can't be final and wide
	  (user-error class-id
		      "A class can't be `wide' and `final'"
		      src-def
		      type))
	 (else
	  #t))))
