;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/plain-access.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Wed Jun 29 16:58:33 2005 (serrano)                */
;*    Copyright   :  1996-2005 Manuel Serrano, see LICENSE file        */
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
(module object_plain-access
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
   (export  (gen-plain-class-accessors! ::pair ::tclass ::obj ::symbol)
	    (import-plain-class-accessors! ::pair ::tclass ::obj ::symbol)
	    (heap-plain-class-accessors! ::tclass)))

;*---------------------------------------------------------------------*/
;*    gen-plain-class-accessors! ...                                   */
;*---------------------------------------------------------------------*/
(define (gen-plain-class-accessors! class-def class src-def import)
   (trace (ast 2) "=====>>> make-plain-class-accessors!: " src-def #\Newline)
   (if (correct-plain-class? class src-def)
       (with-access::tclass class (id)
	  ;; we store inside the class structure some information about
	  ;; its slots
	  (set-plain-class-slots! class-def src-def class)
	  ;; we install the coercion between the new-class and obj
	  ;; and the class and all its super classes.
	  (gen-class-coercions! class)
	  ;; we produces the user access function
	  (multiple-value-bind (fields virtuals)
	     (gen-class-slots-access! class class #f src-def)
	     (let ((creats (gen-plain-class-creators class src-def import))
		   (structs (gen-plain-class<->struct class src-def))
		   (predicate (gen-class-pred! class src-def import)))
		(values `(,@predicate ,@structs ,@creats ,@fields)
			virtuals))))
       (values '() '())))

;*---------------------------------------------------------------------*/
;*    import-plain-class-accessors! ...                                */
;*---------------------------------------------------------------------*/
(define (import-plain-class-accessors! class-def class src-def module)
   (trace (ast 2) "impport-plain-class-accessors!: " src-def #\Newline)
   (if (correct-plain-class? class src-def)
       (with-access::tclass class (id)
	  ;; we store inside the class structure some information about
	  ;; its slots
	  (set-plain-class-slots! class-def src-def class)
	  ;; we install the coercion between the new-class and obj
	  ;; and the class and all its super classes.
	  (gen-class-coercions! class)
	  ;; we produces the user access function
	  (multiple-value-bind (fields virtuals)
	     (gen-class-slots-access! class class #f src-def)
	     (let ((creats (import-plain-class-creators class src-def module))
		   (predicate (import-class-pred! class src-def module)))
		(values `(,@predicate ,@creats ,@fields)
			virtuals))))
       '()))

;*---------------------------------------------------------------------*/
;*    heap-plain-class-accessors! ...                                  */
;*---------------------------------------------------------------------*/
(define (heap-plain-class-accessors! class)
   (let ((src-def '(no def found)))
      (with-access::tclass class (id)
	 ;; we install the coercion between the new-class and obj
	 ;; and the class and all its super classes.
	 ;; we produces the user access functions
	 (gen-class-slots-access! class class #f src-def))))

;*---------------------------------------------------------------------*/
;*    set-plain-class-slots! ...                                       */
;*    -------------------------------------------------------------    */
;*    Parse the class slots and store inside the class structure       */
;*    how many slots are virtual.                                      */
;*---------------------------------------------------------------------*/
(define (set-plain-class-slots! class-def src-def class)
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
      (let* ((cslots (make-class-slots class slots super super-vnum src-def))
	     (local-vnum (get-local-virtual-slots-number class cslots)))
	 ;; we set the number of virtual slots for the current class
	 (tclass-virtual-slots-number-set! class local-vnum)
	 ;; and we store the slots
	 (tclass-slots-set! class cslots))))

;*---------------------------------------------------------------------*/
;*    correct-plain-class? ...                                         */
;*    -------------------------------------------------------------    */
;*    This function checks that the super class is conform to the      */
;*    class. That is, the class is not a wide class and the super      */
;*    class is not final.                                              */
;*---------------------------------------------------------------------*/
(define (correct-plain-class? class src-def)
   (let ((super    (tclass-its-super class))
	 (class-id (tclass-id class)))
      ;; Now that the class is defined we check the super (is it or
      ;; not a class).
      (cond
	 ((and (type? super) (not (tclass? super)))
	  (user-error (type-id super)
		      (string-append "super of `"
				     (symbol->string class-id)
				     "' is not a class")
		      src-def
		      type)
	  #f)
	 ((wide-class? class)
	  ;; internal error because wide classes must be processed
	  ;; by make-wide-class-accesses
	  (internal-error "make-class-accesses!"
			  "Should not be able to see a wide class here"
			  src-def)
	  #f)
	 ((wide-class? super)
	  ;; no one can inherite of a wide class
	  (user-error (type-id super)
                      (string-append "super of `"
                                     (symbol->string class-id)
                                     "' is a wide class")
                      src-def
		      type)
	  #f)
	 ((final-class? super)
	  ;; only wide class can inherit of final classes
	  (user-error (type-id super)
                      "Only wide classes can inherit of final classes" 
                      src-def
		      type)
	  #f)
	 (else
	  #t))))


