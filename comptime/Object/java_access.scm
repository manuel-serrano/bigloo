;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/java_access.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Fri Nov 25 06:41:42 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
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
(module object_java-access
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
	    object_slots
	    object_tools
	    object_getter
	    object_creator
	    object_predicate
	    object_coercion
	    module_module
	    module_impuse
	    engine_param)
   (export  (import-java-class-accessors! ::pair-nil ::pair-nil
					  ::jclass ::bool
					  ::obj ::pair)))

;*---------------------------------------------------------------------*/
;*    import-java-class-accessors! ...                                 */
;*---------------------------------------------------------------------*/
(define (import-java-class-accessors! pslots constrs class abstract? module src)
   (trace (ast 2) "impport-java-class-accessors!: " src #\Newline)
   (if (correct-java-class? class src)
       (with-access::jclass class (id slots)
	  ;; we store inside the class structure some information about
	  ;; its slots
	  (set-java-class-slots! pslots src class)
	  ;; we install the coercion between the new-class and obj
	  ;; and the class and all its super classes.
	  (gen-java-class-coercions! class)
	  ;; we produces the user access function
	  (let ((fields (gen-java-class-slots-access! class slots src)))
	     (if (eq? id 'foreign)
		 fields
		 (let ((pred (import-java-class-pred! class src module)))
		    (if abstract?
			(append pred fields)
			(let* ((const (gen-java-class-constructors class
					 constrs src))
			       (creator (import-java-class-creator class
					   constrs src)))
			   `(,@pred ,@creator ,@fields ,@const)))))))
       '()))

;*---------------------------------------------------------------------*/
;*    correct-java-class? ...                                          */
;*    -------------------------------------------------------------    */
;*    This function checks that the super class is conform to the      */
;*    class. That is, the class is not a wide class and the super      */
;*    class is not final.                                              */
;*---------------------------------------------------------------------*/
(define (correct-java-class? class src)
   (let ((super    (jclass-its-super class))
	 (class-id (jclass-id class)))
      ;; Now that the class is defined we check the super (is it or
      ;; not a class).
      (cond
	 ((and (type? super) (not (jclass? super)))
	  (user-error (type-id super)
		      (string-append "super of `"
				     (symbol->string class-id)
				     "' is not a class")
		      src
		      type)
	  #f)
	 (else
	  #t))))

;*---------------------------------------------------------------------*/
;*    set-java-class-slots! ...                                        */
;*    -------------------------------------------------------------    */
;*    Parse the class slots and store inside the class structure       */
;*    how many slots are virtual.                                      */
;*---------------------------------------------------------------------*/
(define (set-java-class-slots! slots src-def class)
   ;; we store inside the class the result of the parsing
   ;; we pre-parse the class slot and we return the data structure
   ;; describing the result of this parsing
   (let ((cslots (make-java-class-slots class
					slots
					(jclass-its-super class)
					src-def)))
      ;; and we store the slots
      (jclass-slots-set! class cslots)))

   
