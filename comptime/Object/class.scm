;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/class.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu May 30 16:46:40 1996                          */
;*    Last change :  Mon Dec 11 09:12:40 2017 (serrano)                */
;*    Copyright   :  1996-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The class definition                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_class

   (import  tools_error
	    type_type
	    type_cache
	    type_env
	    type_coercion
	    object_tools
	    object_slots
	    object_coercion
	    module_module
	    engine_param
	    foreign_jtype
	    ast_var
	    ast_ident
	    (find-location tools_location))

   (include "Object/class.sch")

   (export  (wide-class tclass::type
	       ;; the `super' field
	       its-super
	       ;; the slots of the class
	       (slots (default #unspecified))
	       ;; a global variable holding the class info
	       (holder::global read-only)
	       ;; widening
	       (widening (default #f) read-only)
	       ;; the depth of the class in the inheritance tree
	       (depth::long (default 0))
	       ;; final
	       (final?::bool read-only (default #f))
	       ;; constructor
	       (constructor read-only)
	       ;; the number of virtual slots this class implements
	       (virtual-slots-number (default 0))
	       ;; abstract class
	       (abstract?::bool read-only (default #f))
	       ;; the true wide type associated with the wide classes
	       (wide-type (default #f))
	       ;; the list of subclasses
	       (subclasses::pair-nil (default '())))

	    (wide-class jclass::type
	       ;; the `super' field
	       (its-super (default #unspecified))
	       ;; the slots of the class
	       (slots (default #unspecified))
	       ;; package
	       (package::bstring read-only (default "")))

	    (wide-class wclass::type
	       ;; the plain class that uses this wide chunk
	       (its-class (default #unspecified)))
	    
	    (get-class-list::pair-nil)
	    (heap-add-class! ::tclass)
	    (wide-chunk-class-id::symbol ::symbol)
	    (type-class-name::bstring ::type)
	    (declare-class-type!::type ::obj ::global ::obj ::bool ::bool ::obj)
	    (declare-java-class-type!::type ::symbol ::obj ::bstring ::bstring ::pair)
	    (final-class?::bool ::obj)
	    (wide-class?::bool ::obj)
	    (find-class-constructor ::tclass)
	    (find-common-super-class ::tclass ::tclass)
	    (type-subclass?::bool ::type ::type)
	    (class-make::obj ::tclass)
	    (class-fill::obj ::tclass)
	    (class-predicate::symbol ::tclass)
	    (class-allocate::symbol ::tclass)
	    (check-class-declaration?::bool ::tclass ::obj)
	    (set-class-slots! ::tclass ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *class-type-list* ...                                            */
;*---------------------------------------------------------------------*/
(define *class-type-list* '())

;*---------------------------------------------------------------------*/
;*    get-class-list ...                                               */
;*---------------------------------------------------------------------*/
(define (get-class-list)
   *class-type-list*)

;*---------------------------------------------------------------------*/
;*    heap-add-class! ...                                              */
;*    -------------------------------------------------------------    */
;*    This function is to be used when restoring class from a heap     */
;*    file.                                                            */
;*---------------------------------------------------------------------*/
(define (heap-add-class! type::tclass)
   (set! *class-type-list* (cons type *class-type-list*)))

;*---------------------------------------------------------------------*/
;*    wide-chunk-class-id ...                                          */
;*    -------------------------------------------------------------    */
;*    This function construct type name of the wide component          */
;*    of a wide class. The idea is to generate a private name that the */
;*    user cannot specify himself in his programs.                     */
;*---------------------------------------------------------------------*/
(define (wide-chunk-class-id class-id)
   (string->symbol (string-append "#!" (symbol->string class-id))))

;*---------------------------------------------------------------------*/
;*    type-class-name ...                                              */
;*---------------------------------------------------------------------*/
(define (type-class-name class)
   (cond
      ((not (tclass? class))
       (type-name class))
      ((and (tclass-widening class) *saw*)
       (type-name (tclass-wide-type class)))
      (else
       (type-name class))))

;*---------------------------------------------------------------------*/
;*    declare-class-type! ...                                          */
;*    -------------------------------------------------------------    */
;*    declare-class-type! is said to be returning a type and not       */
;*    a class in order to help the error management.                   */
;*    -------------------------------------------------------------    */
;*    No check is processed in this function about the super class.    */
;*    This check is performed by the function that creates the         */
;*    accessors for the class (make-class-accesses! and make-wide      */
;*    -class-accesses of the module object_access).                    */
;*---------------------------------------------------------------------*/
(define (declare-class-type!::type class-def class-holder widening final? abstract? src)
   (let* ((class-ident (parse-id (car class-def) (find-location src)))
	  (class-id    (car class-ident))
	  (super       (let ((super (cdr class-ident)))
			  (cond
			     ((eq? (type-id super) class-id)
			      #f)
			     ((eq? super *_*)
			      (get-object-type))
			     (else
			      super))))
	  (name        (id->name class-id))
	  (sizeof      (string-append "struct " name "_bgl"))
	  (t-name      (string-append name "_bglt"))
	  (type        (declare-type! class-id t-name 'bigloo)))
      ;; we mark that the holder is a read-only variable
      (global-set-read-only! class-holder)
      (global-evaluable?-set! class-holder #t)
      (global-type-set! class-holder (get-class-type))
      ;; By now we make the assumption that super is a correct class.
      ;; Super will be checked in `make-class-accesses!' (see module
      ;; object_access).
      (widen!::tclass type
	 (its-super   super)
	 (depth       (if (not (tclass? super))
			  0
			  (+fx (tclass-depth super) 1)))
	 (holder      class-holder)
	 (widening    widening)
	 (final?      final?)
	 (abstract?   abstract?)
	 (constructor (cadr class-def)))
      (when (isa? super tclass)
	 (with-access::tclass super (subclasses)
	    (set! subclasses (cons type subclasses))))
      ;; wide classes creates a new type denoting the wide chunk of the
      ;; wide class. In addition, the type name of a wide classes is the
      ;; type name of its super class.
      (if (eq? widening 'widening)
	  (let* ((wtid (wide-chunk-class-id class-id))
		 (wt (widen!::wclass (declare-type! wtid t-name 'bigloo)
			(its-class type))))
	     (wclass-size-set! wt sizeof)
	     (tclass-wide-type-set! type wt)
	     (type-name-set! type (type-name super))
	     (type-size-set! type (type-size super))
	     (gen-coercion-clause! type wtid super #f)
	     (gen-class-coercers! wt super))
	  (type-size-set! type sizeof))
      ;; we add the class for the C type emission
      (set! *class-type-list* (cons type *class-type-list*))
      ;; we are done
      type))

;*---------------------------------------------------------------------*/
;*    declare-java-class-type! ...                                     */
;*    -------------------------------------------------------------    */
;*    declare-class-type! is said to be returning a type and not       */
;*    a class in order to help the error management.                   */
;*---------------------------------------------------------------------*/
(define (declare-java-class-type!::type class-id super jname package src)
   (let ((super (cond
		   ((eq? (type-id super) class-id)
		    #f)
		   ((eq? super *_*)
		    #f)
		   (else
		    super)))
	 (type  (declare-type! class-id jname 'java)))
      ;; By now we make the assumption that super is a correct class.
      ;; Super will be checked in `make-class-accesses!' (see module
      ;; object_access).
      (widen!::jclass type
	 (its-super super)
	 (package package))
      ;; we are done
      type))

;*---------------------------------------------------------------------*/
;*    final-class? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Is a class a final class ?                                       */
;*---------------------------------------------------------------------*/
(define (final-class? class)
   (and (tclass? class) (tclass-final? class)))

;*---------------------------------------------------------------------*/
;*    wide-class? ...                                                  */
;*    -------------------------------------------------------------    */
;*    Is a class a wide-class ?                                        */
;*---------------------------------------------------------------------*/
(define (wide-class? class)
   (and (tclass? class) (tclass-widening class)))

;*---------------------------------------------------------------------*/
;*    type-subclass? ...                                               */
;*---------------------------------------------------------------------*/
(define (type-subclass? subclass class)
   (cond
      ((and (tclass? class) (tclass? subclass))
       (let loop ((subclass subclass))
	  (cond
	     ((eq? subclass class)
	      #t)
	     ((not (tclass? subclass))
	      #f)
	     ((eq? (tclass-its-super subclass) subclass)
	      #f)
	     (else
	      (loop (tclass-its-super subclass))))))
      ((and (jclass? class) (jclass? subclass))
       (let loop ((subclass subclass))
	  (cond
	     ((eq? subclass class)
	      #t)
	     ((not (jclass? subclass))
	      #f)
	     ((eq? (jclass-its-super subclass) subclass)
	      #f)
	     (else
	      (loop (jclass-its-super subclass))))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    find-class-constructor ...                                       */
;*---------------------------------------------------------------------*/
(define (find-class-constructor class::tclass)
   (let loop ((class class))
      (when (tclass? class)
	 (with-access::tclass class (constructor its-super)
	    (cond
	       ((eq? class its-super) #f)
	       (constructor constructor)
	       (else (loop its-super)))))))

;*---------------------------------------------------------------------*/
;*    find-common-super-class ...                                      */
;*---------------------------------------------------------------------*/
(define (find-common-super-class c1::tclass c2::tclass)
   (define (tclass-super* c)
      (with-access::tclass c (its-super)
	 (if (or (not its-super) (eq? its-super c))
	     '()
	     (cons c (tclass-super* its-super)))))
   (cond
      ((type-subclass? c1 c2)
       c2)
      ((type-subclass? c2 c1)
       c1)
      (else
       (let ((l1 (tclass-super* c1))
	     (l2 (tclass-super* c2)))
	  (let loop ((l l1))
	     (cond
		((null? l)
		 #f)
		((memq (car l) l2)
		 (car l))
		(else
		 (loop (cdr l)))))))))
      
;*---------------------------------------------------------------------*/
;*    class-make ...                                                   */
;*    -------------------------------------------------------------    */
;*    The name of the constructor                                      */
;*---------------------------------------------------------------------*/
(define (class-make t::tclass)
   (if (tclass-abstract? t)
       #f
       (symbol-append 'make- (type-id t))))

;*---------------------------------------------------------------------*/
;*    class-fill ...                                                   */
;*    -------------------------------------------------------------    */
;*    The name of the filler                                           */
;*---------------------------------------------------------------------*/
(define (class-fill t::tclass)
   (if (tclass-abstract? t)
       #f
       (symbol-append 'fill- (type-id t) '!)))

;*---------------------------------------------------------------------*/
;*    class-predicate ...                                              */
;*    -------------------------------------------------------------    */
;*    The name of the predicate                                        */
;*---------------------------------------------------------------------*/
(define (class-predicate::symbol t::tclass)
   (symbol-append (type-id t) '?))

;*---------------------------------------------------------------------*/
;*    class-allocate ...                                               */
;*    -------------------------------------------------------------    */
;*    The name of the allocator                                        */
;*---------------------------------------------------------------------*/
(define (class-allocate::symbol t::tclass)
   (symbol-append '%allocate- (type-id t)))

;*---------------------------------------------------------------------*/
;*    type-occurrence-increment! ::tclass ...                          */
;*---------------------------------------------------------------------*/
(define-method (type-occurrence-increment! t::tclass)
   (call-next-method)
   (with-access::tclass t (widening its-super)
      (when widening (type-occurrence-increment! its-super))))

;*---------------------------------------------------------------------*/
;*    check-class-declaration? ...                                     */
;*    -------------------------------------------------------------    */
;*    This function checks if the super class is conform to the        */
;*    class declaration.                                               */
;*    -------------------------------------------------------------    */
;*    Called when registering a class.                                 */
;*    See @ref ../Module/class.scm:register-class@                     */
;*---------------------------------------------------------------------*/
(define (check-class-declaration? class src-def)
   (if (wide-class? class)
       (check-wide-class-declaration? class src-def)
       (check-plain-class-declaration? class src-def)))

;*---------------------------------------------------------------------*/
;*    check-plain-class-declaration? ...                               */
;*    -------------------------------------------------------------    */
;*    This function checks that the super class is conform to the      */
;*    class. That is, the class is not a wide class and the super      */
;*    class is not final.                                              */
;*---------------------------------------------------------------------*/
(define (check-plain-class-declaration? class src-def)
   (let ((super (tclass-its-super class))
	 (class-id (tclass-id class)))
      ;; Now that the class is defined we check the super (is it or
      ;; not a class).
      (cond
	 ((and (type? super) (not (tclass? super)))
	  (user-error (symbol->string (type-id super))
	     (format "super of \"~a\" is not a class" class-id)
	     src-def
	     type)
	  #f)
	 ((wide-class? class)
	  ;; internal error because wide classes must be processed
	  ;; by make-wide-class-accesses
	  (internal-error "check-plain-class-declation?"
	     "Should not be able to see a wide class here"
	     src-def)
	  #f)
	 ((wide-class? super)
	  ;; no one can inherite of a wide class
	  (user-error (symbol->string (type-id super))
	     (format "super of \"~a\" is a wide class" class-id)
	     src-def
	     type)
	  #f)
	 ((final-class? super)
	  ;; only wide class can inherit of final classes
	  (user-error (symbol->string (type-id super))
	     "Only wide classes can inherit of final classes" 
	     src-def
	     type)
	  #f)
	 (else
	  #t))))

;*---------------------------------------------------------------------*/
;*    check-wide-class-declaration? ...                                */
;*    -------------------------------------------------------------    */
;*    This function checks that the super class is conform to the      */
;*    class. That is, the class is a wide class and the super          */
;*    class is final.                                                  */
;*---------------------------------------------------------------------*/
(define (check-wide-class-declaration? class src-def)
   (let* ((super (tclass-its-super class))
	  (class-id (tclass-id class)))
      ;; Now that the class is defined we check the super (is it or
      ;; not a class).
      (cond
	 ((and (type? super) (not (tclass? super)))
	  (user-error (symbol->string (type-id super))
	     (format "super of \"~a\" is not a class" class-id)
	     src-def
	     type))
	 ((not (wide-class? class))
	  ;; internal error because wide classes must be processed
	  ;; by make-class-accesses
	  (internal-error "check-wide-class-declaration?"
	     "Should not be able to see a plain class here"
	     src-def))
	 ((wide-class? super)
	  ;; no one can inherite of a wide class
	  (user-error (symbol->string (type-id super))
	     (format "super of \"~a\" is a wide class" class-id)
	     src-def
	     type))
	 ((not (final-class? super))
	  ;; wide class can only inherit of final classes
	  (user-error (symbol->string (type-id super))
	     (format "super of wide class \"~a\" is not a final class"
		class-id)
	     src-def
	     type))
	 ((final-class? class)
	  ;; a class can't be final and wide
	  (user-error class-id
	     "A class can't be \"wide\" and \"final\""
	     src-def
	     type))
	 (else
	  #t))))

;*---------------------------------------------------------------------*/
;*    set-class-slots! ...                                             */
;*    -------------------------------------------------------------    */
;*    Parse the class slots and store inside the class structure       */
;*    how many slots are virtual.                                      */
;*    -------------------------------------------------------------    */
;*    Called when registering a class.                                 */
;*    See @ref ../Module/class.scm:register-class@                     */
;*---------------------------------------------------------------------*/
(define (set-class-slots! class class-def src-def)
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

       
