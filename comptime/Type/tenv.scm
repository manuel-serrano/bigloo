;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Type/tenv.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 25 11:32:49 1994                          */
;*    Last change :  Wed May  7 08:42:35 2014 (serrano)                */
;*    Copyright   :  1994-2014 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The Type environment manipulation                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module type_env

   (include "Tools/trace.sch"
	    "Type/coercer.sch")
   
   (import  init_main
	    tools_shape
	    tools_error
	    tools_location
	    ast_ident
	    ast_var
	    engine_param
	    module_module
	    module_class
	    type_type
	    type_tools
	    type_cache
	    foreign_ctype
	    object_class
	    object_predicate
	    object_creator
	    object_slots
	    tvector_tvector
	    foreign_access
	    module_foreign
	    module_java
	    module_type
	    tvector_access)

   (static  (bind-type!::type ::symbol ::bool loc)
	    (uninitialized-types))

   (export  (initialize-Tenv!)
	    (set-tenv! <Tenv>)
	    (add-tenv! <Tenv>)
	    (get-tenv)
	    (find-type::type ::symbol)
	    (find-type/location::type ::symbol loc)
	    (use-type!::type ::symbol loc)
	    (use-type/import-loc!::type ::symbol loc loci)
	    (use-foreign-type!::type ::symbol loc)
	    (use-foreign-type/import-loc!::type ::symbol loc loci)
	    (type-exists?::bool ::symbol)
	    (declare-type!::type ::symbol ::bstring ::symbol)
	    (declare-subtype!::type ::symbol ::bstring symbol* ::symbol)
	    (declare-aliastype!::type ::symbol ::bstring ::symbol ::type)
	    (for-each-type! ::procedure)
	    (check-types)
	    (sub-type?::bool ::type ::type)))

;*---------------------------------------------------------------------*/
;*    *Tenv* ...                                                       */
;*    -------------------------------------------------------------    */
;*    The Global environment (for global variable definitions).        */
;*---------------------------------------------------------------------*/
(define *Tenv* 'the-global-environment)

;*---------------------------------------------------------------------*/
;*    initialize-Tenv! ...                                             */
;*---------------------------------------------------------------------*/
(define (initialize-Tenv!)
   ;; the global environment
   (unless (hashtable? *Tenv*)
      (set! *Tenv* (make-hashtable))))

;*---------------------------------------------------------------------*/
;*    get-tenv ...                                                     */
;*---------------------------------------------------------------------*/
(define (get-tenv)
   *Tenv*)

;*---------------------------------------------------------------------*/
;*    set-tenv! ...                                                    */
;*---------------------------------------------------------------------*/
(define (set-tenv! Tenv)
   ;; versions 2.6 and smaller used: (set! *Tenv* Tenv)
   ;; this is nolonger a valid implementation because the standard
   ;; library of bigloo2.7 provides classes with slots (e.g. &exception).
   ;; the add-tenv! function takes care of defining the accessors of
   ;; the classes that are defined in a heap.
   (initialize-Tenv!)
   (add-tenv! Tenv))

;*---------------------------------------------------------------------*/
;*    add-tenv! ...                                                    */
;*---------------------------------------------------------------------*/
(define (add-tenv! Tenv)
   
   (define (adjust-type-coercers! type)
      ;; fix aliased type
      (when (type? (type-alias type))
	 (type-alias-set! type (find-type (type-id (type-alias type)))))
      ;; fix subtypes
      (type-parents-set! type
			 (map (lambda (parent)
				 (find-type (type-id parent)))
			      (type-parents type)))
      ;; fix coercers
      (for-each (lambda (coercer)
		   (let ((from (coercer-from coercer))
			 (to (coercer-to coercer)))
		      (coercer-from-set! coercer (find-type (type-id from)))
		      (coercer-to-set! coercer (find-type (type-id to)))
		      (for-each (lambda (co)
				   (set-cdr! co (find-type (type-id (cdr co)))))
				(coercer-check-op coercer))
		      (for-each (lambda (co)
				   (set-cdr! co (find-type (type-id (cdr co)))))
				(coercer-coerce-op coercer))))
		(type-coerce-to type)))
   
   (define (find-coercer from to)
      (let loop ((coercer (type-coerce-to from)))
	 (cond
	    ((null? coercer) #f)
	    ((eq? (coercer-to (car coercer)) to) (car coercer))
	    (else (loop (cdr coercer))))))
   
   (define (add-type-coercers! old new)
      (for-each (lambda (coercer)
		   (let* ((from (coercer-from coercer))
			  (to (coercer-to coercer))
			  (tid (type-id to))
			  (tid-exists? (type-exists? tid)))
		      (when (or (not tid-exists?)
				(not (find-coercer old (find-type tid))))
			 (type-coerce-to-set! old
					      (cons coercer
						    (type-coerce-to old))))))
		(type-coerce-to new)))

   (define (find-new t)
      (if (isa? t type)
	  (with-access::type t (id)
	     (let ((old (hashtable-get *Tenv* id)))
		(or old t)))
	  t))
   
   ;; in a first stage, we bind all new types
   ;; (not rebinding already binded types)
   (let ((remember-list '())
	 (tvector-list '()))
      ;; the remember list is used to store class type. We have to make
      ;; three traversals over classes. A first one for defining them,
      ;; a second one, when all classes are correctly set up, to
      ;; fix the its-super class fields. Without the fix, new installed
      ;; classes would have a its-super field that points to the old
      ;; hash table (the one restored, not the current compiler's one).
      ;; The last traversal used to construct the class accessors
      ;; The same thing apply to tvectors
      (hashtable-for-each Tenv
	 (lambda (k new)
	    (let* ((id  (type-id new))
		   (old (hashtable-get *Tenv* id)))
	       (cond
		  ((not (type? old))
		   (hashtable-put! *Tenv* id new)
		   (when (tclass? new)
		      (heap-add-class! new)
		      (set! remember-list
			 (cons new remember-list)))
		   (when (jclass? new)
		      (heap-add-jclass! new))
		   (when (tvec? new)
		      (set! tvector-list
			 (cons new tvector-list))))
		  ((not (type-init? old))
		   (error "add-Tenv!"
		      "Illegal type heap redefinition"
		      id)
		   (compiler-exit 55))
		  (else
		   ;; we have to store the new coercers 
		   ;; for the old type
		   (add-type-coercers! old new))))))
      (hashtable-for-each Tenv
	 (lambda (k new)
	    (let* ((id  (type-id new))
		   (old (hashtable-get *Tenv* id)))
	       (when (ctype? old)
		  (let ((l (type-location old)))
		     (foreign-accesses-add!
			(make-ctype-accesses! old old l)))))))
      ;; we have to walk thru the remember list in order to
      ;; setup the correct super class fields
      (for-each (lambda (new)
		   (when (tclass? new)
		      ;; super class
		      (let ((super (tclass-its-super new)))
			 (when (tclass? super)
			    (let* ((super-id (tclass-id super))
				   (old-s (find-type super-id)))
			       (if (not (tclass? old-s))
				   (error 'add-Tenv
				      "Can't find super class of"
				      (tclass-name new))
				   (tclass-its-super-set! new old-s)))))))
	 remember-list)
      ;; the tvector traversal
      (for-each (lambda (new)
		   (delay-tvector! new 'heap #f))
	 tvector-list)
      ;; the last walk to construct the class accessors
      ;; When we load an additional heap that contains classes
      ;; definition, the accessors are build when compiling the module
      ;; that uses the library. These accessors have to be added to
      ;; the other classes accessors.
      (for-each (lambda (n)
		   (if (tclass? n)
		       (delay-class-accessors!
			  n
			  (delay
			     ;; The test cannot be lifter out of the delay
			     ;; scope otherwise it gets evaluated before the
			     ;; modules options are evaluated.
			     '()))))
	 remember-list))
   ;; in a second stage, we have to reset the all coercer for _all_ types.
   ;; This is mandatory because some types have not been rebound and
   ;; then we need to adjust the coercer fields of freshly bound types.
   ;; we have to walk thru all types, not only the freshly defined ones
   ;; because old types may have coercion to new types (for instance, for
   ;; fresh classes).
   (hashtable-for-each *Tenv*
		       (lambda (k new)
			  (adjust-type-coercers! new))))
		 
;*---------------------------------------------------------------------*/
;*    find-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-type::type id::symbol)
   (let ((type (hashtable-get *Tenv* id)))
      (if (not (type? type))
	  (error 'find-type "Can't find type" id)
	  type)))

;*---------------------------------------------------------------------*/
;*    find-type/loc ...                                                */
;*---------------------------------------------------------------------*/
(define (find-type/location::type id::symbol loc)
   (let ((type (hashtable-get *Tenv* id)))
      (if (not (type? type))
	  (user-error/location loc 'find-type "Can't find type" id)
	  type)))

;*---------------------------------------------------------------------*/
;*    type-exists? ...                                                 */
;*    -------------------------------------------------------------    */
;*    Returns #t if the type exists _and_ is initialized.              */
;*---------------------------------------------------------------------*/
(define (type-exists?::bool id::symbol)
   (let ((type (hashtable-get *Tenv* id)))
      (if (not (type? type))
	  #f
	  (type-init? type))))

;*---------------------------------------------------------------------*/
;*    bind-type! ...                                                   */
;*---------------------------------------------------------------------*/
(define (bind-type!::type id::symbol init?::bool loc)
   (let ((type (hashtable-get *Tenv* id)))
      (if (type? type)
	  (if (and (not *allow-type-redefinition*)
		   (not *lib-mode*)
		   (type-init? type))
	      (user-error 'bind-type! "Type redefinition" (shape type))
	      (begin
		 (when (and (type? type)
			    (not *lib-mode*)
			    (type-init? type))
		    (unless *allow-type-redefinition*
		       (user-warning 'bind-type!
				     "Type redefinition"
				     (shape type))))
		 ;; the type has already been allocated, we mark it
		 ;; has initialized.
		 (if init? (type-init?-set! type #t))
		 ;; and we return it.
		 type))
	  (let ((new (instantiate::type (id id) (init? init?) (location loc))))
	     (hashtable-put! *Tenv* id new)
	     new))))

;*---------------------------------------------------------------------*/
;*    use-type! ...                                                    */
;*---------------------------------------------------------------------*/
(define (use-type!::type id::symbol loc)
   (trace (ast 2) "~~~ use-type!: " id " loc: " loc #\Newline)
   (let ((type (hashtable-get *Tenv* id)))
      (cond
	 ((type? type)
	  (type-occurrence-increment! type)
	  type)
	 (*types-already-checked?*
	  (user-error/location loc 'use-type! "Can't find type" id))
	 (else
	  (trace (ast 3) "    TYPE BOUND " id #\Newline)
	  (let ((type (bind-type! id #f loc)))
	     (type-occurrence-increment! type)
	     type)))))

;*---------------------------------------------------------------------*/
;*    use-type/import-loc! ...                                         */
;*---------------------------------------------------------------------*/
(define (use-type/import-loc!::type id::symbol loc loci)
   (trace (ast 2) "~~~ use-type!: " id " loc: " loc #\Newline)
   (let ((type (hashtable-get *Tenv* id)))
      (cond
	 ((type? type)
	  type)
	 (*types-already-checked?*
	  (user-error/location loc 'use-type! "Can't find type" id))
	 (else
	  (trace (ast 3) "    TYPE BOUND " id #\Newline)
	  (let ((type (bind-type! id #f loc)))
	     (type-import-location-set! type loci)
	     type)))))

;*---------------------------------------------------------------------*/
;*    use-foreign-type! ...                                            */
;*    -------------------------------------------------------------    */
;*    I have changed the syntax for the foreign declaration. In order  */
;*    to be consistent now, extern clauses have to be written using    */
;*    the :: notation (e.g. (print::int (::string ::int) "printf")).   */
;*    Since I also want a backward compatibility Bigloo accepts the    */
;*    two syntaxes. This function implement the compatibility.         */
;*---------------------------------------------------------------------*/
(define (use-foreign-type!::type id::symbol loc)
   (trace (ast 2) "~~~ use-foreign-type!: " id #\Newline)
   (let ((tid (parse-id id loc)))
      ;; parse-id calls  use-type! so, here we have to call use-type!
      ;; if and only if parse-id did do it with a real type.
      ;; That is, if the cdr of the result of parse-id is not
      ;; the default type.
      (if (eq? (cdr tid) (get-default-type))
	  ;; This works only because the default type is not a legal type
	  ;; that one can use in a foreign clause.
	  (use-type! (car tid) loc)
	  (cdr tid))))

;*---------------------------------------------------------------------*/
;*    use-foreign-type/import-loc! ...                                 */
;*    -------------------------------------------------------------    */
;*    I have changed the syntax for the foreign declaration. In order  */
;*    to be consistent now, extern clauses have to be written using    */
;*    the :: notation (e.g. (print::int (::string ::int) "printf")).   */
;*    Since I also want a backward compatibility Bigloo accepts the    */
;*    two syntaxes. This function implement the compatibility.         */
;*---------------------------------------------------------------------*/
(define (use-foreign-type/import-loc!::type id::symbol loc loci)
   (trace (ast 2) "~~~ use-foreign-type!: " id #\Newline)
   (let ((tid (parse-id id loc)))
      ;; parse-id calls  use-type! so, here we have to call use-type!
      ;; if and only if parse-id did do it with a real type.
      ;; That is, if the cdr of the result of parse-id is not
      ;; the default type.
      (if (eq? (cdr tid) (get-default-type))
	  ;; This works only because the default type is not a legal type
	  ;; that one can use in a foreign clause.
	  (use-type/import-loc! (car tid) loc loci)
	  (cdr tid))))

;*---------------------------------------------------------------------*/
;*    declare-type! ...                                                */
;*---------------------------------------------------------------------*/
(define (declare-type!::type id::symbol name::bstring class::symbol)
   (trace (ast 2) "~~~ declare-type!: " id #\Newline)
   (if (not (memq class '(bigloo C _ java)))
       (user-error "declare-type!"
		   "Illegal type class"
		   class)
       (let ((type (bind-type! id #t #unspecified)))
	  (type-name-set!   type name)
	  (type-$-set!      type ($-in-name? name))
	  (type-class-set!  type class)
	  type)))
 
;*---------------------------------------------------------------------*/
;*    declare-subtype! ...                                             */
;*    -------------------------------------------------------------    */
;*    Subtype inherit from coercion of their parents.                  */
;*---------------------------------------------------------------------*/
(define (declare-subtype!::type id::symbol name::bstring parents class::symbol)
   (trace (ast 2) "~~~ declare-subtype!: " id #\Newline)
   (assert (parents) (list? parents))
   (let ((type    (bind-type! id #t #unspecified))
	 (parents (map find-type parents)))
      (type-name-set!    type name)
      (type-$-set!       type ($-in-name? name))
      (type-class-set!   type class)
      (type-parents-set! type parents)
      type))

;*---------------------------------------------------------------------*/
;*    declare-aliastype! ...                                           */
;*---------------------------------------------------------------------*/
(define (declare-aliastype! id name class::symbol alias::type)
   (trace (ast 2) "~~~ declare-aliastype!: " id #\Newline)
   (let ((type (declare-type! id name class)))
      (type-alias-set! type alias)
      type)) 

;*---------------------------------------------------------------------*/
;*    for-each-type! ...                                               */
;*---------------------------------------------------------------------*/
(define (for-each-type! proc)
   (hashtable-for-each *Tenv* (lambda (k x) (proc x))))

;*---------------------------------------------------------------------*/
;*    uninitialized-types ...                                          */
;*    -------------------------------------------------------------    */
;*    We build the list of the unitialized types.                      */
;*---------------------------------------------------------------------*/
(define (uninitialized-types)
   (let ((uninit '()))
      (for-each-type! (lambda (t)
			 (if (not (type-init? t))
			     (set! uninit (cons t uninit)))))
      uninit))

;*---------------------------------------------------------------------*/
;*    *types-already-checked?* ...                                     */
;*---------------------------------------------------------------------*/
(define *types-already-checked?* #f)

;*---------------------------------------------------------------------*/
;*    check-types ...                                                  */
;*    -------------------------------------------------------------    */
;*    We check that all types are initialized.                         */
;*    -------------------------------------------------------------    */
;*    After this function is called, `use-type' does not tolerate the  */
;*    usage of undefined types (this is implemented using the          */
;*    `types-already-checked?*' variable).                             */
;*---------------------------------------------------------------------*/
(define (check-types)
   (let ((ut (uninitialized-types)))
      (when (pair? ut)
	 (newline (current-error-port))
	 (fprint (current-error-port)
		 (length ut) " type(s) used but not defined.")
	 (let loop ((ut ut))
	    (cond
	       ((null? ut)
		(fprint (current-error-port) "Stopping compilation...")
		(compiler-exit 56))
	       ((type? (car ut))
		(with-exception-handler
		   error-notify
		   (lambda ()
		      (if (type-import-location (car ut))
			  (user-error/location (type-import-location (car ut))
					       *module*
					       "Undefined type used in export clause"
					       (shape (car ut)))
			  (user-error/location (type-location (car ut))
					       *module*
					       "Undefined used type"
					       (shape (car ut))))))
		(loop (cdr ut)))
	       (else
		(with-exception-handler
		   error-notify
		   (lambda ()
		      (error *module* "Undefined type" (shape (car ut)))))))))
      (set! *types-already-checked?* #t)))

;*---------------------------------------------------------------------*/
;*    sub-type? ...                                                    */
;*    -------------------------------------------------------------    */
;*    Is a type a subtype of `obj'?                                    */
;*---------------------------------------------------------------------*/
(define (sub-type? minor major)
   (cond
      ((eq? minor major)
       #t)
      ((memq major (type-parents minor))
       #t)
      (else
       #f)))
