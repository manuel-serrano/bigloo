;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/java.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 16:05:33 2000                          */
;*    Last change :  Wed Nov 21 07:30:08 2012 (serrano)                */
;*    Copyright   :  2000-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Java module clause handling.                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_java
   (include "Ast/unit.sch"
	    "Tools/trace.sch"
	    "Module/java.sch")
   (import  module_module
	    module_checksum
	    module_class
	    module_prototype
	    module_foreign
	    engine_param
	    backend_backend
	    tools_error
	    tools_shape
	    tools_misc
	    tools_location
	    type_type
	    type_env
	    type_tools
	    type_cache
	    object_class
	    object_java-access
	    ast_var
	    ast_glo-decl
	    ast_env
	    ast_ident
	    read_jvm
	    foreign_jtype
	    foreign_access)
   (export  (make-java-compiler)
	    (find-java-class ::symbol)
	    ;; heap-add-jclass is untyped other it force the module
	    ;; object-module to be imported in too many places.
	    (heap-add-jclass! jclass))
   (static  (class jklass
	       (bind-jklass!)
	       (src::pair read-only)
	       (loc read-only)
	       (id::symbol read-only)
	       (idd::symbol read-only)
	       (jname (default #unspecified))
	       (package (default #unspecified))
	       (fields::pair-nil (default '()))
	       (methods::pair-nil (default '()))
	       (constructors::pair-nil (default '()))
	       (abstract?::bool (default #f))
	       (module (default #unspecified)))
	    (class jmethod
	       (src::pair read-only)
	       (id::symbol read-only)
	       (args::pair-nil read-only)
	       (jname::bstring read-only)
	       (modifiers::pair-nil read-only (default '())))
	    (class jconstructor::jmethod)))

;*---------------------------------------------------------------------*/
;*    make-java-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-java-compiler)
   (instantiate::ccomp
      (id 'java)
      (producer (lambda (c) (java-producer *module* c)))
      (consumer (lambda (m c) (java-producer m c)))
      (finalizer java-finalizer)))

;*---------------------------------------------------------------------*/
;*    java-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (java-error java . msg)
   (user-error "Parse error"
	       (if (pair? msg) (car msg) "Illegal java variable")
	       java
	       '()))

;*---------------------------------------------------------------------*/
;*    java-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (java-producer module clause)
   (if (memq 'java (backend-foreign-clause-support (the-backend)))
       (match-case clause
	  ((?- . ?protos)
	   (for-each (lambda (p) (java-parser p module)) protos)
	   '())
	  (else
	   (java-error clause "Illegal `java' clause")))
       '()))

;*---------------------------------------------------------------------*/
;*    java-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (java-parser java module)
   (trace (ast 2) "java parser: " java " " module #\Newline)
   (match-case java
      ;; export clauses
      ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
       (set! *jexported* (cons (cons java module) *jexported*)))
      ((export . ?-)
       (java-error java "Illegal java export form"))
      ;; a java class
      ((class ?ident . ?rest)
       (java-parse-class java ident rest #f module))
      ;; an abstract java class
      ((abstract-class ?ident . ?rest)
       (java-parse-class java ident rest #t module))
      ((array (and (? symbol?) ?ident) (and (? symbol?) ?of))
       (java-declare-array java ident of))
      (else
       (java-error java))))

;*---------------------------------------------------------------------*/
;*    *jklasses* ...                                                   */
;*    -------------------------------------------------------------    */
;*    This variable helds parsed but not yet declared java classes.    */
;*    It accumulates all the definition (a class may be defined        */
;*    using several steps). It is consumed by the Java finalizer       */
;*    that declares the Bigloo type.                                   */
;*---------------------------------------------------------------------*/
(define *jklasses* '())

;*---------------------------------------------------------------------*/
;*    *jclasses*                                                       */
;*    -------------------------------------------------------------    */
;*    This local variable is used to implement the checking of the     */
;*    Java class super-class. As soon as it is checked that each       */
;*    Java class has a Java class for super class, this variable       */
;*    is reset to '().                                                 */
;*---------------------------------------------------------------------*/
(define *jclasses* '())

;*---------------------------------------------------------------------*/
;*    *jexported* ...                                                  */
;*---------------------------------------------------------------------*/
(define *jexported* '())

;*---------------------------------------------------------------------*/
;*    find-jklass ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-jklass ident)
   (getprop ident 'jklass))

;*---------------------------------------------------------------------*/
;*    bind-jklass! ...                                                 */
;*---------------------------------------------------------------------*/
(define (bind-jklass! jklass::jklass)
   (putprop! (jklass-id jklass) 'jklass jklass)
   (set! *jklasses* (cons jklass *jklasses*)))

;*---------------------------------------------------------------------*/
;*    java-finalizer ...                                               */
;*    -------------------------------------------------------------    */
;*    Now that all the Java classes have been parsed, we have          */
;*    to create associated Bigloo types.                               */
;*---------------------------------------------------------------------*/
(define (java-finalizer)
   ;; First, we check for the foreign class. If defined but bound (i.e.,
   ;; we have seen fields or methods but not the declaration of the
   ;; class itself), we bind it
   (let ((jklass (find-jklass *jvm-foreign-class-id*)))
      (if (jklass? jklass)
	  (if (not (string? (jklass-jname jklass)))
	      (begin
		 (jklass-abstract?-set! jklass #t)
		 (jklass-jname-set! jklass *jvm-foreign-class-name*))
	      (if (not (eq? (jklass-jname jklass) *jvm-foreign-class-name*))
		  (java-error (jklass-src jklass)
			      "Illegal foreign class definition")))))
   ;; we declare all the Java classes
   (for-each (lambda (jklass)
		(with-access::jklass jklass (id jname src package)
		   (remprop! (jklass-id jklass) 'jklass)
		   (if (not (string? jname))
		       (begin
			  (set! *jclasses* '())
			  (java-error src "Can't find main class declaration"))
		       (let ((prefix (prefix jname)))
			  (if (string=? prefix jname)
			      (set! package "")
			      (set! package prefix))
			  (set! *jclasses* (cons (declare-jklass jklass)
						 *jclasses*))))))
	     *jklasses*)
   ;; we check that each Java class has a correct super class
   (if (pair? *jclasses*)
       (for-each (lambda (jklass jclass)
		    (with-access::jclass jclass (its-super)
		       (if (and its-super (not (jclass? its-super)))
			   (java-error
			    (jklass-src jklass)
			    "Super class is not a Java class"))))
		 *jklasses* (reverse! *jclasses*)))
   (set! *jclasses* '())
   ;; we patch bigloo java exported variables name
   (for-each (lambda (jmod)
		(let* ((java (car jmod))
		       (module (cdr jmod))
		       (global (find-global (cadr java)))
		       (name (caddr java)))
		   (cond
		      ((not (global? global))
		       (if (and (not (or (eq? *pass* 'make-add-heap)
					 (eq? *pass* 'make-heap)))
				(eq? module *module*))
			   (java-error java
				       "Unbound (or static) global variable")))
		      ((string? (global-name global))
		       (user-warning
			"Java"
			"Re-exportation of global variable (ignored)"
			java))
		      (else
		       (global-name-set! global name)))))
	     *jexported*)
   ;; reset the whole stuff
   (set! *jexported* '())
   (set! *jklasses* '()))
      
;*---------------------------------------------------------------------*/
;*    java-parse-class ...                                             */
;*---------------------------------------------------------------------*/
(define (java-parse-class java ident rest abstract? module)
   (let* ((tser (reverse rest))
	  (jname (if (pair? tser) (car tser) #f)))
      (cond
	 ((not (symbol? ident))
	  (java-error java "Illegal java class"))
	 ((string? jname)
	  (java-declare-class java ident jname (cdr tser) abstract? module))
	 (else
	  (java-refine-class java ident rest module)))))

;*---------------------------------------------------------------------*/
;*    java-declare-class ...                                           */
;*---------------------------------------------------------------------*/
(define (java-declare-class j id::symbol jname::bstring comp::pair-nil a::bool
			    module)
   (let* ((loc (find-location j))
	  (jklass (let ((jklass (find-jklass id)))
		    (cond
		       ((not (jklass? jklass))
			(instantiate::jklass
			   (src j)
			   (loc loc)
			   (id id)
			   (idd (fast-id-of-id id loc))
			   (jname jname)
			   (abstract? a)
			   (module module)))
		       ((not (eq? (jklass-abstract? jklass) a))
			(java-error j "Illegal Java class redefinition"))
		       ((not (string? (jklass-jname jklass)))
			(jklass-jname-set! jklass jname))
		       ((string=? (jklass-jname jklass) jname)
			jklass)
		       (else
			(java-error j "Illegal Java class redefinition"))))))
      (for-each (lambda (c) (java-declare-component j jklass c)) comp)))

;*---------------------------------------------------------------------*/
;*    java-refine-class ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is used when someone refine the declaration        */
;*    of a Java class.                                                 */
;*---------------------------------------------------------------------*/
(define (java-refine-class j ident::symbol comp::pair-nil module)
   (let ((jklass (let ((jklass (find-jklass ident)))
		    (if (jklass? jklass)
			jklass
			(instantiate::jklass
			   (src j)
			   (loc (find-location j))
			   (idd (fast-id-of-id ident (find-location j)))
			   (id ident)
			   (module module))))))
      (for-each (lambda (c) (java-declare-component j jklass c)) comp)))

;*---------------------------------------------------------------------*/
;*    java-declare-component ...                                       */
;*---------------------------------------------------------------------*/
(define (java-declare-component j jklass::jklass component)
   (define (every pred? lst)
      (let loop ((lst lst))
	 (cond
	    ((null? lst)
	     #t)
	    ((not (pair? lst))
	     #f)
	    ((pred? (car lst))
	     (loop (cdr lst)))
	    (else
	     #f))))
   (define (arg-list? lst)
      (every (lambda (s) (and (symbol? s) (type-ident? s))) lst))
   (define (modifier-list? lst)
      (every (lambda (s)
		(and (symbol? s)
		     (memq s '(public private protected
			       static final synchronized
			       abstract))))
	      lst))
   (match-case component
      ((field . ?rest)
       (match-case (reverse rest)
	  (((and (? string?) ?jname) (and (? symbol?) ?id) . ?mod)
	   (if (not (modifier-list? mod))
	       (java-error component "Illegal Java field (wrong modifiers)")
	       (with-access::jklass jklass (fields)
		  (set! fields (cons (list component id jname mod) fields)))))
	  (else
	   (java-error component "Illegal Java field"))))
      ((method . ?rest)
       (match-case (reverse rest)
	  (((and (? string?) ?jname)
	    (and (? arg-list?) ?args)
	    (and (? symbol?) ?id)
	    . ?mod)
	   (if (not (modifier-list? mod))
	       (java-error component "Illegal Java method (wrong modifiers)")
	       (with-access::jklass jklass (methods idd abstract?)
		  (let* ((mod (if abstract? (cons 'abstract mod) mod))
			 (jmet (instantiate::jmethod
				  (src component)
				  (id (if (eq? idd 'foreign)
					  id
					  (symbol-append idd '- id)))
				  (args args)
				  (jname jname)
				  (modifiers mod))))
		     (set! methods (cons jmet methods))))))
	  (else
	   (java-error component "Illegal Java method"))))
      ((or (constructor public (and (? symbol?) ?id) (and (? arg-list?) ?args))
	   (constructor (and (? symbol?) ?id) (and (? arg-list?) ?args)))
       (with-access::jklass jklass (constructors methods idd)
	  (let ((jconstr (instantiate::jconstructor
			    (src component)
			    (id (make-typed-ident (symbol-append '%% idd '- id)
						  idd))
			    (args args)
			    (jname "<init>"))))
	     (set! methods (cons jconstr methods))
	     (set! constructors (cons (cons id args) constructors)))))
      (else
       (if (pair? component)
	   (java-error component "Illegal class field")
	   (java-error j (string-append "Illegal class field `"
					(with-output-to-string
					   (lambda ()
					      (write component)))
					"'"))))))

;*---------------------------------------------------------------------*/
;*    declare-jklass ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is called in the Java finalization stage.          */
;*---------------------------------------------------------------------*/
(define (declare-jklass jklass::jklass)
   (with-access::jklass jklass (id jname constructors methods src loc)
      (define (declare-java-static-method jmet)
	 (with-access::jmethod jmet (id args jname src modifiers)
	    (declare-java-method! id (jklass-id jklass)
				  jname args modifiers
				  (jklass-jname jklass)
				  src)))
      (define (declare-java-virtual-method jmet)
	 (with-access::jmethod jmet (id args jname src modifiers)
	    (if (and (not (jconstructor? jmet))
		     (not (and (pair? args)
			       (eq? (type-id (type-of-id (car args) loc))
				    (jklass-idd jklass)))))
		(java-error src "Illegal first argument of virtual method")
		(declare-java-method! id (jklass-id jklass)
				      jname args modifiers
				      (jklass-jname jklass)
				      src))))
      (define (declare-java-method jmet::jmethod)
	 (with-access::jmethod jmet (modifiers)
	    (if (memq 'static modifiers)
		(declare-java-static-method jmet)
		(declare-java-virtual-method jmet))))
      ;; we add a qualified type so Bigloo won't complain when fetching
      ;; slots or calling methods of this class
      (add-qualified-type! id jname)
      (add-qualified-type! (fast-id-of-id id loc) jname)
      (for-each declare-java-method methods)
      (let ((jclass (declare-java-class! jklass)))
	 (with-access::jclass jclass (its-super)
	    (if its-super
		(let ((type (cond
			       ((jclass? its-super)
				its-super)
			       ((type? its-super)
				its-super)
			       (else
				(find-type its-super)))))
		   (set! its-super type)))
	    jclass))))

;*---------------------------------------------------------------------*/
;*    declare-java-method! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-java-method! id module jname args modifiers kname src)
   (let* ((pid (parse-id id (find-location src)))
	  (ln (car pid))
	  (tid (type-id (cdr pid))))
      (let ((g (declare-global-cfun! ln #f module jname tid args #f #f src #f)))
	 (cfun-method-set! (global-value g) modifiers)
	 (global-jvm-type-name-set! g kname)
	 g)))

;*---------------------------------------------------------------------*/
;*    declare-java-class! ...                                          */
;*---------------------------------------------------------------------*/
(define (declare-java-class!::jclass jklass::jklass)
   (with-access::jklass jklass (src id jname package loc
				    fields constructors
				    abstract?
				    module)
      (let* ((pid (parse-id id loc))
	     (jid (car pid))
	     (super (cdr pid)))
	 (register-java-class! jid jname)
	 ;; We create the class holder
	 ;; and we create a type for this class
	 (let ((jclass (declare-java-class-type! jid super jname package src)))
	    ;; some paranoid checking
	    (assert (jclass) (jclass? jclass))
	    ;; we store the src-import location in order to print a nice error
	    ;; message if that tclass is not defined
	    (type-import-location-set! jclass loc)
	    ;; when importing a class, we import the accessors...
	    (delay-class-accessors!
	     jclass
	     (delay (import-java-class-accessors!
		     fields
		     constructors
		     jclass
		     abstract?
		     module
		     src)))
	    ;; we are done
	    jclass))))

;*---------------------------------------------------------------------*/
;*    *java-classes* ...                                               */
;*---------------------------------------------------------------------*/
(define *java-classes* '())

;*---------------------------------------------------------------------*/
;*    register-java-class! ...                                         */
;*---------------------------------------------------------------------*/
(define (register-java-class! class::symbol qualified-name::bstring)
   (set! *java-classes* (cons (cons class qualified-name) *java-classes*)))

;*---------------------------------------------------------------------*/
;*    heap-add-jclass! ...                                             */
;*---------------------------------------------------------------------*/
(define (heap-add-jclass! new)
   (assert (new) (jclass? new))
   (register-java-class! (jclass-id new) (jclass-name new)))

;*---------------------------------------------------------------------*/
;*    find-java-class ...                                              */
;*---------------------------------------------------------------------*/
(define (find-java-class class::symbol)
   (let ((cell (assq class *java-classes*)))
      (if (pair? cell)
	  (cdr cell)
	  #f)))

;*---------------------------------------------------------------------*/
;*    java-declare-array ...                                           */
;*---------------------------------------------------------------------*/
(define (java-declare-array j::pair id::symbol of::symbol)
   ;; Only arrays are explictly associated to types. Java classes
   ;; are defined by jclasses.
   (if (not (type-ident? of))
       (java-error j "Illegal array item type")
       (let* ((sof (symbol->string of))
	      (tof (string->symbol (substring sof 2 (string-length sof))))
	      (jtype (declare-jvm-type! id tof j)))
	  (foreign-accesses-add!
	   (make-ctype-accesses! jtype jtype (find-location j))))))
      
