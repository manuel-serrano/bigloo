;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/comptime/Module/java.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jul 20 16:05:33 2000                          */
;*    Last change :  Tue Feb 17 09:17:26 2026 (serrano)                */
;*    Copyright   :  2000-26 Manuel Serrano                            */
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
	    object_slots
	    ast_var
	    ast_glo-decl
	    ast_env
	    ast_ident
	    ast_private
	    read_jvm
	    foreign_jtype
	    foreign_access)
   (export  (class jklass
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
	    
	    (make-java-compiler)
	    (java-finalizer)
	    (find-java-class ::symbol)
	    ;; heap-add-jclass is untyped other it force the module
	    ;; object-module to be imported in too many places.
	    (heap-add-jclass! jclass)
	    (parse-java-clause ::symbol ::pair)
	    (java-parser ::pair ::symbol ::symbol)
	    (java-declare-array j::pair id::symbol of::symbol ::symbol))
   (static  (class jfield
	       (src::pair read-only)
	       (id::symbol read-only)
	       (qid::symbol read-only)
	       (jname::bstring read-only)
	       (modifiers::pair-nil read-only (default '())))
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
      (producer (lambda (c) (parse-java-clause *module* c)))
      (consumer (lambda (m c) (parse-java-clause m c)))
      (finalizer java-finalizer)))

;*---------------------------------------------------------------------*/
;*    java-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (java-error java . msg)
   (user-error "Parse error"
      (if (pair? msg) (car msg) "Illegal Java variable")
      java
      '()))

;*---------------------------------------------------------------------*/
;*    parse-java-clause ...                                            */
;*---------------------------------------------------------------------*/
(define (parse-java-clause module clause)
   (if (memq 'java (backend-foreign-clause-support (the-backend)))
       (match-case clause
	  ((?- . ?protos)
	   (for-each (lambda (p) (java-parser p module '-)) protos)
	   '())
	  (else
	   (java-error clause "Illegal Java clause")))
       '()))

;*---------------------------------------------------------------------*/
;*    java-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (java-parser java module::symbol separator::symbol)
   (with-trace 'jvm "java-parser"
      (trace-item "module=" module)
      (trace-item "java="
	 (if (>fx (length java) 10) (append (take java 6) '("...")) java))
      (match-case java
	 ;; export clauses
	 ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
	  (set! *jexported* (cons (cons java module) *jexported*)))
	 ((export . ?-)
	  (java-error java "Illegal Java export form"))
	 ;; a java class
	 ((class ?ident . ?rest)
	  (java-parse-class java ident rest #f module separator))
	 ;; an abstract java class
	 ((abstract-class ?ident . ?rest)
	  (java-parse-class java ident rest #t module separator))
	 ((array (and (? symbol?) ?ident) (and (? symbol?) ?of))
	  (java-declare-array java ident of module))
	 (else
	  (java-error java)))))

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
;*    *jarrays* ...                                                    */
;*    -------------------------------------------------------------    */
;*    List of created arrays, only used in for declaring automaticall  */
;*    unbound Java classes in auto-declare-jarray-klass-types.         */
;*---------------------------------------------------------------------*/
(define *jarrays* '())

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
   (with-trace 'jvm "java-finalizer"
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
      ;; declare all the associated types
      (set! *jklasses* (reverse! *jklasses*))
      (let ((jclasses (map jklass->jclass *jklasses*)))
	 ;; declare all the Java classes
	 (for-each (lambda (jklass jclass)
		      (with-access::jklass jklass (id jname src package)
			 (remprop! (jklass-id jklass) 'jklass)
			 (unless (string? jname)
			    (java-error src "Can't find class declaration"))
			 (declare-jklass-properties! jklass jclass)))
	    *jklasses* jclasses)
	 ;; check that each Java class has a correct super class
	 (for-each (lambda (jklass jclass)
		      (with-access::jclass jclass (its-super)
			 (if (and its-super (not (jclass? its-super)))
			     (java-error
				(jklass-src jklass)
				"Super class is not a Java class"))))
	    *jklasses* jclasses)
	 ;; patch bigloo java exported variables name
	 (for-each (lambda (jmod)
		      (let* ((java (car jmod))
			     (mod (cdr jmod))
			     (global (find-global (get-genv) (cadr java)))
			     (name (caddr java)))
			 (cond
			    ((not (global? global))
			     (if (and (not (or (eq? *pass* 'make-add-heap)
					       (eq? *pass* 'make-heap)))
				      (eq? mod *module*))
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
	 ;; collect all the undeclared references type
	 ;; bind all the undeclared field types (automatic class declaration)
	 (let ((r (append
		     (append-map auto-declare-jklass-klass-types *jklasses*)
		     (filter-map auto-declare-jarray-klass-types *jarrays*))))
	    ;; cleanup
	    (set! *jexported* '())
	    (set! *jklasses* '())
	    (set! *jarrays* '())
	    ;; only used by module5
	    (if (pair? r)
		(list (unit 'java 47 r #t #f))
		'())))))

;*---------------------------------------------------------------------*/
;*    type-declared? ...                                               */
;*---------------------------------------------------------------------*/
(define (type-declared? ty::type)
   ;; can't use (get-type-object) because it would not work for
   ;; library modules during bootstrap
   (or (eq? (type-id ty) 'object) (type-init? ty)))

;*---------------------------------------------------------------------*/
;*    auto-class-predicate ...                                         */
;*---------------------------------------------------------------------*/
(define (auto-class-predicate ty::type)
   (let ((o (gensym 'obj))
	 (id (type-id ty)))
      `(define-inline (,(symbol-append id '?::bool) ,(symbol-append o '|::obj|))
	  ,(make-private-sexp 'instanceof id o))))

;*---------------------------------------------------------------------*/
;*    auto-declare-jklass-klass-types ...                              */
;*---------------------------------------------------------------------*/
(define (auto-declare-jklass-klass-types jklass::jklass)

   (define new-klasses '())
   
   (define (auto-declare-jklass ty::type jklass::jklass src)
      (with-trace 'jvm "auto-declare-jklass"
	 (trace-item "ty=" (type-id ty))
	 (trace-item "jk=" (jklass-id jklass))
	 (with-access::jklass jklass (package module)
	    (let ((k (instantiate::jklass
			(src src)
			(loc (find-location src))
			(id (type-id ty))
			(idd (type-id ty))
			(jname (symbol->string (type-id ty)))
			(package package)
			(abstract? #t)
			(module module))))
	       (set! new-klasses (cons ty new-klasses))
	       (declare-java-class! k)))))
   
   (with-access::jklass jklass (fields methods loc id)
      ;; field types
      (for-each (lambda (f::jfield)
		   (with-access::jfield f (id src)
		      (let ((ty (cdr (parse-id id loc))))
			 (unless (type-declared? ty)
			    (auto-declare-jklass ty jklass src)))))
	 fields)
      ;; method types
      (for-each (lambda (m::jmethod)
		   (with-access::jmethod m (id args src)
		      (let ((ty (cdr (parse-id id loc))))
			 (unless (type-declared? ty)
			    (auto-declare-jklass ty jklass src)))
		      (for-each (lambda (a)
				   (let ((ty (cdr (parse-id a loc))))
				      (unless (type-declared? ty)
					 (auto-declare-jklass ty jklass src))))
			 args)))
	 methods)
      ;; return the predicate definition
      (map auto-class-predicate new-klasses)))

;*---------------------------------------------------------------------*/
;*    auto-declare-jarray-klass-types ...                              */
;*---------------------------------------------------------------------*/
(define (auto-declare-jarray-klass-types jarray::jarray)
   
   (define (auto-declare-jklass ty::type jarray::jarray)
      (with-trace 'jvm "auto-declare-jklass"
	 (trace-item "ty=" (type-id ty))
	 (with-access::jarray jarray (location)
	    (let* ((n (symbol->string (type-id ty)))
		   (k (instantiate::jklass
			 (src '(auto))
			 (loc location)
			 (id (type-id ty))
			 (idd (type-id ty))
			 (jname n)
			 (package (prefix n))
			 (abstract? #t)
			 (module 'foreign))))
	       (declare-java-class! k)))))
   
   (with-access::jarray jarray (item-type)
      (unless (type-declared? item-type)
	 (auto-declare-jklass item-type jarray)
	 (auto-class-predicate item-type))))
	    
;*---------------------------------------------------------------------*/
;*    java-parse-class ...                                             */
;*---------------------------------------------------------------------*/
(define (java-parse-class java ident rest abstract? module separator)
   (with-trace 'jvm "java-parse-class"
      (trace-item "ident=" ident)
      (let* ((tser (reverse rest))
	     (jname (if (pair? tser) (car tser) #f)))
	 (cond
	    ((not (symbol? ident))
	     (java-error java "Illegal Java class"))
	    ((string? jname)
	     (java-declare-class java ident jname (cdr tser) abstract? module separator))
	    (else
	     (java-refine-class java ident rest module separator))))))

;*---------------------------------------------------------------------*/
;*    java-declare-class ...                                           */
;*---------------------------------------------------------------------*/
(define (java-declare-class::jklass j id::symbol jname::bstring
	   comp::pair-nil a::bool module::symbol separator::symbol)
   (with-trace 'jvm "java-declare-class"
      (trace-item "id=" id)
      (trace-item "jname=" jname)
      (let ((loc (find-location j))
	    (klass (find-jklass id)))
	 (trace-item "old=" (typeof klass))
	 (cond
	    ((not (jklass? klass))
	     (let ((k (instantiate::jklass
			 (src j)
			 (loc loc)
			 (id id)
			 (idd (fast-id-of-id id loc))
			 (jname jname)
			 (abstract? a)
			 (module module))))
		(for-each (lambda (c)
			     (java-declare-component j k c separator))
		   comp)
		k))
	    ((not (eq? (jklass-abstract? klass) a))
	     (user-error/location loc "Parse error"
		"Illegal Java class redefinition" id))
	    ((not (string? (jklass-jname klass)))
	     (jklass-jname-set! klass jname)
	     klass)
	    ((string=? (jklass-jname klass) jname)
	     klass)
	    (else
	     (user-error/location loc "Parse error"
		"Illegal Java class redefinition" id))))))

;*---------------------------------------------------------------------*/
;*    java-refine-class ...                                            */
;*    -------------------------------------------------------------    */
;*    This function is used when someone refine the declaration        */
;*    of a Java class.                                                 */
;*---------------------------------------------------------------------*/
(define (java-refine-class::jklass j ident::symbol comp::pair-nil
	   module::symbol separator::symbol)
   (with-trace 'jvm "java-refine-class"
      (trace-item "id=" ident)
      (let ((jklass (let ((jklass (find-jklass ident)))
		       (if (jklass? jklass)
			   jklass
			   (instantiate::jklass
			      (src j)
			      (loc (find-location j))
			      (idd (fast-id-of-id ident (find-location j)))
			      (id ident)
			      (module module))))))
	 (for-each (lambda (c) (java-declare-component j jklass c separator))
	    comp)
	 jklass)))

;*---------------------------------------------------------------------*/
;*    java-declare-component ...                                       */
;*---------------------------------------------------------------------*/
(define (java-declare-component j jklass::jklass component separator::symbol)
   
   (define (every pred? lst)
      (let loop ((lst lst))
	 (cond
	    ((null? lst) #t)
	    ((not (pair? lst)) #f)
	    ((pred? (car lst)) (loop (cdr lst)))
	    (else #f))))
   
   (define (arg-list? lst)
      (every (lambda (s) (and (symbol? s) (type-ident? s))) lst))
   
   (define (modifier-list? lst)
      (every (lambda (s)
		(and (symbol? s)
		     (memq s '(public private protected
			       static final synchronized
			       transient abstract volatile))))
	 lst))
   
   (define (make-ident base id)
      (let* ((b (symbol->string! base))
	     (j (string-index-right b #\$)))
	 (if (and #f j (>fx j 0))
	     (let ((baseid (string-replace b #\$ #\.)))
		(string->symbol (format "~a~a~a" baseid separator id)))
	     (symbol-append base separator id))))
   
   (match-case component
      ((field . ?rest)
       (match-case (reverse rest)
	  (((and (? string?) ?jname) (and (? symbol?) ?id) . ?mod)
	   (if (not (modifier-list? mod))
	       (java-error component "Illegal Java field (wrong modifiers)")
	       (with-access::jklass jklass (fields idd)
		  (let ((jfield (instantiate::jfield
				   (src component)
				   (id id)
				   (qid (if (eq? idd 'foreign)
					    id
					    (make-ident idd id)))
				   (jname jname)
				   (modifiers mod))))
		     (set! fields (cons jfield fields))))))
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
					  (make-ident idd id)))
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
			    (id (make-typed-ident
				   (symbol-append '%% (make-ident idd id))
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
;*    jklass->jclass ...                                               */
;*---------------------------------------------------------------------*/
(define (jklass->jclass jklass::jklass)
   (with-trace 'jvm "jklass->jclass"
      (with-access::jklass jklass (id jname package src loc)
	 (trace-item "id=" id)
	 (trace-item "jname=" jname)
	 (let ((prefix (prefix jname)))
	    ;; set the java class package
	    (if (string=? prefix jname)
		(set! package "")
		(set! package prefix)))
	 ;; add a qualified type so Bigloo won't complain when fetching
	 ;; slots or calling methods of this class
	 (class-qualified-type-name-set! id jname)
	 (class-qualified-type-name-set! (fast-id-of-id id loc) jname)
	 ;; construct the associated jclass
	 (declare-java-class! jklass))))
   
;*---------------------------------------------------------------------*/
;*    declare-jklass-properties! ...                                   */
;*    -------------------------------------------------------------    */
;*    This function is called in the Java finalization stage.          */
;*---------------------------------------------------------------------*/
(define (declare-jklass-properties! jklass::jklass jclass::jclass)
   (with-trace 'jvm "declare-jklass-properties!"
      (with-access::jklass jklass (id jname constructors methods fields src loc)
	 
	 (define (is-class? a jklass)
	    (with-access::jklass jklass (id idd)
	       (let ((aid (string->symbol (substring (symbol->string! a) 2))))
		  (or (eq? aid id) (eq? aid idd)))))
	 
	 (define (declare-java-static-method jmet)
	    (with-trace 'jvm "declare-java-static-method"
	       (with-access::jmethod jmet (id args jname src modifiers)
		  (declare-java-method! id (jklass-id jklass)
		     jname args modifiers
		     (jklass-jname jklass)
		     src))))
	 
	 (define (declare-java-virtual-method jmet)
	    (with-trace 'jvm "declare-java-virtual-method"
	       (with-access::jmethod jmet (id args jname src modifiers)
		  (trace-item "id=" id)
		  (trace-item "modifiers=" modifiers)
		  (trace-item "jname=" jname)
		  (trace-item "args=" (map shape args))
		  (if (and (not (jconstructor? jmet))
			   (not (and (pair? args) (is-class? (car args) jklass))))
		      (java-error src "Illegal first argument of virtual method")
		      (declare-java-method! id (jklass-id jklass)
			 jname args modifiers
			 (jklass-jname jklass)
			 src)))))
	 
	 (define (declare-java-method jmet::jmethod)
	    (with-trace 'jvm "declarel-java-method"
	       (with-access::jmethod jmet (id modifiers jname args)
		  (trace-item "id=" id)
		  (trace-item "modifiers=" modifiers)
		  (trace-item "jname=" jname)
		  (trace-item "args=" (map shape args))
		  (if (memq 'static modifiers)
		      (declare-java-static-method jmet)
		      (declare-java-virtual-method jmet)))))
	 
	 (define (declare-java-field jfd::jfield)
	    (with-trace 'jvm "declarel-java-field"
	       (with-access::jfield jfd (qid jname src modifiers)
		  (trace-item "qid=" qid)
		  (trace-item "jname=" jname)
		  (when (memq 'static modifiers)
		     (declare-java-static-field qid (jklass-id jklass) jname
			modifiers (jklass-jname jklass) src)))))
	 
	 (trace-item "id=" id)
	 (trace-item "jname=" jname)
	 
	 ;; we add a qualified type so Bigloo won't complain when fetching
	 ;; slots or calling methods of this class
	 (for-each declare-java-method methods)
	 (for-each declare-java-field fields)
	 (with-access::jclass jclass (its-super)
	    (trace-item "its-super=" (shape its-super))
	    (if its-super
		(let ((typ (cond
			      ((jclass? its-super) its-super)
			      ((type? its-super) its-super)
			      (else (find-type its-super)))))
		   (set! its-super typ)))
	    jclass))))

;*---------------------------------------------------------------------*/
;*    declare-java-method! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-java-method! id module jname args modifiers kname src)
   (with-trace 'jvm "declare-java-method!"
      (trace-item "id=" id)
      (trace-item "jname=" jname)
      (let* ((pid (parse-id id (find-location src)))
	     (ln (car pid))
	     (tid (type-id (cdr pid))))
	 (trace-item "ln=" ln)
	 (trace-item "tid=" tid)
	 (trace-item "args=" (map shape args))
	 (let ((g (declare-global-cfun! (get-genv) ln #f module jname tid args #f #f src #f)))
	    (cfun-method-set! (global-value g) modifiers)
	    (global-qualified-type-name-set! g kname)
	    g))))

;*---------------------------------------------------------------------*/
;*    declare-java-static-field ...                                    */
;*---------------------------------------------------------------------*/
(define (declare-java-static-field id module jname modifiers kname src)
   (let* ((pid (parse-id id (find-location src)))
	  (ln (car pid))
	  (tid (type-id (cdr pid))))
      (let ((g (declare-global-cvar! (get-genv) ln #f module jname tid #f src #f)))
	 (global-qualified-type-name-set! g kname)
	 g)))

;*---------------------------------------------------------------------*/
;*    declare-java-class! ...                                          */
;*---------------------------------------------------------------------*/
(define (declare-java-class!::jclass jklass::jklass)
   
   (define (jfield->lfield jfd::jfield)
      (with-access::jfield jfd ((component src) id jname (mod modifiers))
	 (list component id jname mod)))
   
   (with-trace 'jvm "declare-java-class!"
      (with-access::jklass jklass (src id jname package loc
				     fields constructors
				     abstract?
				     module)
	 (trace-item "id=" id)
	 (trace-item "jname=" jname)
	 (trace-item "package=" package)
	 (let* ((pid (parse-id id loc))
		(jid (car pid))
		(super (cdr pid))
		(qid (string->symbol jname)))
	    (trace-item "jid=" jid)
	    (trace-item "super=" (shape super))
	    ;; create the class holder
	    ;; and create a type for this class
	    (let ((jclass (declare-java-class-type! jid super jname package src)))
	       ;; bind the method names for the expansion of the
	       ;; ((-> v f) ...) method call syntax (see Ast/object.scm)
	       (with-access::jclass jclass (methods)
		  (set! methods
		     (filter-map (lambda (m)
				    (with-access::jmethod m (id modifiers)
				       (unless (memq 'static modifiers)
					  (fast-id-of-id id loc))))
			(with-access::jklass jklass (methods)
			   methods))))
	       ;; both registration are needed for the SawJvm backend
	       (register-java-class! jid jname)
	       (when (>fx (string-length package) 0)
		  (let ((fqid (string->symbol jname)))
		     (unless (eq? jid fqid)
			;; declare an alias for the fully qualified type name
			(trace-item "rebind=" fqid)
			(trace-item "init?=" (type-init? jclass))
			(register-java-class! fqid jname)
			(rebind-type! fqid jclass))))
	       ;; store the src-import location in order to print a nice error
	       ;; message if that tclass is not defined
	       (type-import-location-set! jclass loc)
	       ;; when importing a class, import the accessors...
	       (delay-class-accessors! jclass
		  (delay (begin
			    (import-java-class-accessors!
			       (map jfield->lfield fields)
			       constructors
			       jclass
			       abstract?
			       module
			       src))))
	       jclass)))))

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
(define (java-declare-array j::pair id::symbol of::symbol module::symbol)
   (with-trace 'jvm "java-declare-array"
      (trace-item "id=" id)
      (trace-item "module=" module)
      ;; Only arrays are explictly associated to types. Java classes
      ;; are defined by jclasses.
      (cond
	 ((and (type-exists? id) (find-type id))
	  =>
	  (lambda (ty)
	     (unless (and (isa? ty jarray)
			  (not (eq? (type-id (jarray-item-type ty)) of)))
		(java-error j "Illegal type redeclaration"))))
	 ((not (type-ident? of))
	  (java-error j "Illegal array item type"))
	 (else
	  (let* ((sof (symbol->string of))
		 (tof (string->symbol (substring sof 2 (string-length sof))))
		 (jtype (declare-jvm-type! id tof j)))
	     (set! *jarrays* (cons jtype *jarrays*))
	     (foreign-accesses-add!
		(make-ctype-accesses! jtype jtype (find-location j) module)))))))
      
