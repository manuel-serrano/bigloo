;*=====================================================================*/
;*    .../project/bigloo/5.0.x/comptime/Module/extern-java.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:51:54 2026                          */
;*    Last change :  Sun Jun 21 08:39:18 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 extern plugins                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_extern-java
   
   (import engine_param
	   tools_error
	   tools_shape
 	   tools_location
	   tools_misc
	   read_jvm
	   backend_backend
	   module_module
	   module_class
	   module_checksum
	   module_pragma
	   module_foreign
	   module_java
	   module_type
	   module_eval
	   module_extern5
	   heap_restore
	   expand_eps
	   expand_object
	   expand_assert
	   ast_node
	   ast_var
	   ast_env
	   ast_glo-decl
	   ast_ident
	   ast_toplevel
	   ast_build
	   ast_sexp
	   ast_private
	   ast_walk
	   type_type
	   type_env
	   type_cache
	   object_class
	   object_slots
	   object_coercion
	   foreign_jtype)

   (export (module5-extern-plugin-java ::Module ::pair)
	   (module5-extern-plugin-java-finalizer ::Module)
	   (module4-extern-plugin-java ::Module ::pair)))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-java-finalizer ...                         */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-java-finalizer mod::Module)
   ;; Mark that all the java class predicates cannot be removed
   ;; until the coercion and checks have been inserted in the AST
   ;; Because of the complex Java code generation and complex declarations
   ;; associated with these codes, this cannot mark assignments cannot
   ;; done while the classes are constructed
   (for-each-type! (lambda (t)
		      (when (isa? t jarray)
			 (let* ((p (symbol-append (type-id t) '?))
				(g (find-global (get-genv) p)))
			    (when (isa? g global)
			       (global-removable-set! g 'coerce)))))))

;*---------------------------------------------------------------------*/
;*    module4-extern-plugin-java ...                                   */
;*---------------------------------------------------------------------*/
(define (module4-extern-plugin-java mod::Module x::pair)

   (define (parse-clause clause)
      (match-case clause
	 ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
	  (java-parser clause (-> mod id) '-))
	 ((or (class ?ident . ?rest)
	      (abstract-class ?ident . ?rest))
	  (let ((jklass (java-parser clause (-> mod id) '-)))
	     (declare-java-jklass! jklass mod clause)
	     (with-access::jklass jklass (delayed-accessors? idd)
		(set! delayed-accessors? #f)
		(declare-java-predicate! idd mod clause)
		(declare-jklass-constructors! jklass mod clause)
		(declare-jklass-methods! jklass mod clause)
		(declare-jklass-fields! jklass mod clause))))
	 (else
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))))
   
   (when (memq 'java (backend-foreign-clause-support (the-backend)))
      (for-each parse-clause (cdr x)))
   '())

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-java ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-java mod::Module x::pair)
   
   (define (parse-clause clause mod::Module x::pair pkg)
      
      (define modifier-list
	 '(public private protected static final synchronized
	   abstract transient volatile))
      
      (define (class-name name)
	 (let ((i (string-contains name "::")))
	    (if i
		(substring name 0 i)
		name)))
      
      (define (parse-class5-ident ident)
	 (let* ((s (symbol->string ident))
		(i (string-contains s "::"))
		(name (if i (substring s 0 i) s))
		(super (when i (string->symbol (substring s (+fx i 2))))))
	    (if (char=? (string-ref name 0) #\.)
		(let ((name (substring name 1)))
		   (values #f (class-name name) (string->symbol name) super))
		(let ((i (string-index-right name #\.)))
		   (if i
		       (let ((pkg (substring name 0 i))
			     (id (substring name (+fx i 1))))
			  (values pkg (class-name name) (string->symbol id) super))
		       (values #f (class-name name) (string->symbol name) super))))))
      
      (define (field5->field4 field clazz)
	 (if (symbol? field)
	     (multiple-value-bind (id type)
		(parse-ident field)
		`(field ,field ,(symbol->string id)))
	     (let loop ((f field)
			(m '()))
		(cond
		   ((null? field)
		    (error/loc mod "Illegal class field" field x))
		   ((not (pair? f))
		    (error/loc mod "Illegal class field" field x))
		   ((memq (car f) modifier-list)
		    (loop (cdr f) (cons (car f) m)))
		   (else
		    (match-case f
		       ;; field
		       ((field (and (? symbol?) ?ident))
			(multiple-value-bind (id type)
			   (parse-ident ident)
			   `(field ,@(reverse! m)
			       ,ident ,(symbol->string id))))
		       ((field ?modf . ?rest)
			(let ((ident (car (last-pair rest))))
			   (if (symbol? ident)
			       (multiple-value-bind (id type)
				  (parse-ident ident)
				  `(field ,@(append (reverse! m) (list modf) (drop-last rest 1))
				      ,ident ,(symbol->string id)))
			       (error/loc mod "Illegal class field" field x))))
		       ;; constructor
		       ((constructor ?id . ?rest)
			`(constructor ,@(reverse! m)
			    ,id ,rest))
		       ((?ident . (and (? list?) ?args))
			;; method
			(multiple-value-bind (id type)
			   (parse-ident ident)
			   (if (and (pair? args) (string? (car (last-pair args))))
			       ;; the last argument is the actual method java name
			       (let ((sgra (reverse args)))
				  `(method ,@(reverse m)
				      ,ident
				      ,(if (memq 'static m)
					   (reverse (cdr sgra))
					   (cons clazz (reverse (cdr sgra))))
				      ,(car sgra)))
			       `(method ,@(reverse m)
				   ,ident
				   ,(if (memq 'static m) args (cons clazz args))
				   ,(symbol->string id)))))
		       (else
			(error/loc mod "Illegal class field" field x))))))))
      
      (define (class5->class4 clause cpkg name id super rest)
	 (let ((clazz (symbol-append '|::| id)))
	    (localize clause
	       `(,(car clause)
		 ,(if super (string->symbol (format "~a::~a" id super)) id)
		 ,@(map (lambda (f) (localize f (field5->field4 f clazz))) rest)
		 ,(cond
		     (cpkg name)
		     (pkg (format "~a.~a" pkg id))
		     (else name))))))
      
      (define (jigloo file x mod)
	 (module5-extern-plugin-preprocessor "jigloo" file x mod))

      (match-case clause
	 ((export (? symbol?) (? string?))
	  (java-parser clause (-> mod id) '|.|))
	 ((export (and ?id (? symbol?)))
	  (java-parser (localize clause
			  `(export ,id ,(symbol->string id)))
	     (-> mod id) '|.|))
	 ((or (class ?ident . ?rest)
	      (abstract-class ?ident . ?rest))
	  (multiple-value-bind (cpkg name id super)
	     (parse-class5-ident ident)
	     ;;(unless (java-type-exists? (symbol->string! id) mod)
	     (begin
		(let* ((clazz (class5->class4 clause cpkg name id super rest))
		       (jklass::jklass (java-parser clazz (-> mod id) '|.|)))
		   (trace-item "ident=" ident)
		   (trace-item "id=" id)
		   (trace-item "super=" super)
		   (trace-item "name=" name)
		   (trace-item "class5="
		      (if (>fx (length clazz) 5)
			  (append (take clazz 5) '("..."))
			  clazz))
		   (declare-java-jklass! jklass mod clause)
		   (set! (-> jklass delayed-accessors?) #f)
		   (declare-java-predicate! (-> jklass idd) mod clause)
		   (declare-jklass-constructors! jklass mod clause)
		   (declare-jklass-methods! jklass mod clause)
		   (declare-jklass-fields! jklass mod clause)))))
	 ((array (and (? symbol?) ?ident) (and (? symbol?) ?of))
	  (unless (java-type-exists? (symbol->string! ident) mod)
	     (declare-java-jarray! ident mod clause)
	     (java-declare-array clause ident of (-> mod id) #f)
	     (declare-java-predicate! ident mod clause)
	     (declare-jarray-make ident of mod clause)
	     (declare-jarray-length ident of mod clause)
	     (declare-jarray-accessors ident of mod clause)))
	 ((import (and ?class (? symbol?)))
	  (module5-extern-plugin-java mod
	     (call-with-input-file (jigloo (symbol->string class) clause mod)
		read)))
	 (else
	  (error/loc mod "Illegal extern \"java\" module clause" clause x))))
   
   (with-trace 'module_extern-java "module5-extern-plugin-java"
      (trace-item "module=" (-> mod id))
      (when (memq 'java (backend-foreign-clause-support (the-backend)))
	 (match-case (cddr x)
	    (((package (and (? symbol?) ?pkg)) . ?other-clauses)
	     (let ((qn (string->symbol
			  (format "~a.~a" pkg (prefix (basename (-> mod path)))))))
		(jvm-qualified-name-set! (-> mod id) qn)
		(set! (-> mod qualified-name) qn))
	     (for-each (lambda (c) (parse-clause c mod x pkg)) other-clauses))
	    (else
	     (for-each (lambda (c) (parse-clause c mod x #f)) (cddr x)))))
      '()))

;*---------------------------------------------------------------------*/
;*    declare-java-jklass! ...                                         */
;*    -------------------------------------------------------------    */
;*    Qualified type name are handled in the Java finalizer so         */
;*    declare-java-type! does not need to deal with type aliasing.     */
;*---------------------------------------------------------------------*/
(define (declare-java-jklass! j::jklass mod::Module clause)
   (with-trace 'module_extern-java "declare-java-jklass!"
      (with-access::jklass j (id jname package src)
	 (trace-item "jklass=" id)
	 (trace-item "mod=" (-> mod id))
	 (trace-item "pkg=" package)
	 (multiple-value-bind (clazz super)
	    (parse-ident id)
	    (co-instantiate
		  ((def (instantiate::JDef
			   (id clazz)
			   (kind 'jvm-type)
			   (ronly #t)
			   (expr src)
			   (decl decl)
			   (name jname)
			   (package (if (string? package) package (jname-package jname ".")))
			   (super (if (string? super) (string->symbol super) '_))))
		   (decl (instantiate::Decl
			    (id clazz)
			    (alias clazz)
			    (mod mod)
			    (expr clause)
			    (ronly #t)
			    (scope 'extern)
			    (def def))))
	       (with-access::Module mod (decls defs exports)
		  (let ((name (symbol->string! clazz)))
		     (hashtable-put! exports name decl)
		     (hashtable-put! decls name decl)
		     (hashtable-put! defs name def))))))))

;*---------------------------------------------------------------------*/
;*    declare-java-jarray! ...                                         */
;*---------------------------------------------------------------------*/
(define (declare-java-jarray! id::symbol mod::Module clause)
   (co-instantiate
	 ((def (instantiate::ADef
		  (id id)
		  (kind 'jvm-array)
		  (expr clause)
		  (ronly #t)
		  (decl decl)))
	  (decl (instantiate::Decl
		   (id id)
		   (alias id)
		   (mod mod)
		   (expr clause)
		   (ronly #t)
		   (scope 'extern)
		   (def def))))
      (with-access::Module mod (decls defs exports)
	 (let ((name (symbol->string! id)))
	    (hashtable-put! decls name decl)
	    (hashtable-put! defs name def)))))

;*---------------------------------------------------------------------*/
;*    java-type-exists? ...                                            */
;*---------------------------------------------------------------------*/
(define (java-type-exists? name::bstring mod::Module)
   (with-access::Module mod (defs)
      (hashtable-get defs name)))

;*---------------------------------------------------------------------*/
;*    declare-java-predicate! ...                                      */
;*---------------------------------------------------------------------*/
(define (declare-java-predicate! id::symbol mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-java-predicate!"
      (trace-item "id=" id)
      (let* ((pid (symbol-append id '?))
	     (pidt (symbol-append id '?::bool))
	     (obj (mark-symbol-non-user! (gensym 'obj)))
	     (tobj (symbol-append obj '::obj))
	     (expr (localize clause
		      `(define-inline (,pidt ,tobj)
			  ,(make-private-sexp 'instanceof id obj))))
	     (attrs `((pragma ((predicate-of ,id))) (removable 'coerce)))
	     (alias (string->symbol (format "__~a.~a" (-> mod id) pid)))
	     (decl (instantiate::Decl
		      (id pid)
		      (alias alias)
		      (xid alias)
		      (mod mod)
		      (expr expr)
		      (ronly #t)
		      (attributes attrs)
		      (scope 'export))))
	 (with-access::Module mod (decls defs exports body)
	    (set! body (cons expr body))
	    (let ((iname (symbol->string! pid))
		  (aname (symbol->string! alias)))
	       (hashtable-put! decls iname decl)
	       (hashtable-put! exports aname decl))))))

;*---------------------------------------------------------------------*/
;*    declare-jklass-constructors! ...                                 */
;*---------------------------------------------------------------------*/
(define (declare-jklass-constructors! class::jklass mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-jklass-constructors!"
      (trace-item "jklass=" (-> class id))

      (define (declare-ctor! m::jmethod)
	 (when (isa? m jconstructor)
	    (multiple-value-bind (mid _)
	       (parse-ident (-> m id))
	       (let* ((id (-> class idd))
		      (types (map (lambda (t)
				     (multiple-value-bind (_ ty)
					(parse-ident t)
					(string->symbol ty)))
				(-> m args)))
		      (args (map (lambda (t) (gensym 'a)) (-> m args)))
		      (targs (map symbol-append args (-> m args)))
		      (expr (localize clause
			       `(define-inline (,(-> m id) ,@targs)
				   ,(apply make-private-sexp 'new id
				       `',types args))))
		      (alias (string->symbol (format "__~a.~a" (-> mod id) mid)))
		      (decl (instantiate::Decl
			       (id mid)
			       (alias alias)
			       (xid alias)
			       (mod mod)
			       (expr expr)
			       (ronly #t)
			       (scope 'export))))
		  (trace-item "ctor=" (-> m id))
		  (with-access::Module mod (decls exports body)
		     (set! body (cons expr body))
		     (let ((iname (symbol->string! mid))
			   (aname (symbol->string! alias)))
			(hashtable-put! decls iname decl)
			(hashtable-put! exports aname decl)))))))

      (with-trace 'module_exter-jvm "declare-jklass-constructors"
	 (trace-item "jklass=" (-> class id))
	 (trace-item "mod=" (-> mod id))
	 (for-each declare-ctor! (-> class methods)))))
   
;*---------------------------------------------------------------------*/
;*    declare-jklass-methods! ...                                      */
;*---------------------------------------------------------------------*/
(define (declare-jklass-methods! class::jklass mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-jklass-methods!"
      (trace-item "jklass=" (-> class id))
      
      (define (declare-method! m::jmethod)
	 (unless (isa? m jconstructor)
	    (multiple-value-bind (mid mty)
	       (parse-ident (-> m id))
	       (let* ((id (-> class idd))
		      (types (map (lambda (t)
				     (multiple-value-bind (_ ty)
					(parse-ident t)
					(string->symbol ty)))
				(-> m args)))
		      (args (map (lambda (t) (gensym 'a)) (-> m args)))
		      (targs (map symbol-append args (-> m args))))
		  (co-instantiate
			((def (instantiate::CDef
				 (id mid)
				 (type (string->symbol mty))
				 (kind 'jvm-method)
				 (expr clause)
				 (ronly #t)
				 (decl decl)
				 (args types)
				 (name (-> m jname))
				 (macro #f)
				 (infix #f)
				 (modifiers (-> m modifiers))
				 (module (string->symbol (-> class jname)))))
			 (decl (instantiate::Decl
				  (id mid)
				  (alias mid)
				  (mod mod)
				  (expr clause)
				  (ronly #t)
				  (scope 'extern)
				  (def def))))
		     (with-access::Module mod (decls defs exports)
			(let ((name (symbol->string! mid)))
			   (hashtable-put! exports name decl)
			   (hashtable-put! decls name decl)
			   (hashtable-put! defs name def)))
		     (when (= (-> mod version) 4)
			'todo))))))
      
      (for-each declare-method! (-> class methods))))
   
;*---------------------------------------------------------------------*/
;*    declare-jklass-fields! ...                                       */
;*---------------------------------------------------------------------*/
(define (declare-jklass-fields! class::jklass mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-jklass-fields!"
      (trace-item "jklass=" (-> class id))
      
      (define (declare-field! f::jfield)
	 (when (memq 'static (-> f modifiers))
	    (multiple-value-bind (fid mty)
	       (parse-ident (-> f id))
	       (if (eq? mty #unspecified)
		   (error/loc mod "Missing field type" (-> f id) clause)
		   (let* ((id (-> class idd))
			  (sid (string->symbol (format "~a.~a" id fid))))
		      (co-instantiate
			    ((def (instantiate::CDef
				     (id sid)
				     (type (string->symbol mty))
				     (kind 'jvm-variable)
				     (expr clause)
				     (ronly #t)
				     (decl decl)
				     (name (-> f jname))
				     (args '())
				     (modifiers (-> f modifiers))
				     (module (string->symbol (-> class jname)))))
			     (decl (instantiate::Decl
				      (id sid)
				      (alias sid)
				      (mod mod)
				      (expr clause)
				      (ronly #t)
				      (scope 'extern)
				      (def def))))
			 (with-access::Module mod (decls defs exports)
			    (let ((name (symbol->string! sid)))
			       (hashtable-put! exports name decl)
			       (hashtable-put! decls name decl)
			       (hashtable-put! defs name def)))
			 (when (= (-> mod version) 4)
			    'todo)))))))
      
      (for-each declare-field! (-> class fields))))

;*---------------------------------------------------------------------*/
;*    declare-jarray-make ...                                          */
;*---------------------------------------------------------------------*/
(define (declare-jarray-make id::symbol of::symbol mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-jarray-constructor"
      (trace-item "array=" id)
      (multiple-value-bind (_ iname)
	 (parse-ident of)
	 (let* ((iid (string->symbol iname))
		(lid (make-typed-ident 'len 'int))
		(cid (symbol-append 'make- id))
		(tid (make-typed-ident cid id))
		(expr (localize clause
			 `(define-inline (,tid ,lid)
			     ,(make-private-sexp 'valloc id iid 'int
				 "" ""
				 #f 'len))))
		(decl (instantiate::Decl
			 (id cid)
			 (alias cid)
			 (mod mod)
			 (expr expr)
			 (ronly #t)
			 (attributes '(extern))
			 (scope 'static))))
	    (with-access::Module mod (decls exports body)
	       (set! body (cons expr body))
	       (let ((name (symbol->string! cid)))
		  ;;(hashtable-put! exports name decl)
		  (hashtable-put! decls name decl)))))))
      
;*---------------------------------------------------------------------*/
;*    declare-jarray-length ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-jarray-length id::symbol of::symbol mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-jarray-length"
      (trace-item "array=" id)
      (multiple-value-bind (_ iname)
	 (parse-ident of)
	 (let* ((iid (string->symbol iname))
		(oid (make-typed-ident 'o id))
		(lid (symbol-append id '-length))
		(tid (make-typed-ident lid 'int))
		(expr (localize clause
			 `(define-inline (,tid ,oid)
			     ,(make-private-sexp 'vlength id iid 'int "" 'o))))
		(decl (instantiate::Decl
			 (id lid)
			 (alias lid)
			 (mod mod)
			 (expr expr)
			 (ronly #t)
			 (attributes '(extern))
			 (scope 'static))))
	    (with-access::Module mod (decls exports body)
	       (set! body (cons expr body))
	       (let ((name (symbol->string! lid)))
		  ;;(hashtable-put! exports name decl)
		  (hashtable-put! decls name decl)))))))

;*---------------------------------------------------------------------*/
;*    declare-jarray-accessors ...                                     */
;*---------------------------------------------------------------------*/
(define (declare-jarray-accessors id::symbol of::symbol mod::Module clause::pair)
   (with-trace 'module_extern-java "declare-jarray-accessors"
      (trace-item "array=" id)
      (multiple-value-bind (_ iname)
	 (parse-ident of)
	 (let* ((iid (string->symbol iname))
		(oid (make-typed-ident 'o 'int))
		(aid (make-typed-ident 'a id))
		(vid (make-typed-ident 'v iid))
		(gid (symbol-append id '-ref))
		(sid (symbol-append id '-set!))
		(gtid (make-typed-ident gid iid))
		(gexpr (localize clause
			  `(define-inline (,gtid ,aid ,oid)
			      ,(make-private-sexp 'vref id iid 'int
				 "" 'a 'o))))
		(sexpr (localize clause
			  `(define-inline (,sid ,aid ,oid ,vid)
			      ,(make-private-sexp 'vset! id iid 'int
				  "" 'a 'o 'v))))
		(gdecl (instantiate::Decl
			  (id gid)
			  (alias gid)
			  (mod mod)
			  (expr gexpr)
			  (ronly #t)
			  (attributes '(extern))
			  (scope 'static)))
		(sdecl (instantiate::Decl
			  (id sid)
			  (alias sid)
			  (mod mod)
			  (expr sexpr)
			  (ronly #t)
			  (attributes '(extern))
			  (scope 'static))))
	    (with-access::Module mod (decls exports body)
	       (set! body (cons* gexpr sexpr body))
	       (let ((name (symbol->string! gid)))
		  ;;(hashtable-put! exports name gdecl)
		  (hashtable-put! decls name gdecl))
	       (let ((name (symbol->string! sid)))
		  ;;(hashtable-put! exports name sdecl)
		  (hashtable-put! decls name sdecl)))))))
