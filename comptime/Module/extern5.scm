;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0.x/comptime/Module/extern5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Thu Jun 11 08:51:54 2026                          */
;*    Last change :  Sat Jun 13 06:10:20 2026 (serrano)                */
;*    Copyright   :  2026 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    Module5 extern plugins                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_extern5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
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

   (export (module5-extern-plugin-c ::Module ::pair)
	   (module5-extern-plugin-java ::Module ::pair)
	   (module5-extern-plugin-java-finalizer ::Module)
	   (module5-extern-plugin-wasm ::Module ::pair)
	   (module4-extern-plugin-c ::Module ::pair)
	   (module4-extern-plugin-java ::Module ::pair))

   (export (class CDef::Def
	      (args read-only)
	      (name::bstring read-only)
	      (infix::bool read-only (default #f))
	      (macro::bool read-only (default #f))
	      (module::symbol read-only)
	      (modifiers::pair-nil read-only (default '())))

	   (class TDef::Def
	      (name::bstring read-only))

	   (class JDef::TDef
	      (super::obj read-only)
	      (package::bstring read-only))))

;*---------------------------------------------------------------------*/
;*    object-copy ::CDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::CDef)
   (duplicate::CDef d))

;*---------------------------------------------------------------------*/
;*    object-copy ::TDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::TDef)
   (duplicate::TDef d))

;*---------------------------------------------------------------------*/
;*    object-copy ::JDef ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-copy d::JDef)
   (duplicate::JDef d))

;*---------------------------------------------------------------------*/
;*    parse-extern-c-clause ...                                        */
;*---------------------------------------------------------------------*/
(define (parse-extern-c-clause version::long clause mod::Module x::pair)
   
   (define (parse-include string clause mod::Module)
      (unless (member string *include-foreign*)
	 (set! *include-foreign* (cons string *include-foreign*))))
   
   (define (illegal-args args src mod)
      (let loop ((args args))
	 (cond
	    ((null? args)
	     #f)
	    ((symbol? args)
	     (multiple-value-bind (id type)
		(parse-ident args)
		(unless (string? type)
		   args)))
	    ((not (pair? args))
	     args)
	    ((not (symbol? (car args)))
	     args)
	    (else
	     (multiple-value-bind (id type)
		(parse-ident (car args))
		(if (string? type)
		    (loop (cdr args))
		    args))))))
   
   (define (parse-function macro infix ident args name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident)
	 (cond
	    ((not (string? type))
	     (error/loc mod "Missing C type" ident clause))
	    ((illegal-args args clause mod)
	     =>
	     (lambda (args) (error/loc mod "Illegal C args" args clause)))
	    (else
	     (co-instantiate
		   ((def (instantiate::CDef
			    (id id)
			    (type (string->symbol type))
			    (kind 'c-function)
			    (expr clause)
			    (ronly #t)
			    (decl decl)
			    (args args)
			    (name name)
			    (macro macro)
			    (infix infix)
			    (module 'foreign)))
		    (decl (instantiate::Decl
			     (id id)
			     (alias id)
			     (mod mod)
			     (expr clause)
			     (ronly #t)
			     (scope 'extern)
			     (def def))))
		(with-access::Module mod (decls defs exports)
		   (hashtable-put! exports (symbol->string! id) decl)
		   (hashtable-put! decls (symbol->string! id) decl)
		   (hashtable-put! defs (symbol->string! id) def)))))))
   
   (define (parse-variable macro ident name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident)
	 (cond
	    ((not (string? type))
	     (error/loc mod "Missing C type" ident clause))
	    (else
	     (co-instantiate
		   ((def (instantiate::CDef
			    (id id)
			    (type (string->symbol type))
			    (kind 'c-variable)
			    (expr clause)
			    (ronly #f)
			    (decl decl)
			    (args '())
			    (name name)
			    (macro macro)
			    (infix #f)
			    (module 'foreign)))
		    (decl (instantiate::Decl
			     (id id)
			     (alias id)
			     (mod mod)
			     (expr clause)
			     (ronly #f)
			     (scope 'extern)
			     (def def))))
		(with-access::Module mod (decls defs exports)
		   (hashtable-put! exports (symbol->string! id) decl)
		   (hashtable-put! decls (symbol->string! id) decl)
		   (hashtable-put! defs (symbol->string! id) def)))))))
   
   (define (parse-type id name clause mod::Module)
      (co-instantiate
	    ((def (instantiate::TDef
		     (id id)
		     (kind 'c-type)
		     (expr clause)
		     (ronly #t)
		     (expr clause)
		     (decl decl)
		     (name name)))
	     (decl (instantiate::Decl
		      (id id)
		      (alias id)
		      (mod mod)
		      (expr clause)
		      (ronly #t)
		      (scope 'extern)
		      (def def))))
	 (with-access::Module mod (decls defs exports)
	    (hashtable-put! exports (symbol->string! id) decl)
	    (hashtable-put! decls (symbol->string! id) decl)
	    (hashtable-put! defs (symbol->string! id) def))
	 (parse-c-foreign-type clause)))

   (define (cigloo file x mod)
      (module5-extern-plugin-preprocessor "cigloo" file x mod))
   
   (define (parse-args id::symbol args::pair-nil mod clause x)
      (cond
	 ((null? args)
	  (values '() (symbol->name id clause mod)))
	 ((not (list? args))
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))
	 ((string? (car (last-pair args)))
	  (let* ((name (car (last-pair args)))
		 (args (drop-last args 1)))
	     (if (every symbol? args)
		 (values args name)
		 (error/loc mod "Illegal extern \"C\" module clause" clause x))))
	 (else
	  (values args (symbol->name id clause mod)))))

   (define (symbol->name ident::symbol src mod)
      (multiple-value-bind (id type)
	 (parse-ident ident)
	 (symbol->string id)))

   (define (parse5 clause)
      (with-trace 'module_module5 "parse-extern-c-clause"
	 (trace-item "clause=" clause)
	 (match-case clause
	    ((include (and (? string?) ?string))
	     (parse-include string clause mod))
	    ((import (and (? string?) ?string))
	     (module5-extern-plugin-c mod
		(localize clause `(include ,string)))
	     (module5-extern-plugin-c mod
		(call-with-input-file (cigloo string clause mod)
		   read)))
	    ((export . ?-)
	     (parse-c-foreign-export clause #t))
	    ((type (and (? symbol?) ?id))
	     (parse-type id (symbol->string id) clause mod))
	    ((type (and (? symbol?) ?id) (and (? string?) ?name))
	     (parse-type id name clause mod))
	    ((macro (and (? symbol?) ?ident) . ?args)
	     (multiple-value-bind (args name)
		(parse-args ident args mod clause x)
		(parse-function #t #f ident args name clause mod)))
	    ((infix macro (and (? symbol?) ?ident) . ?args)
	     (multiple-value-bind (args name)
		(parse-args ident args mod clause x)
		(parse-function #t #t ident args name clause mod)))
	    ((cnst macro (and (? symbol?) ?ident))
	     (parse-variable #t ident (symbol->name ident clause mod) clause mod))
	    ((cnst macro (and (? symbol?) ?ident) (and (? string?) ?name))
	     (parse-variable #t ident name clause mod))
	    ((variable (and (? symbol?) ?ident))
	     (parse-variable #f ident (symbol->name ident x mod) clause mod))
	    ((variable (and (? symbol?) ?ident) (and (? string?) ?name))
	     (parse-variable #f ident name clause mod))
	    (((and (? symbol?) ?ident) . ?args)
	     (multiple-value-bind (args name)
		(parse-args ident args mod clause x)
		(parse-function #f #f ident args name clause mod)))
	    ((and (? symbol?) ?ident)
	     (parse-variable #f ident (symbol->name ident x mod) clause mod))
	    (else
	     (error/loc mod "Illegal extern \"C\" module clause" clause x)))))

   (define (parse4 clause)
      (match-case clause
	 ((include (and (? string?) ?string))
	  (parse-include string clause mod))
	 ((type (and (? symbol?) ?id) (and (? string?) ?name))
	  (parse-type id name clause mod))
	 ((type (and (? symbol?) ?id) ?- (and (? string?) ?name))
	  (parse-type id name clause mod))
	 ((macro (and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #t #f ident args name clause mod))
	 ((infix macro (and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #t #t ident args name clause mod))
	 ((macro (and (? symbol?) ?ident) (and (? string?) ?name))
	  (parse-variable #t ident name clause mod))
	 ((export . ?-)
	  (parse-c-foreign-export clause #t))
	 (((and (? symbol?) ?ident) ?args (and (? string?) ?name))
	  (parse-function #f #f ident args name clause mod))
	 (((and (? symbol?) ?ident) (and (? string?) ?name))
	  (parse-variable #f ident name clause mod))
	 (else
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))))

   (if (=fx version 5)
       (parse5 clause)
       (parse4 clause)))
   
;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-c mod::Module x::pair)
   (with-trace 'module_module5 "module5-extern-plugin-c"
      (trace-item "x=" x)
      (when (memq 'extern (backend-foreign-clause-support (the-backend)))
	 (for-each (lambda (c) (parse-extern-c-clause 5 c mod x)) (cddr x)))
      '()))

;*---------------------------------------------------------------------*/
;*    module4-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module4-extern-plugin-c mod::Module x::pair)
   (when (memq 'extern (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-extern-c-clause 4 c mod x)) (cdr x)))
   '())

;*---------------------------------------------------------------------*/
;*    declare-java-type! ...                                           */
;*    -------------------------------------------------------------    */
;*    Qualified type name are handled in the Java finalizer so         */
;*    declare-java-type! does not need to deal with type aliasing.     */
;*---------------------------------------------------------------------*/
(define (declare-java-type! j::jklass mod::Module clause)
   (with-trace 'module_module5 "declare-java-type"
      (with-access::jklass j (id jname package src)
	 (trace-item "jklass=" id)
	 (trace-item "mod=" (-> mod id))
	 (trace-item "pkg=" package)
	 (multiple-value-bind (clazz super)
	    (parse-ident id)
	    (co-instantiate
		  ((def (instantiate::JDef
			   (id clazz)
			   (kind 'java-type)
			   (expr clause)
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
	     (declare-java-type! jklass mod clause)
	     (with-access::jklass jklass (delayed-accessors?)
		(set! delayed-accessors? #f)
		(declare-jklass-predicate! jklass mod clause))))
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
		(declare-java-type! jklass mod clause)
		(set! (-> jklass delayed-accessors?) #f)
		(declare-jklass-predicate! jklass mod clause)
		(declare-jklass-constructors! jklass mod clause)
		(declare-jklass-methods! jklass mod clause)
		(declare-jklass-fields! jklass mod clause))))
	 ((array (and (? symbol?) ?ident) (and (? symbol?) ?of))
 	  (java-declare-array clause ident of (-> mod id)))
	 ((import (and ?class (? symbol?)))
	  (module5-extern-plugin-java mod
	     (call-with-input-file (jigloo (symbol->string class) clause mod)
		read)))
	 (else
	  (error/loc mod "Illegal extern \"java\" module clause" clause x))))
   
   (with-trace 'module_module5 "module5-extern-plugin-java"
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
;*    declare-jklass-predicate! ...                                    */
;*---------------------------------------------------------------------*/
(define (declare-jklass-predicate! class::jklass mod::Module clause::pair)
   (with-trace 'module_extern5 "declare-jklass-predicate!"
      (trace-item "jklass=" (-> class id))
      (let* ((id (-> class idd))
	     (pid (symbol-append id '?))
	     (pidt (symbol-append id '?::bool))
	     (obj (mark-symbol-non-user! (gensym 'obj)))
	     (expr (localize clause
		      `(define-inline (,pidt ,obj)
			  ,(make-private-sexp 'instanceof id obj))))
	     (attrs `((pragma ((predicate-of ,id))) (removable 'coerce)))
	     (decl (instantiate::Decl
		      (id pid)
		      (alias pid)
		      (mod mod)
		      (expr clause)
		      (ronly #t)
		      (attributes attrs)
		      (attributes '(extern))
		      (scope 'export))))
	 (with-access::Module mod (decls defs exports body)
	    (set! body (cons expr body))
	    (let ((name (symbol->string! pid)))
	       (hashtable-put! exports name decl)
	       (hashtable-put! decls name decl))))))

;*---------------------------------------------------------------------*/
;*    declare-jklass-constructors! ...                                 */
;*---------------------------------------------------------------------*/
(define (declare-jklass-constructors! class::jklass mod::Module clause::pair)
   (with-trace 'module_extern5 "declare-jklass-constructors!"
      (trace-item "jklass=" (-> class id))

      (define (declare-method! m::jmethod)
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
		      (decl (instantiate::Decl
			       (id mid)
			       (alias mid)
			       (mod mod)
			       (expr clause)
			       (ronly #t)
			       (attributes '(extern))
			       (scope 'export))))
		  (with-access::Module mod (decls exports body)
		     (set! body (cons expr body))
		     (let ((name (symbol->string! mid)))
			 (hashtable-put! exports name decl)
			 (hashtable-put! decls name decl)))))))
      
      (for-each declare-method! (-> class methods))))
   
;*---------------------------------------------------------------------*/
;*    declare-jklass-methods! ...                                      */
;*---------------------------------------------------------------------*/
(define (declare-jklass-methods! class::jklass mod::Module clause::pair)
   (with-trace 'module_extern5 "declare-jklass-methods!"
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
   (with-trace 'module_extern5 "declare-jklass-fields!"
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
;*    module5-extern-plugin-preprocessor ...                           */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-preprocessor cmd::bstring file::bstring x mod::Module)
   (with-trace 'module_module5 "module5-extern-plugin-preprocessor"
      (trace-item "cmd=" cmd)
      (trace-item "file=" file)
      (let ((path (if (file-name-absolute? file)
		      file
		      (make-file-name (dirname (-> mod path)) file))))
	 (trace-item "path=" path)
	 (let* ((cache-dir (make-file-path *module-cache-dir* "preprocessor"))
		(lock-path (make-file-name cache-dir "LOCK"))
		(cache (make-file-name cache-dir
			  (string-append (string-replace file #\/ #\_)
			     ".bgh"))))
	    (trace-item "cache=" cache)
	    (make-directories cache-dir)
	    (unless (directory? cache-dir)
	       (error/loc mod "Cannot create cache directory"
		  cache-dir x))
	    (call-with-output-file lock-path
	       (lambda (lock)
		  (lockf lock 'lock)
		  (unwind-protect
		     (if (or (not (file-exists? cache))
			     (and (file-exists? path)
				  (<elong (file-modification-time cache)
				     (file-modification-time path))))
			 (let ((cmd (format "~a/~a -cp ~a -s --module5 ~a -o ~a"
				       (bigloo-config 'binary-directory)
				       cmd
				       (dirname (-> mod path))
				       (if (file-exists? path) path file)
				       cache)))
			    (trace-item "cmd=" cmd)
			    (if (=fx (system cmd) 0)
				cache
				(begin
				   (when (file-exists? cache)
				      (delete-file cache))
				   (error/loc mod
				      (format "~a Cannot preprocess" cmd)
				      file x))))
			 cache)
		     (lockf lock 'ulock))))))))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-java-finalizer ...                         */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-java-finalizer mod::Module)
   ;; export global variables to java
   (java-finalizer-exports)
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
;*    module5-extern-plugin-wasm ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-wasm mod::Module expr::pair)
   
   (define (parse-clause clause mod::Module)
      (match-case clause
	 (((and (? symbol?) ?ident) (and (? string?) ?name) . ?deps)
	  (multiple-value-bind (id type)
	     (parse-ident ident)
	     (let ((decl (hashtable-get (-> mod decls) (symbol->string! id))))
		(if (isa? decl Decl)
		    (with-access::Decl decl (attributes)
		       (if (pair? deps)
			   (set! attributes
			      (cons* (cons 'wasm deps)
				 ;(cons 'qualified-type-name name)
				 attributes))
			   '(set! attributes
			      (cons (cons 'qualified-type-name name)
				 attributes))))
		    (error/loc "mod" "Cannot find declaration" clause expr)))))
	 (((and (? symbol?) ?id) ?proto (and (? string?) ?cn))
	  (extern-parser clause #f))
	 (else
	  (error/loc mod "Illegal extern \"wasm\" module clause" clause expr))))
   
   (when (memq 'wasm (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))
   '())

;*---------------------------------------------------------------------*/
;*    module5-export-global! ...                                       */
;*---------------------------------------------------------------------*/
(define (module5-export-global! mod::Module g::global kind::symbol decl-expr def-expr)
   (with-access::global g (id value)
      (co-instantiate
	    ((def (instantiate::Def
		     (id id)
		     (type (string->symbol type))
		     (kind kind)
		     (expr def-expr)
		     (ronly #t)
		     (decl decl)))
	     (decl (instantiate::Decl
		      (id id)
		      (alias id)
		      (mod mod)
		      (expr decl-expr)
		      (ronly #t)
		      (scope 'export)
		      (def def))))
	 (with-access::Module mod (decls defs exports)
	    (let ((name (symbol->string! id)))
	       (hashtable-put! exports name decl)
	       (hashtable-put! decls name decl)
	       (hashtable-put! defs name def))))))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id::symbol)
   (let* ((s (symbol->string id))
	  (l (string-length s)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i (-fx l 2))
	     (values id #unspecified))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (values (string->symbol (substring s 0 i))
		    (substring s (+fx i 2)))
		 (loop (+fx i 1))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc mod msg obj container)
   (let ((id (if (isa? mod Module)
		 (with-access::Module mod (id) id)
		 "module5")))
      (match-case (cond
		   ((epair? obj) (cer obj))
		   ((epair? container) (cer container))
		   (else #f))
	 ((at ?fname ?loc) (error/location id msg obj fname loc))
	 (else (error id msg obj)))))

