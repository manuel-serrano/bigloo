;*=====================================================================*/
;*    serrano/bigloo/5.0a/comptime/Module/module5.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Wed Feb  4 12:03:35 2026 (serrano)                */
;*    Copyright   :  2025-26 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    Compilation of the a Module5 clause.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_module5
   
   (include "Ast/unit.sch"
	    "Engine/pass.sch")
   
   (import engine_param
	   tools_error
	   tools_shape
 	   tools_location
	   read_jvm
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
	   ast_node
	   ast_var
	   ast_env
	   ast_glo-decl
	   ast_ident
	   ast_toplevel
	   ast_build
	   ast_sexp
	   ast_private
	   type_type
	   type_env
	   object_class
	   object_slots
	   object_coercion)

   (export (module5-expand ::pair-nil)
	   (module5-import-def ::Module ::Decl)
	   (module5-ast! ::Module ::obj ::symbol)
	   (module5-main ::Module ::obj)
	   (module5-imported-unit ::Module ::procedure ::obj)
	   (module5-object-unit ::Module)
	   (module5-imported-inline mod::Module ::obj)
	   (module5-extern-plugin-c ::Module ::pair)
	   (module5-extern-plugin-java ::Module ::pair)
	   (module5-extern-plugin-wasm ::Module ::pair)
	   (module5-plugin-pragma ::Module ::pair)
	   (module5-plugin-eval ::Module ::pair)
	   (module4-extern-plugin-c ::Module ::pair)
	   (module4-extern-plugin-java ::Module ::pair)
	   (module4-plugin-eval ::Module ::pair)
	   (module4-plugin-type ::Module ::pair)
	   (module5-resolve-pragma! ::Module ::obj)
	   (module5-heap4-modules::pair-nil)
	   (module5-init-xenv! xenv ::Module))

   (export (class CDef::Def
	      (args read-only)
	      (name::bstring read-only)
	      (infix::bool read-only)
	      (macro::bool read-only))

	   (class TDef::Def
	      (name::bstring read-only))))

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
;*    module5-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (module5-expand x)
   (expand-compile-cond-expand x (lambda (x e) x)))

;*---------------------------------------------------------------------*/
;*    module5-import-def ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-import-def mod::Module decl::Decl)
   (with-trace 'module5-resolve "modulet5-import-def"
      (trace-item "id=" (-> decl id))
      (with-access::Decl decl ((dmod mod) def xid id)
	 (if (eq? mod dmod)
	     def
	     (module5-get-export-def dmod (or xid id))))))

;*---------------------------------------------------------------------*/
;*    module5-ast! ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-ast! mod::Module env mode::symbol)
   
   (define unsorted-classes '())
   
   (define (procedure-args src id mid)
      (match-case src
	 ((define (?- . ?args) . ?-) args)
	 ((define-inline (?- . ?args) . ?-) args)
	 ((define-method (?- . ?args) . ?-) args)
	 ((define-generic (?- . ?args) . ?-) args)
	 ((define (and (? symbol?)) (lambda ?args . ?-)) args)
	 (else (error mid (format "Illegal procedure expression \"~a\"" id) src))))
   
   (define (import-kind src ronly)
      (if (not ronly)
	  'variable
	  (match-case src
	     ((define (?- . ?args) . ?-) 'procedure)
	     ((define-inline (?- . ?args) . ?-) 'inline)
	     ((define-generic (?- . ?args) . ?-) 'generic)
	     ((define (and (? symbol?)) (lambda ?args . ?-)) 'procedure)
	     ((define-macro (?- . ?args) . ?-) 'macro)
	     ((define-expander (?- . ?args) . ?-) 'expander)
	     ((define-class . ?-) 'class)
	     (else 'variable))))
   
   (define (make-class-slot p i ty expr)
      (let ((id (cdr (assq 'id p))))
	 (instantiate::slot
	    (id id)
	    (index i)
	    (name (id->name id))
	    (src (cdr (assq 'expr p)))
	    (class-owner ty)
	    (user-info #f)
	    (type (find-type/expr (cdr (assq 'type p)) expr)))))

   (define (type-class-module id)
      (let ((old (find-type id)))
	 (when (isa? old tclass)
	    (with-access::tclass old (holder)
	       (global-module holder)))))

   (define (declare-class-definition! id alias mid scope src def::KDef)
      (with-trace 'module5 "declare-class-definition!"
	 (trace-item "id=" id)
	 (with-access::KDef def (expr id decl super ctor kkind properties)
	    (when (isa? decl Decl)
	       (with-access::Decl decl (mod)
		  (with-access::Module mod ((mid id))
		     (unless (find-global/module env id mid)
			;; a class declared in the module being compiled
			(let ((var (declare-global-svar! env id id mid scope expr expr)))
			   (global-type-set! var (find-type/expr 'class expr))
			   (global-set-read-only! var)
			   (cond
			      ((not (type-exists? id))
			       (let* ((sup (and super (find-type/expr super expr)))
				      (ty (declare-class-type! id sup
					     ctor var #f
					     (eq? kkind 'define-final-class)
					     (eq? kkind 'define-abstract-class)
					     src)))
				  (gen-class-coercions! ty)
				  ty))
			      ((not (eq? (type-class-module id) mid))
			       (error mid
				  (format "Illegal type redefinition \"~a\"" id)
				  src))
			      (else
			       #f))))))))))

   (define (declare-class-slots! id alias src def::KDef ty::tclass)
      (with-access::KDef def (properties)
	 (let* ((sup (tclass-its-super ty))
		(sslots (if sup (tclass-slots sup) '()))
		(nslots (map (lambda (p i)
				(make-class-slot p i ty src))
			   properties
			   (iota (length properties) (length sslots)))))
	    (tclass-slots-set! ty (append sslots nslots)))))

   (define (make-typed-ident id type)
      (if (string? type)
	  (string->symbol (format "~a::~a" id type))
	  id))
      
   (define (declare-definition! kind id type alias mid scope expr def::Def)
      (with-trace 'module5 "declare-definition!"
	 (trace-item "id=" id)
	 (trace-item "kind=" kind)
	 (case kind
	    ((variable)
	     (declare-global-svar! env (make-typed-ident id type) alias
		mid scope expr expr))
	    ((procedure)
	     (declare-global-sfun! env (make-typed-ident id type) alias
		(procedure-args expr id mid)
		mid scope 'sfun expr expr))
	    ((inline)
	     (declare-global-sfun! env (make-typed-ident id type) alias
		(procedure-args expr id mid)
		mid scope 'sifun expr expr))
	    ((generic)
	     (declare-global-sfun! env (make-typed-ident id type) alias
		(procedure-args expr id mid)
		mid scope 'sgfun expr expr))
	    ((macro)
	     (with-access::Def def (expr)
		(add-macro-definition! expr id)))
	    ((expander)
	     (with-access::Def def (expr)
		(add-macro-definition! expr id)))
	    ((c-function)
	     (with-access::CDef def (name type infix args macro)
		(declare-global-cfun! env id alias 'foreign name type args
		   #f macro expr expr)))
	    ((c-variable)
	     (with-access::CDef def (name type macro)
		(declare-global-cvar! env id alias 'foreign name type macro expr expr)))
	    ((c-type)
	     ;; already processed so ignore
	     #unspecified)
	    ((class)
	     ;; postponed classes, do nothing
	     #unspecified)
	    (else
	     (error "module5-ast"
		(format "Unsupported definition kind \"~a\"" kind)
		id)))))
   
   (define (def-scope def::Def)
      (with-access::Def def (decl)
	 (if (isa? decl Decl)
	     (with-access::Decl decl ((imod mod) scope)
		(if (eq? imod mod)
		    scope
		    'import))
	     'static)))
   
   (define (def-alias def::Def)
      (with-access::Def def (decl)
	 (if (isa? decl Decl)
	     (with-access::Decl decl (alias) alias)
	     (with-access::Def def (id) id))))
   
   (define (def-mid def::Def)
      (with-access::Def def (decl)
	 (if (isa? decl Decl)
	     (with-access::Decl decl (mod)
		(with-access::Module mod (id) id))
	     (with-access::Module mod (id) id))))
   
   (define (split-local-definitions mid defs)
      (let ((types '())
	    (classes '())
	    (others '()))
	 (hashtable-for-each defs
	    (lambda (k def)
	       (with-access::Def def (id type)
		  (let ((scope (def-scope def)))
		     (unless (eq? scope 'import)
			(let ((e (vector def mid id scope)))
			   (cond
			      ((isa? def KDef) (set! classes (cons e classes)))
			      ((isa? def TDef) (set! types (cons e types)))
			      (else (set! others (cons e others))))))))))
	 (values types classes others)))

   (define (split-imported-declarations mid decls)
      (let ((types '())
	    (classes '())
	    (others '()))
	 (hashtable-for-each decls
	    (lambda (k decl)
	       (with-access::Decl decl (mod xid id alias)
		  (with-access::Module mod ((mid id))
		     (let* ((d (module5-get-export-def mod (or xid id)))
			    (e (vector d mid alias 'import)))
			(cond
			   ((isa? d KDef) (set! classes (cons e classes)))
			   ((isa? d TDef) (set! types (cons e types)))
			   (else (set! others (cons e others)))))))))
	 (values types classes others)))

   (define (split-definitions mid defs decls)
      (multiple-value-bind (deft defc defo)
	 (split-local-definitions mid defs)
	 (multiple-value-bind (declt declc declo)
	    (split-imported-declarations mid decls)
	    (values (append declt deft)
	       (append declc defc)
	       (append declo defo)))))

   (with-trace 'module5 "module5-ast!"
      (with-access::Module mod (defs imports (mid id))
	 (trace-item "mid=" mid)
	 
	 (multiple-value-bind (types classes others)
	    (split-definitions mid defs imports)
	    
	    ;; declare all C types
	    (for-each (lambda (e)
			 (with-access::TDef (vector-ref e 0) (id name)
			    (declare-type! id name 'C)))
	       types)
	    
	    ;; declare all classes
	    (let* ((cs (sort (lambda (ex ey)
				(with-access::KDef (vector-ref ex 0) ((dx depth))
				   (with-access::KDef (vector-ref ey 0) ((dy depth))
				      (<fx dx dy))))
			  classes))
		   (ts (map (lambda (e)
			       (let ((def (vector-ref e 0))
				     (alias (vector-ref e 2))
				     (scope (vector-ref e 3)))
				  (with-access::KDef (vector-ref e 0) (expr kind id depth)
				     (declare-class-definition! kind id alias
					scope expr def))))
			  cs)))
	       (when (eq? mode 'compile)
		  (for-each (lambda (e ty)
			       (when ty
				  (let ((def (vector-ref e 0))
					(alias (vector-ref e 2)))
				     (with-access::KDef (vector-ref e 0) (expr kind id depth)
					(declare-class-slots! id alias expr def ty)))))
		     cs ts)))
	    
	    ;; other declarations
	    (for-each (lambda (e)
			 (let ((def (vector-ref e 0))
			       (mid (vector-ref e 1))
			       (alias (vector-ref e 2))
			       (scope (vector-ref e 3)))
			    (with-access::Def def (expr kind id type)
			       (declare-definition! kind id type alias mid scope expr def))))
	       others)))))

;*---------------------------------------------------------------------*/
;*    module5-main ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-main mod::Module env)
   (with-access::Module mod (main id)
      (when main
	 (let ((v (find-global/module env main id)))
	    (if v
		(with-access::global v (import)
		   (set! import 'export)
		   v)
		(error id "Cannot find main definition" main))))))

;*---------------------------------------------------------------------*/
;*    module5-imported-unit ...                                        */
;*---------------------------------------------------------------------*/
(define (module5-imported-unit mod::Module expand env)

   (define (init-module! imod::Module path)
      (with-access::Module imod (id checksum version expr)
	 (module5-expand-and-resolve! imod module5-init-xenv!
	    :heap-modules (module5-heap4-modules))
	 (if (=fx version 5)
	     (module5-checksum! imod)
	     (set! checksum (module-checksum expr '())))
	 (declare-global-sfun! env 'module-initialization
	    'module-initialization
	    '(checksum::long path::string) id 'import 'sfun
	    #f #f)
	 `((@ module-initialization ,id) ,checksum ,path)))
   
   (with-access::Module mod (inits path)
      (let ((body (map (lambda (m) (init-module! m path)) inits)))
	 (unit 'imported-modules 12 body #f #f))))

;*---------------------------------------------------------------------*/
;*    module5-object-unit ...                                          */
;*---------------------------------------------------------------------*/
(define (module5-object-unit mod::Module)
   (with-access::Module mod (decls)
      (let* ((defs (sort (lambda (x y)
			    (with-access::KDef x ((xdepth depth))
			       (with-access::KDef y ((ydepth depth))
				  (<fx xdepth ydepth))))
		      (filter-map (lambda (xdecl)
				     (when xdecl
					(with-access::Decl xdecl (def)
					   def)))
			 (hashtable-map decls
			    (lambda (k decl)
			       (with-access::Decl decl ((dmod mod) def)
				  (when (and (eq? dmod mod) (isa? def KDef))
				     decl)))))))
	     (body (map (lambda (def)
			   (with-access::KDef def (id registration)
			      `(define ,id ,registration)))
		      defs)))
	 (when (pair? body)
	    (unit 'object 19 body #f #f)))))

;*---------------------------------------------------------------------*/
;*    *module5-envs* ...                                               */
;*---------------------------------------------------------------------*/
(define *module5-envs* '())

;*---------------------------------------------------------------------*/
;*    module5-env ...                                                  */
;*---------------------------------------------------------------------*/
(define (module5-env mod)
   (let ((e (assq mod *module5-envs*)))
      (if (pair? e)
	  (values (cadr e) (cddr e))
	  (multiple-value-bind (env tenv)
	     (restore-heap)
	     (module5-ast! mod env 'import-inline)
	     (set! *module5-envs*
		(cons (cons mod (cons env tenv)) *module5-envs*))
	     (values env tenv)))))
   
;*---------------------------------------------------------------------*/
;*    module5-imported-inline ...                                      */
;*    -------------------------------------------------------------    */
;*    Bind the imported inline in the current module environment       */
;*    but build the inline body in the imported module environment.    */
;*---------------------------------------------------------------------*/
(define (module5-imported-inline mod::Module env)
   (with-trace 'module5 "module5-imported-inline"
      (with-access::Module mod (imports)
	 (hashtable-for-each imports
	    (lambda (k decl)
	       (with-access::Decl decl (def xid id (imod mod))
		  (with-access::Module mod ((mid id) resolved)
		     (let ((def (module5-get-export-def imod (or xid id))))
			(when (isa? def Def)
			   (with-access::Def def (kind expr)
			      (when (eq? kind 'inline)
				 (multiple-value-bind (genv tenv)
				    (module5-env imod)
				    ;; force all globals of imod
				    ;; to be considered as imported in mod
				    (for-each-global! genv
				       (lambda (g)
					  (when (eq? (global-import g) 'export)
					     (global-import-set! g 'import)
					     (add-global! env g (global-id g)))))
				    (let ((d (find-global env id))
					  (e (find-global genv id)))
				       (trace-item "inline id=" id "@" mid)
				       (toplevel->ast expr '() mid genv)
				       (let* ((nd (find-global genv id))
					      (f (global-value nd))
					      (args (sfun-args f))
					      (body (sexp->node (sfun-body f)
						       args
						       (find-location expr)
						       'value genv)))
					  (trace-item "body=" (shape body))
					  (sfun-body-set! (global-value d) body)
					  (sfun-args-set! (global-value d) args))
				       #unspecified)))))))))))))
   
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

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id src mod)
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
;*    parse-extern-c-clause ...                                        */
;*---------------------------------------------------------------------*/
(define (parse-extern-c-clause clause mod::Module x::pair)

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
		(parse-ident args src mod)
		(unless (string? type)
		   args)))
	    ((not (pair? args))
	     args)
	    ((not (symbol? (car args)))
	     args)
	    (else
	     (multiple-value-bind (id type)
		(parse-ident (car args) src mod)
		(if (string? type)
		    (loop (cdr args))
		    args))))))
      
   (define (parse-function macro infix ident args name clause mod::Module)
      (multiple-value-bind (id type)
	 (parse-ident ident clause mod)
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
			    (infix infix)))
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
	 (parse-ident ident clause mod)
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
			    (ronly #t)
			    (decl decl)
			    (args '())
			    (name name)
			    (macro macro)
			    (infix #f)))
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
      (((and (? symbol?) ?ident) ?args (and (? string?) ?name))
       (parse-function #f #f ident args name clause mod))
      ((macro (and (? symbol?) ?ident) (and (? string?) ?name))
       (parse-variable #t ident name clause mod))
      (((and (? symbol?) ?ident) (and (? string?) ?name))
       (parse-variable #f ident name clause mod))
      (else
       (error/loc mod "Illegal extern \"C\" module clause" clause x))))
   
;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-c mod::Module x::pair)
   (for-each (lambda (c) (parse-extern-c-clause c mod x)) (cddr x)))

;*---------------------------------------------------------------------*/
;*    module4-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module4-extern-plugin-c mod::Module x::pair)
   (for-each (lambda (c) (parse-extern-c-clause c mod x)) (cdr x))
   '())

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
	  (java-parser clause (-> mod id) '-))
	 (else
	  (error/loc mod "Illegal extern \"C\" module clause" clause x))))
   
   (for-each parse-clause (cdr x))
   '())

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-java ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-java mod::Module x::pair)
   
   (define (parse-clause clause mod::Module x::pair pkg)


      (define modifier-list
	 '(public private protected static final synchronized abstract))
      
      (define (class-name name)
	 (let ((i (string-contains name "::")))
	    (if i
		(substring name 0 i)
		name)))
      
      (define (parse-class5-ident ident)
	 (let ((name (symbol->string ident)))
	    (if (char=? (string-ref name 0) #\.)
		(let ((name (substring name 1)))
		   (values #f (class-name name) (string->symbol name)))
		(let ((i (string-index-right name #\.)))
		   (if i
		       (let ((pkg (substring name 0 i))
			     (id (substring name (+fx i 1))))
			  (values pkg (class-name name) (string->symbol id)))
		       (values #f (class-name name) ident))))))
      
      (define (field5->field4 field)
	 (if (symbol? field)
	     (multiple-value-bind (id type)
		(parse-ident field field mod)
		`(field ,field ,(symbol->string id)))
	     (let loop ((f field)
			(m '()))
		(cond
		   ((null? field)
		    (error/loc mod "Illegal class field" field x))
		   ((memq (car f) modifier-list)
		    (loop (cdr f) (cons (car f) m)))
		   (else
		    (match-case f
		       ;; field
		       (((and (? symbol?) ?ident))
			(multiple-value-bind (id type)
			   (parse-ident ident field mod)
			   `(field ,@(reverse! m)
			       ,ident ,(symbol->string id))))
		       ;; constructor
		       ((constructor ?id . ?rest)
			`(constructor ,@(reverse! m)
			    ,id ,rest))
		       ((?ident . (and (? list?) ?args))
			;; method
			(multiple-value-bind (id type)
			   (parse-ident ident field mod)
			   `(method ,@(reverse! m)
			       ,ident ,args ,(symbol->string id))))
		       (else
			(error/loc mod "Illegal class field" field x))))))))
      
      (define (class5->class4 keyword cpkg name id rest)
	 `(,keyword ,id ,@(map field5->field4 rest)
	     ,(cond
		 (cpkg name)
		 (pkg (format "~a.~a" pkg id))
		 (else name))))

      (define (class-predicate id x)
	 (let ((o (gensym 'obj))
	       (id (fast-id-of-id id (find-location x))))
	    `(define (,(symbol-append id '?::bool) ,(symbol-append o '|::obj|))
		,(make-private-sexp 'instanceof id o))))
      
      (match-case clause
	 ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
	  (java-parser clause (-> mod id) '|.|))
	 ((or (class ?ident . ?rest)
	      (abstract-class ?ident . ?rest))
	  (multiple-value-bind (cpkg name id)
	     (parse-class5-ident ident)
	     (let ((clazz (class5->class4 (car clause) cpkg name id rest))
		   (pred (class-predicate id clause)))
		(trace-item "clazz=" clazz)
		(java-parser clazz (-> mod id) '|.|)
		(with-access::Module mod (body)
		   (set! body (cons pred body))))))
	 (else
	  (error/loc mod "Illegal extern \"java\" module clause" clause x))))

   (with-trace 'jvm "module5-extern-plugin-java"
      (match-case (cddr x)
	 (((package (and (? symbol?) ?pkg)) . ?other-clauses)
	  (add-qualified-type! (-> mod id) (format "~a.~a" pkg (-> mod id)))
	  (for-each (lambda (c) (parse-clause c mod x pkg)) other-clauses))
	 (else
	  (for-each (lambda (c) (parse-clause c mod x #f)) (cddr x))))))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-wasm ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-wasm mod::Module expr::pair)
   
   (define (parse-clause clause mod::Module)
      (match-case clause
	 (((and (? symbol?) ?ident) (and (? string?) ?name) . ?deps)
	  (multiple-value-bind (id type)
	     (parse-ident ident clause mod)
	     (let ((decl (hashtable-get (-> mod decls) (symbol->string! id))))
		(if (isa? decl Decl)
		    (with-access::Decl decl (attributes)
		       (if (pair? deps)
			   (set! attributes
			      (cons* (cons 'wasm deps)
				 (cons 'qualified-type-name name)
				 attributes))
			   (set! attributes
			      (cons (cons 'qualified-type-name name)
				 attributes))))
		    (error/loc "mod" "Cannot find declaration" clause expr)))))
	 (else
	  (error/loc mod "Illegal extern \"wasm\" module clause" clause expr))))
   
   (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))

;*---------------------------------------------------------------------*/
;*    module5-plugin-pragma ...                                        */
;*---------------------------------------------------------------------*/
(define (module5-plugin-pragma mod::Module expr::pair)

   (define (parse-clause clause::pair mod::Module)
      (match-case clause
	 ((?id . ?props)
	  (let ((decl (module5-get-decl mod id clause)))
	     (with-access::Decl decl ((dmod mod) attributes)
		(cond
		   ((not (eq? dmod mod))
		    (error/loc mod
		       (format "\"~a\" is not defined in module" id)
		       clause mod))
		   (else
		    (set! attributes (append attributes props)))))))
	 (else
	  (error/loc mod "Illegal pragma clause" clause expr))))
   
   (for-each (lambda (c) (parse-clause c mod)) (cdr expr)))

;*---------------------------------------------------------------------*/
;*    module5-plugin-eval ...                                          */
;*---------------------------------------------------------------------*/
(define (module5-plugin-eval mod::Module expr::pair)
   (for-each (lambda (c) (parse-eval c expr)) (cdr expr)))

;*---------------------------------------------------------------------*/
;*    module4-plugin-eval ...                                          */
;*---------------------------------------------------------------------*/
(define (module4-plugin-eval mod::Module expr::pair)
   (module5-plugin-eval mod expr)
   '())

;*---------------------------------------------------------------------*/
;*    module4-plugin-type ...                                          */
;*---------------------------------------------------------------------*/
(define (module4-plugin-type mod::Module x::pair)
   (for-each (lambda (c) (type-parser #f c x)) (cdr x))
   '())

;*---------------------------------------------------------------------*/
;*    module5-resolve-pragma! ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-resolve-pragma! mod::Module env)
   (with-access::Module mod (decls (mid id))
      (hashtable-for-each decls
	 (lambda (k d)
	    (with-access::Decl d ((dmod mod) id attributes scope)
	       (when (and (eq? dmod mod) (pair? attributes))
		  (let* ((m (if (eq? scope 'extern) 'foreign mid))
			 (g (find-global/module env id m)))
		     (if (isa? g global)
			 (for-each (lambda (p)
				      (set-global-pragma-property! g p p))
			    attributes)
			 (error/loc mod "Cannot find global definition" id
			    attributes)))))))))

;*---------------------------------------------------------------------*/
;*    *heap4-modules* ...                                              */
;*---------------------------------------------------------------------*/
(define *heap4-modules* #f)

;*---------------------------------------------------------------------*/
;*    module5-heap4-modules ...                                        */
;*    -------------------------------------------------------------    */
;*    Returns a list of dummy module5 that are as automatically        */
;*    imported by the compiled module.                                 */
;*---------------------------------------------------------------------*/
(define (module5-heap4-modules::pair-nil)
   (if *heap4-modules*
       *heap4-modules*
       (begin
	  (set! *heap4-modules* (heap4-modules))
	  *heap4-modules*)))

;*---------------------------------------------------------------------*/
;*    heap4-modules ...                                                */
;*    -------------------------------------------------------------    */
;*    This function takes no argument as it builds its module list     */
;*    from the global environment that has been built when loading     */
;*    the heap file.                                                   */
;*---------------------------------------------------------------------*/
(define (heap4-modules)
   
   (define mods '())
   
   (define (symbol->path id)
      (string-append "/" (symbol->string id)))
   
   (define (get-module::pair id::symbol)
      (let ((c (assq id mods)))
	 (if (pair? c)
	     (cdr c)
	     (let ((m (list `(module ,id))))
		(set! mods (cons (cons id m) mods))
		m))))
   
   (define (src->def src)
      (match-case src
	 ((class . ?rest) `(define-class ,@rest))
	 ((wide-class . ?rest) `(define-wide-class ,@rest))
	 ((abstract-class . ?rest) `(define-abstract-class ,@rest))
	 ((final-class . ?rest) `(define-final-class ,@rest))
	 (else (error "module5-from-heap4" "Illegal class source" src))))
   
   (define (module-export-class! m::pair t::tclass)
      (with-access::tclass t (src holder id its-super depth)
	 ;; super class
	 (when its-super
	    (with-access::tclass its-super (holder (sid id))
	       (with-access::global holder (module)
		  (let ((ms (get-module module)))
		     (unless (eq? m ms)
			(set-cdr! (last-pair (car m))
			   `((import ,(symbol->path module) ,sid))))))))
	 ;; new class
	 (set-cdr! (last-pair (car m)) `((export ,id)))
	 (set-cdr! (last-pair m) (list (src->def src)))))
   
   (let ((mods '()))
      (for-each set-class-depth! (get-class-list))
      
      (for-each (lambda (t::tclass)
		   (with-access::tclass t (holder)
		      (with-access::global holder (module)
			 (let ((m (get-module module)))
			    (module-export-class! m t)))))
	 (sort (lambda (c1 c2)
		  (with-access::tclass c1 ((d1 depth))
		     (with-access::tclass c2 ((d2 depth))
			(<fx d1 d2))))
	    (get-class-list))))

   (map (lambda (c)
	   (let ((mi (cdr c)))
	      (let ((m (module5-parse mi (symbol->path (car c)))))
		 (module5-expand-and-resolve! m module5-init-xenv!))))
      (reverse mods)))

;*---------------------------------------------------------------------*/
;*    module5-init-xenv! ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-init-xenv! xenv mod)

   (define (define-expander x e)
      (match-case x
	 ((?def ?proto ?body)
	  (localize `(,def ,proto ,(e body e)) x))
	 ((?def ?proto . ?body)
	  (localize `(,def ,proto ,@(map (lambda (x) (e x e)) body)) x))
	 (else
	  (error "expand" "Illegal form" x))))
   
   (install-module5-expander xenv 'define #f define-expander)
   (install-module5-expander xenv 'define-inline #f define-expander)
   (install-module5-expander xenv 'define-generic #f define-expander)
   (install-module5-expander xenv 'define-method #f define-expander)
   (install-module5-expander xenv 'cond-expand #f expand-compile-cond-expand)
   (install-module5-expander xenv '$class-allocate #f expand-class-allocate)
   
   xenv)

;*---------------------------------------------------------------------*/
;*    localize ...                                                     */
;*---------------------------------------------------------------------*/
(define (localize nx x)
   (if (epair? x)
       (econs (car nx) (cdr nx) (cer x))
       nx))
