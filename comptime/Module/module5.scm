;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/comptime/Module/module5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Wed Mar 18 17:14:00 2026 (serrano)                */
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
	   type_type
	   type_env
	   object_class
	   object_slots
	   object_coercion
	   foreign_jtype)

   (export (module5-expand ::pair-nil)
	   (module5-import-def ::Module ::Decl)
	   (module5-ast! ::Module ::obj ::symbol)
	   (module5-module-qualified-name-set! ::Module)
	   (module5-main ::Module ::obj)
	   (module5-imported-unit ::Module ::procedure ::obj)
	   (module5-object-unit ::Module)
	   (module5-imported-inline mod::Module ::obj)
	   (module5-extern-plugin-c ::Module ::pair)
	   (module5-extern-plugin-java ::Module ::pair)
	   (module5-extern-plugin-java-finalizer ::Module)
	   (module5-extern-plugin-wasm ::Module ::pair)
	   (module5-plugin-pragma ::Module ::pair)
	   (module5-plugin-eval ::Module ::pair)
	   (module4-extern-plugin-c ::Module ::pair)
	   (module4-extern-plugin-java ::Module ::pair)
	   (module4-plugin-eval ::Module ::pair)
	   (module4-plugin-type ::Module ::pair)
	   (module4-plugin-pragma ::Module ::pair)
	   (module5-resolve-pragma! ::Module ::obj)
	   (module5-heap4-modules::pair-nil)
	   (module5-init-xenv! xenv ::Module))

   (export (class CDef::Def
	      (args read-only)
	      (name::bstring read-only)
	      (infix::bool read-only)
	      (macro::bool read-only))

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
      (let ((virtual (assq 'vindex p)))
	 (if (and (pair? virtual) (>=fx (cdr virtual) 0))
	     (make-class-virtual-slot p i ty expr)
	     (make-class-direct-slot p i ty expr))))

   (define (make-class-virtual-slot p i ty expr)
      (let ((id (cdr (assq 'id p))))
	 (instantiate::slot
	    (id id)
	    (index i)
	    (name (id->name id))
	    (src (cdr (assq 'expr p)))
	    (class-owner ty)
	    (user-info #f)
	    (virtual-num (cdr (assq 'vindex p)))
	    (getter (cdr (assq 'get p)))
	    (setter (cdr (assq 'set p)))
	    (type (find-type/expr (cdr (assq 'type p)) expr)))))

   (define (make-class-direct-slot p i ty expr)
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
	 (with-access::KDef def (expr id decl super ctor kkind properties)
	    (trace-item "id=" id " alias=" alias)
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

   (define (super-kdef k::KDef)
      (with-access::KDef k (super decl expr id)
	 (when (symbol? super)
	    (with-access::Decl decl (mod def scope)
	       (let ((d (module5-get-decl* mod super expr)))
		  (with-access::Decl d (def)
		     def))))))

   (define (same-kdef? kx ky)
      (with-access::KDef kx ((dx decl))
	 (with-access::KDef ky ((dy decl))
	    (with-access::Decl dx ((idx id) (modx mod))
	       (with-access::Decl dy ((idy id) (mody mod))
		  (and (eq? idx idy) (eq? modx mody)))))))
   
   (define (find-imported-classes classes)
      (let loop ((lclasses classes)
		 (iclasses '()))
	 (cond
	    ((null? lclasses)
	     ;; create iclasses for those that are not
	     ;; explicitly imported
	     (filter (lambda (ic)
			(not (find (lambda (c) (same-kdef? ic c)) classes)))
		iclasses))
	    (else
	     (let* ((c (car lclasses))
		    (s (super-kdef c)))
		(if (not s)
		    (loop (cdr lclasses) iclasses)
		    (with-access::KDef s (decl)
		       (with-access::Decl decl ((imod mod))
			  (cond
			     ((eq? mod imod)
			      (loop (cdr lclasses) iclasses))
			     ((memq s iclasses)
			      (loop (cdr lclasses) iclasses))
			     (else
			      (loop (cdr lclasses)
				 (cons s (append (find-imported-classes (list s)) iclasses)))))))))))))
   
   (with-trace 'module5 "module5-ast!"
      (with-access::Module mod (defs imports (mid id))
	 (trace-item "mid=" mid)
	 
	 (multiple-value-bind (types classes others)
	    (split-definitions mid defs imports)

	    ;; declare all C types
	    (for-each (lambda (e)
			 (let ((t::Def (vector-ref e 0)))
			    (trace-item "type=" (-> t id) " " (typeof t))
			    (if (isa? t JDef)
				(with-access::JDef t (id name super package expr decl scope)
				   (with-access::Decl decl ((dmod mod) scope)
				      (trace-item "mod=" (-> dmod id))
				      (trace-item "scope=" scope)
				      (trace-item "pckage=" package)
				      (unless (or (eq? dmod mod)
						  (eq? scope 'static))
					 
					 (declare-java-class-type! id
					    (find-type super) name package expr))))
				(with-access::TDef t (id name decl kind)
				   (with-access::Decl decl ((dmod mod) scope)
				      (trace-item "kind=" kind)
				      (trace-item "mod=" (-> dmod id))
				      (trace-item "scope=" scope)
				      (declare-type! id name 'C))))))
	       types)

	    ;; declare all classes
	    (let* ((iclasses (find-imported-classes
				(map (lambda (v) (vector-ref v 0))
				   classes)))
		   (ic (map (lambda (def::KDef)
			       (with-access::KDef def (decl id)
				  (with-access::Decl decl (mod)
				     (with-access::Module mod ((mid id))
					(vector def mid id 'import)))))
			  iclasses))
		   (cs (sort (lambda (ex ey)
				(with-access::KDef (vector-ref ex 0) ((dx depth))
				   (with-access::KDef (vector-ref ey 0) ((dy depth))
				      (<fx dx dy))))
			  (append ic classes)))
		   (ts (map (lambda (e)
			       (let ((def (vector-ref e 0))
				     (alias (vector-ref e 2))
				     (scope (vector-ref e 3)))
				  (with-access::KDef def (expr kind id depth)
				     (declare-class-definition! kind id alias
					scope expr def))))
			  cs)))
	       (when (eq? mode 'compile)
		  (for-each (lambda (e ty)
			       (when ty
				  (let ((def (vector-ref e 0))
					(alias (vector-ref e 2)))
				     (with-access::KDef def (expr kind id depth)
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
;*    module5-module-qualified-name-set! ...                           */
;*---------------------------------------------------------------------*/
(define (module5-module-qualified-name-set! mod::Module)
   (with-trace 'module5 "module5-module-qualified-name-set!"
      (trace-item "mod=" (-> mod id))
      (trace-item "qn=" (-> mod qualified-name))
      (trace-item "path=" (-> mod path))
      (when (symbol? (-> mod qualified-name))
	 (unless (string=? (dirname (-> mod path)) "/")
	    (jvm-qualified-name-set! (-> mod id) (-> mod qualified-name))))))

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
      (with-trace 'module5 "init-module!"
	 (trace-item "id=" (-> imod id))
	 (with-access::Module imod (id checksum version expr)
	    (module5-expand-and-resolve! imod module5-init-xenv!
	       :heap-modules (module5-heap4-modules)
	       :default-package (default-jvm-package)
	       :qualified-names (jvm-qualified-names))
	    ;; See engine compiler
	    ;; (module5-module-package-set! imod)
	    (when (symbol? (-> imod qualified-name))
	       (jvm-qualified-name-set! id (-> imod qualified-name)))
	    (if (=fx version 5)
		(module5-checksum! imod)
		(set! checksum (module-checksum expr '())))
	    (declare-global-sfun! env 'module-initialization
	       'module-initialization
	       '(checksum::long path::string) id 'import 'sfun
	       #f #f)
	    `((@ module-initialization ,id) ,checksum ,path))))

   (with-access::Module mod (inits path)
      (with-trace 'module5 "module5-imported-unit"
	 (trace-item "path=" path)
	 (let ((body (map (lambda (m) (init-module! m path)) inits)))
	    (unit 'imported-modules 12 body #f #f)))))

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
   
;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-c mod::Module x::pair)
   (when (memq 'extern (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-extern-c-clause c mod x)) (cddr x)))
   '())

;*---------------------------------------------------------------------*/
;*    module4-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module4-extern-plugin-c mod::Module x::pair)
   (when (memq 'extern (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-extern-c-clause c mod x)) (cdr x)))
   '())

;*---------------------------------------------------------------------*/
;*    declare-java-type! ...                                           */
;*---------------------------------------------------------------------*/
(define (declare-java-type! j::jklass mod::Module clause)
   (with-trace 'jvm "declare-java-type"
      (with-access::jklass j (id jname package src)
	 (trace-item "jklass=" id)
	 (trace-item "mod=" (-> mod id))
	 (trace-item "pkg=" package)
	 (multiple-value-bind (clazz super)
	    (parse-ident id src mod)
	    (co-instantiate
		  ((def (instantiate::JDef
			   (id id)
			   (kind 'java-type)
			   (expr clause)
			   (ronly #t)
			   (expr src)
			   (decl decl)
			   (name jname)
			   (package (if (string? package) package (jname-package jname ".")))
			   (super (if (string? super) (string->symbol super) '_))))
		   (decl (instantiate::Decl
			    (id id)
			    (alias id)
			    (mod mod)
			    (expr clause)
			    (ronly #t)
			    (scope 'export)
			    (def def))))
	       (with-access::Module mod (decls defs exports)
		  (hashtable-put! exports (symbol->string! id) decl)
		  (hashtable-put! decls (symbol->string! id) decl)
		  (hashtable-put! defs (symbol->string! id) def)))))))

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
	  (declare-java-type! (java-parser clause (-> mod id) '-) mod clause))
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
		       ;; method (used for static methods with no arguments)
		       ((method ?ident . (and (? list?) ?args))
			(multiple-value-bind (id type)
			   (parse-ident ident field mod)
			   (if (and (pair? args) (string? (car (last-pair args))))
			       ;; the last argument is the actual method java name
			       (let ((sgra (reverse args)))
				  `(method ,@(reverse! m)
				      ,ident ,(reverse (cdr sgra)) ,(car sgra)))
			       `(method ,@(reverse! m)
				   ,ident ,args ,(symbol->string id)))))
		       ((?ident . (and (? list?) ?args))
			;; method
			(multiple-value-bind (id type)
			   (parse-ident ident field mod)
			   (if (and (pair? args) (string? (car (last-pair args))))
			       ;; the last argument is the actual method java name
			       (let ((sgra (reverse args)))
				  `(method ,@(reverse! m)
				      ,ident ,(reverse (cdr sgra)) ,(car sgra)))
			       `(method ,@(reverse! m)
				   ,ident ,args ,(symbol->string id)))))
		       (else
			(error/loc mod "Illegal class field" field x))))))))
      
      (define (class5->class4 clause cpkg name id super rest)
	 (localize clause
	    `(,(car clause)
	      ,(if super (string->symbol (format "~a::~a" id super)) id)
	      ,@(map (lambda (f) (localize f (field5->field4 f))) rest)
	      ,(cond
		  (cpkg name)
		  (pkg (format "~a.~a" pkg id))
		  (else name)))))
      
      (define (class-predicate id x)
	 (let* ((o (gensym 'obj))
		(fid (fast-id-of-id id (find-location x)))
		(pid (symbol-append fid '?)))
	    (localize x
	       `(define-inline (,(symbol-append pid '::bool) ,(symbol-append o '|::obj|))
		   ,(make-private-sexp 'instanceof id o)))))

      (define (jigloo file x)
	 (with-trace 'module5 "jigloo"
	    (trace-item "file=" file)
	    (let ((path (if (file-name-absolute? file)
			    file
			    (make-file-name (dirname (-> mod path)) file))))
	       (trace-item "path=" path)
	       (let* ((cache-dir (make-file-path *module-cache-dir* "class"))
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
			       (let ((cmd (format "~a -cp ~a -s --module5 ~a -o ~a" *jvm-jigloo*
					     (dirname (-> mod path))
					     (if (file-exists? path) path file)
					     cache)))
				  (trace-item "cmd=" cmd)
				  (if (=fx (system cmd) 0)
				      cache
				      (begin
					 (when (file-exists? cache)
					    (delete-file cache))
					 (error/loc mod "Cannot generate Java header"
					    file x))))
			       cache)
			   (lockf lock 'ulock))))))))

      (match-case clause
	 ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
	  (java-parser clause (-> mod id) '|.|))
	 ((or (class ?ident . ?rest)
	      (abstract-class ?ident . ?rest))
	  (multiple-value-bind (cpkg name id super)
	     (parse-class5-ident ident)
	     (let ((clazz (class5->class4 clause cpkg name id super rest))
		   (pred (class-predicate id clause)))
		(trace-item "ident=" ident)
		(trace-item "id=" id)
		(trace-item "super=" super)
		(trace-item "name=" name)
		(trace-item "class5="
		   (if (>fx (length clazz) 5)
		       (append (take clazz 5) '("..."))
		       clazz))
		(let ((jklazz (java-parser clazz (-> mod id) '|.|)))
		   (declare-java-type! jklazz mod clause))
		(with-access::Module mod (body)
		   (set! body (cons pred body))))))
	 ((array (and (? symbol?) ?ident) (and (? symbol?) ?of))
 	  (java-declare-array clause ident of (-> mod id)))
	 ((import (and ?class (? symbol?)))
	  (module5-extern-plugin-java mod
	     (call-with-input-file (jigloo (symbol->string class) clause)
		read)))
	 (else
	  (error/loc mod "Illegal extern \"java\" module clause" clause x))))
   
   (with-trace 'jvm "module5-extern-plugin-java"
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
;*    module5-extern-plugin-java-finalizer ...                         */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-java-finalizer mod::Module)
   ;; Mark that all the java class predicates cannot be removed
   ;; until the coercion and checks have been inserted in the AST
   ;; Because of the complex Java code generation and complex declarations
   ;; associated with these codes, this cannot mark assignments cannot
   ;; done while the classes are constructed
   (for-each-type! (lambda (t)
		      (when (or (isa? t jclass) (isa? t jarray))
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
   
   (when (memq 'wasm (backend-foreign-clause-support (the-backend)))
      (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))
   '())

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
;*    module4-plugin-pragma ...                                        */
;*---------------------------------------------------------------------*/
(define (module4-plugin-pragma mod::Module x::pair)
   (for-each (lambda (c) (pragma-parser c (-> mod id) x)) (cdr x))
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
	      (let ((m::Module (module5-parse mi (symbol->path (car c)))))
		 (module5-expand-and-resolve! m module5-init-xenv!
		    :default-package (default-jvm-package)
		    :qualified-names (jvm-qualified-names))
		 m)))
      (reverse mods)))

;*---------------------------------------------------------------------*/
;*    module5-init-xenv! ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-init-xenv! xenv mod)

   (define (define-expander x e)
      (match-case x
	 ((?def ?proto ?body)
	  (localize x `(,def ,proto ,(e body e))))
	 ((?def ?proto . ?body)
	  (localize x `(,def ,proto ,@(map (lambda (x) (e x e)) body))))
	 (else
	  (error "expand" "Illegal form" x))))

   (define (define-macro-expander-TBR-13ma42026 x e)
      ;; macro expander cannot use regular module5 initial env because
      ;; the inner define expanders of that environment are incompatible
      ;; with eval
      (let ((envx *module5-env*))
	 (set! *module5-env* #f)
	 (let ((nx (expand-define-macro x e)))
	    (set! *module5-env* envx)
	    x)))

   (define (define-macro-expander x e)
      ;; macro expander cannot use regular module5 initial env because
      ;; the inner define expanders of that environment are incompatible
      ;; with eval
      (let ((envx *module5-env*))
	 (set! *module5-env* #f)
	 (let ((nx (expand-define-macro x e)))
	    (set! *module5-env* envx)
	    #unspecified)))

   (define (define-macro-expander-new x e)
      ;; macro expander cannot use regular module5 initial env because
      ;; the inner define expanders of that environment are incompatible
      ;; with eval
      (expand-define-macro x e)
      #unspecified)

   ;;(install-module5-expander xenv 'define-macro #f define-macro-expander)
   (install-module5-expander xenv 'define #f define-expander)
   (install-module5-expander xenv 'define-inline #f define-expander)
   (install-module5-expander xenv 'define-generic #f define-expander)
   (install-module5-expander xenv 'define-method #f define-expander)
   (install-module5-expander xenv 'cond-expand #f expand-compile-cond-expand)
   (install-module5-expander xenv '$class-allocate #f expand-class-allocate)
   (install-module5-expander xenv 'assert #f expand-assert)
   
   xenv)
