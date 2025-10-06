;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/comptime/Module/module5.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 17:14:08 2025                          */
;*    Last change :  Mon Oct  6 07:49:12 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
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
	   module_module
	   module_class
	   module_checksum
	   module_pragma
	   expand_eps
	   ast_node
	   ast_var
	   ast_env
	   ast_glo-decl
	   ast_ident
	   type_type
	   type_env
	   object_class
	   object_slots
	   object_coercion)

   (export (module5-expand ::pair-nil)
	   (module5-import-def ::Module ::Decl)
	   (module5-ast! ::Module)
	   (module5-main ::Module)
	   (module5-imported-unit ::Module ::procedure)
	   (module5-object-unit ::Module)
	   (module5-imported-inline-unit ::Module)
	   (module5-extern-plugin-c ::Module ::pair)
	   (module5-extern-plugin-java ::Module ::pair)
	   (module5-extern-plugin-wasm ::Module ::pair)
	   (module5-plugin-pragma ::Module ::pair)
	   (module5-resolve-pragma! ::Module)
	   (module5-heap4-modules::pair-nil))

   (export (class CDef::Def
	      (args read-only)
	      (name::bstring read-only)
	      (infix::bool read-only)
	      (macro::bool read-only))))

;*---------------------------------------------------------------------*/
;*    module5-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (module5-expand x)
   (module5-expander x initial-expander))

;*---------------------------------------------------------------------*/
;*    module5-import-def ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-import-def mod::Module decl::Decl)
   (with-access::Decl decl ((dmod mod) def id)
      (if (eq? mod dmod)
	  def
	  (module5-get-export-def dmod id))))

;*---------------------------------------------------------------------*/
;*    module5-ast! ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-ast! mod::Module)
   
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
   
   (define (make-class-slot p i ty)
      (let ((id (cdr (assq 'id p))))
	 (instantiate::slot
	    (id id)
	    (index i)
	    (name (id->name id))
	    (src (cdr (assq 'expr p)))
	    (class-owner ty)
	    (user-info #f)
	    (type (find-type (cdr (assq 'type p)))))))
   
   (define (declare-class-definition! id alias mid scope src def::KDef)
      (with-access::KDef def (expr id decl super ctor kkind properties)
	 (when (isa? decl Decl)
	    (with-access::Decl decl (mod)
	       (with-access::Module mod ((mid id))
		  (unless (find-global/module id mid) 
		     ;; a class declared in the module being compiled
		     (let ((var (declare-global-svar! id id mid scope expr expr)))
			(global-type-set! var (find-type 'class))
			(global-set-read-only! var)
			(let* ((sup (and super (find-type super)))
			       (ty (declare-class-type! id sup
				      ctor var #f
				      (eq? kkind 'define-final-class)
				      (eq? kkind 'define-abstract-class)
				      (eq? kkind 'define-wide-class))))
			   (gen-class-coercions! ty)
			   (let* ((sslots (if sup (tclass-slots sup) '()))
				  (nslots (map (lambda (p i)
						  (make-class-slot p i ty))
					     properties
					     (iota (length properties)
						(length sslots)))))
			      (tclass-slots-set! ty (append sslots nslots))))
			var)))))))
   
   (define (declare-definition! kind id alias mid scope expr def::Def)
      (case kind
	 ((variable)
	  (declare-global-svar! id alias
	     mid scope expr expr))
	 ((procedure)
	  (declare-global-sfun! id alias (procedure-args expr id mid)
	     mid scope 'sfun expr expr))
	 ((inline)
	  (declare-global-sfun! id alias (procedure-args expr id mid)
	     mid scope 'sifun expr expr))
	 ((generic)
	  (declare-global-sfun! id alias (procedure-args expr id mid)
	     mid scope 'sgfun expr expr))
	 ((macro)
	  (with-access::Def def (expr)
	     (add-macro-definition! expr id)))
	 ((expander)
	  (with-access::Def def (expr)
	     (add-macro-definition! expr id)))
	 ((c-function)
	  (with-access::CDef def (name type infix args macro)
	     (declare-global-cfun! id alias 'foreign name type args
		#f macro expr expr)))
	 ((c-variable)
	  (with-access::CDef def (name type macro)
	     (declare-global-cvar! id alias name type macro expr expr)))
	 ((class)
	  ;; postpone the declaration of classes because they must be sorted
	  ;; before declared
	  (set! unsorted-classes (cons (vector def alias scope) unsorted-classes)))
	 (else
	  (error "module5-ast"
	     (format "Unsupported definition kind \"~a\"" kind)
	     id))))
   
   (define (declare-local-definition! d::Def mid)
      (with-access::Def d (expr kind id decl ronly)
	 (let ((alias (if (isa? decl Decl)
			  (with-access::Decl decl (alias) alias)
			  id))
	       (scope (if (isa? decl Decl)
			  (with-access::Decl decl (scope) scope)
			  'static)))
	    (declare-definition! kind id id mid scope expr d))))
   
   (define (declare-import-declaration! d::Decl mid)
      (with-access::Decl d (def id alias ronly scope (imod mod))
	 (when (eq? scope 'import)
	    (let ((def (module5-get-export-def imod id)))
	       (with-access::Def def (kind id expr)
		  (with-access::Module imod ((mid id))
		     (declare-definition! kind id alias mid 'import expr def)))))))
   
   (define (kdefs)
      (sort (lambda (kx ky)
	       (with-access::KDef (vector-ref kx 0) ((ix index))
		  (with-access::KDef (vector-ref ky 0) ((iy index))
		     (<fx ix iy))))
	 unsorted-classes))
      
   (with-access::Module mod (defs decls exports (mid id))
      
      (hashtable-for-each defs
	 (lambda (k d)
	    (declare-local-definition! d mid)))
      
      (hashtable-for-each decls
	 (lambda (k d)
	    (declare-import-declaration! d mid)))
      
      (for-each (lambda (k)
		   (let ((d (vector-ref k 0))
			 (alias (vector-ref k 1))
			 (scope (vector-ref k 2)))
		      (with-access::Def d (expr kind id decl ronly)
			 (declare-class-definition! kind id alias scope expr d))))
	 (kdefs))))

;*---------------------------------------------------------------------*/
;*    module5-main ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-main mod::Module)
   (with-access::Module mod (main id)
      (when main
	 (let ((v (find-global/module main id)))
	    (if v
		(with-access::global v (import)
		   (set! import 'export)
		   v)
		(error id "Cannot find main definition" main))))))

;*---------------------------------------------------------------------*/
;*    module5-imported-unit ...                                        */
;*---------------------------------------------------------------------*/
(define (module5-imported-unit mod::Module expand)
   (with-access::Module mod (inits path)
      (let ((body (map (lambda (imod)
			  (with-access::Module imod (id checksum version expr)
			     (module5-expand-and-resolve! imod
				(lambda ()
				   (create-hashtable :weak 'open-string))
				:heap-modules (module5-heap4-modules))
			     (if (=fx version 5)
				 (module5-checksum! imod)
				 (set! checksum (module-checksum expr '())))
			     (declare-global-sfun! 'module-initialization
				'module-initialization
				'(checksum::long path::string) id 'import 'sfun
				#f #f)
			     `((@ module-initialization ,id) ,checksum ,path)))
		     inits)))
	 
	 (unit 'imported-modules 12 body #f #f))))

;*---------------------------------------------------------------------*/
;*    module5-object-unit ...                                          */
;*---------------------------------------------------------------------*/
(define (module5-object-unit mod::Module)
   (with-access::Module mod (decls)
      (let* ((defs (sort (lambda (x y)
			    (with-access::KDef x ((xindex index))
			       (with-access::KDef y ((yindex index))
				  (<fx xindex yindex))))
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
;*    module5-imported-inline-unit ...                                 */
;*---------------------------------------------------------------------*/
(define (module5-imported-inline-unit mod::Module)
   (with-access::Module mod (imports)
      (let ((body (filter (lambda (x) x)
		     (hashtable-map imports
			(lambda (k decl)
			   (with-access::Decl decl (def)
			      (when (isa? def Def)
				 (with-access::Def def (kind expr)
				    (when (eq? kind 'inline)
				       expr)))))))))
	 (when (pair? body)
	    (unit 'inline 0 body #t #f)))))
   
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
;*    module5-extern-plugin-c ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-c mod::Module expr::pair)

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
   
   (define (parse-clause clause mod::Module)
      (match-case clause
	 ((include (and (? string?) ?string))
	  (parse-include string clause mod))
	 ((type (and (? symbol?) ?id) (and (? string?) ?name))
	  (declare-type! id name 'C))
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
	  (error/loc mod "Illegal extern \"C\" module clause" clause expr))))
   
   (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))

;*---------------------------------------------------------------------*/
;*    module5-extern-plugin-java ...                                   */
;*---------------------------------------------------------------------*/
(define (module5-extern-plugin-java mod::Module expr::pair)
   
   (define (parse-clause clause mod::Module)
      (match-case clause
	 (else
	  (error/loc mod "Illegal extern \"java\" module clause" clause expr))))
   
   (for-each (lambda (c) (parse-clause c mod)) (cddr expr)))

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
;*    module5-resolve-pragma! ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-resolve-pragma! mod::Module)
   (with-access::Module mod (decls (mid id))
      (hashtable-for-each decls
	 (lambda (k d)
	    (with-access::Decl d ((dmod mod) id attributes scope)
	       (when (and (eq? dmod mod) (pair? attributes))
		  (let* ((m (if (eq? scope 'extern) 'foreign mid))
			 (g (find-global/module id m)))
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
		 (module5-expand-and-resolve! m
		    (lambda ()
		       (create-hashtable :weak 'open-string))))))
      (reverse mods)))
