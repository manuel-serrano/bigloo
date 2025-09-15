;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Llib/module5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 07:29:51 2025                          */
;*    Last change :  Sun Sep 14 12:25:29 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    module5 parser                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __module5
   
   (import  __error
	    __object
	    __configure
	    __reader
	    __hash
	    __library
	    __binary)

   (use     __type
	    __tvector
	    __bexit
	    __thread
	    __bit
	    __bignum
	    __bigloo
	    __os
	    __structure
	    __rgc
	    __evenv

	    __r4_input_6_10_2
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r5_control_features_6_4
	    __r4_output_6_10_3
	    __r4_ports_6_10_1)

   (export (class Module
	      (id::symbol read-only)
	      (path::bstring read-only)
	      (decls read-only (default (create-hashtable :weak 'open-string)))
	      (defs read-only (default (create-hashtable :weak 'open-string)))
	      (inits::pair-nil (default '()))
	      (libraries::pair-nil (default '()))
	      (body::obj (default '())))
	   
	   (class Decl
	      (id::symbol read-only)
	      (alias::symbol read-only)
	      (mod::Module read-only)
	      (def (default #unspecified))
	      (src (default #unspecified))
	      (ronly (default #unspecified))
	      scope::symbol)

	   (class Def
	      (id::symbol read-only)
	      (type::obj (default #unspecified))
	      kind::symbol 
	      (src (default #unspecified))
	      (ronly (default #unspecified))
	      (decl (default #unspecified)))
	   
	   (module5-resolve-path ::bstring ::bstring)
	   (module5-resolve-library ::symbol ::pair-nil)
	   (module5-read::Module ::bstring #!key lib-path expand)
	   (module5-read-library::Module ::bstring)
	   (module5-read-heap::Module ::bstring)
	   (module5-write-heap ::bstring ::Module)
	   (module5-parse::Module ::obj ::bstring #!key lib-path expand)
	   (module5-expand!::Module ::Module #!key expand)
	   (module5-resolve!::Module ::Module)))

;*---------------------------------------------------------------------*/
;*    object-write ::Module ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-write m::Module . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Module id=~a path=~s>" (-> m id) (-> m path)))

;*---------------------------------------------------------------------*/
;*    object-display ::Module ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-display m::Module . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Module id=~a path=~s>" (-> m id) (-> m path)))

;*---------------------------------------------------------------------*/
;*    object-write ::Decl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (object-write d::Decl . port)
   (let ((m::Module (-> d mod)))
      (fprintf (if (pair? port) (car port) (current-output-port))
	 "#<Decl ~a/~a mod=~a scope=~a ronly=~a>"
	 (-> d id) (-> d alias) (-> m id) (-> d scope) (-> d ronly))))

;*---------------------------------------------------------------------*/
;*    object-display ::Decl ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display d::Decl . port)
   (let ((m::Module (-> d mod)))
      (fprintf (if (pair? port) (car port) (current-output-port))
	 "#<Decl ~a/~a mod=~a scope=~a ronly=~a>"
	 (-> d id) (-> d alias) (-> m id) (-> d scope) (-> d ronly))))

;*---------------------------------------------------------------------*/
;*    *all-modules* ...                                                */
;*---------------------------------------------------------------------*/
(define *all-modules* (create-hashtable :weak 'open-string))
(define module-mutex (make-mutex "modules"))

;*---------------------------------------------------------------------*/
;*    module5-resolve-path ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-resolve-path rel::bstring base::bstring)
   (let ((path (make-file-name (dirname base) rel)))
      (when (file-exists? path)
	 (file-name-unix-canonicalize! path))))

;*---------------------------------------------------------------------*/
;*    module5-resolve-library ...                                      */
;*---------------------------------------------------------------------*/
(define (module5-resolve-library id::symbol search-path)
   (let* ((init (library-init-file id))
	  (path (find-file/path init search-path)))
      (when (file-exists? path)
	 (file-name-unix-canonicalize! path))))

;*---------------------------------------------------------------------*/
;*    module5-read ...                                                 */
;*---------------------------------------------------------------------*/
(define (module5-read path::bstring #!key lib-path expand)
   (with-trace 'module5 "module5-read"
      (trace-item "path=" path)
      (trace-item "expand=" expand)
      (synchronize module-mutex
	 (let ((omod (hashtable-get *all-modules* path)))
	    (if omod
		omod
		(let ((exprs (call-with-input-file path
				(lambda (p) (port->sexp-list p #t)))))
		   (if (null? exprs)
		       (error "module5-read" "Missing module clause" path)
		       (let ((nmod::Module (module5-parse (car exprs) path
					      :lib-path lib-path
					      :expand expand)))
			  (set! (-> nmod body) (cdr exprs))
			  nmod))))))))

;*---------------------------------------------------------------------*/
;*    module5-read-library ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-read-library path::bstring)
   (let ((init (call-with-input-file path read)))
      (match-case init 
	 ((declare-library! (quote ?id) . ?rest)
	  (let ((heap (member :heap rest)))
	     (match-case heap
		((:heap (and (? string?) ?file . ?-))
		 (module5-read-heap
		    (make-file-name (dirname path) file)))
		(#f
		 (module5-read-heap
		    (make-file-name (dirname path)
		       (string-append (prefix (basename path)) ".heap5"))))
		(else
		 (error/loc (format "Illegal library heap \"~a\"" path) heap init)))))
	 (else
	  (error/loc "Illegal library" path init)))))

;*---------------------------------------------------------------------*/
;*    module5-read-heap ...                                            */
;*---------------------------------------------------------------------*/
(define (module5-read-heap path::bstring)
   (let ((port (open-input-binary-file path)))
      (if (not (binary-port? port))
	  (error "module5" "Cannot read heap file" path)
	  (unwind-protect
	     (heap->module5 (input-obj port))
	     (close-binary-port port)))))

;*---------------------------------------------------------------------*/
;*    module5-write-heap ...                                           */
;*---------------------------------------------------------------------*/
(define (module5-write-heap path::bstring mod::Module)
   (let ((port (open-output-binary-file path)))
      (if (not (binary-port? port))
	  (error "module5" "Cannot write heap file" path)
	  (unwind-protect
	     (output-obj port (module5->heap mod))
	     (close-binary-port port)))))

;*---------------------------------------------------------------------*/
;*    heap->module5 ...                                                */
;*---------------------------------------------------------------------*/
(define (heap->module5::Module heap)
   heap)

;*---------------------------------------------------------------------*/
;*    module5->heap ...                                                */
;*---------------------------------------------------------------------*/
(define (module5->heap mod::Module)
   mod)

;*---------------------------------------------------------------------*/
;*    module5-parse ...                                                */
;*---------------------------------------------------------------------*/
(define (module5-parse::Module expr path::bstring #!key lib-path expand)

   (define (parse id path clauses)
      (let ((mod (instantiate::Module
		    (id id)
		    (path path))))
	 (hashtable-put! *all-modules* path mod)
	 (for-each (lambda (c)
		      (module5-parse-clause c expr mod lib-path expand))
	    clauses)
	 mod))
   
   (with-trace 'module5 "module5-parse"
      (trace-item "path=" path)
      (trace-item "expr=" expr)
      (trace-item "expand=" expand)
      (match-case expr
	 ((module (and (? symbol?) ?id) :version 5 . ?clauses)
	  (parse id path clauses))
	 ((module (and (? symbol?) ?id) . ?clauses)
	  (parse id path clauses))
	 (else
	  (error/loc "Illegal expression" expr #f)))))

;*---------------------------------------------------------------------*/
;*    module5-parse-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-parse-clause clause expr::pair mod::Module lib-path expand)

   (define (unbound-error path id clause)
      (error/loc (format "Cannot find declaration in module \"~a\"" path)
	 id clause))

   (define (scope-error path id clause)
      (error/loc (format "Module \"~a\" does not export" path)
	 id clause))

   (define (hashtable-symbol-get table id)
      (hashtable-get table (symbol->string! id)))

   (define (hashtable-symbol-put! table id d)
      (hashtable-put! table (symbol->string! id) d))

   (define (parse-import-binding b imod::Module expr::pair mod::Module expand)
      (match-case b
	 ((? symbol?)
	  (let ((idecl (hashtable-symbol-get (-> imod decls) b)))
	     (if (isa? idecl Decl)
		 (with-access::Decl idecl (scope)
		    (if (eq? scope 'export)
			(let ((d (duplicate::Decl idecl
				    (scope 'import))))
			   (hashtable-symbol-put! (-> mod decls) b d)
			   d)
			(scope-error (-> imod path) b clause)))
		 (unbound-error (-> imod path) b clause))))
	 (((and (? symbol?) ?alias) (and (? symbol?) ?id))
	  (let ((idecl (hashtable-symbol-get (-> imod decls) id)))
	     (if (isa? idecl Decl)
		 (with-access::Decl idecl (scope)
		    (if (eq? scope 'export)
			(let ((d (duplicate::Decl idecl
				    (scope 'import)
				    (alias alias))))
			   (hashtable-symbol-put! (-> mod decls) alias d)
			   d)
			(scope-error (-> imod path) b clause)))
		 (unbound-error (-> imod path) b clause))))
	 (else
	  (error/loc "Illegal import binding" b clause))))
   
   (define (parse-reexport-all clause::pair expr::pair mod::Module expand)
      (let* ((rclause (reverse (cdr clause)))
	     (path (car rclause))
	     (bindings (cdr rclause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path :expand expand)))
		(hashtable-for-each (-> imod decls)
		   (lambda (key d::Decl)
		      (when (eq? (-> d scope) 'export)
			 (hashtable-put! (-> mod decls) key d))))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc "Cannot find file" (cadr clause) clause))))
   
   (define (parse-reexport clause::pair expr::pair mod::Module expand)
      (let* ((rclause (reverse (cdr clause)))
	     (path (car rclause))
	     (bindings (cdr rclause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path :expand expand)))
		(for-each (lambda (b)
			     (let ((d::Decl (parse-import-binding b
					       imod expr mod expand)))
				(set! (-> d scope) 'export)
				d))
		   bindings)
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc "Cannot find file" (cadr clause) clause))))
   
   (define (parse-import-init clause::pair expr::pair mod::Module expand)
      (let* ((path (caddr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path :expand expand)))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc "Cannot find file" path clause))))
   
   (define (parse-import-all clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path :expand expand)))
		(hashtable-for-each (-> imod decls)
		   (lambda (key d::Decl)
		      (when (eq? (-> d scope) 'export)
			 (let ((nd (duplicate::Decl d
				      (scope 'import))))
			    (hashtable-put! (-> mod decls) key nd)))))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc "Cannot find file" path clause))))
   
   (define (parse-import-some clause::pair expr::pair mod::Module expand)
      (let* ((rclause (reverse (cdr clause)))
	     (path (car rclause))
	     (bindings (cdr rclause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path :expand expand)))
		(for-each (lambda (b)
			     (parse-import-binding b imod expr mod expand))
		   bindings)
		(set! (-> mod inits) (append! (-> mod inits) (list rfrom))))
	     (error/loc "Cannot find file" path clause))))

   (define (parse-export clause expr::pair mod::Module expand)
      (for-each-expr (lambda (expr src)
			(match-case expr
			   ((and ?id (? symbol?))
			    (hashtable-symbol-put! (-> mod decls) id
			       (instantiate::Decl
				  (id id)
				  (alias id)
				  (mod mod)
				  (scope 'export)
				  (src src))))
			   (((and ?alias (? symbol?)) (and ?id (? symbol?)))
			    (hashtable-symbol-put! (-> mod decls) alias
			       (instantiate::Decl
				  (id id)
				  (alias alias)
				  (mod mod)
				  (scope 'export)
				  (src expr))))
			   (else
			    (error/loc "Illegal export clause" clause expr))))
	 (cdr clause)))

   (define (parse-include clause expr::pair mod::Module expand)
      (for-each (lambda (f)
		   (cond
		      ((not (string? f))
		       (error/loc "Illegal include clause" f clause))
		      ((module5-resolve-path f (-> mod path))
		       (call-with-input-file f
			  (lambda (p)
			     (for-each (lambda (c)
					  (module5-parse-clause c clause mod
					     lib-path expand))
				(port->sexp-list p #t)))))
		      (else
		       (error/loc "Cannot find file" f clause))))
	 (cdr clause)))

   (define (parse-cond-expand clause expr::pair mod::Module expand)
      (for-each (lambda (c)
		   (module5-parse-clause c clause mod lib-path expand))
	 (expand clause)))

   (define (parse-library-all clause expr::pair mod::Module expand)
      (let* ((lib (cadr clause))
	     (rlib (module5-resolve-library lib lib-path)))
	 (if (string? rlib)
	     (let ((lmod::Module (module5-read-library rlib)))
		(hashtable-for-each (-> lmod decls)
		   (lambda (k d::Decl)
		      (when (eq? (-> d scope) 'export)
			 (let ((nd (duplicate::Decl d
				      (scope 'import))))
			    (hashtable-put! (-> mod decls) k nd)))))
		(set! (-> mod inits)
		   (append! (-> mod inits) (list lmod)))
		(set! (-> mod libraries)
		   (cons (cons lib rlib) (-> mod libraries))))
	     (error/loc "Cannot find library" lib clause))))
   
   (define (parse-library-some clause expr::pair mod::Module expand)
      (let* ((rclause (reverse (cdr clause)))
	     (lib (car clause))
	     (bindings (cdr rclause))
	     (rlib (module5-resolve-library lib lib-path)))
	 (if (string? rlib)
	     (let ((lmod::Module (module5-read-library rlib)))
		(for-each (lambda (b)
			     (parse-import-binding b lmod expr mod expand))
		   bindings)
		(set! (-> mod libraries)
		   (cons (cons lib rlib) (-> mod libraries)))
		(set! (-> mod inits)
		   (append! (-> mod inits) (list lmod))))
	     (error/loc "Cannot find library" lib clause))))

   (with-trace 'module5 "module5-parse-clause"
      (trace-item "clause=" clause)
      (trace-item "lib-path=" lib-path)
      (match-case clause
	 ((export (? string?))
	  (parse-reexport-all clause expr mod expand))
	 ((export ??- (? string?))
	  (parse-reexport clause expr mod expand))
	 ((import () (? string?))
	  (parse-import-init clause expr mod expand))
	 ((import (? string?))
	  (parse-import-all clause expr mod expand))
	 ((import ??- (? string?))
	  (parse-import-some clause expr mod expand))
	 ((export . ?bindings)
	  (parse-export clause expr mod expand))
	 ((include ??-)
	  (parse-include clause expr mod expand))
	 ((library (? symbol?))
	  (parse-library-all clause expr mod expand))
	 ((library ??- (? symbol?))
	  (parse-library-some clause expr mod expand))
	 ((extern (and (? string?) ?backend) . ?clauses)
	  (print "extern " backend " " clauses))
	 ((cond-expand)
	  (parse-cond-expand clause expr mod expand))
	 (else
	  (error/loc "Illegal module clause" clause expr)))))

;*---------------------------------------------------------------------*/
;*    module5-expand! ...                                              */
;*---------------------------------------------------------------------*/
(define (module5-expand! mod::Module #!key expand)
   mod)

;*---------------------------------------------------------------------*/
;*    module5-resolve! ...                                             */
;*---------------------------------------------------------------------*/
(define (module5-resolve! mod::Module)
   (with-access::Module mod (body)
      (collect-define*! mod body)
      (check-unbounds mod)
      (ronly! mod))
   mod)

;*---------------------------------------------------------------------*/
;*    check-unbounds ...                                               */
;*---------------------------------------------------------------------*/
(define (check-unbounds mod::Module)
   (with-access::Module mod (decls (mid id))
      (let ((unbounds '()))
	 (hashtable-for-each decls
	    (lambda (k d)
	       (with-access::Decl d (def id)
		  (unless (isa? def Def)
		     (set! unbounds (cons d unbounds))))))
	 (when (pair? unbounds)
	    (for-each (lambda (d)
			 (with-access::Decl d (id src)
			    (with-handler exception-notify
					  (error/loc "Definition missing"
					     id src))))
	       (reverse! unbounds))
	    (error "module5-resolve!" "Unbound exported identifiers"
	       (map (lambda (d) (with-access::Decl d (id) id)) unbounds))))))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id src)
   (let* ((s (symbol->string id))
	  (l (-fx (string-length s) 2)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i l)
	     (values id #unspecified))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (if (=fx i (-fx l 3))
		     (error/loc "Illegal identifier" id src)
		     (values (string->symbol (substring s 0 i))
			(substring s (+fx i 2))))
		 (loop (+fx i 2))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    collect-define*! ...                                             */
;*---------------------------------------------------------------------*/
(define (collect-define*! mod body)
   (define (module-define! mod kind::symbol id::symbol type src)
      (with-access::Module mod (defs decls)
	 (let* ((name (symbol->string! id))
		(old (hashtable-get defs name))
		(decl (hashtable-get decls name)))
	    (if old
		(error/loc "Identifier ~s has already been declared" name src)
		(let ((def (instantiate::Def
				 (id id)
				 (type type)
				 (kind kind)
				 (src src))))
		   (hashtable-put! defs name def)
		   (when decl
		      (with-access::Decl decl (scope (ddef def))
			 (with-access::Def def ((ddecl decl))
			    (case scope
			       ((export)
				(set! ddef def)
				(set! ddecl decl))
			       ((static)
				(set! ddecl decl))))))
		   def)))))
   
   (define (collect-define! mod expr)
      (match-case expr
	 ((define (and (? symbol?) ?id) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'variable name type expr)))
	 ((define ((and (? symbol?) ?id) . ?-) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'variable name type expr)))
	 ((define-inline ((and (? symbol?) ?id) . ?-) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'inline name type expr)))
	 ((define-generic ((and (? symbol?) ?id) . ?-) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'generic name type expr)))
	 ((define-class (and (? symbol?) ?id) . ?-)
	  (multiple-value-bind (name type)
	     (parse-ident id expr)
	     (module-define! mod 'class name type expr)))
	 ((begin . ?exprs)
	  (collect-define*! mod exprs))))
   
   (for-each (lambda (expr) (collect-define! mod expr)) body))

;*---------------------------------------------------------------------*/
;*    ronly! ...                                                       */
;*---------------------------------------------------------------------*/
(define (ronly! mod::Module)

   (define (args-id args)
      (cond
	 ((null? args)
	  '())
	 ((pair? args)
	  (multiple-value-bind (name type)
	     (parse-ident (car args) args)
	     (cons name (args-id (cdr args)))))
	 (else
	  (multiple-value-bind (name type)
	     (parse-ident args args)
	     (list name)))))

   (define (binding-id binding)
      (multiple-value-bind (name type)
	 (if (pair? binding)
	     (parse-ident (car binding) binding)
	     (parse-ident binding binding))
	 name))
	  
   (define (bindings-id bindings)
      (append-map binding-id bindings))
   
   (define (ronly-exprs! exprs env defs)
      (let loop ((exprs exprs))
	 (cond
	    ((pair? exprs)
	     (ronly-expr! (car exprs) env defs)
	     (ronly-exprs! (cdr exprs) env defs))
	    ((null? exprs)
	     #unspecified)
	    (else
	     (ronly-expr! exprs env defs)))))
	  
   (define (ronly-expr! expr env defs)
      (when (pair? expr)
	 (match-case expr
	    ((set! (and (? symbol?) ?id) . ?val)
	     (ronly-expr! val env defs)
	     (let ((def (hashtable-get defs (symbol->string! id))))
		(when (isa? def Def)
		   (let ((l (memq id env)))
		      (unless (pair? l)
			 (with-access::Def def (ronly)
			    (when (eq? ronly #t)
			       (error/loc "Illegal assignment" id expr))
			    (set! ronly #f)))))))
	    ((define (and (? symbol?) ?id) ?expr)
	     (ronly-expr! expr env defs))
	    ((define ((and (? symbol?) ?id) . ?args) . ?body)
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-inline ((and (? symbol?) ?id) . ?args) . ?body)
	     (let ((def (hashtable-get defs (symbol->string! id))))
		(if (isa? def Def)
		    (with-access::Def def (ronly kind)
		       (unless (eq? ronly #unspecified)
			  (error/loc "Illegally mutated inline function"
			     id expr))
		       (begin
			  (set! ronly #t)
			  (set! kind 'inline)))))
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-generic ((and (? symbol?) ?id) . ?args) . ?body)
	     (let ((def (hashtable-get defs (symbol->string! id))))
		(if (isa? def Def)
		    (with-access::Def def (ronly kind)
		       (unless (eq? ronly #unspecified)
			  (error/loc "Illegally mutated generic function"
			     id expr))
		       (begin
			  (set! ronly #t)
			  (set! kind 'generic)))))
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-class (and (? symbol?) ?id) . ?-)
	     #unspecified)
	    ((begin . ?exprs)
	     (ronly-exprs! exprs env defs))
	    ((lambda ?args . ?exprs)
	     (ronly-exprs! exprs (append (args-id args) env) defs))
	    ((let (and (? symbol?) ?loop) ?bindings . ?exprs)
	     (multiple-value-bind (name type)
		(parse-ident loop (cdr expr))
		(let ((nenv (cons name (append (bindings-id bindings) env))))
		   (ronly-exprs! exprs nenv defs))))
	    (((or let* letrec letrec*) ?bindings . ?exprs)
	     (let ((nenv (append (bindings-id bindings) env)))
		(ronly-exprs! exprs env defs)))
	    (else
	     (ronly-exprs! expr env defs)))))
	 
   (with-access::Module mod (body defs)
      (for-each (lambda (expr)
		   (ronly-expr! expr '() defs))
	 body)
      (hashtable-for-each defs
	 (lambda (k d)
	    (with-access::Def d (ronly src kind)
	       (when (eq? ronly #unspecified)
		  (set! ronly #t)
		  (match-case src
		     ((define ?- (lambda . ?-))
		      (set! kind 'procedure))
		     ((define (?- . ?-) . ?-)
		      (set! kind 'procedure)))))))))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc msg obj container)
   (match-case (cond
		((epair? obj) (cer obj))
		((epair? container) (cer container))
		(else #f))
      ((at ?fname ?loc) (error/location "module5" msg obj fname loc))
      (else (error "module5" msg obj))))

;*---------------------------------------------------------------------*/
;*    for-each-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (for-each-expr proc lst)
   (let loop ((lst lst))
      (when (pair? lst)
	 (proc (car lst) lst)
	 (loop (cdr lst)))))

