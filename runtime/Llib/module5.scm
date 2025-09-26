;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Llib/module5.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Fri Sep 12 07:29:51 2025                          */
;*    Last change :  Fri Sep 26 07:21:18 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    module5 parser                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __module5

   (include "Llib/class.sch")
   
   (import  __error
	    __object
	    __configure
	    __reader
	    __hash
	    __library
	    __binary
	    __macro
	    __eval
	    __expand
	    __class)

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
	    __evobject
	    
	    __param
	    __trace

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
	    __r4_output_6_10_3
	    __r4_ports_6_10_1

	    __r5_control_features_6_4)
   
   (export (class Module
	      (id::symbol read-only)
	      (path::bstring read-only)
	      (checksum::long (default -1))
	      (decls read-only (default (create-hashtable :weak 'open-string)))
	      (exports read-only (default (create-hashtable :weak 'open-string)))
	      (defs read-only (default (create-hashtable :weak 'open-string)))
	      (classes read-only (default (create-hashtable :weak 'open-string)))
	      (main (default #f))
	      (inits::pair-nil (default '()))
	      (libraries::pair-nil (default '()))
	      (body::obj (default '()))
	      (resolved::bool (default #f)))
	   
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

	   (class KDef::Def
	      (index::long read-only)
	      (registration read-only)
	      (super read-only)
	      (ctor read-only)
	      (kkind::symbol read-only)
	      (properties::pair-nil read-only))

	   (class Kprop
	      (id::symbol read-only)
	      (type read-only)
	      (class read-only)
	      (defv?::bool read-only)
	      (ronly?::bool read-only)
	      (virtual?::bool read-only)
	      (get read-only)
	      (set read-only)
	      (value read-only)
	      (src read-only))
	   
	   (module5-register-plugin! ::bstring ::procedure)
	   (module5-resolve-path ::bstring ::bstring)
	   (module5-resolve-library ::symbol ::pair-nil)
	   (module5-read::Module ::bstring #!key lib-path cache-path expand)
	   (module5-read-library::Module ::bstring)
	   (module5-read-heap::Module ::bstring)
	   (module5-write-heap ::bstring ::Module)
	   (module5-parse::Module ::obj ::bstring #!key lib-path cache-path expand)
	   ;; (module5-expand::Module ::Module expand)
	   ;; (module5-expand-exports!::Module ::Module expand)
	   (module5-expander::obj ::obj ::procedure)
	   (module5-expand-and-resolve!::Module ::Module ::obj)
	   (module5-checksum!::Module ::Module)
	   (module5-get-decl::Decl ::Module ::symbol)
	   (module5-get-def::Def ::Module ::symbol ::obj)
	   (module5-get-export-def ::Module ::symbol)
	   (module-get-class ::Module ::symbol)))

;*---------------------------------------------------------------------*/
;*    object-write ::Module ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-write m::Module . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Module id=~a path=~s resolved=~a>" (-> m id) (-> m path) (-> m resolved)))

;*---------------------------------------------------------------------*/
;*    object-display ::Module ...                                      */
;*---------------------------------------------------------------------*/
(define-method (object-display m::Module . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Module id=~a path=~s resolved=~a>" (-> m id) (-> m path) (-> m resolved)))

;*---------------------------------------------------------------------*/
;*    object-print ::Module ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-print m::Module port ds)
   (object-write m port))
   
;*---------------------------------------------------------------------*/
;*    object-write ::Decl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (object-write d::Decl . port)
   (let ((m::Module (-> d mod)))
      (fprintf (if (pair? port) (car port) (current-output-port))
	 "#<Decl ~a/~a mod=~a scope=~a ronly=~a def=~a>"
	 (-> d id) (-> d alias) (-> m id) (-> d scope) (-> d ronly) (typeof (-> d def)))))

;*---------------------------------------------------------------------*/
;*    object-display ::Decl ...                                        */
;*---------------------------------------------------------------------*/
(define-method (object-display d::Decl . port)
   (let ((m::Module (-> d mod)))
      (fprintf (if (pair? port) (car port) (current-output-port))
	 "#<Decl ~a/~a mod=~a scope=~a ronly=~a def=~a>"
	 (-> d id) (-> d alias) (-> m id) (-> d scope) (-> d ronly) (typeof (-> d def)))))

;*---------------------------------------------------------------------*/
;*    object-print ::Decl ...                                          */
;*---------------------------------------------------------------------*/
(define-method (object-print d::Decl port ds)
   (object-write d port))

;*---------------------------------------------------------------------*/
;*    object-write ::Def ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-write d::Def . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Def ~a kind=~a ronly=~a>"
      (-> d id) (-> d kind) (-> d ronly)))

;*---------------------------------------------------------------------*/
;*    object-display ::Def ...                                         */
;*---------------------------------------------------------------------*/
(define-method (object-display d::Def . port)
   (fprintf (if (pair? port) (car port) (current-output-port))
      "#<Def ~a kind=~a ronly=~a>"
      (-> d id) (-> d kind) (-> d ronly)))

;*---------------------------------------------------------------------*/
;*    object-print ::Def ...                                           */
;*---------------------------------------------------------------------*/
(define-method (object-print d::Def port ds)
   (object-write d port))

;*---------------------------------------------------------------------*/
;*    *modules-by-path* ...                                            */
;*---------------------------------------------------------------------*/
(define module-mutex (make-mutex "modules"))
(define *modules-by-path* (create-hashtable :weak 'open-string))
(define *modules-by-id* (create-hashtable :weak 'open-string))
(define *plugins* '())

;*---------------------------------------------------------------------*/
;*    module5-register-plugin! ...                                     */
;*---------------------------------------------------------------------*/
(define (module5-register-plugin! name::bstring plugin::procedure)
   (synchronize module-mutex
      (if (assoc name *plugins*)
	  (error "module5-register-plugin!" "Plugin already registered" name)
	  (set! *plugins* (cons (cons name plugin) *plugins*)))))

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
(define (module5-read path::bstring #!key lib-path cache-path expand)
   (with-trace 'module5 "module5-read"
      (trace-item "path=" path)
      (trace-item "expand=" expand)
      (synchronize module-mutex
	 (or (hashtable-get *modules-by-path* path)
	     (let ((exprs (call-with-input-file path
			     (lambda (p) (port->sexp-list p #t)))))
		(if (null? exprs)
		    (error "module5-read" "Missing module clause" path)
		    (let ((nmod::Module (module5-parse (car exprs) path
					   :lib-path lib-path
					   :cache-path cache-path
					   :expand expand)))
		       (set! (-> nmod body) (cdr exprs))
		       nmod)))))))

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
		 (error/loc #f (format "Illegal library heap \"~a\"" path) heap init)))))
	 (else
	  (error/loc #f "Illegal library" path init)))))

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
(define (module5-parse::Module expr path::bstring #!key lib-path cache-path expand)

   (define (parse id path clauses)
      (let ((mod (instantiate::Module
		    (id id)
		    (path path))))
	 (hashtable-put! *modules-by-path* path mod)
	 (let ((omod (hashtable-get *modules-by-id* (symbol->string id))))
	    (if omod
		(with-access::Module omod ((opath path))
		   (error/loc mod
		      (format "Module \"~a\" has already been declared in file ~s"
			 id opath)
		      path expr))
		(hashtable-put! *modules-by-id* (symbol->string id) mod)))
	 (for-each (lambda (c)
		      (module5-parse-clause c expr mod lib-path cache-path expand))
	    clauses)
	 (with-access::Module mod (inits)
	    (set! inits (delete-duplicates! inits)))
	 mod))
   
   (with-trace 'module5-parse "module5-parse"
      (trace-item "path=" path)
      (trace-item "expr=" expr)
      (let ((eexpr (if expand (expand expr) expr)))
	 (trace-item "eexpr=" eexpr)
	 (match-case eexpr
	    ((module (and (? symbol?) ?id) :version 5 . ?clauses)
	     (parse id path clauses))
	    ((module (and (? symbol?) ?id) . ?clauses)
	     (parse id path clauses))
	    (else
	     (error/loc #f "Illegal expression" expr #f))))))

;*---------------------------------------------------------------------*/
;*    module5-parse-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (module5-parse-clause clause expr::pair mod::Module lib-path cache-path expand)

   (define (unbound-error path id clause)
      (error/loc mod (format "Cannot find declaration in module \"~a\"" path)
	 id clause))

   (define (scope-error path id clause)
      (error/loc mod (format "Module \"~a\" does not export" path)
	 id clause))

   (define (hashtable-symbol-get table id)
      (hashtable-get table (symbol->string! id)))

   (define (hashtable-symbol-put! table id d)
      (hashtable-put! table (symbol->string! id) d))

   (define (parse-import-binding b imod::Module expr::pair mod::Module expand)
      (match-case b
	 ((? symbol?)
	  (let ((idecl (hashtable-symbol-get (-> imod exports) b)))
	     (if (isa? idecl Decl)
		 (let ((d (duplicate::Decl idecl
			     (id b)
			     (alias b)
			     (scope 'import))))
		    (hashtable-symbol-put! (-> mod decls) b d)
		    d)
		 (unbound-error (-> imod path) b clause))))
	 (((and (? symbol?) ?alias) (and (? symbol?) ?id))
	  (let ((idecl (hashtable-symbol-get (-> imod exports) id)))
	     (if (isa? idecl Decl)
		 (let ((d (duplicate::Decl idecl
			     (id id)
			     (alias alias)
			     (scope 'import))))
		    (hashtable-symbol-put! (-> mod decls) alias d)
		    d)
		 (unbound-error (-> imod path) b clause))))
	 (else
	  (error/loc mod "Illegal import binding" b clause))))
   
   (define (parse-reexport-all clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (bindings (cddr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path
				    :cache-path cache-path
				    :expand expand)))
		(hashtable-for-each (-> imod exports)
		   (lambda (key d::Decl)
		      (when (eq? (-> d scope) 'export)
			 (hashtable-put! (-> mod exports) key d))))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" (cadr clause) clause))))
   
   (define (parse-reexport-some clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (bindings (cddr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path
				    :cache-path cache-path
				    :expand expand)))
		(for-each (lambda (b)
			     (let ((d::Decl (parse-import-binding b
					       imod expr mod expand)))
				(with-access::Decl d (scope id alias (dmod mod))
				   (hashtable-put! (-> mod exports)
				      (symbol->string! alias)
				      d))
				d))
		   bindings)
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" (cadr clause) clause))))
   
   (define (parse-import-init clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path
				    :cache-path cache-path
				    :expand expand)))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" path clause))))
   
   (define (parse-import-all id clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path
				    :cache-path cache-path
				    :expand expand)))
		(hashtable-for-each (-> imod exports)
		   (lambda (key d::Decl)
		      (let* ((alias (if id
					(string->symbol
					   (format "~a.~a" id (-> d alias)))
					(-> d alias)))
			     (nd (duplicate::Decl d
				    (alias alias)
				    (scope 'import))))
			 (hashtable-put! (-> mod decls) key nd))))
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" path clause))))
   
   (define (parse-import-some clause::pair expr::pair mod::Module expand)
      (let* ((path (cadr clause))
	     (bindings (cddr clause))
	     (rfrom (module5-resolve-path path (-> mod path))))
	 (if (string? rfrom)
	     (let ((imod::Module (module5-read rfrom
				    :lib-path lib-path
				    :cache-path cache-path
				    :expand expand)))
		(for-each (lambda (b)
			     (parse-import-binding b imod expr mod expand))
		   bindings)
		(set! (-> mod inits) (append! (-> mod inits) (list imod))))
	     (error/loc mod "Cannot find file" path clause))))

   (define (parse-export clause expr::pair mod::Module expand)
      (for-each-expr (lambda (expr src)
			(match-case expr
			   ((and ?id (? symbol?))
			    (let ((decl (instantiate::Decl
					   (id id)
					   (alias id)
					   (mod mod)
					   (scope 'export)
					   (src src))))
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) id decl)))
			   (((and ?alias (? symbol?)) (and ?id (? symbol?)))
			    (let ((decl (instantiate::Decl
					   (id id)
					   (alias alias)
					   (mod mod)
					   (scope 'export)
					   (src expr))))
			       (hashtable-symbol-put! (-> mod decls) id decl)
			       (hashtable-symbol-put! (-> mod exports) alias decl)))
			   (else
			    (error/loc mod "Illegal export clause" clause expr))))
	 (cdr clause)))

   (define (parse-main id expr::pair mod::Module expand)
      (set! (-> mod main) id))

   (define (parse-include clause expr::pair mod::Module expand)
      (for-each (lambda (f)
		   (cond
		      ((not (string? f))
		       (error/loc mod "Illegal include clause" f clause))
		      ((module5-resolve-path f (-> mod path))
		       (call-with-input-file f
			  (lambda (p)
			     (for-each (lambda (c)
					  (module5-parse-clause c clause mod
					     lib-path cache-path expand))
				(port->sexp-list p #t)))))
		      (else
		       (error/loc mod "Cannot find file" f clause))))
	 (cdr clause)))

   (define (parse-cond-expand clause expr::pair mod::Module expand)
      (for-each (lambda (c)
		   (module5-parse-clause c clause mod lib-path cache-path expand))
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
	     (error/loc mod "Cannot find library" lib clause))))
   
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
	     (error/loc mod "Cannot find library" lib clause))))

   (define (parse-extern clause expr::pair mod::Module expand)
      (let* ((name (cadr clause))
	     (plugin (assoc name *plugins*)))
	 (if plugin
	     ((cdr plugin) mod clause) 
	     (error/loc mod "No extern plugin" name clause))))
      
   (with-trace 'module5-parse "module5-parse-clause"
      (trace-item "clause=" clause)
      (trace-item "lib-path=" lib-path)
      (match-case clause
	 ((export (? string?))
	  (parse-reexport-all clause expr mod expand))
	 ((export (? string?) . ?-)
	  (parse-reexport-some clause expr mod expand))
	 ((import (? string?))
	  (parse-import-all #f clause expr mod expand))
	 ((import (? string?) ())
	  (parse-import-init clause expr mod expand))
	 ((import (? string?) ((and (? symbol?) ?id)))
	  (parse-import-all id clause expr mod expand))
	 ((import (? string?) . ?-)
	  (parse-import-some clause expr mod expand))
	 ((export . ?bindings)
	  (parse-export clause expr mod expand))
	 ((main)
	  (parse-main 'main expr mod expand))
	 ((main (and (? symbol?) ?main))
	  (parse-main main expr mod expand))
	 ((include . ?-)
	  (parse-include clause expr mod expand))
	 ((library (? symbol?))
	  (parse-library-all clause expr mod expand))
	 ((library (? symbol?) . ?-)
	  (parse-library-some clause expr mod expand))
	 ((extern (and (? string?) ?name) . ?clauses)
	  (parse-extern clause expr mod expand))
	 ((cond-expand . ?-)
	  (tprint "IN MODULE5 cond-expand...")
	  (parse-cond-expand clause expr mod expand))
	 (else
	  (error/loc mod "Illegal module clause" clause expr)))))

;*---------------------------------------------------------------------*/
;*    module5-expand-exports! ...                                      */
;*---------------------------------------------------------------------*/
;* (define (module5-expand-exports! mod::Module expand)                */
;*    (with-access::Module mod (id exports resolved)                   */
;* 	 ((parsed)                                                     */
;* 	  (with-trace 'module5 "module5-expand-exports!"               */
;* 	     (trace-item mod)                                          */
;* 	     (set! state 'expanded)                                    */
;* 	     (hashtable-for-each exports                               */
;* 		(lambda (k d::Decl)                                    */
;* 		   (unless (eq? (-> d mod) mod)                        */
;* 		      (module5-expand-exports! (-> d mod) expand))))))) */
;*       mod))                                                         */

;*---------------------------------------------------------------------*/
;*    module5-expander ...                                             */
;*---------------------------------------------------------------------*/
(define (module5-expander x e)
   (match-case x
      ((module ?- . ?rest)
       ;; module5 form
       (set-cdr! (cdr x)
	  (map (lambda (x)
		  (match-case x
		     ((import ?from ())
		      ;; treat import specially for the syntax () that
		      ;; would trigger a syntax error otherwise
		      (set-car! (cdr x) (e from e))
		      x)
		     (else
		      (e x e))))
	     rest))
       x)
      (else
       x)))

;*---------------------------------------------------------------------*/
;*    module5-expand-and-resolve! ...                                  */
;*---------------------------------------------------------------------*/
(define (module5-expand-and-resolve! mod::Module new-xenv)
   (unless (-> mod resolved)
      (with-trace 'module5 "module5-resolve!"
	 (trace-item mod)
	 (trace-item "decls="
	    (hashtable-map (-> mod decls)
	       (lambda (k d::Decl)
		  (format "~a/~a" (-> d id) (-> d alias)))))
	 (set! (-> mod resolved) #t)
	 (let* ((xenv (if (procedure? new-xenv) (new-xenv) new-xenv))
		(kx (make-class-expander mod xenv)))
	    (install-module5-expander xenv 'define-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'define-wide-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'define-final-class
	       '(define-class) kx)
	    (install-module5-expander xenv 'define-abstract-class
	       '(define-class) kx)
	    (hashtable-for-each (-> mod decls)
	       (lambda (k d::Decl)
		  (with-access::Decl d ((imod mod) alias id)
		     (unless (eq? imod mod)
			(module5-expand-and-resolve! (-> d mod)
			   (lambda ()
			      (create-hashtable :weak 'open-string)))
			(let ((def (module5-get-export-def imod id)))
			   (with-access::Def def (kind src)
			      (case kind
				 ((macro)
				  (trace-item "bind-macro alias="
				     alias " id=" id)
				  (install-module5-expander xenv alias src
				     (eval! (macro->expander src))))
				 ((expander)
				  (trace-item "bind-expander alias="
				     alias " id=" id)
				  (install-module5-expander xenv alias src
				     (eval! (caddr src)))))))))))
	    (when (pair? (-> mod body))
	       (trace-item "body avant-expand=" (-> mod body))
	       (set! (-> mod body)
		  (map (lambda (x) (expand/env x xenv)) (-> mod body)))
	       (trace-item "body apres-expand=" (-> mod body)))
	    ;; Macro and class definitions are disgarded by the macro-expansion.
	    ;; Because these definitions are needed to resolve the module
	    ;; exports, INSTALL-MODULE5-EXPANDER (runtime/macro.scm),
	    ;; stores these definition in XENV.
	    (let ((dm (hashtable-map xenv (lambda (k e) (car e)))))
	       (collect-defines! mod (filter (lambda (x) x) dm)))
	    (collect-defines! mod (-> mod body))
	    (collect-classes! mod)
	    ;; bind all the classes
	    (check-unbounds mod))
	 (ronly! mod)
	 (trace-item "exports="
	    (hashtable-map (-> mod exports) (lambda (k d) d)))
	 (trace-item "defs="
	    (hashtable-map (-> mod defs) (lambda (k d) d)))))
   mod)

;*---------------------------------------------------------------------*/
;*    check-unbounds ...                                               */
;*---------------------------------------------------------------------*/
(define (check-unbounds mod::Module)
   (with-access::Module mod (decls (mid id) defs)
      (let ((unbounds '()))
	 (hashtable-for-each decls
	    (lambda (k d)
	       (with-access::Decl d (def id scope (dmod mod))
		  (unless (or (isa? def Def) (not (eq? dmod mod)))
		     (when (or (eq? scope 'export) (eq? scope 'static))
			(set! unbounds (cons d unbounds)))))))
	 (when (pair? unbounds)
	    (for-each (lambda (d)
			 (with-access::Decl d (id src)
			    (with-handler
			       exception-notify
			       (error/loc mod "Cannot find definition"
				  id src))))
	       (reverse! unbounds))
	    (error mid "Unbound exported identifier"
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
		 (if (=fx i (-fx l 1))
		     (error/loc #f "Illegal identifier" id src)
		     (values (string->symbol (substring s 0 i))
			(substring s (+fx i 2))))
		 (loop (+fx i 2))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    collect-defines! ...                                             */
;*---------------------------------------------------------------------*/
(define (collect-defines! mod body)
   
   (define (module-define! mod kind::symbol id::symbol type src)
      (with-access::Module mod (defs decls)
	 (let* ((name (symbol->string! id))
		(old (hashtable-get defs name))
		(decl (hashtable-get decls name)))
	    (if old
		(error/loc mod
		   (format "Identifier ~s has already been declared" name)
		   (with-access::Def old (src) src)
		   (with-access::Decl decl (src) src))
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
	 ((define-macro ((and (? symbol?) ?name) . ?-) . ?-)
	  (module-define! mod 'macro name #unspecified expr))
	 ((define-expander (and (? symbol?) ?name) . ?-)
	  (module-define! mod 'expander name #unspecified expr))
	 ((begin . ?exprs)
	  (collect-defines! mod exprs))))

   (for-each (lambda (expr) (collect-define! mod expr)) body))

;*---------------------------------------------------------------------*/
;*    collect-classes! ...                                             */
;*---------------------------------------------------------------------*/
(define (collect-classes! mod::Module)

   (define (klass-def ci)
      (let ((id (class-info-id ci)))
	 (instantiate::KDef
	    (id id)
	    (index (class-info-index ci))
	    (type 'class)
	    (kind 'class)
	    (ronly #t)
	    (ctor (class-info-ctor ci))
	    (src (class-info-src ci))
	    (registration (class-info-registration ci))
	    (super (when (class-info-super ci)
		      (class-info-id (class-info-super ci))))
	    (kkind (class-info-kind ci))
	    (properties (filter-map (lambda (p)
				       (when (eq? (prop-info-class p) id)
					  `((id . ,(prop-info-id p))
					    (src . ,(prop-info-src p))
					    (type . ,(prop-info-type p))
					    (ronly . ,(prop-info-ronly? p))
					    (defvalue . ,(prop-info-defv? p))
					    (value . ,(prop-info-value p))
					    (get . ,(prop-info-get p))
					    (set . ,(prop-info-set p)))))
			   (class-info-properties ci))))))
      
   (hashtable-for-each (-> mod classes)
      (lambda (k ci)
	 (with-access::Module mod (defs decls)
	    (let* ((name (symbol->string! (class-info-id ci)))
		   (old (hashtable-get defs name))
		   (decl (hashtable-get decls name)))
	       (if old
		   (error/loc mod
		      (format "Identifier ~s has already been declared" name)
		      (with-access::Def old (src) src)
		      (with-access::Decl decl (src) src))
		   (let ((def (klass-def ci)))
		      (hashtable-put! defs name def)
		      (when decl
			 (with-access::Decl decl (scope ronly (ddef def))
			    (set! ronly #t)
			    (with-access::Def def ((ddecl decl))
			       (case scope
				  ((export)
				   (set! ddef def)
				   (set! ddecl decl))
				  ((static)
				   (set! ddecl decl))))))
		      def)))))))

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
			       (error/loc mod "Illegal assignment" id expr))
			    (set! ronly #f)))))))
	    ((define (and (? symbol?) ?id) ?expr)
	     (ronly-expr! expr env defs))
	    ((define ((and (? symbol?) ?id) . ?args) . ?body)
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-inline ((and (? symbol?) ?id) . ?args) . ?body)
	     (multiple-value-bind (name type)
		(parse-ident id (cadr expr))
		(let ((def (hashtable-get defs (symbol->string! name))))
		   (if (isa? def Def)
		       (with-access::Def def (ronly kind)
			  (unless (eq? ronly #unspecified)
			     (error/loc mod "Illegally mutated inline function"
				id expr))
			  (begin
			     (set! ronly #t)
			     (set! kind 'inline))))))
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-generic ((and (? symbol?) ?id) . ?args) . ?body)
	     (multiple-value-bind (name type)
		(parse-ident id (cadr expr))
		(let ((def (hashtable-get defs (symbol->string! name))))
		   (if (isa? def Def)
		       (with-access::Def def (ronly kind)
			  (unless (eq? ronly #unspecified)
			     (error/loc mod "Illegally mutated generic function"
				id expr))
			  (begin
			     (set! ronly #t)
			     (set! kind 'generic))))))
	     (ronly-exprs! body (append (args-id args) env) defs))
	    ((define-class (and (? symbol?) ?id) . ?-)
	     (multiple-value-bind (name type)
		(parse-ident id (cdr expr))
		(let ((def (hashtable-get defs (symbol->string! name))))
		   (if (isa? def Def)
		       (with-access::Def def (ronly kind)
			  (unless (eq? ronly #unspecified)
			     (error/loc mod "Illegally mutated class"
				id expr))
			  (begin
			     (set! ronly #t)
			     (set! kind 'class))))))
	     '(ronly-exprs! body (append (args-id args) env) defs))
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
;*    module5-checksum ...                                             */
;*---------------------------------------------------------------------*/
(define (module5-checksum! mod::Module)
   
   (define (add-hash n checksum)
      (bit-and #xffffff (+fx n checksum)))
   
   (define (scope-number scope)
      (case scope
	 ((export) 124)
	 ((static) 23544)
	 ((export) 33)
	 (else 8843)))
   
   (define (kind-number kind)
      (case kind
	 ((procedure) 98)
	 ((inline) 2443)
	 ((generic) 223)
	 ((class) 33455)
	 ((variable) 8739284)
	 (else 3493)))
   
   (with-access::Module mod (defs decls checksum)
      (when (<fx checksum 0)
	 (let ((cs (+fx (hashtable-size defs) (hashtable-size decls))))
	    (hashtable-for-each decls
	       (lambda (k d)
		  (with-access::Decl d (alias scope)
		     (set! cs (add-hash (scope-number scope) cs))
		     
		     (set! cs (add-hash (get-hashnumber k) cs))
		     (set! cs (add-hash (get-hashnumber alias) cs)))))
	    (hashtable-for-each defs
	       (lambda (k d)
		  (with-access::Def d (kind)
		     (set! cs (add-hash (+fx 3 (get-hashnumber k)) cs))
		     (set! cs (add-hash (kind-number kind) cs)))))
	    (set! checksum cs)))
      mod))

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
;*    for-each-expr ...                                                */
;*---------------------------------------------------------------------*/
(define (for-each-expr proc lst)
   (let loop ((lst lst))
      (when (pair? lst)
	 (proc (car lst) lst)
	 (loop (cdr lst)))))

;*---------------------------------------------------------------------*/
;*    module5-get-decl ...                                             */
;*---------------------------------------------------------------------*/
(define (module5-get-decl mod::Module id)
   (with-access::Module mod (decls (mid id))
      (let ((decl (hashtable-get decls (symbol->string! id))))
	 (if (isa? decl Decl)
	     decl
	     (error "module5-get-decl"
		(format "Cannot find declaration \"~a\"" id) id)))))

;*---------------------------------------------------------------------*/
;*    module5-get-def ...                                              */
;*---------------------------------------------------------------------*/
(define (module5-get-def mod::Module id src)
   (with-access::Module mod (defs decls (mid id) resolved)
      (unless resolved
	 (error/loc mod "Module definitions not resolved yet" id #f))
      (let ((def (hashtable-get defs (symbol->string! id))))
	 (if (isa? def Def)
	     def
	     (error/loc mod "Cannot find definition" id src)))))

;*---------------------------------------------------------------------*/
;*    module5-get-export-def ...                                       */
;*---------------------------------------------------------------------*/
(define (module5-get-export-def mod::Module id)
   (with-access::Module mod (exports (mid id) resolved defs decls)
      (unless resolved
	 (error/loc mod "Module definitions not resolved yet" id #f))
      (let ((decl (hashtable-get exports (symbol->string! id))))
	 (if (isa? decl Decl)
	     (with-access::Decl decl (def src (dmod mod))
		(if (isa? def Def)
		    def
		    (error/loc mod "Cannot find definition" id src)))
	     (error/loc mod "Cannot find declaration" id #f)))))

;*---------------------------------------------------------------------*/
;*    module5-expand-class ...                                         */
;*---------------------------------------------------------------------*/
(define (make-class-expander mod::Module xenv)
   (lambda (x e)
      (let ((ci (parse-class x)))
	 ;; bind the class in the module
	 (let ((o (module-get-class mod (class-info-id ci))))
	    (if o
		(error/loc mod
		   (format "Class \"~a\" has already bin declared in module ~a"
		      (class-info-id ci) (-> mod id))
		   x x)
		(module-bind-class! mod (class-info-id ci) ci)))
	 ;; check the super class
	 (when (class-info-super ci)
	    (let ((si (module-get-class mod (class-info-super ci))))
	       (if si
		   ;; update the super class info and add additional props
		   (begin
		      (class-info-super-set! ci si)
		      (class-info-properties-set! ci
			 (append (class-info-properties si)
			    (class-info-properties ci))))
		   (error/loc mod
		      (format "Cannot find super class of \"~a\""
			 (class-info-id ci))
		      (class-info-super ci) x))))
	 ;; install the expanders
	 (install-module5-expander xenv
	    (string->symbol (format "instantiate::~a" (class-info-id ci)))
	    #f (instantiate-expander ci))
	 (install-module5-expander xenv
	    (string->symbol (format "with-access::~a" (class-info-id ci)))
	    #f (with-access-expander ci))
	 ;; expanded class registration form
	 (class-info-registration-set! ci (e (registration-expand ci mod) e))
	 #unspecified)))

;*---------------------------------------------------------------------*/
;*    module-get-class ...                                             */
;*---------------------------------------------------------------------*/
(define (module-get-class mod::Module id::symbol)
   (hashtable-get (-> mod classes) (symbol->string! id)))

;*---------------------------------------------------------------------*/
;*    module-bind-class! ...                                           */
;*---------------------------------------------------------------------*/
(define (module-bind-class! mod::Module id::symbol ci)
   (class-info-index-set! ci (hashtable-size (-> mod classes)))
   (hashtable-put! (-> mod classes) (symbol->string! id) ci))
