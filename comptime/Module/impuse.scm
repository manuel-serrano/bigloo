;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/impuse.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 12:25:53 1996                          */
;*    Last change :  Fri Mar 27 13:37:22 2009 (serrano)                */
;*    Copyright   :  1996-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compilation of import/use/from clauses                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_impuse
   
   (include "Ast/unit.sch"
	    "Tools/trace.sch")
   
   (import  read_reader
	    backend_backend
	    module_module
	    module_prototype
	    module_class
	    module_include
	    module_checksum
	    tools_speek
	    tools_error
	    tools_location
	    tools_progn
	    type_type
	    object_class
	    read_access
	    read_inline
	    ast_var
	    ast_find-gdefs
	    ast_glo-decl
	    ast_ident
	    engine_param
	    init_main
	    expand_eps)

   (static  (class import
	       (module::symbol read-only)
	       (number::long (default -1))
	       (mode::symbol (default 'import))
	       (vars (default '()))
	       (checksum (default #unspecified))
	       (loc read-only)
	       (src read-only)
	       (decl (default #unspecified))
	       (provide::pair-nil (default '()))
	       (code (default #unspecified))
	       (access (default '()))))
	       
   (export  (make-import-compiler)
	    (make-use-compiler)
	    (make-from-compiler)
	    (get-imported-modules)
	    (import-with-module! ::symbol loc)
	    (import-parser ::symbol prototype . import-src)
	    (initialize-imported-modules::pair-nil ::procedure)))

;*---------------------------------------------------------------------*/
;*    make-import-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-import-compiler)
   (instantiate::ccomp
      (id 'import)
      (producer impuse-producer)))
 
;*---------------------------------------------------------------------*/
;*    make-use-compiler ...                                            */
;*---------------------------------------------------------------------*/
(define (make-use-compiler)
   (instantiate::ccomp
      (id 'use)
      (producer impuse-producer)
      (finalizer impuse-finalizer)))

;*---------------------------------------------------------------------*/
;*    make-from-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-from-compiler)
   (instantiate::ccomp
      (id 'from)
      (producer impuse-producer)
      (consumer (lambda (module clause)
		   (impuse-producer clause)
		   '()))))

;*---------------------------------------------------------------------*/
;*    *imports* ...                                                    */
;*---------------------------------------------------------------------*/
(define *imports* (make-hashtable))
(define *import-number* 0)

;*---------------------------------------------------------------------*/
;*    register-import! ...                                             */
;*---------------------------------------------------------------------*/
(define (register-import! import)
   (set! *import-number* (+fx 1 *import-number*))
   (import-number-set! import *import-number*)
   (hashtable-put! *imports* (import-module import) import))

;*---------------------------------------------------------------------*/
;*    impuse-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (impuse-producer clause)
   (let ((mode (car clause)))
      (match-case clause
	 ((?- . ?protos)
	  (for-each (lambda (proto) (impuse-parser proto mode clause)) protos))
	 (else
	  (user-error/location (find-location *module-clause*)
			       "Parse error"
			       (format "Illegal `~a' clause" mode)
			       clause
			       '())))))
   
;*---------------------------------------------------------------------*/
;*    import-all-module ...                                            */
;*---------------------------------------------------------------------*/
(define (import-all-module module::symbol mode src)
   (let ((mi (hashtable-get *imports* module)))
      (if (import? mi)
	  (import-vars-set! mi 'all)
	  (let ((loc (find-location/loc src (find-location *module-clause*))))
	     (register-import!
	      (instantiate::import
		 (module module)
		 (mode mode)
		 (vars 'all)
		 (loc loc)
		 (src src)))))))

;*---------------------------------------------------------------------*/
;*    import-1-module ...                                              */
;*---------------------------------------------------------------------*/
(define (import-1-module module::symbol var mode src)
   (let ((mi (hashtable-get *imports* module)))
      (if (import? mi)
	  ;; patch the previous import
	  (case (import-vars mi)
	     ((with)
	      (import-mode-set! mi mode)
	      (import-vars-set! mi (list var)))
	     ((all)
	      'nothing)
	     (else
	      (import-vars-set! mi (cons var (import-vars mi)))))
	  (let ((loc (find-location/loc src (find-location *module-clause*))))
	     ;; register a new import
	     (register-import!
	      (instantiate::import
		 (module module)
		 (mode mode)
		 (vars (list var))
		 (loc loc)
		 (src src)))))))

;*---------------------------------------------------------------------*/
;*    import-with-module! ...                                          */
;*---------------------------------------------------------------------*/
(define (import-with-module! module src)
   (let ((mi (hashtable-get *imports* module)))
      (unless (import? mi)
	 (let ((loc (find-location/loc src (find-location *module-clause*))))
	    (register-import!
	     (instantiate::import
		(module module)
		(mode 'with)
		(checksum 0)
		(loc loc)
		(src src)))))))

;*---------------------------------------------------------------------*/
;*    impuse-parser ...                                                */
;*    -------------------------------------------------------------    */
;*    The syntaxe of importation clause is:                            */
;*    import ::= module-name                                           */
;*            |  (module-name "file-name" *)                           */
;*            |  (variable module-name)                                */
;*            |  (variable module-name "file-name" *)                  */
;*---------------------------------------------------------------------*/
(define (impuse-parser prototype mode import-src)
   (trace (ast 2) "impuse-parser: " prototype " " mode #\Newline)
   (define (err)
      (user-error "Parse error"
		  (format "Illegal ~a clause" mode)
		  prototype
		  '()))
   (cond
      ((symbol? prototype)
       ;; module-name
       (import-all-module prototype mode import-src))
      ((list? prototype)
       (let ((inv (reverse prototype)))
	  (let loop ((lst inv)
		     (files '()))
	     (cond
		((not (pair? lst))
		 (err))
		((string? (car lst))
		 (loop (cdr lst) (cons (car lst) files)))
		((symbol? (car lst))
		 (let ((mod (car lst))
		       (vars (cdr lst)))
		    (cond
		       ((null? vars)
			;; (module-name "file-name"+)
			(if (pair? files) (module-add-access! mod files "."))
			(import-all-module mod mode prototype))
		       ((every? symbol? vars)
			;; (var1 var2 ... varN module-name "file-name"*)
			(if (pair? files) (module-add-access! mod files "."))
			(for-each (lambda (v)
				     (import-1-module mod v mode prototype))
				  vars))
		       (else
			(err)))))
		(else
		 (err))))))
      (else
       (err))))
   
;*---------------------------------------------------------------------*/
;*    get-imported-modules ...                                         */
;*---------------------------------------------------------------------*/
(define (get-imported-modules)
   (map import-module (hashtable->list *imports*)))

;*---------------------------------------------------------------------*/
;*    import-finalizer ...                                             */
;*    -------------------------------------------------------------    */
;*    @label importation unit@                                         */
;*    -------------------------------------------------------------    */
;*    In the `impuse' finalizer we read all the imported modules and   */
;*    if we have to, we create a unit in order to initialize imported  */
;*    modules.                                                         */
;*---------------------------------------------------------------------*/
(define (import-finalizer)
   ;; first read all the module declaration until nothing new is read
   (let loop ((num *import-number*))
      (hashtable-for-each *imports* (lambda (k i) (read-import! i)))
      (when (>fx *import-number* num)
	 (loop *import-number*)))
   ;; get the ordered list of imported modules
   (let ((imports (sort (lambda (i1 i2)
			   (<fx (import-number i1) (import-number i2)))
			(hashtable->list *imports*))))
      ;; import everything needed from imported modules
      (for-each (lambda (i)
		   (unless (or (eq? (import-mode i) 'with)
			       (eq? (import-decl i) 'error))
		      (import-module! i)))
		imports)
      ;; prepare the initialization functions
      (let ((inits (filter (lambda (i)
			      (memq (import-mode i) '(import with from)))
			   imports)))
	 (if (pair? inits)
	     (list (imported-modules-unit inits))
	     '()))))

;*---------------------------------------------------------------------*/
;*    *imported-modules-in-unit* ...                                   */
;*---------------------------------------------------------------------*/
(define *imported-modules-in-unit* '())

;*---------------------------------------------------------------------*/
;*    initialize-imported-modules ...                                  */
;*---------------------------------------------------------------------*/
(define (initialize-imported-modules get-init)
   (define (initialize-module import)
      (with-access::import import ((mod module) checksum)
	 (let* ((fun (get-init mod))
		(var (import-parser mod `(,fun checksum::long from::string))))
	    ;; module initializer can't be invoked
	    ;; from eval. We mark this.
	    (global-evaluable?-set! var #f)
	    `((@ ,fun ,mod) ,checksum ,(symbol->string *module*)))))
   (define (trace-initialize-module import call)
      `(begin
	  (pragma::void ,(format "bgl_init_module_debug_import(\"~a\", \"~a\")"
				 (symbol->string (get-init *module*))
				 (symbol->string (import-module import))))
	  ,call))
   (let* ((calls (map initialize-module *imported-modules-in-unit*)))
      (if (and (>fx *debug-module* 0)
	       (memq 'module (backend-debug-support (the-backend))))
	  `((begin
	       ,@(map trace-initialize-module *imported-modules-in-unit*
		      calls)))
	  calls)))
   
;*---------------------------------------------------------------------*/
;*    imported-modules-unit ...                                        */
;*---------------------------------------------------------------------*/
(define (imported-modules-unit imports)
   (set! *imported-modules-in-unit* imports)
   (let ((body (initialize-imported-modules module-initialization-id))
	 (priority (if (eq? *object-init-mode* 'staged) 22 12)))
      (unit 'imported-modules priority body #t #f)))

;*---------------------------------------------------------------------*/
;*    impuse-finalizer ...                                             */
;*    -------------------------------------------------------------    */
;*    In the `impuse' finalizer we read all the imported modules and   */
;*    if we have to, we create a unit in order to initialize imported  */
;*    modules.                                                         */
;*---------------------------------------------------------------------*/
(define (impuse-finalizer)
   (let* ((imports (import-finalizer))
	  (inlines (inline-finalizer))
	  (finalizers (append imports inlines)))
      (if (null? finalizers)
	  'void
	  finalizers)))

;*---------------------------------------------------------------------*/
;*    import-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (import-parser module::symbol prototype . import-src)
   (let ((proto (parse-prototype prototype))
	 (src   (if (pair? import-src)
		    (car import-src)
		    #f)))
      (if (not (pair? proto))
	  (user-error/location (find-location *module-clause*)
			       "Parse error"
			       "Illegal prototype"
			       prototype
			       '())
	  (case (car proto)
	     ((sfun sifun sgfun)
	      (declare-global-sfun! (cadr proto)
				    (caddr proto)
				    module
				    'import
				    (car proto)
				    prototype
				    src))
	     ((svar)
	      (declare-global-svar! (cadr proto) module	'import	prototype src))
	     ((class)
	      (declare-class! (cdr proto) module 'import #f #f prototype src))
	     ((abstract-class)
	      (declare-class! (cdr proto) module 'import #f #t prototype src))
	     ((final-class)
	      (declare-class! (cdr proto) module 'import #t #f prototype src))
	     ((wide-class)
	      (declare-wide-class! (cdr proto) module 'import prototype src))
	     ((define-macro)
	      (eval proto))
	     ((macro)
	      proto)
	     ((syntax)
	      proto)
	     ((expander)
	      proto)
	     (else
	      (user-error "Parse error" "Illegal prototype" prototype '()))))))

;*---------------------------------------------------------------------*/
;*    import-module! ...                                               */
;*---------------------------------------------------------------------*/
(define (import-module! import)
   (with-access::import import (module vars decl access code)
      (multiple-value-bind (inline macro syntax expander)
	 (if (pair? vars)
	     (import-wanted import)
	     (import-everything import))
	 (look-for-inlines-and-macros inline
				      macro
				      syntax
				      expander
				      code
				      (progn-tail-expressions decl)
				      access
				      module))))

;*---------------------------------------------------------------------*/
;*    read-import! ...                                                 */
;*---------------------------------------------------------------------*/
(define (read-import! import)
   (define (err obj)
      (import-decl-set! import 'error)
      (user-error/location (import-loc import)
			   'import
			   "Cannot find module"
			   obj
			   '()))
   (define (errfile module file)
      (import-decl-set! import 'error)
      (user-error/location
       (import-loc import)
       'import
       (format "Cannot open source file for module \"~a\"" module)
       file))
   (when (eq? (import-decl import) #unspecified)
      (let* ((module (import-module import))
	     (abase (map dirname *access-files*))
	     (fnames ((bigloo-module-resolver) module abase)))
	 (verbose 2 "      [reading "
		  (if (eq? (import-mode import) 'use) "used" "imported")
		  " module " module "]" #\Newline)
	 (if (not (pair? fnames))
	     (err module)
	     (let ((fname (find-file/path (car fnames) *load-path*)))
		(if (not (string? fname))
		    (errfile module (car fnames))
		    (let ((port (open-input-file fname)))
		       (import-access-set! import fnames)
		       (reader-reset!)
		       (if (not (input-port? port))
			   (errfile module (car fnames))
			   (unwind-protect
			      (let* ((mdecl (compiler-read port #t))
				     (emdecl (comptime-expand/error mdecl))
				     (mod (progn-first-expression emdecl)))
				 (reset-include-consumed-directive!)
				 (reset-include-consumed-code!)
				 (let* ((cm (consume-module! module mod))
					(cim (get-include-consumed-directive))
					(pro (append cm cim))
					(code (get-include-consumed-code))
					(check (module-checksum
						mod *mco-include-path*)))
				    (import-decl-set! import emdecl)
				    (import-checksum-set! import check)
				    (import-code-set! import code)
				    (import-provide-set! import pro)))
			      (close-input-port port))))))))))
   
;*---------------------------------------------------------------------*/
;*    import-everything ...                                            */
;*---------------------------------------------------------------------*/
(define (import-everything import)
   (with-access::import import (module provide src)
      (let loop ((provided provide)
		 (inline '())
		 (macro '())
		 (syntax '())
		 (expd '()))
	 (if (null? provided)
	     (values inline macro syntax expd)
	     (let ((p (import-parser module (car provided) src)))
		(match-case p
		   ((? global?)
		    (let ((val (global-value p)))
		       (loop (cdr provided)
			     (cond
				((or (not (global? p)) (not (sfun? val)))
				 inline)
				((eq? (sfun-class val) 'sifun)
				 (cons (cons (global-id p) 'sifun) inline))
				(else
				 inline))
			     macro
			     syntax
			     expd)))
		   ((macro . ?mac)
		    (loop (cdr provided) inline (cons mac macro) syntax expd))
		   ((syntax . ?syn)
		    (loop (cdr provided) inline macro (cons syn syntax) expd))
		   ((expander . ?exp)
		    (loop (cdr provided) inline macro syntax (cons exp expd)))
		   (else
		    (loop (cdr provided) inline macro syntax expd))))))))

;*---------------------------------------------------------------------*/
;*    import-wanted ...                                                */
;*---------------------------------------------------------------------*/
(define (import-wanted import)
   (with-access::import import (module provide src vars)
      (let loop ((provided provide)
		 (inline '())
		 (macro '())
		 (syntax '())
		 (expander '())
		 (wanted vars))
	 ;; we check that all wanted functions are in the list and at
	 ;; the same time, we compute the list of all inlines and macros
	 ;; to be fetched.
	 (cond
	    ((null? wanted) 
	     (values inline macro syntax expander))
	    ((null? provided)
	     (user-error/location (find-location *module-clause*)
				  (import-module import)
				  "Can't find export for these identifiers"
				  wanted
				  '()))
	    (else
	     (let ((proto (parse-prototype (car provided))))
		(if (pair? proto)
		    (let ((id (fast-id-of-id (cadr proto)
					     (find-location (car provided)))))
		       (if (not (memq id wanted))
			   (loop (cdr provided)
				 inline
				 macro
				 syntax
				 expander
				 wanted)
			   (let ((p (import-parser module (car provided) src)))
			      (match-case p
				 ((? global?)
				  (loop (cdr provided)
					(cond
					   ((eq? (car proto) 'sifun)
					    (cons (cons id 'sifun) inline))
					   (else
					    inline))
					macro
					syntax
					expander
					(remq! id wanted)))
				 ((? type?)
				  (loop (cdr provided)
					inline
					macro
					syntax
					expander
					(remq! id wanted)))
				 ((macro . ?mac)
				  (loop (cdr provided)
					inline
					(cons mac macro)
					syntax
					expander
					(remq! id wanted)))
				 ((syntax  . ?syn)
				  (loop (cdr provided)
					inline
					macro
					(cons syn syntax)
					expander
					(remq! id wanted)))
				 ((expander . ?exp)
				  (loop (cdr provided)
					inline
					macro
					syntax
					(cons exp expander)
					(remq! id wanted)))
				 (else
				  (loop (cdr provided)
					inline
					macro
					syntax
					expander
					(remq! id wanted)))))))
		    (loop (cdr provided)
			  inline
			  macro
			  syntax
			  expander
			  wanted))))))))

