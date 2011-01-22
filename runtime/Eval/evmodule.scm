;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evmodule.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 17 09:40:04 2006                          */
;*    Last change :  Sat Jan 22 15:49:24 2011 (serrano)                */
;*    Copyright   :  2006-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Eval module management                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evmodule
   
   (import  __type
	    __error
	    __bigloo
	    __module
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __bit
	    __param
	    __object
	    __hash
	    __reader
	    __macro
	    __expand
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    
	    __r5_control_features_6_4
	    
	    __progn
	    __thread
	    
	    __eval
	    __evenv
	    __everror
	    __evcompile
	    __evobject)

   (extern  (macro $eval-module-set!::obj (::obj) "BGL_MODULE_SET")
	    (macro $eval-module::obj () "BGL_MODULE"))

   (java    (class foreign
	       (method static $eval-module-set!::obj (::obj) "BGL_MODULE_SET")
	       (method static $eval-module::obj () "BGL_MODULE")))

   (export  evmodule-uninitialized
	    (evmodule?::bool ::obj)
	    (evmodule-name::symbol ::obj)
	    (evmodule-find-global ::obj ::symbol)
	    (evmodule-bind-global! ::obj ::symbol ::obj)
	    (evmodule-macro-table ::obj)
	    (evmodule-extension ::obj)
	    (evmodule ::pair-nil ::obj)
	    (eval-find-module ::symbol)
	    (eval-module)
	    (eval-module-set! ::obj)
	    (evmodule-static-class ::pair-nil))

   (option  (set! *unsafe-type* #f)))

;*---------------------------------------------------------------------*/
;*    *modules-mutex* ...                                              */
;*---------------------------------------------------------------------*/
(define *modules-mutex* (make-mutex "modules"))
(define *loadq-mutex* (make-mutex "loadq"))

;*---------------------------------------------------------------------*/
;*    evmodule-uninitialized ...                                       */
;*---------------------------------------------------------------------*/
(define evmodule-uninitialized
   '|#uninitialized|)

;*---------------------------------------------------------------------*/
;*    *modules-table* ...                                              */
;*---------------------------------------------------------------------*/
(define *modules-table* '())

;*---------------------------------------------------------------------*/
;*    %evmodule ...                                                    */
;*---------------------------------------------------------------------*/
(define-struct %evmodule key id path env exports macros extension)

;*---------------------------------------------------------------------*/
;*    evmodule? ...                                                    */
;*---------------------------------------------------------------------*/
(define (evmodule? obj)
   (and (%evmodule? obj) (eq? (%evmodule-key obj) make-%evmodule)))

;*---------------------------------------------------------------------*/
;*    evmodule-name ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-name mod)
   (if (evmodule? mod)
       (%evmodule-id mod)
       (bigloo-type-error 'evmodule-name 'module mod)))

;*---------------------------------------------------------------------*/
;*    evmodule-macro-table ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-macro-table mod)
   (if (evmodule? mod)
       (%evmodule-macros mod)
       (bigloo-type-error 'evmodule-macro-table 'module mod)))

;*---------------------------------------------------------------------*/
;*    evmodule-extension ...                                           */
;*---------------------------------------------------------------------*/
(define (evmodule-extension mod)
   (if (evmodule? mod)
       (%evmodule-extension mod)
       (bigloo-type-error 'evmodule-extension 'module mod)))

;*---------------------------------------------------------------------*/
;*    make-evmodule ...                                                */
;*---------------------------------------------------------------------*/
(define (make-evmodule id path loc)
   (mutex-lock! *modules-mutex*)
   (let* ((env (make-hashtable 100 #unspecified eq?))
	  (mactable (make-hashtable 64))
	  (mod (%evmodule make-%evmodule id path env '() mactable '())))
      (if (not (hashtable? *modules-table*))
	  (begin
	     (set! *modules-table* (make-hashtable 100))
	     (hashtable-put! *modules-table* id mod))
	  (let ((old (hashtable-get *modules-table* id)))
	     (if old
		 (begin
		    (hashtable-update! *modules-table* id (lambda (v) mod) mod)
		    (unless (string=? (%evmodule-path old) path)
		       (let ((msg (string-append "Module redefinition `"
						 (symbol->string id)
						 "'. Previous \""
						 (%evmodule-path old)
						 "\", new (ignored) \""
						 path "\"")))
			  (warning/loc loc msg))))
		 (hashtable-put! *modules-table* id mod))))
      (mutex-unlock! *modules-mutex*)
      mod))

;*---------------------------------------------------------------------*/
;*    eval-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-module)
   ($eval-module))

;*---------------------------------------------------------------------*/
;*    eval-module-set! ...                                             */
;*---------------------------------------------------------------------*/
(define (eval-module-set! mod)
   (if (or (evmodule? mod)
	   (eq? mod (interaction-environment))
	   (eq? mod #unspecified))
       ($eval-module-set! mod)
       (error 'eval-module-set! "Illegal module" mod)))

;*---------------------------------------------------------------------*/
;*    eval-find-module ...                                             */
;*---------------------------------------------------------------------*/
(define (eval-find-module id)
   (when (hashtable? *modules-table*)
      (hashtable-get *modules-table* id)))

;*---------------------------------------------------------------------*/
;*    evmodule-find-global ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-find-global mod id)
   (if (evmodule? mod)
       (or (hashtable-get (%evmodule-env mod) id) (eval-lookup id))
       (eval-lookup id)))

;*---------------------------------------------------------------------*/
;*    evmodule-bind-global ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-bind-global! mod id var)
   (when (get-eval-expander id)
      (let ((msg (string-append "Variable `" (symbol->string id)
				"' hidden by an expander.")))
	 (evmeaning-warning #f msg)))
   (if (evmodule? mod)
       (hashtable-put! (%evmodule-env mod) id var)
       (bind-eval-global! id var)))

;*---------------------------------------------------------------------*/
;*    evmodule-library ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-library clause loc)
   (if (not (and (list? clause) (every? symbol? clause)))
       (evcompile-error loc 'eval "Illegal `library' clause" clause)
       (for-each (lambda (s) (eval `(library-load ',s))) (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-static ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-static mod clause loc classp)
   (define (evmodule-static-clause s)
      (match-case s
	 ((? symbol?)
	  (let ((id (untype-ident s)))
	     (eval `(define ,id ',evmodule-uninitialized) mod)))
	 ((class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (eval-class cla #f clauses clause mod)))
	 ((final-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (eval-class cla #f clauses clause mod)))
	 ((abstract-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (eval-class cla #t clauses clause mod)))
	 ((wide-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (evcompile-error
	      loc
	      'eval
	      "Wide classes are not supported within eval"
	      clause)))
	 (((or inline generic) (and (? symbol?) ?s) . ?-)
	  (let ((id (untype-ident s)))
	     (eval `(define ,id ',evmodule-uninitialized) mod)))
	 (((and (? symbol?) ?s) . ?-)
	  (let ((id (untype-ident s)))
	     (eval `(define ,id ',evmodule-uninitialized) mod)))
	 (else
	  (evcompile-error
	   loc
	   'eval "Illegal `static' clause" clause))))
   (if (not (list? clause))
       (evcompile-error loc 'eval "Illegal `static' clause" clause)
       (for-each evmodule-static-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-export! ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-export! mod id)
   (%evmodule-exports-set! mod (cons id (%evmodule-exports mod))))

;*---------------------------------------------------------------------*/
;*    evmodule-export ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-export mod clause loc classp)
   (define (evmodule-export-clause s)
      (match-case s
	 ((? symbol?)
	  (let ((id (untype-ident s)))
	     (evmodule-export! mod id)
	     (eval `(define ,id ',evmodule-uninitialized) mod)))
	 ((class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (let ((idents (eval-class cla #f clauses clause mod)))
		(for-each (lambda (i) (evmodule-export! mod i)) idents))))
	 ((final-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (let ((idents (eval-class cla #f clauses clause mod)))
		(for-each (lambda (i) (evmodule-export! mod i)) idents))))
	 ((abstract-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (let ((idents (eval-class cla #t clauses clause mod)))
		(for-each (lambda (i) (evmodule-export! mod i)) idents))))
	 ((wide-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (evcompile-error
	      loc
	      'eval
	      "Wide classes are not supported within eval"
	      clause)))
	 (((or inline generic) (and (? symbol?) ?s) . ?-)
	  (let ((id (untype-ident s)))
	     (evmodule-export! mod id)
	     (eval `(define ,id ',evmodule-uninitialized) mod)))
	 (((and (? symbol?) ?s) . ?-)
	  (let ((id (untype-ident s)))
	     (evmodule-export! mod id)
	     (eval `(define ,id ',evmodule-uninitialized) mod)))
	 (else
	  (evcompile-error
	   loc
	   "eval" "Illegal `export' clause" clause))))
   (if (not (list? clause))
       (evcompile-error loc 'eval "Illegal `export' clause" clause)
       (for-each evmodule-export-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-import-binding! ...                                     */
;*---------------------------------------------------------------------*/
(define (evmodule-import-binding! to-mod to-ident from-mod from-ident loc)
   (let ((var (evmodule-find-global from-mod from-ident)))
      (if (not var)
	  (evcompile-error loc 'eval
			   (string-append
			    "Cannot find binding from module `"
			    (symbol->string (evmodule-name from-mod))
			    "'")
			   from-ident)
	  (evmodule-bind-global! to-mod to-ident var))))

;*---------------------------------------------------------------------*/
;*    evmodule-load ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-load path)
   ((or (bigloo-load-module) evmodule-loadq) path))

;*---------------------------------------------------------------------*/
;*    *loading-list* ...                                               */
;*---------------------------------------------------------------------*/
(define *loading-list* '())

;*---------------------------------------------------------------------*/
;*    evmodule-loadq ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-loadq file)
   (define (loadq-with-cv path cv)
      (let ((cell (cons path cv)))
	 (unwind-protect
	    (begin
	       (set! *loading-list* (cons cell *loading-list*))
	       (mutex-unlock! *loadq-mutex*)
	       (loadq path))
	    (begin
	       (mutex-lock! *loadq-mutex*)
	       (set! *loading-list* (remq! cell *loading-list*))
	       (condition-variable-signal! cv)
	       (mutex-unlock! *loadq-mutex*)))))
   (let ((path (file-name-unix-canonicalize file)))
      (mutex-lock! *loadq-mutex*)
      (let ((c (assoc path *loading-list*)))
	 ;; is the path currently being loaded
	 (if (pair? c)
	     ;; yes it is
	     (let ((cv (cdr c)))
		(condition-variable-wait! cv *loadq-mutex*)
		(loadq-with-cv path cv))
	     ;; not it is not
	     (loadq-with-cv path (make-condition-variable (gensym 'loadq)))))))

;*---------------------------------------------------------------------*/
;*    evmodule-import! ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-import! mod ident path set abase loc)
   (define (import-error msg obj)
      (evcompile-error loc 'eval msg obj))
   (define (import-module mod2)
      ;; bind imported the macros
      (let ((t (%evmodule-macros mod)))
	 (hashtable-for-each (%evmodule-macros mod2)
			     (lambda (k v)
				(hashtable-put! t k v))))
      ;; bind variablabes
      (for-each (lambda (b)
		   (when (or (null? set) (memq b set))
		      (evmodule-import-binding! mod b mod2 b loc)))
		(%evmodule-exports mod2)))
   (let ((mod2 (eval-find-module ident)))
      (cond
	 ((evmodule? mod2)
	  (import-module mod2))
	 ((not (pair? path))
	  (import-error
	   (format "Cannot find imported module in base \"~a\"" abase)
	   ident))
	 (else
	  (when (>fx (bigloo-debug-module) 0)
	     (fprint (current-error-port)
		     "*** loading module `" ident "' [" path "]..."))
	  (unwind-protect
	     (let loop ((path path)
			(mod2 #f))
		(if (pair? path)
		    (begin
		       (evmodule-load (car path))
		       (let ((mod2 (eval-find-module ident)))
			  ($eval-module-set! mod2)
			  (loop (cdr path) mod2)))
		    (if (not (evmodule? mod2))
			(import-error (string-append "Cannot find module `"
						     (symbol->string ident)
						     "' in source")
				      path)
			(import-module mod2))))
	     ($eval-module-set! mod))))))

;*---------------------------------------------------------------------*/
;*    evmodule-import ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-import mod clause loc)
   (define (import-error arg)
      (evcompile-error loc 'eval "Illegal `import' clause" arg))
   (define (import-clause s)
      (let ((loc (or (get-source-location s) loc))
	    (abase (module-abase)))
	 (unwind-protect
	    (cond
	       ((symbol? s)
		(let ((path ((bigloo-module-resolver) s abase)))
		   (evmodule-import! mod s path '() (module-abase) loc)))
	       ((or (not (pair? s)) (not (list? s)) (not (symbol? (car s))))
		(import-error s))
	       (else
		(module-add-access! (car s) (cdr s) (pwd))
		(let ((path ((bigloo-module-resolver) (car s) (pwd))))
		   (evmodule-import! mod (car s) path '() (module-abase) loc))))
	    (module-abase-set! abase))))
   (if (not (list? clause))
       (import-error clause)
       (for-each import-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-from! ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-from! mod ident path set loc)
   (define (from-error msg obj)
      (evcompile-error loc 'eval msg obj))
   (define (from-module mod2)
      (let ((nx (append (if (pair? set)
			    (filter (lambda (b) (memq b set))
				    (%evmodule-exports mod2))
			    (%evmodule-exports mod2))
			(%evmodule-exports mod))))
	 (%evmodule-exports-set! mod nx)))
   (let ((mod2 (eval-find-module ident)))
      (cond
	 ((evmodule? mod2)
	  (from-module mod2))
	 ((not (pair? path))
	  (from-error "Cannot find module" ident))
	 (else
	  (when (>fx (bigloo-debug-module) 0)
	     (fprint (current-error-port)
		     "*** loading module `" ident "' [" path "]..."))
	  (for-each evmodule-load path)
	  (let ((mod2 (eval-find-module ident)))
	     (if (not (evmodule? mod2))
		 (from-error (string-append "Cannot find module `"
					    (symbol->string ident)
					    "' in source")
			     path)
		 (from-module mod2)))))))

;*---------------------------------------------------------------------*/
;*    evmodule-from ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-from mod clause loc)
   (define (from-error arg)
      (evcompile-error loc 'eval "Illegal `from' clause" arg))
   (define (from-clause s)
      (let ((loc (or (get-source-location s) loc))
	    (abase (module-abase)))
	 (unwind-protect
	    (cond
	       ((symbol? s)
		(let ((path ((bigloo-module-resolver) s abase)))
		   (evmodule-from! mod s path '() loc)))
	       ((or (not (pair? s)) (not (list? s)) (not (symbol? (car s))))
		(from-error s))
	       (else
		(module-add-access! (car s) (cdr s) (pwd))
		(let ((path ((bigloo-module-resolver) (car s) (pwd))))
		   (evmodule-from! mod (car s) path '() loc))))
	    (module-abase-set! abase))))
   (if (not (list? clause))
       (from-error clause)
       (for-each from-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-include ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-include mod clauses loc)

   (define (evmodule-include-file! file path)
      (let ((ffile (find-file/path file path)))
	 (if (string? ffile)
	     (call-with-input-file ffile
		(lambda (p)
		   (let ((e0 (read p)))
		      (if (and (pair? e0) (eq? (car e0) 'directives))
			  (values (cdr e0) (port->list read p))
			  (values '() (cons e0 (port->list read p)))))))
	     (evcompile-error loc "eval"
			      (format "Cannot find include file ~s" file)
			      path))))
   
   (define (evmodule-include-files! files path)
      (let loop ((files files)
		 (iclauses '())
		 (iexprs '()))
	 (if (null? files)
	     (values iclauses iexprs)
	     (multiple-value-bind (ic ie)
		(evmodule-include-file! (car files) path)
		(loop (cdr files) (append iclauses ic) (append iexprs ie))))))
   
   (let ((path (if (string? (%evmodule-path mod))
		   (cons (dirname (%evmodule-path mod)) *load-path*)
		   *load-path*)))
      (let loop ((clauses clauses)
		 (iclauses '())
		 (iexprs '()))
	 (cond
	    ((null? clauses)
	     (values iclauses iexprs))
	    ((not (pair? (car clauses)))
	     (evcompile-error loc "eval" "Illegal module clause" (car clauses)))
	    ((eq? (caar clauses) 'include)
	     (multiple-value-bind (ic ie)
		(evmodule-include-files! (cdar clauses) path)
		(multiple-value-bind (ic2 ie2)
		   (evmodule-include mod ic loc)
		   (loop (cdr clauses)
			 (append iclauses ic2 ic)
			 (append iexprs ie2 ie)))))
	    (else
	     (loop (cdr clauses)
		   (append iclauses (list (car clauses)))
		   iexprs))))))

;*---------------------------------------------------------------------*/
;*    evmodule-step1 ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-step1 mod clauses loc)
   (for-each (lambda (clause)
		($eval-module-set! mod)
		(let ((loc (or (get-source-location clause) loc)))
		   (when loc (evmeaning-set-error-location! loc))
		   (case (car clause)
		      ((library)
		       (evmodule-library clause loc))
		      ((static)
		       (evmodule-static mod clause loc #f))
		      ((export)
		       (evmodule-export mod clause loc #f))
		      ((load)
		       (evmodule-import mod clause loc)))))
	     clauses))

;*---------------------------------------------------------------------*/
;*    evmodule-step2 ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-step2 mod clauses loc)
   (for-each (lambda (clause)
		($eval-module-set! mod)
		(let ((loc (or (get-source-location clause) loc)))
		   (when loc (evmeaning-set-error-location! loc))
		   (case (car clause)
		      ((import use with)
		       (evmodule-import mod clause loc))
		      ((from)
		       (evmodule-from mod clause loc)))))
	     clauses))

;*---------------------------------------------------------------------*/
;*    evmodule-step3 ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-step3 mod clauses loc)
   (for-each (lambda (clause)
		($eval-module-set! mod)
		(let ((loc (or (get-source-location clause) loc)))
		   (when loc (evmeaning-set-error-location! loc))
		   (case (car clause)
		      ((static)
		       (evmodule-static mod clause loc #t))
		      ((export)
		       (evmodule-export mod clause loc #t)))))
	     clauses))

;*---------------------------------------------------------------------*/
;*    evmodule-cond-expand ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-cond-expand mod clauses loc)
   
   (define (evmodule-cond-expand-clause c0)
      (let loop ((c c0))
	 (cond
	    ((not (and (pair? c) (list? c) (symbol? (car c))))
	     (let ((loc (or (get-source-location c) loc)))
		(evcompile-error loc 'eval "Illegal module clause" c0)))
	    ((eq? (car c) 'cond-expand)
	     (let ((nc (expand c)))
		(cond
		   ((pair? nc)
		    (if (eq? (car nc) 'begin)
			(append-map loop (cdr nc))
			(loop nc)))
		   ((eq? nc #unspecified)
		    '())
		   (else
		    (list nc)))))
	    (else
	     (list c)))))
   
   (append-map evmodule-cond-expand-clause clauses))

;*---------------------------------------------------------------------*/
;*    evmodule-module ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-module mod clauses loc)
   ;; check the syntax and resolve the cond-expand clauses
   (let ((mclauses (evmodule-cond-expand mod clauses loc)))
      (multiple-value-bind (iclauses iexprs)
	 (evmodule-include mod mclauses loc)
	 ;; Step1: evaluate export clauses (and static for coherency).
	 ;; During that step, classes are not evaluated.
	 ;; Also process include directives clauses. 
	 (evmodule-step1 mod iclauses loc)
	 ;; Step2: evaluate import and from clauses.
	 (evmodule-step2 mod iclauses loc)
	 ;; step3: evaluate classes and build the module body (with includes).
	 (evmodule-step3 mod mclauses loc)
	 ;; returns the include expressions
	 `(begin ,@iexprs))))

;*---------------------------------------------------------------------*/
;*    evmodule ...                                                     */
;*    -------------------------------------------------------------    */
;*    Evaluate a module form                                           */
;*---------------------------------------------------------------------*/
(define (evmodule exp loc)
   (let* ((loc (or (get-source-location exp) loc))
	  (hdl (bigloo-module-extension-handler)))
      (match-case exp
	 ((module (and (? symbol?) ?name) . ?clauses)
	  (when loc (evmeaning-set-error-location! loc))
	  (if (not (list? clauses))
	      (evcompile-error loc 'eval "Illegal module clauses" clauses)
	      (let* ((path (or (evcompile-loc-filename loc) "."))
		     (mod (make-evmodule name path loc)))
		 (unwind-protect
		    (begin
		       (when (procedure? hdl)
			  (%evmodule-extension-set! mod (hdl exp)))
		       (evmodule-module mod clauses loc))
		    ($eval-module-set! mod)))))
	 (else
	  (evcompile-error loc 'eval "Illegal module expression" exp)))))

;*---------------------------------------------------------------------*/
;*    evmodule-static-class ...                                        */
;*---------------------------------------------------------------------*/
(define (evmodule-static-class x)
   (let* ((mod (eval-module))
	  (rest (cdr x))
	  (clause (case (car x)
		     ((define-class)
		      (evepairify `(static (class ,@rest)) x))
		     ((define-final-class)
		      (evepairify `(static (final-class ,@rest)) x))
		     ((define-abstract-class)
		      (evepairify `(static (abstract-class ,@rest)) x)))))
      (evmodule-static mod clause (get-source-location x) #t)))
