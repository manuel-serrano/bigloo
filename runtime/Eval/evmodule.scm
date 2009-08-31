;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evmodule.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 17 09:40:04 2006                          */
;*    Last change :  Mon Aug 31 15:17:58 2009 (serrano)                */
;*    Copyright   :  2006-09 Manuel Serrano                            */
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
(define (make-evmodule id path)
   (mutex-lock! *modules-mutex*)
   (let* ((env (make-hashtable 100 #unspecified eq?))
	  (mactable (make-hashtable 64))
	  (mod (%evmodule make-%evmodule id path env '() mactable '())))
      (if (not (hashtable? *modules-table*))
	  (begin
	     (set! *modules-table* (make-hashtable 100))
	     (hashtable-put! *modules-table* id mod))
	  (if (hashtable-get *modules-table* id)
	      (begin
		 (hashtable-update! *modules-table* id (lambda (v) mod) mod)
		 (let ((msg (string-append "Module redefinition `"
					   (symbol->string id)
					   "'")))
		    (evmeaning-warning #f msg)))
	      (hashtable-put! *modules-table* id mod)))
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
   (hashtable-get *modules-table* id))

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
	     (multiple-value-bind (exprs _)
		(eval-class cla #f clauses clause)
		(for-each (lambda (e) (eval e mod)) exprs))))
	 ((final-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (multiple-value-bind (exprs _)
		(eval-class cla #f clauses clause)
		(for-each (lambda (e) (eval e mod)) exprs))))
	 ((abstract-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (multiple-value-bind (exprs _)
		(eval-class cla #t clauses clause)
		(for-each (lambda (e) (eval e mod)) exprs))))
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
	     (multiple-value-bind (exprs idents)
		(eval-class cla #f clauses clause)
		(for-each (lambda (e) (eval e mod)) exprs)
		(for-each (lambda (i) (evmodule-export! mod i)) idents))))
	 ((final-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (multiple-value-bind (exprs idents)
		(eval-class cla #f clauses clause)
		(for-each (lambda (e) (eval e mod)) exprs)
		(for-each (lambda (i) (evmodule-export! mod i)) idents))))
	 ((abstract-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (multiple-value-bind (exprs idents)
		(eval-class cla #t clauses clause)
		(for-each (lambda (e) (eval e mod)) exprs)
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
	   'eval "Illegal `export' clause" clause))))
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
	  (for-each evmodule-load path)
	  (let ((mod2 (eval-find-module ident)))
	     (if (not (evmodule? mod2))
		 (import-error (string-append "Cannot find module `"
					      (symbol->string ident)
					      "' in source")
			       path)
		 (import-module mod2)))))))

;*---------------------------------------------------------------------*/
;*    evmodule-import ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-import mod clause loc)
   (define (import-error arg)
      (evcompile-error loc 'eval "Illegal `import' clause" arg))
   (define (import-clause s)
      (let ((loc (find-loc s loc))
	    (abase (if (string? (module-abase)) (module-abase) '*)))
	 (cond
	    ((symbol? s)
	     (let ((path ((bigloo-module-resolver) s abase)))
		(evmodule-import! mod s path '() abase loc)))
	    ((or (not (pair? s)) (not (list? s)) (not (symbol? (car s))))
	     (import-error s))
	    (else
	     (module-add-access! (car s) (cdr s) abase)
	     (let ((path ((bigloo-module-resolver) (car s) abase)))
		(evmodule-import! mod (car s) path '() abase loc))))))
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
      (let ((loc (find-loc s loc)))
	 (cond
	    ((symbol? s)
	     (let ((path ((bigloo-module-resolver) s (module-abase))))
		(evmodule-from! mod s path '() loc)))
	    ((or (not (pair? s)) (not (list? s)) (not (symbol? (car s))))
	     (from-error s))
	    (else
	     (module-add-access! (car s) (cdr s) (module-abase))
	     (let ((path ((bigloo-module-resolver) (car s) (module-abase))))
		(evmodule-from! mod (car s) path '() loc))))))
   (if (not (list? clause))
       (from-error clause)
       (for-each from-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-include! ...                                            */
;*---------------------------------------------------------------------*/
(define (evmodule-include! mod file loc)
   (let ((file (find-file/path file *load-path*)))
      (if (string? file)
	  (with-input-from-file file
	     (lambda ()
		(let ((e0 (read)))
		   (if (and (pair? e0) (eq? (car e0) 'directives))
		       (append (evmodule-module mod (cdr e0) (find-loc e0 loc))
			       (port->list read (current-input-port)))
		       (cons e0 (port->list read (current-input-port)))))))
	  (evcompile-error loc 'eval "Cannot find include file" file))))

;*---------------------------------------------------------------------*/
;*    evmodule-include ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-include mod clause loc)
   (if (not (every? string? (cdr clause)))
       (evcompile-error 'loc 'eval "Illegal `include' module clause" clause)
       (append-map (lambda (file)
		      (evmodule-include! mod file loc))
		   (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-module ...                                              */
;*    -------------------------------------------------------------    */
;*    Because of recursive modules, evaluating a module is a multi-    */
;*    steps process.                                                   */
;*      step1: evaluates export clauses (and static for coherency).    */
;*             during that step, classes are not evaluated.            */
;*      step2: evaluates other clauses (in particular imports).        */
;*      step3: evaluates includes (in particular imports).             */
;*      step4: evaluates classes.                                      */
;*---------------------------------------------------------------------*/
(define (evmodule-module mod clauses loc)
   ;; check the syntax and resolve the cond-expand clauses
   (let ((clauses (filter-map (lambda (c0)
				 (let loop ((c c0))
				    (cond
				       ((not (and (pair? c)
						  (list? c)
						  (symbol? (car c))))
					(let ((loc (find-loc c loc)))
					   (evcompile-error
					    loc 'eval
					    "Illegal module clause" c0)))
				       ((eq? (car c) 'cond-expand)
					(let ((nc (expand c)))
					   (unless (or (eq? nc #f)
						       (eq? nc #unspecified))
					      (loop (expand c)))))
				       (else
					c))))
		       clauses)))
      ;; step1
      (for-each (lambda (clause)
		   ($eval-module-set! mod)
		   (let ((loc (find-loc clause loc)))
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
		clauses)
      ;; step2
      (for-each (lambda (clause)
		   ($eval-module-set! mod)
		   (let ((loc (find-loc clause loc)))
		      (when loc (evmeaning-set-error-location! loc))
		      (case (car clause)
			 ((import use with)
			  (evmodule-import mod clause loc))
			 ((from)
			  (evmodule-from mod clause loc)))))
		clauses)
      ;; step3, include
      (let ((incs (append-map (lambda (clause)
				 ($eval-module-set! mod)
				 (let ((loc (find-loc clause loc)))
				    (when loc
				       (evmeaning-set-error-location! loc))
				    (case (car clause)
				       ((include)
					(evmodule-include mod clause loc))
				       (else
					'()))))
			      clauses)))
	 ;; step3, classes
	 (for-each (lambda (clause)
		      ($eval-module-set! mod)
		      (let ((loc (find-loc clause loc)))
			 (when loc (evmeaning-set-error-location! loc))
			 (case (car clause)
			    ((static)
			     (evmodule-static mod clause loc #t))
			    ((export)
			     (evmodule-export mod clause loc #t)))))
		   clauses)
	 `(begin ,@incs))))

;*---------------------------------------------------------------------*/
;*    evmodule ...                                                     */
;*    -------------------------------------------------------------    */
;*    Evaluate a module form                                           */
;*---------------------------------------------------------------------*/
(define (evmodule exp loc)
   (let* ((loc (find-loc exp loc))
	  (hdl (bigloo-module-extension-handler)))
      (match-case exp
	 ((module (and (? symbol?) ?name) . ?clauses)
	  (when loc (evmeaning-set-error-location! loc))
	  (if (not (list? clauses))
	      (evcompile-error loc 'eval "Illegal module clauses" clauses)
	      (let* ((path (or (evcompile-loc-filename loc) "."))
		     (mod (make-evmodule name path)))
		 (unwind-protect
		    (begin
		       (evmodule-module mod clauses loc)
		       (when (procedure? hdl)
			  (%evmodule-extension-set! mod (hdl exp))))
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
      (evmodule-static mod clause (find-loc x #f) #t)))
