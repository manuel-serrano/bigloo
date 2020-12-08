;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/evmodule.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 17 09:40:04 2006                          */
;*    Last change :  Thu Apr 16 16:14:18 2020 (serrano)                */
;*    Copyright   :  2006-20 Manuel Serrano                            */
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
	    __expand
	    __trace
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
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

   (use     __macro)

   (extern  (macro $eval-module-set!::obj (::obj) "BGL_MODULE_SET")
	    (macro $eval-module::obj () "BGL_MODULE"))

   (java    (class foreign
	       (method static $eval-module-set!::obj (::obj) "BGL_MODULE_SET")
	       (method static $eval-module::obj () "BGL_MODULE")))

   (export  evmodule-uninitialized
	    (evmodule?::bool ::obj)
	    (evmodule-name::symbol ::obj)
	    (evmodule-path::obj ::obj)
	    (evmodule-find-global ::obj ::symbol)
	    (evmodule-bind-global! ::obj ::symbol ::obj ::obj)
	    (evmodule-macro-table ::obj)
	    (evmodule-extension ::obj)
	    (evmodule-extension-set! ::obj ::obj)
	    (evmodule ::pair-nil ::obj)
	    (evmodule-comp! ::symbol ::pair ::obj . bindings)
	    (evmodule-check-unbound mod loc)
	    (eval-find-module ::symbol)
	    (eval-module)
	    (eval-module-set! ::obj)
	    (evmodule-static-class ::pair-nil)
	    (call-with-eval-module ::obj ::procedure))

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
;*    untype-ident ...                                                 */
;*---------------------------------------------------------------------*/
(define (untype-ident id)
   (if (not (symbol? id))
       id
       (let* ((string (symbol->string id))
	      (len    (string-length string)))
	  (let loop ((walker  0))
	     (cond
		((=fx walker len)
		 id)
		((and (char=? (string-ref string walker) #\:)
		      (<fx walker (-fx len 1))
		      (char=? (string-ref string (+fx walker 1)) #\:))
		 (string->symbol (substring string 0 walker)))
		(else
		 (loop (+fx walker 1))))))))

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
       (bigloo-type-error "evmodule-name" 'module mod)))

;*---------------------------------------------------------------------*/
;*    evmodule-path ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-path mod)
   (if (evmodule? mod)
       (%evmodule-path mod)
       (bigloo-type-error "evmodule-path" 'module mod)))

;*---------------------------------------------------------------------*/
;*    evmodule-macro-table ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-macro-table mod)
   (if (evmodule? mod)
       (%evmodule-macros mod)
       (bigloo-type-error "evmodule-macro-table" 'module mod)))

;*---------------------------------------------------------------------*/
;*    evmodule-extension ...                                           */
;*---------------------------------------------------------------------*/
(define (evmodule-extension mod)
   (if (evmodule? mod)
       (%evmodule-extension mod)
       (bigloo-type-error "evmodule-extension" 'module mod)))

;*---------------------------------------------------------------------*/
;*    evmodule-extension-set! ...                                      */
;*---------------------------------------------------------------------*/
(define (evmodule-extension-set! mod ext)
   (%evmodule-extension-set! mod ext))

;*---------------------------------------------------------------------*/
;*    make-evmodule ...                                                */
;*---------------------------------------------------------------------*/
(define (make-evmodule id path loc)
   (synchronize *modules-mutex*
      (let* ((env (make-hashtable 100 #unspecified eq?))
	     (mactable (make-hashtable 64))
	     (mod (%evmodule make-%evmodule id path env '() mactable '())))
	 (if (not (hashtable? *modules-table*))
	     (begin
		(set! *modules-table* (make-hashtable 256))
		(hashtable-put! *modules-table* id mod))
	     (let ((old (hashtable-get *modules-table* id)))
		(if old
		    (begin
		       (hashtable-update! *modules-table* id
			  (lambda (v) mod) mod)
		       (unless (string=? (%evmodule-path old) path)
			  (let ((msg (string-append "Module redefinition `"
					(symbol->string id)
					"'. Previous \""
					(%evmodule-path old)
					"\", new (ignored) \""
					path "\"")))
			     (warning/loc loc msg))))
		    (hashtable-put! *modules-table* id mod))))
	 mod)))

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
       (error "eval-module-set!" "Illegal module" mod)))

;*---------------------------------------------------------------------*/
;*    eval-find-module ...                                             */
;*---------------------------------------------------------------------*/
(define (eval-find-module id)
   (when (hashtable? *modules-table*)
      (hashtable-get *modules-table* id)))

;*---------------------------------------------------------------------*/
;*    evalias? ...                                                     */
;*---------------------------------------------------------------------*/
(define (evalias? v)
   (and (eval-global? v) (=fx (eval-global-tag v) 6)))

;*---------------------------------------------------------------------*/
;*    evalias-module ...                                               */
;*---------------------------------------------------------------------*/
(define (evalias-module v)
   (cond
      ((evmodule? (eval-global-module v))
       (eval-global-module v))
      ((symbol? (eval-global-module v))
       (let ((m (eval-find-module (eval-global-module v))))
	  (eval-global-module-set! v m)
	  m))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    evmodule-find-global ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-find-global mod id)
   (if (evmodule? mod)
       (let ((v (hashtable-get (%evmodule-env mod) id)))
	  (if (evalias? v)
	      (evmodule-find-global (evalias-module v) (eval-global-value v))
	      (or v (eval-lookup id))))
       (eval-lookup id)))

;*---------------------------------------------------------------------*/
;*    evmodule-bind-global ...                                         */
;*---------------------------------------------------------------------*/
(define (evmodule-bind-global! mod id var loc)
   (when (get-eval-expander id)
      (let ((msg (string-append "Variable `" (symbol->string id)
				"' hidden by an expander.")))
	 (evwarning loc msg)))
   (if (evmodule? mod)
       (hashtable-put! (%evmodule-env mod) id var)
       (bind-eval-global! id var)))

;*---------------------------------------------------------------------*/
;*    for-each/loc ...                                                 */
;*---------------------------------------------------------------------*/
(define (for-each/loc loc proc l)
   (let loop ((l l)
	      (loc (or (get-source-location l) loc)))
      (when (pair? l)
	 (proc loc (car l))
	 (loop (cdr l) (or (get-source-location (cdr l)) loc)))))
   
;*---------------------------------------------------------------------*/
;*    evmodule-library ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-library clause loc)
   (if (not (and (list? clause) (every symbol? clause)))
       (evcompile-error loc "eval" "Illegal `library' clause" clause)
       (for-each/loc loc
		     (lambda (loc s)
			(eval/loc loc `(library-load ',s)))
		     (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-option ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-option clause loc)
   (match-case clause
      ((option . ?exp)
       (let ((loc (or (get-source-location clause) loc)))
	  (for-each (lambda (e) (eval/loc loc e)) exp)))
      (else
       (evcompile-error loc "eval" "Illegal `option' clause" clause))))

;*---------------------------------------------------------------------*/
;*    mark-global! ...                                                 */
;*---------------------------------------------------------------------*/
(define (mark-global! id mod tag loc)
   (let ((v (evmodule-find-global mod id)))
      (if (eval-global? v)
	  (begin
	     (eval-global-tag-set! v tag)
	     id)
	  (evcompile-error loc "eval" "variable unbound" id))))

;*---------------------------------------------------------------------*/
;*    mark-global-uninitialized! ...                                   */
;*---------------------------------------------------------------------*/
(define (mark-global-uninitialized! id mod loc)
   (mark-global! id mod 3 loc))

;*---------------------------------------------------------------------*/
;*    mark-global-readonly! ...                                        */
;*---------------------------------------------------------------------*/
(define (mark-global-readonly! id mod loc)
   (mark-global! id mod 4 loc))

;*---------------------------------------------------------------------*/
;*    bind-global! ...                                                 */
;*---------------------------------------------------------------------*/
(define (bind-global! id mod loc)
   (let ((g (make-eval-global id mod loc)))
      (evmodule-bind-global! mod id g loc)
      g))
   
;*---------------------------------------------------------------------*/
;*    evmodule-static ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-static mod clause loc classp)
   (define (evmodule-static-clause loc s)
      (match-case s
	 ((? symbol?)
	  (unless classp
	     (let ((id (untype-ident s)))
		(bind-global! id mod loc)
		(mark-global-uninitialized! id mod loc))))
	 ((class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (eval-class cla #f clauses s mod)))
	 ((final-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (eval-class cla #f clauses s mod)))
	 ((abstract-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (eval-class cla #t clauses s mod)))
	 ((wide-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (evcompile-error
	      loc
	      "eval"
	      "Wide classes are not supported within eval"
	      clause)))
	 (((or inline generic) (and (? symbol?) ?s) . ?-)
	  (unless classp
	     (let ((id (untype-ident s)))
		(eval/loc loc `(define ,id ',evmodule-uninitialized) mod)
		(mark-global-readonly! id mod loc))))
	 (((and (? symbol?) ?s) . ?-)
	  (unless classp
	     (let ((id (untype-ident s)))
		(bind-global! id mod loc)
		(mark-global-readonly! id mod loc))))
	 (else
	  (evcompile-error
	   loc
	   "eval" "Illegal `static' clause" clause))))
   (if (not (list? clause))
       (evcompile-error loc "eval" "Illegal `static' clause" clause)
       (for-each/loc loc evmodule-static-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-export! ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-export! mod id m)
   (%evmodule-exports-set! mod (cons (cons id m) (%evmodule-exports mod))))

;*---------------------------------------------------------------------*/
;*    eval/loc ...                                                     */
;*---------------------------------------------------------------------*/
(define (eval/loc loc exp #!optional env)
   (let ((exp (if loc
		  (econs (car exp) (cdr exp) loc)
		  exp)))
      (eval exp env)))

;*---------------------------------------------------------------------*/
;*    evmodule-export ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-export mod clause loc classp)
   (define (evmodule-export-clause loc s)
      (match-case s
	 ((? symbol?)
	  (unless classp
	     (let ((id (untype-ident s)))
		(bind-global! id mod loc)
		(evmodule-export! mod id mod)
		(mark-global-uninitialized! id mod loc))))
	 ((class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (let ((idents (eval-class cla #f clauses s mod)))
		(for-each (lambda (i) (evmodule-export! mod i mod)) idents))))
	 ((final-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (let ((idents (eval-class cla #f clauses s mod)))
		(for-each (lambda (i) (evmodule-export! mod i mod)) idents))))
	 ((abstract-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (let ((idents (eval-class cla #t clauses s mod)))
		(for-each (lambda (i) (evmodule-export! mod i mod)) idents))))
	 ((wide-class (and ?cla (? symbol?)) . ?clauses)
	  (when classp
	     (evcompile-error
	      loc
	      "eval"
	      "Wide classes are not supported within eval"
	      clause)))
	 (((or inline generic) (and (? symbol?) ?s) . ?-)
	  (unless classp
	     (let ((id (untype-ident s)))
		(evmodule-export! mod id mod)
		(eval/loc loc `(define ,id ',evmodule-uninitialized) mod)
		(mark-global-readonly! id mod loc))))
	 ((macro . ?-)
	  #unspecified)
	 ((syntax . ?-)
	  #unspecified)
	 ((expander ?-)
	  #unspecified)
	 (((and (? symbol?) ?s) . ?-)
	  (unless classp
	     (let ((id (untype-ident s)))
		(bind-global! id mod loc)
		(evmodule-export! mod id mod)
		(mark-global-readonly! id mod loc))))
	 (else
	  (evcompile-error
	   loc
	   "eval" "Illegal `export' clause" clause))))
   (if (not (list? clause))
       (evcompile-error loc "eval" "Illegal `export' clause" clause)
       (for-each/loc loc evmodule-export-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    evmodule-import-binding! ...                                     */
;*---------------------------------------------------------------------*/
(define (evmodule-import-binding! to-mod to-ident from-mod from-ident loc)
   (let ((var (evmodule-find-global from-mod from-ident)))
      (if (not var)
	  (begin
	     (tprint "ERROR: " (hashtable-key-list (%evmodule-env from-mod)))
	     (evcompile-error loc "eval"
		(string-append
		   "Cannot find imported variable from module `"
		   (symbol->string (evmodule-name to-mod))
		   "'")
		`(@ ,from-ident ,(evmodule-name from-mod))))
	  (evmodule-bind-global! to-mod to-ident var loc))))

;*---------------------------------------------------------------------*/
;*    evmodule-check-unbound ...                                       */
;*    -------------------------------------------------------------    */
;*    Raise an error if a module contains un unbound variable.         */
;*---------------------------------------------------------------------*/
(define (evmodule-check-unbound mod loc)
   
   (define (unbound-error v)
      (evcompile-error (or (eval-global-loc v) loc)
	 (evmodule-name mod)
	 "Unbound variable" (eval-global-name v)))
   
   (let ((l '()))
      
      (define (global-check-unbound k g)
	 (unless (evalias? g)
	    (let ((tag (eval-global-tag g)))
	       (when (and (eq? (eval-global-module g) mod)
			  (or (=fx tag 3) (=fx tag 4)))
		  (set! l (cons g l))))))
      
      (hashtable-for-each (%evmodule-env mod) global-check-unbound)
      (when (pair? l)
	 (for-each (lambda (v)
		      (with-handler
			 (lambda (e)
			    (error-notify e)
			    (newline (current-error-port))
			    #f)
			 (unbound-error v)))
	    l)
	 (let ((len (length l)))
	    (evcompile-error #f
	       (evmodule-name mod)
	       (format "~a unbound variable~a" len (if (> len 1) "s" ""))
	       (format "~l" (map eval-global-name l)))))))
	  
;*---------------------------------------------------------------------*/
;*    evmodule-load ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-load mod ident paths loc)
   
   (define (user-load-module)
      (let ((proc (bigloo-load-module)))
	 (when (procedure? proc)
	    (cond
	       ((correct-arity? proc 2)
		proc)
	       ((correct-arity? proc 1)
		(lambda (path _) (proc path)))
	       (else
		(error "evmodule-load"
		   "incorect user module loader" proc))))))

   (let ((load (or (user-load-module) evmodule-loadq)))
      (for-each (lambda (p) (load p mod)) paths))
   
   (let ((m (eval-find-module ident)))
      (if (evmodule? m)
	  (begin
	     (evmodule-check-unbound m loc)
	     m)
	  (evcompile-error loc "eval"
	     (format "~a:cannot find module \"~a\"" (evmodule-name mod) ident)
	     (if (pair? (cdr paths))
		 paths
		 (car paths))))))

;*---------------------------------------------------------------------*/
;*    *loading-list* ...                                               */
;*---------------------------------------------------------------------*/
(define *loading-list* '())

;*---------------------------------------------------------------------*/
;*    evmodule-loadq ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-loadq file mod)
   (let* ((path (file-name-unix-canonicalize file))
	  (cv (make-condition-variable (gensym 'loadq)))
	  (cell (cons path cv)))
      (let loop ()
	 (synchronize *loadq-mutex*
	    (let ((c (assoc path *loading-list*)))
	       ;; is the path currently being loaded
	       (if (pair? c)
		   ;; yes it is
		   (begin
		      ;; wait for the load to complete
		      (condition-variable-wait! (cdr c) *loadq-mutex*)
		      (loop))
		   ;; not it is not
		   (set! *loading-list* (cons cell *loading-list*))))))
      (unwind-protect
	 (loadq path)
	 (synchronize *loadq-mutex*
	    (set! *loading-list* (remq! cell *loading-list*))
	    (condition-variable-signal! cv)))))

;*---------------------------------------------------------------------*/
;*    evmodule-import! ...                                             */
;*---------------------------------------------------------------------*/
(define (evmodule-import! mod ident path set abase loc)
   
   (define (import-error msg obj)
      (evcompile-error loc "eval" msg obj))
   
   (define (import-module mod2)
      ;; bind imported the macros
      (let ((t (%evmodule-macros mod)))
	 (hashtable-for-each (%evmodule-macros mod2)
			     (lambda (k v)
				(hashtable-put! t k v))))
      ;; bind variables
      (for-each (lambda (b)
		   (when (or (null? set) (memq (car b) set))
		      (evmodule-import-binding! mod (car b) mod2 (car b) loc)))
		(%evmodule-exports mod2)))

   (define (load-module)
      (unwind-protect
	 (import-module (evmodule-load mod ident path loc))
	 ($eval-module-set! mod)))
   
   (let ((mod2 (eval-find-module ident)))
      (cond
	 ((evmodule? mod2)
	  (import-module mod2))
	 ((not (pair? path))
	  (import-error
	   (format "Cannot find imported module in base \"~a\"" abase)
	   ident))
	 (else
	  (if (>fx (bigloo-debug-module) 0)
	      (%with-trace 'module ident
		 (lambda ()
		    (trace-item "path=" path)
		    (load-module)))
	      (load-module))))))

;*---------------------------------------------------------------------*/
;*    location-dir ...                                                 */
;*---------------------------------------------------------------------*/
(define (location-dir loc)
   (match-case loc
      ((at ?fname . ?-) (dirname fname))
      (else #f)))

;*---------------------------------------------------------------------*/
;*    evmodule-import ...                                              */
;*---------------------------------------------------------------------*/
(define (evmodule-import mod clause loc)
   
   (define (import-error arg)
      (evcompile-error loc "eval" "Illegal `import' clause" arg))
   
   (define (find-module-files clause)
      (cond
	 ((null? clause) '())
	 ((string? (car clause)) clause)
	 (else (find-module-files (cdr clause)))))

   (define (find-module-imports clause)
      (let ((l (find-tail symbol? clause)))
	 (let loop ((lst clause)
		    (res '()))
	    (if (eq? lst l)
		res
		(if (alias-pair? (car lst))
		    (loop (cdr lst) (cons (cadr (car lst)) res))
		    (loop (cdr lst) (cons (car lst) res)))))))

   (define (find-module-aliases clause)
      (let ((l (find-tail symbol? clause)))
	 (let loop ((lst clause)
		    (res '()))
	    (if (eq? lst l)
		res
		(if (alias-pair? (car lst))
		    (loop (cdr lst) (cons (car lst) res))
		    (loop (cdr lst) res))))))

   (define (import-clause s)
      (let ((loc (or (get-source-location s) loc))
	    (abase (location-dir loc)))
	 (cond
	    ((symbol? s)
	     (let ((path ((bigloo-module-resolver) s '() abase)))
		(evmodule-import! mod s path '() abase loc)))
	    ((or (not (pair? s))
		 (not (list? s))
		 (not (or (symbol? (car s)) (alias-pair? (car s)))))
	     (import-error s))
	    (else
	     (let ((files (find-module-files s))
		   (imod (find symbol? s))
		   (imports (find-module-imports s))
		   (aliases (find-module-aliases s))
		   (dir (or (location-dir loc) (pwd))))
		(let ((path ((bigloo-module-resolver) imod files dir)))
		   (for-each (lambda (ap)
				(bind-alias! mod imod
				   (car ap)
				   (cadr ap)
				   (or (get-source-location ap) loc)))
		      aliases)
		   (evmodule-import! mod imod path imports abase loc)))))))
   
   (if (not (list? clause))
       (import-error clause)
       (for-each import-clause (cdr clause))))

;*---------------------------------------------------------------------*/
;*    bind-alias! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bind-alias! mod imodname alias orig loc)
   (let ((a (make-eval-global alias imodname loc)))
      (eval-global-tag-set! a 6)
      (set-eval-global-value! a orig)
      (evmodule-bind-global! mod alias a loc)))

;*---------------------------------------------------------------------*/
;*    alias-pair? ...                                                  */
;*---------------------------------------------------------------------*/
(define (alias-pair? v)
   (match-case v
      (((? symbol?) (? symbol?)) #t)
      (else #f)))
   
;*---------------------------------------------------------------------*/
;*    evmodule-from! ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-from! mod ident path set loc)
   (define (from-error msg obj)
      (evcompile-error loc "eval" msg obj))
   (define (from-module mod2)
      (let* ((ex (if (pair? set)
		     (filter (lambda (b) (memq (car b) set))
			(%evmodule-exports mod2))
		     (%evmodule-exports mod2)))
	     (nx (append ex (%evmodule-exports mod))))
	 (for-each (lambda (b)
		      (bind-alias! mod mod2 (car b) (car b) loc))
	    ex)
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
	  (unwind-protect
	     (from-module (evmodule-load mod ident path loc))
	     ($eval-module-set! mod))))))

;*---------------------------------------------------------------------*/
;*    evmodule-from ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-from mod clause loc)
   
   (define (from-error arg)
      (evcompile-error loc "eval" "Illegal `from' clause" arg))
   
   (define (from-clause s)
      (let ((loc (or (get-source-location s) loc))
	    (abase (location-dir loc)))
	 (cond
	    ((symbol? s)
	     (let ((path ((bigloo-module-resolver) s '() abase)))
		(evmodule-from! mod s path '() loc)))
	    ((or (not (pair? s)) (not (list? s)) (not (symbol? (car s))))
	     (from-error s))
	    (else
	     (let ((path ((bigloo-module-resolver) (car s) (cdr s) (pwd))))
		(evmodule-from! mod (car s) path '() loc))))))
   
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
			 (append iclauses ic2)
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
		   (case (car clause)
		      ((library)
		       (evmodule-library clause loc))
		      ((static)
		       (evmodule-static mod clause loc #f))
		      ((export)
		       (evmodule-export mod clause loc #f))
		      ((load)
		       (evmodule-import mod clause loc))
		      ((option)
		       (evmodule-option clause loc)))))
      clauses))

;*---------------------------------------------------------------------*/
;*    evmodule-step2 ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-step2 mod clauses loc)
   (for-each (lambda (clause)
		($eval-module-set! mod)
		(let ((loc (or (get-source-location clause) loc)))
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
		(evcompile-error loc "eval" "Illegal module clause" c0)))
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
	 (evmodule-step3 mod iclauses loc)
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
	  (if (not (list? clauses))
	      (evcompile-error loc "eval" "Illegal module clauses" clauses)
	      (let* ((path (or (evcompile-loc-filename loc) "."))
		     (mod (make-evmodule name path loc)))
		 (module-load-access-file (dirname path))
		 (when (procedure? hdl)
		    (evmodule-extension-set! mod (hdl exp)))
		 (unwind-protect
		    (evmodule-module mod clauses loc)
		    ($eval-module-set! mod)))))
	 (else
	  (evcompile-error loc "eval" "Illegal module expression" exp)))))

;*---------------------------------------------------------------------*/
;*    evmodule-comp! ...                                               */
;*---------------------------------------------------------------------*/
(define (evmodule-comp! id path loc . exports)
   (let ((mod (make-evmodule id path loc)))
      (for-each (lambda (g)
		   (let ((id (eval-global-name g))
			 (val (eval-global-value g)))
		      (when (class? val)
			 (eval-expand-instantiate val)
			 (eval-expand-duplicate val)
			 (eval-expand-with-access val))
		      (evmodule-export! mod id g)
		      (evmodule-bind-global! mod id g loc)))
	 exports)
      #f))

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

;*---------------------------------------------------------------------*/
;*    call-with-eval-module ...                                        */
;*---------------------------------------------------------------------*/
(define (call-with-eval-module newm proc)
   (let ((oldm (eval-module)))
      (eval-module-set! newm)
      (unwind-protect
	 (proc)
	 (eval-module-set! oldm))))
