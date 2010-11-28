;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cnst/alloc.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  6 13:51:36 1995                          */
;*    Last change :  Sun Nov 28 10:04:17 2010 (serrano)                */
;*    Copyright   :  1995-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The constant allocations.                                        */
;*=====================================================================*/
     
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cnst_alloc
   (include "Tvector/tvector.sch")
   (import  engine_param
	    module_module
	    backend_backend
	    tools_shape
	    tools_error
	    type_type
	    type_cache
	    tvector_tvector
	    tvector_cnst
	    ast_var
	    ast_node
	    ast_sexp
	    ast_local
	    ast_glo-def
	    coerce_coerce
	    cnst_cache
	    cnst_node
	    ast_ident
	    ast_env
	    type_env)
   (export  (start-cnst-alloc!)
	    (stop-cnst-alloc!)
	    (get-cnst-offset)
	    (get-cnst-set)
	    (get-cnst-sexp)
	    (get-cnst-table)
	    (cnst-table-id)
	    (cnst-alloc-string::node ::bstring <loc>)
	    (cnst-alloc-ucs2-string::node ::ucs2string <loc>)
	    (cnst-alloc-symbol::node ::symbol <loc>)
	    (cnst-alloc-bignum::obj ::obj <loc>)
	    (cnst-alloc-keyword::node ::keyword <loc>)
	    (cnst-alloc-procedure::node ::node <loc>)
	    (cnst-alloc-real::node ::real <loc>)
	    (cnst-alloc-elong::node ::elong <loc>)
	    (cnst-alloc-llong::node ::llong <loc>)
	    (cnst-alloc-list::node ::pair-nil <loc>)
	    (cnst-alloc-vector::node ::vector <loc>)
	    (cnst-alloc-homogenous-vector::node ::obj <loc>)
	    (cnst-alloc-struct::node ::struct <loc>)
	    (cnst-alloc-tvector::node <a-tvector> <loc>)))

;*---------------------------------------------------------------------*/
;*    structures                                                       */
;*---------------------------------------------------------------------*/
(define-struct cnst-info cnst offset)
(define-struct procedure-info proc variable)

;*---------------------------------------------------------------------*/
;*    *cnst-table* ...                                                 */
;*---------------------------------------------------------------------*/
(define *cnst-table* #unspecified)
(define *cnst-offset* -1)

;*---------------------------------------------------------------------*/
;*    get-cnst-offset ...                                              */
;*---------------------------------------------------------------------*/
(define (get-cnst-offset)
   *cnst-offset*)

;*---------------------------------------------------------------------*/
;*    get-cnst-set ...                                                 */
;*---------------------------------------------------------------------*/
(define (get-cnst-set)
   *global-set*)

;*---------------------------------------------------------------------*/
;*    get-cnst-table ...                                               */
;*---------------------------------------------------------------------*/
(define (get-cnst-table)
   *cnst-table*)

;*---------------------------------------------------------------------*/
;*    get-cnst-sexp ...                                                */
;*---------------------------------------------------------------------*/
(define (get-cnst-sexp)
   (reverse! *global-sexp*))

;*---------------------------------------------------------------------*/
;*    add-cnst-sexp! ...                                               */
;*---------------------------------------------------------------------*/
(define (add-cnst-sexp! sexp)
   (set! *global-sexp* (cons sexp *global-sexp*)))

;*---------------------------------------------------------------------*/
;*    cnst-info-create ...                                             */
;*---------------------------------------------------------------------*/
(define (cnst-info-create cnst)
   (let ((new (make-cnst-info)))
      (cnst-info-cnst-set! new cnst)
      new))

;*---------------------------------------------------------------------*/
;*    The local hash-tables                                            */
;*---------------------------------------------------------------------*/
(define *string-env*     '())
(define *ucs2string-env* '())
(define *real-env*       '())
(define *elong-env*      '())
(define *llong-env*      '())
(define *symbol-env*     '())
(define *keyword-env*    '())
(define *bignum-env*     '())
(define *list-env*       '())
(define *vector-env*     '())
(define *struct-env*     '())
(define *global-set*     '())
(define *global-sexp*    '())

;*---------------------------------------------------------------------*/
;*    *old-debug*                                                      */
;*---------------------------------------------------------------------*/
(define *old-debug* #unspecified)

;*---------------------------------------------------------------------*/
;*    cnst-table-id ...                                                */
;*---------------------------------------------------------------------*/
(define (cnst-table-id)
   '__cnsts_table)

;*---------------------------------------------------------------------*/
;*    typed-cnst-table-id ...                                          */
;*---------------------------------------------------------------------*/
(define (typed-cnst-table-id)
   (make-typed-ident (cnst-table-id) 'cnst*))

;*---------------------------------------------------------------------*/
;*    start-cnst-alloc! ...                                            */
;*---------------------------------------------------------------------*/
(define (start-cnst-alloc!)
   (set! *old-debug* *compiler-debug*)
   ;; this pass use the sexp->ast construction which can
   ;; introduce `trace' expression when the variable *compiler-debug* is #t.
   ;; In order to avoid this, we force it to #f
   (set! *compiler-debug* 0)
   (set! *cnst-table* (def-global-svar! (typed-cnst-table-id)
			 *module*
			 'cnst-vector
			 'now))
   ;; when compiling for bdb2, default global variable are exported. of
   ;; course this may lead to incorrect compilation if the global variable
   ;; (its name is not gensymed nor qualified) is exported.
   (if (and (>=fx *bdb-debug* 3)
	    (memq 'bdb (backend-debug-support (the-backend))))
       (global-import-set! *cnst-table* 'static))
   (set! *cnst-offset* 0)
   (set! *string-env* (make-hashtable))
   (set! *ucs2string-env* (make-hashtable))
   (set! *real-env* '())
   (set! *symbol-env* (make-hashtable))
   (set! *keyword-env* (make-hashtable))
   (set! *bignum-env* '())
   #t)

;*---------------------------------------------------------------------*/
;*    stop-cnst-alloc! ...                                             */
;*---------------------------------------------------------------------*/
(define (stop-cnst-alloc!)
   (set! *compiler-debug* *old-debug*)
   (set! *string-env*     #unspecified)
   (set! *ucs2string-env* #unspecified)
   (set! *real-env*       #unspecified) 
   (set! *symbol-env*     #unspecified)
   (set! *keyword-env*    #unspecified)
   (set! *bignum-env*     #unspecified)
   (set! *list-env*       #unspecified) 
   (set! *vector-env*     #unspecified)
   (set! *struct-env*     #unspecified)
   #t)

;*---------------------------------------------------------------------*/
;*    make-cnst-table-ref ...                                          */
;*---------------------------------------------------------------------*/
(define (make-cnst-table-ref offset loc)
   (instantiate::app
      (loc loc)
      (type (get-default-type))
      (fun (instantiate::var
	      (loc loc)
	      (type (variable-type *cnst-table-ref*))
	      (variable *cnst-table-ref*)))
      (args (list (instantiate::atom
		     (loc loc)
		     (type *int*)
		     (value offset))))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-string ...                                            */
;*    -------------------------------------------------------------    */
;*    Strings can't be allocated via the reader because we use         */
;*    the string constant compilation to create the                    */
;*    input-string-port!                                               */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-string string loc)
   (define (alloc-string)
      ;; in lib-mode string are statically allocated
      (let ((var (def-global-scnst! (make-typed-ident (gensym 'string)
						      'bstring)
		    *module*
		    string
		    'sstring
		    loc)))
	 (if *shared-cnst?*
	     (hashtable-put! *string-env* string (cnst-info string var)))
	 (instantiate::var
	    (loc loc)
	    (type *bstring*)
	    (variable var))))
   (let ((old (and *shared-cnst?* (hashtable-get *string-env* string))))
      (cond
	 (old
	  (instantiate::var
	     (loc loc)
	     (type *bstring*)
	     (variable (cnst-info-offset old))))
	 (else
	  (alloc-string)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-ucs2-string ...                                       */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-ucs2-string string loc)
   (define (lib-alloc-ucs2string bstring)
      (let ((var (def-global-svar! (make-typed-ident (gensym 'ucs2string)
						     'ucs2string)
		    *module*
		    'an-ucs2-string
		    'now))
	    (vs (cnst-alloc-string bstring loc)))
	 (hashtable-put! *ucs2string-env* string (cnst-info string var))
	 (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				,(coerce!
				  (instantiate::app
				     (loc loc)
				     (type *ucs2string*)
				     (fun (instantiate::var
					     (loc loc)
					     (type *ucs2string*)
					     (variable *string->ucs2string*)))
				     (args (list vs)))
				  var
				  *obj*
				  #f)))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (read-alloc-ucs2string)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons string *global-set*))
	 (hashtable-put! *ucs2string-env* string (cnst-info string offset))
	 (make-cnst-table-ref offset loc)))
   (let ((old (hashtable-get *ucs2string-env* string))
	 (string-as-bstring (ucs2-string->utf8-string string)))
      (cond
	 (old
	  (if (eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type (get-default-type))
		 (variable (cnst-info-offset old)))
	      (make-cnst-table-ref (cnst-info-offset old) loc)))
	 ((or (eq? *init-mode* 'lib)
	      (=fx (string-length string-as-bstring) 0)
	      (char=? (string-ref string-as-bstring 0) #\;))
	  (lib-alloc-ucs2string string-as-bstring))
	 (else
	  (read-alloc-ucs2string)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-symbol ...                                            */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-symbol symbol loc)
   (define (lib-alloc-symbol)
      (let ((var (def-global-svar! (make-typed-ident (gensym 'symbol) 'symbol)
		    *module*
		    'a-symbol
		    'now))
	    (vs (cnst-alloc-string (symbol->string symbol) loc)))
	 (hashtable-put! *symbol-env* symbol (cnst-info symbol var))
	 (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				,(coerce!
				  (instantiate::app
				     (loc loc)
				     (type *symbol*)
				     (fun (instantiate::var
					     (loc loc)
					     (type *symbol*)
					     (variable *bstring->symbol*)))
				     (args (list vs)))
				  var
				  *obj*
				  #f)))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (read-alloc-symbol)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons symbol *global-set*))
	 (hashtable-put! *symbol-env* symbol (cnst-info symbol offset))
	 (make-cnst-table-ref offset loc)))
   (let ((old (hashtable-get *symbol-env* symbol))
	 (symbol-as-string (symbol->string symbol)))
      (cond
	 ((cnst-info? old)
	  (cond
	     ((variable? (cnst-info-offset old))
	      (instantiate::var
		 (loc loc)
		 (type *symbol*)
		 (variable (cnst-info-offset old))))
	     ((eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type *symbol*)
		 (variable (cnst-info-offset old))))
	     (else
	      (make-cnst-table-ref (cnst-info-offset old) loc))))
	 (old
	  (internal-error 'cnst-alloc-symbol
			  "old should be either #f or cnst-info"
			  (shape old)))
	 ((or (eq? *init-mode* 'lib)
	      (=fx (string-length symbol-as-string) 0)
	      (char=? (string-ref symbol-as-string 0) #\;))
	  (lib-alloc-symbol))
	 (else
	  (read-alloc-symbol)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-keyword ...                                           */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-keyword keyword loc)
   (define (lib-alloc-keyword)
      (let ((var (def-global-svar! (make-typed-ident (gensym 'keyword)
						     'keyword)
		    *module*
		    'a-keyword
		    'now))
	    (vs (cnst-alloc-string (keyword->string keyword) loc)))
	 (hashtable-put! *keyword-env* keyword (cnst-info keyword var))
	 (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				,(coerce!
				  (instantiate::app
				     (loc loc)
				     (type *keyword*)
				     (fun (instantiate::var
					     (loc loc)
					     (type *keyword*)
					     (variable *bstring->keyword*)))
				     (args (list vs)))
				  var
				  *obj*
				  #f)))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (read-alloc-keyword)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons keyword *global-set*))
	 (hashtable-put! *keyword-env* keyword (cnst-info keyword offset))
	 (make-cnst-table-ref offset loc)))
   (let ((old (hashtable-get *keyword-env* keyword))
	 (keyword-as-string (keyword->string keyword)))
      (cond
	 (old
	  (if (eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type *keyword*)
		 (variable (cnst-info-offset old)))
	      (make-cnst-table-ref (cnst-info-offset old) loc)))
	 ((or (eq? *init-mode* 'lib)
	      (=fx (string-length keyword-as-string) 0)
	      (char=? (string-ref keyword-as-string 0) #\;))
	  (lib-alloc-keyword))
	 (else
	  (read-alloc-keyword)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-bignum ...                                            */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-bignum bignum loc)
   (define (lib-alloc-bignum)
      ;; bignum literal cannot be used in the library because of the cyclic
      ;; initialization of fixnum and bignum modules
      #f)
   (define (read-alloc-bignum)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons bignum *global-set*))
	 (set! *bignum-env* (cons (cons bignum (cnst-info bignum offset))
				  *bignum-env*))
	 (make-cnst-table-ref offset loc)))
   (let ((old (assoc bignum *bignum-env*))
	 (bignum-as-string (bignum->string bignum)))
      (cond
	 ((pair? old)
	  (cond
	     ((variable? (cnst-info-offset (cdr old)))
	      (instantiate::var
		 (loc loc)
		 (type (variable-type (cnst-info-offset (cdr old))))
		 (variable (cnst-info-offset (cdr old)))))
	     ((eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type (variable-type (cnst-info-offset (cdr old))))
		 (variable (cnst-info-offset (cdr old)))))
	     (else
	      (make-cnst-table-ref (cnst-info-offset (cdr old)) loc))))
	 ((and (pair? old) (cdr old))
	  (internal-error 'cnst-alloc-bignum
			  "old should be either #f or cnst-info"
			  (shape old)))
	 ((or (eq? *init-mode* 'lib)
	      (=fx (string-length bignum-as-string) 0)
	      (char=? (string-ref bignum-as-string 0) #\;))
	  (lib-alloc-bignum))
	 (else
	  (read-alloc-bignum)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-procedure ...                                         */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-procedure procedure loc)
   (let ((var (def-global-scnst! (make-typed-ident (gensym 'proc) 'procedure)
		 *module*
		 procedure
		 'sfun
		 loc)))
      (instantiate::var
	 (loc loc)
	 (type (variable-type var))
	 (variable var))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-real ...                                              */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-real real loc)
   (define (lib-alloc-real)
      (let ((var (def-global-scnst! (make-typed-ident (gensym 'real) 'real)
		    *module*
		    real
		    'sreal
		    loc)))
	 (set! *real-env* (cons (cons real var) *real-env*))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (find-real)
      (let loop ((list *real-env*))
	 (cond
	    ((null? list)
	     #f)
	    ((=fl (car (car list)) real)
	     (cdr (car list)))
	    (else
	     (loop (cdr list))))))
   (let ((old (find-real)))
      (cond
	 (old
	  (instantiate::var
	     (loc loc)
	     (type (variable-type old))
	     (variable old)))
	 (else
	  (lib-alloc-real)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-elong ...                                             */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-elong elong loc)
   (define (lib-alloc-elong)
      (let ((var (def-global-scnst! (make-typed-ident (gensym 'elong) 'belong)
		    *module*
		    elong
		    'selong
		    loc)))
	 (set! *elong-env* (cons (cons elong var) *elong-env*))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (find-elong)
      (let loop ((list *elong-env*))
	 (cond
	    ((null? list)
	     #f)
	    ((=elong (car (car list)) elong)
	     (cdr (car list)))
	    (else
	     (loop (cdr list))))))
   (let ((old (find-elong)))
      (cond
	 (old
	  (instantiate::var
	     (loc loc)
	     (type (variable-type old))
	     (variable old)))
	 (else
	  (lib-alloc-elong)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-llong ...                                             */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-llong llong loc)
   (define (lib-alloc-llong)
      (let ((var (def-global-scnst! (make-typed-ident (gensym 'llong) 'bllong)
		    *module*
		    llong
		    'sllong
		    loc)))
	 (set! *llong-env* (cons (cons llong var) *llong-env*))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (find-llong)
      (let loop ((list *llong-env*))
	 (cond
	    ((null? list)
	     #f)
	    ((=llong (car (car list)) llong)
	     (cdr (car list)))
	    (else
	     (loop (cdr list))))))
   (let ((old (find-llong)))
      (cond
	 (old
	  (instantiate::var
	     (loc loc)
	     (type (variable-type old))
	     (variable old)))
	 (else
	  (lib-alloc-llong)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-list ...                                              */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-list pair loc)
   (define (cnst-list pair)
      (let loop ((pair pair))
	 (cond
	    ((null? pair)
	     (instantiate::atom (loc loc) (type *bnil*)  (value '())))
	    ((not (pair? pair))
	     (cnst! (instantiate::kwote
		       (loc loc)
		       (type *pair*)
		       (value pair))))
	    (else
	     (instantiate::app
		(loc loc)
		(type *pair*)
		(fun (instantiate::var
			(loc loc)
			(type (variable-type *cons*))
			(variable *cons*)))
		(args (list (cnst! (instantiate::kwote
				      (loc loc)
				      (type (get-default-type))
				      (value (car pair))))
			    (loop (cdr pair)))))))))
   (define (lib-alloc-list)
      (let ((var (def-global-svar! (make-typed-ident (gensym 'list) 'pair)
		    *module*
		    'cnst-list
		    'now)))
	 (if *shared-cnst?*
	     (set! *list-env* (cons (cnst-info pair var) *list-env*)))
	 (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				,(coerce! (cnst-list pair) var *obj* #f)))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (read-alloc-list)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons pair *global-set*))
	 (if *shared-cnst?*
	     (set! *list-env* (cons (cnst-info pair offset) *list-env*)))
	 (make-cnst-table-ref offset loc)))
   (let ((old (and *shared-cnst?*
		   (let loop ((env *list-env*))
		      (cond
			 ((null? env)
			  #f)
			 ((cnst-equal? (cnst-info-cnst (car env)) pair)
			  (car env))
			 (else
			  (loop (cdr env))))))))
      (cond
	 (old
	  (if (eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type (variable-type (cnst-info-offset old)))
		 (variable (cnst-info-offset old)))
	      (make-cnst-table-ref (cnst-info-offset old) loc)))
	 ((eq? *init-mode* 'lib)
	  (lib-alloc-list))
	 (else
	  (read-alloc-list)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-vector ...                                            */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-vector vec loc)
   (define (cnst-vector-node)
      (cnst!
       (coerce!
	(let ((var (make-local-svar 'var *vector*)))
	   (instantiate::let-var
	      (loc loc)
	      (type *obj*)
	      (bindings
	       (list (cons
		      var
		      (instantiate::app
			 (loc loc)
			 (type *vector*)
			 (fun (instantiate::var
				 (loc loc)
				 (type (variable-type *list->vector*))
				 (variable *list->vector*)))
			 (args (list
				(instantiate::kwote
				   (loc loc)
				   (type *obj*)
				   (value (vector->list vec)))))))))
	      (body (let ((var-body (instantiate::var
				       (loc loc)
				       (type (variable-type var))
				       (variable var))))
		       (if (>fx (vector-tag vec) 0)
			   (instantiate::sequence
			      (loc loc)
			      (type *vector*)
			      (nodes
			       (list
				(instantiate::app
				   (loc loc)
				   (type *obj*)
				   (fun (instantiate::var
					   (loc loc)
					   (type (variable-type
						  *vector-tag-set!*))
					   (variable *vector-tag-set!*)))
				   (args (list
					  (instantiate::var
					     (loc loc)
					     (type (variable-type var))
					     (variable var))
					  (instantiate::atom
					     (loc loc)
					     (type *obj*)
					     (value (vector-tag vec))))))
				var-body)))
			   var-body)))))
	#unspecified
	*obj*
	#f)))
   (define (read-alloc-vector)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons vec *global-set*))
	 (if *shared-cnst?*
	     (set! *vector-env* (cons (cnst-info vec offset) *vector-env*)))
	 (make-cnst-table-ref offset loc)))
   (define (lib-alloc-vector)
      (let ((var (def-global-svar! (make-typed-ident (gensym 'vector) 'vector)
		    *module*
		    'cnst-vector
		    'now)))
	 (if *shared-cnst?*
	     (set! *vector-env* (cons (cnst-info vec var) *vector-env*)))
	 (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				,(cnst-vector-node)))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (let ((old (and *shared-cnst?*
		   (let loop ((env *vector-env*))
		      (cond
			 ((null? env)
			  #f)
			 ((cnst-equal? (cnst-info-cnst (car env)) vec)
			  (car env))
			 (else
			  (loop (cdr env))))))))
      (cond
	 (old
	  (if (eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type (variable-type (cnst-info-offset old)))
		 (variable (cnst-info-offset old)))
	      (make-cnst-table-ref (cnst-info-offset old) loc)))
	 ((eq? *init-mode* 'lib)
	  (lib-alloc-vector))
	 (else
	  (read-alloc-vector)))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-homogenous-vector ...                                 */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-homogenous-vector vec loc)

   (let* ((vec-tag (multiple-value-bind (tag _ _ _)
		      (homogeneous-vector-info vec)
		      tag))
	  (vec-type-id (symbol-append vec-tag 'vector))
	  (vec-type (find-type vec-type-id))
	  (list->vector (symbol-append 'list-> vec-tag 'vector))
	  (vector->list (case vec-tag
			   ((s8) s8vector->list)
			   ((u8) u8vector->list)
			   ((s16) s16vector->list)
			   ((u16) u16vector->list)
			   ((s32) s32vector->list)
			   ((u32) u32vector->list)
			   ((s64) s64vector->list)
			   ((u64) u64vector->list)
			   ((f32) f32vector->list)
			   ((f64) f64vector->list))))
      
      (define (cnst-vector-node)
	 (cnst!
	  (coerce!
	   (let ((var (make-local-svar 'var vec-type)))
	      (instantiate::let-var
		 (loc loc)
		 (type *obj*)
		 (bindings
		  (list (cons
			 var
			 (instantiate::app
			    (loc loc)
			    (type vec-type)
			    (fun (instantiate::var
				    (loc loc)
				    (type vec-type)
				    (variable (find-global list->vector))))
			    (args (list
				   (instantiate::kwote
				      (loc loc)
				      (type *obj*)
				      (value (vector->list vec)))))))))
		 (body (instantiate::var
			  (loc loc)
			  (type vec-type)
			  (variable var)))))
	   #unspecified
	   *obj*
	   #f)))
      
      (define (read-alloc-vector)
	 (let ((offset *cnst-offset*))
	    (set! *cnst-offset* (+fx 1 *cnst-offset*))
	    (set! *global-set* (cons vec *global-set*))
	    (if *shared-cnst?*
		(set! *vector-env* (cons (cnst-info vec offset) *vector-env*)))
	    (make-cnst-table-ref offset loc)))
      
      (define (lib-alloc-vector)
	 (let ((var (def-global-svar!
		       (make-typed-ident (gensym 'hvector) vec-type-id)
		       *module*
		       'cnst-vector
		       'now)))
	    (if *shared-cnst?*
		(set! *vector-env* (cons (cnst-info vec var) *vector-env*)))
	    (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				   ,(cnst-vector-node)))
	    (instantiate::var
	       (loc loc)
	       (type vec-type)
	       (variable var))))
      
      (let ((old (and *shared-cnst?*
		      (let loop ((env *vector-env*))
			 (cond
			    ((null? env)
			     #f)
			    ((cnst-equal? (cnst-info-cnst (car env)) vec)
			     (car env))
			    (else
			     (loop (cdr env))))))))
	 (cond
	    (old
	     (if (eq? *init-mode* 'lib)
		 (instantiate::var
		    (loc loc)
		    (type (variable-type (cnst-info-offset old)))
		    (variable (cnst-info-offset old)))
		 (make-cnst-table-ref (cnst-info-offset old) loc)))
	    ((eq? *init-mode* 'lib)
	     (lib-alloc-vector))
	    (else
	     (read-alloc-vector))))))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-tvector ...                                           */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-tvector tvec loc)
   (define (C-static-alloc-tvector)
      (let ((var (def-global-scnst! (make-typed-ident (gensym 'tvec) 'tvector)
		    *module*
		    tvec
		    'stvector
		    loc)))
	 (let* ((id  (type-id (a-tvector-type tvec)))
		(aid (cnst! (instantiate::kwote
			       (loc loc)
			       (type (get-default-type))
			       (value id)))))
	    ;; we first need to compile the symbol holding the identifier
	    ;; because this compilation modifies the *global-sexp*
	    ;; variable.
	    (add-cnst-sexp! `($tvector-descr-set!
			      (@ ,(global-id var) ,(global-module var))
			      (get-tvector-descriptor ,aid))))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (define (read-alloc-tvector)
      (internal-error "read-alloc-tvector"
		      "Unimplementable until bootstrap"
		      tvec)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons tvec *global-set*))
	 (make-cnst-table-ref offset loc)))
   (if (tvector-C-static? tvec)
       (C-static-alloc-tvector)
       (read-alloc-tvector)))

;*---------------------------------------------------------------------*/
;*    cnst-alloc-struct ...                                            */
;*---------------------------------------------------------------------*/
(define (cnst-alloc-struct struct loc)
   (define (cnst-struct-node)
      (cnst!
       (coerce!
	(let ((var (make-local-svar 'var *struct*)))
	   (instantiate::let-var
	      (loc loc)
	      (type *obj*)
	      (bindings
	       (list (cons
		      var
		      (instantiate::app
			 (loc loc)
			 (type *struct*)
			 (fun (instantiate::var
				 (loc loc)
				 (type (variable-type *list->struct*))
				 (variable *list->struct*)))
			 (args (list
				(instantiate::kwote
				   (loc loc)
				   (type *obj*)
				   (value (struct->list struct)))))))))
	      (body (instantiate::var
		       (loc loc)
		       (type (variable-type var))
		       (variable var)))))
	#unspecified
	*obj*
	#f)))
   (define (read-alloc-struct)
      (let ((offset *cnst-offset*))
	 (set! *cnst-offset* (+fx 1 *cnst-offset*))
	 (set! *global-set* (cons struct *global-set*))
	 (if *shared-cnst?*
	     (set! *struct-env* (cons (cnst-info struct offset) *struct-env*)))
	 (make-cnst-table-ref offset loc)))
   (define (lib-alloc-struct)
      (let ((var (def-global-svar! (make-typed-ident (gensym 'struct) 'struct)
		    *module*
		    'cnst-struct
		    'now)))
	 (if *shared-cnst?*
	     (set! *struct-env* (cons (cnst-info struct var) *struct-env*)))
	 (add-cnst-sexp! `(set! (@ ,(global-id var) ,(global-module var))
				,(cnst-struct-node)))
	 (instantiate::var
	    (loc loc)
	    (type (variable-type var))
	    (variable var))))
   (let ((old (and *shared-cnst?*
		   (let loop ((env *struct-env*))
		      (cond
			 ((null? env)
			  #f)
			 ((cnst-equal? (cnst-info-cnst (car env)) struct)
			  (car env))
			 (else
			  (loop (cdr env))))))))
      (cond
	 (old
	  (if (eq? *init-mode* 'lib)
	      (instantiate::var
		 (loc loc)
		 (type (variable-type (cnst-info-offset old)))
		 (variable (cnst-info-offset old)))
	      (make-cnst-table-ref (cnst-info-offset old) loc)))
	 ((eq? *init-mode* 'lib)
	  (lib-alloc-struct))
	 (else
	  (read-alloc-struct)))))

;*---------------------------------------------------------------------*/
;*    cnst-equal? ...                                                  */
;*---------------------------------------------------------------------*/
(define (cnst-equal? obj1 obj2)
   (cond
      ((eq? obj1 obj2)
       #t)
      ((number? obj1)
       #f)
      ((string? obj1)
       (and (string? obj2) (string=? obj1 obj2)))
      ((symbol? obj1)
       #f)
      ((pair? obj1)
       (and (pair? obj2)
	    (cnst-equal? (car obj1) (car obj2))
	    (cnst-equal? (cdr obj1) (cdr obj2))))
      ((vector? obj1)
       (let ((lobj1 (vector-length obj1)))
	  (and (vector? obj2)
	       (=fx (vector-length obj2) lobj1)
	       (=fx (vector-tag obj1) (vector-tag obj2))
	       (let test ((i 0))
		  (or (=fx i lobj1)
		      (and (cnst-equal? (vector-ref-ur obj1 i)
					(vector-ref-ur obj2 i))
			   (test (+fx i 1))))))))
      ((homogeneous-vector? obj1)
       (equal? obj1 obj2))
      ((c-struct? obj1)
       (let ((lobj1 (struct-length obj1)))
	  (and (struct? obj2)
	       (=fx (struct-length obj2) lobj1)
	       (let test ((i 0))
		  (or (=fx i lobj1)
		      (and (cnst-equal? (struct-ref obj1 i) (struct-ref obj2 i))
			   (test (+fx i 1))))))))
      ((cell? obj1)
       (and (cell? obj2) (cnst-equal? (cell-ref obj1) (cell-ref obj2))))
      ((ucs2-string? obj1)
       (and (ucs2-string? obj2) (ucs2-string=? obj1 obj2)))
      ((ucs2? obj1)
       (and (ucs2? obj2) (ucs2=? obj1 obj2)))
      ((date? obj1)
       (equal? obj1 obj2))
      (else
       #f)))
