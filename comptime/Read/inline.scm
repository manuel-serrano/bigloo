;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/inline.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 10:30:51 1994                          */
;*    Last change :  Wed Aug  2 16:04:54 2017 (serrano)                */
;*    Copyright   :  1994-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We scan files in order to find `inline' definitions.             */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_inline
   (include "Tools/trace.sch"
	    "Ast/unit.sch")
   (import  read_reader
	    tools_error
	    tools_speek
	    tools_shape
	    tools_progn
	    ast_ident
	    ast_env
	    type_type
	    ast_var
	    (find-location tools_location))
   (export  (look-for-inlines-and-macros inline macro syntax expander
	       code exps fnames module)
	    (inline-definition-queue::pair-nil)
	    (inline-finalizer)
	    (import-macro-finalizer)))

;*---------------------------------------------------------------------*/
;*    open-inline-file ...                                             */
;*---------------------------------------------------------------------*/
(define (open-inline-file file)
   (let ((path (find-file/path file *load-path*)))
      (if (string? path)
	  (let ((port (open-input-file path)))
	     (if (input-port? port)
		 port
		 (user-error 'inline
			     "Cannot open file for input"
			     path)))
	  (user-error 'inline "Cannot find source file" file))))

;*---------------------------------------------------------------------*/
;*    look-for-inlines-and-macros ...                                  */
;*    -------------------------------------------------------------    */
;*    We read until we have found all inline and macro definitions.    */
;*---------------------------------------------------------------------*/
(define (look-for-inlines-and-macros inlines macros syntaxes expanders
				     code exps fnames module)
   (let ((port (open-inline-file (car fnames))))
      (unwind-protect
	 (begin
	    ;; skip the module declaration
	    (compiler-read port #f)
	    ;; start reading the module body
	    (look-for-definitions inlines macros syntaxes expanders
				  code exps (cdr fnames) module port))
	 (close-input-port port))))

;*---------------------------------------------------------------------*/
;*    look-for-definitions ...                                         */
;*---------------------------------------------------------------------*/
(define (look-for-definitions inlines macros syntaxes expanders
	   code exps fnames module port)
   (let loop ((inlines inlines)
	      (macros macros)
	      (syntaxes syntaxes)
	      (expanders expanders)
	      (exp (cons 'begin (append exps code)))
	      (fnames fnames)
	      (port port))
      (multiple-value-bind (inlines macros syntaxes expanders)
	 (look-for/exp inlines macros syntaxes expanders exp module)
	 (cond
	    ((and (null? inlines) (null? macros)
		  (null? syntaxes) (null? expanders))
	     'done)
	    ((eof-object? exp)
	     (if (null? fnames)
		 (cond
		    ((pair? macros)
		     (user-error "import"
			"Can't find macro definition(s)"
			(map car macros)))
		    ((pair? syntaxes)
		     (user-error "import"
			"Can't find syntax definition(s)"
			(map car syntaxes)))
		    ((pair? expanders)
		     (user-error "import"
			"Can't find expander definition(s)"
			(map car expanders)))
		    (else
		     (user-error "import"
			"Can't find inline/generic definition(s)"
			(map car inlines))))
		 (let ((fname (find-file/path (car fnames) *load-path*)))
		    (if (not (string? fname))
			(user-error "import" "Can't find file" (car fnames))
			(let ((port (open-inline-file fname)))
			   (unwind-protect
			      (loop inlines
				 macros
				 syntaxes
				 expanders
				 (compiler-read port #t)
				 (cdr fnames)
				 port)
			      (if (input-port? port)
				  (close-input-port port))))))))
	    (else
	     (loop inlines
		macros
		syntaxes
		expanders
		(compiler-read port #t)
		fnames
		port))))))

;*---------------------------------------------------------------------*/
;*    look-for/exp ...                                                 */
;*---------------------------------------------------------------------*/
(define (look-for/exp inlines macros syntaxes expanders exp module)
   (if (and (null? inlines) (null? macros) (null? syntaxes) (null? expanders))
       (values '() '() '() '())
       (match-case exp
	  ((define-inline (and ?proto (?name . ?-)) . ?body)
	   (let* ((id (fast-id-of-id name (find-location exp)))
		  (cell (assq id inlines))
		  (inl (if (and (pair? cell) (eq? (cdr cell) 'sifun))
			   (begin
			      (set-car! proto `(@ ,id ,module))
			      (set! *inline-definitions*
				    (cons exp *inline-definitions*))
			      (remq! cell inlines))
			   inlines)))
	      (values inl macros syntaxes expanders)))
	  ((define-macro ((and ?id (? symbol?)) . ?-) . ?-)
	   (let* ((cell (assq id macros))
		  (mac (if (pair? cell)
			   (begin
			      (set! *macro-definitions*
				    (cons exp *macro-definitions*))
			      (remq! cell macros))
			   macros)))
	      (values inlines mac syntaxes expanders)))
	  ((define-syntax (and ?id (? symbol?)) . ?-)
	   (let* ((cell (assq id syntaxes))
		  (syn (if (pair? cell)
			   (begin
			      (set! *macro-definitions*
				    (cons exp *macro-definitions*))
			      (remq! cell syntaxes))
			   syntaxes)))
	      (values inlines macros syn expanders)))
	  ((define-expander (and ?id (? symbol?)) . ?-)
	   (let* ((cell (assq id expanders))
		  (expd (if (pair? cell)
			    (begin
			       (set! *macro-definitions*
				     (cons exp *macro-definitions*))
			       (remq! cell expanders))
			    expanders)))
	      (values inlines macros syntaxes expd)))
	  ((begin . ?exp*)
	   (let loop ((inlines inlines)
		      (macros macros)
		      (syntaxes syntaxes)
		      (expanders expanders)
		      (exp* exp*))
	      (if (null? exp*)
		  (values inlines macros syntaxes expanders)
		  (multiple-value-bind (inlines macros syntaxes expanders)
		     (look-for/exp inlines macros syntaxes expanders (car exp*) module)
		     (loop inlines macros syntaxes expanders (cdr exp*))))))
	  ((cond-expand . ?exp*)
	   ;; Can't really macro expand cond-expand so we use here
	   ;; a rough approximation. Some inline definition won't be
	   ;; found and programmers will have to re-write their code with
	   ;; plain define forms...
	   (let loop ((inlines inlines)
		      (macros macros)
		      (syntaxes syntaxes)
		      (expanders expanders)
		      (exp* (let loop ((exp* exp*))
			       (if (null? exp*)
				   '()
				   (match-case (car exp*)
				      (((and (? symbol?) ?clause) . ?r)
				       (if (or (compile-srfi? clause) (eq? clause 'else))
					   r
					   (loop (cdr exp*))))
				      ((bigloo-compile . ?rest)
				       rest)
				      (else
				       (loop (cdr exp*))))))))
	      (if (null? exp*)
		  inlines
		  (multiple-value-bind (inlines macros syntaxes expanders)
		     (look-for/exp inlines macros syntaxes expanders (car exp*) module)
		     (loop inlines macros syntaxes expanders (cdr exp*))))))
	  (else
	   (values inlines macros syntaxes expanders)))))

;*---------------------------------------------------------------------*/
;*    *inline-definitions* ...                                         */
;*---------------------------------------------------------------------*/
(define *inline-definitions* '())

;*---------------------------------------------------------------------*/
;*    inline-definition-queue ...                                      */
;*---------------------------------------------------------------------*/
(define (inline-definition-queue)
   *inline-definitions*)

;*---------------------------------------------------------------------*/
;*    inline-finalizer ...                                             */
;*---------------------------------------------------------------------*/
(define (inline-finalizer)
   (if (null? *inline-definitions*)
       '()
       (list (unit 'imported-inlines 0 *inline-definitions* #t #f))))

;*---------------------------------------------------------------------*/
;*    *macro-definitions* ...                                          */
;*---------------------------------------------------------------------*/
(define *macro-definitions* '())

;*---------------------------------------------------------------------*/
;*    import-macro-finalizer ...                                       */
;*---------------------------------------------------------------------*/
(define (import-macro-finalizer)
   (if (null? *macro-definitions*)
       '()
       (list (unit 'imported-macros 0 *macro-definitions* #t #f))))

       
     
   
