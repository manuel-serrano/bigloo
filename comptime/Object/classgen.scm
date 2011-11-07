;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/classgen.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  6 06:14:12 2011                          */
;*    Last change :  Mon Nov  7 12:34:12 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generate the class accessors.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_classgen
   (include "Engine/pass.sch")
   (import  tools_error
	    type_type
	    type_tools
	    engine_param
	    object_class
	    object_slots
	    object_nil
	    ast_var
	    ast_ident
	    ast_private
	    ast_node
	    backend_backend
	    module_module
	    write_scheme)
   (export  (classgen-walk)
	    (classgen-predicate-anonymous ::tclass)
	    (classgen-nil-expr ::tclass)
	    (classgen-make-anonymous ::tclass)
	    (classgen-allocate-expr ::tclass)
	    (classgen-allocate-anonymous ::tclass)
	    (classgen-slot-anonymous ::tclass ::slot)))

;*---------------------------------------------------------------------*/
;*    classgen-walk ...                                                */
;*---------------------------------------------------------------------*/
(define (classgen-walk)
   (pass-prelude "Class accessors generation")
   ;; output file
   (let* ((oname (cond
		    ((string? *dest*)
		     *dest*)
		    ((eq? *dest* '--to-stdout)
		     #f)
		    ((and (pair? *src-files*) (string? (car *src-files*)))
		     (string-append (prefix (car *src-files*)) ".sch"))
		    (else
		     #f)))
	  (po (if (string? oname)
		  (open-output-file oname)
		  (current-output-port))))
      ;; the header
      (write-scheme-file-header po "Class accessors")
      ;; we emit the generated type for the classes
      (let ((protos '())
	    (defs '()))
	 (for-each (lambda (c)
		      (when (bigloo-domestic-class? c)
			 (multiple-value-bind (p d)
			    (classgen c)
			    (set! protos (cons (cons c p) protos))
			    (set! defs (cons (cons c d) defs)))))
	    (get-class-list))
	 (when (pair? protos)
	    (write-scheme-comment po "The directives")
	    (display "(directives\n   (cond-expand (bigloo-class-sans\n\n" po)
	    (for-each (lambda (p)
			 (let ((c (car p))
			       (protos (cdr p)))
			    (write-scheme-comment po (tclass-id c))
			    (display "(" po)
			    (display (car protos) po)
			    (newline po)
			    (for-each (lambda (p)
					 (display "  " po)
					 (display p po)
					 (newline po))
			       (cdr protos))
			    (newline po)))
	       protos)
	    (display ")))\n\n" po)
	    (write-scheme-comment po "The definitions")
	    (display "(cond-expand (bigloo-class-sans\n\n" po)
	    (for-each (lambda (p)
			 (let ((c (car p))
			       (defs (cdr p)))
			    (write-scheme-comment po (tclass-id c))
			    (for-each (lambda (p)
					 (display p po)
					 (newline po))
			       defs)
			    (newline po)))
	       defs)
	    (display "))\n" po)
	    (close-output-port po)))))

;*---------------------------------------------------------------------*/
;*    bigloo-domestic-class? ...                                       */
;*---------------------------------------------------------------------*/
(define (bigloo-domestic-class? c)
   (and (tclass? c) (eq? (global-module (tclass-holder c)) *module*)))

;*---------------------------------------------------------------------*/
;*    classgen ...                                                     */
;*---------------------------------------------------------------------*/
(define (classgen c)
   (multiple-value-bind (pred-p pred-d)
      (classgen-predicate c)
      (multiple-value-bind (make-p make-d)
	 (classgen-make c)
	 (multiple-value-bind (nil-p nil-d)
	    (classgen-nil c)
	    (multiple-value-bind (access-p access-d)
	       (classgen-accessors c)
	       (values
		  `(,(class-import c)
		    ,pred-p
		    ,make-p
		    ,nil-p
		    ,@access-p)
		  (cons* pred-d make-d nil-d access-d)))))))

;*---------------------------------------------------------------------*/
;*    class-import ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-import c)
   (global-import (tclass-holder c)))

;*---------------------------------------------------------------------*/
;*    classgen-predicate ...                                           */
;*---------------------------------------------------------------------*/
(define (classgen-predicate c)
   
   (define (predicate-def id g)
      `(define-inline (,id obj::obj)
	  ((@ is-a? __object) obj (@ ,(global-id g) ,(global-module g)))))

   (let* ((holder (tclass-holder c))
	  (id (symbol-append (tclass-id c) '?))
	  (tid (make-typed-ident id 'bool)))
      (values
	 `(inline ,id ::obj)
	 (predicate-def tid holder))))

;*---------------------------------------------------------------------*/
;*    classgen-predicate-anonymous ...                                 */
;*---------------------------------------------------------------------*/
(define (classgen-predicate-anonymous c)
   (multiple-value-bind (proto def)
      (classgen-predicate c)
      (match-case def
	 ((?- (?id . ?formals) ?body)
	  `(,(make-typed-ident 'lambda 'bool) ,formals ,body)))))

;*---------------------------------------------------------------------*/
;*    classgen-make ...                                                */
;*---------------------------------------------------------------------*/
(define (classgen-make c)
   
   (define (make-def id tid slots formals tformals)
      (let ((plain-slots (filter (lambda (s) (not (slot-virtual? s))) slots)))
	 `(define-inline (,id ,@tformals)
	     (,(make-typed-ident 'instantiate tid)
	      ,@(map (lambda (s f)
			(list (slot-id s) f))
		   plain-slots formals)))))

   (let* ((tid (type-id c))
	  (slots (tclass-slots c))
	  (id (symbol-append 'make- tid))
	  (mk-tid (make-typed-ident id tid))
	  (f-ids (make-class-make-formals slots))
	  (f-tids (make-class-make-typed-formals f-ids slots)))
      (values
	 `(inline ,mk-tid ,@f-tids)
	 (make-def mk-tid tid slots f-ids f-tids))))

;*---------------------------------------------------------------------*/
;*    classgen-nil ...                                                 */
;*---------------------------------------------------------------------*/
(define (classgen-nil c)
   
   (define (nil-def id tid slots)
      (let* ((plain-slots (filter (lambda (s) (not (slot-virtual? s))) slots))
	     (new (gensym 'new))
	     (tnew (make-typed-ident new tid)))
	 `(define (,id)
	     (let ((,tnew ,(classgen-allocate-expr c)))
		,@(map (lambda (s)
			  `(set! ,(symbol-append '__bigloo__ '|.| new '|.| (slot-id s)) ,(type-nil-value (slot-type s))))
		     plain-slots)
		,new))))
   
   (let* ((tid (type-id c))
	  (slots (tclass-slots c))
	  (id (symbol-append (type-id c) '-nil))
	  (nil-tid (make-typed-ident id tid)))
      (values
	 `(,nil-tid)
	 (nil-def nil-tid tid slots))))

;*---------------------------------------------------------------------*/
;*    classgen-nil-expr ...                                            */
;*---------------------------------------------------------------------*/
(define (classgen-nil-expr c)
   (multiple-value-bind (proto def)
      (classgen-nil c)
      (match-case def
	 ((?- ?- ?body)
	  body))))
   
;*---------------------------------------------------------------------*/
;*    classgen-make-anonymous ...                                      */
;*---------------------------------------------------------------------*/
(define (classgen-make-anonymous c)
   (if (tclass-abstract? c)
       `(lambda l
	   (error ,(symbol->string (type-id c))
	      "Can't allocate instance of abstract classes"
	      #f))
       (multiple-value-bind (proto def)
	  (classgen-make c)
	  (match-case def
	     ((?- (?id . ?formals) ?body)
	      `(,(make-typed-ident 'lambda (type-id c)) ,formals ,body))))))

;*---------------------------------------------------------------------*/
;*    classgen-allocate ...                                            */
;*---------------------------------------------------------------------*/
(define (classgen-allocate c)
   
   (define (c-malloc tid)
      (let ((tname  (string-sans-$ (type-name c)))
	    (sizeof (if (string? (type-size c))
			(type-size c)
			(type-name c))))
	 `(,(make-typed-ident 'free-pragma tid)
	   ,(string-append "((" tname
	       ")BREF( GC_MALLOC ( sizeof(" sizeof ") )))"))))
   
   (define (pragma-allocate id tid g)
      (let ((new (mark-symbol-non-user! (gensym 'new))))
	 `(define-inline (,id)
	     (let ((,(make-typed-ident new tid) ,(c-malloc tid)))
		(object-class-num-set! ,new
		   ((@ class-num __object)
		    (@ ,(global-id g) ,(global-module g))))
		(object-widening-set! ,new #f)
		,new))))
   
   (define (nopragma-allocate id tid g)
      (let ((new (mark-symbol-non-user! (gensym 'new))))
	 `(define-inline (,id)
	     (let ((,(make-typed-ident new tid) ,(make-private-sexp 'new tid)))
		(object-class-num-set! ,new
		   ((@ class-num __object)
		    (@ ,(global-id g) ,(global-module g))))
		(object-widening-set! ,new #f)
		,new))))
   
   (let* ((tid (type-id c))
	  (alloc-id (symbol-append '%allocate- tid))
	  (alloc-tid (make-typed-ident alloc-id tid))
	  (holder (tclass-holder c)))
      (values
	 `(inline ,alloc-tid)
	 (if (backend-pragma-support (the-backend))
	     (pragma-allocate alloc-tid tid holder)
	     (nopragma-allocate alloc-tid tid holder)))))

;*---------------------------------------------------------------------*/
;*    classgen-allocate-expr ...                                       */
;*---------------------------------------------------------------------*/
(define (classgen-allocate-expr c)
   (if (tclass-abstract? c)
       `(error ,(symbol->string (type-id c))
	   "Can't allocate instance of abstract classes"
	   #f)
       (multiple-value-bind (proto def)
	  (classgen-allocate c)
	  (match-case def
	     ((?- ?- ?body)
	      body)))))

;*---------------------------------------------------------------------*/
;*    classgen-allocate-anonymous ...                                  */
;*---------------------------------------------------------------------*/
(define (classgen-allocate-anonymous c)
   `(,(make-typed-ident 'lambda (type-id c)) () ,(classgen-allocate-expr c)))

;*---------------------------------------------------------------------*/
;*    classgen-accessors ...                                           */
;*---------------------------------------------------------------------*/
(define (classgen-accessors c)
   (let ((protos '())
	 (defs '()))
      (for-each (lambda (s)
		   (multiple-value-bind (p d)
		      (classgen-slot c s)
		      (set! protos (append p protos))
		      (set! defs (append d defs))))
	 (tclass-slots c))
      (values protos defs)))

;*---------------------------------------------------------------------*/
;*    classgen-slot ...                                                */
;*---------------------------------------------------------------------*/
(define (classgen-slot c s)
   
   (define (get-proto s)
      (let* ((tid (type-id c))
	     (id (symbol-append tid '- (slot-id s)))
	     (gid (make-typed-ident id (type-id (slot-type s))))
	     (o (make-typed-formal tid)))
	 `(inline ,gid ,o)))
   
   (define (set-proto s)
      (let* ((tid (type-id c))
	     (id (symbol-append tid '- (slot-id s) '-set!))
	     (o (make-typed-formal tid))
	     (v (make-typed-formal (type-id (slot-type s)))))
	 `(inline ,id ,o ,v)))
   
   (define (get-def s)
      (let* ((tid (type-id c))
	     (sid (slot-id s))
	     (o (make-typed-ident 'o tid))
	     (id (symbol-append tid '- sid))
	     (gid (make-typed-ident id (type-id (slot-type s)))))
	 `(define-inline (,gid ,o)
	     (,(make-typed-ident 'with-access tid) o (,sid) ,sid))))
   
   (define (set-def s)
      (let* ((tid (type-id c))
	     (sid (slot-id s))
	     (o (make-typed-ident 'o tid))
	     (id (symbol-append tid '- sid '-set!))
	     (v (make-typed-ident 'v (type-id (slot-type s)))))
	 `(define-inline (,id ,o ,v)
	     (,(make-typed-ident 'with-access tid) o (,sid) (set! ,sid v)))))

   (if (slot-read-only? s)
       (values (list (get-proto s))
	  (list (get-def s))))
       (values (list (get-proto s) (set-proto s))
	  (list (get-def s) (set-def s))))

;*---------------------------------------------------------------------*/
;*    classgen-slot-anonymous ...                                      */
;*---------------------------------------------------------------------*/
(define (classgen-slot-anonymous class s)
   (multiple-value-bind (_ d)
      (classgen-slot class s)
      (map (match-lambda
	      ((?- (?id . ?args) ?body)
	       (let* ((pid (parse-id id #f))
		      (tlam (make-typed-ident 'lambda (type-id (cdr pid)))))
		  `(,tlam ,args ,body))))
	 d)))
