;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/classgen.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  6 06:14:12 2011                          */
;*    Last change :  Thu Nov 10 16:05:27 2011 (serrano)                */
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
	    ast_object
	    ast_node
	    backend_backend
	    module_module
	    write_scheme)
   (export  (classgen-walk)
	    (classgen-predicate-anonymous ::tclass)
	    (classgen-nil-anonymous ::tclass)
	    (classgen-make-anonymous ::tclass)
	    (classgen-allocate-expr ::tclass)
	    (classgen-allocate-anonymous ::tclass)
	    (classgen-widen-expr ::tclass ::obj)
	    (classgen-widen-anonymous ::tclass)
	    (classgen-shrink-anonymous ::tclass)
	    (classgen-slot-anonymous ::tclass ::slot)
	    (classgen-struct-methods ::tclass)))

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
	    (display "(directives\n   (cond-expand ((and bigloo-class-sans (not (bigloo-class-generate)))\n\n" po)
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
			    (display ")\n" po)))
	       protos)
	    (display ")))\n\n" po)
	    (write-scheme-comment po "The definitions")
	    (display "(cond-expand ((and bigloo-class-sans (not (bigloo-class-generate)))" po)
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
	  ((@ isa? __object) obj (@ ,(global-id g) ,(global-module g)))))

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
	     (let ((,tnew ,(if (wide-class? c)
			       (classgen-widen-expr c
				  (classgen-allocate-expr (tclass-its-super c)))
			       (classgen-allocate-expr c))))
		,@(map (lambda (s)
			  `(set! ,(field-access new (slot-id s))
			      ,(type-nil-value (slot-type s))))
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
;*    classgen-nil-anonymous ...                                       */
;*---------------------------------------------------------------------*/
(define (classgen-nil-anonymous c)
   (multiple-value-bind (proto def)
      (classgen-nil c)
      (match-case def
	 ((?- ?- ?body)
	  `(lambda () ,body)))))
   
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
;*    -------------------------------------------------------------    */
;*    Generate plain class allocators                                  */
;*---------------------------------------------------------------------*/
(define (classgen-allocate c)

   (define (unsafe expr)
      (make-private-sexp 'unsafe (type-id c) expr))
   
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
	     ,(unsafe 
		`(let ((,(make-typed-ident new tid) ,(c-malloc tid)))
		   (object-class-num-set! ,new
		      ((@ class-num __object)
		       (@ ,(global-id g) ,(global-module g))))
		   (object-widening-set! ,new #f)
		   ,new)))))
   
   (define (nopragma-allocate id tid g)
      (let ((new (mark-symbol-non-user! (gensym 'new))))
	 `(define-inline (,id)
	     ,(unsafe
		 `(let ((,(make-typed-ident new tid) ,(make-private-sexp 'new tid)))
		     (object-class-num-set! ,new
			((@ class-num __object)
			 (@ ,(global-id g) ,(global-module g))))
		     (object-widening-set! ,new #f)
		     ,new)))))

   [assert (c) (not (wide-class? c))]
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
;*    classgen-widen-expr ...                                          */
;*---------------------------------------------------------------------*/
(define (classgen-widen-expr c o)
   
   (define (pragma-allocate w)
      (let ((tname  (string-sans-$ (type-name w)))
	    (sizeof (if (string? (type-size w))
			(type-size w)
			(type-name w))))
	 `(,(make-typed-ident 'free-pragma (type-id w))
	   ,(string-append "((" tname
	       ")BREF( GC_MALLOC ( sizeof(" sizeof ") )))"))))
   
   (define (nopragma-allocate w)
      (make-private-sexp 'new (type-id w)))
   
   (let* ((w (tclass-wide-type c))
	  (s (tclass-its-super c))
	  (wid (type-id w))
	  (sid (type-id s))
	  (tmp (mark-symbol-non-user! (gensym 'tmp)))
	  (ttmp (make-typed-ident tmp sid))
	  (wide (mark-symbol-non-user! (gensym 'wide)))
	  (twide (make-typed-ident wide wid))
	  (holder (tclass-holder c)))
      `(let ((,ttmp ,(make-private-sexp 'cast sid o))
	     (,twide ,(if (backend-pragma-support (the-backend))
			  (pragma-allocate w)
			  (nopragma-allocate w))))
	  (object-widening-set! ,tmp ,wide)
	  ((@ object-class-num-set! __object)
	   ,tmp
	   ((@ class-num __object)
	    (@ ,(global-id holder) ,(global-module holder))))
	  ,tmp)))

;*---------------------------------------------------------------------*/
;*    classgen-widen-anonymous ...                                     */
;*---------------------------------------------------------------------*/
(define (classgen-widen-anonymous c)
   (let* ((s (tclass-its-super c))
	  (sid (type-id s))
	  (cid (type-id c))
	  (o (gensym 'o))
	  (to (make-typed-ident o sid))
	  (lam (make-typed-ident 'lambda cid)))
      `(,lam (,to) ,(classgen-widen-expr c o))))

;*---------------------------------------------------------------------*/
;*    classgen-shrink-anonymous ...                                    */
;*---------------------------------------------------------------------*/
(define (classgen-shrink-anonymous c)
   (let* ((o (gensym 'o))
	  (to (make-typed-ident o (type-id c)))
	  (super (tclass-its-super c))
	  (sid (type-id super)))
      `(,(make-typed-ident 'lambda sid)
	(,to)
	(,(make-typed-ident 'shrink! sid) ,o))))

;*---------------------------------------------------------------------*/
;*    classgen-accessors ...                                           */
;*---------------------------------------------------------------------*/
(define (classgen-accessors c)
   (let ((protos '())
	 (defs '()))
      (for-each (lambda (s)
		   (multiple-value-bind (p d)
		      (classgen-slot c s)
		      (if (slot-read-only? s)
			  (begin
			     (set! protos (cons (car p) protos))
			     (set! defs (append d defs)))
			  (begin
			     (set! protos (append p protos)) 
			     (set! defs (append d defs))))))
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
   
   (values (list (get-proto s) (set-proto s))
      ;; always build a getter and a setter, the former is
      ;; filtered out by classgen-accessors for read-only slots
      (list (get-def s)
	 (unless (and (slot-read-only? s) (slot-virtual? s))
	    (set-def s)))))

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

;*---------------------------------------------------------------------*/
;*    classgen-struct-methods ...                                      */
;*---------------------------------------------------------------------*/
(define (classgen-struct-methods c)
   (cond
      ((tclass-abstract? c) '())
      ((wide-class? c) (classgen-struct-wide c))
      (else (classgen-struct-plain c))))

;*---------------------------------------------------------------------*/
;*    classgen-struct-plain ...                                        */
;*---------------------------------------------------------------------*/
(define (classgen-struct-plain c)
   
   (define (classgen-class->struct slots)
      (let* ((len (+fx 1 (length slots)))
	     (id (type-id c))
	     (o (gensym 'o))
	     (to (make-typed-ident o id))
	     (r (gensym 'r))
	     (tr (make-typed-ident r 'struct))
	     (waccess (make-typed-ident 'with-access id)))
	 `(define-method (object->struct::struct ,to)
	     (let ((,tr (make-struct ',id ,len #unspecified)))
		(struct-set! ,r 0 #f)
		,@(map (lambda (s i)
			  (unless (slot-virtual? s)
			     `(,waccess ,o (,(slot-id s))
				 (struct-set! ,r ,i ,(slot-id s)))))
		     slots (iota len 1))
		,r))))
   
   (define (classgen-struct->class slots)
      (let* ((len (+fx 1 (length slots)))
	     (id (type-id c))
	     (o (gensym 'o))
	     (to (make-typed-ident o id))
	     (r (gensym 'r))
	     (tr (make-typed-ident r 'struct))
	     (waccess (make-typed-ident 'with-access id)))
	 `(define-method (struct+object->object::object ,to ,tr)
	     ,@(map (lambda (s i)
		       (unless (slot-virtual? s)
			  `(set! ,(field-access o (slot-id s))
			      (struct-ref ,r ,i))))
		  slots (iota len 1))
	     ,o)))
   
   (if *optim-object-serialization*
       (let ((slots (tclass-slots c)))
	  (list (classgen-class->struct slots) (classgen-struct->class slots)))
       '()))

;*---------------------------------------------------------------------*/
;*    classgen-struct-wide ...                                         */
;*---------------------------------------------------------------------*/
(define (classgen-struct-wide c)
   
   (define (classgen-class->struct slots)
      (let* (
	     (len (+fx 1 (length slots)))
	     (id (type-id c))
	     (o (gensym 'o))
	     (to (make-typed-ident o id))
	     (r (gensym 'r))
	     (tr (make-typed-ident r 'struct))
	     (plain (gensym 'plain))
	     (tplain (make-typed-ident plain 'struct))
	     (waccess (make-typed-ident 'with-access id)))
	 `(define-method (object->struct::struct ,to)
	     (let ((,tplain (call-next-method))
		   (,tr (make-struct ',id ,len #unspecified)))
		(struct-set! ,r 0 #f)
		,@(map (lambda (s i)
			  (unless (slot-virtual? s)
			     `(,waccess ,o (,(slot-id s))
				 (struct-set! ,r ,i ,(slot-id s)))))
		     slots (iota len 1))
		(struct-set! ,plain 0 ,r)
		;; we now swap the structures' keys
		(struct-key-set! ,r (struct-key ,plain))
		(struct-key-set! ,plain ',id)
		,plain))))
   
   (define (classgen-struct->class slots)
      (let* ((len (+fx 1 (length slots)))
	     (id (type-id c))
	     (super (tclass-its-super c))
	     (sid (type-id super))
	     (o (gensym 'o))
	     (to (make-typed-ident o id))
	     (r (gensym 'r))
	     (tr (make-typed-ident r 'struct))
	     (plain (gensym 'plain))
	     (tplain (make-typed-ident plain sid))
	     (aux (gensym 'aux))
	     (taux (make-typed-ident aux 'struct))
	     (widen (make-typed-ident 'widen! id)))
	 `(define-method (struct+object->object::object ,to ,tr)
	     (let* ((,plain (call-next-method))
		    (,taux (struct-ref ,r 0)))
		(,widen ,plain
		   ,@(map (lambda (s i)
			     (unless (slot-virtual? s)
				`(,(slot-id s) (struct-ref ,aux ,i))))
			slots (iota len 1)))
		,plain))))
   
   (let ((slots (filter (lambda (s)
			   (eq? (slot-class-owner s) c))
		   (tclass-slots c))))
      (if *optim-object-serialization*
	  (list (classgen-class->struct slots)
	     (classgen-struct->class slots))
	  '())))
