;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tvector/access.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 27 13:33:40 1995                          */
;*    Last change :  Thu Nov 10 07:09:46 2011 (serrano)                */
;*    Copyright   :  1995-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We install all the coercer and accessor for `tvector' types.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tvector_access
   (import  tools_misc
	    type_type
	    type_env
	    engine_param
	    backend_backend
	    tvector_tvector
	    module_module
	    tools_error
	    tools_shape
	    ast_ident
	    ast_private)
   (export  (make-tvector-accesses tvector::tvec ::obj ::bool)))

;*---------------------------------------------------------------------*/
;*    make-tvector-accesses ...                                        */
;*---------------------------------------------------------------------*/
(define (make-tvector-accesses tv::tvec src import)
   (let* ((tv-id         (tvec-id tv))
	  (tv-name       (tvec-name tv))
	  (obj           (find-type 'obj))
	  (item-type     (tvec-item-type tv))
	  (item-id       (type-id item-type))
	  (item-name     (type-name item-type))
	  (mitem-name    (if (string? item-name)
			     (bigloo-mangle item-name)
			     "obj_t"))
	  (descr-id      (symbol-append tv-id '-descriptor))
	  (tv-make-id    (symbol-append 'make- tv-id))
	  (tv-alloc-id   (symbol-append 'allocate- tv-id))
	  (tv-ref-id     (symbol-append tv-id '-ref))
	  (tv-set!-id    (symbol-append tv-id '-set!))
	  (tv?-id        (symbol-append tv-id '?))
	  (tv->vector-id (symbol-append tv-id '->vector))
	  (vector->tv-id (symbol-append 'vector-> tv-id))
	  (tv->list      (symbol-append tv-id '->list))
	  (tv-length-id  (symbol-append tv-id '-length)))
      
      (define (make-descr)
	 (if import
	     `(define ,(symbol-append descr-id '::obj)
		 ((@ get-tvector-descriptor __tvector) ',tv-id))
	     `(define ,(symbol-append descr-id '::obj)
		 ((@ declare-tvector! __tvector)
		  ,(symbol->string tv-id)
		  ,tv-alloc-id
		  ,tv-ref-id
		  ,tv-set!-id))))
      
      (define (make-c-tv?)
	 `(define-inline (,(symbol-append tv?-id '::bool) o::obj)
	     (if (tvector? o)
		 (eq? ($tvector-descr o) ,descr-id)
		 #f)))

      (define (make-jvm-tv?)
	 `(define-inline (,(symbol-append tv?-id '::bool) o::obj)
	     ,(make-private-sexp 'instanceof tv-id 'o)))
       
      (define (make-tv?)
	 (if (backend-tvector-descr-support (the-backend))
	     (make-c-tv?)
	     (make-jvm-tv?)))
      
      (define (make-tv-ref)
	 (let* ((pfmt (string-append "TVECTOR_REF( " mitem-name ",$1,$2 )"))
		(exp (make-private-sexp 'vref tv-id item-id 'int
					pfmt 'tv 'o)))
	    `(define-inline (,(make-typed-ident tv-ref-id item-id)
			     ,(make-typed-ident 'tv tv-id)
			     o::int)
		,exp)))
      
      (define (make-tv-set!)
	 (let* ((pfmt (string-append "TVECTOR_SET( " mitem-name ",$1,$2,$3 )"))
		(exp (make-private-sexp 'vset! tv-id item-id 'int
					pfmt 'tv 'o 'v)))
	    `(define-inline (,(symbol-append tv-set!-id '::obj)
			     ,(make-typed-ident 'tv tv-id)
			     o::int
			     ,(make-typed-ident 'v item-id))
		,exp)))
      
      (define (make-tv)
	 `(define-inline (,(make-typed-ident tv-make-id tv-id)
			  len::int
			  ,(make-typed-ident 'v item-id))
	     (let ((,(make-typed-ident 'tv tv-id) (,tv-alloc-id len)))
		(labels ((,(make-typed-ident 'loop tv-id) (i::int)
				(if (=fx i len)
				    tv
				    (let ((ni::int (+fx i 1)))
				       (,tv-set!-id tv i v)
				       (loop ni)))))
		   (loop 0)))))
      
      (define (allocate-tvector)
	 (if (or (string=? item-name "double") (string=? item-name "float"))
	     "ALLOCATE_ATOMIC_TVECTOR( "
	     "ALLOCATE_TVECTOR( "))
      
      (define (make-c-alloc-tv)
	 `(define-inline (,(make-typed-ident tv-alloc-id tv-id) len::int)
	     ,(make-private-sexp 'valloc tv-id item-id 'int
				 (string-append (allocate-tvector)
						mitem-name ", "
						item-name
						", $1, $2 )")
				 (string-append "ALLOCATE_S_TVECTOR( "
						mitem-name ", "
						item-name
						", $1, $2 )")
				 #f
				 'len descr-id)))
      
      (define (make-jvm-alloc-tv)
	 `(define-inline (,(make-typed-ident tv-alloc-id tv-id) len::int)
	     (let ((,(make-typed-ident 'v tv-id)
		    ,(make-private-sexp 'valloc tv-id item-id 'int
					  (string-append (allocate-tvector)
							 mitem-name ", "
							 item-name
							 ", $1, $2 )")
					  (string-append "ALLOCATE_S_TVECTOR( "
							 mitem-name ", "
							 item-name
							 ", $1, $2 )")
					  #f
					  'len)))
		($tvector-descr-set! v ,descr-id)
		v)))

      (define (make-alloc-tv)
	 (if (backend-tvector-descr-support (the-backend))
	     (make-c-alloc-tv)
	     (make-jvm-alloc-tv)))
	     
      (define (make-tv->vector)
	 `(define-inline (,(symbol-append tv->vector-id '::vector)
			  ,(make-typed-ident 'tv tv-id))
	     (tvector->vector tv)))

      (define (make-tv->list)
	 `(define (,(symbol-append tv->list '::obj)
		   ,(make-typed-ident 'tv tv-id))
	     (let ((len::int (,tv-length-id tv)))
		(if (=fx len 0)
		    '()
		    (labels ((loop::pair (i::int acc::obj)
				   (if (=fx i 0)
				       (cons (,tv-ref-id tv i) acc)
				       (loop (-fx i 1)
					     (cons (,tv-ref-id tv i) acc)))))
		       (loop (-fx len 1) '()))))))
      
      (define (make-vector->tv)
	 `(define-inline (,(make-typed-ident vector->tv-id tv-id) v::vector)
	     (vector->tvector ',tv-id v)))
      
      (define (make-tv-length)
	 `(define-inline (,(symbol-append tv-length-id '::int)
			  ,(make-typed-ident 'o tv-id))
	     ,(make-private-sexp 'vlength tv-id item-id 'int
				 "TVECTOR_LENGTH( $1 )" 'o)))

      ;; we parse a pragma clause for predicate and allocator and accessors
      (produce-module-clause!
       `(static ;; tv?
	        (inline ,(symbol-append tv?-id '::bool)
			::obj)
		;; tv-ref
		(inline ,(make-typed-ident tv-ref-id item-id)
			,(make-typed-ident 'tv tv-id)
			::int)
		;; tv-set!
		(inline ,(symbol-append tv-set!-id '::obj)
			,(make-typed-ident 'tv tv-id)
			::int
			,(make-typed-ident 'v item-id))
		;; make-tv
		(inline ,(make-typed-ident tv-make-id tv-id)
			::int
			,(make-typed-formal item-id))
		;; alloc-tv
		(inline ,(make-typed-ident tv-alloc-id tv-id)
			::int)
		;; tv->vector
		(inline ,(symbol-append tv->vector-id '::vector)
			,(make-typed-ident 'tv tv-id))
		;; vector->tv
		(inline ,(make-typed-ident vector->tv-id tv-id) 
			::vector)
		;; tv-length
		(inline ,(symbol-append tv-length-id '::int)
			,(make-typed-ident 'o tv-id))
		;; tv->list
		(,(symbol-append tv->list '::obj)
		 ,(make-typed-ident 'tv tv-id))))
      
      (produce-module-clause!
       `(pragma (,tv?-id (predicate-of ,tv-id))))

      ;; we check that the item type is correctly defined
      (if (not (string? item-name))
	  (user-error "tvector"
		      (string-append "Undefined type `"
				     (symbol->string item-id)
				     "'")
		      src))
      
      ;; and we return the definitions
      (list (make-descr)
	    (make-tv?)
	    (make-tv-ref)
	    (make-tv-set!)
	    (make-tv)
	    (make-alloc-tv)
	    (make-tv->vector)
	    (make-vector->tv)
	    (make-tv-length)
	    (make-tv->list))))
