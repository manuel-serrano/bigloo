;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Eval/evobject.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan 14 17:11:54 2006                          */
;*    Last change :  Mon Jun  4 08:46:51 2018 (serrano)                */
;*    Copyright   :  2006-18 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Eval class definition                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evobject

   (include "Llib/object.sch")
   
   (import  __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __tvector
	    __bexit
	    __bignum
	    __os
	    __bit
	    __param
	    __object
	    __thread
	    __hash
	    __reader
	    
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
	    __evenv
	    __everror
	    __evcompile
	    __eval
	    __evmodule
	    __expander_define
	    __expand
	    __macro)
    
  (export (eval-class ::symbol ::bool ::pair-nil ::pair ::obj)
	  (eval-expand-instantiate ::class)
	  (eval-expand-duplicate ::class)
	  (eval-expand-with-access ::class)
	  (eval-co-instantiate-expander::pair-nil ::pair-nil ::procedure)))

;*---------------------------------------------------------------------*/
;*    expand-error ...                                                 */
;*---------------------------------------------------------------------*/
(define (expand-error p m x)
   (let ((loc (when (epair? x) (cer x))))
      (if (and (pair? loc) (pair? (cdr loc)) (pair? (cddr loc)))
	  (error/location p m x (cadr loc) (caddr loc))
	  (error p m x))))

;*---------------------------------------------------------------------*/
;*    slot                                                             */
;*---------------------------------------------------------------------*/
(define-struct slot
   id type read-only? default-value virtual-num getter setter user-info)

;*---------------------------------------------------------------------*/
;*    slot-virtual? ...                                                */
;*---------------------------------------------------------------------*/
(define (slot-virtual? slot)
   (slot-getter slot))

;*---------------------------------------------------------------------*/
;*    decompose-ident ...                                              */
;*---------------------------------------------------------------------*/
(define (decompose-ident id::symbol)
   (let* ((string (symbol->string! id))
	  (len (string-length string)))
      (let loop ((walker  0))
	 (cond
	    ((=fx walker len)
	     (values id #f))
	    ((and (char=? (string-ref string walker) #\:)
		  (<fx walker (-fx len 1))
		  (char=? (string-ref string (+fx walker 1)) #\:))
	     (values (string->symbol (substring string 0 walker))
		     (string->symbol (substring string (+fx walker 2) len))))
	    (else
	     (loop (+fx walker 1)))))))

;*---------------------------------------------------------------------*/
;*    localize ...                                                     */
;*---------------------------------------------------------------------*/
(define (localize loc p)
   (if (or (not loc) (not (epair? loc)))
       p
       (let loop ((p p))
	  (if (or (epair? p) (not (pair? p)))
	      p
	      (econs (loop (car p)) (loop (cdr p)) (cer loc))))))

;*---------------------------------------------------------------------*/
;*    eval-creator ...                                                 */
;*---------------------------------------------------------------------*/
(define (eval-creator id native len classnum)
   
   (define (make-create n)
      (let ((creator (class-creator native)))
	 (lambda l
	    (if (=fx (length l) (+fx n len))
		(let ((o (apply creator (take l n))))
		   (object-class-num-set! o (cell-ref classnum))
		   (%object-widening-set! o (list->vector (list-tail l n)))
		   o)
		(expand-error id
		   (format
		      "Wrong number of arguments for constructor (expecting ~a)"
		      (+fx n len))
		   l)))))
   
   (let ((fields (class-all-fields native)))
      (let loop ((i (-fx (vector-length fields) 1))
		 (n 0))
	 (cond
	    ((=fx i -1)
	     (make-create n))
	    ((class-field-virtual? (vector-ref fields i))
	     (loop (-fx i 1) n))
	    (else
	     (loop (-fx i 1) (+fx n 1)))))))
		
;*---------------------------------------------------------------------*/
;*    eval-allocator ...                                               */
;*---------------------------------------------------------------------*/
(define (eval-allocator native length classnum)
   (let ((alloc (class-allocator native)))
      (lambda ()
	 (let ((o (alloc)))
	    (object-class-num-set! o (cell-ref classnum))
	    (%object-widening-set! o (make-vector length))
	    o))))

;*---------------------------------------------------------------------*/
;*    eval-nil ...                                                     */
;*---------------------------------------------------------------------*/
(define (eval-nil native length classnum)
   (lambda (o)
      (object-class-num-set! o (cell-ref classnum))
      (%object-widening-set! o (make-vector length #f))
      o))

;*---------------------------------------------------------------------*/
;*    patch-field-default-values! ...                                  */
;*---------------------------------------------------------------------*/
(define (patch-field-default-values! slots fields module)
   ;; the default values compilation has been postponned after the class
   ;; is properly defined. this function compiles these values.
   (for-each (lambda (field slot)
		;; the index is extract from the function
		;; (@ CLASS-FIELD-DEFAULT-VALUE __OBJECT)
		(vector-set-ur! field 6
		   (eval! `(lambda ()
			      ,(slot-default-value slot))
		      module)))
      (vector->list fields) slots))

;*---------------------------------------------------------------------*/
;*    patch-vfield-accessors! ...                                      */
;*---------------------------------------------------------------------*/
(define (patch-vfield-accessors! slots fields virtuals module)
   ;; virtual getters and setters compilation have been postponned after
   ;; the class is properly defined. this function compiles these functions.
   (for-each (lambda (field)
		(let* ((id (class-field-name field))
		       (slot (find (lambda (s) (eq? (slot-id s) id)) slots)))
		   ;; the indexes are extract from the function
		   ;; (@ MAKE-CLASS-FIELD __OBJECT)
		   (when (slot-virtual? slot)
		      (slot-getter-set! slot (eval! (slot-getter slot) module))
		      (slot-setter-set! slot (eval! (slot-setter slot) module))
		      (vector-set-ur! field 1 (slot-getter slot))
		      (vector-set-ur! field 2 (slot-setter slot))
		      (let* ((num (slot-virtual-num slot))
			     (vfield (vector-ref virtuals num)))
			 (set-car! vfield (slot-getter slot))
			 (set-cdr! vfield (slot-setter slot))))))
      (vector->list fields)))

;*---------------------------------------------------------------------*/
;*    eval-register-class ...                                          */
;*---------------------------------------------------------------------*/
(define (eval-register-class id module super abstract slots hash constr)
   (let* ((size (length (filter (lambda (s) (not (slot-virtual? s))) slots)))
	  (offset (if (eval-class? super) (class-evdata super) 0))
	  (native (let loop ((super super))
		     (cond
			((eval-class? super) (loop (class-super super)))
			((eq? super object) object)
			((class-abstract? super) (loop (class-super super)))
			(else super))))
	  (length (+fx offset size))
	  (classnum (make-cell -1))
	  (clazz (register-class!
		    ;; name
		    id
		    ;; module name
		    (if (evmodule? module)
			(evmodule-name module)
			'||)
		    ;; super class
		    super
		    ;; hash number
		    hash
		    ;; creator
		    (eval-creator id native length classnum)
		    ;; allocate
		    (eval-allocator native length classnum)
		    ;; constructor
		    (or constr (find-class-constructor super))
		    ;; nil
		    (eval-nil native length classnum)
		    ;; shrink
		    #f
		    ;; fields are set after the class is created
		    '#()
		    ;; virtual fields
		    (make-class-virtual-fields slots))))
      (cell-set! classnum (class-num clazz))
      (class-evdata-set! clazz length)
      ;; once we have created the class, set the fields
      (let ((fields (make-class-fields id clazz slots size offset)))
	 (class-evfields-set! clazz fields)
	 ;; now the class is properly defined, it is possible to evaluate
	 ;; the default values and virtual getters/setters
	 '(patch-field-default-values! slots fields)
	 '(patch-vfield-accessors! slots fields (class-virtual clazz)))
      clazz))

;*---------------------------------------------------------------------*/
;*    find-class-constructor ...                                       */
;*---------------------------------------------------------------------*/
(define (find-class-constructor c)
   (let ((const (class-constructor c)))
      (if const
	  const
	  (let ((s (class-super c)))
	     (if (class? s)
		 (find-class-constructor s)
		 #f)))))

;*---------------------------------------------------------------------*/
;*    classgen-slot-anonymous ...                                      */
;*    -------------------------------------------------------------    */
;*    Kept in a separate function for the sake of the symmetry with    */
;*    the compiler code.                                               */
;*---------------------------------------------------------------------*/
(define (classgen-slot-anonymous index s class)
   (list
      (lambda (o)
	 (if (isa? o class)
	     (vector-ref-ur (%object-widening o) index)
	     (bigloo-type-error (slot-id s) (class-name class) o)))
      (lambda (o v)
	 (if (isa? o class)
	     (vector-set-ur! (%object-widening o) index v)
	     (bigloo-type-error (slot-id s) (class-name class) o)))))

;*---------------------------------------------------------------------*/
;*    make-class-fields ...                                            */
;*---------------------------------------------------------------------*/
(define (make-class-fields id class slots size offset)
   
   (define (make-class-field-virtual s)
      ((@ make-class-field __object)
       (slot-id s)
       (slot-getter s) (slot-setter s) (slot-read-only? s)
       #t
       (eval! (slot-user-info s))
       (slot-default-value s)
       (slot-type s)))
   
   (define (make-class-field-plain s i class)
      (let ((defs (classgen-slot-anonymous i s class)))
	 ((@ make-class-field __object)
	  (slot-id s)
	  (car defs) (cadr defs) (slot-read-only? s)
	  #f
	  (eval! (slot-user-info s))
	  (slot-default-value s)
	  (slot-type s))))

   (list->vector
      (append
	 ;; cannot combine map and filter otherwise the slot list
	 ;; and the iota list dont have the same length
	 (map (lambda (slot index)
		 (make-class-field-plain slot index class))
	    (filter (lambda (slot) (not (slot-virtual? slot))) slots)
	    (iota size offset))
	 (filter-map (lambda (slot)
			(when (slot-virtual? slot)
			   (make-class-field-virtual slot)))
	    slots))))

;*---------------------------------------------------------------------*/
;*    make-class-virtual-fields ...                                    */
;*---------------------------------------------------------------------*/
(define (make-class-virtual-fields slots)
   (list->vector
      (filter-map (lambda (slot)
		     (when (slot-virtual? slot)
			(cons (slot-virtual-num slot)
			   (cons (slot-getter slot) (slot-setter slot)))))
	 slots)))

;*---------------------------------------------------------------------*/
;*    eval-expand-instantiate ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-expand-instantiate class)
   (let ((wid (symbol-append 'instantiate:: (class-name class))))
      (install-expander wid
	 (eval-instantiate-expander class))))

;*--------------------------------------------------------------------*/
;*    eval-instantiate-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (eval-instantiate-expander class)
   (lambda (x e)
      (instantiate-fill (car x) (cdr x) class
	 (class-all-fields class)
	 (localize x `(,(class-allocator class))) x e)))

;*---------------------------------------------------------------------*/
;*    instantiate-fill ...                                             */
;*---------------------------------------------------------------------*/
(define (instantiate-fill op provided class fields::vector init x e)

   (define (collect-field-values fields)
      (let* ((len (vector-length fields))
	     (vargs (make-vector len)))
	 ;; collect the default values
	 (let loop ((i 0))
	    (when (<fx i len)
	       (let ((s (vector-ref fields i)))
		  (cond
		     ((class-field-default-value? s)
		      (vector-set! vargs
			 i (cons #t
			      `((@ class-field-default-value __object) ,s))))
		     (else
		      (vector-set! vargs
			 i (cons #f #unspecified))))
		  (loop (+fx i 1)))))
	 ;; collect the provided values
	 (let loop ((provided provided))
	    (when (pair? provided)
	       (let ((p (car provided)))
		  (match-case p
		     (((and (? symbol?) ?s-name) ?value)
		      ;; plain field
		      (let ((pval (vector-ref
				     vargs
				     (find-field-offset fields s-name op p))))
			 (set-car! pval #t)
			 (set-cdr! pval (localize p value))))
		     (else
		      (expand-error op "Illegal argument" p)))
		  (loop (cdr provided)))))
	 ;; build the result
	 vargs))

   (define (check-all-values args fields)
      (let ((len (vector-length fields)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (let ((a (vector-ref args i))
		     (s (vector-ref fields i)))
		  (unless (car a)
		     (unless (class-field-virtual? s)
			;; value missing
			(expand-error op
			   (format "Missing value for field \"~a\""
			      (class-field-name s))
			   x))))
	       (loop (+fx i 1))))))
   
   (let* ((new (gensym 'new))
	  (args (collect-field-values fields)))
      ;; check that there is enough values
      (check-all-values args fields)
      ;; allocate the object and set the fields,
      ;; first the actual fields, second the virtual fields
      (localize x
	 `(let ((,new ,(e init e)))
	     (begin
		;; actual fields
		,@(vector-filter-map
		     (lambda (field val)
			(unless (class-field-virtual? field)
			   (let ((v (e (cdr val) e)))
			      (localize val
				 `(,(class-field-mutator field) ,new ,v)))))
		     fields args)
		;; constructor
		,(let ((constr (find-class-constructor class)))
		    (when constr
		       (e (localize x `(,constr ,new)) e)))
		;; virtual fields
		,@(vector-filter-map
		     (lambda (field val)
			(when (and (class-field-virtual? field)
				   (class-field-mutable? field)
				   (car val))
			   (let ((v (e (cdr val) e)))
			      (localize val
				 `(,(class-field-mutator field) ,new ,v)))))
		     fields args)
		;; return the new instance
		,new)))))

;*---------------------------------------------------------------------*/
;*    vector-filter-map ...                                            */
;*---------------------------------------------------------------------*/
(define (vector-filter-map f v1 v2)
   (let ((len (vector-length v1)))
      (let loop ((i 0)
		 (acc '()))
	 (if (=fx i len)
	     (reverse! acc)
	     (let ((v (f (vector-ref v1 i) (vector-ref v2 i))))
		(loop (+fx i 1) (if v (cons v acc) acc)))))))

;*---------------------------------------------------------------------*/
;*    eval-expand-duplicate ...                                        */
;*---------------------------------------------------------------------*/
(define (eval-expand-duplicate class)
   (let ((did (symbol-append 'duplicate:: (class-name class))))
      (install-expander did
	 (eval-duplicate-expander class))))

;*---------------------------------------------------------------------*/
;*    eval-duplicate-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-duplicate-expander class)
   (lambda (x e)
      (match-case x
	 ((?duplicate ?dup . ?prov)
	  (duplicate-expander class dup prov x e))
	 (else
	  (expand-error "duplicate" "Illegal form" x)))))

;*---------------------------------------------------------------------*/
;*    duplicate-expander ...                                           */
;*---------------------------------------------------------------------*/
(define (duplicate-expander class duplicated provided x e)
   
   (define (collect-field-values fields dupvar)
      (let* ((len (vector-length fields))
	     (vargs (make-vector len)))
	 ;; we collect the provided values
	 (let loop ((provided provided))
	    (when (pair? provided)
	       (let ((p (car provided)))
		  (match-case p
		     (((and (? symbol?) ?s-name) ?value)
		      ;; plain field
		      (let ((i (find-field-offset fields s-name "duplicate" p)))
			 (vector-set! vargs
			    i
			    (cons #t (localize p value)))))
		     (else
		      (expand-error (car x) "Illegal form" x)))
		  (loop (cdr provided)))))
	 ;; we collect the duplicated values
	 (let loop ((i 0))
	    (when (<fx i len)
	       (let ((value (vector-ref vargs i)))
		  (unless (pair? value)
		     (let ((field (vector-ref fields i)))
			;; no value is provided for this object we pick
			;; one from this duplicated object.
			(vector-set! vargs
			   i
			   (cons #t `(,(class-field-accessor field) ,dupvar)))))
		  (loop (+fx i 1)))))
	 ;; build the result
	 vargs))

   (let* ((fields (class-all-fields class))
	  (new (gensym 'new))
	  (dupvar (gensym 'duplicated))
	  (args (collect-field-values fields dupvar)))
      ;; allocate the object and set the fields,
      ;; first the actual fields, second the virtual fields
      `(let ((,dupvar ,(e duplicated e))
	     (,new ,(e `(,(class-allocator class)) e)))
	  (begin
	     ;; actual fields
	     ,@(vector-filter-map
		  (lambda (field val)
		     (unless (class-field-virtual? field)
			(let ((v (e (cdr val) e)))
			   `(,(class-field-mutator field) ,new ,v))))
		  fields args)
	     ;; constructor
	     ,(let ((constr (find-class-constructor class)))
		 (when constr
		    (e `(,constr ,new) e)))
	     ;; virtual fields
	     ,@(vector-filter-map
		  (lambda (field val)
		     (when (and (class-field-virtual? field)
				(class-field-mutable? field))
			(let ((v (e (cdr val) e)))
			   `(,(class-field-mutator field) ,new ,v))))
		  fields args)
	     ;; return the new instance
	     ,new))))

;*---------------------------------------------------------------------*/
;*    find-field-offset ...                                            */
;*---------------------------------------------------------------------*/
(define (find-field-offset fields::vector name::symbol form sexp)
   (let ((len (vector-length fields)))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     (expand-error name
		(format "Illegal ~a, field unknown \"~a\"" form name)
		sexp))
	    ((eq? (class-field-name (vector-ref fields i)) name)
	     i)
	    (else   
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    eval-expand-with-access ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-expand-with-access class)
   (let ((wid (symbol-append 'with-access:: (class-name class))))
      (install-expander wid (eval-with-access-expander class))))

;*---------------------------------------------------------------------*/
;*    eval-with-access-expander ...                                    */
;*---------------------------------------------------------------------*/
(define (eval-with-access-expander class)

   (lambda (x e)
      (match-case x
	 ((?waccess ?instance (and (? pair?) ?fields) . (and (? pair?) ?body))
	  (let loop ((s fields)
		     (nfields '()))
	     (cond
		((null? s)
		 (let* ((instance (e instance e))
			(aux (gensym 'i))
			(taux (symbol-append aux '|::| (class-name class))))
		    (localize
		       x
		       `(let ((,taux ,instance))
			   ,(%with-lexical
			       (map car nfields)
			       (expand-progn body)
			       (eval-begin-expander
				  (with-access-expander
				     e aux nfields x))
			       aux)))))
		((not (pair? s))
		 (expand-error s "Illegal field" x))
		((symbol? (car s))
		 (loop (cdr s) (cons (list (car s) (car s)) nfields)))
		((and (pair? (car s))
		      (symbol? (car (car s)))
		      (pair? (cdr (car s)))
		      (symbol? (cadr (car s)))
		      (null? (cddr (car s))))
		 (loop (cdr s) (cons (car s) nfields)))
		(else
		 (expand-error (car s) "Illegal form" x)))))
	 (else
	  (expand-error "with-access" "Illegal with-access" x)))))

;*---------------------------------------------------------------------*/
;*    with-access-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (with-access-expander olde i fields form)
   
   (define (id var) (cadr (assq var fields)))
   
   (let ((ids (map car fields)))
      (lambda (x e)
	 (match-case x
	    ((and ?var (? symbol?))
	     (if (and (memq var ids)
		      (let ((cell (assq var (%lexical-stack))))
			 (and (pair? cell) (eq? (cdr cell) i))))
		 `(-> ,(olde i olde) ,(id var))
		 (olde var olde)))
	    ((set! (and (? symbol?) ?var) ?val)
	     (let ((val (e val e)))
		(if (and (memq var ids)
			 (let ((cell (assq var (%lexical-stack))))
			    (and (pair? cell) (eq? (cdr cell) i))))
		    `(set! (-> ,(olde i olde) ,(id var)) ,(olde val olde))
		    (localize x (olde `(set! ,(cadr x) ,val) olde)))))
	    (else
	     (olde x e))))))

;*---------------------------------------------------------------------*/
;*    eval-parse-class-slot ...                                        */
;*---------------------------------------------------------------------*/
(define (eval-parse-class-slot loc f)
   (cond
      ((symbol? f)
       (multiple-value-bind (id type)
	  (decompose-ident f)
	  (list (slot id (if type (or (class-exists type) type) 'obj)
		   #f #f 0 #f #f #f))))
      ((not (and (list? f) (symbol? (car f))))
       (evcompile-error (or (get-source-location f) loc)
	  "eval" "Illegal slot declaration" f))
      (else
       (let ((id (car f))
	     (attrs (cdr f)))
	  (multiple-value-bind (id type)
	     (decompose-ident id)
	     (let ((def #f)
		   (get #f)
		   (set #f)
		   (info #f)
		   (ronly #f))
		(for-each (lambda (attr)
			     (cond
				((eq? attr 'read-only)
				 (set! ronly #t))
				(else
				 (match-case attr
				    ((info ?value)
				     (set! info value))
				    ((get ?expr)
				     (if (symbol? expr)
					 (let ((o (gensym)))
					    (set! get `(lambda (,o)
							  (,expr ,o))))
					 (set! get expr)))
				    ((set ?expr)
				     (if (symbol? expr)
					 (let ((o (gensym))
					       (v (gensym)))
					    (set! set `(lambda (,o ,v)
							  (,expr ,o ,v))))
					 (set! set expr)))
				    ((default ?expr)
				     (set! def expr))
				    (else
				     (evcompile-error
					(or (get-source-location f) loc)
					"eval" "Illegal slot declaration" f))))))
		   attrs)
		(cond
		   ((and get (not ronly) (not set))
		    (evcompile-error
		       (or (get-source-location f) loc)
		       "eval" "Missing virtual set" f))
		   ((and set (not get))
		    (evcompile-error
		       (or (get-source-location f) loc)
		       "eval" "Missing virtual get" f))
		   (else
		    (let ((s (slot id
				(if type (or (class-exists type) type) 'obj)
				ronly def 0 get set info)))
		       (list s))))))))))

;*---------------------------------------------------------------------*/
;*    eval-parse-class ...                                             */
;*    -------------------------------------------------------------    */
;*    Parse the class clauses, returning the constructor and           */
;*    the new slots.                                                   */
;*---------------------------------------------------------------------*/
(define (eval-parse-class loc clauses)
   (let ((loc (or (get-source-location clauses) loc)))
      (cond
	 ((null? clauses)
	  (values #f '()))
	 ((not (list? clauses))
	  (evcompile-error (or (get-source-location clauses) loc)
	     "eval" "Illegal class declaration" clauses))
	 ((match-case (car clauses) (((? symbol?)) #t) (else #f))
	  ;; the constructor must be protected under a lambda because
	  ;; may be still uninitialized
	  (values `(lambda (o) (,(caar clauses) o))
		  (append-map (lambda (f)
				 (eval-parse-class-slot loc f))
			      (cdr clauses))))
	 ((match-case (car clauses) (((lambda . ?-)) #t) (else #f))
	  ;; the constructor must be protected under a lambda because
	  ;; may be still uninitialized
	  (values `(lambda (o) (,(caar clauses) o))
		  (append-map (lambda (f)
				 (eval-parse-class-slot loc f))
			      (cdr clauses))))
	 (else
	  (values #f
		  (append-map (lambda (f)
				 (eval-parse-class-slot loc f))
			      clauses))))))

;*---------------------------------------------------------------------*/
;*    eval-class ...                                                   */
;*---------------------------------------------------------------------*/
(define (eval-class id abstract clauses src mod)
   (multiple-value-bind (cid sid)
      (decompose-ident id)
      (let* ((loc (get-source-location src))
	     (sid (or sid 'object))
	     (super (find-class sid)))
	 (if (not (class? super))
	     (evcompile-error loc "eval" "Cannot find super class" sid)
	     (multiple-value-bind (constructor slots)
		(eval-parse-class loc clauses)
		;; make the class and bind it to its global variable
		(let* ((clazz (eval-register-class
				 cid mod super abstract
				 slots 
				 (get-class-hash src)
				 (eval! constructor mod))))
		   (eval! `(define ,cid ,clazz))
		   ;; with-access
		   (eval-expand-with-access clazz)
		   ;; now the class is properly defined, it is possible to
		   ;; evaluate the default values and virtual getters/setters
		   (let ((fields (class-fields clazz)))
		      (patch-field-default-values!
			 slots fields mod)
		      (patch-vfield-accessors!
			 slots fields (class-virtual clazz) mod))
		   ;; instantiate and duplicate expanders
		   (unless abstract
		      (eval-expand-instantiate clazz)
		      (eval-expand-duplicate clazz))
		   (list cid)))))))

;*---------------------------------------------------------------------*/
;*    eval-co-instantiate-expander ...                                 */
;*---------------------------------------------------------------------*/
(define (eval-co-instantiate-expander x e)
   (match-case x
      ((co-instantiate ?bindings . ?body)
       (co-instantiate->let bindings body x e))
      (else
       (expand-error "co-instantiate" "Illegal `co-instantiate' form" x))))

;*---------------------------------------------------------------------*/
;*    eval-expand-co-instantiate ...                                   */
;*---------------------------------------------------------------------*/
(define (eval-expand-co-instantiate x e)
   (eval-co-instantiate-expander x e))

;*---------------------------------------------------------------------*/
;*    co-instantiate->let ...                                          */
;*---------------------------------------------------------------------*/
(define (co-instantiate->let bindings body x e)

   (define (parse-id id)
      (multiple-value-bind (id type)
	 (decompose-ident id)
	 (cons id type)))
   
   (define (find-instantiate-class expr bdg)
      (match-case expr
	 ((?instantiate . ?body)
	  (let* ((id-type (parse-id instantiate))
		 (kclass (find-class (cdr id-type))))
	     (cond
		((not (eq? (car id-type) 'instantiate))
		 (expand-error instantiate "Illegal binding" bdg))
		((not (class? kclass))
		 (expand-error instantiate "Illegal class" bdg))
		((class-abstract? kclass)
		 (expand-error instantiate
		    "Abstract classes can't be instantiated"
		    bdg))
		(else
		 kclass))))
	 (else
	  (expand-error "co-instantiate" "Illegal binding" bdg))))
   
   (let ((vars (map (lambda (bdg)
		       (match-case bdg
			  (((and ?var (? symbol?)) ?expr)
			   (let* ((id-type (parse-id var))
				  (id (car id-type))
				  (t (cdr id-type))
				  (klass (find-instantiate-class expr bdg)))
			      (if (or (not t) (eq? t (class-name klass)))
				  (list id klass expr)
				  (expand-error (car x) "Illegal variable type"
				     bdg))))
			  (else
			   (expand-error (car x) "Illegal binding" bdg))))
		  bindings)))
      `(let ,(map (lambda (var)
		     (let ((id (car var))
			   (klass (cadr var)))
			`(,(symbol-append id '|::| (class-name klass))
			  (,(class-allocator klass)))))
		vars)
	  (begin
	     ,@(map (lambda (var)
		       (let ((id (car var))
			     (klass (cadr var))
			     (expr (caddr var)))
			  (instantiate-fill (car expr) (cdr expr)
			     klass (class-all-fields klass) id expr e)))
		  vars)
	     ,(e `(begin ,@body) e)))))
