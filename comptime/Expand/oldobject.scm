;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Expand/oldobject.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  3 10:13:58 1996                          */
;*    Last change :  Wed Nov  9 15:46:25 2011 (serrano)                */
;*    Copyright   :  1996-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The Object expanders                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module expand_oldobject
   (import  tools_args
	    tools_misc
	    tools_location
	    type_type
	    type_env
	    type_cache
	    expand_eps
	    engine_param
	    ast_var
	    ast_ident
	    ast_private
	    ast_object
	    ast_node
	    object_class
	    object_slots
	    object_tools
	    object_classgen
	    expand_lambda
	    module_prototype
	    module_module
	    module_class)
   (export  (instantiate->fill-accessors form class alloc e)
	    (co-instantiate->let-accessors bindings body x e)
	    (duplicate->make-accessors form class duplicated provided e)
	    (make-widening-accessors form class obj provided e)))

;*---------------------------------------------------------------------*/
;*    instantiate->fill-accessors ...                                  */
;*---------------------------------------------------------------------*/
(define (instantiate->fill-accessors form class alloc e)
   (let* ((slots    (tclass-all-slots class))
	  (len      (length slots))
	  (vargs    (make-vector (length slots)))
	  (new      (gensym 'new))
	  (provided (cdr form)))    
      ;; we collect the default values
      (let loop ((i 0)
		 (slots slots))
	 (if (null? slots)
	     'done
	     (let ((s (car slots)))
		(cond
		   ((slot-default? s)
		    (vector-set! vargs i (cons #t (slot-default-value s))))
		   (else
		    (vector-set! vargs i (cons #f #unspecified))))
		(loop (+fx i 1) (cdr slots)))))
      ;; we collect the provided values
      (let loop ((provided provided))
	 (if (null? provided)
	     'done
	     (let ((p (car provided)))
		(match-case p
		   (((and (? symbol?) ?s-name) ?value)
		    ;; plain slot
		    (let ((pval (vector-ref
				   vargs
				   (find-slot-offset
				      slots s-name "instantiate" p))))
		       (set-car! pval #t)
		       (set-cdr! pval (object-epairify value p))))
		   (else
		    (error #f "Illegal instantiate" form)))
		(loop (cdr provided)))))
      ;; we check that we have a value for all formals
      (let loop ((i 0)
		 (s slots))
	 (cond
	    ((=fx i len)
	     'ok)
	    ((and (not (car (vector-ref vargs i)))
		  (not (slot-virtual? (car s))))
	     ;; no, this is not correct, at least one argument is missing
	     (error #f
		    (format 
		       "Illegal instantiate, missing value for field \"~a\""
		     (slot-id (car s)))
		    form))
	    (else
	     (loop (+fx i 1) (cdr s)))))
      ;; we just have now to build the make call
      (let loop ((i        0)
		 (slots    slots)
		 (largs    '())
		 (virtuals '()))
	 (if (=fx i len)
	     (if (null? virtuals)
		 (alloc (reverse! largs))
		 `(let ((,new ,(alloc (reverse! largs))))
		     ,@(reverse! virtuals)
		     ,new))
	     (let ((value (e (cdr (vector-ref vargs i)) e)))
		(cond
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
			  (cdr slots)
			  largs
			  (if (eq? value #unspecified)
			      virtuals
			      (if (epair? value)
				  (econs (make-virtual-set
					    (car slots) value new
					    class form e "instantiate")
					 virtuals
					 (cer value))
				  (cons (make-virtual-set
					   (car slots) value new
					   class form e "instantiate")
					virtuals)))))
		   (else
		    (loop (+fx i 1)
			  (cdr slots)
			  (if (epair? value)
			      (econs value largs (cer value))
			      (cons value largs))
			  virtuals))))))))

;*---------------------------------------------------------------------*/
;*    co-instantiate->let-accessors ...                                */
;*---------------------------------------------------------------------*/
(define (co-instantiate->let-accessors bindings body x e)
   (define (find-instantiate-class expr bdg loc)
      (match-case expr
	 ((?instantiate . ?body)
	  (let* ((loc (find-location/loc body loc))
		 (id-type (parse-id instantiate loc))
		 (kclass (cdr id-type)))
	     (cond
		((not (eq? (car id-type) 'instantiate))
		 (error #f
		    "co-instantiate:Illegal binding"
		    bdg))
		((not (tclass? kclass))
		 (error #f
		    "co-instantiate:Illegal class"
		    bdg))
		((tclass-abstract? kclass)
		 (error #f
		    "co-instantiate:Abstract classes can't be instantiated"
		    bdg))
		(else
		 kclass))))
	 (else
	  (error #f
	     "co-instantiate:Illegal binding"
	     bdg))))
   (let ((loc (find-location x)))
      (let loop ((bindings bindings)
		 (user-variables '())
		 (private-variables '())
		 (rewrite-bindings '())
		 (classes '())
		 (instantiates '()))
	 (if (null? bindings)
	     (letrec ((e1 (lambda (x ne)
			     (if (symbol? x)
				 (let ((cell (assq x rewrite-bindings)))
				    (if (pair? cell)
					(cdr cell)
					x))
				 (e x ne)))))
		`(let ,(map (lambda (id class)
			       (let* ((cid (tclass-id class))
				      (tv (make-typed-ident id cid)))
				  `(,tv (,(symbol-append '%allocate- cid)))))
			  private-variables
			  classes)
		    (let ,(reverse!
			     (map (lambda (user private class instantiate)
				     (let* ((cid (tclass-id class))
					    (fill (symbol-append 'fill- cid '!))
					    (constrs (find-class-constructors class))
					    (alloc (lambda (args)
						      `(begin
							  (,fill ,private ,@args)
							  ,@(map (lambda (c)
								    `(,c ,private))
							       constrs)
							  ,private))))
					`(,user ,(instantiate->fill-accessors
						    instantiate class alloc e1))))
				user-variables
				private-variables
				classes
				instantiates))
		       ,(e `(begin ,@body) e))))
	     (match-case (car bindings)
		(((and ?var (? symbol?)) ?expr)
		 (let* ((bdg (car bindings))
			(loc (find-location/loc bdg x))
			(id-type (parse-id var loc))
			(id (car id-type))
			(t (cdr id-type))
			(priv (gensym))
			(kclass (find-instantiate-class expr bdg loc)))
		    (loop (cdr bindings)
		       (cons var user-variables)
		       (cons priv private-variables)
		       (cons (cons id priv) rewrite-bindings)
		       (cons kclass classes)
		       (cons expr instantiates))))
		(else
		 (error #f
		    "co-instantiate:Illegal binding"
		    (car bindings))))))))

;*---------------------------------------------------------------------*/
;*    duplicate->make-accessors ...                                    */
;*---------------------------------------------------------------------*/
(define (duplicate->make-accessors form class duplicated provided e)
   (let* ((slots (tclass-all-slots class))
	  (len (length slots))
	  (dup-var (mark-symbol-non-user! (gensym 'duplicated)))
	  (dup-var-typed (make-typed-ident dup-var (type-id class)))
	  (new (gensym 'new))
	  (vargs (make-vector (length slots))))
      ;; we collect the provided values
      (let loop ((provided provided))
	 (if (null? provided)
	     'done
	     (let ((p (car provided)))
		(match-case p
		   (((and (? symbol?) ?s-name) ?value)
		    ;; plain slot
		    (vector-set! vargs
		       (find-slot-offset slots s-name "duplicate" p)
		       (cons #t (object-epairify value p))))
		   (else
		    (error #f "Illegal duplicate" form)))
		(loop (cdr provided)))))
      ;; we collect the duplicated values
      (let loop ((i 0)
		 (slots slots))
	 (if (null? slots)
	     'done
	     (let ((value (vector-ref vargs i)))
		(if (pair? value)
		    ;; a value is already provided for this object
		    'nothing
		    (let* ((slot (car slots))
			   (a-name (symbol-append
				      (type-id class) '- (slot-id slot))))
		       ;; no value is provided for this object we pick
		       ;; one from this duplicated object.
		       (let ()
			  (vector-set! vargs
			     i
			     (cons #t `(,a-name ,dup-var))))))
		(loop (+fx i 1) (cdr slots)))))
      ;; we just have now to build the make call
      (let loop ((i 0)
		 (slots slots)
		 (largs '())
		 (virtuals '()))
	 (if (=fx i len)
	     (let* ((make-name (symbol-append 'make- (type-id class)))
		    (alloc `(,make-name ,@(reverse! largs))))
		`(let* ((,dup-var-typed ,duplicated)
			(,new           ,alloc))
		    ,@(reverse! virtuals)
		    ,new))
	     (let ((value (cdr (vector-ref vargs i))))
		(cond
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
		       (cdr slots)
		       largs
		       (cons (make-virtual-set
				(car slots) value new
				class form e "duplicate")
			  virtuals)))
		   (else
		    (loop (+fx i 1)
		       (cdr slots)
		       (cons value largs)
		       virtuals))))))))

;*---------------------------------------------------------------------*/
;*    make-widening-accessors ...                                      */
;*---------------------------------------------------------------------*/
(define (make-widening-accessors form class obj provided e)
   (let* ((slots (filter (lambda (s)
			    (eq? (slot-class-owner s) class))
		    (tclass-slots class)))
	  (holder (tclass-holder class))
	  (len (length slots))
	  (vargs (make-vector (length slots)))
	  (dup-var (mark-symbol-non-user! (gensym 'obj)))
	  (tid (type-id class))
	  (tsid (type-id (tclass-its-super class)))
	  (dup-var-typed (make-typed-ident dup-var tsid))
	  (cast (make-private-sexp 'cast tsid obj)))
      ;; we collect the default values
      (let loop ((i 0)
		 (slots slots))
	 (if (null? slots)
	     'done
	     (let ((s (car slots)))
		(if (slot-default? s)
		    (vector-set! vargs i (cons #t (slot-default-value s)))
		    (vector-set! vargs i (cons #f #unspecified)))
		(loop (+fx i 1) (cdr slots)))))
      ;; we collect the provided values
      (let loop ((provided provided))
	 (if (null? provided)
	     'done
	     (let ((p (car provided)))
		(match-case p
		   (((and (? symbol?) ?s-name) ?value)
		    (vector-set! vargs
		       (find-slot-offset slots s-name "widen!" p)
		       (cons #t (object-epairify value p))))
		   (else
		    (error #f "Illegal widening" form)))
		(loop (cdr provided)))))
      ;; we check that we have a value for all formals
      (let loop ((i 0)
		 (s slots))
	 (cond
	    ((=fx i len)
	     'ok)
	    ((and (not (car (vector-ref vargs i)))
		  (not (slot-virtual? (car s))))
	     ;; no, this is not correct, at least one argument is missing
	     (error #f
		(format
		   "Illegal widening, missing value for field \"~a\""
		   (slot-id (car s)))
		form))
	    (else
	     (loop (+fx i 1) (cdr s)))))
      ;; we just have now to build the make call
      (let loop ((i 0)
		 (slots slots)
		 (largs '())
		 (virtuals '()))
	 (if (=fx i len)
	     (let ((widening (symbol-append (tclass-widening class)
				'-
				(type-id class))))
		(if *unsafe-type*
		    `(let ((,dup-var-typed ,cast))
			(object-widening-set!
			   ,dup-var
			   (,widening ,@(reverse! largs)))
			;; the new class must be set after initialization
			;; otherwise type errors are possibles
			((@ object-class-num-set! __object)
			 ,dup-var
			 ((@ class-num __object)
			  (@ ,(global-id holder) ,(global-module holder))))
			,@(reverse! virtuals)
			,dup-var)
		    `(let ((,dup-var-typed ,cast))
			(if (object-widening ,dup-var)
			    (shrink! ,dup-var))
			(if (eq? ((@ class-super __object)
				  (@ ,(global-id holder)
				     ,(global-module holder)))
			       ((@ object-class __object) ,dup-var))
			    (begin
			       (object-widening-set!
				  ,dup-var
				  (,widening ,@(reverse! largs)))
			       ;; same remark as below
			       ((@ object-class-num-set! __object)
				,dup-var
				((@ class-num __object)
				 (@ ,(global-id holder)
				    ,(global-module holder))))
			       ,@(reverse! virtuals)
			       ,dup-var)
			    (error
			       "widen!"
			       "This object can't be widened to the wanted class"
			       ,dup-var)))))
	     (let ((value (cdr (vector-ref vargs i))))
		(cond
		   ((slot-virtual? (car slots))
		    (loop (+fx i 1)
		       (cdr slots)
		       largs
		       (if (eq? value #unspecified)
			   virtuals
			   (cons (make-virtual-set
				    (car slots) value dup-var
				    class form e "widen!")
			      virtuals))))
		   (else
		    (loop (+fx i 1)
		       (cdr slots)
		       (cons value largs)
		       virtuals))))))))

;*---------------------------------------------------------------------*/
;*    make-virtual-set ...                                             */
;*---------------------------------------------------------------------*/
(define (make-virtual-set slot value var class form e alloc-kind)
   (if (slot-read-only? slot)
       (error #f
	  (format "Illegal \"~a\", field read-only \"~a\""
	     alloc-kind (slot-id slot))
	  form)
       (object-epairify
	(e `(,(symbol-append (tclass-id class) '- (slot-id slot) '-set!)
	     ,var
	     ,value)
	   e)
	value)))

;*---------------------------------------------------------------------*/
;*    object-epairify ...                                              */
;*---------------------------------------------------------------------*/
(define (object-epairify obj epair)
   (if (epair? epair)
       (if (pair? obj)
	   (econs (car obj) (cdr obj) (cer epair))
	   (object-epairify `(begin ,obj) epair))
       obj))

;*---------------------------------------------------------------------*/
;*    find-slot-offset ...                                             */
;*---------------------------------------------------------------------*/
(define (find-slot-offset slots::pair-nil name::symbol form sexp)
   (let loop ((slots slots)
	      (i 0))
      (cond
	 ((null? slots)
	  (error #f
	     (format "Illegal ~a, field unknown \"~a\"" form name)
	     sexp))
	 ((eq? (slot-id (car slots)) name)
	  i)
	 (else   
	  (loop (cdr slots) (+fx i 1))))))
