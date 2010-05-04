;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Eval/evobject.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jan 14 17:11:54 2006                          */
;*    Last change :  Tue Jan 12 11:55:22 2010 (serrano)                */
;*    Copyright   :  2006-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Eval class definition                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __evobject

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
	    __evenv
	    __everror
	    __evcompile
	    __expander_define)
    
  (export (eval-class ::symbol ::bool ::pair-nil ::pair)
	  (eval-make-slot ::symbol ::obj ::bool ::obj ::obj ::obj ::bool ::obj)
	  (eval-expand-instantiate::pair-nil ::symbol ::pair-nil)
	  (eval-expand-duplicate::pair-nil ::symbol ::pair-nil)
	  (eval-expand-with-access::pair-nil ::symbol ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    slot                                                             */
;*---------------------------------------------------------------------*/
(define-struct slot id type ronlyp default-value getter setter indexp info)

;*---------------------------------------------------------------------*/
;*    eval-make-slot ...                                               */
;*---------------------------------------------------------------------*/
(define (eval-make-slot id type ronlyp default-value getter setter indexp info)
   (slot id type ronlyp default-value getter setter indexp info))

;*---------------------------------------------------------------------*/
;*    slot-virtualp ...                                                */
;*---------------------------------------------------------------------*/
(define (slot-virtualp slot)
   (slot-getter slot))

;*---------------------------------------------------------------------*/
;*    decompose-ident ...                                              */
;*---------------------------------------------------------------------*/
(define (decompose-ident id::symbol)
   (let* ((string (symbol->string id))
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
;*    class-predicate ...                                              */
;*---------------------------------------------------------------------*/
(define (class-predicate id)
   (symbol-append id '?))

;*---------------------------------------------------------------------*/
;*    class-make ...                                                   */
;*---------------------------------------------------------------------*/
(define (class-make id)
   (symbol-append 'make- id))

;*---------------------------------------------------------------------*/
;*    class-allocate ...                                               */
;*---------------------------------------------------------------------*/
(define (class-allocate id)
   (symbol-append '%allocate- id))

;*---------------------------------------------------------------------*/
;*    class-nil ...                                                    */
;*---------------------------------------------------------------------*/
(define (class-nil id)
   (symbol-append id '-nil))

;*---------------------------------------------------------------------*/
;*    slot-ref ...                                                     */
;*---------------------------------------------------------------------*/
(define (slot-ref fid cid)
   (symbol-append cid '- fid))

;*---------------------------------------------------------------------*/
;*    slot-set ...                                                     */
;*---------------------------------------------------------------------*/
(define (slot-set fid cid)
   (symbol-append cid '- fid '-set!))

;*---------------------------------------------------------------------*/
;*    slot-len ...                                                     */
;*---------------------------------------------------------------------*/
(define (slot-len fid)
   (symbol-append fid '-len))

;*---------------------------------------------------------------------*/
;*    find-super-native-class ...                                      */
;*---------------------------------------------------------------------*/
(define (find-super-native-class class)
   (if (eval-class? class)
       (find-super-native-class (class-super class))
       class))

;*---------------------------------------------------------------------*/
;*    slot-non-virtual-id ...                                          */
;*---------------------------------------------------------------------*/
(define (slot-non-virtual-id s)
   (and (not (slot-virtualp s)) (slot-id s)))

;*---------------------------------------------------------------------*/
;*    non-virtual-slots ...                                            */
;*---------------------------------------------------------------------*/
(define (non-virtual-slots lst)
   (filter (lambda (s) (not (slot-virtualp s))) lst))

;*---------------------------------------------------------------------*/
;*    localize ...                                                     */
;*---------------------------------------------------------------------*/
(define (localize loc p)
   (if (not loc)
       p
       (let loop ((p p))
	  (if (not (pair? p))
	      p
	      (econs (loop (car p)) (loop (cdr p)) loc)))))

;*---------------------------------------------------------------------*/
;*    eval-make-class ...                                              */
;*---------------------------------------------------------------------*/
(define (eval-make-class loc id slots const super super-slots native native-slots)
   (let* ((fid (class-make id))
	  (tmp (gensym))
	  (make-native (class-make (class-name native)))
	  (slots (non-virtual-slots slots))
	  (super-slots (non-virtual-slots super-slots))
	  (native-slots (non-virtual-slots native-slots))
	  (all-slots (append super-slots slots))
	  (native-args (filter-map slot-non-virtual-id native-slots))
	  (all-args (append (map slot-id super-slots) (map slot-id slots)))
	  (eval-slots (drop all-slots (length native-slots)))
	  (eval-args (drop all-args (length native-args))))
      (define (init-slot s a)
	 (let ((v (if (slot-indexp s)
		      `(make-vector ,(slot-len (slot-id s)) ,a)
		      a)))
	    v))
      (localize
       loc
       `(define (,fid ,@all-args)
	   (let ((,tmp (,make-native ,@native-args)))
	      (object-class-num-set! ,tmp (class-num ,id))
	      (%object-widening-set! ,tmp
				     (vector ,@(map init-slot
						    eval-slots
						    eval-args)))
	      ,(when const `(,const ,tmp))
	      ,tmp)))))

;*---------------------------------------------------------------------*/
;*    eval-allocate-class ...                                          */
;*---------------------------------------------------------------------*/
(define (eval-allocate-class loc id slots super super-slots native native-slots)
   (let* ((allocate-native (class-allocate (class-name native)))
	  (native-args (filter-map slot-non-virtual-id native-slots))
	  (args (append (filter-map slot-non-virtual-id super-slots)
			(filter-map slot-non-virtual-id slots)))
	  (fid (class-allocate id))
	  (tmp (gensym)))
      (localize
       loc
       `(define (,fid)
	   (let ((,tmp (,allocate-native)))
	      (object-class-num-set! ,tmp (class-num ,id))
	      (%object-widening-set! ,tmp (make-vector
					   ,(- (length args)
					       (length native-args))))
	      ,tmp)))))

;*---------------------------------------------------------------------*/
;*    eval-class-nil ...                                               */
;*---------------------------------------------------------------------*/
(define (eval-class-nil loc id super native)
   (let* ((native-nil (class-nil (class-name native)))
	  (fid (class-nil id))
	  (tmp (gensym)))
      (localize
       loc
       `(define (,fid)
	   (let ((,tmp (,native-nil)))
	      (object-class-num-set! ,tmp (class-num ,id))
	      ,tmp)))))

;*---------------------------------------------------------------------*/
;*    eval-class-predicate ...                                         */
;*---------------------------------------------------------------------*/
(define (eval-class-predicate loc id)
   (let ((fid (class-predicate id)))
      (localize
       loc
       `(define (,fid x)
	   (is-a? x ,id)))))

;*---------------------------------------------------------------------*/
;*    eval-class-slot-alias ...                                        */
;*---------------------------------------------------------------------*/
(define (eval-class-slot-alias loc cid slot super)
   (let ((sid (slot-id slot)))
      (define (ref)
	 `(define ,(slot-ref sid cid)
	     ,(slot-ref sid (class-name super))))
      (define (set)
	 `(define ,(slot-set sid cid)
	     ,(slot-set sid (class-name super))))
      (if (slot-ronlyp slot)
	  (list (localize loc (ref)))
	  (list (localize loc (ref)) (localize loc (set))))))

;*---------------------------------------------------------------------*/
;*    eval-class-slot ...                                              */
;*---------------------------------------------------------------------*/
(define (eval-class-slot loc cid slot offset)
   (define (ref-plain)
      (let ((body (if (slot-getter slot)
		      `(,(slot-getter slot) o)
		      `(vector-ref-ur (%object-widening o) ,offset))))
	 `(define (,(slot-ref (slot-id slot) cid) o)
	     (if (,(class-predicate cid) o)
		 ,body
		 (bigloo-type-error
		  ',(slot-ref (slot-id slot) cid) ',cid o)))))
   (define (set-plain)
      (let ((body (if (slot-setter slot)
		      `(,(slot-setter slot) o v)
		      `(vector-set-ur! (%object-widening o) ,offset v))))
	 `(define (,(slot-set (slot-id slot) cid) o v)
	     (if (,(class-predicate cid) o)
		 ,body
		 (bigloo-type-error
		  ',(slot-set (slot-id slot) cid) ',cid o)))))
   (define (ref-index)
      `(define (,(slot-ref (slot-id slot) cid) o i)
	  (if (,(class-predicate cid) o)
	      (vector-ref (vector-ref-ur (%object-widening o) ,offset) i)
	      (bigloo-type-error
	       ',(slot-ref (slot-id slot) cid) ',cid o))))
   (define (set-index)
      (let ((body `(vector-set! (vector-ref-ur (%object-widening o) ,offset) i v)))
	 `(define (,(slot-set (slot-id slot) cid) o i v)
	     (if (,(class-predicate cid) o)
		 ,body
		 (bigloo-type-error
		  ',(slot-set (slot-id slot) cid) ',cid o)))))
   (define (ref)
      (if (slot-indexp slot)
	  (ref-index)
	  (ref-plain)))
   (define (set)
      (if (slot-indexp slot)
	  (set-index)
	  (set-plain)))
   (if (slot-ronlyp slot)
       (list (localize loc (ref)))
       (list (localize loc (ref)) (localize loc (set)))))

;*---------------------------------------------------------------------*/
;*    eval-register-class ...                                          */
;*---------------------------------------------------------------------*/
(define (eval-register-class loc id super abstract slots sz hash constructor)
   (let ((cla (gensym)))
      (localize
       loc
       `(define ,id
	   (let ((,cla (register-class!
			',id
			,(class-name super)
			,abstract
			,(unless abstract (class-make id))
			,(if abstract list (class-allocate id))
			,(class-nil id)
			,(class-predicate id)
			,hash
			(list ,@(map (lambda (f)
					(let ((sid (slot-id f)))
					   `(make-class-field
					     ',sid
					     ,(or (slot-getter f)
						  (slot-ref sid id))
					     ,(unless (slot-ronlyp f)
						 (or (slot-setter f)
						     (slot-set sid id)))
					     ,(if (slot-indexp f)
						  (slot-ref (slot-len sid) id)
						  #f)
					     ,(slot-virtualp f)
					     ,(slot-info f)
					     ',(slot-default-value f)
					     ;; MS: 21 June 2007
					     #;(class-field-no-default-value))))
				     slots))
			,constructor
			'#())))
	      (class-evdata-set! ,cla ,sz)
	      ,cla)))))

;*---------------------------------------------------------------------*/
;*    field->slot ...                                                  */
;*---------------------------------------------------------------------*/
(define (field->slot field)
   (slot (class-field-name field)
	 #f
	 (not (class-field-mutable? field))
	 (class-field-default-value field)
	 (when (class-field-virtual? field)
	    (class-field-accessor field))
	 (when (and (class-field-virtual? field)
		    (class-field-mutable? field))
	    (class-field-mutator field))
	 #f
	 (class-field-info field)))

;*---------------------------------------------------------------------*/
;*    eval-instantiate->fill ...                                       */
;*---------------------------------------------------------------------*/
(define (eval-instantiate->fill cid args slots source)
   (let* ((mk (class-make cid))
	  (ins (symbol-append 'instantiate:: cid))
	  (nodef (class-field-no-default-value))
	  (new (gensym))
	  (tmp (gensym)))
      (let loop ((slots slots)
		 (vals '())
		 (virtuals '()))
	 (if (null? slots)
	     (list 'quasiquote
		   (if (null? virtuals)
		       `(,mk ,@(map (lambda (v) (list 'unquote v))
				    (reverse! vals)))
		       `(let ((,new (,mk ,@(map (lambda (v) (list 'unquote v))
						(reverse! vals)))))
			   ,@(map (lambda (id)
				     `(unless (eq? ,(list 'unquote id) ,new)
					 (,(slot-set id cid)
					  ,new ,(list 'unquote id))))
				  virtuals)
			   ,new)))
	     (let* ((s (car slots))
		    (id (slot-id s)))
		(cond
		   ;; a virtual slot
		   ((slot-virtualp s)
		    (if (slot-ronlyp s)
			`(if (pair? (assq ',id ,args))
			     (error/source
			      ',ins
			      "value provided for read-only virtual slot"
			      ',id
			      ,source)
			     ,(loop (cdr slots) vals virtuals))
			`(let ((,id (let ((c (assq ',id ,args)))
				       (if (pair? c)
					   (cadr c)
					   ,(let ((d (slot-default-value s)))
					       (if (eq? d nodef)
						   ;; new play the role of
						   ;; a mark for nodef
						   `',new
						   (list 'quote d)))))))
			    ,(loop (cdr slots) vals (cons id virtuals)))))
		   (else
		    ;; a plain slot
		    `(let ((,id (let ((c (assq ',id ,args)))
				   (if (pair? c)
				       (cadr c)
				       ,(let ((d (slot-default-value s)))
					   (if (eq? d nodef)
					       `(error/source ',ins
							      "argument missing"
							      ',id
							      ,source)
					       (list 'quote d)))))))
			,(loop (cdr slots) (cons id vals) virtuals)))))))))

;*---------------------------------------------------------------------*/
;*    eval-instantiate-check ...                                       */
;*---------------------------------------------------------------------*/
(define (eval-instantiate-check id args all-slots body)
   `(let ((lst (filter (lambda (s)
			  (or (not (pair? s))
			      (not (symbol? (car s)))
			      (not (memq (car s) ',(map slot-id all-slots)))))
		       ,args)))
       (if (pair? lst)
	   (error ',id "Illegal slot(s)" lst)
	   ,body)))

;*---------------------------------------------------------------------*/
;*    eval-expand-instantiate ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-expand-instantiate cid all-slots)
   (let ((id (symbol-append 'instantiate:: cid))
	 (a (gensym))
	 (loc (gensym))
	 (slots all-slots))
      `(define-expander ,id
	  :eval!
	  (lambda (x e)
	     (let ((,a (cdr x)))
		(e ,(eval-instantiate-check
		     id a slots
		     (eval-instantiate->fill cid a slots 'x))
		   e))))))

;*---------------------------------------------------------------------*/
;*    eval-duplicate->fill ...                                         */
;*---------------------------------------------------------------------*/
(define (eval-duplicate->fill cid args old slots)
   (let* ((mk (class-make cid))
	  (ins (symbol-append 'duplicate:: cid))
	  (nodef (class-field-no-default-value))
	  (new (gensym))
	  (tmp (gensym))
	  (slots slots))
      (let loop ((slots slots)
		 (vals '())
		 (virtuals '()))
	 (if (null? slots)
	     (list 'quasiquote
		   (if (null? virtuals)
		       `(let* ((,tmp ,(list 'unquote old)))
			   (,mk ,@(map (lambda (v) (list 'unquote v))
				       (reverse! vals))))
		       `(let* ((,tmp ,(list 'unquote old))
			       (,new (,mk ,@(map (lambda (v) (list 'unquote v))
						 (reverse! vals)))))
			   ,@(map (lambda (id)
				     `(,(slot-set id cid)
				       ,new ,(list 'unquote id)))
				  virtuals)
			   ,new)))
	     (let* ((s (car slots))
		    (id (slot-id s)))
		(cond
		   ;; a virtual slot
		   ((slot-virtualp s)
		    `(let ((,id (let ((c (assq ',id ,args)))
				   (if (pair? c)
				       (cadr c)
				       ,(let ((d (slot-default-value s)))
					   (if (eq? d nodef)
					       '#unspecified
					       (list 'quote d)))))))
			,(loop (cdr slots) vals (cons id virtuals))))
		   (else
		    ;; a plain slot
		    `(let ((,id (let ((c (assq ',id ,args)))
				   (if (pair? c)
				       (cadr c)
				       '(,(slot-ref id cid) ,tmp)))))
			,(loop (cdr slots) (cons id vals) virtuals)))))))))

;*---------------------------------------------------------------------*/
;*    eval-expand-duplicate ...                                        */
;*---------------------------------------------------------------------*/
(define (eval-expand-duplicate cid all-slots)
   (let ((id (symbol-append 'duplicate:: cid))
	 (o (gensym))
	 (a (gensym))
	 (loc (gensym)))
      `(define-expander ,id
	  :eval!
	  (lambda (x e)
	     (if (null? (cdr x))
		 (error/source x "object missing" #f x)
		 (let ((,o (cadr x))
		       (,a (cddr x)))
		    (e ,(eval-instantiate-check
			 id a all-slots
			 (eval-duplicate->fill cid a o all-slots))
		       e)))))))

;*---------------------------------------------------------------------*/
;*    eval-expand-with-access ...                                      */
;*---------------------------------------------------------------------*/
(define (eval-expand-with-access id all-slots)
   (let ((wid (symbol-append 'with-access:: id))
	 (loc (gensym)))
      `(define-expander ,wid
	  :eval!
	  (lambda (x e)
	     (match-case x
		((?- ?i (and (? list?) ?vars) . ?body)
		 ,(eval-with-access-expander id all-slots 'x))
		(else
		 (error/source ',wid "Illegal `with-access' form" x x)))))))

;*---------------------------------------------------------------------*/
;*    eval-with-access-expander ...                                    */
;*    -------------------------------------------------------------    */
;*    VARS and BODY are not transmitted because this code generates    */
;*    an expression that supposes that these two variables are bound.  */
;*---------------------------------------------------------------------*/
(define (eval-with-access-expander id all-slots source)
   `(let* ((slots ',(map (lambda (s)
			    (list (slot-id s)
				  (if (slot-indexp s) #t #f)
				  (slot-ronlyp s)))
			 all-slots))
	   (bdgs (map (lambda (v)
			 (cond
			    ((symbol? v)
			     (if (assq v slots)
				 (list v v)
				 (error/source
				  ',(symbol-append 'with-access:: id)
				  "Illegal attribute"
				  v
				  ,source)))
			    ((and (pair? v) (symbol? (car v))
				  (pair? (cdr v)) (symbol? (cadr v))
				  (null? (cddr v)))
			     (if (assq (cadr v) slots)
				 v
				 (error/source
				  ',(symbol-append 'with-access:: id)
				  "Illegal attribute"
				  v
				  ,source)))
			    (else
			     (error/source
			      ',(symbol-append 'with-access:: id)
			      "Illegal attribute"
			      v
			      ,source))))
		      vars))
	   (ins (gensym))
	   (body `(let ((,ins ,i))
		     ,,(make-eval-with-access-body id all-slots 'ins)))
	   (e2 ,(make-eval-with-access-expander id 'ins)))
       (%with-lexical (map car bdgs) body e2 ins)))

;*---------------------------------------------------------------------*/
;*    make-eval-with-access-expander ...                               */
;*---------------------------------------------------------------------*/
(define (make-eval-with-access-expander id tmp)
   `(eval-begin-expander
     (lambda (x e3)
	(match-case x
	   ((? symbol?)
	    (let* ((c1 (assq x bdgs))
		   (d (and (pair? c1)
			   (let ((c (assq x (%lexical-stack))))
			      (and (pair? c) (eq? (cdr c) ,tmp))))))
	       (if (not d)
		   (e x e)
		   (e `(,(symbol-append ',id '- (cadr c1)) ,,tmp) e))))
	   ((set! ?s ?v)
	    (let* ((c1 (assq s bdgs))
		   (d (and (pair? c1)
			   (let ((c (assq s (%lexical-stack))))
			      (and (pair? c) (eq? (cdr c) ,tmp)))
			   (assq (cadr c1) slots))))
	       (let ((v2 (e3 v e3)))
		  (if (or (not d) (caddr d))
		      (e `(set! ,s ,v2) e)
		      (e `(,(symbol-append ',id '- (cadr c1) '-set!) ,,tmp ,v2) e)))))
	   (else
	    (e x e3))))))

;*---------------------------------------------------------------------*/
;*    make-eval-with-access-body ...                                   */
;*---------------------------------------------------------------------*/
(define (make-eval-with-access-body id all-slots tmp)
   (if (every? (lambda (s) (slot-indexp s) all-slots))
       (make-eval-with-access-body-plain id all-slots tmp)
       (make-eval-with-access-body-indexed id all-slots tmp)))

;*---------------------------------------------------------------------*/
;*    make-eval-with-access-body-plain ...                             */
;*---------------------------------------------------------------------*/
(define (make-eval-with-access-body-plain id all-slots tmp)
   ``(begin ,@body))

;*---------------------------------------------------------------------*/
;*    make-eval-with-access-body-indexed ...                           */
;*---------------------------------------------------------------------*/
(define (make-eval-with-access-body-indexed id all-slots tmp)
   `(let loop ((vars vars))
       (if (null? vars)
	   `(begin ,@body)
	   (let ((d (assq (cadr (car bdgs)) slots)))
	      (cond
		 ((not d)
		  (error ',(symbol-append 'with-access:: id)
			 "Illegal attribute"
			 (car vars)))
		 ((cadr d)
		  ;; an indexed slot
		  (let ((v `(,(symbol-append ',id '- (caar vars) '-len)
			     ,tmp))
			(r `(lambda (r)
			       (,(symbol-append ',id '- (caar vars) '-ref)
				,tmp r)))
			(s `(lambda (r v)
			       (,(symbol-append ',id '- (caar vars) '-set!)
				,tmp r v))))
		     (if (caddr d)
			 `(let ((,(symbol-append (caar vars) '-len) ,v)
				(,(symbol-append (caar vars) '-ref) ,r))
			     ,(loop (cdr vars)))
			 `(let ((,(symbol-append (caar vars) '-len) ,v)
				(,(symbol-append (caar vars) '-ref) ,r)
				(,(symbol-append (caar vars) '-set!) ,s))
			     ,(loop (cdr vars))))))
		 (else
		  ;; a direct slot
		  (loop (cdr vars))))))))

;*---------------------------------------------------------------------*/
;*    make-eval-object->struct ...                                     */
;*---------------------------------------------------------------------*/
(define (make-eval-object->struct cid slots)
   `(define-method (object->struct ,(symbol-append 'obj:: cid))
       (let ((res (make-struct ',cid ,(+fx (length slots) 1) #unspecified)))
	  (struct-key-set! res ',cid)
	  (struct-set! res 0 #f)
	  ,@(let loop ((i 1)
		       (slots slots))
	       (if (null? slots)
		   '()
		   (let* ((s (car slots))
			  (ref `(,(symbol-append cid '- (slot-id s)) obj)))
		      (cons `(struct-set! res ,i ,ref)
			    (loop (+fx i 1) (cdr slots))))))
	  res)))

;*---------------------------------------------------------------------*/
;*    make-eval-struct+object->object ...                              */
;*---------------------------------------------------------------------*/
(define (make-eval-struct+object->object cid slots)
   `(define-method (struct+object->object ,(symbol-append 'obj:: cid)
					  struct::struct)
       ,@(let loop ((i 1)
		    (slots slots))
	    (if (null? slots)
		'()
		(let ((s (car slots)))
		   (cons `(,(symbol-append cid '- (slot-id s) '-set!)
			   obj
			   (struct-ref struct ,i))
			 (loop (+fx i 1) (cdr slots))))))
       obj))

;*---------------------------------------------------------------------*/
;*    class-all-slots ...                                              */
;*---------------------------------------------------------------------*/
(define (class-all-slots class)
   (map field->slot (class-all-fields class)))

;*---------------------------------------------------------------------*/
;*    eval-parse-class-slot ...                                        */
;*---------------------------------------------------------------------*/
(define (eval-parse-class-slot loc f)
   (cond
      ((symbol? f)
       (multiple-value-bind (id type)
	  (decompose-ident f)
	  (list (slot id type #f (class-field-no-default-value) #f #f #f #f))))
      ((not (and (list? f) (symbol? (car f))))
       (evcompile-error (find-loc f loc)
			'eval "Illegal slot declaration" f))
      ((and (eq? (car f) '*)
	    (or (null? (cdr f))
		(not (symbol? (cadr f)))
		(assq 'get (cddr f))
		(assq 'set (cddr f))))
       (evcompile-error (find-loc f loc)
			'eval "Illegal indexed slot declaration" f))
      (else
       (let ((indexp #f)
	     (id (car f))
	     (attrs (cdr f)))
	  (when (eq? (car f) '*)
	     (set! id (cadr f))
	     (set! indexp #t)
	     (set! attrs (cddr f)))
	  (multiple-value-bind (id type)
	     (decompose-ident id)
	     (let ((def (class-field-no-default-value))
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
				      (find-loc f loc)
				      'eval "Illegal slot declaration" f))))))
			  attrs)
		(cond
		   ((and get (not ronly) (not set))
		    (evcompile-error
		     (find-loc f loc)
		     'eval "Missing virtual set" f))
		   ((and set (not get))
		    (evcompile-error
		     (find-loc f loc)
		     'eval "Missing virtual get" f))
		   (else
		    (let ((s (slot id type ronly def get set indexp info)))
		       (if indexp
			   (list
			    (slot (slot-len id)
				  'integer
				  #t
				  (class-field-no-default-value)
				  #f
				  #f
				  #f
				  #f)
			    s)
			   (list s)))))))))))

;*---------------------------------------------------------------------*/
;*    eval-parse-class ...                                             */
;*    -------------------------------------------------------------    */
;*    Parse the class clauses, returning the constructor and           */
;*    the new slots.                                                   */
;*---------------------------------------------------------------------*/
(define (eval-parse-class loc clauses)
   (let ((loc (find-loc clauses loc)))
      (cond
	 ((null? clauses)
	  (values #f '()))
	 ((not (list? clauses))
	  (evcompile-error (find-loc clauses loc)
			   'eval "Illegal class declaration" clauses))
	 ((match-case (car clauses) (((? symbol?)) #t) (else #f))
	  ;; the constructor must be protected under a lambda because
	  ;; may be still unitialized
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
;*    eval-class ...                                                   */
;*    -------------------------------------------------------------    */
;*    Returns a double value, the list of declarations and the list    */
;*    of bound variables.                                              */
;*---------------------------------------------------------------------*/
(define (eval-class id abstract clauses src)
   (multiple-value-bind (cid sid)
      (decompose-ident id)
      (let ((loc (find-loc src #f))
	    (super (find-class (or sid 'object))))
	 (cond
	    ((not (class? super))
	     (evcompile-error loc 'eval "Cannot find super class" sid))
;* 	    (((class-abstract? super)                                  */
;* 	     (evcompile-error loc 'eval                                */
;* 			      (format "Eval class \"~a\" cannot inherit from abstract native classes" cid) sid)) */
	    (else
	     (multiple-value-bind (constructor slots)
		(eval-parse-class loc clauses)
		(let* ((super-slots (class-all-slots super))
		       (native (find-super-native-class super))
		       (native-slots (class-all-slots native))
		       (all-slots (append super-slots slots))
		       (offset (if (eval-class? super)
				   (class-evdata super)
				   0))
		       (plain-slots (filter (lambda (s)
					       (not (slot-virtualp s)))
					    slots))
		       (size (length plain-slots)))
		   ;; check illegally overriden fields
		   (let loop ((slots all-slots))
		      (when (pair? slots)
			 (let ((s (car slots)))
			    (for-each (lambda (t)
					 (when (eq? (slot-id s) (slot-id t))
					    (evcompile-error
					     loc
					     'eval
					     "Illegal duplicate field"
					     (slot-id s))))
				      (cdr slots))
			    (loop (cdr slots)))))
		   (let ((exprs '())
			 (idents '()))
		      ;; class definition
		      (set! exprs (list (eval-register-class
					 loc cid super abstract
					 slots (+ offset size)
					 (get-eval-class-hash id src)
					 constructor)))
		      (set! idents (list cid))
		      ;; class slots
		      (let ((e1 (append-map (lambda (f)
					       (eval-class-slot loc cid f -1))
					    (filter slot-virtualp slots)))
			    (e2 (append-map (lambda (f o)
					       (eval-class-slot loc cid f o))
					    plain-slots
					    (iota size offset))))
			 (set! exprs (append e2 exprs))
			 (set! exprs (append e1 exprs))
			 (set! idents (append (map caadr e2) idents))
			 (set! idents (append (map caadr e1) idents)))
		      ;; super slots
		      (let ((e (append-map (lambda (f)
					      (eval-class-slot-alias
					       loc cid f super))
					   super-slots)))
			 (set! exprs (append e exprs))
			 (set! idents (append (map cadr e) idents)))
		      ;; with-access
		      (set! exprs
			    (cons (eval-expand-with-access cid all-slots)
				  exprs))
		      ;; predicate
		      (let ((e (eval-class-predicate loc cid)))
			 (set! exprs (cons e exprs))
			 (set! idents (cons (caadr e) idents)))
		      ;; class-nil
		      (let ((e (eval-class-nil loc cid super native)))
			 (set! exprs (cons e exprs))
			 (set! idents (cons (caadr e) idents)))
		      ;; constructor
		      (unless abstract
			 ;; make
			 (let ((e (eval-make-class
				   loc cid slots
				   (or constructor (find-class-constructor super))
				   super super-slots
				   native native-slots)))
			    (set! exprs (cons e exprs))
			    (set! idents (cons (caadr e) idents)))
			 ;; allocate
			 (let ((e (eval-allocate-class loc cid slots
						       super super-slots
						       native native-slots)))
			    (set! exprs (cons e exprs))
			    (set! idents (cons (caadr e) idents)))
			 ;; instantiate
			 (let ((e (eval-expand-instantiate cid all-slots)))
			    (set! exprs (cons e exprs)))
			 ;; duplicate
			 (let ((e (eval-expand-duplicate cid all-slots)))
			    (set! exprs (cons e exprs)))
			 ;; intext
			 (let ((int (make-eval-struct+object->object cid all-slots))
			       (ext (make-eval-object->struct cid all-slots)))
			    (set! exprs (append exprs (list ext int)))))
 		      (values exprs idents)))))))))

;*---------------------------------------------------------------------*/
;*    get-eval-class-hash ...                                          */
;*---------------------------------------------------------------------*/
(define (get-eval-class-hash class-id fields)
   (let loop ((fields fields)
	      (hash (get-hashnumber class-id)))
      (if (null? fields)
	  hash
	  (let ((field (car fields)))
	     (match-case field
		((?-)
		 (loop (cdr fields) (bit-xor hash 2344)))
		((? symbol?)
		 (loop (cdr fields) (bit-xor hash (get-hashnumber field))))
		((* (and ?id (? symbol?)) . ?att)
		 (loop (cdr fields) (bit-xor hash (get-hashnumber id))))
		(((and ?id (? symbol?)) . ?att)
		 (loop (cdr fields) (bit-xor hash (get-hashnumber id)))))))))
