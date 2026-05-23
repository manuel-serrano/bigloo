;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0.x/runtime/Llib/class.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 23 09:51:35 2025                          */
;*    Last change :  Sat May 23 09:24:04 2026 (serrano)                */
;*    Copyright   :  2025-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Tools for parsing and expanding classes                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __class
   
   (include "Llib/class.sch"
	    "Llib/object.sch")
   
   (import  __error
	    __object
	    __configure
	    __reader
	    __hash
	    __expander_define
	    __progn
	    __expand
	    __module5)

   (use     __type
	    __error
	    __bigloo
	    __tvector
	    __structure
	    __foreign
	    __param
	    __bexit
	    __bignum
	    __thread
	    __bit
	    __hash
	    __thread
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_vectors_6_8
	    __r4_control_features_6_9
	    __r4_pairs_and_lists_6_3
 	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_strings_6_7
	    __r4_ports_6_10_1
	    __r4_output_6_10_3

	    __r5_control_features_6_4
	    
	    __pp_circle
	    __evenv)
   
   (export (parse-class::struct ::pair ::Module)
	   (creator-expand::pair ::struct)
	   (nil-creator-expand::pair ::struct ::obj)
	   (properties-expand::pair ::struct ::bool)
	   (registration-expand::pair ::struct ::Module)
	   (instantiate-expander::procedure ::struct ::Module)
	   (duplicate-expander::procedure ::struct ::Module)
	   (widen-expander::procedure ::struct ::Module)
	   (with-access-expander::procedure ::struct ::Module)
	   (co-instantiate-expander::procedure ::Module)))

;*---------------------------------------------------------------------*/
;*    parse-class ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-class x::pair mod::Module)
   
   (define (class-kind? k)
      (or (eq? k 'define-class)
	  (eq? k 'define-abstract-class)
	  (eq? k 'define-wide-class)
	  (eq? k 'define-final-class)))
   
   (define (parse-class-ident ident x)
      (multiple-value-bind (id super)
	 (parse-ident ident x)
	 (cond
	    ((not super) (values id 'object))
	    ((eq? id super) (values id #f))
	    (else (values id super)))))

   (define (class-depth-and-virtual-properties k)
      (if (not k)
	  (values 0 '())
	  (let ((ci (module5-get-class mod k)))
	     (if (not ci)
		 (error/loc (-> mod id)
		    (format "Cannot find super class \"~a\"" k) x x)
		 (values (+fx 1 (class-info-depth ci))
		    (class-info-vproperties ci))))))
   
   (match-case x
      (((and (? class-kind?) ?kind)  ?ident (?ctor) . ?props)
       (multiple-value-bind (id super)
	  (parse-class-ident ident x)
	  (multiple-value-bind (depth vproperties)
	     (class-depth-and-virtual-properties super)
	     (multiple-value-bind (props vprops)
		(parse-properties props id vproperties)
		(class-info id depth super kind ctor
		   props #unspecified x #f vprops)))))
      (((and (? class-kind?) ?kind) ?ident . ?props)
       (multiple-value-bind (id super)
	  (parse-class-ident ident x)
	  (multiple-value-bind (depth vproperties)
	     (class-depth-and-virtual-properties super)
	     (multiple-value-bind (props vprops)
		(parse-properties props id vproperties)
		(class-info id depth super kind #f
		   props #unspecified x #f vprops)))))
      (else
       (error/loc "parse" "Illegal class definition" x x))))

;*---------------------------------------------------------------------*/
;*    make-typed-ident ...                                             */
;*---------------------------------------------------------------------*/
(define (make-typed-ident id type)
   (symbol-append id '|::| (or type 'obj)))

;*---------------------------------------------------------------------*/
;*    parse-ident ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-ident id x)
   (let* ((s (symbol->string id))
	  (l (string-length s)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i (-fx l 2))
	     (values id #f))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (values (string->symbol (substring s 0 i))
		    (string->symbol (substring s (+fx i 2))))
		 (loop (+fx i 1))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    parse-properties ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-properties props klass vproperties)

   (define (ronly-error pi a)
      (error/loc klass
	 (format "Abstract property \"~a\" is declared read-only but is given a setter"
	    (prop-info-id pi))
	 a a))


   (define vindex (length vproperties))
   (define vprops '())

   (define (find-property id props)
      (find (lambda (p) (eq? (prop-info-id p) id)) props))
   
   (define (parse-attribute a pi x)
      (match-case a
	 (read-only
	  (if (eq? (prop-info-set pi) #t)
	      (ronly-error pi a)
	      (prop-info-ronly?-set! pi #t)))
	 ((default ?val)
	  (prop-info-defv?-set! pi #t)
	  (prop-info-value-set! pi val))
	 ((get ?get)
	  (prop-info-get-set! pi get)
	  (unless (prop-info-virtual? pi)
	     (prop-info-virtual?-set! pi #t)
	     (let ((o (find-property (prop-info-id pi) vproperties)))
		(if o
		    (prop-info-vindex-set! pi (prop-info-vindex o))
		    (begin
		       (set! vprops (cons pi vprops))
		       (prop-info-vindex-set! pi vindex)
		       (set! vindex (+fx vindex 1)))))))
	 ((set ?set)
	  (if (prop-info-ronly? pi)
	      (ronly-error pi a)
	      (begin
		 (prop-info-set-set! pi set)
		 (unless (prop-info-virtual? pi)
		    (prop-info-virtual?-set! pi #t)
		    (let ((o (find-property (prop-info-id pi) vproperties)))
		       (if o
			   (prop-info-vindex-set! pi (prop-info-vindex o))
			   (begin
			      (set! vprops (cons pi vprops))
			      (prop-info-vindex-set! pi vindex)
			      (set! vindex (+fx vindex 1)))))))))
	 ((info ?info)
	  (prop-info-info-set! pi info))
	 (else
	  (error/loc (prop-info-id pi) "Illegal attribute" a x))))
      
   (define (parse-property p x)
      (match-case p
	 ((?ident . ?attrs)
	  (multiple-value-bind (id type)
	     (parse-ident ident p)
	     (let ((pi (prop-info id (or type 'obj) klass #f #f #f
			  #unspecified #unspecified #unspecified
			  p -1 #f)))
		(for-each (lambda (a)
			     (parse-attribute a pi p))
		   attrs)
		pi)))
	 ((? symbol?)
	  (multiple-value-bind (id type)
	     (parse-ident p x)
	     (prop-info id (or type 'obj) klass #f #f #f
		#unspecified #unspecified #unspecified
		p -1 #f)))
	 (else
	  (error/loc klass "Illegal property" p props))))
   
   (let ((props (map (lambda (p) (parse-property p klass)) props)))
      (values props (reverse vprops))))

;*---------------------------------------------------------------------*/
;*    allocate-expand ...                                              */
;*---------------------------------------------------------------------*/
(define (allocate-expand class-info)
   `((@ class-allocate __object) ,(class-info-id class-info)))

;*---------------------------------------------------------------------*/
;*    allocator-expand ...                                             */
;*---------------------------------------------------------------------*/
(define (allocator-expand class-info mod)
   `($class-allocate ,(class-info-id class-info)
       ,@(append-map (lambda (p)
			(cond
			   ((prop-info-virtual? p)
			    '())
			   ((prop-info-defv? p)
			    (list (prop-info-value p)))
			   (else
			    (list (type-nil (prop-info-type p) mod)))))
	    (class-info-properties class-info))))

;*---------------------------------------------------------------------*/
;*    creator-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (creator-expand class-info)
   (let* ((props (filter (lambda (p)
			    (or (not (prop-info-virtual? p))
				(not (prop-info-ronly? p))))
		    (class-info-properties class-info)))
	  (targs (map (lambda (p)
			 (make-typed-ident (prop-info-id p) (prop-info-type p)))
		    props)))
      `(lambda ,targs
	  (,(make-typed-ident 'instantiate (class-info-id class-info))
	   ,@(map (lambda (p)
		     `(,(prop-info-id p) ,(prop-info-id p)))
		props))))) 

;*---------------------------------------------------------------------*/
;*    type-nil ...                                                     */
;*---------------------------------------------------------------------*/
(define (type-nil ty mod)
   (cond
      ((memq ty '(obj unspec)) #unspecified)
      ((module5-get-class mod ty) `(class-nil ,ty))
      ((or (eq? ty 'bool) (eq? ty 'bbool)) #f)
      ((eq? ty 'cell) '(make-cell #unspecified))
      ((memq ty '(bint blong int long)) 0)
      ((memq ty '(bllong belong)) '(string->llong "0"))
      ((eq? ty 'bignum) #z0)
      ((memq ty '(real double float)) 0.0)
      ((eq? ty 'bchar) #\_)
      ((memq ty '(nil pair pair-nil)) ''())
      ((eq? ty 'pair) '(econs #f #f))
      ((eq? ty 'epair) '(econs #f #f #f))
      ((eq? ty 'bstring) "")
      ((eq? ty 'symbol) ''_)
      ((eq? ty 'keyword) ':_)
      ((eq? ty 'vector) ''#())
      ((eq? ty 'procedure) 'cons)
      ((eq? ty 'input-port) '(current-input-port))
      ((eq? ty 'output-port) '(current-output-port))
      ((eq? ty 'error-port) '(current-error-port))
      ((eq? ty 'binary-port) '(current-output-port))
      ((eq? ty 'mmap) '(string->mmap ""))
      ((eq? ty 'date) '(current-date))
      ((eq? ty 'struct) `(make-struct ',(gensym) 0 #f))
      ((eq? ty 'process) '(process-nil))
      ((eq? ty 'custom) '(custom-nil))
      ((eq? ty 'opaque) '(opaque-nil))
      ((eq? ty 'socket) '(make-server-socket))
      ((eq? ty 'datagram-socket) '(make-datagram-server-socket))
      ((eq? ty 'bucs2) '(char->ucs2 #\_))
      ((eq? ty 'ucs2string) '(utf8-string->ucs2-string ""))
      ((eq? ty 'mutex) '(mutex-nil))
      ((eq? ty 'condvar) '(condition-variable-nil))
      ((eq? ty 's8vector) '(make-s8vector 0))
      ((eq? ty 'u8vector) '(make-u8vector 0))
      ((eq? ty 's16vector) '(make-s16vector 0))
      ((eq? ty 'u16vector) '(make-u16vector 0))
      ((eq? ty 's32vector) '(make-s32vector 0))
      ((eq? ty 'u32vector) '(make-u32vector 0))
      ((eq? ty 's64vector) '(make-s64vector 0))
      ((eq? ty 'u64vector) '(make-u64vector 0))
      ((eq? ty 'f32vector) '(make-f32vector 0))
      ((eq? ty 'f64vector) '(make-f64vector 0))
      (else `(cast-null ,ty))))

;*---------------------------------------------------------------------*/
;*    nil-creator-expand ...                                           */
;*---------------------------------------------------------------------*/
(define (nil-creator-expand class-info mod)
   (let* ((props (filter (lambda (p)
			    (or (not (prop-info-virtual? p))
				(not (prop-info-ronly? p))))
		    (class-info-properties class-info)))
	  (targs (map (lambda (p)
			 (make-typed-ident (prop-info-id p) (prop-info-type p)))
		    props)))
      `(lambda (,(make-typed-ident 'o (class-info-id class-info)))
	  ,@(map (lambda (p)
		    (let ((ty (prop-info-type p)))
		       `(set! (-> o ,(prop-info-id p)) ,(type-nil ty mod))))
	       props)
	  o)))

;*---------------------------------------------------------------------*/
;*    properties-expand ...                                            */
;*---------------------------------------------------------------------*/
(define (properties-expand class-info virtual?)
   
   (define to (make-typed-ident 'o (class-info-id class-info)))
   
   (define (expand-property p)
      (when (eq? (prop-info-virtual? p) virtual?)
	 (let ((ty (prop-info-type p))
	       (id (prop-info-id p)))
	    `((@ make-class-field+ __object)
	      ;; id
	      ',id
	      ;; get
	      ,(if virtual?
		   (prop-info-get p)
		   `(lambda (,to) (-> o ,id)))
	      ;; set
	      ,(cond
		   ((prop-info-ronly? p)
		    #f)
		   (virtual?
		    (prop-info-set p))
		   (else
		    (let ((tv (make-typed-ident 'v ty)))
		       `(lambda (,to ,tv)
			   (set! (-> o ,id) v)))))
	      ;; ronly
	      ,(prop-info-ronly? p)
	      ;; virtual
	      ,(prop-info-virtual? p)
	      ;; info
	      ,(prop-info-info p)
	      ;; default
	      (lambda () ,(prop-info-value p))
	      ;; type
	      ',(prop-info-type p)
	      ;; field-index
	      ,(prop-info-vindex p)))))
   `(vector
       ,@(filter-map expand-property
	    (class-info-properties class-info))))

;*---------------------------------------------------------------------*/
;*    registration-expand ...                                          */
;*---------------------------------------------------------------------*/
(define (registration-expand ci mod::Module)
   `((@ register-class! __object)
     ;; class id
     ',(class-info-id ci)
     ;; module id
     ',(-> mod id)
     ;; super class
     ,(and (class-info-super ci) (class-info-id (class-info-super ci)))
     ;; hash
     ,(get-class-hash (class-info-expr ci))
     ;; creator
     ,(creator-expand ci)
     ;; allocator
     (lambda () ,(allocator-expand ci mod))
     ;; ctor
     ,(class-info-register-ctor ci)
     ;; nil
     ,(nil-creator-expand ci mod)
     ;; shrink
     ,(when (eq? (class-info-kind ci) 'define-wide-class)
	'(lambda (o) (shrink! o)))
     ;; plain
     ,(properties-expand ci #f)
     ;; virtual
     ,(properties-expand ci #t)))
				
;*---------------------------------------------------------------------*/
;*    instantiate-expander ...                                         */
;*    -------------------------------------------------------------    */
;*    Create an instantiate expander, suitable for the interpreter     */
;*    and the compiler. Called during the expansion of a module 5.     */
;*---------------------------------------------------------------------*/
(define (instantiate-expander class-info mod::Module)
   (lambda (x e)
      (let* ((args (cdr x))
	     (o (gensym 'o))
	     (cid (class-info-id class-info))
	     (mid (-> mod id))
	     (to (make-typed-ident o cid)))
	 ;; syntactic check
	 (for-each (lambda (a)
		      (unless (match-case a (((? symbol?) ?e) #t) (else #f))
			 (error/loc (car x) "Illegal property" a (if (pair? a) a args)))
		      (unless (find (lambda (p) (eq? (prop-info-id p) (car a)))
				 (class-info-properties class-info))
			 (error/loc (car x) "Illegal property" (car a) a)))
	    args)
	 (let ((nx `(let ((,to ($class-allocate ,cid
				  ;; concrete properties
				  ,@(map (lambda (p)
					    (cond
					       ((prop-info-virtual? p)
						#f)
					       ((assq (prop-info-id p) args)
						=>
						(lambda (arg)
						   (e (cadr arg) e)))
					       ((prop-info-defv? p)
						(e (prop-info-value p) e))
					       (else
						(error/loc (car x)
						   "Property missing"
						   (prop-info-id p)
						   (cond
						      ((epair? x)
						       x)
						      ((epair? (prop-info-expr p))
						       (prop-info-expr p))
						      (else
						       x))))))
				       (filter (lambda (p)
						  (not (prop-info-virtual? p)))
					  (class-info-properties class-info))))))
		       ;; constructor
		       ,@(if (class-info-ctor class-info)
			     (list `(,(class-info-ctor class-info) ,o))
			     '())
		       ;; virtual propertys
		       ,@(filter-map (lambda (p)
					(cond
					   ((not (prop-info-virtual? p))
					    #f)
					   ((assq (prop-info-id p) args)
					    =>
					    (lambda (arg)
					       (unless (prop-info-ronly? p)
						  `(set! (-> ,o ,(prop-info-id p)) ,(e (cadr arg) e)))))))
			    (class-info-properties class-info))
		       ;; done
		       ,o)))
	    (e (localize x nx) e)))))

;*---------------------------------------------------------------------*/
;*    duplicate-expander ...                                           */
;*    -------------------------------------------------------------    */
;*    Create a duplicate expander, suitable for the interpreter        */
;*    and the compiler. Called during the expansion of a module 5.     */
;*---------------------------------------------------------------------*/
(define (duplicate-expander class-info mod::Module)
   (lambda (x e)

      (define (cast sym clazz)
	 `(,(make-typed-ident 'cast clazz) ,sym))
      
      (let* ((args (cddr x))
	     (o (gensym 'o))
	     (d (gensym 'd))
	     (cid (class-info-id class-info))
	     (mid (-> mod id))
	     (to (make-typed-ident o cid)))
	 ;; syntactic check
	 (for-each (lambda (a)
		      (unless (match-case a (((? symbol?) ?e) #t) (else #f))
			 (error/loc (car x) "Illegal property" a (if (pair? a) a args)))
		      (unless (find (lambda (p) (eq? (prop-info-id p) (car a)))
				 (class-info-properties class-info))
			 (error/loc (car x) "Illegal property" (car a) a)))
	    args)
	 (let ((nx `(let* ((,d ,(cadr x))
			   (,to ($class-allocate ,cid
				   ;; concrete properties
				   ,@(map (lambda (p)
					     (cond
						((prop-info-virtual? p)
						 #f)
						((assq (prop-info-id p) args)
						 =>
						 cadr)
						(else
						 `(-> ,(cast d (prop-info-class p)) ,(prop-info-id p)))))
					(filter (lambda (p)
						   (not (prop-info-virtual? p)))
					   (class-info-properties class-info))))))
		       ;; constructor
		       ,@(if (class-info-ctor class-info)
			     (list `(,(class-info-ctor class-info) ,o))
			     '())
		       ;; duplicated properties
		       ,@(filter-map (lambda (p)
					(cond
					   ((not (prop-info-virtual? p))
					    #f)
					   ((assq (prop-info-id p) args)
					    =>
					    (lambda (arg)
					       (unless (prop-info-ronly? p)
						  `(set! (-> ,o ,(prop-info-id p)) ,(cadr arg)))))
					   ((not (prop-info-ronly? p))
					    `(set! (-> ,o ,(prop-info-id p)) (-> ,(cast d (prop-info-class p)) ,(prop-info-id p))))))
			    (class-info-properties class-info))
		       ;; done
		       ,o)))
	    (e (localize x nx) e)))))

;*---------------------------------------------------------------------*/
;*    widen-expander ...                                               */
;*    -------------------------------------------------------------    */
;*    Create a widen expander suitable for the interpreter             */
;*    and the compiler. Called during the expansion of a module 5.     */
;*---------------------------------------------------------------------*/
(define (widen-expander class-info mod::Module)
   (lambda (x e)

      (define (cast sym clazz)
	 `(,(make-typed-ident 'cast clazz) ,sym))

      (match-case x
	 ((?- ?expr . ?args)
	  (let* ((o (gensym 'o))
		 (d (gensym 'd))
		 (cid (class-info-id class-info))
		 (mid (-> mod id)))
	     ;; syntactic check
	     (for-each (lambda (a)
			  (unless (match-case a (((? symbol?) ?e) #t) (else #f))
			     (error/loc (car x) "Illegal property" a (if (pair? a) a args)))
			  (unless (find (lambda (p) (eq? (prop-info-id p) (car a)))
				     (class-info-properties class-info))
			     (error/loc (car x) "Illegal property" (car a) a)))
		args)
	     (let ((nx `(let* ((,d ,(cadr x))
			       (,o ($class-allocate wide ,cid
				      ,@(filter-map
					   (lambda (p)
					      (cond
						 ((not (eq? (prop-info-class p) cid))
						  #f)
						 ((prop-info-virtual? p)
						  #f)
						 ((assq (prop-info-id p) args)
						  =>
						  cadr)
						 ((prop-info-defv? p)
						  (prop-info-value p))
						 (else
						  (error/loc (car x)
						     "Property missing"
						     (prop-info-id p)
						     (cond
							((epair? x)
							 x)
							((epair? (prop-info-expr p))
							 (prop-info-expr p))
							(else
							 x))))))
					   (class-info-properties class-info)))))
			   ((@ object-widening-set! __object) ,d ,o)
			   ((@ object-class-num-set! __object)
			    ,d ((@ class-num __object) ,cid))
			   ,@(if (class-info-ctor class-info)
				 (list `(,(class-info-ctor class-info) ,d))
				 '())
			   ,d)))
		(e (localize x nx) e))))
	 (else
	  (error/loc "widen!" "Illegal form" x x)))))

;*---------------------------------------------------------------------*/
;*    with-access-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (with-access-expander class-info mod::Module)
   (lambda (x e)
      (match-case x
	 ((?w ?o (and (? list?) ?bindings) . ?body)
	  (let loop ((s bindings)
		     (nfields '()))
	     (cond
		((null? s)
		 (let* ((to (gensym 'o))
			(tto (make-typed-ident to (class-info-id class-info))))
		    (localize (cdr x)
		       `(let ((,tto ,(e o e)))
			   ,(%with-lexical
			       (map car nfields)
			       (localize (cddr x) (expand-progn body))
			       (eval-begin-expander
				  (with-access-expand
				     e to nfields x))
			       to)))))
		((not (pair? s))
		 (error/loc w "Illegal field" s x))
		((symbol? (car s))
		 (loop (cdr s) (cons (list (car s) (car s)) nfields)))
		((and (pair? (car s))
		      (symbol? (car (car s)))
		      (pair? (cdr (car s)))
		      (symbol? (cadr (car s)))
		      (null? (cddr (car s))))
		 (loop (cdr s) (cons (car s) nfields)))
		(else
		 (error/loc w "Illegal form" (car s) x)))))
	 ((?w . ?rest)
	  (error/loc w "Illegal syntax" rest x)))))
					
;*---------------------------------------------------------------------*/
;*    with-access-expand ...                                           */
;*---------------------------------------------------------------------*/
(define (with-access-expand olde i fields form)
   
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
;*    co-instantiate-expander ...                                      */
;*---------------------------------------------------------------------*/
(define (co-instantiate-expander mod::Module)

   (define (instantiate-class op bdg)
      (multiple-value-bind (key klass)
	 (parse-ident op bdg)
	 (cond
	    ((not (eq? key 'instantiate))
	     (error/loc "co-instantiate" "Illegal instantiate form" op bdg))
	    ((not klass)
	     (error/loc "co-instantiate" "Illegal instantiate form" op bdg))
	    (else
	     klass))))
   
   (define (co-instantiate-expand bindings body x)
      (let ((vis (map (lambda (bdg)
			 (match-case bdg
			    (((and ?var (? symbol?)) (?op . ?args))
			     (let* ((k (instantiate-class op bdg))
				    (ci (module5-get-class mod k)))
				(if (not ci)
				    (error/loc "co-instantiate"
				       "class unbound" k bdg)
				    (vector var ci args bdg))))
			    (else
			     (error/loc "co-instantiate"
				"Wrong binding" bdg x))))
		    bindings)))
	 `(let ,(map (lambda (vi)
			(let ((v (vector-ref vi 0))
			      (ci (vector-ref vi 1))
			      (x (vector-ref vi 3)))
			   (localize x 
			      (list v (allocator-expand ci mod)))))
		   vis)
	     ;; class constructors
	     ,@(filter-map (lambda (vi)
			      (let ((v (vector-ref vi 0))
				    (ci (vector-ref vi 1)))
				 (when (class-info-ctor ci)
				    (class-info-ctor ci))))
		  vis)
	     ;; properties
	     ,@(append-map (lambda (vi)
			      (let ((v (vector-ref vi 0))
				    (ci (vector-ref vi 1))
				    (as (vector-ref vi 2))
				    (x (vector-ref vi 3)))
				 (filter-map (lambda (a)
						(match-case a
						   (((and (? symbol?) ?p) ?val)
						    (let ((cast (string->symbol (format "cast::~a" (class-info-id ci)))))
						       `(set! (-> (,cast ,v) ,p) ,val)))
						   (else
						    (error/loc "co-instantiate"
						       "Wrong instantiate form" a x))))
				    as)))
		  vis)
	     ,@body)))
   
   (lambda (x e)
      (match-case x
	 ((co-instantiate (and (? list?) ?bindings) . ?body)
	  (let ((nx (co-instantiate-expand bindings body x)))
	     (e (localize x nx) e)))
	 (else
	  (error/loc "co-instantiate" "Illegal form" x x)))))

;*---------------------------------------------------------------------*/
;*    error/loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (error/loc proc msg obj container)
   (match-case (cond
		((epair? obj) (cer obj))
		((epair? container) (cer container))
		(else #f))
      ((at ?fname ?loc) (error/location proc msg obj fname loc))
      (else (error proc msg obj))))

;*---------------------------------------------------------------------*/
;*    localize ...                                                     */
;*---------------------------------------------------------------------*/
(define (localize x nx)
   (if (or (not x) (not (epair? x)))
       nx
       (let loop ((nx nx))
	  (if (or (epair? nx) (not (pair? nx)))
	      nx
	      (econs (loop (car nx)) (loop (cdr nx)) (cer x))))))

