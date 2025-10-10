;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Llib/class.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 23 09:51:35 2025                          */
;*    Last change :  Fri Oct 10 05:13:54 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
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
	   (instantiate-expander::procedure ::struct)
	   (with-access-expander::procedure ::struct)
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

   (define (class-depth k)
      (if (not k)
	  0
	  (let ((ci (module-get-class mod k)))
	     (if (not ci)
		 (error/loc "parse-class"
		    "Cannot find super class" x x)
		 (+fx 1 (class-info-depth ci))))))
   
   (match-case x
      (((and (? class-kind?) ?kind)  ?ident (?ctor) . ?props)
       (multiple-value-bind (id super)
	  (parse-class-ident ident x)
	  (class-info id (class-depth super) super kind
	     ctor (parse-properties props id) #unspecified x)))
      (((and (? class-kind?) ?kind) ?ident . ?props)
       (multiple-value-bind (id super)
	  (parse-class-ident ident x)
	  (class-info id (class-depth super) super kind
	     #f (parse-properties props id) #unspecified x)))
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
(define (parse-properties props klass)

   (define (ronly-error pi a)
      (error/loc klass
	 (format "Abstract property \"~a\" is declared read-only but is given a setter"
	    (prop-info-id pi))
	 a a))
   
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
	  (prop-info-virtual?-set! pi #t)
	  (prop-info-get-set! pi get))
	 ((set ?set)
	  (if (prop-info-ronly? pi)
	      (ronly-error pi a)
	      (begin
		 (prop-info-virtual?-set! pi #t)
		 (prop-info-get-set! pi set))))
	 (else
	  (error/loc (prop-info-id pi) "Illegal attribute" a x))))
      
   (define (parse-property p x)
      (match-case p
	 ((?ident . ?attrs)
	  (multiple-value-bind (id type)
	     (parse-ident ident p)
	     (let ((pi (prop-info id (or type 'obj) klass #f #f #f
			  #unspecified #unspecified #unspecified
			  p)))
		(for-each (lambda (a)
			     (parse-attribute a pi p))
		   attrs)
		pi)))
	 ((? symbol?)
	  (multiple-value-bind (id type)
	     (parse-ident p x)
	     (prop-info id (or type 'obj) klass #f #f #f
		#unspecified #unspecified #unspecified
		p)))
	 (else
	  (error/loc klass "Illegal property" p props))))
   
   (map (lambda (p) (parse-property p klass)) props))

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
       ,@(filter-map (lambda (p)
			(cond
			   ((prop-info-virtual? p)
			    #f)
			   ((prop-info-defv? p)
			    (prop-info-value p))
			   (else
			    (let ((ty (prop-info-type p)))
			       `(set! (-> o ,(prop-info-id p))
				   ,(cond
				       ((module-get-class mod ty)
					`(class-nil ,ty))
				       (else
					`(cast-null ,(prop-info-type p)))))))))
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
		       `(set! (-> o ,(prop-info-id p))
			   ,(cond
			      ((module-get-class mod ty)
			       `(class-nil ,ty))
			      (else
			       `(cast-null ,(prop-info-type p)))))))
	       props)
	  o)))

;*---------------------------------------------------------------------*/
;*    properties-expand ...                                            */
;*---------------------------------------------------------------------*/
(define (properties-expand class-info virtual?)
   (let ((to (make-typed-ident 'o (class-info-id class-info))))
      `(vector
	  ,@(filter-map (lambda (p)
			   (when (eq? (prop-info-virtual? p) virtual?)
			      (let ((ty (prop-info-type p))
				    (id (prop-info-id p)))
				 `((@ make-class-field __object)
				   ;; id
				   ',id
				   ;; get
				   (lambda (,to) (-> o ,id))
				   ;; set
				   ,@(if (prop-info-ronly? p)
					 '()
					 (let ((tv (make-typed-ident 'v ty)))
					    (list `(lambda (,to ,tv)
						      (set! (-> o ,id) v)))))
				   ;; ronly
				   ,(prop-info-ronly? p)
				   ;; virtual
				   ,(prop-info-virtual? p)
				   ;; info
				   #f
				   ;; default
				   (lambda () ,(prop-info-value p))
				   ;; type
				   ',(prop-info-type p)))))
	       (class-info-properties class-info)))))

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
     ,(lambda () (allocator-expand ci mod))
     ;; ctor
     ,(class-info-ctor ci)
     ;; nil
     ,(nil-creator-expand ci mod)
     ;; shrink
     #f
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
(define (instantiate-expander class-info)
   (lambda (x e)
      (let* ((args (cdr x))
	     (o (gensym 'o))
	     (to (make-typed-ident o (class-info-id class-info))))
	 ;; syntactic check
	 (for-each (lambda (p)
		      (unless (match-case p (((? symbol?) ?e) #t) (else #f))
			 (error/loc (car x) "Illegal property" p args)))
	    args)
	 `(let ((,to ($class-allocate ,(class-info-id class-info)
			;; concrete properties
			,@(filter-map (lambda (p)
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
						(prop-info-expr p)))))
			     (class-info-properties class-info)))))
	     ;; constructor
	     ,@(if (class-info-ctor class-info)
		   (list (class-info-ctor class-info))
		   '())
	     ;; virtual propertys
	     ,@(filter-map (lambda (p)
			      (cond
				 ((not (prop-info-virtual? p))
				  #f)
				 ((assq (prop-info-id p) args)
				  =>
				  (lambda (arg)
				     `(class-instance-virtual-property-set! ,o
					(e ,(cadr arg) e))))))
		  (class-info-properties class-info))
	     ;; done
	     ,o))))

;*---------------------------------------------------------------------*/
;*    with-access-expander ...                                         */
;*---------------------------------------------------------------------*/
(define (with-access-expander class-info)
   (lambda (x e)
      (match-case x
	 ((?w ?o (and (? list?) ?bindings) . ?body)
	  (let loop ((s bindings)
		     (nfields '()))
	     (cond
		((null? s)
		 (let* ((to (gensym 'o))
			(tto (make-typed-ident to (class-info-id class-info))))
		    (localize x
		       `(let ((,tto ,(e o e)))
			   ,(%with-lexical
			       (map car nfields)
			       (expand-progn body)
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
				    (ci (module-get-class mod k)))
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
						    `(set! (-> ,v ,p) ,val))
						   (else
						    (error/loc "co-instantiate"
						       "Wrong instantiate form" a x))))
				    as)))
		  vis)
	     ,@body)))
   
   (lambda (x e)
      (match-case x
	 ((co-instantiate (and (? list?) ?bindings) . ?body)
	  (e (co-instantiate-expand bindings body x) e))
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

