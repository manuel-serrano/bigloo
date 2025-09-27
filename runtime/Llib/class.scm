;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Llib/class.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 23 09:51:35 2025                          */
;*    Last change :  Sat Sep 27 14:21:57 2025 (serrano)                */
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
   
   (export (parse-class::struct ::pair)
	   (allocator-expand::pair ::struct)
	   (creator-expand::pair ::struct)
	   (nil-creator-expand::pair ::struct ::obj)
	   (properties-expand::pair ::struct ::bool)
	   (registration-expand::pair ::struct ::Module)
	   (instantiate-expander::procedure ::struct)
	   (with-access-expander::procedure ::struct)))

;*---------------------------------------------------------------------*/
;*    parse-class ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-class x::pair)
   
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
   
   (match-case x
      (((and (? class-kind?) ?kind)  ?ident (?ctor) . ?props)
       (multiple-value-bind (id super)
	  (parse-class-ident ident x)
	  (class-info id -1 super kind
	     ctor (parse-properties props id) #unspecified x)))
      (((and (? class-kind?) ?kind) ?ident . ?props)
       (multiple-value-bind (id super)
	  (parse-class-ident ident x)
	  (class-info id -1 super kind
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
	  (l (-fx (string-length s) 2)))
      (let loop ((i 0))
	 (cond
	    ((>=fx i l)
	     (values id #f))
	    ((char=? (string-ref s i) #\:)
	     (if (char=? (string-ref s (+fx i 1)) #\:)
		 (if (=fx i (-fx l 3))
		     (error/loc "parse" "Illegal identifier" id x)
		     (values (string->symbol (substring s 0 i))
			(string->symbol (substring s (+fx i 2)))))
		 (loop (+fx i 2))))
	    (else
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    parse-properties ...                                             */
;*---------------------------------------------------------------------*/
(define (parse-properties props klass)

   (define (ronly-error id a)
      (error/loc klass
	 (format "Abstract property \"~a\" is declared read-only but is given a setter"
	    id)
	 a a))
   
   (define (parse-attribute a pi x)
      (with-access::Kprop pi (id set ronly defv virtual get set value)
	 (match-case a
	    (read-only
	     (if set
		 (ronly-error id a)
		 (set! ronly #t)))
	    ((default ?val)
	     (set! defv #t)
	     (set! value val))
	    ((get ?g)
	     (set! virtual #t)
	     (set! get g))
	    ((set ?s)
	     (if ronly
		 (ronly-error id a)
		 (begin
		    (set! virtual #t)
		    (set! set s))))
	    (else
	     (error/loc id "Illegal attribute" a x)))))
      
   (define (parse-property p x)
      (match-case p
	 ((?ident . ?attrs)
	  (multiple-value-bind (id type)
	     (parse-ident ident p)
	     (let ((pi (instantiate::Kprop
			  (id id)
			  (type (or type 'obj))
			  (class klass)
			  (src p))))
		(for-each (lambda (a)
			     (parse-attribute a pi p))
		   attrs)
		pi)))
	 ((? symbol?)
	  (multiple-value-bind (id type)
	     (parse-ident p x)
	     (instantiate::Kprop
		(id id)
		(type (or type 'obj))
		(class klass)
		(src p))))
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
(define (allocator-expand class-info)
   `(lambda () ($class-allocate ,(class-info-id class-info))))

;*---------------------------------------------------------------------*/
;*    creator-expand ...                                               */
;*---------------------------------------------------------------------*/
(define (creator-expand class-info)
   (let* ((props (filter (lambda (p)
			    (with-access::Kprop p (virtual ronly)
			       (or (not virtual) (not ronly))))
		    (class-info-properties class-info)))
	  (targs (map (lambda (p)
			 (with-access::Kprop p (id type)
			    (make-typed-ident id type)))
		    props)))
      `(lambda ,targs
	  (,(make-typed-ident 'instantiate (class-info-id class-info))
	   ,@(map (lambda (p)
		     (with-access::Kprop p (id)
			`(,id ,id)))
		props))))) 
	  
;*---------------------------------------------------------------------*/
;*    nil-creator-expand ...                                           */
;*---------------------------------------------------------------------*/
(define (nil-creator-expand class-info mod)
   (let* ((props (filter (lambda (p)
			    (with-access::Kprop p (virtual ronly)
			       (or (not virtual) (not ronly))))
		    (class-info-properties class-info)))
	  (targs (map (lambda (p)
			 (with-access::Kprop p (id type)
			    (make-typed-ident id type)))
		    props)))
      `(lambda ()
	  (,(make-typed-ident 'instantiate (class-info-id class-info))
	   ,@(map (lambda (p)
		     (with-access::Kprop p (id type)
			(cond
			   ((module-get-class mod type)
			    `(,id (class-nil ,type)))
			   (else
			    `($cast-null ,id)))))
		props)))))

;*---------------------------------------------------------------------*/
;*    properties-expand ...                                            */
;*---------------------------------------------------------------------*/
(define (properties-expand class-info virtual?)
   (let ((to (make-typed-ident 'o (class-info-id class-info))))
      `(vector
	  ,@(filter-map (lambda (p)
			   (with-access::Kprop p (virtual ronly id type value)
			      (when (eq? virtual virtual?)
				 `((@ make-class-field __object)
				   ;; id
				   ',id
				   ;; get
				   (lambda (,to) (-> o ,id))
				   ;; set
				   ,@(if ronly
					 '()
					 (let ((tv (make-typed-ident 'v type)))
					    (list `(lambda (,to ,tv)
						      (set! (-> o ,id) v)))))
				   ;; ronly
				   ,ronly
				   ;; virtual
				   ,virtual
				   ;; info
				   #f
				   ;; default
				   (lambda () ,value)
				   ;; type
				   ',type))))
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
     ,(get-class-hash (class-info-src ci))
     ;; creator
     ,(creator-expand ci)
     ;; allocator
     ,(allocator-expand ci)
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
					 (with-access::Kprop p (id virtual defv value src)
					    (cond
					       (virtual
						#f)
					       ((assq id args)
						=>
						(lambda (arg)
						   (e (cadr arg) e)))
					       (defv
						(e value e))
					       (else
						(error/loc (car x)
						   "Property missing"
						   id
						   src)))))
			     (class-info-properties class-info)))))
	     ;; constructor
	     ,@(if (class-info-ctor class-info)
		   (list (class-info-ctor class-info))
		   '())
	     ;; virtual propertys
	     ,@(filter-map (lambda (p)
			      (with-access::Kprop p (virtual id)
				 (cond
				    ((not virtual)
				     #f)
				    ((assq id args)
				     =>
				     (lambda (arg)
					`(class-instance-virtual-property-set! ,o
					    (e ,(cadr arg) e)))))))
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

