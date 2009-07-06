;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/creator.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Sun Mar 30 07:12:56 2008 (serrano)                */
;*    Copyright   :  1996-2008 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the class constructors                                   */
;*    -------------------------------------------------------------    */
;*    In this module we cannot use consume-module-clause! because      */
;*    the importation are already done.                                */
;*    -------------------------------------------------------------    */
;*    This constructors does not require any importation information   */
;*    since all accessors are always static.                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_creator
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    tools_location
	    tools_shape
	    engine_param
	    backend_backend
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    ast_private
	    object_class
	    object_struct
	    object_slots
	    object_tools
	    object_nil
	    module_module
	    module_impuse)
   (export  (gen-plain-class-creators ::tclass ::pair ::symbol)
	    (gen-wide-class-creators ::tclass ::pair ::symbol)
	    (import-plain-class-creators ::tclass ::pair ::symbol)
	    (import-wide-class-creators ::tclass ::pair ::symbol)
	    (import-java-class-creator ::jclass ::pair-nil ::pair)
	    (gen-java-class-constructors ::jclass ::pair-nil ::pair)))

;*---------------------------------------------------------------------*/
;*    inline-creators? ...                                             */
;*    -------------------------------------------------------------    */
;*    This function returns #t if the class creatorss must be          */
;*    inlined.                                                         */
;*---------------------------------------------------------------------*/
(define (inline-creators?)
   (and (>=fx *optim* 2)
	(or (not (number? *profile-mode*))
	    (<fx *profile-mode* 1))
	(not (memq *pass* '(make-heap make-add-heap)))))
   
;*---------------------------------------------------------------------*/
;*    gen-plain-class-creators ...                                     */
;*---------------------------------------------------------------------*/
(define (gen-plain-class-creators class src-def import)
   (append (gen-noinline-plain-class-creators class src-def import)
	   (gen-inline-plain-class-creators class src-def import)
	   (gen-plain-class-nil class src-def import)))

;*---------------------------------------------------------------------*/
;*    gen-inline-plain-class-creators ...                              */
;*    -------------------------------------------------------------    */
;*    Even abstract class have allocators and fillers. These are       */
;*    used when -nil instance is constructed.                          */
;*---------------------------------------------------------------------*/
(define (gen-inline-plain-class-creators class src-def import)
   (with-access::tclass class (abstract?)
      (let* ((id (tclass-id class))
	     (holder (tclass-holder class))
	     (fill-id (symbol-append 'fill- id '!)))
	 (list (gen-class-fill! fill-id id class src-def import)
	       (gen-class-allocate! id class holder src-def import)))))

;*---------------------------------------------------------------------*/
;*    gen-noinline-plain-class-creators ...                            */
;*---------------------------------------------------------------------*/
(define (gen-noinline-plain-class-creators class src-def import)
   (with-access::tclass class (abstract?)
      (if abstract?
	  '()
	  (let* ((id (tclass-id class))
		 (mk-id (symbol-append 'make- id)))
	     (list (gen-class-make! 'make mk-id id class src-def import))))))

;*---------------------------------------------------------------------*/
;*    gen-wide-class-creators ...                                      */
;*---------------------------------------------------------------------*/
(define (gen-wide-class-creators class src-def import)
   (append (gen-noinline-wide-class-creators class src-def import)
	   (gen-inline-wide-class-creators class src-def import)
	   (gen-wide-class-nil class src-def import)))
 
;*---------------------------------------------------------------------*/
;*    gen-noinline-wide-class-creators ...                             */
;*---------------------------------------------------------------------*/
(define (gen-noinline-wide-class-creators class src-def import)
   (with-access::tclass class (id holder widening abstract?)
      (if abstract?
	  '()
	  (let ((fill-id (symbol-append 'fill- id '!))
		(widening (symbol-append widening '- id))
		(super (tclass-its-super class)))
	     (list (gen-class-make! 'widening widening id class src-def import)
		   (gen-wide-class-make! id class src-def import))))))

;*---------------------------------------------------------------------*/
;*    gen-inline-wide-class-creators ...                               */
;*---------------------------------------------------------------------*/
(define (gen-inline-wide-class-creators class src-def import)
   (with-access::tclass class (id holder widening abstract?)
      (if abstract?
	  '()
	  (let ((fill-id (symbol-append 'fill- id '!))
		(widening (symbol-append widening '- id))
		(super (tclass-its-super class)))
	     (list (gen-class-fill! fill-id id class src-def import)
		   (gen-class-allocate! id super holder src-def import))))))

;*---------------------------------------------------------------------*/
;*    gen-java-class-creator ...                                       */
;*---------------------------------------------------------------------*/
(define (gen-java-class-creator class src-def)
   (let* ((id (jclass-id class))
	  (alloc-id (symbol-append '%allocate- id)))
      (define (jvm-allocate)
	 `(define-inline (,alloc-id)
	     ,(make-private-sexp 'new id)))
      (produce-module-clause! `(static (inline ,alloc-id)))
      (list (epairify* (jvm-allocate) src-def))))

;*---------------------------------------------------------------------*/
;*    import-plain-class-creators ...                                  */
;*---------------------------------------------------------------------*/
(define (import-plain-class-creators class src-def module)
   (import-class-nil class src-def module)
   (if (tclass-abstract? class)
       '()
       (if (inline-creators?)
	   ;; if we compile for optimization, we inline class creators
	   (begin
	      (import-noinline-plain-class-creators class src-def module)
	      (gen-inline-plain-class-creators class src-def 'static))
	   (begin
	      (import-noinline-plain-class-creators class src-def module)
	      (import-inline-plain-class-creators class src-def module)))))

;*---------------------------------------------------------------------*/
;*    import-inline-plain-class-creators ...                           */
;*---------------------------------------------------------------------*/
(define (import-inline-plain-class-creators class src-def module)
   (with-access::tclass class (id slots)
      (let* ((mk-id (symbol-append 'make- id))
	     (fill-id (symbol-append 'fill- id '!))
	     (mk-tid (make-typed-ident mk-id id))
	     (f-ids  (make-class-make-formals slots))
	     (f-tids (make-class-make-typed-formals f-ids slots))
	     (to (make-typed-ident (mark-symbol-non-user! (gensym 'o)) id))
	     (alloc-id (symbol-append '%allocate- id))
	     (alloc-tid (make-typed-ident alloc-id id)))
	 ;; the module clause of the maker
	 (import-parser module `(,alloc-tid))
	 (import-parser module `(,fill-id ,to ,@f-tids))
	 ;; and no code as result
	 '())))

;*---------------------------------------------------------------------*/
;*    import-noinline-plain-class-creators ...                         */
;*---------------------------------------------------------------------*/
(define (import-noinline-plain-class-creators class src-def module)
   (with-access::tclass class (id slots)
      (let* ((mk-id (symbol-append 'make- id))
	     (mk-tid (make-typed-ident mk-id id))
	     (f-ids  (make-class-make-formals slots))
	     (f-tids (make-class-make-typed-formals f-ids slots)))
	 ;; the module clause of the maker
	 (import-parser module `(,mk-tid ,@f-tids))
	 ;; and no code as result
	 '())))

;*---------------------------------------------------------------------*/
;*    import-wide-class-creators ...                                   */
;*---------------------------------------------------------------------*/
(define (import-wide-class-creators class src-def module)
   (import-class-nil class src-def module)
   (if (tclass-abstract? class)
       '()
       (if (inline-creators?)
	   ;; if we compile for optimization, we inline class creators
	   (begin
	      (import-noinline-wide-class-creators class src-def module)
	      (gen-inline-wide-class-creators class src-def 'static))
	   (begin
	      (import-noinline-wide-class-creators class src-def module)
	      (import-inline-wide-class-creators class src-def module)))))

;*---------------------------------------------------------------------*/
;*    import-inline-wide-class-creators ...                            */
;*---------------------------------------------------------------------*/
(define (import-inline-wide-class-creators class src-def module)
   (with-access::tclass class (id slots its-super)
      (let* ((mk-id (symbol-append 'make- id))
	     (fill-id (symbol-append 'fill- id '!))
	     (mk-tid (make-typed-ident mk-id id))
	     (alloc-wide (symbol-append 'widening- id))
	     (talloc-wide (make-typed-ident alloc-wide id))
	     (f-ids (make-class-make-formals slots))
	     (f-tids (make-class-make-typed-formals f-ids slots))
	     (to (make-typed-ident (mark-symbol-non-user! (gensym 'o)) id))
	     (alloc-id (symbol-append '%allocate- id))
	     (alloc-tid (make-typed-ident alloc-id id)))
	 ;; the module clause of the maker
	 (import-parser module `(,alloc-tid))
	 (import-parser module `(,fill-id ,to ,@f-tids))
	 ;; and no code as result
	 '())))

;*---------------------------------------------------------------------*/
;*    import-noinline-wide-class-creators ...                          */
;*---------------------------------------------------------------------*/
(define (import-noinline-wide-class-creators class src-def module)
   (with-access::tclass class (id slots its-super)
      (let* ((mk-id (symbol-append 'make- id))
	     (fill-id (symbol-append 'fill- id '!))
	     (mk-tid (make-typed-ident mk-id id))
	     (alloc-wide (symbol-append 'widening- id))
	     ;; Obj is a rough approximation. It should be id
	     ;; This must be consistent with GEN-CLASS-MAKE!
	     ;; @ref creator.scm:Widening type declaration@
	     ;; @label Widening type import@
	     (tid (if *saw* (saw-wide-class-id id) 'obj))
	     (talloc-wide (make-typed-ident alloc-wide tid))
	     (f-ids (make-class-make-formals slots))
	     (f-tids (make-class-make-typed-formals f-ids slots))
	     (sslots (tclass-slots its-super))
	     (s-ids (make-class-make-formals sslots))
	     (s-tids (make-class-make-typed-formals s-ids sslots)))
	 ;; the module clause of the maker
	 (import-parser module `(,mk-tid ,@s-tids ,@f-tids))
	 (import-parser module `(,talloc-wide ,@f-tids))
	 ;; and no code as result
	 '())))

;*---------------------------------------------------------------------*/
;*    import-java-class-creator ...                                    */
;*---------------------------------------------------------------------*/
(define (import-java-class-creator class constrs src-def)
   (if (pair? constrs)
       (gen-java-class-creator class src-def)
       '()))

;*---------------------------------------------------------------------*/
;*    gen-java-class-constructors ...                                  */
;*---------------------------------------------------------------------*/
(define (gen-java-class-constructors class constrs src-def)
   (define (gen-one-constructor constr)
      (match-case constr
	 ((?ident . ?args-type)
	  ;; if we have reached that point, it means that the class
	  ;; declaration has alreay been enterily parsed so there is no need
	  ;; now to make any user correctness check
	  (let* ((jid (jclass-id class))
		 (loc (find-location constrs))
		 (cid (fast-id-of-id ident loc))
		 (tid (make-typed-ident (symbol-append jid '- cid) jid))
		 (args-id (map (lambda (_) (mark-symbol-non-user! (gensym)))
			       args-type))
		 (self (mark-symbol-non-user! (gensym)))
		 (tself (make-typed-ident self jid))
		 (targs (map (lambda (id type)
				(symbol-append id type))
			     args-id args-type))
		 (def `(define (,tid ,@targs)
			      ;; call the actual Java constructor
			      ,(apply make-private-sexp 'new jid
				      `',(map (lambda (a)
						 (type-id
						  (type-of-id a loc)))
					      args-type)
				      args-id)))
		 (scope (if (inline-creators?) 'static 'export))
		 (mod `(,scope (,tid ,@args-type))))
	     (produce-module-clause! mod)
	     (epairify* def constr)))))
   (map gen-one-constructor constrs))
   
;*---------------------------------------------------------------------*/
;*    gen-class-fill! ...                                              */
;*    -------------------------------------------------------------    */
;*    The class predicate is not inlined when compiling for            */
;*    profiling.                                                       */
;*---------------------------------------------------------------------*/
(define (gen-class-fill! fill-id::symbol id type src-def import)
   (let* ((tid      (if (and (wide-class? type) *saw*)
			(saw-wide-class-id (type-id type))
			(type-id type)))
	  (slots    (tclass-slots type))
	  (fill-tid (make-typed-ident fill-id tid))
	  (f-ids    (make-class-make-formals slots))
	  (f-tids   (make-class-make-typed-formals f-ids slots))
	  (new      (mark-symbol-non-user! (gensym 'new)))
	  (tnew     (if (and (wide-class? type)
			     (backend-typed (the-backend)))
			(make-typed-ident new 'obj)
			(make-typed-ident new tid)))
	  (rid      (mark-symbol-non-user! (gensym 'i)))
	  (rtid     (symbol-append rid '::long)))
      ;; the body of the filler
      (define (make-body)
	 (let* ((tid (type-id type))
		(alloc-id (symbol-append '%allocate- id)))
	    `(let ,(map (lambda (ft f) `(,ft ,f)) f-tids f-ids)
		,@(make-class-slot-make! type new rid rtid slots f-ids)
		,new)))
      ;; the definition of the maker. The maker is defined inline in
      ;; optimizing and non profiling mode only
      (let ((proto (cons* fill-tid tnew f-tids))
	    (body (make-body)))
	 ;; the module clause of the filler
	 (produce-module-clause! `(,import (inline ,@proto)))
	 ;; the body
	 (epairify* `(define-inline ,proto ,body) src-def))))

;*---------------------------------------------------------------------*/
;*    gen-class-make! ...                                              */
;*    -------------------------------------------------------------    */
;*    The class constructor is NEVER inlined.                          */
;*---------------------------------------------------------------------*/
(define (gen-class-make! widening mk-id::symbol id type src-def import)
   (let* ((tid     (if (and (eq? widening 'widening)
			    (wide-class? type)
			    *saw*)
		       (saw-wide-class-id (type-id type))
		       (type-id type)))
	  (slots   (tclass-slots type))
	  (holder  (tclass-holder type))
	  (constrs (find-class-constructors type))
	  ;; @label Widening type declaration@
	  ;; @ref creator.scm:Widening type import@
	  (mk-tid  (make-typed-ident mk-id tid))
	  (fill-id (symbol-append 'fill- id '!))
	  (f-ids   (make-class-make-formals slots))
	  (f-tids  (make-class-make-typed-formals f-ids slots))
	  (new     (mark-symbol-non-user! (gensym 'new)))
	  (rid     (mark-symbol-non-user! (gensym 'i)))
	  (rtid    (symbol-append rid '::long)))
      (define (alloc-widening)
	 (define (pragma-allocate)
	    (let ((tname (string-sans-$ (type-name type)))
		  (sizeof (if (string? (type-size type))
			      (type-size type)
			      (type-name type))))
	       `(,(make-typed-ident 'free-pragma tid)
		 ,(string-append "((" tname
				 ")BREF( GC_MALLOC (sizeof(" sizeof "))))"))))
	 (define (nopragma-allocate)
	    (make-private-sexp 'new tid))
	 (if (backend-pragma-support (the-backend))
	     (pragma-allocate)
	     (nopragma-allocate)))
      (define (make-body.debug)
	 (let* ((alloc-id (symbol-append '%allocate- id)))
	    `(let ((,(make-typed-ident new tid)
		    ,(if (not (eq? widening 'widening))
			 `(,alloc-id)
			 (alloc-widening))))
		(begin
		   ;; we call the inlined filler 
		   (,fill-id ,new ,@f-ids)
		   ;; then we add the class constructors
		   ,@(if (and (pair? constrs) (not (eq? widening 'widening)))
			 (map (lambda (constr) `(,constr ,new)) constrs)
			 '())
		   ,new))))
      (define (make-body.sans-debug)
	 (let* ((alloc-id (symbol-append '%allocate- id)))
	    `(let ((,(make-typed-ident new tid)
		    ,(if (not (eq? widening 'widening))
			 `(,alloc-id)
			 (alloc-widening))))
		(begin
		   ;; we call the inlined filler 
		   (,fill-id ,new ,@f-ids)
		   ;; then we add the class constructors
		   ,@(if (and (pair? constrs) (not (eq? widening 'widening)))
			 (map (lambda (constr) `(,constr ,new)) constrs)
			 '())
		   ,new))))
      (define (make-body)
	 (if (eq? (backend-language (the-backend)) 'jvm)
	     (make-body.debug)
	     (make-body.sans-debug)))
      ;; the module clause of the maker
      (produce-module-clause! `(,import (,mk-tid ,@f-tids)))
      (produce-module-clause! `(pragma (,mk-id allocator)))
      ;; the definition of the maker. 
      (let ((proto (cons mk-tid f-tids))
	    (body (make-body)))
	 (epairify* `(define ,proto ,body) src-def))))

;*---------------------------------------------------------------------*/
;*    make-class-slot-make! ...                                        */
;*---------------------------------------------------------------------*/
(define (make-class-slot-make! type new rid rtid slots f-ids)
   (define (make-class-slot slot formal flen)
      (let ((loop (mark-symbol-non-user! (gensym 'loop))))
	 (cond
	    ((slot-indexed slot)
	     ;; for an indexed field we have to make a
	     ;; malloc call and then to fill all the field slots
	     `(begin
		 ,(make-pragma-indexed-init-set! type slot new flen)
		 ;; this loop fills the field slots
		 (labels ((,loop (,rtid)
				 (if (=fx ,rid ,flen)
				     'done
				     (begin
					,(make-pragma-indexed-set!/widening
					  type
					  slot
					  new
					  formal
					  rid
					  #f)
					(,loop (+fx ,rid 1))))))
		    (,loop 0))))
	    (else
	     (make-pragma-direct-set! type slot new formal)))))
   (let loop ((slots slots)
	      (f-ids f-ids)
	      (res   '())
	      (len-id #unspecified))
      (cond
	 ((null? slots)
	  (reverse! res))
	 ((slot-virtual? (car slots))
	  (loop (cdr slots) f-ids res len-id))
	 (else
	  (loop (cdr slots)
		(cdr f-ids)
		(cons (make-class-slot (car slots) (car f-ids) len-id) res)
		(car f-ids))))))

;*---------------------------------------------------------------------*/
;*    gen-class-allocate! ...                                          */
;*---------------------------------------------------------------------*/
(define (gen-class-allocate! id type holder src-def import)
   (let* ((tid       (type-id type))
	  (alloc-id  (symbol-append '%allocate- id))
	  (alloc-tid (make-typed-ident alloc-id tid))
	  (new       (mark-symbol-non-user! (gensym 'new))))
      (define (c-malloc type)
	 (let ((tid    (type-id type))
	       (tname  (string-sans-$ (type-name type)))
	       (sizeof (if (string? (type-size type))
			   (type-size type)
			   (type-name type))))
	    `(,(make-typed-ident 'free-pragma tid)
	      ,(string-append "((" tname
			      ")BREF( GC_MALLOC ( sizeof(" sizeof ") )))"))))
      (define (pragma-allocate)
	 `(define-inline (,alloc-tid)
	     (let ((,(make-typed-ident new tid) ,(c-malloc type)))
		(object-class-num-set! ,new
				       ((@ class-num __object)
					(@ ,(global-id holder)
					   ,(global-module holder))))
		(object-widening-set! ,new #f)
		,new)))
      (define (nopragma-allocate)
	 `(define-inline (,alloc-tid)
	     (let ((,(make-typed-ident new tid)
		    ,(make-private-sexp 'new tid)))
		(object-class-num-set! ,new
				       ((@ class-num __object)
					(@ ,(global-id holder)
					   ,(global-module holder))))
		(object-widening-set! ,new #f)
		,new)))
      ;; in all cases, produce an parse an import clause
      (produce-module-clause! `(,import (inline ,alloc-tid)))
      (epairify* (if (backend-pragma-support (the-backend))
		     (pragma-allocate)
		     (nopragma-allocate))
		 src-def)))

;*---------------------------------------------------------------------*/
;*    gen-wide-class-make! ...                                         */
;*---------------------------------------------------------------------*/
(define (gen-wide-class-make! id type src-def import)
   (let* ((mk-heap-id  (symbol-append 'make- id))
	  (super       (tclass-its-super type))
	  (slots       (tclass-slots type))
	  (sslots      (tclass-slots super))
	  (holder      (tclass-holder type))
	  (constrs     (find-class-constructors type))
	  (tid         (type-id type))
	  (stid        (type-id super))
	  (mk-tid      (make-typed-ident (symbol-append 'make- id) tid))
	  (f-ids       (make-class-make-formals slots))
	  (sf-ids      (make-class-make-formals sslots))
	  (f-tids      (make-class-make-typed-formals f-ids slots))
	  (sf-tids     (make-class-make-typed-formals sf-ids sslots))
	  (widening    (symbol-append
			(tclass-widening type) '- (type-id type)))
	  (aux         (mark-symbol-non-user! (gensym 'aux)))
	  (new         (mark-symbol-non-user! (gensym 'new)))
	  (mk-class-id (symbol-append 'make- stid)))
      ;; the module clause of the maker
      (produce-module-clause! `(,import (,mk-tid ,@sf-tids ,@f-tids)))
      ;; the definition of the maker
      (epairify* `(define (,mk-tid ,@sf-tids ,@f-tids)
		   ;; we make the allocation in several times:
		   ;; 1- we allocate a super object ...
		   (let ((,(make-typed-ident aux stid)
			  (,mk-class-id ,@sf-ids)))
		      ;; 2- we create a variable of type type aliased
		      ;; to the super object ...
		      ;; 3- we set the class number of the new object ...
		      (object-class-num-set!
		       ,aux
		       (class-num (@ ,(global-id holder)
				     ,(global-module holder))))
		      ;; 4- we set the widening property ...
		      (object-widening-set! ,aux (,widening ,@f-ids))
		      ;; 5- we mark the object (in bactracking compilation)
		      (let ((,(make-typed-ident new tid)
			     ,(make-private-sexp 'cast tid aux)))
			 ;; 6- if there is a constructor for that
			 ;; object we call it
			 ,@(if (and (pair? constrs)
				    (not (eq? widening 'widening)))
			       (map (lambda (constr) `(,constr ,new)) constrs)
			       '())
			 ;; 6- we return the object
			 ,new)))
		 src-def)))
