;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/class.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 10:52:20 1996                          */
;*    Last change :  Mon Nov 29 09:41:28 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The class clause handling                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_class
   (include "Ast/unit.sch"
	    "Tools/trace.sch")
   (import  module_module
	    module_impuse
	    module_include
	    backend_backend
	    engine_param
	    tools_shape
	    tools_error
	    tools_location
	    type_type
	    type_env
	    ast_ident
	    ast_var
	    ast_env
	    object_class
	    object_plain-access
	    object_wide-access)
   (export  (declare-class! ::pair ::symbol ::symbol ::bool ::bool ::obj ::obj)
	    (declare-wide-class! ::pair ::symbol ::symbol ::obj ::obj)
	    (get-class-hash ::symbol ::pair-nil)
	    (get-object-unit)
	    (get-method-unit)
	    (get-generic-unit)
	    (class-finalizer)
	    (class-finalizer-add-static!)
	    (delay-class-accessors!::type ::type delayed)))

;*---------------------------------------------------------------------*/
;*    Object units ...                                                 */
;*---------------------------------------------------------------------*/
(define *object-unit* #unspecified)
(define *method-unit* #unspecified)
(define *generic-unit* #unspecified)

;*---------------------------------------------------------------------*/
;*    get-object-unit ...                                              */
;*---------------------------------------------------------------------*/
(define (get-object-unit)
   *object-unit*)

;*---------------------------------------------------------------------*/
;*    get-method-unit ...                                              */
;*---------------------------------------------------------------------*/
(define (get-method-unit)
   *method-unit*)
 
;*---------------------------------------------------------------------*/
;*    get-generic-unit ...                                             */
;*---------------------------------------------------------------------*/
(define (get-generic-unit)
   *generic-unit*)
 
;*---------------------------------------------------------------------*/
;*    declare-class! ...                                               */
;*---------------------------------------------------------------------*/
(define (declare-class! class-def module import final? abstract? def-src decl-src)
   (cond
      ((memq import '(export static))
       (declare-export-class! gen-plain-class-accessors!
			      class-def module
			      (if final? 'final 'plain)
			      abstract?
			      def-src decl-src import))
      (else
       (declare-import-class! import-plain-class-accessors!
			      class-def module
			      (if final? 'final 'plain)
			      abstract?
			      def-src decl-src))))

;*---------------------------------------------------------------------*/
;*    declare-wide-class! ...                                          */
;*---------------------------------------------------------------------*/
(define (declare-wide-class! class-def module import def-src decl-src)
   (cond
      ((memq import '(export static))
       (declare-export-class! gen-wide-class-accessors!
			      class-def module 'wide #f
			      def-src decl-src import))
      (else
       (declare-import-class! import-wide-class-accessors!
			      class-def module 'wide #f
			      def-src decl-src))))

;*---------------------------------------------------------------------*/
;*    declare-export-class! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-export-class! gen cdef module kind abstract? src-def src-decl import)
   (trace (ast 2) "declare-export-class!: " src-def #\Newline)
   ;; We create the class holder
   ;; and we create a type for this class
   (let* ((loc (find-location src-def))
	  (class-var (car cdef))
	  (class-id  (id-of-id class-var loc))
	  (holder (begin
	 	     (produce-module-clause! `(,import ,class-id))
		     (find-global/module class-id module)))
	  (final? (eq? kind 'final))
	  (wide (if (eq? kind 'wide) 'widening #f))
	  (tclass (declare-class-type! cdef holder wide
				       final? abstract?
				       src-def)))
      ;; debug information
      (global-src-set! holder src-def)
      ;; some paranoid checking
      [assert (tclass) (tclass? tclass)]
      ;; we store the src-import location in order to print a nice error
      ;; message if that tclass is not defined
      (type-import-location-set! tclass (find-location/loc src-decl loc))
      ;; tclass can be something else than a class if an error has been found
      (delay-class-accessors!
       tclass
       (delay
	  (multiple-value-bind (concretes virtuals)
	     (gen cdef tclass src-def import)
	     (trace (ast 3)
		    "declare-class! (export static): "#\Newline
		    "    concretes: " concretes #\Newline
		    "     virtuals: " virtuals #\Newline)
	     ;; this is a domestic class, we have to declare the
	     ;; global variable that holds the class object
	     (make-add-class! holder tclass src-def virtuals)
	     ;; we return the list of concretes
	     concretes)))))
   
;*---------------------------------------------------------------------*/
;*    declare-import-class! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-import-class! gen cdef module kind abstract? src-def src-decl)
   (trace (ast 2) "declare-import-class!: " src-def #\Newline)
   ;; We create the class holder
   ;; and we create a type for this class
   (let* ((loc (find-location src-def))
	  (class-var (car cdef))
	  (class-id (id-of-id class-var loc))
	  (holder (import-parser module class-id))
	  (final? (eq? kind 'final))
	  (wide (if (eq? kind 'wide) 'widening #f))
	  (tclass (declare-class-type! cdef holder wide
				       final? abstract?
				       src-def)))
      ;; some paranoid checking
      [assert (tclass) (tclass? tclass)]
      ;; we store the src-import location in order to print a nice error
      ;; message if that tclass is not defined
      (type-import-location-set! tclass (find-location/loc src-decl loc))
      ;; when importing a class, we import the accessors...
      (delay-class-accessors!
       tclass
       (delay
	  (multiple-value-bind (concretes virtuals)
	     (gen cdef tclass src-def module)
	     concretes)))))

;*---------------------------------------------------------------------*/
;*    delay-class-accessors! ...                                       */
;*---------------------------------------------------------------------*/
(define (delay-class-accessors!::type type::type delayed)
   (set! *class-accesses* (cons (cons type delayed) *class-accesses*))
   type)

;*---------------------------------------------------------------------*/
;*    *class-accesses* ...                                             */
;*---------------------------------------------------------------------*/
(define *class-accesses* '())

;*---------------------------------------------------------------------*/
;*    *declared-classes* ...                                           */
;*---------------------------------------------------------------------*/
(define *declared-classes* '())

;*---------------------------------------------------------------------*/
;*    make-add-class! ...                                              */
;*---------------------------------------------------------------------*/
(define (make-add-class! holder class src-def virtuals)
   (let* ((super          (tclass-its-super class))
	  (holder-id      (global-id holder))
	  (class-id       (type-id class))
	  (class-module   (global-module holder))
	  (class-make-id  (class-make class))
	  (class-alloc-id (symbol-append '%allocate- class-id))
	  (class-make     (cond
			     ((tclass-abstract? class)
			      `(lambda (x)
				  (error
				   ',class-id
				   "Can't make instance of abstract classes"
				   ',class-id)))
			     ((not *reflection?*)
			      #f)
			     ((not (symbol? class-make-id))
			      (internal-error "make-add-class!"
					      "make-class-id not a symbol"
					      class-make-id))
			     (else
			      `(@ ,class-make-id ,class-module))))
	  (class-nil      (symbol-append class-id '-nil))
	  (class-pred     (class-predicate class))
	  (class-alloc    (if (tclass-abstract? class)
			      `(lambda (x)
				  (error
				   ',class-alloc-id
				   "Can't allocate instance of abstract classes"
				   ',class-id))
			      `(@ ,class-alloc-id ,class-module)))
	  (hash           (get-class-hash class-id (cddr src-def)))
	  (constr         (tclass-constructor class))
	  (loc            (find-location src-def))
	  (fields         (if *reflection?*
			      (make-class-fields class-id (cddr src-def) loc)
			      #f))
	  (super-class    (if (not (tclass? super))
			      #f
			      (let* ((sholder (tclass-holder super))
				     (sholder-id (global-id sholder))
				     (sholder-module (global-module sholder)))
				 `(@ ,sholder-id ,sholder-module)))))
      (trace (ast 2) "make-add-class!: " (shape class) " " virtuals #\Newline)
      (let ((decl `(define ,holder-id
		      ((@ register-class! __object)
		       ',class-id ,super-class ,(tclass-abstract? class)
		       ,class-make ,class-alloc ,class-nil ,class-pred ,hash
		       ,fields ,constr
		       (vector ,@(map (lambda (v)
					 `(cons ,(car v)
						(cons ,(cadr v)
						      ,(caddr v))))
				      virtuals))))))
	 ;; we have to inject a source positioning into the build declaration
	 (let ((loc-decl (if (epair? src-def)
			     (econs (car decl) (cdr decl) (cer src-def))
			     decl)))
	    (set! *declared-classes* (cons loc-decl *declared-classes*))))))

;*---------------------------------------------------------------------*/
;*    get-hash-class ...                                               */
;*---------------------------------------------------------------------*/
(define (get-class-hash class-id fields)
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

;*---------------------------------------------------------------------*/
;*    make-class-fields ...                                            */
;*    -------------------------------------------------------------    */
;*    We have not found a better way to do it. We re-parse the class   */
;*    definition (according to module_prototype and object_slots)      */
;*    to produce the correct proper list for the class declaration.    */
;*---------------------------------------------------------------------*/
(define (make-class-fields class-id slot-defs loc)
   (define (read-only? attr)
      (let loop ((attr attr))
	 (cond
	    ((null? attr)
	     #f)
	    ((eq? (car attr) 'read-only)
	     #t)
	    (else
	     (loop (cdr attr))))))
   (define (virtual? attr)
      (let loop ((attr attr))
	 (cond
	    ((null? attr)
	     #f)
	    ((and (pair? (car attr)) (eq? (caar attr) 'get))
	     #t)
	    (else
	     (loop (cdr attr))))))
   (define (find-info-attribute attr)
      (if (not (pair? attr))
	  #f
	  (match-case (car attr)
	     ((info ?value)
	      value)
	     (else
	      (find-info-attribute (cdr attr))))))
   (define (find-default-attribute attr)
      (if (not (pair? attr))
	  '((@ class-field-no-default-value __object))
	  (match-case (car attr)
	     ((default ?value)
	      `',value)
	     (else
	      (find-default-attribute (cdr attr))))))
   (define (make-slot-field slot)
      (match-case slot
	 ((? symbol?)
	  ;; simple slot without any attributes
	  (let ((id (fast-id-of-id slot loc)))
	     `((@ make-class-field __object)
	       ',id
	       ,(symbol-append class-id '- id)
	       ,(symbol-append class-id '- id '-set!)
	       #unspecified
	       #f
	       #f
	       ((@ class-field-no-default-value __object)))))
	 ((* (and ?id (? symbol?)) . ?att)
	  ;; indexed slot with possible attributes
	  (if (any? virtual? att)
	      (error id "Illegal indexed slot" slot)
	      (let ((id (fast-id-of-id id loc)))
		 `((@ make-class-field __object)
		   ',id
		   ,(symbol-append class-id '- id '-ref)
		   ,(if (not (read-only? att))
			(symbol-append class-id '- id '-set!)
			'#unspecified)
		   ,(symbol-append class-id '- id '-len)
		   #f
		   ,(find-info-attribute slot)
		   ,(find-default-attribute slot)))))
	 (((and ?id (? symbol?)) . ?att)
	  ;; simple slot with attributes
	  (let ((id (fast-id-of-id id loc)))
	     `((@ make-class-field __object)
	       ',id
	       ,(symbol-append class-id '- id)
	       ,(if (not (read-only? att))
		    (symbol-append class-id '- id '-set!)
		    '#unspecified)
	       #unspecified
	       ,(virtual? att)
	       ,(find-info-attribute slot)
	       ,(find-default-attribute slot))))
	 (else
	  (internal-error "make-class-fields"
			  "Illegal slot definition"
			  slot))))
   (let ((slot-defs (match-case slot-defs
		       (((?-) . ?rest)
			rest)
		       (else
			slot-defs))))
      `(list ,@(map make-slot-field slot-defs))))
   
;*---------------------------------------------------------------------*/
;*    class-finalizer ...                                              */
;*    -------------------------------------------------------------    */
;*    @label class unit@                                               */
;*---------------------------------------------------------------------*/
(define (class-finalizer)
   (cond
      ((and (null? *class-accesses*) (not (eq? *object-init-mode* 'staged)))
       'void)
      (else
       ;; class definitions and method definitions are splitted because
       ;; method can't be added before imported modules are initialized and
       ;; modules cannot be initialized before classes are defined. The
       ;; initialization order is a difficult problem. The unit number
       ;; for object _must_ be greater than the unit number for importations.
       ;; See the module_impuse module (@ref impuse.scm:importation unit@)
       ;; to the unit number for importations. The consequence of that
       ;; initialization order is that it is not possible inside a module M1
       ;; that defines a class C to import a module M2 that uses C.
       ;; We start forcing class accessors creation. This will produce
       ;; class declarations that have to be added to the module object
       ;; initialization
       (let* ((body (force-class-accesses))
	      (body (append (reverse! *declared-classes*) body))
	      (body (if (and (>fx *debug-module* 0)
			     (memq 'module
				   (backend-debug-support (the-backend)))
			     (pair? *declared-classes*))
			`((begin
			    (pragma::void
			     ,(string-append "bgl_init_module_debug_object(\""
					     (symbol->string *module*)
					     "\")"))
			    ,@body))
			body)))
	  (set! *object-unit* (unit 'object
				    19
				    (if (pair? body) body '(#unspecified))
				    #t
				    (eq? *object-init-mode* 'staged)))
	  ;; the method unit weight (20 here) is constraint as follow:
	  ;;    object-weight < import-weight < method-weight < toplevel-weight
	  ;;    any change must be reported in modules:
	  ;;    module_impuse (function import-finalizer)
	  ;;    module_include (function include-finalizer)
	  [assert () (<fx 20 (get-toplevel-unit-weight))]
	  (set! *method-unit* (unit 'method
				    21
				    ;; this unit may be empty so we initialize
				    ;; it with a dummy expression.
				    '(#unspecified)
				    #t
				    (eq? *object-init-mode* 'staged)))
	  (set! *generic-unit* (unit 'generic
				     20
				     ;; this unit may be empty so we initialize
				     ;; it with a dummy expression.
				     '(#unspecified)
				     #t
				     (eq? *object-init-mode* 'staged)))
	  ;; MS: 6 Janvier 2007
	  (set! *class-accesses* '())
	  (set! *declared-classes* '())
	  (list *object-unit* *generic-unit* *method-unit*)))))

;*---------------------------------------------------------------------*/
;*    class-finalizer-add-static! ...                                  */
;*    -------------------------------------------------------------    */
;*    This function is used for static classes declared inside         */
;*    the body of the module.                                          */
;*---------------------------------------------------------------------*/
(define (class-finalizer-add-static!)
   (let* ((body (force-class-accesses))
	  (body (append (reverse! *declared-classes*) body)))
      (if (unit? *object-unit*)
	  (unit-sexp*-set! *object-unit*
			   (append (unit-sexp* *object-unit*) body))
	  (set! *object-unit*
		(unit 'object
		      19
		      (if (pair? body) body '(#unspecified))
		      #t
		      (eq? *object-init-mode* 'staged))))
      (unless (unit? *method-unit*)
	 (set! *method-unit*
	       (unit 'method
		     21
		     ;; this unit may be empty so we initialize
		     ;; it with a dummy expression.
		     '(#unspecified)
		     #t
		     (eq? *object-init-mode* 'staged))))
      (unless (unit? *generic-unit*)
	 (set! *generic-unit*
	       (unit 'generic
		     21
		     ;; this unit may be empty so we initialize
		     ;; it with a dummy expression.
		     '(#unspecified)
		     #t
		     (eq? *object-init-mode* 'staged))))
      (set! *class-accesses* '())
      (set! *declared-classes* '())
      #unspecified))
   
;*---------------------------------------------------------------------*/
;*    force-class-accesses ...                                         */
;*    -------------------------------------------------------------    */
;*    Class accessors computation is tricky. We have to sort the class */
;*    before creating the accessors. That is, the classes have to be   */
;*    defined from the root of the tree to the nodes.                  */
;*---------------------------------------------------------------------*/
(define (force-class-accesses)
   ;; first we process the non wide classes
   (let loop ((cur    (reverse! *class-accesses*))
	      (next   '())
	      (access '()))
      (if (null? cur)
	  (if (null? next)
	      access
	      (loop next '() access))
	  (let* ((class (car (car cur)))
		 (super (if (tclass? class)
			    (tclass-its-super class)
			    (jclass-its-super class))))
	     (cond
		((eq? super class)
		 ;; this is the root we proceed now
		 (loop (cdr cur)
		       next
		       (append (force (cdr (car cur)))
			       access)))
		((and (tclass? class) (not (tclass? super)))
		 (if (type? super)
		     ;; this is and error that will be pointed out later. for
		     ;; the moment we simply ignore it.
		     (begin
			(tclass-slots-set! class '())
			(loop (cdr cur)
			      next
			      access))
		     (loop (cdr cur)
			   next
			   (append (force (cdr (car cur)))
				   access))))
		((and (jclass? class) (not (jclass? super)))
		 (if (type? super)
		     ;; this is and error that will be pointed out later. for
		     ;; the moment we simply ignore it.
		     (begin
			(jclass-slots-set! class '())
			(loop (cdr cur)
			      next
			      access))
		     (loop (cdr cur)
			   next
			   (append (force (cdr (car cur)))
				   access))))
		((and (tclass? class) (eq? (tclass-slots super) #unspecified))
		 ;; we have not yet seen the super, we delay again
		 (loop (cdr cur)
		       (cons (car cur) next)
		       access))
		((and (jclass? class) (eq? (jclass-slots super) #unspecified))
		 ;; we have not yet seen the super, we delay again
		 (loop (cdr cur)
		       (cons (car cur) next)
		       access))
		(else
		 ;; ok, the super has been proceed, we go for this class
		 (loop (cdr cur)
		       next
		       (append (force (cdr (car cur)))
			       access))))))))

