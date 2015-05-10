;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/class.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 10:52:20 1996                          */
;*    Last change :  Sun May 10 08:57:29 2015 (serrano)                */
;*    Copyright   :  1996-2015 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The class clause handling                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_class
   
   (include "Ast/unit.sch"
	    "Tools/trace.sch"
	    "../runtime/Llib/object.sch")
   
   (import  module_module
	    module_impuse
	    module_include
	    backend_backend
	    engine_param
	    tools_shape
	    tools_error
	    tools_location
	    tools_misc
	    type_type
	    type_env
	    ast_ident
	    ast_var
	    ast_env
	    ast_object
	    ast_node
	    object_class
	    object_coercion
	    object_classgen
	    object_slots)
   (export  (declare-class! ::pair ::symbol ::symbol ::bool ::bool ::obj ::obj)
	    (declare-wide-class! ::pair ::symbol ::symbol ::obj ::obj)
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
       (declare-export-class!
	  class-def module
	  (if final? 'final 'plain)
	  abstract?
	  def-src decl-src import))
      (else
       (declare-import-class!
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
       (declare-export-class!
	  class-def module 'wide #f
	  def-src decl-src import))
      (else
       (declare-import-class!
	  class-def module 'wide #f
	  def-src decl-src))))

;*---------------------------------------------------------------------*/
;*    declare-export-class! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-export-class! cdef module kind abstract? src-def src-decl import)
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
      (assert (tclass) (tclass? tclass))
      ;; we store the src-import location in order to print a nice error
      ;; message if that tclass is not defined
      (type-import-location-set! tclass (find-location/loc src-decl loc))
      ;; tclass can be something else than a class if an error has been found
      (delay-class-accessors!
	 tclass
	 (delay
	    (gen-register-class! cdef holder tclass src-def)))))

;*---------------------------------------------------------------------*/
;*    declare-import-class! ...                                        */
;*---------------------------------------------------------------------*/
(define (declare-import-class! cdef module kind abstract? src-def src-decl)
   (trace (ast 2) "declare-import-class!: " src-def #\Newline)
   ;; We create the class holder
   ;; and we create a type for this class
   (let* ((loc (find-location src-def))
	  (class-var (car cdef))
	  (class-id (id-of-id class-var loc))
	  (holder (import-parser module class-id #f))
	  (final? (eq? kind 'final))
	  (wide (if (eq? kind 'wide) 'widening #f))
	  (tclass (declare-class-type! cdef holder wide
				       final? abstract?
				       src-def)))
      ;; some paranoid checking
      (assert (tclass) (tclass? tclass))
      ;; we store the src-import location in order to print a nice error
      ;; message if that tclass is not defined
      (type-import-location-set! tclass (find-location/loc src-decl loc))
      ;; when importing a class, we import the accessors...
      (delay-class-accessors!
	 tclass
	 (delay
	    (begin
	       ;; store inside the class structure some
	       ;; information about its slots
	       (set-class-slots! tclass cdef src-def)
	       ;; install the coercion between the new-class and obj
	       ;; and the class and all its super classes
	       (gen-class-coercions! tclass)
	       '())))))

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
;*    ge-register-class! ...                                           */
;*    -------------------------------------------------------------    */
;*    @label register-class@                                           */
;*---------------------------------------------------------------------*/
(define (gen-register-class! class-def holder class src-def)
   (if (check-class-declaration? class src-def)
       (begin
	  ;; store inside the class structure some information about its slots
	  (set-class-slots! class class-def src-def)
	  ;; install the coercion between the new-class and obj
	  ;; and the class and all its super classes
	  (gen-class-coercions! class)
	  (let* ((classid (type-id class))
		 (classmod (global-module (tclass-holder class)))
		 (super (tclass-its-super class))
		 (superv (when (tclass? super)
			    (let* ((sholder (tclass-holder super))
				   (sholderid (global-id sholder))
				   (sholdermodule (global-module sholder)))
			       `(@ ,sholderid ,sholdermodule))))
		 (decl `(define ,(global-id holder)
			   ((@ register-class! __object)
			    ;; class id
			    ',classid
			    ;; class module
			    ',classmod
			    ;; super class
			    ,superv
			    ;; hash
			    ,(get-class-hash src-def)
			    ;; new
			    ,(unless (tclass-abstract? class)
				(classgen-make-anonymous class))
			    ;; allocator
			    ,(cond
				((wide-class? class)
				 (classgen-widen-anonymous class))
				(else
				 ;; generator an allocator even for
				 ;; abstract class in order to be able
				 ;; to build the nil instance
				 (classgen-allocate-anonymous class)))
			    ;; constructor
			    ,(find-class-constructor class)
			    ;; nil
			    ,(classgen-nil-anonymous class)
			    ;; predicate
			    ,(when (wide-class? class)
				(classgen-shrink-anonymous class))
			    ;; plain fields
			    ,(make-class-fields class) 
			    ;; virtual fields
			    ,(make-class-virtual-fields class))))
		 (edecl (if (epair? src-def)
			    (econs (car decl) (cdr decl) (cer src-def))
			    decl)))
	     (set! *declared-classes*
		(cons edecl *declared-classes*))
	     '()))
       ;; the class is incorrect, an error has been signaled, keep going
       ;; as if everything is fine
       (begin
	  (tclass-slots-set! class '())
	  '())))
		       
;*---------------------------------------------------------------------*/
;*    make-class-fields ...                                            */
;*---------------------------------------------------------------------*/
(define (make-class-fields class)
   
   (define (type-default-id type)
      (if (eq? (type-id type) '_)
	  'obj
	  (type-id type)))

   (define (slot-default-expr s)
      (if (slot-default? s)
	  `(lambda () ,(slot-default-value s))
	  #f))
   
   (define (make-class-field-virtual s)
      `((@ make-class-field __object)
	',(slot-id s)
	(lambda (o)
	   ((@ call-virtual-getter __object) o ,(slot-virtual-num s)))
	(lambda (o v)
	   ((@ call-virtual-setter __object) o ,(slot-virtual-num s) v))
	,(slot-read-only? s)
	#t
	,(slot-user-info s)
	,(slot-default-expr s)
	,(if (tclass? (slot-type s))
	     (tclass-holder (slot-type s))
	     `',(type-default-id (slot-type s)))))
   
   (define (make-class-field-plain s)
      (let ((defs (classgen-slot-anonymous class s)))
	 ;; plain slot
	 `((@ make-class-field __object)
	   ',(slot-id s)
	   ,(car defs) ,(cadr defs) ,(slot-read-only? s)
	   #f
	   ,(slot-user-info s)
	   ,(slot-default-expr s)
	   ,(if (tclass? (slot-type s))
		(tclass-holder (slot-type s))
		`',(type-default-id (slot-type s))))))
   
   `(vector
       ,@(filter-map (lambda (s)
			(when (eq? (slot-class-owner s) class)
			   (if (slot-virtual? s)
			       (unless (slot-virtual-override s)
				  (make-class-field-virtual s))
			       (make-class-field-plain s))))
	    (tclass-slots class))))

;*---------------------------------------------------------------------*/
;*    make-class-virtual-fields ...                                    */
;*---------------------------------------------------------------------*/
(define (make-class-virtual-fields class)

   (define (epairify-slot def slot)
      (with-access::slot slot (src)
	 (if (epair? src)
	     (epairify-rec def src)
	     def)))
   
   (define (getter slot)
      (epairify-slot
	 `(lambda (o)
	     (define (call-next-slot)
		(call-next-virtual-getter ,(tclass-id class) o
		   ,(slot-virtual-num slot)))
	     (,(slot-getter slot) o))
	 slot))

   (define (setter slot)
      (if (slot-read-only? slot)
	  #f
	  (epairify-slot
	     `(lambda (o v)
		 (define (call-next-slot)
		    (call-next-virtual-setter ,(tclass-id class) o
		       ,(slot-virtual-num slot) v))
		 (,(slot-setter slot) o v))
	     slot)))
   
   `(vector
       ,@(filter-map (lambda (s)
			(when (and (slot-virtual? s)
				   (eq? (slot-class-owner s) class))
			   `(cons ,(slot-virtual-num s)
			       (cons ,(getter s) ,(setter s)))))
	    (tclass-slots class))))

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
	      (body (append (reverse *declared-classes*) body))
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
	  (assert () (<fx 20 (get-toplevel-unit-weight)))
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
	  (body (append (reverse *declared-classes*) body)))
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
   (let loop ((cur    (reverse *class-accesses*))
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
		       (append (force (cdr (car cur))) access)))
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
			   (append (force (cdr (car cur))) access))))
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
			   (append (force (cdr (car cur))) access))))
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
		       (append (force (cdr (car cur))) access))))))))

