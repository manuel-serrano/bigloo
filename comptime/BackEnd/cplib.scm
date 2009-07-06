;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/BackEnd/cplib.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec  8 10:40:16 2003                          */
;*    Last change :  Wed Jan 19 11:37:27 2005 (serrano)                */
;*    Copyright   :  2003-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    BackEnd common facilities                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_cplib
   (include "Tvector/tvector.sch")
   (import  tools_shape
	    type_type
	    type_env
	    ast_var
	    ast_node
	    ast_env
	    type_cache
	    cnst_node
	    object_class
	    object_slots
	    read_jvm
	    module_java
	    engine_param
	    backend_c_prototype
	    tools_error)
   (export  (class-id->type-name::bstring ::symbol ::symbol)
	    (qualified-tclass-name::bstring ::tclass)
	    (qualified-wclass-name::bstring ::wclass)
	    (qualified-jclass-name::bstring ::jclass)
	    (qualified-type-name::bstring ::type)
	    (reset-global! ::global)
	    (set-variable-name! ::variable)
	    (global-entry::global ::global)
	    (global-arity ::global)
	    (for-each-declared-classes fun)
	    (get-its-super::tclass ::type)
	    (get-declared-classes)
	    (get-declared-fields ::type)
	    (get-field-type ::slot)
	    (wide->chunk c::tclass)
	    (get-declared-global-variables module)
	    (get-global-variables-to-be-initialized module)))

;*---------------------------------------------------------------------*/
;*    class-id->type-name ...                                          */
;*    -------------------------------------------------------------    */
;*    Constructs a type name from a class identifier + a module        */
;*    identifier.                                                      */
;*---------------------------------------------------------------------*/
(define (class-id->type-name cid mid)
   (let ((id (bigloo-module-mangle (symbol->string cid) (symbol->string mid))))
      (string-append "_K" id)))

;*---------------------------------------------------------------------*/
;*    qualified-real-tclass-name ...                                   */
;*---------------------------------------------------------------------*/
(define (qualified-real-tclass-name::bstring class::tclass) 
   (define (on-package pkgc name)
      ;; name is already mangled.
      [assert (name) (not (bigloo-need-mangling? name))]
      (if (string=? pkgc "")
	  name
	  (string-append pkgc "." name)))
   
   (define (tclass-id-mangling::bstring class::tclass)
      (if (eq? class (get-object-type))
	  "object"
	  (with-access::tclass class (id holder)
	     (let ((mod (global-module holder)))
		(class-id->type-name id mod)))))
   
   (define (class-package name)
      (let* ((s (module->qualified-type name))
	     (pref (prefix s)))
	 (if (string=? pref s)
	     ""
	     pref)))
   
   (define (super-package super)
      (with-access::tclass super (its-super id)
	 (if (or (not (tclass? its-super))
		 (and (eq? super its-super)
		      (not (eq? (type-id its-super) 'obj))))
	     "bigloo"
	     (let ((holder (tclass-holder super)))
		(add-qualified-type! (global-module holder)
				     (global-jvm-type-name holder)
				     id)
		(class-package (global-module holder))))))
   
   (on-package (super-package class) (tclass-id-mangling class)))

;*---------------------------------------------------------------------*/
;*    qualified-tclass-name ...                                        */
;*---------------------------------------------------------------------*/
(define (qualified-tclass-name::bstring class::tclass)
   (let ((c (if (wide-class? class) (tclass-its-super class) class)))
      (qualified-real-tclass-name c)))

;*---------------------------------------------------------------------*/
;*    qualified-wclass-name ...                                        */
;*---------------------------------------------------------------------*/
(define (qualified-wclass-name::bstring class::wclass)
   (qualified-real-tclass-name (wclass-its-class class)))

;*---------------------------------------------------------------------*/
;*    qualified-jclass-name ...                                        */
;*---------------------------------------------------------------------*/
(define (qualified-jclass-name::bstring class::jclass)
   (jclass-name class))

;*---------------------------------------------------------------------*/
;*    qualified-type-name ...                                          */
;*---------------------------------------------------------------------*/
(define (qualified-type-name::bstring type::type)
   (let ( (id (type-id type)) )
      (if (eq? id 'foreign)
	  *jvm-foreign-class-name*
	  (let ((java-class (find-java-class id)))
	     (if (string? java-class)
		 java-class
		 (string-append "bigloo." (symbol->string id)) )))))


;*---------------------------------------------------------------------*/
;*    reset-global! ...                                                */
;*---------------------------------------------------------------------*/
(define (reset-global! var::global)
   ;; Set the name of the global
   (let ((name (global-name var)))
      (if (string? name)
	  name
	  (let ((name (symbol->string (global-id var))))
	     (global-name-set! var
			       (if (bigloo-need-mangling? name)
				   (bigloo-mangle name)
				   name)))))
   ;; Patch the type for global vectors.
   (let ((value (global-value var)))
      (if (and (scnst? value) (eq? (scnst-class value) 'stvector))
	  (global-type-set! var (a-tvector-type (scnst-node value))))))

;*---------------------------------------------------------------------*/
;*    set-variable-name! ...                                           */
;*---------------------------------------------------------------------*/
(define (set-variable-name! variable)
   (with-access::variable variable (name)
      (if (string? name)
	  name
	  (let ((n (cond
		      ((global? variable)
		       (with-access::global variable (id module)
			     (bigloo-module-mangle (symbol->string id)
						   (symbol->string module))))
		      ((local? variable)
		       (with-access::local variable (id key)
			     (bigloo-mangle (string-append
					     (symbol->string id)
					     "_"
					     (integer->string key)))))
		      (else
		       (internal-error "set-variable-name!"
				       "Unknown variable sort"
				       (shape variable))))))
	     (set! name n)
	     n))))

;*---------------------------------------------------------------------*/
;*    global-entry ...                                                 */
;*---------------------------------------------------------------------*/
(define (global-entry::global var::global)
   ;; Return the entry point associated to a global variable.
   (let ((value (global-value var)))
      (if (scnst? value)
	  (var-variable (car (app-args (scnst-node value))))
	  var)))

;*---------------------------------------------------------------------*/
;*    global-arity ...                                                 */
;*---------------------------------------------------------------------*/
(define (global-arity var::global)
   ;; Return the arity of the entry point associated to a global variable.
   (let ((value (global-value var)))
      (cond
	 ((scnst? value)
	  (get-node-atom-value (cadr (app-args (scnst-node value)))))
	 ((sfun? value)
	  (let ((x (sfun-arity value)))
	     (if (>= x 0) (- x 1) (+ x 1))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    for-each-declared-classes ...                                    */
;*---------------------------------------------------------------------*/
(define (for-each-declared-classes fun)
   (for-each (lambda (c)    
		(let ((wide? (wide-class? c)))
		   (fun
		    ;; the class who have the fields
		    c
		    ;; the class whe have the names
		    (if wide? (wide->chunk c) c)
		    ;. the super class
		    (if wide? (find-type 'obj) (tclass-its-super c)))))
	     (get-declared-classes)))

;*---------------------------------------------------------------------*/
;*    get-declared-classes ...                                         */
;*---------------------------------------------------------------------*/
(define (get-declared-classes)
   ;; Get the classes declared (not imported) in the current module.
   ;; CARE (global-import (tclass-holder (get-object-type))) is import ?!?!
   (let ((r '()))
      (for-each (lambda (c)
		   (unless (or (eq? (global-import (tclass-holder c)) 'import)
			       (eq? c (get-object-type)) )
		      (set! r (cons (if (wide-class? c) (wide->chunk c) c)
				    r ))))
		(get-class-list))
      r))

;*---------------------------------------------------------------------*/
;*    type->class ...                                                  */
;*---------------------------------------------------------------------*/
(define (type->class type)
   (cond
      ((tclass? type)
       type)
      ((wclass? type)
       (wclass-its-class type))
      (else
       (internal-error 'get-declared-fields "Illegal type" type))))

;*---------------------------------------------------------------------*/
;*    get-its-super                                                    */
;*---------------------------------------------------------------------*/
(define (get-its-super type)
   (tclass-its-super (type->class type)))

;*---------------------------------------------------------------------*/
;*    get-declared-global-variables                                    */
;*---------------------------------------------------------------------*/
(define (get-declared-fields type::type)
   ;; Get the fields declared (not herited) by a classe.
   (let ((class (type->class type)))
      (filter (lambda (slot::slot)
		 (and (not (slot-virtual? slot))
		      (eq? class (slot-class-owner slot))))
	      (tclass-slots class))))

;*---------------------------------------------------------------------*/
;*    get-field-type ...                                               */
;*---------------------------------------------------------------------*/
(define (get-field-type slot::slot)
   ;; Get the type of field
   (or (slot-indexed slot) (slot-type slot)))

;*---------------------------------------------------------------------*/
;*    wide->chunk                                                      */
;*---------------------------------------------------------------------*/
(define (wide->chunk c::tclass)
   ;; There is still some place where I need the name of the shunk associated
   ;; with a wide-class (as (compiling a class) and code (get/setfield))
   ;; MS: TEST a VIRER...
   (if (not (eq? (find-type (saw-wide-class-id (tclass-id c)))
		 (tclass-wide-type c)))
       (error "wide-shunk" "internal error" c)
       (tclass-wide-type c)))

;*---------------------------------------------------------------------*/
;*    get-declared-globlal-variables                                   */
;*---------------------------------------------------------------------*/
(define (get-declared-global-variables module)
   ;; Get the global variables (not functions) associated to a module
   (let ((r '()))
      (for-each-global! (lambda (v)
			   (if (and (eq? (global-module v) module)
				    (not (cfun? (global-value v)))
				    (not (sfun? (global-value v)))
				    (not (eq? (global-id v) '__cnsts_table)))
			       (set! r (cons v r)))))
      r))

;*---------------------------------------------------------------------*/
;*    get-global-variables-to-be-initialized                           */
;*---------------------------------------------------------------------*/
(define (get-global-variables-to-be-initialized module)
   ;; Get the global variables that have a value assigned to.
   (let ((r '()))
      (for-each-global! (lambda (global)
			   (if (and (eq? (global-module global) module)
				    ;; CARE lot of redondancy
				    (require-prototype? global)
				    (scnst? (global-value global)))
			       (set! r (cons global r)))))
      r))
