;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/predicate.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jun  5 11:16:50 1996                          */
;*    Last change :  Wed Nov 21 07:20:11 2012 (serrano)                */
;*    Copyright   :  1996-2012 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We make the class predicate                                      */
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
(module object_predicate
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    ast_private
	    object_class
	    object_slots
	    object_tools
	    module_module
	    module_impuse
	    engine_param)
   (export  (gen-class-pred! ::tclass ::pair ::symbol)
	    (import-class-pred! ::tclass ::pair ::symbol)
	    (import-java-class-pred! ::jclass ::pair ::symbol)))

;*---------------------------------------------------------------------*/
;*    inline-pred? ...                                                 */
;*    -------------------------------------------------------------    */
;*    This function returns #t if the class predicate must be          */
;*    inlined.                                                         */
;*---------------------------------------------------------------------*/
(define (inline-pred?)
   (and (or (not (number? *profile-mode*))
	    (<fx *profile-mode* 1))
	(not (memq *pass* '(make-heap make-add-heap)))))
   
;*---------------------------------------------------------------------*/
;*    gen-class-pred! ...                                              */
;*    -------------------------------------------------------------    */
;*    The class predicate is not inlined when compiling for            */
;*    profiling.                                                       */
;*---------------------------------------------------------------------*/
(define (gen-class-pred! class::tclass src-def import)
   (let* ((id      (tclass-id class))
	  (id?     (symbol-append id '?))
	  (pred-id (symbol-append id '?::bool))
	  (holder  (tclass-holder class))
	  (obj     (mark-symbol-non-user! (gensym 'obj)))
	  (super   (tclass-its-super class)))
      (define (predicate-body)
	 `((@ isa? __object) ,obj (@ ,(global-id holder) ,(global-module holder))))
      (if (not (tclass? super))
 	  ;; roots class tree must have ad-hoc predicate checker as
	  ;; the (@ object? __object) predicate.
	  '()
	  (let ((super-pred-id (symbol-append 'super- pred-id))
		(super-typed   (make-typed-ident 'super (type-id super))))
	     ;; the pragma declaration
	     (produce-module-clause!
	      (if (inline-pred?)
		  `(,import (inline ,pred-id ::obj))
		  `(,import (,pred-id ::obj))))
	     (produce-module-clause!
	      `(pragma (,id? (predicate-of ,(tclass-id class)) no-cfa-top
			     (effect))))
	     ;; we produce the predicat definitions...
	     (list
	      (epairify* `(,(if (inline-pred?) 'define-inline 'define)
			   (,pred-id ,obj)
			   ,(predicate-body))
			 src-def))))))

;*---------------------------------------------------------------------*/
;*    import-class-pred! ...                                           */
;*---------------------------------------------------------------------*/
(define (import-class-pred! class::tclass src-def module)
   (if (inline-pred?)
       ;; if we compile for optimization, we inline class predicate
       (gen-class-pred! class src-def 'static)
       ;; othwerise, we simply import it
       (let* ((id      (tclass-id class))
	      (id?     (symbol-append id '?))
	      (pred-id (symbol-append id '?::bool))
	      (holder  (tclass-holder class))
	      (super   (tclass-its-super class)))
	  (if (tclass? super)
	      ;; the module declaration
	      (import-parser module `(,pred-id ::obj) #f))
	  ;; and we return no code in that particular situation
	  '())))

;*---------------------------------------------------------------------*/
;*    gen-java-class-pred! ...                                         */
;*    -------------------------------------------------------------    */
;*    The class predicate is not inlined when compiling for            */
;*    profiling.                                                       */
;*---------------------------------------------------------------------*/
(define (gen-java-class-pred! class::jclass src-def mclause)
   (let* ((id      (jclass-id class))
	  (id?     (symbol-append id '?))
	  (pred-id (symbol-append id '?::bool))
	  (obj     (mark-symbol-non-user! (gensym 'obj)))
	  (super   (jclass-its-super class)))
      (define (predicate-body)
	 (make-private-sexp 'instanceof id obj))
      ;; the pragma declaration
      (produce-module-clause!
       `(,mclause (inline ,pred-id ::obj)))
      (produce-module-clause!
       `(pragma (,id? (predicate-of ,(jclass-id class)) no-cfa-top (effect))))
      ;; we produce the predicat definitions...
      (list
       (epairify* `(,(if (inline-pred?) 'define-inline 'define)
		    (,pred-id ,obj)
		    ,(predicate-body))
		  src-def))))

;*---------------------------------------------------------------------*/
;*    import-java-class-pred! ...                                      */
;*---------------------------------------------------------------------*/
(define (import-java-class-pred! class::jclass src-def module)
   (if (inline-pred?)
       ;; if we compile for optimization, we inline class predicate
       (gen-java-class-pred! class
			     src-def
			     (if (eq? module *module*) 'export 'static))
       ;; othwerise, we simply import it
       (let* ((id      (jclass-id class))
	      (id?     (symbol-append id '?))
	      (pred-id (symbol-append id '?::bool))
	      (super   (jclass-its-super class)))
	  ;; the module declaration
	  (import-parser module `(,pred-id ::obj) #f)
	  ;; and we return no code in that particular situation
	  '())))
