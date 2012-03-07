;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/coercion.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 17 10:02:36 2000                          */
;*    Last change :  Wed Mar  7 11:37:37 2012 (serrano)                */
;*    Copyright   :  2000-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    We make the class coercions functions.                           */
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
(module object_coercion
   (include "Tools/trace.sch")
   (import  tools_error
	    tools_misc
	    tools_shape
	    engine_param
	    backend_backend
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_ident
	    object_class
	    object_slots
	    object_tools
	    module_module
	    module_impuse
	    ast_private)
   (export (gen-class-coercions! class)
	   (gen-java-class-coercions! class)
	   (gen-coercion-clause! ::type ::symbol super . testing)
	   (gen-class-coercers! class super)))

;*---------------------------------------------------------------------*/
;*    gen-class-coercions! ...                                         */
;*---------------------------------------------------------------------*/
(define (gen-class-coercions! class)
   (with-access::tclass class (id its-super)
      (gen-coercion-clause! class id its-super)
      (gen-class-coercers! class its-super)))

;*---------------------------------------------------------------------*/
;*    gen-java-class-coercions! ...                                    */
;*---------------------------------------------------------------------*/
(define (gen-java-class-coercions! class)
   (with-access::jclass class (id its-super)
      (gen-coercion-clause! class id its-super)
      (gen-class-coercers! class its-super)))

;*---------------------------------------------------------------------*/
;*    gen-coercion-clause! ...                                         */
;*    -------------------------------------------------------------    */
;*    This function has to take care that the super class may be       */
;*    incorrect (because this error is now detected very late in       */
;*    compilation). Thus on the iteration on super, we have to check   */
;*    that super is a class. If not, it is not a problem, we can       */
;*    simply stop the iteration. We can do this simple thing because   */
;*    eventually the super error will be detected and the compilation  */
;*    will be stopped.                                                 */
;*---------------------------------------------------------------------*/
(define (gen-coercion-clause! class c-id super . testing)
   (produce-module-clause!
      (make-coercion-clause class c-id super testing)))

;*---------------------------------------------------------------------*/
;*    make-coercion-clause ...                                         */
;*---------------------------------------------------------------------*/
(define (make-coercion-clause class c-id super testing)
   (let* ((class->obj `(lambda (x)
			  ,(make-private-sexp 'cast 'obj 'x)))
	  (obj->class `(lambda (x)
			  ,(make-private-sexp 'cast c-id 'x)))
	  (ttest (if (null? testing)
		     (let ((o (gensym 'o)))
			(if (jclass? class)
			    `((lambda (,o)
				 ,(make-private-sexp 'instanceof c-id o)))
			    `((lambda (,o)
				 ((@ isa? __object) ,o ,c-id)))))
		     '()))
	  (x (make-typed-ident 'x c-id)))
      (let loop ((super super)
		 (coercer (list `(coerce obj ,c-id ,ttest (,obj->class))
				`(coerce ,c-id obj () (,class->obj))
				`(coerce ,c-id bool () ((lambda (,x) #t))))))
	 (if (not (or (jclass? super) (tclass? super)))
	     `(type ,@coercer)
	     (let* ((super-id (if (tclass? super)
				  (tclass-id super)
				  (jclass-id super)))
		    (class->super `(lambda (x)
				      ,(make-private-sexp 'cast super-id 'x)))
		    (super->class `(lambda (x)
				      ,(make-private-sexp 'cast c-id 'x))))
		(loop (if (tclass? super)
			  (tclass-its-super super)
			  (jclass-its-super super))
		      (cons* `(coerce ,super-id ,c-id ,ttest (,super->class))
			     `(coerce ,c-id ,super-id () (,class->super))
			     coercer)))))))

;*---------------------------------------------------------------------*/
;*    gen-class-coercers! ...                                          */
;*    -------------------------------------------------------------    */
;*    We create all the coercers between type, obj and its super       */
;*    classes.                                                         */
;*    -------------------------------------------------------------    */
;*    This function has to take care that the super class may be       */
;*    incorrect (because this error is now detected very late in       */
;*    compilation). Thus on the iteration on super, we have to check   */
;*    that super is a class. If not, it is not a problem, we can       */
;*    simply stop the iteration. We can do this simple thing because   */
;*    eventually the super error will be detected and the compilation  */
;*    will be stopped.                                                 */
;*---------------------------------------------------------------------*/
(define (gen-class-coercers! class super)
   (when (backend-pragma-support (the-backend))
      (c-gen-class-coercers! class super)))

;*---------------------------------------------------------------------*/
;*    c-gen-class-coercers! ...                                        */
;*---------------------------------------------------------------------*/
(define (c-gen-class-coercers! class super)
   (define (make-one-coercion from-id from-name to-id to-name)
      (let ((t->f (symbol-append to-id '-> from-id))
	    (f->t (symbol-append from-id '-> to-id)))
	 (values `(pragma (,t->f nesting args-safe side-effect-free no-cfa-top (effect))
			  (,f->t nesting args-safe side-effect-free no-cfa-top (effect)))
		 (list `(macro ,from-id ,t->f (,to-id)
			       ,(string-append "(" from-name ")"))
		       `(macro ,to-id ,f->t (,from-id)
			       ,(string-append "(" to-name ")"))))))
   (let ((tid   (type-id   class))
	 (tname (type-name class)))
      (multiple-value-bind (prag coercers)
	 (make-one-coercion tid tname 'obj "obj_t")
	 (let loop ((super super)
		    (coercers coercers)
		    (pragmas (list prag)))
	    (if (not (tclass? super))
		(begin
		   (produce-module-clause! `(foreign ,@coercers))
		   (for-each produce-module-clause! pragmas))
		(let ((sid   (type-id super))
		      (sname (type-name super)))
		   (multiple-value-bind (prag coercs)
		      (make-one-coercion tid tname sid sname)
		      (loop (tclass-its-super super)
			    (append coercs coercers)
			    (cons prag pragmas)))))))))

   

