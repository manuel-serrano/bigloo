;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdl/src/env.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 22 12:11:43 2002                          */
;*    Last change :  Fri Dec  9 11:13:40 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Bdl constructor                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdl_env
   
   (import __bdl_misc
	   __bdl_types)
   
   (export (new-location::bdl-location ::bstring ::int)
	   (new-program::bdl-program ::obj ::obj ::pair-nil)
	   (new-module::bdl-module ::bdl-program ::bstring ::pair-nil ::bdl-location)
	   (new-function::bdl-function ::bdl-program ::bstring ::bdl-module ::bdl-location)
	   (new-method::bdl-method ::bdl-program ::bstring ::bdl-module ::bdl-location ::obj ::obj)
	   (new-generic::bdl-generic ::bdl-program ::bstring ::bdl-module ::bdl-location)
	   (new-macro::bdl-macro ::bdl-program ::bstring ::bdl-module ::bdl-location)
	   (new-variable::bdl-variable ::bdl-program ::bstring ::bdl-module ::bdl-location)
	   (new-type::bdl-type ::bdl-program ::bstring ::bdl-module ::bdl-location)
	   (new-class::bdl-class ::bdl-program ::bstring ::bdl-module ::bdl-location ::obj ::obj)
	   (new-structure::bdl-structure ::bdl-program ::bstring ::bdl-module ::bdl-location)
	   (new-extern::bdl-extern ::bdl-program ::bstring ::bdl-module ::bdl-location)

	   ;; finders
	   (find-bdl-ident ::bdl-program ::bstring)
	   (find-bdl-regexp-ident ::bdl-program ::bstring)
	   (find-bdl-module ::bdl-program ::bstring)
	   (find-bdl-method ::bdl-program ::bstring)
	   (find-bdl-generic ::bdl-program ::bstring)
	   (find-bdl-variable ::bdl-program ::bstring)
	   (find-bdl-type ::bdl-program ::bstring)
	   (find-bdl-class ::bdl-program ::bstring)
	   (find-bdl-structure ::bdl-program ::bstring)
	   (find-bdl-extern ::bdl-program ::bstring)

	   ;; list
	   (get-bdl-files::pair-nil ::bdl-program)
	   (get-bdl-modules::pair-nil ::bdl-program)
	   (get-bdl-functions::pair-nil ::bdl-program)
	   (get-bdl-methods::pair-nil ::bdl-program)
	   (get-bdl-generics::pair-nil ::bdl-program)
	   (get-bdl-variables::pair-nil ::bdl-program)
	   (get-bdl-types::pair-nil ::bdl-program)
	   (get-bdl-classes::pair-nil ::bdl-program)
	   (get-bdl-structures::pair-nil ::bdl-program)
	   (get-bdl-externs::pair-nil ::bdl-program)
	   
	   ;; user functions
	   make-location
	   make-program
	   make-module
	   make-function
	   make-method
	   make-generic
	   make-macro
	   make-variable
	   make-type
	   make-class
	   make-structure
	   make-extern))

;*---------------------------------------------------------------------*/
;*    make-new-sans-bind ...                                           */
;*---------------------------------------------------------------------*/
(define-macro (make-new-sans-bind type . args)
   (let ((id (symbol-append 'new- type))
	 (id? (symbol-append 'bdl- type ))
	 (make-id (symbol-append 'make- type))
	 (inst (symbol-append 'instantiate (string->symbol "::") 'bdl- type))
	 (tmp (gensym 'tmp)))
      `(begin
	  (define (,id ,@args)
	     (let ((,tmp (,make-id ,@args)))
		(if (isa? ,tmp ,id? )
		    ,tmp
		    (bdl-error ,id
		       (string-append
			  "Illegal instantiation, result not a "
			  ,(symbol->string type))
		       ,tmp))))
	  (define (,make-id ,@args)
	     (,inst ,@(map (lambda (a) (list a a)) args))))))

;*---------------------------------------------------------------------*/
;*    make-new ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (make-new type . args)
   (let ((id (symbol-append 'new- type))
	 (id? (symbol-append 'bdl- type ))
	 (make-id (symbol-append 'make- type))
	 (inst (symbol-append 'instantiate (string->symbol "::") 'bdl- type))
	 (tmp (gensym 'tmp))
	 (find-id (symbol-append 'find-bdl- type))
	 (get-id (symbol-append 'get-bdl- type
		    (let* ((s (symbol->string type))
			   (l (string-length s)))
		       (if (char=? (string-ref s (-fx l 1)) #\s)
			   'es
			   's))))
	 (env (symbol-append type 's)))
      `(begin
	  (define (,find-id prgm::bdl-program obj) (hashtable-get (-> prgm ,env ) obj))
	  (define (,id prgm ,@args)
	     (let ((,(symbol-append tmp '::bdl-entity) (,make-id ,@args)))
		(if (isa?  ,tmp ,id?)
		    (begin
		       (hashtable-put! (-> prgm ,env) (-> ,tmp ident ) ,tmp)
		       ,tmp)
		    (bdl-error ,id
		       (string-append
			  "Illegal instantiation, result not a "
			  ,(symbol->string type))
		       ,tmp))))
	  (define (,make-id ,@args)
	     (,inst ,@(map (lambda (a) (list a a)) args)))
	  (define (,get-id prgm::bdl-program)
	     (hashtable->list (-> prgm ,env ))))))

;*---------------------------------------------------------------------*/
;*    find-bdl-ident ...                                               */
;*    -------------------------------------------------------------    */
;*    Find the exact match of an identifier in all the environments.   */
;*---------------------------------------------------------------------*/
(define (find-bdl-ident prgm::bdl-program ident)
   (apply append
      (map (lambda (e)
	      (or (hashtable-get e ident) '()))
	 (-> prgm hashtables))))

;*---------------------------------------------------------------------*/
;*    find-bdl-regexp-ident ...                                        */
;*    -------------------------------------------------------------    */
;*    Find all objects whose identifier match the regular expression.  */
;*---------------------------------------------------------------------*/
(define (find-bdl-regexp-ident prgm::bdl-program regexp)
   (define (find-regexp-ident-env env)
      (let ((res '()))
	 (hashtable-for-each env
	    (lambda (key obj)
	       (if (pregexp-match regexp key)
		   (set! res (cons obj res)))))
	 res))
   (apply append (map find-regexp-ident-env (-> prgm hashtables))))

;*---------------------------------------------------------------------*/
;*    get-bdl-files ...                                                */
;*---------------------------------------------------------------------*/
(define (get-bdl-files prgm::bdl-program)
   (-> prgm files))

;*---------------------------------------------------------------------*/
;*    Constructors                                                     */
;*---------------------------------------------------------------------*/
(make-new-sans-bind location file line)
(make-new-sans-bind program afile etags files)
(make-new module ident files loc)
(make-new function ident module loc)
(make-new generic ident module loc)
(make-new method ident module loc select-type return-type)
(make-new macro ident module loc)
(make-new variable ident module loc)
(make-new type ident module loc)
(make-new class ident module loc super kind)
(make-new structure ident module loc)
(make-new extern ident module loc)

	      
