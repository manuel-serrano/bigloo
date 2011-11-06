;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Object/classgen.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Nov  6 06:14:12 2011                          */
;*    Last change :  Sun Nov  6 08:59:22 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generate the class accessors.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module object_classgen
   (include "Engine/pass.sch")
   (import  tools_error
	    type_type
	    engine_param
	    object_class
	    object_slots
	    ast_var
	    ast_ident
	    ast_node
	    module_module
	    write_scheme)
   (export  (classgen-walk)))

;*---------------------------------------------------------------------*/
;*    classgen-walk ...                                                */
;*---------------------------------------------------------------------*/
(define (classgen-walk)
   (pass-prelude "Class accessors generation")
   ;; output file
   (let* ((oname (cond
		    ((string? *dest*)
		     *dest*)
		    ((eq? *dest* '--to-stdout)
		     #f)
		    ((and (pair? *src-files*) (string? (car *src-files*)))
		     (string-append (prefix (car *src-files*)) ".sch"))
		    (else
		     #f)))
	  (po (if (string? oname)
		  (open-output-file oname)
		  (current-output-port))))
      ;; the header
      (write-scheme-file-header po "Class accessors")
      ;; we emit the generated type for the classes
      (let ((protos '())
	    (defs '()))
	 (for-each (lambda (c)
		      (when (bigloo-domestic-class? c)
			 (multiple-value-bind (p d)
			    (classgen c)
			    (set! protos (cons (cons c p) protos))
			    (set! defs (cons (cons c d) defs)))))
	    (get-class-list))
	 (when (pair? protos)
	    (write-scheme-comment po "The directives")
	    (display "(directives\n   (cond-expand (bigloo-class-sans\n\n" po)
	    (for-each (lambda (p)
			 (let ((c (car p))
			       (protos (cdr p)))
			    (write-scheme-comment po (tclass-id c))
			    (display "(" po)
			    (display (car protos) po)
			    (newline po)
			    (for-each (lambda (p)
					 (display "  " po)
					 (display p po)
					 (newline po))
			       (cdr protos))
			    (newline po)))
	       protos)
	    (display ")))\n\n" po)
	    (write-scheme-comment po "The definitions")
	    (display "(cond-expand (bigloo-class-sans\n\n" po)
	    (for-each (lambda (p)
			 (let ((c (car p))
			       (defs (cdr p)))
			    (write-scheme-comment po (tclass-id c))
			    (for-each (lambda (p)
					 (display p po)
					 (newline po))
			       defs)
			    (newline po)))
	       defs)
	    (display "))\n" po)
	    (close-output-port po)))))

;*---------------------------------------------------------------------*/
;*    bigloo-domestic-class? ...                                       */
;*---------------------------------------------------------------------*/
(define (bigloo-domestic-class? c)
   (and (tclass? c) (eq? (global-module (tclass-holder c)) *module*)))

;*---------------------------------------------------------------------*/
;*    class-import ...                                                 */
;*---------------------------------------------------------------------*/
(define (class-import c)
   (global-import (tclass-holder c)))

;*---------------------------------------------------------------------*/
;*    classgen ...                                                     */
;*---------------------------------------------------------------------*/
(define (classgen c)
   (multiple-value-bind (pred-p pred-d)
      (classgen-predicate c)
      (multiple-value-bind (make-p make-d)
	 (classgen-make c)
	 (multiple-value-bind (access-p access-d)
	    (classgen-accessors c)
	    (values
	       `(,(class-import c)
		 ,pred-p
		 ,make-p
		 ,@access-p)
	       (cons* pred-d make-d access-d))))))
   
;*---------------------------------------------------------------------*/
;*    classgen-predicate ...                                           */
;*---------------------------------------------------------------------*/
(define (classgen-predicate c)
   
   (define (predicate-def id g)
      `(define-inline (,id obj::obj)
	  ((@ is-a? __object) obj (@ ,(global-id g) ,(global-module g)))))

   (let* ((holder (tclass-holder c))
	  (id (symbol-append (tclass-id c) '?))
	  (tid (make-typed-ident id 'bool)))
      (values
	 `(inline ,id ::obj)
	 (predicate-def tid holder))))

;*---------------------------------------------------------------------*/
;*    classgen-make ...                                                */
;*---------------------------------------------------------------------*/
(define (classgen-make c)
   
   (define (make-def id tid slots formals tformals)
      `(define-inline (,id ,@tformals)
	  (,(make-typed-ident 'instantiate tid)
	   ,@(map (lambda (s f)
		     (list (slot-id s) f))
		slots formals))))
   
   (let* ((tid (type-id c))
	  (slots (tclass-slots c))
	  (id (symbol-append 'make- tid))
	  (mk-tid (make-typed-ident id tid))
	  (f-ids (make-class-make-formals slots))
	  (f-t (map (lambda (s)
		       (make-typed-formal (type-id (slot-type s))))
		  slots))
	  (f-tids (make-class-make-typed-formals f-ids slots)))
      
      (values
	 `(inline ,mk-tid ,@f-t)
	 (make-def mk-tid tid slots f-ids f-tids))))

;*---------------------------------------------------------------------*/
;*    classgen-accessors ...                                           */
;*---------------------------------------------------------------------*/
(define (classgen-accessors c)
   
   (define (get-proto s)
      (let* ((tid (type-id c))
	     (id (symbol-append tid '- (slot-id s)))
	     (gid (make-typed-ident id (type-id (slot-type s))))
	     (o (make-typed-formal tid)))
	 `(inline ,gid ,o)))
   
   (define (get-def s)
      (let* ((tid (type-id c))
	     (sid (slot-id s))
	     (o (make-typed-ident 'o tid))
	     (id (symbol-append tid '- sid))
	     (gid (make-typed-ident id (type-id (slot-type s)))))
	 `(define-inline (,gid ,o)
	     (,(make-typed-ident 'with-access tid) o (,sid) ,sid))))
   
   (let ((slots (tclass-slots c)))
      (values (map get-proto slots) (map get-def slots))))
