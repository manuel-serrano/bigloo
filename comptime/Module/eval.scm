;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Module/eval.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Thu Mar 19 12:09:34 2020 (serrano)                */
;*    Copyright   :  1996-2020 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The eval clauses compilation.                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_eval
   (include "Ast/unit.sch"
	    "Ast/node.sch"
	    "Module/libinfo.sch")
   (import  module_module
	    module_include
	    engine_param
	    tools_shape
	    tools_error
	    tools_location
	    type_cache
	    type_env
	    object_class
	    object_slots
	    ast_env
	    ast_glo-decl
	    ast_sexp
	    ast_ident
	    backend_backend)
   (export  (make-eval-compiler)
	    (get-eval-libraries::pair-nil)
	    (add-eval-library! ::symbol)
	    *all-eval?*
	    *all-export-eval?*
	    *all-module-eval?*
	    *all-export-mutable?*))

;*---------------------------------------------------------------------*/
;*    make-eval-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-eval-compiler)
   (instantiate::ccomp
      (id 'eval)
      (producer eval-producer)
      (finalizer eval-finalizer)))

;*---------------------------------------------------------------------*/
;*    eval-producer ...                                                */
;*---------------------------------------------------------------------*/
(define (eval-producer clause)
   (match-case clause
      ((?- . ?protos)
       (for-each (lambda (proto) (eval-parser proto clause)) protos)
       '())
      (else
       (user-error "Parse error"
		   (string-append "Illegal `eval' clause")
		   clause
		   '()))))
   
;*---------------------------------------------------------------------*/
;*    eval-parser ...                                                  */
;*---------------------------------------------------------------------*/
(define (eval-parser proto clause)
   (match-case proto
      ((export-all)
       (set! *all-eval?* #t))
      ((export-exports)
       (set! *all-export-eval?* #t))
      ((export-module)
       (set! *all-module-eval?* #t))
      ((export (and (? symbol?) ?var))
       (set! *one-eval?* #t)
       (remember-eval-exported! var #f proto))
      ((export (@ (and (? symbol?) ?var) (and (? symbol?) ?module)))
       (set! *one-eval?* #t)
       (remember-eval-exported! var module proto))
      ((class (and (? symbol?) ?class))
       (set! *eval-classes* (cons proto *eval-classes*)))
      ((import (and (? symbol?) ?var))
       (declare-global-svar! var #f 'eval 'eval clause #f))
      ((library . ?libs)
       (for-each (lambda (lib)
		    (if (not (symbol? lib))
			(user-error "Parse error"
				    "Illegal `eval' clause"
				    clause '())
			(add-eval-library! lib)))
		 libs))
      (else
       (user-error "Parse error" "Illegal `eval' clause" clause '()))))

;*---------------------------------------------------------------------*/
;*    *eval-libraries* ...                                             */
;*---------------------------------------------------------------------*/
(define *eval-libraries* '())

;*---------------------------------------------------------------------*/
;*    get-eval-libraries ...                                           */
;*---------------------------------------------------------------------*/
(define (get-eval-libraries)
   *eval-libraries*)

;*---------------------------------------------------------------------*/
;*    add-eval-library! ...                                            */
;*---------------------------------------------------------------------*/
(define (add-eval-library! lib)
   (set! *eval-libraries* (cons lib *eval-libraries*)))

;*---------------------------------------------------------------------*/
;*    *eval-exported* ...                                              */
;*---------------------------------------------------------------------*/
(define *eval-exported* '())

;*---------------------------------------------------------------------*/
;*    *eval-classes* ...                                               */
;*---------------------------------------------------------------------*/
(define *eval-classes* '())

;*---------------------------------------------------------------------*/
;*    remember-eval-exported! ...                                      */
;*---------------------------------------------------------------------*/
(define (remember-eval-exported! var::symbol module loc)
   (set! *eval-exported* (cons (list var module loc) *eval-exported*)))

;*---------------------------------------------------------------------*/
;*    *all-eval?* ...                                                  */
;*---------------------------------------------------------------------*/
(define *all-eval?* #f)
(define *all-export-eval?* #f)
(define *all-module-eval?* #f)
(define *all-export-mutable?* #f)

;*---------------------------------------------------------------------*/
;*    *one-eval?* ...                                                  */
;*---------------------------------------------------------------------*/
(define *one-eval?* #f)

;*---------------------------------------------------------------------*/
;*    eval-finalizer ...                                               */
;*---------------------------------------------------------------------*/
(define (eval-finalizer)
   
   (define (eval-export-global g)
      (global-eval?-set! g #t)
      (cond
	 ((svar? (global-value g))
	  (variable-access-set! g 'write)
	  (define-primop-ref/src->node g (location->node g) (global-src g)))
	 ((scnst? (global-value g))
	  (define-primop-ref->node g (location->node g)))
	 (else
	  (define-primop->node g))))
   
   (if (or *one-eval?*
	   *all-eval?*
	   *all-export-eval?*
	   *all-module-eval?*
	   (pair? *eval-libraries*)
	   (pair? *eval-classes*))
       (list
	  (unit 'eval
	     (-fx (get-toplevel-unit-weight) 2)
	     (delay
		(let loop ((globals (append (get-evaluated-globals
					       (cond
						  (*all-eval?* 'all)
						  (*all-export-eval?* 'export)
						  (else 'one)))
				       (get-evaluated-class-holders)))
			   (init*  '(#unspecified)))
		   (if (null? globals)
		       `(begin
			   ;; initialize the library module
			   ,@(map library_e *eval-libraries*)
			   ;; declare the sfri
			   ,@(get-eval-srfi-libraries)
			   ;; bind the classes
			   ,@(get-evaluated-class-macros)
			   ;; the variables
			   ,@(reverse! init*)
			   ;; the module
			   ,@(evmodule-comp)
			   #unspecified)
		       (let ((g (car globals)))
			  (set-eval-types! g)
			  (loop (cdr globals)
			     (if (global-eval? g)
				 init*
				 (cons (eval-export-global g) init*)))))))
	     #f
	     #f))
       'void))

;*---------------------------------------------------------------------*/
;*    evmodule-comp ...                                                */
;*---------------------------------------------------------------------*/
(define (evmodule-comp)
   
   (define (export-global g)
      (global-eval?-set! g #t)
      (let ((id (global-id g)))
	 (cond
	    ((svar? (global-value g))
	     (variable-access-set! g 'write)
	     `(vector 1 ',id (__evmeaning_address ,id) #f #f))
	    ((scnst? (global-value g))
	     `(vector 1 ',id (__evmeaning_address ,id) #f #f))
	    (else
	     `(vector 0 ',id ,id #f #f)))))
   
   (if *all-module-eval?*
       (list
	  (sexp->node
	     `((@ evmodule-comp! __evmodule)
	       ',*module* ',*src-files* ',*module-location*
	       ,@(map export-global (get-evaluated-globals 'module)))
	     '() #f 'value))
       '()))

;*---------------------------------------------------------------------*/
;*    get-library-info ...                                             */
;*---------------------------------------------------------------------*/
(define (get-library-info library)
   (or (library-info library)
       (let* ((init (library-init-file library))
	      (path (find-file/path init *lib-dir*)))
	  (if path
	      (begin
		 (loadq path)
		 (let ((i (library-info library)))
		    (if i
			i
			(warning library (format "cannot find info \"~a\"" init)))))
	      (warning library (format "cannot find file \"~a\" in lib path" init))))))

;*---------------------------------------------------------------------*/
;*    library_e ...                                                    */
;*    -------------------------------------------------------------    */
;*    This function is in charge of initializing the _e library.       */
;*---------------------------------------------------------------------*/
(define (library_e lib)
   (let ((info (get-library-info lib)))
      (if (and (libinfo? info) (libinfo-module_e info))
	  (let* ((init (module-initialization-id (libinfo-module_e info)))
		 (glo (declare-global-cfun! init
			 init
			 (libinfo-module_e info)
			 (bigloo-module-mangle
			    (symbol->string init)
			    (symbol->string (libinfo-module_e info)))
			 'obj '(int string) #f #f #f #f)))
	     (if (backend-pragma-support (the-backend))
		 `(begin
		     ((@ library-load-init __library) ',lib (bigloo-library-path))
		     (,glo 0 (pragma::string ,(format "~s" (symbol->string *module*))))
		     ((@ library-mark-loaded! __library) ',lib))
		 `((@ library-load __library) ',lib)))
	  (warning lib "cannot initialize library for eval"))))
      
;*---------------------------------------------------------------------*/
;*    set-eval-types! ...                                              */
;*    -------------------------------------------------------------    */
;*    Global variables send to eval must be obj variable. This         */
;*    function enforce that.                                           */
;*---------------------------------------------------------------------*/
(define (set-eval-types! global)
   (let ((val (global-value global)))
      (if (not (sfun? val))
	  (let ((type (global-type global)))
	     (cond
		((eq? type *_*)
		 (global-type-set! global *obj*))
		((not (bigloo-type? type))
		 (error "eval"
			"Non bigloo prototyped value can't be evaluated"
			(global-id global))))))))
   
;*---------------------------------------------------------------------*/
;*    get-evaluated-globals ...                                        */
;*---------------------------------------------------------------------*/
(define (get-evaluated-globals scope)
   (let* ((globals (get-evaluated-classes-accesses)))
      (when (memq scope '(all module export))
	 (let ((scope-lst (case scope
			     ((all) '(import static export))
			     ((module) '(export))
			     (else '(export)))))
	    (for-each-global!
	       (lambda (g)
		  (if (and (memq (global-import g) scope-lst)
			   (global-evaluable? g)
			   (or *lib-mode* (not (global-library g))))
		      (set! globals (cons g globals)))))))
      (let loop ((eval-exported *eval-exported*)
		 (res globals))
	 (if (null? eval-exported)
	     res
	     (let ((var-module-pos (car eval-exported)))
		(let ((g (if (cadr var-module-pos)
			     (find-global/module (car var-module-pos)
				(cadr var-module-pos))
			     (find-global (car var-module-pos)))))
		   (cond
		      ((not (global? g))
		       (user-error/location (find-location
					       (caddr var-module-pos))
			  "eval-init"
			  "Unbound eval variable"
			  (car var-module-pos)
			  '())
		       (loop (cdr eval-exported) res))
		      ((not (global-evaluable? g))
		       (user-error/location (find-location
					       (caddr var-module-pos))
			  "eval-init"
			  "This variable cannot be known by eval"
			  (car var-module-pos)
			  '())
		       (loop (cdr eval-exported) res))
		      (else
		       (loop (cdr eval-exported) (cons g res))))))))))

;*---------------------------------------------------------------------*/
;*    get-evaluated-classes-accesses ...                               */
;*---------------------------------------------------------------------*/
(define (get-evaluated-classes-accesses)
   '())

;*---------------------------------------------------------------------*/
;*    get-eval-srfi-libraries ...                                      */
;*---------------------------------------------------------------------*/
(define (get-eval-srfi-libraries)
   (map (lambda (l)
	   `(begin
	       (register-eval-srfi! ',l)))
	*eval-libraries*))

;*---------------------------------------------------------------------*/
;*    get-evaluated-class-macros ...                                   */
;*---------------------------------------------------------------------*/
(define (get-evaluated-class-macros)
   (map (lambda (s)
	   (let* ((t (find-type/location (cadr s) (find-location s)))
		  (id (tclass-id t))
		  (libp (pair? (cddr s)))
		  (holder (tclass-holder t))
		  (holdere `(@ ,(global-id holder) ,(global-module holder))))
	      `(begin
		  ,(if (tclass-abstract? t)
		       #unspecified
		       `(begin
			   (eval-expand-instantiate ,holdere)
			   (eval-expand-duplicate ,holdere)))
		  (eval-expand-with-access ,holdere)
		  '())))
	*eval-classes*))

;*---------------------------------------------------------------------*/
;*    get-evaluated-class-holders ...                                  */
;*---------------------------------------------------------------------*/
(define (get-evaluated-class-holders)
   (map (lambda (s)
	   (let ((t (find-type/location (cadr s) (find-location s))))
	      (tclass-holder t)))
      *eval-classes*))
	      
