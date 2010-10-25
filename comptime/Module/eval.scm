;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/eval.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Mon Oct 25 10:18:46 2010 (serrano)                */
;*    Copyright   :  1996-2010 Manuel Serrano, see LICENSE file        */
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
	    backend_backend)
   (export  (make-eval-compiler)
	    (get-eval-libraries::pair-nil)
	    *all-eval?*
	    *all-export-eval?*
	    *all-module-eval?*
	    *all-export-mutable?*)
   (static  (wide-class evglobal::global)))

;*---------------------------------------------------------------------*/
;*    make-eval-compiler ...                                           */
;*---------------------------------------------------------------------*/
(define (make-eval-compiler)
   (instantiate::ccomp (id 'eval)
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
       (declare-global-svar! var 'eval 'eval clause #f))
      ((library . ?libs)
       (for-each (lambda (lib)
		    (if (not (symbol? lib))
			(user-error "Parse error"
				    "Illegal `eval' clause"
				    clause '())
			(set! *eval-libraries* (cons lib *eval-libraries*))))
		 libs))
      (else
       (user-error "Parse error" "Illegal `eval' clause" clause '()))))

;*---------------------------------------------------------------------*/
;*    get-eval-libraries ...                                           */
;*---------------------------------------------------------------------*/
(define (get-eval-libraries)
   *eval-libraries*)

;*---------------------------------------------------------------------*/
;*    *eval-exported* ...                                              */
;*---------------------------------------------------------------------*/
(define *eval-exported* '())

;*---------------------------------------------------------------------*/
;*    *eval-classes* ...                                               */
;*---------------------------------------------------------------------*/
(define *eval-classes* '())

;*---------------------------------------------------------------------*/
;*    *eval-libraries* ...                                             */
;*---------------------------------------------------------------------*/
(define *eval-libraries* '())

;*---------------------------------------------------------------------*/
;*    remember-eval-exported! ...                                      */
;*---------------------------------------------------------------------*/
(define (remember-eval-exported! var::symbol module loc)
   (set! *eval-exported* (cons (list var module loc) *eval-exported*)))

;*---------------------------------------------------------------------*/
;*    remember-eval-libs ...                                           */
;*---------------------------------------------------------------------*/
(define (remember-eval-libs lib)
   (set! *eval-libraries* (cons lib *eval-libraries*)))

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
   
   (if (or *one-eval?*
	   *all-eval?*
	   *all-export-eval?*
	   *all-module-eval?*
	   (pair? *eval-libraries*)
	   (pair? *eval-classes*))
       (list
	(unit
	 'eval
	 (-fx (get-toplevel-unit-weight) 2)
	 (delay
	    (let loop ((globals (get-evaluated-globals))
		       (init*  '(#unspecified)))
	       (if (null? globals)
		   `(begin
		       ;; declare the sfri
		       ,@(get-eval-srfi-libraries)
		       ;; bind the classes
		       ,@(get-evaluated-class-macros)
		       ;; the variables
		       ,@(reverse! init*)
		       ;; initialize the library module
		       ,@(map library_e *eval-libraries*)
		       #unspecified)
		   (let ((g (car globals)))
		      (set-eval-types! g)
		      (loop (cdr globals)
			    (if (evglobal? g)
				init*
				(begin
				   (widen!::evglobal g)
				   (cons (cond
					    ((svar? (global-value g))
					     (variable-access-set! g 'write)
					     (define-primop-ref->node g
						(location->node g)))
					    ((scnst? (global-value g))
					     (define-primop-ref->node g
						(location->node g)))
					    (else
					     (define-primop->node g)))
					 init*))))))))
	 #f
	 #f))
       'void))

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
;*    This function is in charge of initialization the _e library.     */
;*---------------------------------------------------------------------*/
(define (library_e lib)
   (let ((info (get-library-info lib)))
      (if (and (libinfo? info) (libinfo-module_e info))
	  (let ((init (module-initialization-id (libinfo-module_e info))))
	     (if (backend-pragma-support (the-backend))
		 `(begin
		     (pragma ,(format "~a( 0, ~s )"
				      (bigloo-module-mangle
				       (symbol->string init)
				       (symbol->string (libinfo-module_e info)))
				      (symbol->string *module*)))
		     ((@ library-mark-loaded! __library) ',lib))
		 `((@ library-mark-loaded! __library) ',lib)))
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
(define (get-evaluated-globals)
   (let* ((globals (get-evaluated-classes-accesses)))
      (if (or *all-eval?* *all-export-eval?* *all-module-eval?*)
	  (let ((scope-lst (cond
			      (*all-eval?* '(import static export))
			      (*all-module-eval?* '(static export))
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
	     (let ((var.module.pos (car eval-exported)))
		(let ((g (if (cadr var.module.pos)
			     (find-global/module (car var.module.pos)
						 (cadr var.module.pos))
			     (find-global (car var.module.pos)))))
		   (cond
		      ((not (global? g))
		       (user-error/location (find-location
					     (caddr var.module.pos))
					    "eval-init"
					    "Unbound eval variable"
					    (car var.module.pos)
					    '())
		       (loop (cdr eval-exported) res))
		      ((not (global-evaluable? g))
		       (user-error/location (find-location
					     (caddr var.module.pos))
					    "eval-init"
					    "This variable cannot be known by eval"
					    (car var.module.pos)
					    '())
		       (loop (cdr eval-exported) res))
		      (else
		       (loop (cdr eval-exported) (cons g res))))))))))

;*---------------------------------------------------------------------*/
;*    get-evaluated-classes-accesses ...                               */
;*---------------------------------------------------------------------*/
(define (get-evaluated-classes-accesses)
   (let ((err '())
	 (res '()))
      (with-exception-handler
	 (lambda (e)
	    (error-notify e)
	    (set! err (cons (&error-obj e) err)))
	 (lambda ()
	    (if (null? *eval-classes*)
		'()
		(set! res
		      (append
		       (append-map get-evaluated-class-accesses *eval-classes*)
		       res)))))
      (if (pair? err)
	  (error 'eval "Undefined classes found" err)
	  res)))

;*---------------------------------------------------------------------*/
;*    get-evaluated-class-accesses ...                                 */
;*---------------------------------------------------------------------*/
(define (get-evaluated-class-accesses ev)
   (define (get-global id)
      (let ((g (find-global id)))
	 (if (global? g)
	     g
	     (internal-error 'eval "Can't find global access" id))))
   (define (access s id)
      (let ((i (slot-id s)))
	 (if (slot-indexed s)
	     (let* ((gidl (symbol-append id '- i '-len))
		    (gid (symbol-append id '- i '-ref))
		    (sid (symbol-append id '- i '-set!))
		    (gl (get-global gidl))
		    (g (get-global gid)))
		(if (slot-read-only? s)
		    (list gl gl)
		    (list gl g (get-global sid))))
	     (let* ((gid (symbol-append id '- i))
		    (sid (symbol-append gid '-set!))
		    (g (get-global gid)))
		(if (slot-read-only? s)
		    (list g)
		    (list g (get-global sid)))))))
   (match-case ev
      ((class ?id library)
       '())
      ((class ?id)
       (let ((t (find-type/location id (find-location ev))))
	  (if (not (tclass? t))
	      (user-error/location (find-location ev)
				   'eval
				   "Referenced type is not a Bigloo class"
				   id)
	      (let* ((slots (map (lambda (s) (access s id))
				 (filter (lambda (s)
					    (eq? (slot-class-owner s) t))
					 (tclass-all-slots t))))
		     (commons (cons* (get-global (class-predicate t))
				     (get-global (class-nil-constructor t))
				     (apply append slots))))
		 (if (tclass-abstract? t)
		     commons
		     (cons* (get-global (class-make t))
			    (get-global (class-allocate t))
			    (get-global (class-fill t))
			    (tclass-holder t)
			    commons))))))
      (else
       (internal-error 'eval "(eval (class ...)) malformed" ev))))

;*---------------------------------------------------------------------*/
;*    get-eval-srfi-libraries ...                                      */
;*---------------------------------------------------------------------*/
(define (get-eval-srfi-libraries)
   (map (lambda (l)
	   `(begin
	       (register-eval-srfi! ',l)))
	*eval-libraries*))

;*---------------------------------------------------------------------*/
;*    eval-bind-super-access ...                                       */
;*---------------------------------------------------------------------*/
(define (eval-bind-super-access t libraryp)
   (let ((slots (tclass-all-slots t))
	 (id (tclass-id t)))
      (define (slot-bind s)
	 (let* ((i (slot-id s))
		(ssi (symbol-append (tclass-id (slot-class-owner s)) '- i))
		(asi (symbol-append id '- i))
		(get `'(define (,asi o) (,ssi o))))
	    (if (slot-read-only? s)
		(list get)
		(let ((sssi (symbol-append ssi '-set!))
		      (sasi (symbol-append asi '-set!)))
		   (list get `'(define (,sasi o x) (,sssi o x)))))))
      (define (slot-bind-compiled s)
	 (let* ((i (slot-id s))
		(ssi (symbol-append (tclass-id (slot-class-owner s)) '- i))
		(get (find-global ssi *module*)))
	    (if (slot-read-only? s)
		(list (define-primop->node get))
		(let ((set (find-global (symbol-append ssi '-set!) *module*)))
		   (list (define-primop->node get)
			 (define-primop->node set))))))
      (if libraryp
	  (append-map (lambda (s)
			 (if (not (eq? (slot-class-owner s) t))
			     (slot-bind s)
			     (slot-bind-compiled s)))
		      (tclass-all-slots t))
	  (map (lambda (e) `(eval! ,e))
	       (append-map (lambda (s)
			      (if (not (eq? (slot-class-owner s) t))
				  (slot-bind s)
				  '()))
			   (tclass-all-slots t))))))

;*---------------------------------------------------------------------*/
;*    get-evaluated-class-macros ...                                   */
;*---------------------------------------------------------------------*/
(define (get-evaluated-class-macros)
   (map (lambda (s)
	   (let* ((t (find-type/location (cadr s) (find-location s)))
		  (id (tclass-id t))
		  (libp (pair? (cddr s))))
	      `(begin
		  ,(if (tclass-abstract? t)
		       #unspecified
		       `(begin
			   (eval! (eval-expand-instantiate ',id))
			   (eval! (eval-expand-duplicate ',id))))
		  (eval! (eval-expand-with-access ',id))
		  ,@(eval-bind-super-access t libp))))
	*eval-classes*))
