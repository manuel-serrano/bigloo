;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Module/foreign.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  4 16:28:03 1996                          */
;*    Last change :  Thu Apr 11 13:55:49 2013 (serrano)                */
;*    Copyright   :  1996-2013 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The foreign and extern clauses compilation. Foreign and extern   */
;*    clauses only differs by their syntax. They play the same role    */
;*    and have equivalent semantic. I kept foreign clauses for upward  */
;*    compatibility with Bigloo 1.8.                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module module_foreign
   (include "Ast/unit.sch"
	    "Tools/trace.sch")
   (import  module_module
	    module_checksum
	    engine_param
	    backend_backend
	    ast_glo-decl
	    tools_error
	    tools_shape
	    type_type
	    type_env
	    type_tools
	    type_cache
	    ast_var
	    ast_env
	    ast_ident
	    foreign_ctype
	    foreign_access
	    (find-location tools_location))
   (export  (make-foreign-compiler)
	    (make-extern-compiler)
	    (foreign-accesses-add! ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    make-foreign-compiler ...                                        */
;*---------------------------------------------------------------------*/
(define (make-foreign-compiler)
   (instantiate::ccomp
      (id 'foreign)
      (producer foreign-producer)
      (consumer (lambda (m c) (foreign-producer c)))
      (finalizer foreign-finalizer)))

;*---------------------------------------------------------------------*/
;*    make-extern-compiler ...                                         */
;*---------------------------------------------------------------------*/
(define (make-extern-compiler)
   (instantiate::ccomp
      (id 'extern)
      (producer (lambda (c) (extern-producer c #t)))
      (consumer (lambda (m c) (extern-producer c #f)))))

;*---------------------------------------------------------------------*/
;*    foreign-producer ...                                             */
;*---------------------------------------------------------------------*/
(define (foreign-producer clause)
   (if (memq 'foreign (backend-foreign-clause-support (the-backend)))
       (match-case clause
	  ((?- . ?protos)
	   (for-each (lambda (p) (foreign-parser p #t)) protos)
	   '())
	  (else
	   (user-error "Parse error"
		       (string-append "Illegal `foreign' clause")
		       clause
		       '())))
       '()))

;*---------------------------------------------------------------------*/
;*    extern-producer ...                                              */
;*---------------------------------------------------------------------*/
(define (extern-producer clause exportp)
   (if (memq 'extern (backend-foreign-clause-support (the-backend)))
       (match-case clause
	  ((?- . ?protos)
	   (for-each (lambda (p) (extern-parser p exportp)) protos)
	   '())
	  (else
	   (user-error "Parse error"
		       (string-append "Illegal `extern' clause")
		       clause
		       '())))
       '()))

;*---------------------------------------------------------------------*/
;*    check-c-args? ...                                                */
;*---------------------------------------------------------------------*/
(define (check-c-args? proto)
   (let loop ((proto proto))
      (cond
	 ((null? proto)
	  #t)
	 ((symbol? proto)
	  #t)
	 ((not (pair? proto))
	  #f)
	 ((not (symbol? (car proto)))
	  #f)
	 (else
	  (loop (cdr proto))))))

;*---------------------------------------------------------------------*/
;*    foreign-parser ...                                               */
;*---------------------------------------------------------------------*/
(define (foreign-parser foreign exportp)
   (trace (ast 2) "foreign parser: " foreign #\Newline)
   (match-case foreign
      ((include ?string)
       (if (not (string? string))
	   (user-error "Parse error" "Illegal `include' clause" foreign '())
	   (if (not (member string *include-foreign*))
	       (set! *include-foreign* (cons string *include-foreign*)))))
      ((type . ?-)
       (parse-c-foreign-type foreign))
      ((export (and (? symbol?) ?bname) (and (? string?) ?cname))
       (set! *foreign-exported*
	  (cons (cons foreign exportp) *foreign-exported*)))
      (((or export type include) . ?-)
       ;; an error
       (user-error "Parse error" "Illegal foreign form" foreign '()))
      ((or (macro ?type ?l-name ?proto ?c-name)
	   (infix macro ?type ?l-name ?proto ?c-name))
       (if (not (and (string? c-name)
		     (symbol? type)
		     (symbol? l-name)
		     (check-c-args? proto)))
	   (user-error "Parse error" "Illegal `macro' form" foreign '())
	   (declare-global-cfun! l-name #f 'foreign c-name
	      type proto (eq? (car foreign) 'infix) #t foreign foreign)))
      ((macro ?type ?l-name ?c-name)
       (if (not (and (string? c-name)
		     (symbol? type)
		     (symbol? l-name)))
	   (user-error "Parse error" "Illegal `macro' form" foreign '())
	   (declare-global-cvar! l-name #f c-name type #t foreign foreign)))
      ((macro . ?-)
       (user-error "Parse error" "Illegal foreign form" foreign '()))
      ((?type ?l-name ?proto ?c-name)
       (if (not (and (string? c-name)
		     (symbol? type)
		     (symbol? l-name)
		     (check-c-args? proto)))
	   (user-error "Parse error" "Illegal `function' form" foreign '())
	   (declare-global-cfun! l-name #f 'foreign c-name
	      type proto #f #f foreign foreign)))
      ((?type ?l-name ?c-name)
       (if (not (and (string? c-name)
		     (symbol? type)
		     (symbol? l-name)))
	   (user-error "Parse error" "Illegal `variable' form" foreign '())
	   (declare-global-cvar! l-name #f c-name type #f foreign #f)))
      (else
       (user-error "Parse error" "Illegal foreign form" foreign '()))))

;*---------------------------------------------------------------------*/
;*    extern-parser ...                                                */
;*---------------------------------------------------------------------*/
(define (extern-parser extern exportp)
   (trace (ast 2) "extern parser: " extern #\Newline)
   (match-case extern
      ((type . ?-)
       ;; type clauses
       (parse-c-extern-type extern))
      (((or export include) . ?-)
       (foreign-parser extern exportp))
      ((or (macro (and (? symbol?) ?id) ?proto (and (? string?) ?cn))
	   (infix macro (and (? symbol?) ?id) ?proto (and (? string?) ?cn)))
       ;; macro function definitions
       (let* ((pid  (parse-id id (find-location extern)))
	      (ln   (car pid))
	      (type (type-id (default-c-type (cdr pid) extern))))
	  (if (or (not (check-id pid extern))
		  (not (check-c-args? proto)))
	      (user-error "Parse error" "Illegal extern form" extern '())
	      (let ((infix? (eq? (car extern) 'infix)))
		 (declare-global-cfun! ln #f 'foreign
		    cn type proto infix? #t extern #f)))))
      ((macro (and (? symbol?) ?id) (and (? string?) ?c-name))
       ;; macro variable definitions
       (let* ((pid    (parse-id id (find-location extern)))
	      (l-name (car pid))
	      (type   (type-id (default-c-type (cdr pid) extern))))
	  (if (not (check-id pid extern))
	      (user-error "Parse error" "Illegal extern form" extern '())
	      (declare-global-cvar! l-name #f c-name type #t extern #f))))
      ((macro . ?-)
       (user-error "Parse error" "Illegal extern form" extern '()))
      (((and (? symbol?) ?id) ?proto (and (? string?) ?cn))
       ;; function definitions
       (let* ((pid  (parse-id id (find-location extern)))
	      (ln   (car pid))
	      (type (type-id (default-c-type (cdr pid) extern))))
	  (if (or (not (check-id pid extern))
		  (not (check-c-args? proto)))
	      (user-error "Parse error" "Illegal extern form" extern '())
	      (declare-global-cfun! ln #f 'foreign
		 cn type proto #f #f extern #f))))
      (((and (? symbol?) ?id) (and (? string?) ?c-name))
       ;; variable definitions
       (let* ((pid    (parse-id id (find-location extern)))
	      (l-name (car pid))
	      (type   (type-id (default-c-type (cdr pid) extern))))
	  (if (not (check-id pid extern))
	      (user-error "Parse error" "Illegal extern form" extern '())
	      (declare-global-cvar! l-name #f c-name type #f extern #f))))
      (else
       (user-error "Parse error" "Illegal extern form" extern '()))))

;*---------------------------------------------------------------------*/
;*    parse-c-foreign-type ...                                         */
;*---------------------------------------------------------------------*/
(define (parse-c-foreign-type type)
   (match-case type
      ((type (and (? symbol?) ?id) (and (? string?) ?name))
       (declare-type! id name 'C))
      ((type (and (? symbol?) ?id) ?t-exp (and (? string?) ?name))
       (if (check-c-foreign-type-exp? t-exp)
	   (let ((ctype (declare-c-type! type id t-exp name))
		 (loc   (find-location type)))
	      ;; declare-c-type! can return otherthing than a type
	      ;; (for instance on type redefinition).
	      (if (type? ctype)
		  (let ((accesses (make-ctype-accesses! ctype ctype loc)))
		     (foreign-accesses-add! accesses)
		     ;; if the declared type is an alias to a structure
		     ;; we automatically create the pending corresponding
		     ;; aliasing.
		     (if (and (calias? ctype) (cstruct? (type-alias ctype)))
			 (parse-c-foreign-type
			  `(type ,(symbol-append id '*)
				 ,(symbol-append t-exp '*)
				 ,(make-pointer-to-name ctype))))))
	      #unspecified)
	   (user-error "Parse error" "Illegal `C foreign type'" type '())))
      (else
       (user-error "Parse error" "Illegal `C foreign type'" type '()))))
 
;*---------------------------------------------------------------------*/
;*    check-c-foreign-type-exp? ...                                    */
;*---------------------------------------------------------------------*/
(define (check-c-foreign-type-exp? t-exp)   
   (match-case t-exp
      ((? symbol?)
       #t)
      (((or union struct) . ?slots)
       (let loop ((slots slots))
	  (if (null? slots)
	      #t
	      (match-case (car slots)
		 (((? symbol?) (? symbol?) (? string?))
		  (loop (cdr slots)))
		 (else
		  #f)))))
      ((opaque)
       #t)
      ((pointer (? symbol?))
       #t)
      (((or struct* union*) (? symbol?))
       #t)
      ((array (? symbol?))
       #t)
      ((function (? symbol?) (and (or () (? pair?)) ?t-exp))
       (check-c-args? t-exp))
      ((enum . ?slots)
       (let loop ((slots slots))
	  (if (null? slots)
	      #t
	      (match-case (car slots)
		 (((? symbol?) (? string?))
		  (loop (cdr slots)))
		 (else
		  #f)))))
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    parse-c-extern-type ...                                          */
;*---------------------------------------------------------------------*/
(define (parse-c-extern-type type)
   (match-case type
      ((type (and (? symbol?) ?id) (and (? string?) ?name))
       (parse-c-foreign-type type))
      ((type (and (? symbol?) ?id) ?t-exp (? string?))
       (let ((foreign-type (c-extern-type->c-foreign-type t-exp)))
	  (if foreign-type
	      (parse-c-foreign-type type)
	      (user-error "Parse error" "Illegal `C extern type'" type '()))))
      (else
       (user-error "Parse error" "Illegal `C extern type'" type '()))))
 
;*---------------------------------------------------------------------*/
;*    c-extern-type->c-foreign-type ...                                */
;*---------------------------------------------------------------------*/
(define (c-extern-type->c-foreign-type t-exp)   
   (match-case t-exp
      ((? symbol?)
       t-exp)
      (((or union struct) . ?slots)
       (let loop ((slots slots))
	  (if (null? slots)
	      t-exp
	      (match-case (car slots)
		 (((and ?id (? symbol?)) (and ?name (? string?)))
		  (let* ((pid  (parse-id id (find-location t-exp)))
			 (sid  (car pid))
			 (type (type-id (cdr pid))))
		     (if (not (check-id pid t-exp))
			 #f
			 (begin
			    (set-cdr! (car slots) (cons sid (cons name '())))
			    (set-car! (car slots) type)
			    (loop (cdr slots))))))
		 (else
		  #f)))))
      ((pointer (? symbol?))
       t-exp)
      (((or struct* union*) (? symbol?))
       t-exp)
      ((array (? symbol?))
       t-exp)
      ((function (? symbol?) (and (or () (? pair?)) ?t-args))
       (if (check-c-args? t-args)
	   t-exp
	   #f))
      ((enum . ?slots)
       (let loop ((slots slots))
	  (if (null? slots)
	      t-exp
	      (match-case (car slots)
		 (((? symbol?) (? string?))
		  (loop (cdr slots)))
		 (else
		  #f)))))
      ((opaque)
       t-exp)
      (else
       #f)))

;*---------------------------------------------------------------------*/
;*    *foreign-accesses* ...                                           */
;*---------------------------------------------------------------------*/
(define *foreign-accesses* '())

;*---------------------------------------------------------------------*/
;*    *foreign-exported* ...                                           */
;*---------------------------------------------------------------------*/
(define *foreign-exported* '())

;*---------------------------------------------------------------------*/
;*    foreign-accesses-add! ...                                        */
;*---------------------------------------------------------------------*/
(define (foreign-accesses-add! accesses)
   (set! *foreign-accesses* (append accesses *foreign-accesses*)))

;*---------------------------------------------------------------------*/
;*    foreign-finalizer ...                                            */
;*---------------------------------------------------------------------*/
(define (foreign-finalizer)
   ;; we patch bigloo foreign exported variables
   (for-each (lambda (foreign)
		(let* ((fo (car foreign))
		       (ex (cdr foreign))
		       (global (find-global (cadr fo)))
		       (name (caddr fo)))
		   (cond
		      ((not (global? global))
		       (when ex
			  (if (not (or (eq? *pass* 'make-add-heap)
				       (eq? *pass* 'make-heap)))
			      (user-error "Foreign"
				 (format "Unbound global variable \"~a\""
				    (cadr fo))
				 foreign
				 '()))))
		      ((string? (global-name global))
		       (user-warning
			"Foreign"
			"Re-exportation of global variable (ignored)"
			foreign))
		      (else
		       (global-name-set! global name)))))
	     *foreign-exported*)
   (set! *foreign-exported* '())
   ;; and we build a unit if Foreign types have generated codes
   (if (null? *foreign-accesses*)
       'void
       (let ((accesses (reverse! *foreign-accesses*)))
	  (set! *foreign-accesses* '())
	  (list (unit 'foreign 48 accesses #t #f)))))
	  
;*---------------------------------------------------------------------*/
;*    default-c-type ...                                               */
;*---------------------------------------------------------------------*/
(define (default-c-type type src)
   (if (eq? type (get-default-type))
       (let ((default (get-default-c-type)))
	  (user-warning/location (find-location src)
				 "Foreign"
				 "Unspecified C type, using type"
				 (type-id default))
	  default)
       type))
   
