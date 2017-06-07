;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/shape.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 17:02:04 1994                          */
;*    Last change :  Thu Jun  1 19:01:11 2017 (serrano)                */
;*    Copyright   :  1994-2017 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    In order to print human readable messages, we designed this      */
;*    tool.                                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_shape
   (include "Ast/node.sch"
	    "Tvector/tvector.sch")
   (import  ast_dump
	    ast_env
	    engine_param
	    tools_error)
   (export  (generic shape ::obj)))

;*---------------------------------------------------------------------*/
;*    shape ...                                                        */
;*---------------------------------------------------------------------*/
(define-generic (shape exp::obj)
   (cond
      ((class? exp)
       (format "#<class:~a>" (class-name exp)))
      ((pair? exp)
       (let loop ((exp exp))
	  (cond
	     ((null? exp)
	      '())
	     ((not (pair? exp))
	      (shape exp))
	     ((epair? exp)
	      (econs (shape (car exp)) (loop (cdr exp)) (cer exp)))
	     (else
	      (cons (shape (car exp)) (loop (cdr exp)))))))
      ((vector? exp)
       (let* ((len (vector-length exp))
	      (res (make-vector len '())))
	  (let loop ((indice 0))
	     (if (=fx indice len)
		 res
		 (begin
		    (vector-set! res
				 indice
				 (shape (vector-ref exp indice)))
		    (loop (+fx indice 1)))))))
      ((a-tvector? exp)
       (a-tvector (shape (a-tvector-type exp)) (shape (a-tvector-vector exp))))
      ((struct? exp)
       (let* ((key (struct-key exp))
	      (len (struct-length exp))
	      (res (make-struct key len '())))
	  (let loop ((indice 0))
	     (if (=fx indice len)
		 res
		 (begin
		    (struct-set! res
				 indice
				 (shape (struct-ref exp indice)))
		    (loop (+fx indice 1)))))))
      (else
       exp)))

;*---------------------------------------------------------------------*/
;*    shape ::global                                                   */
;*---------------------------------------------------------------------*/
(define-method (shape var::global)
   (let* ((str-id (if (not *name-shape?*)
		      (symbol->string (global-id var))
		      (format "~a<<~a>>" (global-id var) (global-name var))))
	  (module (symbol->string (global-module var)))
	  (type   (global-type var))
	  (tshape (if (not *type-shape?*)
		      ""
		      (string-append "::" (shape type))))
	  (ushape (if (not *user-shape?*)
		      ""
		      (if (global-user? var)
			  "-<user>"
			  "-<no-user>")))
	  (ashape (cond
		     ((not *access-shape?*)
		      "")
		     (else
		      (string-append "{" (symbol->string (global-access var))
				     "}")))))
      (cond
	 (*module-shape?*
	  (string->symbol
	   (string-append str-id "@" module tshape ushape ashape)))
	 (else
	  (case (global-bucket-position (global-id var) (global-module var))
	     ((-1)
	      (warning "global-shape:"
		       "Can't find global anymore -- "
		       `(@ ,(global-id var) ,(global-module var)))
	      (string->symbol (string-append str-id tshape ushape ashape)))
	     ((0)
	      (string->symbol (string-append str-id tshape ushape ashape)))
	     (else
	      (let ((sym (string->symbol
			  (string-append str-id tshape ushape ashape))))
		 `(@ ,sym ,(string->symbol module)))))))))

;*---------------------------------------------------------------------*/
;*    shape ::local                                                    */
;*---------------------------------------------------------------------*/
(define-method (shape var::local)
   (let* ((sym    (if *key-shape?*
		      (symbol-append (local-id var)
				     '_
				     (string->symbol
				      (integer->string (local-key var))))
		      (local-id var)))
	  (sym    (symbol->string sym))
	  (type   (local-type var))
	  (tshape (if (not *type-shape?*)
		      ""
		      (string-append "::" (shape type))))
	  (ushape (if (not *user-shape?*)
		      ""
		      (if (local-user? var)
			  "-<user>"
			  "-<no-user>")))
	  (ashape (cond
		     ((not *access-shape?*)
		      "")
		     (else
		      (string-append "{" (symbol->string (local-access var))
				     "}")))))
      (string->symbol (string-append sym tshape ushape ashape))))
   
;*---------------------------------------------------------------------*/
;*    shape ::type ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (shape type::type)
   (let* ((sp (symbol->string (type-id type)))
	  (s (if *key-shape?*
		 (string-append
		  sp
		  "-"
		  (cond-expand
		     (bigloo-c
		      (integer->string (pragma::long "(long)($1)" type) 16))
		     (else "")))
		 sp)))
      (if *typename-shape?*
	  (string-append s "\"" (type-name type) "\"")
	  s)))

;*---------------------------------------------------------------------*/
;*    shape ::node ...                                                 */
;*---------------------------------------------------------------------*/
(define-method (shape node::node)
   (node->sexp node))
