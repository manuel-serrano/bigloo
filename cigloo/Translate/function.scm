;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/function.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov 28 09:49:40 1995                          */
;*    Last change :  Sun Dec 30 09:11:05 2007 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The function definition and declaration translation.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_function
   (include "Parser/coord.sch"
	    "Translate/ast.sch"
	    "Translate/type.sch"
	    "Translate/function.sch")
   (import  engine_param
	    translate_type
	    translate_decl
	    translate_tspec
	    translate_eval
	    translate_ident
	    tools_speek
	    tools_error)
   (export  (translate-function-definition  <ast>)
	    (translate-function-declaration <decl> <spec> <para-list>)
	    (translate-function-declarations)
	    (detect-struct-or-union <type>)
	    (parameter-type-list->types     <ptl>)))

;*---------------------------------------------------------------------*/
;*    *fun-decl-list* ...                                              */
;*---------------------------------------------------------------------*/
(define *fun-decl-list* '())

;*---------------------------------------------------------------------*/
;*    translate-function-declaration ...                               */
;*    -------------------------------------------------------------    */
;*    Function declaration are delayed (in order to wait for           */
;*    the function declaration), then this function just store         */
;*    in a list the function declaration.                              */
;*---------------------------------------------------------------------*/
(define (translate-function-declaration decl spec para-list)
   (let ((fun-decl (fun-decl decl spec para-list)))
      (set! *fun-decl-list* (cons fun-decl *fun-decl-list*))))

;*---------------------------------------------------------------------*/
;*    translate-function-declarations ...                              */
;*---------------------------------------------------------------------*/
(define (translate-function-declarations)
   (define (do-translate-function-declaration fd)
      (let* ((decl      (fun-decl-decl fd))
	     (spec      (fun-decl-spec fd))
	     (para-list (fun-decl-para-list fd))
	     (tspec     (type-spec-of-decl-spec spec))
	     (type      (type+decl->type (tspec->type tspec) decl))
	     (f-ident   (get-decl-ident decl))
	     (f-id      (ident-id f-ident)))
	 (verbose 2
		  "do-translate-function-declaration: " #\Newline
		  "   decl: " decl #\Newline
		  "   spec: " spec #\Newline)
	 [assert check (type) (function-t? type)]
	 (let ((sf-id (string->symbol (string-upcase f-id))))
	    (if (not (getprop sf-id 'fun-processed))
		(begin
		   (putprop! sf-id 'fun-processed #t)
		   (verbose 1 "   " f-id #\Newline)
		   (fprin *oport* "   ("
			  (if *macro-function* "macro " "")
			  (ident->ident f-id) "::" 
			  (type-id (function-t-to type))
			  " ")
		   (add-eval-function!
		    f-id
		    (translate-parameter
		     f-ident
		     (list 'parameter-type-list para-list)
		     #f))
		   (fprint *oport* " \"" f-id "\")"))))))
   (for-each do-translate-function-declaration (reverse *fun-decl-list*))
   (set! *fun-decl-list* '()))

;*---------------------------------------------------------------------*/
;*    translate-function-definition ...                                */
;*---------------------------------------------------------------------*/
(define (translate-function-definition ast)
   (let* ((fun-decl      (fun-def-decl ast))
	  (fun-ident     (get-decl-ident fun-decl))
	  (fun-para-decl (get-decl-para-decl fun-decl))
	  (fun-id        (ident-id fun-ident))
	  (sfun-id       (string->symbol (string-upcase fun-id)))
	  (fun-body      (fun-def-body ast)))
      (if (not fun-para-decl)
	  (error/ast fun-id "incorrect function definition" fun-ident)
	  (begin
	     (verbose 1 "   " fun-id)
	     (let ((sspec (storage-class-spec-of-decl-spec
			   (fun-def-decl-spec ast))))
		;; we check the correctness of the storage class specifier
		;; and we check if we skip this definition
		(cond
		   ((not (correct-storage-class-spec? sspec))
		    ;; it is not correct
		    (error/ast (storage-class-spec-value (car sspec))
			       "multiple storage classes in declaration"
			       (car sspec)))
		   ((or (fun-def-processed? ast)
			(getprop sfun-id 'fun-processed))
		    (verbose 1 " (already done)" #\Newline)
		    #unspecified)
		   ((and (pair? sspec)
			 (case (storage-class-spec-value (car sspec))
			    ((static)
			     #t)
			    (else
			     #f)))
		    ;; we ignore this definition
		    (verbose 1 " (ignored because "
			     (storage-class-spec-value (car sspec))
			     #\) #\Newline)
		    #unspecified)
		   (else
		    (putprop! sfun-id 'fun-processed #t)
		    (fun-def-processed?-set! ast #t)
		    (let ((tspec (type-spec-of-decl-spec
				  (fun-def-decl-spec ast))))
		       (verbose 1 #\Newline)
		       (display "   (" *oport*)
		       (if *macro-function* "macro " "")
		       (let ((type (type+decl->type (tspec->type tspec)
						    fun-decl)))
			  [assert check (type) (function-t? type)]
			  (begin
			     (display (ident->ident fun-id) *oport*)
			     (display "::" *oport*)
			     (display (type-id (function-t-to type)) *oport*)
			     (display #\space *oport*)
			     (add-eval-function!
			      fun-id
			      (translate-parameter fun-ident
						   fun-para-decl
						   fun-body))
			     (display " \"" *oport*)
			     (display fun-id *oport*)
			     (display #\" *oport*))
			  (fprint *oport* #\)))))))))))

;*---------------------------------------------------------------------*/
;*    translate-parameter ...                                          */
;*---------------------------------------------------------------------*/
(define (translate-parameter fun-ident para-decl body)
   (match-case para-decl
      ((parameter-identifier-list ?list)
       (translate-para-id fun-ident list body))
      ((parameter-type-list ?list)
       (translate-para-type list))
      (else
       (error "internal-error" "translate-parameter" para-decl))))

;*---------------------------------------------------------------------*/
;*    translate-para-id ...                                            */
;*---------------------------------------------------------------------*/
(define (translate-para-id fun-ident ident-list body)
   (verbose 3 "translate-para-id: " #\Newline)
   (verbose 3 "para-decl: " ident-list #\Newline)
   (verbose 3 "body     : " body #\Newline)
   ;; we compute from body a list of `(id . type);
   (let ((id-types (let loop ((body body)
			      (res  '()))
		      (cond
			 ((null? body)
			  res)
			 ((not (declare? (car body)))
			  res)
			 (else
			  (let* ((declare (car body))
				 (spec    (declare-spec declare))
				 (idcl    (declare-init-decl-list declare))
				 (decl    (match-case idcl
					     ((?decl . ?-)
					      decl)
					     (?decl
					      decl)))
				 (tspec   (type-spec-of-decl-spec spec))
				 (ident   (get-decl-ident decl))
				 (id      (ident-id ident))
				 (type    (type+decl->type (tspec->type tspec)
							   decl)))
			     (loop (cdr body)
				   (cons (cons id type)
					 res))))))))
      (map (lambda (ident)
	      (let ((cell (assoc (ident-id ident) id-types)))
		 (if (not (pair? cell))
		     (error/ast (ident-id fun-ident)
				"missing argument types"
				fun-ident)
		     (cdr cell))))
	   ident-list)))

;*---------------------------------------------------------------------*/
;*    detect-struct-or-union ...                                       */
;*---------------------------------------------------------------------*/
(define (detect-struct-or-union type)
  (type-case type
	     ((typedef-t) 
	      (detect-struct-or-union (typedef-t-alias type)))
	     ((struct-t)
	      (warning "detect-struct-or-union:"
		       "type `" (type-c-name type)
		       "' used as function parameter or return type"))
	     (else
	      #t)))

;*---------------------------------------------------------------------*/
;*    string-end=? ...                                                 */
;*---------------------------------------------------------------------*/
(define (string-end=? string suff)
  (and (string? suff)
       (string? string)
       (let ((sufflen (string-length suff))
	     (strlen (string-length string)))
	 (and (string? string)
	      (>= strlen sufflen)
	      (string=? (substring string (- strlen sufflen) strlen) suff)))))

;*---------------------------------------------------------------------*/
;*    fix-abstract-array-type ...                                      */
;*---------------------------------------------------------------------*/
(define (fix-abstract-array-type type0)
  (or 
   (let loop ((type type0))
     (type-case type
		((typedef-t) (loop (typedef-t-alias type)))
		((array-t)
		 ;; If its an abstract array return a pointer to
		 ;; the element type.
		 (and (string-end=? (type-id type) "array")
		      (make-ptr-to (array-t-type type))))
		(else #f)))
   type0))

;*---------------------------------------------------------------------*/
;*    parameter-type-list->types ...                                   */
;*---------------------------------------------------------------------*/
(define (parameter-type-list->types list)
   (define (translate-one-para-decl p-decl previous)
      (if (not (para-decl? p-decl))
	  (if (eq? p-decl '...)
	      (if (not (type? previous))
		  (error "parameter-type-list->types"
			 "incorrect paremeter-type-list"
			 (list))
		  (make-... previous))
	      (begin
		 (if (ast? p-decl)
		     (error/ast 'parameter-type-list->types
				"Unknow expression"
				p-decl)
		     (error 'parameter-type-list->types
			    "Unknow expression"
			    p-decl))))
	  (let ((tspec (para-decl-type-spec-list p-decl))
		(decl  (para-decl-decl p-decl)))
	     (if tspec
		 (let ((type (tspec->type tspec)))
		    (type+decl->type type decl))
		 (let* ((tspec (t-name-type-spec-list
				(para-decl-type-name p-decl)))
			(adecl (t-name-adecl (para-decl-type-name p-decl)))
			(type  (tspec->type tspec)))
		    (type+adecl->type type adecl))))))
   (let loop ((list     list)
	      (previous #f)
	      (res     '()))
      (if (null? list)
	  (reverse! res)
	  (let ((type (translate-one-para-decl (car list) previous)))
	     (detect-struct-or-union type)
	     (set! type (fix-abstract-array-type type))
	     (loop (cdr list)
		   type
		   (cons type res))))))

;*---------------------------------------------------------------------*/
;*    translate-para-type ...                                          */
;*---------------------------------------------------------------------*/
(define (translate-para-type list)
   (let ((ptl (parameter-type-list->types list)))
      (let ((par-list (if (and (pair? ptl)
			       (null? (cdr ptl))
			       (or (eq? (type-id (car ptl)) 'void)
				   (string=? (type-id (car ptl)) "void")))
			  ;; this is the special case for no argument
			  ;; prototyping
			  '()
			  (map type-id ptl))))
	 (display par-list *oport*)
	 par-list)))
 
