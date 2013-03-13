;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/tspec.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 30 17:24:36 1995                          */
;*    Last change :  Mon Mar 11 14:30:27 2013 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The type specification handling                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_tspec
   (include "Translate/ast.sch"
	    "Translate/type.sch")
   (import  engine_param
	    translate_type
	    translate_decl
	    parser_parser
	    tools_error
	    tools_speek)
   (export  (tspec->type                <tspec>)
	    (simple->type               <type> <string>)
	    (type+tspec-coumpound->type <type> <tspec>)))
   
;*---------------------------------------------------------------------*/
;*    tspec->type ...                                                  */
;*---------------------------------------------------------------------*/
(define (tspec->type tspec)
   (cond
      ((default-tspec? tspec)
       (tspec->default-type))
      ((simple-tspec? tspec)
       (tspec-simple->type (car tspec)))
      (else
       (tspec-coumpound->type tspec))))

;*---------------------------------------------------------------------*/
;*    default-tspec? ...                                               */
;*---------------------------------------------------------------------*/
(define (default-tspec? tspec)
   (null? tspec))

;*---------------------------------------------------------------------*/
;*    simple-tspec? ...                                                */
;*---------------------------------------------------------------------*/
(define (simple-tspec? tspec)
   (and (pair? tspec) (null? (cdr tspec))))

;*---------------------------------------------------------------------*/
;*    tspec->default-type ...                                          */
;*---------------------------------------------------------------------*/
(define (tspec->default-type)
   (simple->type 'int "int"))
   
;*---------------------------------------------------------------------*/
;*    tspec-simple->type ...                                           */
;*---------------------------------------------------------------------*/
(define (tspec-simple->type tspec)
   (case (type-spec-class tspec)
      ((struct)
       (struct->type tspec))
      ((enum)
       (enum->type tspec))
      (else
       (simple->type (type-spec-class tspec) (type-spec-c-name tspec)))))
 
;*---------------------------------------------------------------------*/
;*    simple->type ...                                                 */
;*---------------------------------------------------------------------*/
(define (simple->type class c-name)
   (let ((cell (assq class *c-type-alist*)))
      (if (pair? cell)
	  (make-simple (cdr cell) c-name (list class))
	  (make-simple c-name c-name (list c-name)))))

;*---------------------------------------------------------------------*/
;*    struct->type ...                                                 */
;*---------------------------------------------------------------------*/
(define (struct->type tspec)
   (verbose 2 "struct->type: " tspec #\Newline)
   (let* ((sspec   (type-spec-value tspec))
	  ;; Look for an alias for this type.  It will only be present if
	  ;; the struct was anonymous and inside a typedef.
	  (alias-pair   (assq sspec *anonymous-struct-alist*))
	  (alias   (and alias-pair 
			(not (null? (cdr alias-pair)))
			(cdr alias-pair)))
	  (s-ident (struct-spec-id sspec))
	  (s-id    (ident-id s-ident))
	  (b-id    (string-append "s-" s-id))
	  (c-name  (cond 
		    ;; Use the typedef as the c-name if available.
		    (alias
		     (ident-id alias))
		    ;; If this is an anonymous struct (or union)
		    ;; without an alias, it has no useful c-name.  It
		    ;; was probably embedded in another struct.
		    (alias-pair
		     "void")
		    (else
		       (string-append
			(if (eq? (struct-spec-class sspec) 'struct)
			    "struct "
			    "union ")
			s-id))))
	  (struct  (make-cstruct b-id
				 c-name
				 s-id
				 (struct-spec-class sspec)
				 '()))
	  (fields  (let loop ((fields (struct-spec-fields sspec))
			      (res    '()))
		      (if (null? fields)
			  (reverse! res)
			  (let* ((field (car fields))
				 (tspec (car field))
				 (decls (cadr field)))
			     (let liip ((decls decls)
					(res   res))
				(cond
				   ((null? decls)
				    (loop (cdr fields) res))
				   ((and (not (decl-a-ptr (car decls)))
					 (not (decl-a-decl2 (car decls))))
				    ;; we skip bitfield with no declarator.
				    (loop (cdr fields) res))
				   (else
				    (let* ((decl    (car decls))
					   (type    (tspec->type tspec))
					   (type    (type+decl->type type
								     decl))
					   (f-ident (get-decl-ident decl))
					   (f-id    (ident-id f-ident)))
				       (liip (cdr decls)
					     (cons (cons type f-id)
						   res)))))))))))
      (if (pair? fields)
	  (struct-t-fields-set! struct fields))
      struct))

;*---------------------------------------------------------------------*/
;*    enum->type ...                                                   */
;*---------------------------------------------------------------------*/
(define (enum->type tspec)
   (let* ((espec   (type-spec-value tspec))
	  (e-ident (enum-spec-id espec))
	  (e-id    (if (ident? e-ident)
		       (ident-id e-ident)
		       (symbol->string
			(gensym (string-append *iname* "__e")))))
	  (b-id    (string-append "e-" e-id))
	  (c-name  (if (ident? e-ident)
		       (string-append "enum " e-id)
		       "long"))
	  (enum    (make-enum e-id c-name b-id '())))
      (if (pair? (enum-spec-enumerator-list espec))
	  (enum-t-fields-set! enum (enum-spec-enumerator-list espec)))
      enum))

;*---------------------------------------------------------------------*/
;*    tspec-coumpound->type ...                                        */
;*---------------------------------------------------------------------*/
(define (tspec-coumpound->type tspecs)
   (tspec-coumpound
    tspecs
    (lambda (unsigned signed long longlong short tspec)
       (let ((tspec (cond
		       (longlong (begin
				    (type-spec-class-set!  longlong 'longlong)
				    (type-spec-c-name-set! longlong "long long")
				    longlong))
		       (long     long)
		       (short    short)
		       (else     tspec))))
	  (if (not tspec)
	      (error/ast 'type1 "Illegal type" (car tspecs))
	      (let ((type (type-spec-class tspec)))
		 (cond
		    (unsigned
		     (let ((cell (assq type *c-unsigned-type-alist*)))
			(if (not (pair? cell))
			    (error/ast 'unsigned "Illegal type" unsigned)
			    (let ((name (string-append
					 "unsigned "
					 (type-spec-c-name tspec))))
			       (make-simple (cdr cell)
					    name
					    (list 'unsigned type))))))
		    (signed
		     (let ((cell (assq type *c-signed-type-alist*)))
			(if (not (pair? cell))
			    (error/ast 'signed "Illegal type" signed)
			    (let ((name (string-append
					 "signed "
					 (type-spec-c-name tspec))))
			       (make-simple (cdr cell)
					    name
					    (list 'signed type))))))
		    (else
		     (tspec-simple->type tspec)))))))))

;*---------------------------------------------------------------------*/
;*    type+tspec-coumpound->type ...                                   */
;*---------------------------------------------------------------------*/
(define (type+tspec-coumpound->type type tspecs)
   (tspec-coumpound
    tspecs
    (lambda (unsigned signed long longlong short tspec)
       (let ((tspec (cond
		       (longlong (simple->type 'longlong "longlong"))
		       (long     long)
		       (short    short)
		       (else     tspec))))
	  (if (not tspec)
	      type
	      (error/ast 'type2 "Illegal type" (car tspecs)))))))

;*---------------------------------------------------------------------*/
;*    tspec-coumpound ...                                              */
;*---------------------------------------------------------------------*/
(define (tspec-coumpound tspecs fun)
   (let loop ((unsigned #f)
	      (signed   #f)
	      (long     #f)
	      (longlong #f)
	      (short    #f)
	      (tspec    #f)
	      (tspcs   tspecs))
      (cond
	 ((null? tspcs)
	  (fun unsigned signed long longlong short tspec))
	 (else
	  (let ((ts (car tspcs)))
	     (if (type-qualifier-spec? ts)
		 (loop unsigned
		       signed
		       long
		       longlong
		       short
		       tspec
		       (cdr tspcs))
		 (case (type-spec-class ts)
		    ((unsigned)
		     (if unsigned
			 (error/ast 'unsigned "Duplicate type specification" ts)
			 (loop ts
			   signed
			   long
			   longlong
			   short
			   tspec
			   (cdr tspcs))))
		((signed)
		 (if signed
		     (error/ast 'signed "Duplicate type specification" ts)
		     (loop unsigned
			   ts
			   long
			   longlong
			   short
			   tspec
			   (cdr tspcs))))	
		(else
		 (cond
		    (tspec
		     (error/ast (type-spec-class ts) "Illegal type" ts))
		    ((eq? (type-spec-class ts) 'short)
		     (if (and (not long)
			      (not longlong)
			      (not short)
			      (not tspec))
			 (loop unsigned
			       signed
			       #f
			       #f
			       ts
			       #f
			       (cdr tspcs))
			 (error/ast 'type4 "Illegal type" ts)))
		    ((eq? (type-spec-class ts) 'long)
		     (cond
			((and (not longlong)
			      (not short)
			      (not tspec))
			 (if long
			     (loop unsigned
				   signed
				   #f
				   ts
				   #f
				   #f
				   (cdr tspcs))
			     (loop unsigned
				   signed
				   ts
				   #f
				   #f
				   #f
				   (cdr tspcs))))
			(else
			 (error/ast 'type5 "Illegal type" ts))))
		    ((eq? (type-spec-class ts) 'longlong)
		     (cond
			((and (not longlong)
			      (not short)
			      (not tspec))
			 (if long
			     (loop unsigned
				signed
				#f
				;; this is the only place the longlong flag is 
				;; changed ot something other than #f
				ts
				#f
				#f
				(cdr tspcs))
			     ;; we really shouldn't get here
			     (loop unsigned
				signed
				ts
				#f
				#f
				#f
				(cdr tspcs))))
			(else
			 (error/ast 'type6 "Illegal type" ts))))
		    ((eq? (type-spec-class ts) 'int)
		     (cond
			(short
			 (loop unsigned
			       signed
			       short
			       #f
			       #f
			       short
			       (cdr tspcs)))
			(long
			 (loop unsigned
			       signed
			       long
			       longlong
			       #f
			       long
			       (cdr tspcs)))
			(else
			 (loop unsigned
			       signed
			       #f
			       #f
			       #f
			       ts
			       (cdr tspcs)))))
		    (else
		     (loop unsigned
			   signed
			   long
			   longlong
			   short
			   ts
			   (cdr tspcs))))))))))))
