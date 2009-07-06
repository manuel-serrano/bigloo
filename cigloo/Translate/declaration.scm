;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Translate/declaration.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 29 16:27:51 1995                          */
;*    Last change :  Mon Jul 31 10:15:58 2006 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The translation of the declarations                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module translate_declaration
   (include "Translate/ast.sch"
	    "Translate/type.sch")
   (import  translate_decl
	    translate_function
	    translate_tspec
	    translate_type
	    engine_param
	    tools_speek)
   (export  (translate-declaration <ast>)
	    (typedef-sspec?        <sspec>)))

;*---------------------------------------------------------------------*/
;*    translate-declaration ...                                        */
;*---------------------------------------------------------------------*/
(define (translate-declaration ast)
   (verbose 3 "translate-declaration: " ast #\Newline)
   (let* ((spec  (declare-spec ast))
	  (dil   (declare-init-decl-list ast))
	  (sspec (storage-class-spec-of-decl-spec spec)))
      (cond
	 ((and (pair? sspec)
	       (case (storage-class-spec-value (car sspec))
		  ((static)
		   #t)
		  (else
		   #f)))
	  #unspecified)
	 (else
	  (if (null? dil)
	      ;; it is just a type declaration
	      (tspec->type (type-spec-of-decl-spec spec))
	      ;; it is a variable, typedef or function declaration
	      ;; or even a function definition.
	      (for-each (lambda (init-decl)
			   (match-case init-decl
			      ((?decl . ?init)
			       (translate-decl decl spec))
			      (?decl
			       (translate-decl decl spec))))
			dil))))))

;*---------------------------------------------------------------------*/
;*    function-declaration? ...                                        */
;*    -------------------------------------------------------------    */
;*    Is a declaration a function declaration ? This function is not   */
;*    a real predicate because rather than #t it returns the function  */
;*    identifier and the parameter type list.                          */
;*---------------------------------------------------------------------*/
(define (function-declaration? decl)
   (let loop ((decl decl)
	      (ptl  #f))
      (match-case decl
	 (#{decl ?- ?- ?- ?decl2}
	  (if (not decl2)
	      #f
	      (match-case decl2
		 (#{decl2 ?- ?- #f #f ?decl2 #f ?ptl #f}
		  (cond
		     ((not decl2)
		      #f)
		     ((not ptl)
		      #f)
		     (else
		      (match-case decl2
			 (#{decl2 ?- ?- ?id #f #f #f #f #f}
			  ptl)
			 (#{decl2 ?- ?- #f ?decl #f #f #f #f}
			  (if (not decl)
			      #f
			      (loop decl ptl)))
			 (else
			  #f)))))
		 (else
		  #f))))
	 (else
	  #f))))

;*---------------------------------------------------------------------*/
;*    typedef-sspec? ...                                               */
;*---------------------------------------------------------------------*/
(define (typedef-sspec? sspec)
   (cond
      ((null? sspec)
       #f)
      ((eq? (storage-class-spec-value (car sspec)) 'typedef)
       #t)
      (else
       (typedef-sspec? (cdr sspec)))))
       
;*---------------------------------------------------------------------*/
;*    translate-decl ...                                               */
;*---------------------------------------------------------------------*/
(define (translate-decl decl spec)
   (verbose 2
	    "translate-function-declaration: " #\Newline
	    "   decl: " decl #\Newline
	    "   spec: " spec #\Newline)
   (cond
     ((typedef-sspec? (storage-class-spec-of-decl-spec spec))
       (translate-typedef-declaration decl spec))
     ((function-declaration? decl) =>
       (lambda (fun-decl)
         (translate-function-declaration decl spec fun-decl)))
     (else (translate-variable-declaration decl spec))))

;*---------------------------------------------------------------------*/
;*    translate-variable-declaration ...                               */
;*---------------------------------------------------------------------*/
(define (translate-variable-declaration decl spec)
   (verbose 2
	    "translate-variable-declaration: " #\Newline
	    "   decl: " decl #\Newline
	    "   spec: " spec #\Newline)
   (let* ((tspec   (type-spec-of-decl-spec spec))
	  (type    (type+decl->type (tspec->type tspec) decl))
	  (v-ident (get-decl-ident decl))
	  (v-id    (ident-id v-ident))
	  (sv-id   (string->symbol (string-upcase v-id))))
      (if (not (getprop sv-id 'fun-processed))
	  (begin
	     (putprop! sv-id 'fun-processed #t)
	     (verbose 1 "   " v-id #\Newline)
	     (fprint *oport* "   (" (if *macro-variable* "macro " "")
		     v-id "::" (type-id type) " \"" v-id "\")")))))

;*---------------------------------------------------------------------*/
;*    translate-typedef-declaration ...                                */
;*---------------------------------------------------------------------*/
(define (translate-typedef-declaration decl spec)
   (verbose 2
	    "translate-typedef-declaration: " #\Newline
	    "   decl: " decl #\Newline
	    "   spec: " spec #\Newline)
   (let* ((tspec   (type-spec-of-decl-spec spec))
	  (alias   (type+decl->type (tspec->type tspec) decl))
	  (t-ident (get-decl-ident decl))
	  (t-id    (ident-id t-ident)))
      (verbose 1 "   " t-id #\Newline)
      (putprop! (string->symbol (string-upcase t-id)) 'typedef #t)
      (make-typedef t-id alias)))
      
