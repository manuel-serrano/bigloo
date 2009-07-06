;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Parser/cpp.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Nov 29 09:46:19 1995                          */
;*    Last change :  Sun Aug 31 16:05:42 2008 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The cpp parsing                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module parser_cpp
   (include "Parser/coord.sch")
   (import  engine_param
	    engine_engine
	    parser_tools)
   (export  cpp-parser
	    cpp-lexer
	    (set-cpp-coord! coord)))

;*---------------------------------------------------------------------*/
;*    set-cpp-coord ...                                                */
;*---------------------------------------------------------------------*/
(define (set-cpp-coord! coord)
   (set! *cpp-coord* coord))

;*---------------------------------------------------------------------*/
;*    *cpp-coord* ...                                                  */
;*---------------------------------------------------------------------*/
(define *cpp-coord* #f)

;*---------------------------------------------------------------------*/
;*    cpp-lexer ...                                                    */
;*---------------------------------------------------------------------*/
(define cpp-lexer
   (regular-grammar ((nonzero-digit   (in ("19")))
		     (odigit          (in ("07")))
		     (long-suffix     (or #\l #\L))
		     (unsigned-suffix (or #\u #\U))
		     ctx)

      ;; blank
      ((+ (in #\space #\Newline #\tab #a012 #\\))
       (ignore))

      ;; comment
      ((: "/*" (* (or (out #\*) (: (+ #\*) (out #\/ #\*)))) (+ #\*) "/")
       (ignore))

      ;; `define toto(...)'
      ((bol (: #\# (* #\space) "define"
		   (* #\space)
		   (: (or #\_ alpha) (* (or #\_ alpha #\_ digit)))
		 #\())
       (cell-set! ctx 'define-fun)
       (cons 'DEFINE-FUN (the-string)))

      ;; ending of `define toto( ...'
      (#\)
       (if (eq? (cell-ref ctx) 'define-fun)
	   (begin
	      (cell-set! ctx 'noctx)
	      'EOA)
	   (list 'STRING ")")))

      ;; define
      ((bol (: #\# (* #\space) "define"))
       'DEFINE-VAR)

      ;; include
      ((bol (: #\# (* #\space) "include"))
       'INCLUDE)

      ;; identifier
      ((: (or #\_ alpha) (* (or alpha #\_ digit)))
       (list 'ID (the-string)))

      ;; include strings
      ((: #\< (+ (out #\>)) #\>)
       (list '<STRING> (the-substring 1 (-fx (the-length) 1))))

      ;; strings
      ((: #\" (* (out #\")) #\")
       (list 'STRING (the-substring 1 (-fx (the-length) 1))))

      ;; integer constant
      ((: (or (: nonzero-digit (* digit))
	      (: #\0 (* odigit))
	      (: (uncase "0x") (+ xdigit)))
	  (? (or long-suffix
		 (: long-suffix unsigned-suffix)
		 unsigned-suffix
		 (: unsigned-suffix long-suffix))))
       (list 'INTEGER-CONSTANT (the-string)))

      ;; floating-point constant
      ((or (: (+ digit)
	      (: (in #\e #\E) (? (in #\- #\+)) (+ digit))
	      (? (in #\f #\F #\l #\L)))
	   (: (or (: (+ digit) #\. (* digit)) (: #\. (+ digit)))
	      (? (: (in #\e #\E) (? (in #\- #\+)) (+ digit)))
	      (? (in #\f #\F #\l #\L))))
       (list 'FLOAT-CONSTANT (the-string)))

      ;; character constant
      ((: (? #\L) (: #\' (+ all) #\'))
       (list 'CHAR-CONSTANT (the-string)))

      ;; string constant
      ((: #\L #\" (* (out #\")) #\")
       (list 'STRING-CONSTANT (the-string)))

      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      c
	      '???)))))

;*---------------------------------------------------------------------*/
;*    cpp-parser ...                                                   */
;*---------------------------------------------------------------------*/
(define cpp-parser
   (lalr-grammar
      
      (token <STRING> STRING INTEGER-CONSTANT FLOAT-CONSTANT
	     CHAR-CONSTANT STRING-CONSTANT ??? INCLUDE DEFINE-VAR DEFINE-FUN
	     ID EOA)
      
      (cmd
       (()
	'done)
       ((include-cmd cmd)
	'done)
       ((define-cmd cmd)
	'done)
       ((<STRING> cmd)
	'done)
       ((STRING cmd)
	'done)
       ((INTEGER-CONSTANT cmd)
	'done)
       ((FLOAT-CONSTANT cmd)
	'done)
       ((CHAR-CONSTANT cmd)
	'done)
       ((STRING-CONSTANT cmd)
	'done)
       ((ID cmd)
	'done cmd)
       ((??? cmd)
	'done)) 

      (include-cmd
       ((INCLUDE <STRING>)
	(let ((fname (car <STRING>)))
	   (cond
	      ((or (eq? *open-include* 'all) (member fname *open-include*))
	       (translate-file fname '<include> 'open))
	      ((or (eq? *scan-include* 'all) (member fname *scan-include*))
	       (translate-file fname '<include> 'scan)))))
       ((INCLUDE STRING)
	(let ((fname (car STRING)))
	   (cond
	      ((or (eq? *open-include* 'all) (member fname *open-include*))
	       (translate-file fname 'include 'open))
	      ((or (eq? *scan-include* 'all) (member fname *scan-include*))
	       (translate-file fname 'include 'scan))))))

      (define-cmd
       ((DEFINE-VAR ID)
	'done)
       ((DEFINE-VAR ID value)
	(if *define*
	    (let ((cell  (assq value *c-type-alist*))
		  (coord DEFINE-VAR)
		  (m-id  (car ID)))
	       (cond
		  ((find-macro? m-id)
		   (if (coord? *cpp-coord*)
		       (warning/location (coord-fname *cpp-coord*)
					 (coord-pos *cpp-coord*)
					 "define:"
					 "Macro redefinition (ignored) -- "
					 m-id)
		       (warning "define:"
				"Macro redefinition (ignored) -- "
				m-id))
		   'done)
		  ((not (pair? cell))
		   (if (coord? *cpp-coord*)
		       (warning/location (coord-fname *cpp-coord*)
					 (coord-pos *cpp-coord*)
					 "define:"
					 "Unknown type expression -- "
					 "Using `" *default-type* "' type")
		       (warning "define:"
				"Unknown type expression -- "
				"Using `" *default-type* "' type"))
		   (bind-macro! m-id)
                   (if (or (not *omit-underscore*) (not (eq? (string-ref m-id 0) #\_)))
		     (fprint *oport*
		  	   "   (macro " m-id "::" *default-type*
		  	   " \"" m-id "\")")))
		  (else
		   (bind-macro! m-id)
                   (if (or (not *omit-underscore*) (not (eq? (string-ref m-id 0) #\_)))
		     (fprint *oport*
		  	   "   (macro " m-id "::" (cdr cell) " \"" m-id
		  	   "\")"))))))
	'done)
       ((DEFINE-FUN args)
	(if *define-fun*
	    (begin
	       (if (coord? *cpp-coord*)
		   (warning/location (coord-fname *cpp-coord*)
				     (coord-pos *cpp-coord*)
				     "define:"
				     "Unknown type expression -- "
				     "Using `"
				     *default-type*
				     "' type")
		   (warning "define:"
			    "Unknown type expression -- "
			    "Using `" *default-type* "' type"))
	       (let ((m-id (let ((str DEFINE-FUN))
			      (let loop ((r     1)
					 (start #t))
				 (cond
				    ((char=? (string-ref str r) #\space)
				     (loop (+fx r 1) start))
				    (start
				     (loop (+fx r 6) #f))
				    (else
				     (substring
				      str
				      r
				      (-fx (string-length str) 1))))))))
		  (if (find-macro? m-id)
		      (begin
			 (if (coord? *cpp-coord*)
			     (warning/location
			      (coord-fname *cpp-coord*)
			      (coord-pos *cpp-coord*)
			      "define:"
			      "Macro redefinition (ignored) -- "
			      m-id)
			     (warning "define:"
				      "Macro redefinition (ignored) -- "
				      m-id))
			 'done)
		      (begin
			 (bind-macro! m-id)
                         (if (or (not *omit-underscore*) (not (eq? (string-ref m-id 0) #\_)))
			   (fprint *oport*
			  	 "   (macro " m-id "::" *default-type*
			  	 " " (map (lambda (x) *default-type*) args)
			  	 " \"" m-id "\")")))))))))
      (args
       ((EOA)
	'())
       ((ID EOA)
	`(,ID))
       ((ID ??? args)
	`(,ID ,@args)))

      (value
       ((STRING)
	'char*)
       ((STRING-CONSTANT)
	'char*)
       ((CHAR-CONSTANT)
	'char)
       ((INTEGER-CONSTANT)
	'long)
       ((FLOAT-CONSTANT)
	'double)
       ((???)
	'???))))

 
;*---------------------------------------------------------------------*/
;*    find-macro? ...                                                  */
;*---------------------------------------------------------------------*/
(define (find-macro? string)
   (getprop (string->symbol (string-upcase string)) 'fun-processed))

;*---------------------------------------------------------------------*/
;*    bind-macro! ...                                                  */
;*---------------------------------------------------------------------*/
(define (bind-macro! string)
   (putprop! (string->symbol (string-upcase string)) 'fun-processed #t))
   
