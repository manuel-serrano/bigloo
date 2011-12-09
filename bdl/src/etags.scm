;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdl/src/etags.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  8 10:23:12 1999                          */
;*    Last change :  Fri Dec  9 11:13:58 2011 (serrano)                */
;*    Copyright   :  2000-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The grammar parsing the etags file entries.                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdl_etags
   (import __bdl_types
	   __bdl_misc
	   __bdl_env)
   (export (read-etags! ::bdl-program ::pair-nil . ::obj)))

;*---------------------------------------------------------------------*/
;*    *etags-default-keywords* ...                                     */
;*---------------------------------------------------------------------*/
(define *etags-default-keywords*
   '((define . DEFINE)
     (define-inline . INLINE)
     (define-method . METHOD)
     (define-generic . GENERIC)
     (define-macro . DEFMACRO)
     (module . MODULE)
     (class . CLASS) (final-class . CLASS) (wide-class . CLASS)
     (extern . EXTERN) (macro . MACRO)
     (define-struct . STRUCT)
     (static . STATIC/EXPORT) (export . STATIC/EXPORT)
     (include . INCLUDE)
     (type . TYPE)))

;*---------------------------------------------------------------------*/
;*    *etags-meta-keywords* ...                                        */
;*---------------------------------------------------------------------*/
(define *etags-meta-keywords*
   '(meta-define))

;*---------------------------------------------------------------------*/
;*    Symbols keys ...                                                 */
;*---------------------------------------------------------------------*/
(define *keyword-key* #f)
(define *meta-key* #f)

;*---------------------------------------------------------------------*/
;*    init-read-etags! ...                                             */
;*---------------------------------------------------------------------*/
(define (init-read-etags! keywords-list)
   (if (not *meta-key*)
       (begin
	  (set! *meta-key* (gensym 'bdl))
	  (set! *keyword-key* (gensym 'bdl))
	  ;; meta keywords initialization
	  (for-each (lambda (w)
		       (putprop! w *meta-key* 'META-DEFINE))
		    *etags-meta-keywords*)))
   ;; keywords initialization
   (for-each (lambda (w)
		(putprop! (car w) *keyword-key* (cdr w)))
	     keywords-list))

;*---------------------------------------------------------------------*/
;*    read-etags! ...                                                  */
;*    -------------------------------------------------------------    */
;*    An etags file format is:                                         */
;*    ^L                                                               */
;*    <file-name>,<etags-len>                                          */
;*    <definitions>+                                                   */
;*    <definition> : <keyword> <ident>^?<line-start>,<line-stop>       */
;*    <keyword>    : (define                                           */
;*                 | (define-generic                                   */
;*                 | (define-method                                    */
;*                 | (class                                            */
;*---------------------------------------------------------------------*/
(define (read-etags! prgm mods . keywords)
   ;; init the etags keywords first
   (init-read-etags! (if (null? keywords) *etags-default-keywords* keywords))
   ;; now, proceed to the reading
   (with-access::bdl-program prgm (etags)
      (let ((port (open-input-file etags)))
	 (if (not (input-port? port))
	     (bdl-error "read-etags" "Can't find `etags' file for input" etags)
	     (unwind-protect
		(begin
		   ;; we skip the ^L line
		   (read-line port)
		   ;; then we read all file entries
		   (let loop ((entry (read-etags-file-entry prgm port mods))
			      (modules '()))
		      (cond
			 ((eof-object? entry)
			  (sort modules
			     (lambda (l::bdl-module r::bdl-module)
				(string>?
				   (-> l ident )
				   (-> r ident)))))
			 ((not (isa?  entry bdl-module))
			  (loop (read-etags-file-entry prgm port mods)
			     modules))
			 (else
			  (loop (read-etags-file-entry prgm port mods)
			     (cons entry modules))))))
		(close-input-port port))))))

;*---------------------------------------------------------------------*/
;*    read-etags-file-entry ...                                        */
;*    -------------------------------------------------------------    */
;*    Because etags file format are just brain damaged, we rely on     */
;*    the assumption that reading at the end-of-file returns the       */
;*    eof-object. That is we read twice the end of file. In            */
;*    consequence it is impossible to use that function to read        */
;*    in stdin.                                                        */
;*---------------------------------------------------------------------*/
(define (read-etags-file-entry prgm port modules)
   (define (fetch-file-name str)
      ;; fetch the file name of an etags file-name entry
      (let ((port  (open-input-string str))
	    (fetch (regular-grammar ()
		      ((+ (out #\,))
		       (the-string))
		      (else
		       (bdl-error
			"read-etags-file-entry"
			"Illegal etags file format (can't find file name)"
			str)))))
	 (let ((name (read/rp fetch port)))
	    (close-input-port port)
	    name)))
   (define (find-module-name-from-file-name name modules)
      ;; from a list of modules, we search one implemented by name
      (let loop ((modules modules))
	 (cond
	    ((null? modules)
	     ;; we have not been able to find the module associated
	     ;; to that file. It could means that we are examining a C
	     ;; module instead of the Scheme module. In consequence, we
	     ;; will simply skip that file. This is denoted here by
	     ;; returning #f instead of a module name.
	     #f)
	    ((member name (cdr (car modules)))
	     (symbol->string (caar modules)))
	    (else
	     (loop (cdr modules))))))
   ;; we read the file name
   (let ((line (read-line port)))
      (cond
	 ((eof-object? line)
	  line)
	 ((string=? line "-,0")
	  ;; this is a meta file
	  (let loop ((line (read-line port)))
	     (if (not (or (eof-object? line) (string=? line "")))
		 (match-case (parse-etags-meta-entry-line line)
		    ;; a meta definition (i.e. a definition
		    ;; introduced by btags that tell us to add
		    ;; some new keyword recognition in the current
		    ;; etags parsing).
		    ((meta-define ?kind ?ident)
		     (let ((sym (string->symbol ident)))
			(if (not (getprop sym *keyword-key*))
			    (putprop! sym
				      *keyword-key*
				      (string->symbol (string-upcase kind)))))
		     (loop (read-line port)))
		    (else
		     (bdl-error "read-etags-file-entry"
				"Illegal entry format (can't find etags marker)"
				line)))))
	  #unspecified)
	 (else
	  (let* ((fname (fetch-file-name line))
		 (mod   (find-module-name-from-file-name fname modules)))
	     (if (not (string? mod))
		 #f
		 (read-etags-module prgm port fname mod)))))))

;*---------------------------------------------------------------------*/
;*    read-etags-module ...                                            */
;*---------------------------------------------------------------------*/
(define (read-etags-module prgm port fname mod::bstring)
   (let ((minfo::bdl-module (new-module prgm
			       mod
			       (list fname)
			       (new-location fname 1))))
      (let loop ((line      (read-line port))
		 (fundef    '())
		 (vardef    '())
		 (metdef    '())
		 (classdef  '())
		 (structdef '())
		 (externdef '())
		 (macrodef  '()))
	 (if (or (eof-object? line) (string=? line ""))
	     (begin
		(set! (-> minfo functions) (reverse! fundef))
		(set! (-> minfo variables) (reverse! vardef))
		(set! (-> minfo classes) (reverse! classdef))
		(set! (-> minfo methods) (reverse! metdef))
		(set! (-> minfo structures) (reverse! structdef))
		(set! (-> minfo externs)  (reverse! externdef))
		(set! (-> minfo macros) (reverse! macrodef))
		minfo)
	     (match-case (parse-etags-entry-line line)
		((define (?name ?line))
		 (multiple-value-bind (name rtype)
		    (parse-string-id name "obj")
		    ;; a function definition
		    (loop (read-line port)
		       (cons (new-function prgm
				name
				minfo
				(new-location fname line))
			  fundef)
		       vardef
		       metdef
		       classdef
		       structdef
		       externdef
		       macrodef)))
		((define ?name ?line)
		 (multiple-value-bind (name type)
		    (parse-string-id name "obj")
		    ;; a variable definition
		    (loop (read-line port)
		       fundef
		       (cons (new-variable prgm
				name
				minfo
				(new-location fname line))
			  vardef)
		       metdef
		       classdef
		       structdef
		       externdef
		       macrodef)))
		((define-generic (?name ?line))
		 ;; a generic function definition
		 (loop (read-line port)
		    (cons (new-generic prgm
			     name
			     minfo
			     (new-location fname line))
		       fundef)
		    vardef
		    metdef
		    classdef
		    structdef
		    externdef
		    macrodef))
		((define-method (?id ?arg ?line))
		 ;; a method function definition
		 (multiple-value-bind (name rtype)
		    (parse-string-id id "obj")
		    (multiple-value-bind (- type)
		       (parse-string-id arg "obj")
		       (let ((met (new-method prgm
				     name
				     minfo
				     (new-location fname line)
				     type
				     rtype)))
			  (loop (read-line port)
			     fundef
			     vardef
			     (cons met metdef)
			     classdef
			     structdef
			     externdef
			     macrodef)))))
		((class ?id ?line)
		 (multiple-value-bind (name type)
		    (parse-string-id id "object")
		    ;; a class definition
		    (loop (read-line port)
		       fundef
		       vardef
		       metdef
		       (cons (new-class prgm
				name
				minfo
				(new-location fname line)
				(find-bdl-class prgm type)
				'plain)
			  classdef)
		       structdef
		       externdef
		       macrodef)))
		((wide-class ?id ?line)
		 (multiple-value-bind (name type)
		    (parse-string-id id "object")
		    ;; a class definition
		    (loop (read-line port)
		       fundef
		       vardef
		       metdef
		       (cons (new-class prgm
				name
				minfo
				(new-location fname line)
				(find-bdl-class prgm type)
				'wide)
			  classdef)
		       structdef
		       externdef
		       macrodef)))
		((final-class ?id ?line)
		 (multiple-value-bind (name type)
		    (parse-string-id id "object")
		    ;; a class definition
		    (loop (read-line port)
		       fundef
		       vardef
		       metdef
		       (cons (new-class prgm
				name
				minfo
				(new-location fname line)
				(find-bdl-class prgm type)
				'final)
			  classdef)
		       structdef
		       externdef
		       macrodef)))
		((define-struct ?name ?line)
		 ;; a struct definition
		 (loop (read-line port)
		    fundef
		    vardef
		    metdef
		    classdef
		    (cons (new-structure prgm
			     name
			     minfo
			     (new-location fname line))
		       structdef)
		    externdef
		    macrodef))
		((extern ?name ?line)
		 ;; an extern definition
		 (loop (read-line port)
		    fundef
		    vardef
		    metdef
		    classdef
		    structdef
		    (cons (new-extern prgm
			     name
			     minfo
			     (new-location fname line))
		       externdef)
		    macrodef))
		((define-macro (?name ?line))
		 ;; a macro definition
		 (loop (read-line port)
		    fundef
		    vardef
		    metdef
		    classdef
		    structdef
		    externdef
		    (cons (new-macro prgm
			     name
			     minfo
			     (new-location fname line))
		       macrodef)))
		((module ?name ?line)
		 (loop (read-line port)
		    fundef
		    vardef
		    metdef
		    classdef
		    structdef
		    externdef
		    macrodef))
		((ignore)
		 ;; an entry to be ignored
		 (loop (read-line port)
		    fundef
		    vardef
		    metdef
		    classdef
		    structdef
		    externdef
		    macrodef))
		(else
		 (bdl-error "read-etags-file-entry"
		    "Illegal entry format (illegal line)"
		    line)
		 (loop (read-line port)
		    fundef
		    vardef
		    metdef
		    classdef
		    structdef
		    externdef
		    macrodef)))))))

;*---------------------------------------------------------------------*/
;*    parse-etags-entry-line ...                                       */
;*---------------------------------------------------------------------*/
(define (parse-etags-entry-line line::bstring)
   (let ((port (open-input-string line))
	 (reg  (regular-grammar ((letter   (in ("azAZ") (#a128 #a255)))
				 (special  (in "!@~$%^&*></-_+\\|=?.:"))
				 (kspecial (in "!@~$%^&*></-_+\\|=?."))
				 (id       (: (* digit)
					      (or letter special)
					      (* (or letter
						     special
						     digit
						     (in ",'`"))))))
		  (blank
		   (ignore))
		  ("("
		   (list 'PAR-OPEN))
		  (")"
		   (list 'PAR-CLO))
		  ((+ digit)
		   (cons 'NUMBER (the-fixnum)))
		  (id
		   (let* ((string (the-string))
			  (symbol (the-symbol))
			  (kwd    (getprop symbol *keyword-key*)))
		      (if kwd
			  ;; this is a keyword
			  (cons kwd symbol)
			  ;; this is a regular identifier
			  (cons 'IDENT string))))
		  ((: #\" (* (out #\")) #\")
		   (list 'STRING))
		  (#a127
		   (list 'EOI))
		  (#\,
		   (ignore))
		  (else
		   (let ((c (the-failure)))
		      (if (eof-object? c)
			  c
			  (bdl-error "parse-etage-entry-line"
				     "Illegal char"
				     c))))))
	 (lalr (lalr-grammar
		  (IDENT PAR-OPEN PAR-CLO EOI NUMBER
			 DEFINE INLINE DEFMACRO MODULE
			 GENERIC METHOD
			 TYPESEP
			 CLASS STRUCT EXTERN MACRO STRING INCLUDE TYPE
			 STATIC/EXPORT)
		  
		  ;; the line we are to parse
		  (line
		   ((function)
		    function)
		   ((variable)
		    variable)
		   ((genericdef)
		    genericdef)
		   ((methoddef)
		    methoddef)
		   ((classdef)
		    classdef)
		   ((structdef)
		    structdef)
		   ((externdef)
		    externdef)
		   ((macrodef)
		    macrodef)
		   ((moduledef)
		    moduledef))
		  
		  ;; function definitions
		  (function
		   ((PAR-OPEN DEFINE PAR-OPEN IDENT EOI NUMBER@line NUMBER)
		    `(define (,IDENT ,line)))
		   ((PAR-OPEN DEFINE PAR-OPEN TYPE EOI NUMBER@line NUMBER)
		    `(define ("TYPE" ,line)))
		   ((PAR-OPEN INLINE PAR-OPEN IDENT EOI NUMBER@line NUMBER)
		    `(define (,IDENT ,line)))
		   ((PAR-OPEN INLINE PAR-OPEN TYPE EOI NUMBER@line NUMBER)
		    `(define ("TYPE" ,line))))
		  
		  ;; variable definitions
		  (variable
		   ((PAR-OPEN DEFINE IDENT EOI NUMBER@line NUMBER)
		    `(define ,IDENT ,line))
		   ((PAR-OPEN DEFINE TYPE EOI NUMBER@line NUMBER)
		    `(define "TYPE" ,line)))
		  
		  ;; generic function definitions
		  (genericdef
		   ((PAR-OPEN GENERIC PAR-OPEN IDENT EOI NUMBER@line NUMBER)
		    `(define-generic (,IDENT ,line)))
		   ((PAR-OPEN GENERIC PAR-OPEN TYPE EOI NUMBER@line NUMBER)
		    `(define-generic ("TYPE" ,line))))
		  
		  ;; method definitions
		  (methoddef
		   ((PAR-OPEN METHOD PAR-OPEN IDENT@mnane IDENT@aname dummys
			      EOI NUMBER@line NUMBER)
		    `(define-method (,mnane ,aname ,line)))
		   ((PAR-OPEN METHOD PAR-OPEN TYPE IDENT@aname dummys
			      EOI NUMBER@line NUMBER)
		    `(define-method ("TYPE" ,aname ,line)))
		   ((PAR-OPEN METHOD PAR-OPEN TYPE@type1 TYPE@type2 dummys
			      EOI NUMBER@line NUMBER)
		    `(define-method ("TYPE" "TYPE" ,line))))
		  
		  (dummys
		   (()
		    'dummy)
		   ((PAR-CLO dummys)
		    'dummy)
		   ((PAR-OPEN dummys)
		    'dummy)
		   ((IDENT dummys)
		    'dummy))
		  
		  ;; class definitions
		  (classdef
		   ((PAR-OPEN CLASS IDENT dummys
			      EOI NUMBER@line NUMBER)
		    `(,CLASS ,IDENT ,line))
		   ((PAR-OPEN CLASS EXTERN dummys
			      EOI NUMBER@line NUMBER)
		    `(,CLASS "extern" ,line))
		   ((PAR-OPEN CLASS TYPE dummys
			      EOI NUMBER@line NUMBER)
		    `(,CLASS "type" ,line))
		   ((PAR-OPEN STATIC/EXPORT
			      PAR-OPEN CLASS TYPE dummys
			      EOI NUMBER@line NUMBER)
		    `(,CLASS "type" ,line))
		   ((PAR-OPEN STATIC/EXPORT
			      PAR-OPEN CLASS IDENT dummys
			      EOI NUMBER@line NUMBER)
		    `(,CLASS ,IDENT ,line)))
		  
		  ;; struct definitions
		  (structdef
		   ((PAR-OPEN STRUCT IDENT EOI NUMBER@line NUMBER)
		    `(define-struct ,IDENT ,line))
		   ((PAR-OPEN STRUCT TYPE EOI NUMBER@line NUMBER)
		    `(define-struct "type" ,line))
		   ((PAR-OPEN STRUCT EXTERN EOI NUMBER@line NUMBER)
		    `(define-struct "extern" ,line)))
		  
		  ;; extern definitions
		  (externdef
		   ((PAR-OPEN EXTERN extern-sans-clause)
		    extern-sans-clause)
		   ((extern-sans-clause)
		    extern-sans-clause))
		  
		  (extern-sans-clause
		   ((PAR-OPEN MACRO IDENT EOI NUMBER@line NUMBER)
		    `(extern ,IDENT ,line))
		   ((PAR-OPEN MACRO TYPE EOI NUMBER@line NUMBER)
		    `(extern "TYPE" ,line))
		   ((PAR-OPEN INCLUDE EOI NUMBER@line NUMBER)
		    `(ignore))
		   ((PAR-OPEN IDENT STRING EOI NUMBER@line NUMBER)
		    `(extern ,IDENT ,line))
		   ((PAR-OPEN TYPE STRING EOI NUMBER@line NUMBER)
		    `(extern "TYPE" ,line))
		   ((PAR-OPEN IDENT EOI NUMBER@line NUMBER)
		    `(extern ,IDENT ,line))
		   ((PAR-OPEN STATIC/EXPORT IDENT STRING
			      EOI NUMBER@line NUMBER)
		    '(ignore))
		   ((PAR-OPEN STATIC/EXPORT TYPE STRING
			      EOI NUMBER@line NUMBER)
		    '(ignore))
		   ((PAR-OPEN TYPE EOI NUMBER NUMBER)
		    '(ignore)))
		  
		  ;; macro definitions
		  (macrodef
		   ((PAR-OPEN DEFMACRO PAR-OPEN IDENT EOI NUMBER@line NUMBER)
		    `(define-macro (,IDENT ,line)))
		   ((PAR-OPEN DEFMACRO PAR-OPEN DEFINE EOI NUMBER@line NUMBER)
		    `(define-macro (,(symbol->string DEFINE) ,line)))
		   ((PAR-OPEN DEFMACRO PAR-OPEN TYPE EOI NUMBER@line NUMBER)
		    `(define-macro ("TYPE" ,line))))
		  
		  ;; module definitions
		  (moduledef
		   ((PAR-OPEN MODULE IDENT EOI NUMBER@line NUMBER)
		    `(module ,IDENT ,line))))))
      (try (read/lalrp lalr reg port)
	   (lambda (escape obj proc msg)
	      (escape #f)))))

;*---------------------------------------------------------------------*/
;*    parse-etags-meta-entry-line ...                                  */
;*---------------------------------------------------------------------*/
(define (parse-etags-meta-entry-line line)
   (let ((port  (open-input-string line))
	 (reg   (regular-grammar ((letter  (in ("azAZ") (#a128 #a255)))
				  (special (in "!@~$%^&*></-_+\\|=?.:"))
				  (id      (: (* digit)
					      (or letter special)
					      (* (or letter
						     special
						     digit
						     (in ",'`"))))))
		   (blank
		    (ignore))
		   ("("
		    (list 'PAR-OPEN))
		   ((+ digit)
		    (cons 'NUMBER (the-fixnum)))
		   (id
		    (let* ((string (the-string))
			   (symbol (the-symbol))
			   (kwd    (getprop symbol *meta-key*)))
		       (if kwd
			   ;; this is a keyword
			   (cons kwd symbol)
			   ;; this is a regular identifier
			   (cons 'IDENT string))))
		   (#a127
		    (list 'EOI))
		   (#\,
		    (ignore))
		   (else
		    (let ((c (the-failure)))
		       (if (eof-object? c)
			   c
			   (bdl-error "parse-etage-meta-entry-line"
				      "Illegal char"
				      c))))))
	 (lalr  (lalr-grammar
		   (IDENT PAR-OPEN EOI NUMBER META-DEFINE)
		   ;; variable keywords
		   (metadef
		    ((PAR-OPEN META-DEFINE IDENT@kd IDENT@id EOI NUMBER NUMBER)
		     `(meta-define ,kd ,id))))))
      (with-exception-handler
	 (lambda (e)
	    (error-notify e)
	    #f)
	 (lambda ()
	    (read/lalrp lalr reg port)))))


