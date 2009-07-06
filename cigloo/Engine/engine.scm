;*=====================================================================*/
;*    serrano/prgm/project/bigloo/cigloo/Engine/engine.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jul 12 15:53:47 1995                          */
;*    Last change :  Thu Nov  4 08:05:19 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    We have read the argument line. We start the real compilation    */
;*    process.                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_engine
   (export (cigloo)
	   (translate-file <string> <bool> <symbol>))
   (import tools_speek
	   engine_param
	   engine_translate
	   translate_eval
	   parser_lexer))

;*---------------------------------------------------------------------*/
;*    bigloo-name                                                      */
;*---------------------------------------------------------------------*/
(define-macro (bigloo-name)
   *bigloo-name*)

;*---------------------------------------------------------------------*/
;*    cigloo ...                                                       */
;*---------------------------------------------------------------------*/
(define (cigloo)
   ;; the reader settup
   (init-lexer!)
   (if (string? *dest*)
       (begin
	  (set! *oport* (open-output-file *dest*))
	  (if (not (output-port? *oport*))
	      (error "cigloo" "Can't open file for output" *dest*)
	      (unwind-protect (engine) (close-output-port *oport*))))
       (begin
	  ;; when emiting on stdout we switch to a silent mode
	  (set! *verbose* -1)
	  (engine))))

;*---------------------------------------------------------------------*/
;*    *file-processed* ...                                             */
;*---------------------------------------------------------------------*/
(define *file-processed* '())

;*---------------------------------------------------------------------*/
;*    engine ...                                                       */
;*---------------------------------------------------------------------*/
(define (engine)
   ;; first of all, we emit identification comment and
   ;; the include Bigloo clauses.
   (if (>=fx *verbose* 0)
       (fprint *oport* ";; " *cigloo-name*
	       ", to be used with " (bigloo-name) "."))
   (if *directives*
       (fprint *oport* "(directives"))
   (fprint *oport* " (extern")
   ;; we start emiting the opaque types
   (for-each (lambda (name)
		(fprint *oport* "(type " name " (opaque) \"" name "\")"))
	     *opaque-type*)
   ;; we compile the files
   (if (null? *src*)
       (translate-stdin 'emit)
       (for-each (lambda (fname) (translate-file fname 'file 'open))
		 (reverse! *src*)))
   ;; and we close include clauses
   (if *directives*
       (if *eval-stub?*
	   (begin
	      (fprint *oport* "   )")
	      (translate-eval-declarations)
	      (fprint *oport* "   )")
	      (translate-eval-stubs))
	   (fprint *oport* "   ))"))
       (fprint *oport* "   )")))

;*---------------------------------------------------------------------*/
;*    translate-stdin ...                                              */
;*---------------------------------------------------------------------*/
(define (translate-stdin mode)
   (translate (current-input-port) "stdin" mode))
 
;*---------------------------------------------------------------------*/
;*    translate-file ...                                               */
;*---------------------------------------------------------------------*/
(define (translate-file fname path mode)
   [assert check (mode) (or (eq? mode 'open) (eq? mode 'scan))]
   (let ((fname (cond
		   ((eq? path '<include>)
		    (or (and *src-dirname*
			     (find-file/path 
			      (string-append *src-dirname* "/" fname)
			      *include-path*))
			(find-file/path fname *include-path*)))
		   ((eq? path 'include)
		    (if *src-dirname*
			(string-append *src-dirname* "/" fname)
			fname))
		   (else
		    (set! *src-dirname* (dirname fname))
		    (if (string=? *src-dirname* ".")
			(set! *src-dirname* #f))
		    fname))))
      (cond
	 ((or (not (string? fname)) (not (file-exists? fname)))
	  (error "cigloo" "Can't find file " fname))
	 ((member fname *file-processed*)
	  'done)
	 (else
	  (set! *file-processed* (cons fname *file-processed*))
	  (let ((port (open-input-file fname)))
	     (if (not (input-port? port))
		 (error "cigloo" "Can't open file for input" fname)
		 (unwind-protect (translate port fname mode)
				 (close-input-port port))))))))
   


