;*=====================================================================*/
;*    .../prgm/project/bigloo/bigloo/comptime/BackEnd/c_emit.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Mar 16 18:14:47 1995                          */
;*    Last change :  Mon Mar 11 14:25:38 2019 (serrano)                */
;*    Copyright   :  1995-2019 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The emission of the C code                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module backend_c_emit
   (import  engine_param
	    engine_configure
	    tools_license
	    type_type
	    type_cache
	    ast_var
	    ast_node
	    cgen_cop
	    (emit-bdb-loc cgen_emit-cop)
	    type_tools)
   (export  (start-emission! suffix::bstring)
	    (stop-emission!)
	    (emit-comment ::bstring ::char)
	    (emit-header)
	    (emit-garbage-collector-selection)
	    (emit-include)
	    (emit-debug-activation)
	    (emit-main)
	    *c-port*
	    (emit-dlopen-init ::global ::bstring)
	    (untrigraph::bstring ::bstring)
	    (llong->c-iso::bstring ::llong)
	    (emit-atom-value value)))

;*---------------------------------------------------------------------*/
;*    *dest-prefix* ...                                                */
;*---------------------------------------------------------------------*/
(define *dest-prefix* #f)
(define *c-port*      #f)

;*---------------------------------------------------------------------*/
;*    start-emission! ...                                              */
;*---------------------------------------------------------------------*/
(define (start-emission! suffix)
   (let* ((prefix (cond
		     ((and (string? *dest*)
			   (memq *pass* '(cgen distrib cc cindent hgen)))
		      (prefix *dest*))
		     ((and (pair? *src-files*) (string? (car *src-files*)))
		      (prefix (car *src-files*)))
		     ((and (string? *dest*) (eq? *pass* 'ld))
		      (prefix *dest*))
		     ((and (string? *dest*) (eq? *pass* 'so))
		      (prefix *dest*))
		     (else
		      #f))))
      (if (or (eq? *dest* '--to-stdout) (not (string? prefix)))
	  (set! *c-port* (current-output-port))
	  (let ((f-name (string-append prefix suffix)))
	     (set! *dest-prefix* prefix)
	     (set! *c-port* (open-output-file f-name))
	     (if (not (output-port? *c-port*))
		 (error *bigloo-name* "Can't open file for output" f-name)
		 #unspecified)))))

;*---------------------------------------------------------------------*/
;*    stop-emission! ...                                               */
;*---------------------------------------------------------------------*/
(define (stop-emission!)
   (cond
      ((not (output-port? *c-port*))
       #f)
      ((eq? *c-port* (current-output-port))
       #f)
      (else
       (flush-output-port *c-port*)
       (close-output-port *c-port*)
       (set! *c-port* #f)
       *dest-prefix*)))

;*---------------------------------------------------------------------*/
;*    *max-col* ...                                                    */
;*---------------------------------------------------------------------*/
(define *max-col* 79)

;*---------------------------------------------------------------------*/
;*    emit-comment ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-comment string fill)
   (let ((string (if (>fx (string-length string) (-fx *max-col* 8))
		     (substring string 0 (-fx *max-col* 9))
		     string)))
      (display "/*" *c-port*)
      (let ((len (string-length string)))
	 (if (=fx len 0)
	     (fprint *c-port* (make-string (-fx *max-col* 4) fill) "*/")
	     (begin
		(display (make-string 2 fill) *c-port*)
		(display #\space *c-port*)
		(display string *c-port*)
		(display #\space *c-port*)
		(fprint *c-port*
			(make-string (-fx *max-col* (+ 8 len)) fill)
			"*/"))))))

;*---------------------------------------------------------------------*/
;*    emit-license ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-license)
   (let ((in (open-input-string (bigloo-license))))
      (let loop ((str (read-line in)))
	 (if (eof-object? str)
	     (close-input-port in)
	     (begin
		(emit-comment str #\space)
		(loop (read-line in)))))))

;*---------------------------------------------------------------------*/
;*    emit-header ...                                                  */
;*---------------------------------------------------------------------*/
(define (emit-header)
   (emit-comment "" #\=)
   (emit-comment (let ((p (open-output-string)))
		    (display *src-files* p)
		    (close-output-port p))
		 #\space)
   (emit-comment *bigloo-name* #\space)
   (emit-comment (string-append *bigloo-author* " (c)      " *bigloo-date*)
		 #\space)
   (if *bigloo-licensing?* (emit-license))
   (emit-comment "" #\=)
   (display "/* COMPILATION: " *c-port*)
   (display (command-line) *c-port*)
   (display " */" *c-port*)
   (newline *c-port*)
   (when *saw*
      (display "\n/* SAW compilation */\n" *c-port*)
      (display "#define BGL_SAW 1\n" *c-port*)
      ;;(display "#define BGL_GC BGL_SAW_GC\n\n" *c-port*)
      ))

;*---------------------------------------------------------------------*/
;*    emit-garbage-collector-selection ...                             */
;*---------------------------------------------------------------------*/
(define (emit-garbage-collector-selection)
   (fprint *c-port* "/* GC selection */")
   (case *garbage-collector*
      ((boehm)
       (fprint *c-port* #"#define THE_GC BOEHM_GC\n"))
      ((bumpy)
       (fprint *c-port* "#define THE_GC BOEHM_GC")
       (fprint *c-port* #"#define BUMPY_GC\n"))
      (else
       (error "emit-garbage-collector-selection"
	      "Can't emit code for gc"
	      *garbage-collector*))))
       
;*---------------------------------------------------------------------*/
;*    emit-include ...                                                 */
;*---------------------------------------------------------------------*/
(define (emit-include)
   ;; the regular include files
   (for-each (lambda (i) (fprint *c-port* "#include <" i ">"))
	     (reverse! *include-foreign*))
   ;; the user additional includes
   (for-each (lambda (i) (fprint *c-port* "#include <" i ">"))
	     (reverse! *additional-include-foreign*))
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-debug-activation ...                                        */
;*---------------------------------------------------------------------*/
(define (emit-debug-activation)
   (fprint *c-port* "/* debug mode */")
   (fprint *c-port* "#define BIGLOO_DEBUG 1")
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-main ...                                                    */
;*---------------------------------------------------------------------*/
(define (emit-main)
   (emit-bdb-loc #unspecified)
   ;; import the library setup functions
   (when (pair? *bigloo-libraries-c-setup*)
      (fprint *c-port* "/* Libraries setup imports */")
      (for-each (lambda (f)
		   (fprint *c-port* "BGL_IMPORT void "
			   f "(int, char *[], char *[]);"))
		*bigloo-libraries-c-setup*)
      (newline *c-port*))

   ;; initialize the libraries
   (fprint *c-port* "/* Libraries setup */")
   (fprint *c-port* "static int bigloo_libinit( int argc, char *argv[], char *env[] ) {")
   (for-each (lambda (f)
		(fprint *c-port* f "(argc, argv, env);\n"))
	     *bigloo-libraries-c-setup*)
   (fprint *c-port* "return 0; }\n\n")
   ;; The bigloo_abort function is only used by the debugger
   ;; and the macro THE_FAILURE. It is used by the debugger as
   ;; a mark where to stop execution after an error. That function
   ;; cannot be located inside the library because of the difficulty
   ;; to stop inside a shared library.
   (fprint *c-port* "long bigloo_abort(long n) { return n; }")
   (emit-bdb-loc #unspecified)
   (fprint *c-port* "int BIGLOO_MAIN(int argc, char *argv[], char *env[]) { ")
   ;; start the application
   (fprintf *c-port* "return _bigloo_main(argc, argv, env, &bigloo_main, &bigloo_libinit, ~a);}"
	    *user-heap-size*)
   (newline *c-port*))

;*---------------------------------------------------------------------*/
;*    emit-dlopen-init ...                                             */
;*---------------------------------------------------------------------*/
(define (emit-dlopen-init global init)
   (let ((sym (if (bigloo-need-mangling? init)
		  (bigloo-mangle init)
		  init)))
      (emit-bdb-loc #unspecified)
      (fprint *c-port* "BGL_EXPORTED_DEF obj_t " sym "() {")
      (emit-bdb-loc #unspecified)
      (fprint *c-port* "obj_t res = " (global-name global) "( 0, \""
	 (car *src-files*) "\" );")
      (fprint *c-port* "BGL_MVALUES_NUMBER_SET(2);")
      (fprint *c-port* "BGL_MVALUES_VAL_SET(1,string_to_bstring( \""
	 (global-module global) "\" ));")
      (fprint *c-port* "return res;")
      (emit-bdb-loc #unspecified)
      (fprint *c-port* "}")
      (newline *c-port*)))

;*---------------------------------------------------------------------*/
;*    llong->c-iso ...                                                 */
;*---------------------------------------------------------------------*/
(define (llong->c-iso::bstring llong::llong)
   (define (positive allong)
      (let* ((bits 16)
	     (shift (bit-lshllong #l1 bits))
	     (sshift (llong->string shift))
	     (mask (-llong shift #l1)))
	 (let loop ((allong allong))
	    (if (>=llong allong shift)
		(string-append
		 "("
		 (llong->string (bit-andllong allong mask))
		 " + ((BGL_LONGLONG_T)" sshift " * ("
		 (loop (bit-rshllong allong bits)) ")))")
		(string-append "((BGL_LONGLONG_T)" (llong->string allong)
			       ")")))))
   (if (>=llong llong #l0)
       (positive llong)
       (string-append "(-" (positive (absllong llong)) ")")))

;*---------------------------------------------------------------------*/
;*    emit-atom-value ...                                              */
;*---------------------------------------------------------------------*/
(define (emit-atom-value value)
   (cond
      ((boolean? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *bool*)) *c-port*)
       (display #\) *c-port*)
       (display (if value 1 0) *c-port*)
       (display #\) *c-port*))
      ((null? value)
       (display "BNIL" *c-port*))
      ((char? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *char*)) *c-port*)
       (display ")" *c-port*)
       (cond
	  ((=fx (char->integer value) 0)
	   (display "'\\000'" *c-port*))
	  ((char=? value #\')
	   (display "'\\''" *c-port*))
	  ((char=? value #\\)
	   (display "'\\\\'" *c-port*))
	  ((and (>=fx (char->integer value) 32) (<fx (char->integer value) 128))
	   (write-char #\' *c-port*)
	   (write-char value *c-port*)
	   (write-char #\' *c-port*))
	  (else
	   (display (integer->string (char->integer value)) *c-port*)))
       (write-char #\) *c-port*))
      ((int8? value) 
       (display "(int8_t)(" *c-port*)
       (display (int8->fixnum value) *c-port*)
       (display #\) *c-port*))
      ((uint8? value) 
       (display "(uint8_t)(" *c-port*)
       (display (uint8->fixnum value) *c-port*)
       (display #\) *c-port*))
      ((int16? value) 
       (display "(int16_t)(" *c-port*)
       (display (int16->fixnum value) *c-port*)
       (display #\) *c-port*))
      ((uint16? value) 
       (display "(uint16_t)(" *c-port*)
       (display (uint16->fixnum value) *c-port*)
       (display #\) *c-port*))
      ((int32? value) 
       (display "(int32_t)(" *c-port*)
       (display (int32->llong value) *c-port*)
       (display #\) *c-port*))
      ((uint32? value) 
       (display "(uint32_t)(" *c-port*)
       (display (uint32->llong value) *c-port*)
       (display #\) *c-port*))
      ((int64? value) 
       (display "(int64_t)(" *c-port*)
       (display (int64->llong value) *c-port*)
       (display #\) *c-port*))
      ((uint64? value) 
       (display "(uint64_t)(" *c-port*)
       (display (uint64->llong value) *c-port*)
       (display #\) *c-port*))
      ((ucs2? value)
       (display "BUCS2(" *c-port*)
       (display (ucs2->integer value) *c-port*)
       (display #\) *c-port*))
      ((eq? value #unspecified)
       (display "BUNSPEC" *c-port*))
      ((string? value)
       (display #\" *c-port*)
       (display (untrigraph (string-for-read value)) *c-port*)
       (display #\" *c-port*))
      ((fixnum? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *long*)) *c-port*)
       (display ")" *c-port*)
       (display value *c-port*)
       (display ")" *c-port*))
      ((flonum? value)
       (cond
	  ((nanfl? value)
	   (display "BGL_NAN" *c-port*))
	  ((and (infinitefl? value) (>fl value 0.0))
	   (display "BGL_INFINITY" *c-port*))
	  ((infinitefl? value)
	   (display "(-BGL_INFINITY)" *c-port*))
	  (else
	   (display "((" *c-port*)
	   (display (string-sans-$ (type-name *real*)) *c-port*)
	   (display ")" *c-port*)
	   (display value *c-port*)
	   (display ")" *c-port*))))
      ((elong? value)
       (display "((" *c-port*)
       (display (string-sans-$ (type-name *elong*)) *c-port*)
       (display ")" *c-port*)
       (display (elong->string value) *c-port*)
       (display ")" *c-port*))
      ((llong? value)
       (display (llong->c-iso value) *c-port*))
      ((cnst? value)
       (cond
	  ((eof-object? value)
	   (display "(BEOF)" *c-port*))
	  ((eq? value boptional)
	   (display "(BOPTIONAL)" *c-port*))
	  ((eq? value bkey)
	   (display "(BKEY)" *c-port*))
	  ((eq? value brest)
	   (display "(BREST)" *c-port*))
	  ((eq? value __eoa__)
	   (display "(BEOA)" *c-port*))
	  (else
	   (display "BCNST(" *c-port*)
	   (display (cnst->integer value) *c-port*)
	   (display #\) *c-port*))))
      ((bignum? value)
       (display "(bgl_string_to_bignum( \"" *c-port*)
       (display (number->string value 16) *c-port*)
       (display "\", 16 ))" *c-port*))
      (else
       (display value *c-port*))))

;*---------------------------------------------------------------------*/
;*    untrigraph ...                                                   */
;*    -------------------------------------------------------------    */
;*    We remove ?? and replace it by \077\077 (the octal ascii         */
;*    code of ?) in order to avoir C trigraph confusions.              */
;*---------------------------------------------------------------------*/
(define (untrigraph from)
   (let* ((len   (string-length from))
	  (len-2 (-fx len 2)))
      ;; first we count how many collisions we have
      (let ((nb-col (let loop ((i      0)
			       (nb-col 0))
		       (cond
			  ((>fx i len-2)
			   nb-col)
			  ((not (char=? (string-ref from i) #\?))
			   (loop (+fx i 1) nb-col))
			  ((not (char=? (string-ref from (+fx i 1)) #\?))
			   (loop (+fx i 2) nb-col))
			  ;; yes, we have one
			  (else
			   (loop (+fx i 2) (+fx nb-col 1)))))))
	 (if (=fx nb-col 0)
	     ;; there is no trigraph clashes
	     from
	     ;; there is some, we allocate a new string. Each trigraph
	     ;; require 4 times its size.
	     (let ((res   (make-string (+fx len (*fx 3 (*fx nb-col 2)))))
		   (len-1 (-fx len 1)))
		(let loop ((r 0)
			   (w 0))
		   (cond
		      ((=fx r len)
		       res)
		      ((or (not (char=? (string-ref from r) #\?))
			   (>fx r len-2))
		       (string-set! res w (string-ref from r))
		       (loop (+fx r 1) (+fx w 1)))
		      ((not (char=? (string-ref from (+fx r 1)) #\?))
		       (string-set! res w #\?)
		       (string-set! res (+fx w 1) (string-ref from (+fx r 1)))
		       (loop (+fx r 2) (+fx w 2)))
		      (else
		       ;; this is a trigraph
		       (string-set! res w #\\)
		       (string-set! res (+fx w 1) #\0)
		       (string-set! res (+fx w 2) #\7)
		       (string-set! res (+fx w 3) #\7)
		       (string-set! res (+fx w 4) #\\)
		       (string-set! res (+fx w 5) #\0)
		       (string-set! res (+fx w 6) #\7)
		       (string-set! res (+fx w 7) #\7)
		       (loop (+fx r 2) (+fx w 8))))))))))

