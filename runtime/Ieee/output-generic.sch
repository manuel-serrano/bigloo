;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/output-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 22 15:24:13 2024                          */
;*    Last change :  Wed Feb  5 11:48:10 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Portable output implementation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (bgl_write_char::obj ::bchar ::output-port)
	   (bgl_display_ucs2::obj ::bucs2 ::output-port)
	   (bgl_write_ucs2::obj ::bucs2 ::output-port)
           (bgl_display_fixnum::obj ::bint ::output-port)
           (bgl_display_elong::obj ::elong ::output-port)
           (bgl_write_elong::obj ::elong ::output-port)
           (bgl_display_llong::obj ::llong ::output-port)
           (bgl_write_llong::obj ::elong ::output-port)
	   (bgl_display_bignum::obj ::bignum ::output-port)
	   (bgl_write_bignum::obj ::bignum ::output-port)
	   (bgl_write_mmap::obj ::mmap ::output-port)
	   (bgl_write_process::obj ::process ::output-port)
	   (bgl_write_unknown::obj ::obj ::output-port)
	   (bgl_display_ucs2string::obj ::ucs2string ::output-port)
	   (bgl_write_utf8string::obj ::bstring ::output-port)
	   (inline $$display-fixnum::obj ::bint ::output-port)
	   (inline $$write-procedure ::procedure ::output-port)
	   (inline $$write-input-port ::input-port ::output-port)
	   (inline $$write-output-port ::output-port ::output-port)
	   ($$write-cnst ::obj ::output-port))
   (extern (export bgl_write_char "bgl_write_char")
	   (export bgl_display_ucs2 "bgl_display_ucs2")
	   (export bgl_write_ucs2 "bgl_write_ucs2")
           (export bgl_display_fixnum "bgl_display_fixnum")
           (export bgl_display_elong "bgl_display_elong")
           (export bgl_write_elong "bgl_write_elong")
           (export bgl_display_llong "bgl_display_llong")
	   (export bgl_write_llong "bgl_write_llong")
           (export bgl_display_bignum "bgl_display_bignum")
	   (export bgl_write_bignum "bgl_write_bignum")
	   (export bgl_write_mmap "bgl_write_mmap")
	   (export bgl_write_process "bgl_write_process")
	   (export bgl_write_unknown "bgl_write_unknown")
	   (export bgl_display_ucs2string "bgl_display_ucs2string")
	   (export bgl_write_utf8string "bgl_write_utf8string")))

;*---------------------------------------------------------------------*/
;*    alpha ...                                                        */
;*---------------------------------------------------------------------*/
(define (alpha n)
   (string-ref "0123456789abcdef" n))

;*---------------------------------------------------------------------*/
;*    char-names ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (char-names)
   `',(apply vector
	 (map (lambda (n)
		 (call-with-output-string
		    (lambda (op)
		       (write (integer->char n) op))))
	    (iota 256))))

;*---------------------------------------------------------------------*/
;*    bgl_write_char ...                                               */
;*---------------------------------------------------------------------*/
(define (bgl_write_char o op)
   (define names (char-names))
   ($display-string (vector-ref names (char->integer o)) op))

;*---------------------------------------------------------------------*/
;*    bgl_display_ucs2 ...                                             */
;*---------------------------------------------------------------------*/
(define (bgl_display_ucs2 o op)
   (let ((ch (ucs2->integer o)))
      (if (<fx ch 256)
	  (bgl_display_fixnum ch op)
	  (bgl_write_ucs2 o op))))

;*---------------------------------------------------------------------*/
;*    bgl_write_ucs2 ...                                               */
;*---------------------------------------------------------------------*/
(define (bgl_write_ucs2 o op)
   (let* ((ch (ucs2->integer o))
	  (str (string-append "#u" (integer->string/padding ch 4 16))))
      ($display-string str op)))
	     
;*---------------------------------------------------------------------*/
;*    bgl_display_fixnum ...                                           */
;*---------------------------------------------------------------------*/
(define (bgl_display_fixnum o op)
   ($display-string (integer->string o) op))

;*---------------------------------------------------------------------*/
;*    bgl_display_elong ...                                            */
;*---------------------------------------------------------------------*/
(define (bgl_display_elong o op)
   (display-fixnum ($long->bint ($elong->long o)) op))

;*---------------------------------------------------------------------*/
;*    bgl_write_elong ...                                              */
;*---------------------------------------------------------------------*/
(define (bgl_write_elong o op)
   (display-string "#e" op)
   (display-fixnum ($long->bint ($elong->long o)) op))

;*---------------------------------------------------------------------*/
;*    bgl_display_llong ...                                            */
;*---------------------------------------------------------------------*/
(define (bgl_display_llong o op)
   (display-fixnum ($long->bint ($llong->long o)) op))

;*---------------------------------------------------------------------*/
;*    bgl_write_llong ...                                              */
;*---------------------------------------------------------------------*/
(define (bgl_write_llong o op)
   (display-string "#l" op)
   (display-fixnum ($long->bint ($llong->long o)) op))

;*---------------------------------------------------------------------*/
;*    bgl_display_bignum ...                                           */
;*---------------------------------------------------------------------*/
(define (bgl_display_bignum o op)
   (display-string (bignum->string o) op))

;*---------------------------------------------------------------------*/
;*    bgl_write_bignum ...                                             */
;*---------------------------------------------------------------------*/
(define (bgl_write_bignum o op)
   (display-string "#z" op)
   (display-string (bignum->string o) op))

;*---------------------------------------------------------------------*/
;*    bgl_write_mmap ...                                               */
;*---------------------------------------------------------------------*/
(define (bgl_write_mmap o op)
   (display-string "#<mmap:" op)
   (display (mmap-name o) op)
   (display-string ">" op))

;*---------------------------------------------------------------------*/
;*    bgl_write_process ...                                            */
;*---------------------------------------------------------------------*/
(define (bgl_write_process o op)
   (display-string "#<process:" op)
   (display (process-pid o) op)
   (display-string ">" op))

;*---------------------------------------------------------------------*/
;*    bgl_write_unknown ...                                            */
;*---------------------------------------------------------------------*/
(define (bgl_write_unknown o op)
   (display-string "#<unknown>" op))

;*---------------------------------------------------------------------*/
;*    $$display-fixnum ...                                             */
;*---------------------------------------------------------------------*/
(define-inline ($$display-fixnum o op)
   (display-string (fixnum->string o 10) op))
   
;*---------------------------------------------------------------------*/
;*    $$write-procedure ...                                            */
;*---------------------------------------------------------------------*/
(define-inline ($$write-procedure o op)
   (display-string "#<procedure:" op)
   (bgl_display_fixnum (procedure-arity o) op)
   (display ">" op))

;*---------------------------------------------------------------------*/
;*    $$write-input-port ...                                           */
;*---------------------------------------------------------------------*/
(define-inline ($$write-input-port o op)
   (display-string "#<input-port:" op)
   (display-string (input-port-name o) op)
   (display-string "." op)
   (bgl_display_fixnum (string-length (input-port-buffer o)) op)
   (display-string ">" op))

;*---------------------------------------------------------------------*/
;*    $$write-output-port ...                                          */
;*---------------------------------------------------------------------*/
(define-inline ($$write-output-port o op)
   (display-string "#<output-port:" op)
   (display-string (output-port-name o) op)
   (display-string ">" op))

;*---------------------------------------------------------------------*/
;*    write-cnst-string ...                                            */
;*---------------------------------------------------------------------*/
(define-macro (write-cnst-string cnst op)
   (let ((s (call-with-output-string (lambda (p) (write cnst p)))))
      `(display ,s ,op)))

;*---------------------------------------------------------------------*/
;*    $$write-cnst ...                                                 */
;*---------------------------------------------------------------------*/
(define ($$write-cnst o op)
   (cond
      ((eq? o #t) (write-cnst-string #t op))
      ((eq? o #f) (write-cnst-string #f op))
      ((null? o) (write-cnst-string '() op))
      ((eq? o #unspecified) (write-cnst-string #unspecified op))
      (else (display "#<???>" op))))

;*---------------------------------------------------------------------*/
;*    bgl_display_ucs2string ...                                       */
;*---------------------------------------------------------------------*/
(define (bgl_display_ucs2string s op)
   (let ((len (ucs2-string-length s)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (let ((c (ucs2->integer (ucs2-string-ref s i))))
	       (when (<fx c 256)
		  ($display-byte c op))
	       (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    bgl_write_utf8string ...                                         */
;*---------------------------------------------------------------------*/
(define (bgl_write_utf8string s op)
   (display "#u\"" op)
   (display-string s op)
   (display "\"" op))
