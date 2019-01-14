;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/output.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jul  5 11:13:01 1992                          */
;*    Last change :  Mon Jan 14 14:01:03 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.10.3 Output (page 31, r4)                                      */
;*    -------------------------------------------------------------    */
;*    This module is able to display object even if it is not          */
;*    properly initailzed. This is very important because with this    */
;*    means, error during the initialization time will be correctly    */
;*    prompted. In particular, we must be very carefull not to launch  */
;*    unexpected errors.                                               */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Input And Output@                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_output_6_10_3

   (import  __error
	    __bexit
	    __r4_ports_6_10_1
	    __r4_numbers_6_5_flonum_dtoa
   	    __bigloo
	    __srfi4
	    __param)
   
   (use     __type
	    __tvector
            __weakptr
	    __structure
	    __bexit
	    __object
	    __ucs2
	    __unicode
	    __date
	    __thread
	    __rgc
	    __foreign
	    __mmap
	    __semaphore
	    __process
	    __socket
	    __custom
	    __object
	    __bigloo
	    __binary
	    __bignum
	    __regexp
	    __pp_circle
	    __bit
	    
	    __r4_ports_6_10_1
	    __r4_input_6_10_2
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_numbers_6_5
	    __r4_characters_6_6
	    __r4_strings_6_7
	    __r4_vectors_6_8
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r5_control_features_6_4

	    __evenv)
     
   (extern  ($write-char::obj (::bchar ::output-port)
	       "bgl_write_char")
	    (macro $display-char::obj (::uchar ::output-port)
		   "bgl_display_char")
	    (macro $display-byte::obj (::ubyte ::output-port)
		   "bgl_display_char")
	    ($write-ucs2::obj (::bucs2 ::output-port)
	       "bgl_write_ucs2")
	    ($display-ucs2::obj (::bucs2 ::output-port)
	       "bgl_display_ucs2")
	    ($write-string::obj (::bstring ::bool ::output-port)
	       "bgl_write_string")
	    (macro $display-string::obj (::bstring ::output-port)
		   "bgl_display_string")
	    (macro $display-substring::obj (::bstring ::long ::long ::output-port)
		   "bgl_display_substring")
	    ($display-fixnum::obj (::bint ::output-port)
	       "bgl_display_fixnum")
	    ($write-elong::obj (::elong ::output-port)
	       "bgl_write_elong")
	    ($display-elong::obj (::elong ::output-port)
	       "bgl_display_elong")
	    ($write-llong::obj (::llong ::output-port)
	       "bgl_write_llong")
	    ($display-llong::obj (::llong ::output-port)
	       "bgl_display_llong")
	    ($write-bignum::obj (::bignum ::output-port)
	       "bgl_write_bignum")
	    ($display-bignum::obj (::bignum ::output-port)
	       "bgl_display_bignum")
	    ($write-utf8string::obj (::bstring ::output-port)
	       "bgl_write_utf8string")
	    ($display-ucs2string::obj (::ucs2string ::output-port)
	       "bgl_display_ucs2string")
	    ($write-cnst::obj (::obj ::output-port)
	       "bgl_write_cnst")
	    ($write-procedure::obj (::procedure ::output-port)
	       "bgl_write_procedure")
	    ($write-output-port::obj (::output-port ::output-port)
	       "bgl_write_output_port")
	    ($write-input-port::obj (::input-port ::output-port)
	       "bgl_write_input_port")
	    ($write-binary-port::obj (::binary-port ::output-port)
	       "bgl_write_binary_port")
	    ($write-foreign::obj (::foreign ::output-port)
	       "bgl_write_foreign")
	    ($write-dynamic-env::obj (::dynamic-env ::output-port)
	       "bgl_write_dynamic_env")
	    ($write-process::obj (::process ::output-port)
	       "bgl_write_process")
	    ($write-socket::obj (::socket ::output-port)
	       "bgl_write_socket")
	    ($write-datagram-socket::obj (::datagram-socket ::output-port)
	       "bgl_write_datagram_socket")
	    ($write-regexp::obj (::regexp ::output-port)
	       "bgl_write_regexp")
	    ($write-mmap::obj (::mmap ::output-port)
	       "bgl_write_mmap")
	    ($write-semaphore::obj (::semaphore ::output-port)
	       "bgl_write_semaphore")
	    ($write-opaque::obj (::obj ::output-port)
	       "bgl_write_opaque")
	    ($write-custom::obj (::obj ::output-port)
	       "bgl_write_custom")
	    ($write-unknown::obj (::obj ::output-port)
	       "bgl_write_unknown")
	    ($ill-char-rep::obj (::uchar)
	       "bgl_ill_char_rep")
	    
	    (export write-2 "bgl_write_obj")
	    (export display-2 "bgl_display_obj"))
    
   (java    (class foreign
	       (method static $write-char::obj (::bchar ::output-port)
		  "write_char")
	       (method static $display-char::obj (::uchar ::output-port)
		  "display_char")
	       (method static $display-byte::obj (::ubyte ::output-port)
		  "display_char")
	       (method static $write-ucs2::obj (::bucs2 ::output-port)
		  "write_ucs2")
	       (method static $display-ucs2::obj (::bucs2 ::output-port)
		  "display_ucs2")
	       (method static $write-string::obj (::bstring ::bool ::output-port)
		  "write_string")
	       (method static $display-string::obj (::bstring ::output-port)
		  "display_string")
	       (method static $display-substring::obj (::bstring ::long ::long ::output-port)
		  "display_substring")
	       (method static $display-fixnum::obj (::bint ::output-port)
		  "display_fixnum")
	       (method static $write-elong::obj (::elong ::output-port)
		  "write_elong")
	       (method static $display-elong::obj (::elong ::output-port)
		  "display_elong")
	       (method static $write-llong::obj (::llong ::output-port)
		  "write_llong")
	       (method static $display-llong::obj (::llong ::output-port)
		  "display_llong")
	       (method static $write-bignum::obj (::bignum ::output-port)
		  "bgl_write_bignum")
	       (method static $display-bignum::obj (::bignum ::output-port)
		  "bgl_display_bignum")
	       (method static $write-utf8string::obj (::bstring ::output-port)
		  "write_utf8string")
	       (method static $display-ucs2string::obj (::ucs2string ::output-port)
		  "display_ucs2string")
	       (method static $write-cnst::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-procedure::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-output-port::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-input-port::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-binary-port::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-foreign::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-process::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-socket::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-datagram-socket::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-regexp::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-mmap::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-semaphore::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-opaque::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-custom::obj (::obj ::output-port)
		  "write_object")
	       (method static $write-unknown::obj (::obj ::output-port)
		  "write_object")
	       (method static $ill-char-rep::obj (::uchar)
		  "ill_char_rep")))
    
   (export  (newline . port)
	    (inline newline-1 ::output-port)
	    (write obj . port)
	    (write-2 obj ::output-port)
	    (display obj . port)
	    (display-2 obj ::output-port)
	    (write-char ::uchar #!optional (port (current-output-port)))
	    (write-byte ::ubyte #!optional (port (current-output-port)))
	    (write-symbol ::symbol ::output-port)
	    (inline write-char-2 ::uchar ::output-port)
	    (inline write-byte-2 ::ubyte ::output-port)
	    (inline display-string ::bstring ::output-port)
	    (inline display-substring ::bstring ::long ::long ::output-port)
	    (write-string ::bstring ::output-port)
	    (display-symbol ::symbol ::output-port)
	    (write-symbol ::symbol ::output-port)
	    (inline display-fixnum ::bint ::output-port)
	    (inline display-elong ::elong ::output-port)
	    (display-flonum ::real ::output-port)
	    (display-ucs2string ::ucs2string ::output-port)
	    (write-ucs2string ::ucs2string ::output-port)
	    (illegal-char-rep ::uchar) 
	    (display* . obj)
	    (write* . obj)
	    (print . obj)
	    (fprint ::output-port . obj)
	    (tprint ::output-port . obj)
	    (format::bstring ::bstring . obj)
	    (printf ::bstring . obj)
	    (fprintf ::output-port ::bstring . obj))

   (pragma  (display-2 no-init-flow no-cfa-top)
	    (write-2 no-init-flow no-cfa-top)
	    ($display-char nesting)
	    ($write-char nesting)))
   
;*---------------------------------------------------------------------*/
;*    newline ...                                                      */
;*---------------------------------------------------------------------*/
(define (newline . port)
   (let ((port (match-case port
		  (()
		   (current-output-port))
		  ((?-)
		   (car port))
		  (else
		   (error "newline"
			  "wrong number of optional arguments"
			  port)))))
      (newline-1 port)))

;*---------------------------------------------------------------------*/
;*    newline-1 ...                                                    */
;*---------------------------------------------------------------------*/
(define-inline (newline-1 port::output-port)
   ($display-char #\Newline port))

;*---------------------------------------------------------------------*/
;*    display ...                                                      */
;*---------------------------------------------------------------------*/
(define (display obj . port)
   (let ((port (match-case port
		  (()
		   (current-output-port))
		  ((?-)
		   (car port))
		  (else
		   (error "display"
			  "wrong number of optional arguments"
			  port)))))
      (display-2 obj port)))

;*---------------------------------------------------------------------*/
;*    write ...                                                        */
;*---------------------------------------------------------------------*/
(define (write obj . port)
   (let ((port (match-case port
		  (()
		   (current-output-port))
		  ((?-)
		   (car port))
		  (else
		   (error "write"
			  "wrong number of optional arguments"
			  port)))))
      (write-2 obj port)))

;*---------------------------------------------------------------------*/
;*    write-char ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-char char #!optional (port (current-output-port)))
   (write-char-2 char port))

;*---------------------------------------------------------------------*/
;*    write-char-2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (write-char-2 char::uchar port::output-port)
   ($display-char char port))
 
;*---------------------------------------------------------------------*/
;*    write-byte ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-byte byte #!optional (port (current-output-port)))
   (write-byte-2 byte port))

;*---------------------------------------------------------------------*/
;*    write-byte-2 ...                                                 */
;*---------------------------------------------------------------------*/
(define-inline (write-byte-2 byte::ubyte port::output-port)
   ($display-byte byte port))

;*---------------------------------------------------------------------*/
;*    illegal-char-rep ...                                             */
;*---------------------------------------------------------------------*/
(define (illegal-char-rep char)
   (if (or (char-alphabetic? char) (char-numeric? char))
       char
       (case char
	  ((#\Newline)
	   "#\Newline")
	  ((#\Return)
	   "#\Return")
	  ((#\Space)
	   "#\Space")
	  ((#\Tab)
	   "#\Tab")
	  (else
	   (let ((i (char->integer char)))
	      (if (<fx i (char->integer #\!))
		  ($ill-char-rep char)
		  char))))))

;*---------------------------------------------------------------------*/
;*    print ...                                                        */
;*---------------------------------------------------------------------*/
(define (print . obj)
   (let ((port (current-output-port)))
      (let loop ((l   obj)
		 (res '()))
	 (if (null? l)
	     (begin
		($display-char #\Newline port)
		res)
	     (let ((v (car l)))
		(display-2 v port)
		(loop (cdr l) v))))))

;*---------------------------------------------------------------------*/
;*    display* ...                                                     */
;*---------------------------------------------------------------------*/
(define (display* . obj)
   (let ((port (current-output-port)))
      (let loop ((l obj))
	 (if (null? l)
	     #unspecified
	     (let ((v (car l)))
		(display (car l) port)
		(loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    write* ...                                                       */
;*---------------------------------------------------------------------*/
(define (write* . obj)
   (let ((port (current-output-port)))
      (let loop ((l obj))
	 (if (null? l)
	     #unspecified
	     (let ((v (car l)))
		(write-2 (car l) port)
		(loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    tprint-mutex ...                                                 */
;*---------------------------------------------------------------------*/
(define tprint-mutex (make-mutex "tprint"))

;*---------------------------------------------------------------------*/
;*    tprint ...                                                       */
;*---------------------------------------------------------------------*/
(define (tprint port . obj)
   (synchronize tprint-mutex
      (apply fprint port obj)
      (flush-output-port port)))

;*---------------------------------------------------------------------*/
;*    fprint ...                                                       */
;*---------------------------------------------------------------------*/
(define (fprint port . obj)
   (let loop ((l obj)
	      (res '()))
      (if (null? l)
	  (begin
	     ($display-char #\Newline port)
	     res)
	  (let ((v (car l)))
	     (display-2 (car l) port)
	     (loop (cdr l) v)))))

;*---------------------------------------------------------------------*/
;*    xprintf ...                                                      */
;*---------------------------------------------------------------------*/
(define (xprintf procname p::output-port _fmt::bstring objs::pair-nil)
   (let ((len (string-length _fmt)))
      (let loop ((i 0)
		 (os objs))
	 
	 (define (next os fmt)
	    (if (null? os)
		(error procname "Insufficient number of arguments" fmt)
		(car os)))
	 
	 (define (print-radix radix num)
	    (if (not (number? num))
		(bigloo-type-error procname "number" num)
		(display (number->string num radix) p)))
	 
	 (define (print-flat-list l p sep)
	    (cond
	       ((pair? l)
		(let loop ((l l))
		   (print-flat-list (car l) p sep)
		   (cond
		      ((pair? (cdr l))
		       (display sep p)
		       (loop (cdr l)))
		      ((not (null? (cdr l)))
		       (display " . " p)
		       (print-flat-list (cdr l) p sep)))))
	       ((not (null? l))
		(display l p))))
	 
	 (define (print-list i l p)
	    (let ((j (string-index _fmt #\) i)))
	       (if (not j)
		   (error procname "Illegal tag" _fmt)
		   (let ((sep (substring _fmt (+fx i 1) j)))
		      (print-flat-list l p sep)
		      (+fx j 1)))))
	 
	 (define (print-padded-number i num mincol padding)
	    (if (=fx i len)
		(error procname "Illegal tag" _fmt)
		(let* ((s (case (string-ref _fmt i)
			     ((#\d #\D)
			      (number->string num 10))
			     ((#\x #\X)
			      (number->string num 16))
			     ((#\o #\O)
			      (number->string num 8))
			     ((#\b #\B)
			      (number->string num 2))
			     (else
			      (error procname "Illegal tag" _fmt))))
		       (l (string-length s)))
		   (when (<fx l mincol)
		      (display (make-string (-fx mincol l) padding) p))
		   (display s p)
		   (+fx i 1))))
	 
	 (define (print-formatted-number i num p)
	    (if (not (number? num))
		(bigloo-type-error procname "number" num)
		(let ((j (string-skip _fmt "0123456789" i)))
		   (cond
		      ((not j)
		       (error procname "Illegal tag" _fmt))
		      ((char=? (string-ref _fmt j) #\,)
		       (if (=fx j (-fx len 1))
			   (error procname "Illegal tag" _fmt)
			   (print-padded-number (+fx j 2)
			      num
			      (string->integer (substring _fmt i j))
			      (string-ref _fmt (+fx j 1)))))
		      (else
		       (print-padded-number j
			  num
			  (string->integer (substring _fmt i j))
			  #\space))))))

	 (define (handle-tag f i alt?)
	    (case f
	       ((#\a #\A)
		(if alt?
		    (display-circle (next os f) p)
		    (display (next os f) p))
		(loop (+fx i 1) (cdr os)))
	       ((#\s #\S)
		(if alt?
		    (write-circle (next os f) p)
		    (write (next os f) p))
		(loop (+fx i 1) (cdr os)))
	       ((#\v #\V)
		(if alt?
		    (display-circle (next os f) p)
		    (display (next os f) p))
		(newline p)
		(loop (+fx i 1) (cdr os)))
	       ((#\c #\C)
		(let ((o (next os f)))
		   (if (not (char? o))
		       (error procname "Illegal char" o)
		       (begin
			  (write-char o p)
			  (loop (+fx i 1) (cdr os))))))
	       ((#\d #\D)
		(print-radix 10 (next os f))
		(loop (+fx i 1) (cdr os)))
	       ((#\x #\X)
		(print-radix 16 (next os f))
		(loop (+fx i 1) (cdr os)))
	       ((#\o #\O)
		(print-radix 8 (next os f))
		(loop (+fx i 1) (cdr os)))
	       ((#\b #\B)
		(print-radix 2 (next os f))
		(loop (+fx i 1) (cdr os)))
	       ((#\% #\n)
		(newline p)
		(loop (+fx i 1) os))
	       ((#\r)
		(write-char #\return p)
		(loop (+fx i 1) os))
	       ((#\l #\L)
		(print-flat-list (next os f) p " ")
		(loop (+fx i 1) (cdr os)))
	       ((#\()
		(let ((ni (print-list i (next os f) p)))
		   (loop ni (cdr os))))
	       ((#\~)
		(write-char #\~ p)
		(loop (+fx i 1) os))
	       (else
		(if (char-numeric? f)
		    (let ((ni (print-formatted-number i (next os f) p)))
		       (loop ni (cdr os)))
		    (error procname
		       (string-append
			  "Illegal tag \"" (string f) "\"") _fmt)))))

	 (if (<fx i len)
	     (let ((c (string-ref _fmt i)))
		(if (char=? c #\~)
		    (cond
		       ((=fx i (-fx len 1))
			(error procname
			   "Tag not allowed here"
			   (substring _fmt i len)))
		       ((char=? #\: (string-ref _fmt (+fx i 1)))
			(if (=fx i (-fx len 2))
			    (error procname
			       "Tag not allowed here"
			       (substring _fmt i len))
			    (handle-tag (string-ref _fmt (+fx i 2))
			       (+fx i 2)
			       #t)))
		       (else
			(handle-tag (string-ref _fmt (+fx i 1))
			   (+fx i 1)
			   #f)))
		    (begin
		       (write-char c p)
		       (loop (+fx i 1) os))))))))
			 
;*---------------------------------------------------------------------*/
;*    format ...                                                       */
;*---------------------------------------------------------------------*/
(define (format fmt . obj)
   (let ((p (open-output-string)))
      (xprintf 'format p fmt obj)
      (close-output-port p)))

;*---------------------------------------------------------------------*/
;*    printf ...                                                       */
;*---------------------------------------------------------------------*/
(define (printf fmt . obj)
   (xprintf 'printf (current-output-port) fmt obj))

;*---------------------------------------------------------------------*/
;*    fprintf ...                                                      */
;*---------------------------------------------------------------------*/
(define (fprintf port fmt . obj)
   (xprintf 'fprintf port fmt obj))

;*---------------------------------------------------------------------*/
;*    %write/display-2 ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (%write/display-2 obj port disp)
   (let ((write/display-2 (symbol-append disp (string->symbol "-2")))
	 (write/display-symbol (symbol-append disp '-symbol))
	 (write/display-string (symbol-append disp '-string))
	 ($write/display-char (symbol-append '$ disp '-char))
	 (write/display-pair (symbol-append disp '-pair))
	 ($write/display-elong (symbol-append '$ disp '-elong))
	 ($write/display-llong (symbol-append '$ disp '-llong))
	 ($write/display-bignum (symbol-append '$ disp '-bignum))
	 (write/display-ucs2string (symbol-append disp '-ucs2string))
	 (write/display-object (symbol-append 'object- disp))
	 (write/display-date (symbol-append disp '-date))
	 ($write/display-ucs2 (symbol-append '$ disp '-ucs2)))
      `(cond
	  ((string? ,obj) 
	   (,write/display-string ,obj ,port))
	  ((symbol? ,obj)
	   (,write/display-symbol ,obj ,port))
	  ((fixnum? ,obj)
	   ($display-fixnum ,obj ,port))
	  ((char? ,obj)
	   (,$write/display-char ,obj ,port))
	  ((pair? ,obj)
	   (,write/display-pair ,obj ,port))
	  ((null? ,obj)
	   (display-string "()" ,port))
	  ((eq? ,obj #f)
	   (display-string "#f" ,port))
	  ((eq? ,obj #t)
	   (display-string "#t" ,port))
	  ((eq? ,obj #unspecified)
	   (display-string "#unspecified" ,port))
	  ((elong? ,obj)
	   (,$write/display-elong ,obj ,port))
	  ((flonum? ,obj)
	   (display-flonum ,obj ,port))
	  ((keyword? ,obj)
	   (display-keyword ,obj ,port))
	  ((class? ,obj)
	   (write-class ,obj ,port))
	  ((vector? ,obj)
	   (write/display-vector ,obj ,port ,write/display-2))
	  ((llong? ,obj)
	   (,$write/display-llong ,obj ,port))
	  ((ucs2-string? ,obj)
	   (,write/display-ucs2string ,obj ,port))
	  ((struct? ,obj)
	   (write/display-structure ,obj ,port ,write/display-2))
	  ((object? ,obj)
	   (,write/display-object ,obj ,port))
	  ((date? ,obj)
	   (,write/display-date ,obj ,port))
	  ((mutex? ,obj)
	   (write-mutex ,obj ,port))
	  ((condition-variable? ,obj)
	   (write-condition-variable ,obj ,port))
	  ((ucs2? ,obj)
	   (,$write/display-ucs2 ,obj ,port))
	  ((cell? ,obj)
	   (write/display-cell ,obj ,port ,write/display-2))
	  ((eof-object? ,obj)
	   (display-string "#eof-object" ,port))
	  ((eq? ,obj '#!optional)
	   (display-string "#!optional" ,port))
	  ((eq? ,obj '#!rest)
	   (display-string "#!rest" ,port))
	  ((eq? ,obj '#!key)
	   (display-string "#!key" ,port))
	  ((procedure? ,obj)
	   ($write-procedure ,obj ,port))
	  ((output-port? ,obj)
	   (cond
	      ((output-string-port? ,obj)
	       (display-string "#<output_string_port>" ,port))
	      ((output-procedure-port? ,obj)
	       (display-string "#<output_procedure_port>" ,port))
	      (else
	       ($write-output-port ,obj ,port))))
	  ((input-port? ,obj)
	   ($write-input-port ,obj ,port))
	  ((bignum? ,obj)
	   (,$write/display-bignum ,obj ,port))
	  ((homogeneous-vector? ,obj)
	   (write/display-hvector ,obj ,port ,write/display-2))
	  ((tvector? ,obj)
	   (write/display-tvector ,obj ,port ,write/display-2))
	  ((weakptr? ,obj)
	   (write/display-weakptr ,obj ,port ,write/display-2))
	  ((foreign? ,obj)
	   ($write-foreign ,obj ,port))
	  ((process? ,obj)
	   ($write-process ,obj ,port))
	  ((socket? ,obj)
	   ($write-socket ,obj ,port))
	  ((datagram-socket? ,obj)
	   ($write-datagram-socket ,obj ,port))
	  ((regexp? ,obj)
	   ($write-regexp ,obj ,port))
	  ((mmap? ,obj)
	   ($write-mmap ,obj ,port))
	  ((semaphore? ,obj)
	   ($write-semaphore ,obj ,port))
	  ((opaque? ,obj)
	   ($write-opaque ,obj ,port))
	  ((custom? ,obj)
	   ($write-custom ,obj ,port))
	  ((binary-port? ,obj)
	   ($write-binary-port ,obj ,port))
	  ((dynamic-env? ,obj)
	   (cond-expand
	      (bigloo-c ($write-dynamic-env ,obj ,port))
	      (else (display-string "#<dynamic-env>" ,port))))
	  ((int8? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#s8:" ,port))
	       (display (int8->fixnum ,obj) ,port)))
	  ((uint8? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#u8:" ,port))
	       (display (uint8->fixnum ,obj) ,port)))
	  ((int16? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#s16:" ,port))
	       (display (int16->fixnum ,obj) ,port)))
	  ((uint16? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#u16:" ,port))
	       (display (uint16->fixnum ,obj) ,port)))
	  ((int32? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#s32:" ,port))
	       (display (int32->elong ,obj) ,port)))
	  ((uint32? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#u32:" ,port))
	       (display (uint32->llong ,obj) ,port)))
	  ((int64? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#s64:" ,port))
	       (display (int64->llong ,obj) ,port)))
	  ((uint64? ,obj)
	   (begin
	       ,(when (eq? disp 'write) `(display "#u64:" ,port))
	       (let ((v (/u64 ,obj (fixnum->uint64 10))))
		  (when (>u64 v #u64:0) (display (uint64->llong v) ,port)))
	       (display (uint64->fixnum (remainderu64 ,obj (fixnum->uint64 10))) ,port)))
	  ((cnst? obj)
	   ($write-cnst obj port))
	  (else
	   ($write-unknown ,obj ,port)))))

;*---------------------------------------------------------------------*/
;*    display-2 ...                                                    */
;*---------------------------------------------------------------------*/
(define (display-2 obj port::output-port)
   (%write/display-2 obj port display))

;*---------------------------------------------------------------------*/
;*    write-2 ...                                                      */
;*---------------------------------------------------------------------*/
(define (write-2 obj port::output-port)
   (%write/display-2 obj port write))

;*---------------------------------------------------------------------*/
;*    display-symbol ...                                               */
;*---------------------------------------------------------------------*/
(define (display-symbol obj port)
   (display-string (symbol->string! obj) port))

;*---------------------------------------------------------------------*/
;*    write-symbol ...                                                 */
;*---------------------------------------------------------------------*/
(define (write-symbol obj port)
   
   (define (wrt str)
      (display-string "|" port)
      (display-string ($symbol-for-read str) port)
      (display-string "|" port))
   
   (let* ((str (symbol->string! obj))
	  (len (string-length str))
	  (len-1 (-fx len 1)))
      (let loop ((i 0)
		 (a #f))
	 (if (=fx i len)
	     (cond
		(a (display-string str port))
		((or (eq? obj '+) (eq? obj '-)) (display-string str port))
		(else (wrt str)))
	     (let ((c (string-ref str i)))
		(case c
		   ((#\Newline #\Tab #\Return #\` #\' #\"
		       #\# #\\ #\; #\( #\) #\[ #\] #\{ #\} #\,)
		    (wrt str))
		   ((#\:)
		    (cond
		       ((=fx i 0)
			(if (and (>fx len-1 2)
				 (char=? (string-ref str (+fx i 1)) #\:))
			    (loop (+fx i 2) a)
			    (wrt str)))
		       ((=fx i len-1)
			(wrt str))
		       (else
			(loop (+fx i 1) a))))
		   ((#\.)
		    (if (=fx len 1)
			(wrt str)
			(loop (+fx i 1) a)))
		   (else
		    (if (or (char<=? c #\space) (char>=? c #a127))
			(wrt str)
			(loop (+fx i 1)
			   (or a
			       (or (and (not (char-numeric? c))
					(not (char=? c #\e))
					(not (char=? c #\E))
					(not (char=? c #\-))
					(not (char=? c #\+)))
				   (and (=fx i 0)
					(or (char=? c #\e)
					    (char=? c #\E))))))))))))))

;*---------------------------------------------------------------------*/
;*    display-string ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (display-string obj port)
   ($display-string obj port))

;*---------------------------------------------------------------------*/
;*    display-substring ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (display-substring obj start end port)
   (if (and (>=fx end start)
	    ($string-bound-check? end (+fx (string-length obj) 1))
	    (>=fx start 0))
       ($display-substring obj start end port)
       (error 'display-substring
	      (format "Illegal index, start=~a end=~a" start end)
	      obj)))

;*---------------------------------------------------------------------*/
;*    write-string ...                                                 */
;*---------------------------------------------------------------------*/
(define (write-string obj port)
   (if (bigloo-strict-r5rs-strings)
       (cond-expand
	  (bigloo-c
	   (multiple-value-bind (str esc)
	      (string-for-read obj)
	      ($write-string str esc port)))
	  (else
	   ($write-string (string-for-read obj) #t port)))
       ($write-string (string-for-read obj) #f port)))

;*---------------------------------------------------------------------*/
;*    display-keyword ...                                              */
;*---------------------------------------------------------------------*/
(define (display-keyword obj port)
   ($display-char #\: port)
   (display-string (keyword->string! obj) port))

;*---------------------------------------------------------------------*/
;*    display-fixnum ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (display-fixnum obj port)
   ($display-fixnum obj port))

;*---------------------------------------------------------------------*/
;*    display-elong ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (display-elong obj port)
   ($display-elong obj port))

;*---------------------------------------------------------------------*/
;*    display-flonum ...                                               */
;*---------------------------------------------------------------------*/
(define (display-flonum obj port)
   (display-string (real->string obj) port))

;*---------------------------------------------------------------------*/
;*    display-ucs2string ...                                           */
;*---------------------------------------------------------------------*/
(define (display-ucs2string obj port)
   ($display-ucs2string obj port))

;*---------------------------------------------------------------------*/
;*    write-ucs2string ...                                             */
;*---------------------------------------------------------------------*/
(define (write-ucs2string obj port)
   ($write-utf8string (string-for-read (ucs2-string->utf8-string obj))
		      port))

;*---------------------------------------------------------------------*/
;*    display-date ...                                                 */
;*---------------------------------------------------------------------*/
(define (display-date obj port)
   (display-string (date->string obj) port))
   
;*---------------------------------------------------------------------*/
;*    write-date ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-date obj port)
   (display-string "#<date:" port)
   (display-string (date->string obj) port)
   (display-string ">" port))

;*---------------------------------------------------------------------*/
;*    write-mutex ...                                                  */
;*---------------------------------------------------------------------*/
(define (write-mutex obj port)
   (display-string "#<mutex:" port)
   (display (mutex-name obj) port)
   (display ":" port)
   (display ($mutex-backend obj) port)
   (display-string ">" port))

;*---------------------------------------------------------------------*/
;*    write-condition-variable ...                                     */
;*---------------------------------------------------------------------*/
(define (write-condition-variable obj port)
   (display-string "#<condition-variable:" port)
   (display (condition-variable-name obj) port)
   (display-string ">" port))
 
;*---------------------------------------------------------------------*/
;*    write-class ...                                                  */
;*---------------------------------------------------------------------*/
(define (write-class obj port)
   (display-string "#<class:" port)
   (display-symbol (class-name obj) port)
   (display-string ">" port))
   
;*---------------------------------------------------------------------*/
;*    write/display-cell ...                                           */
;*---------------------------------------------------------------------*/
(define (write/display-cell obj port disp)
   (display-string "#<cell:" port)
   (disp (cell-ref obj) port)
   (display-string ">" port))

;*---------------------------------------------------------------------*/
;*    write/display-structure ...                                      */
;*---------------------------------------------------------------------*/
(define (write/display-structure obj port disp)
   ($display-char #\# port)
   ($display-char #\{ port)
   (disp (struct-key obj) port)
   (if (=fx 0 (struct-length obj))
       ($display-char #\} port)
       (let ((len (-fx (struct-length obj) 1)))
	  ($display-char #\space port)
	  (let loop ((i 0))
	     (cond
		((=fx i len)
		 (disp (struct-ref obj i) port)
		 ($display-char #\} port))
		(else
		 (disp (struct-ref obj i) port)
		 ($display-char #\space port)
		 (loop (+fx 1 i))))))))

;*---------------------------------------------------------------------*/
;*    write/display-vector ...                                         */
;*---------------------------------------------------------------------*/
(define (write/display-vector obj port disp)
   ($display-char #\# port)
   (let ((tag (vector-tag obj)))
      (if (>fx tag 0)
	  (begin
	     (if (>=fx tag 100)
		 (disp tag port)
		 (begin
		    ($display-char #\0 port)
		    (if (>=fx tag 10)
			(disp tag port)
			(begin
			   ($display-char #\0 port)
			   (disp tag port))))))))
   ($display-char #\( port)
   (if (=fx 0 (vector-length obj))
       ($display-char #\) port)
       (let ((len (-fx (vector-length obj) 1)))
	  (let loop ((i 0))
	     (cond
		((=fx i len)
		 (disp (vector-ref obj i) port)
		 ($display-char #\) port))
		(else
		 (disp (vector-ref obj i) port)
		 ($display-char #\space port)
		 (loop (+fx 1 i))))))))
 
;*---------------------------------------------------------------------*/
;*    write/display-tvector ...                                        */
;*---------------------------------------------------------------------*/
(define (write/display-tvector tvec port disp)
   (let ((tvector-ref (tvector-ref tvec))
	 (id (tvector-id tvec)))
      ($display-char #\# port)
      (disp id port)
      ($display-char #\( port)
      (if (not tvector-ref)
	  (begin
	     (display-string "...)" port)
	     tvec)
	  (begin
	     (if (=fx 0 (tvector-length tvec))
		 ($display-char #\) port)
		 (let ((len (-fx (tvector-length tvec) 1)))
		    (let loop ((i 0))
		       (cond
			  ((=fx i len)
			   (disp (tvector-ref tvec i) port)
			   ($display-char #\) port))
			  (else
			   (disp (tvector-ref tvec i) port)
			   ($display-char #\space port)
			   (loop (+fx 1 i)))))))))))

;*---------------------------------------------------------------------*/
;*    write/display-hvector ...                                        */
;*---------------------------------------------------------------------*/
(define (write/display-hvector svec port disp)
   (multiple-value-bind (id _ vref _ _)
      (homogeneous-vector-info svec)
      ($display-char #\# port)
      (display-symbol id port)
      ($display-char #\( port)
      (if (=fx 0 ($hvector-length svec))
	  ($display-char #\) port)
	  (let ((len (-fx ($hvector-length svec) 1)))
	     (let loop ((i 0))
		(cond
		   ((=fx i len)
		    (disp (vref svec i) port)
		    ($display-char #\) port))
		   (else
		    (disp (vref svec i) port)
		    ($display-char #\space port)
		    (loop (+fx 1 i)))))))))

;*---------------------------------------------------------------------*/
;*    write/display-weakptr ...                                        */
;*---------------------------------------------------------------------*/
(define (write/display-weakptr ptr port disp)
   (let ((data (weakptr-data ptr)))
      ($display-string "#<weakptr:" port)
      (disp data port)
      ($display-char #\> port)))

;*---------------------------------------------------------------------*/
;*    %write/display-pair ...                                          */
;*---------------------------------------------------------------------*/
(define-macro (%write/display-pair obj port disp)
   `(begin
       ($display-char #\( ,port)
       (let loop ((l ,obj))
	  (cond
	     ((null? (cdr l))
	      (,disp (car l) ,port)
	      ($display-char #\) ,port))
	     ((not (pair? (cdr l)))
	      (,disp (car l) ,port)
	      ($display-char #\space ,port)
	      ($display-char #\. ,port)
	      ($display-char #\space ,port)
	      (,disp (cdr l) ,port)
	      ($display-char #\) ,port))
	     (else
	      (,disp (car l) ,port)
	      ($display-char #\space ,port)
	      (loop (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    display-pair ...                                                 */
;*---------------------------------------------------------------------*/
(define (display-pair obj port)
   (%write/display-pair obj port display-2))

;*---------------------------------------------------------------------*/
;*    write-pair ...                                                   */
;*---------------------------------------------------------------------*/
(define (write-pair obj port)
   (%write/display-pair obj port write-2))

