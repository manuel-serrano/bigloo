;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/input.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Aug  4 15:42:25 1992                          */
;*    Last change :  Sun Aug 25 09:19:22 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.10.2 Input (page 30, r4)                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __r4_input_6_10_2
   
   (import  __error
	    __r4_ports_6_10_1
	    __param
	    __gunzip)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bexit
	    __object
	    __thread
	    __rgc
	    __bignum
	    __bit
	    
	    __r4_output_6_10_3
	    __r4_equivalence_6_2
	    __r4_vectors_6_8
	    __r4_booleans_6_1
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_strings_6_7
	    __r4_control_features_6_9
	    __r4_characters_6_6
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    
	    __evenv)
   
   (extern  (macro c-eof-object?::bool (::obj) "EOF_OBJECTP")
	    (c-char-ready?::bool (::input-port) "bgl_rgc_charready")
	    ($file->string::bstring (::string) "bgl_file_to_string")
	    ($sendchars::obj (::input-port ::output-port ::long ::long) "bgl_sendchars")
	    ($sendfile::obj (::bstring ::output-port ::long ::long) "bgl_sendfile")
	    (macro beof::obj "BEOF")
	    ($password::bstring (::string) "bgl_password"))
   
   (java    (class foreign
	       (method static c-eof-object?::bool (::obj) "EOF_OBJECTP")
	       (method static c-char-ready?::bool (::input-port) "bgl_rgc_charready")
	       (field static beof::obj "BEOF")
	       (method static $password::bstring (::bstring) "bgl_password")))
   
   (export  (read/rp ::procedure ::input-port . obj)
	    (read/lalrp ::procedure ::procedure ::input-port . obj)
	    
	    (read-char #!optional (ip (current-input-port)))
	    (peek-char #!optional (ip (current-input-port)))
	    (read-byte #!optional (ip (current-input-port)))
	    (peek-byte #!optional (ip (current-input-port)))
	    (inline eof-object::obj)
	    (inline eof-object?::bool ::obj)
	    (inline char-ready?::bool #!optional (ip (current-input-port)))
	    (read-line::obj #!optional (ip (current-input-port)))
	    (read-line-newline::obj #!optional (ip (current-input-port)))
	    (read-lines::pair-nil #!optional (ip (current-input-port)))
	    (read-string::bstring #!optional (ip (current-input-port)))
	    (read-of-strings::obj #!optional (ip (current-input-port)))
	    (read-chars::obj ::obj #!optional (ip (current-input-port)))
	    (read-chars!::obj ::bstring ::obj #!optional (ip (current-input-port)))
	    (inline read-fill-string!::obj ::bstring ::long ::long #!optional (ip (current-input-port)))
	    (unread-char! ::char #!optional (ip (current-input-port)))
	    (unread-string! ::bstring #!optional (ip (current-input-port)))
	    (unread-substring! ::bstring ::long ::long #!optional (ip (current-input-port)))
	    (port->string-list::pair-nil ::input-port)
	    (file->string::bstring ::bstring)
	    (send-chars::long ::input-port ::output-port
			      #!optional (size -1) (offset -1))
	    (send-chars/size::long ::input-port ::output-port ::elong ::elong)
	    (send-file::long file::bstring op::output-port
			     #!optional
			     (size::elong #e-1)
			     (offset::elong #e-1))
	    (file-lines ::bstring)
	    (file-position->line ::int ::obj)
	    
	    (inline password::bstring #!optional (prompt::bstring "")))
   
   (pragma  (eof-object? side-effect-free nesting args-safe)
	    (char-ready? side-effect-free args-safe)))

;*---------------------------------------------------------------------*/
;*    read/rp ...                                                      */
;*    -------------------------------------------------------------    */
;*    In optimized mode, read/rp is overriden by a compiler macro.     */
;*    In consequence prototype changes must be propagate to the        */
;*    compiler (Expand/initial.scm).                                   */
;*---------------------------------------------------------------------*/
(define (read/rp grammar port . opts)
   (cond
      ((pair? opts)
       (apply grammar port opts))
      ((correct-arity? grammar 1)
       (grammar port))
      ((correct-arity? grammar 2)
       (grammar port #unspecified))
      (else
       (error 'read/rp "Grammar arity mismatch" grammar))))

;*---------------------------------------------------------------------*/
;*    read/lalr ...                                                    */
;*---------------------------------------------------------------------*/
(define (read/lalrp lalr rgc port . eof-fun?)
   (if (null? eof-fun?)
       (lalr rgc port eof-object?)
       (lalr rgc port (car eof-fun?))))

;*---------------------------------------------------------------------*/
;*    read-char ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-char #!optional (ip (current-input-port)))
   (let ((grammar (regular-grammar ()
		     ((in all #\Newline) (the-character)))))
      (read/rp grammar ip)))

;*---------------------------------------------------------------------*/
;*    peek-char ...                                                    */
;*---------------------------------------------------------------------*/
(define (peek-char #!optional (ip (current-input-port)))
   (let ((grammar (regular-grammar ()
		     ((in all #\Newline)
		      (let ((c (the-character)))
			 (rgc-buffer-unget-char (the-port) (char->integer c))
			 c)))))
      (read/rp grammar ip)))

;*---------------------------------------------------------------------*/
;*    read-byte ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-byte #!optional (ip (current-input-port)))
   (let ((grammar (regular-grammar ()
		     ((in all #\Newline) (the-byte)))))
      (read/rp grammar ip)))

;*---------------------------------------------------------------------*/
;*    peek-byte ...                                                    */
;*---------------------------------------------------------------------*/
(define (peek-byte #!optional (ip (current-input-port)))
   (let ((grammar (regular-grammar ()
		     ((in all #\Newline)
		      (let ((c (the-byte)))
			 (rgc-buffer-unget-char (the-port) c)
			 c)))))
      (read/rp grammar ip)))

;*---------------------------------------------------------------------*/
;*    eof-object ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (eof-object)
   beof)

;*---------------------------------------------------------------------*/
;*    eof-object? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (eof-object? object)
   (c-eof-object? object))

;*---------------------------------------------------------------------*/
;*    char-ready? ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (char-ready? #!optional (ip (current-input-port)))
   (c-char-ready? ip))

;*---------------------------------------------------------------------*/
;*    read-line ...                                                    */
;*---------------------------------------------------------------------*/
(define (read-line #!optional (ip (current-input-port)))
   (if (>fx (c-input-port-bufsiz ip) 2)
       (let ((grammar (regular-grammar ((xall (or (out #\Newline #\Return)
						  #a000)))
			 ((: (+ xall) (or #\Newline #\Return))
			  (the-substring 0 (-fx (the-length) 1)))
			 ((: (+ xall) #\Return #\Newline)
			  (the-substring 0 (-fx (the-length) 2)))
			 ((+ xall)
			  (the-string))
			 ((or #\Newline #\Return (: #\Return #\Newline))
			  "")
			 (else
			  (the-failure)))))
	  (read/rp grammar ip))
       ;; IOs are unbufferized, uses read-char to get the
       ;; characters one by one
       (let loop ((c (read-char ip))
		  (w 0)
		  (m 100)
		  (acc (make-string 100)))
	  (cond
	     ((eof-object? c)
	      ;; shrink the buffer and return 
	      (if (=fx w 0) c (substring acc 0 w)))
	     ((=fx w m)
	      ;; enlarge the buffer
	      (loop c
		    w
		    (*fx m 2)
		    (let ((new-acc (make-string (*fx m 2))))
		       (blit-string! acc 0 new-acc 0 m)
		       new-acc)))
	     ((char=? c #\Return)
	      (let ((c2 (read-char ip)))
		 (if (char=? c2 #\Newline)
		     (substring acc 0 w)
		     (begin
			(string-set! acc w c)
			(loop c2 (+fx w 1) m acc)))))
	     ((char=? c #\Newline)
	      ;; shrink the buffer and return 
	      (substring acc 0 w))
	     (else
	      ;; fill the buffer
	      (string-set! acc w c)
	      (loop (read-char ip) (+fx w 1) m acc))))))

;*---------------------------------------------------------------------*/
;*    read-line-newline ...                                            */
;*---------------------------------------------------------------------*/
(define (read-line-newline #!optional (ip (current-input-port)))
   (if (>fx (c-input-port-bufsiz ip) 2)
       (let ((grammar (regular-grammar ((xall (or (out #\Newline #\Return)
						  #a000)))
			 ((or (: (+ xall) (or #\Newline #\Return))
			      (: (+ xall) #\Return #\Newline)
			      (+ xall)
			      (or #\Newline #\Return (: #\Return #\Newline)))
			  (the-string))
			 (else
			  (the-failure)))))
	  (read/rp grammar ip))
       ;; std IOs are unbufferized, uses read-char to get the
       ;; characters one by one
       (let loop ((c (read-char ip))
		  (w 0)
		  (m 100)
		  (acc (make-string 100)))
	  (cond
	     ((eof-object? c)
	      ;; shrink the buffer and return 
	      (if (=fx w 0) c (substring acc 0 w)))
	     ((=fx w (- m 2))
	      ;; enlarge the buffer
	      (loop c
		    w
		    (*fx m 2)
		    (let ((new-acc (make-string (*fx m 2))))
		       (blit-string! acc 0 new-acc 0 m)
		       new-acc)))
	     ((char=? c #\Return)
	      (let ((c2 (read-char ip)))
		 (if (char=? c2 #\Newline)
		     (begin
			(string-set! acc w #\Return)
			(string-set! acc (+fx 1 w) #\Newline)
			(substring acc 0 (+fx w 2)))
		     (begin
			(string-set! acc w c)
			(loop c2 (+fx w 1) m acc)))))
	     ((char=? c #\Newline)
	      ;; shrink the buffer and return
	      (string-set! acc w #\Newline)
	      (substring acc 0 (+fx w 1)))
	     (else
	      ;; fill the buffer
	      (string-set! acc w c)
	      (loop (read-char ip) (+fx w 1) m acc))))))

;*---------------------------------------------------------------------*/
;*    read-lines ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-lines #!optional (ip (current-input-port)))
   (let loop ((l (read-line ip))
	      (ls '()))
      (if (eof-object? l)
	  (reverse! ls)
	  (loop (read-line ip) (cons l ls)))))

;*---------------------------------------------------------------------*/
;*    read-string ...                                                  */
;*---------------------------------------------------------------------*/
(define (read-string #!optional (ip (current-input-port)))
   (read/rp (regular-grammar ()
	       ((+ (or all #\Newline)) (the-string))
	       (else ""))
	    ip))
   
;*---------------------------------------------------------------------*/
;*    read-of-strings ...                                              */
;*---------------------------------------------------------------------*/
(define *read-of-strings-grammar*
   (regular-grammar ()
      ((+ (in #\space #\tab #\newline))
       (ignore))
      (#\"
       (the-string))
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (the-substring 1 -1))
      ((+ (out #\space #\tab #\newline #\"))
       (the-string))))

;*---------------------------------------------------------------------*/
;*    read-of-strings ...                                              */
;*---------------------------------------------------------------------*/
(define (read-of-strings #!optional (ip (current-input-port)))
   (read/rp *read-of-strings-grammar* ip))

;*---------------------------------------------------------------------*/
;*    read-chars ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-chars l #!optional (ip (current-input-port)))
   (let ((len (cond
		 ((fixnum? l) l)
		 ((elong? l) (elong->fixnum l))
		 ((llong? l) (llong->fixnum l))
		 (else (bigloo-type-error
			'read-chars "integer" (find-runtime-type l))))))
      (cond
	 ((<=fx len 0)
	  (if (=fx len 0)
	      ""
	      (raise
	       (instantiate::&io-error
		  (proc 'read-chars)
		  (msg "Illegal negative length")
		  (obj len)))))
	 (else
	  (let* ((s ($make-string/wo-fill len))
		 (n ($rgc-blit-string! ip s 0 len)))
	     (cond
		((=fx n 0)
		 (if (rgc-buffer-eof? ip)
		     beof
		     ""))
		((<fx n len)
		 (string-shrink! s n))
		(else
		 s)))))))

;*---------------------------------------------------------------------*/
;*    read-chars! ...                                                  */
;*---------------------------------------------------------------------*/
(define (read-chars! buf l #!optional (ip (current-input-port)))
   (let ((len (cond
		 ((fixnum? l) l)
		 ((elong? l) (elong->fixnum l))
		 ((llong? l) (llong->fixnum l))
		 (else (bigloo-type-error
			'read-chars! "integer" (find-runtime-type l))))))
      (if (<=fx len 0)
	  (if (=fx len 0)
	      0
	      (raise
		 (instantiate::&io-error
		    (proc 'read-chars)
		    (msg "Illegal negative length")
		    (obj len))))
	  ($rgc-blit-string! ip buf 0 (minfx (string-length buf) len)))))

;*---------------------------------------------------------------------*/
;*    read-fill-string! ...                                            */
;*---------------------------------------------------------------------*/
(define-inline (read-fill-string! s o len::long #!optional (ip (current-input-port)))
   (cond-expand
      (bigloo-unsafe-range
       ;;; disable range all array bound checking
       (let ((n ($rgc-blit-string! ip s o len)))
	  (if (and (=fx n 0) ($rgc-buffer-eof? ip)) beof n)))
      (else
       ;; default, full testing
       (if (<=fx len 0)
	   (if (=fx len 0)
	       0
	       (raise
		  (instantiate::&io-error
		     (proc 'read-chars)
		     (msg "Illegal negative length")
		     (obj len))))
	   (let ((n ($rgc-blit-string! ip s o
		       (minfx len (-fx (string-length s) o)))))
	      (if (and (=fx n 0) ($rgc-buffer-eof? ip)) beof n))))))

;*---------------------------------------------------------------------*/
;*    unread-char! ...                                                 */
;*---------------------------------------------------------------------*/
(define (unread-char! c::char #!optional (ip (current-input-port)))
   (unless (rgc-buffer-insert-char! ip (char->integer c))
      (raise
	 (instantiate::&io-error
	    (proc 'unread-char!)
	    (msg "Unread char failed")
	    (obj c)))))

;*---------------------------------------------------------------------*/
;*    unread-string! ...                                               */
;*---------------------------------------------------------------------*/
(define (unread-string! str::bstring #!optional (ip (current-input-port)))
   (when (not (rgc-buffer-insert-substring! ip str 0 (string-length str)))
      (raise
       (instantiate::&io-error
	  (proc 'unread-string!)
	  (msg "Unread string failed")
	  (obj str)))))

;*---------------------------------------------------------------------*/
;*    unread-substring! ...                                            */
;*---------------------------------------------------------------------*/
(define (unread-substring! str::bstring from to
			   #!optional (ip (current-input-port)))
   (when (or (not (>=fx to from))
	     (<fx from 0)
	     (>fx to (string-length str)))
      (raise (instantiate::&io-error
		(proc 'unread-substring!)
		(msg "Invalid positional parameters")
		(obj (list from to (string-length str))))))
   (when (not (rgc-buffer-insert-substring! ip str from to))
      (raise (instantiate::&io-error
		(proc 'unread-sustring!)
		(msg "Unread string failed")
		(obj str)))))

;*---------------------------------------------------------------------*/
;*    port->string-list ...                                            */
;*---------------------------------------------------------------------*/
(define (port->string-list ip)
   (let loop ((res '()))
      (let ((exp (read-of-strings ip)))
	 (if (eof-object? exp)
	     (reverse! res)
	     (loop (cons exp res))))))

;*---------------------------------------------------------------------*/
;*    %sendchars ...                                                   */
;*    -------------------------------------------------------------    */
;*    Internal Scheme implementation of sendchars for back-ends        */
;*    (e.g., JVM and .NET) that don't support the sendfile             */
;*    system call.                                                     */
;*---------------------------------------------------------------------*/
(define (%sendchars::int ip::input-port op::output-port sz::long offset::long)
   (when (>=fx offset 0) (set-input-port-position! ip offset))
   (let* ((bufsize (cond
		      ((=fx sz -1)
		       (c-input-port-bufsiz ip))
		      ((<fx c-default-io-bufsiz sz)
		       c-default-io-bufsiz)
		      (else
		       sz)))
	  (buf (make-string bufsize))
	  (chars-to-read bufsize)
	  (chars-read 0))
      (if (<fx sz 0)
	  (let loop ((chars-read 0))
	     (let ((n (read-chars! buf bufsize ip)))
		(if (=fx n 0)
		    (begin
		       (flush-output-port op)
		       chars-read)
		    (let ((s (if (<fx n bufsize) (substring buf 0 n) buf)))
		       (display s op)
		       (loop (+fx chars-read n))))))
	  (let loop ((chars-read 0)
		     (chars-to-read bufsize)
		     (sz sz))
	     (if (=fx chars-to-read 0)
		 chars-read
		 (let ((n (read-chars! buf chars-to-read ip)))
		    (if (=fx n 0)
			(begin
			   (flush-output-port op)
			   chars-read)
			(let ((s (if (<fx n bufsize) (substring buf 0 n) buf)))
			   (display s op)
			   (let* ((sz (-fx sz n))
				  (ctr (if (<fx sz bufsize) sz bufsize)))
			      (loop (+fx chars-read n) ctr sz))))))))))

;*---------------------------------------------------------------------*/
;*    file->string ...                                                 */
;*---------------------------------------------------------------------*/
(define (file->string path)
   (cond-expand
      (bigloo-c
       (let ((i (string-index path #\:)))
	  (cond
	     ((not i)
	      ($file->string path))
	     ((string-prefix? "file:" path)
	      ($file->string (substring path 5 (string-length path))))
	     (else
	      (let ((ip (open-input-file path)))
		 (unwind-protect
		    (read-string ip)
		    (close-input-port ip)))))))
      (else
       (let ((ip (open-input-file path)))
	  (unwind-protect
	     (read-string ip)
	     (close-input-port ip))))))

;*---------------------------------------------------------------------*/
;*    send-chars/size ...                                              */
;*---------------------------------------------------------------------*/
(define (send-chars/size ip::input-port op::output-port
			 size::elong offset::elong)
   (let ((sz::long (elong->fixnum size))
	 (off::long (elong->fixnum offset)))
      (cond-expand
	 (bigloo-c
	  (or ($sendchars ip op sz off)
	      (cond
		 ((and (input-gzip-port? ip) (=fx sz -1) (=fx off -1))
		  (gunzip-sendchars ip op))
		 (else
		  (%sendchars ip op sz off)))))
	 (else
	  (cond
	     ((and (input-gzip-port? ip) (=fx sz -1) (=fx off -1))
	      (gunzip-sendchars ip op))
	     (else
	      (%sendchars ip op sz off)))))))

;*---------------------------------------------------------------------*/
;*    send-chars ...                                                   */
;*---------------------------------------------------------------------*/
(define (send-chars::long ip::input-port op::output-port
			  #!optional (size -1) (offset -1))
   (let ((sz::elong (cond
		       ((fixnum? size) (fixnum->elong size))
		       ((elong? size) size)
		       (else (error 'send-chars "Illegal size" size))))
	 (off::elong (cond
			((fixnum? offset) (fixnum->elong offset))
			((elong? offset) offset)
			(else (error 'send-chars "Illegal offset" offset)))))
      (send-chars/size ip op sz off)))

;*---------------------------------------------------------------------*/
;*    send-file ...                                                    */
;*---------------------------------------------------------------------*/
(define (send-file::long file::bstring op::output-port
			      #!optional
			      (size::elong #e-1)
			      (offset::elong #e-1))
   (let ((sz (elong->fixnum size))
	 (off (elong->fixnum offset)))
      (cond-expand
	 (bigloo-c
	  (or ($sendfile file op size offset)
	      (let ((ip (open-input-file file)))
		 (unwind-protect
		    (send-chars/size ip op size offset)
		    (close-input-port ip)))))
	 (else
	  (let ((ip (open-input-file file)))
	     (unwind-protect
		(send-chars/size ip op size offset)
		(close-input-port ip)))))))

;*---------------------------------------------------------------------*/
;*    file-lines ...                                                   */
;*    -------------------------------------------------------------    */
;*    Returns a list of lines start/stop positions.                    */
;*---------------------------------------------------------------------*/
(define (file-lines file)
   
   (define gram
      (regular-grammar (start acc)
	 (#\Newline
	  (let* ((stop (input-port-position (the-port)))
		 (desc (cons start stop)))
	     (set! start (+fx 1 stop))
	     (set! acc (cons desc acc))
	     (ignore)))
	 ((+ all)
	  (ignore))
	 (else
	  (let ((c (the-failure)))
	     (if (eof-object? c)
		 (let ((stop (input-port-position (the-port))))
		    (if (>fx stop start)
			(reverse! (cons (cons start stop) acc))
			(reverse! acc)))
		 (error 'file-lines "Illegal files" file))))))
   
   (if (not (file-exists? file))
       #f
       (with-input-from-file file
	  (lambda ()
	     (read/rp gram (current-input-port) 0 '())))))

;*---------------------------------------------------------------------*/
;*    file-position->line ...                                          */
;*---------------------------------------------------------------------*/
(define (file-position->line pos fdesc)
   (cond
      ((pair? fdesc)
       (let loop ((flines fdesc)
		  (line 1))
	  (cond
	     ((null? flines)
	      #f)
	     ((>=fx pos (cdar flines))
	      (loop (cdr flines) (+fx line 1)))
	     (else
	      line))))
      ((string? fdesc)
       (let ((gram (regular-grammar (pos line)
		      (#\Newline
		       (if (>=fx (input-port-position (the-port)) pos)
			   line
			   (begin
			      (set! line (+fx line 1))
			      (ignore))))
		      ((+ all)
		       (ignore))
		      (else
		       #f))))
	  (if (not (file-exists? fdesc))
	      #f
	      (with-input-from-file fdesc
		 (lambda ()
		    (read/rp gram (current-input-port) pos 1))))))
      (else
       #f)))
       
;*---------------------------------------------------------------------*/
;*    password ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (password #!optional (prompt::bstring ""))
   ($password prompt))
