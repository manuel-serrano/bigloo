;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Read/reader.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 27 11:16:00 1994                          */
;*    Last change :  Mon Jan 14 13:58:26 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Bigloo's reader                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __reader
   
   (import  __error
	    __rgc
	    __param
	    __object
	    __thread)
   
   (use     __type
	    __bigloo
	    __param
	    __structure
	    __tvector
	    __dsssl
	    __ucs2
	    __unicode
	    __bexit
	    __binary
	    __srfi4
	    __bignum
	    __bit

	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_numbers_6_5
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    __r4_vectors_6_8
	    __r4_ports_6_10_1
	    __r4_input_6_10_2
	    __r4_output_6_10_3
	    
	    __evenv)
   
   (extern (macro unspec::obj "BUNSPEC")
	   (macro boptional::obj "BOPTIONAL")
	   (macro brest::obj "BREST")
	   (macro bkey::obj "BKEY")
	   (macro make-cnst::obj (::long) "BCNST")
	   (macro $nan::double "BGL_NAN")
	   (macro $infinity::double "BGL_INFINITY"))
   
   (java   (class foreign
	      (field static unspec::obj "BUNSPEC")
	      (field static boptional::obj "BOPTIONAL")
	      (field static brest::obj "BREST")
	      (field static bkey::obj "BKEY")
	      (method static make-cnst::obj (::long) "BCNST")
	      (field static $nan::double "BGL_NAN")
	      (field static $infinity::double "BGL_INFINITY")))

   (export  *bigloo-interpreter*
	    (bigloo-case-sensitivity::symbol)
	    (bigloo-case-sensitivity-set! ::symbol)
	    (bigloo-identifier-syntax::symbol)
	    (bigloo-identifier-syntax-set! ::symbol)
	    (make-source-location name pos)
	    (get-source-location obj)
	    (read #!optional (iport::input-port (current-input-port)) location)
	    (bigloo-regular-grammar)
	    (read/case case . port)
	    (read-case-sensitive . port)
	    (read-case-insensitive . port)
	    (reader-reset!)
	    (port->list::pair-nil ::procedure ::input-port)
	    (port->sexp-list::pair-nil ::input-port #!optional location)
	    (set-read-syntax!::unspecified ::bchar ::procedure)
	    (define-reader-ctor::unspecified ::symbol ::procedure)))

;*---------------------------------------------------------------------*/
;*    *identifier-syntax*                                              */
;*    -------------------------------------------------------------    */
;*    When 'bigloo, identidiers such as x.f are treated as             */
;*    field "f" of instance "x". Otherwise treated as identifier       */
;*    "x.f".                                                           */
;*---------------------------------------------------------------------*/
(define *identifier-syntax* 'r5rs)

(define (bigloo-identifier-syntax) *identifier-syntax*)
(define (bigloo-identifier-syntax-set! v) (set! *identifier-syntax* v))

;*---------------------------------------------------------------------*/
;*    bigloo-case-sensitivity ...                                      */
;*---------------------------------------------------------------------*/
(define (bigloo-case-sensitivity)
   (bigloo-case-sensitive))

;*---------------------------------------------------------------------*/
;*    bigloo-case-sensitivity-set! ...                                 */
;*---------------------------------------------------------------------*/
(define (bigloo-case-sensitivity-set! val)
   (bigloo-case-sensitive-set! val))

;*---------------------------------------------------------------------*/
;*    make-source-location ...                                         */
;*---------------------------------------------------------------------*/
(define (make-source-location name pos)
   (list 'at name pos))

;*---------------------------------------------------------------------*/
;*    get-source-location ...                                          */
;*---------------------------------------------------------------------*/
(define (get-source-location obj)
   (when (epair? obj)
      (match-case (cer obj)
	 ((at ?- ?-)
	  (cer obj)))))

;*---------------------------------------------------------------------*/
;*    cons/loc ...                                                     */
;*---------------------------------------------------------------------*/
(define (cons/loc a d name pos)
   (econs a d (make-source-location name pos)))

;*---------------------------------------------------------------------*/
;*    Control marks ...                                                */
;*---------------------------------------------------------------------*/
(define *end-of-list* (cons 0 0))
(define *dotted-mark* (cons 1 1))

;*---------------------------------------------------------------------*/
;*    *bigloo-interpreter* ...                                         */
;*---------------------------------------------------------------------*/
(define *bigloo-interpreter* #f)

;*---------------------------------------------------------------------*/
;*    reader-reset! ...                                                */
;*---------------------------------------------------------------------*/
(define (reader-reset!)
   #f)

;*---------------------------------------------------------------------*/
;*    read-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-error msg obj port)
   (let (fname loc)
      (if (epair? obj)
	  (match-case (cer obj)
	     ((at ?fn ?pos)
	      (set! fname fn)
	      (set! loc pos))
	     (else
	      (set! fname (input-port-name port))
	      (set! loc (input-port-position port))))
	  (begin
	     (set! fname (input-port-name port))
	     (set! loc (input-port-position port))))
      (raise (instantiate::&io-read-error
		(proc 'read)
		(msg msg)
		(obj obj)
		(fname fname)
		(location loc)))))

;*---------------------------------------------------------------------*/
;*    read-error/loc ...                                               */
;*---------------------------------------------------------------------*/
(define (read-error/loc loc msg obj port)
   (let ((fname (if (epair? obj)
		    (match-case (cer obj)
		       ((at ?fname ?pos)
			fname)
		       (else
			(input-port-name port)))
		    (input-port-name port))))
      (raise (instantiate::&io-read-error
		(proc 'read)
		(msg msg)
		(obj obj)
		(fname fname)
		(location loc)))))

;*---------------------------------------------------------------------*/
;*    unreference! ...                                                 */
;*---------------------------------------------------------------------*/
(define (unreference! obj port cycles)
   (let loop ((obj obj))
      (cond
	 ((procedure? obj)
	  (let* ((no (obj))
                 (val0 (vector-ref cycles no))
                 (val (if (not val0)
                          (read-error "no target for graph reference" no port)
                          val0)))
            (if (eq? val obj)
                (read-error "Illegal cyclic reference" no port)
                val)))
	 ((pair? obj)
	  (set-car! obj (loop (car obj)))
	  (set-cdr! obj (loop (cdr obj)))
	  obj)
	 ((vector? obj)
	  (let ((len (vector-length obj)))
	     (let laap ((i 0))
		(if (<fx i len)
		    (begin
		       (vector-set! obj i (loop (vector-ref obj i)))
		       (laap (+fx i 1)))
		    obj))))
	 ((struct? obj)
	  (let ((len (struct-length obj)))
	     (let laap ((i 0))
		(if (<fx i len)
		    (begin
		       (struct-set! obj i (loop (struct-ref obj i)))
		       (laap (+fx i 1)))
		    obj))))
	 (else
	  obj))))
   
;*---------------------------------------------------------------------*/
;*    make-list! ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-list! l port)
   (define (reverse-proper-list! l)
      (let nr ((l l)
	       (r '()))
	 (cond
	    ((eq? (car l) *dotted-mark*)
	     (read-error "Illegal pair" r port))
	    ((null? (cdr l))
	     (set-cdr! l r)
	     l)
	    (else
	     (let ((cdrl (cdr l)))
		(nr cdrl
		    (begin (set-cdr! l r)
			   l)))))))
   (define (reverse-improper-list! l)
      (let nr ((l (cddr l))
	       (r (car l)))
	 (cond
	    ((eq? (car l) *dotted-mark*)
	     (read-error "Illegal pair" r port))
	    ((null? (cdr l))
	     (set-cdr! l r)
	     l)
	    (else
	     (let ((cdrl (cdr l)))
		(nr cdrl
		    (begin (set-cdr! l r)
			   l)))))))
   (cond
      ((null? l)
       l)
      ((and (pair? l) (pair? (cdr l)) (eq? (cadr l) *dotted-mark*))
       (if (null? (cddr l))
	   (car l)
	   (reverse-improper-list! l)))
      (else
       (reverse-proper-list! l)))) 
	   
;*---------------------------------------------------------------------*/
;*    collect-up-to ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (collect-up-to ignore kind port posp)
   ;; move one character backward for the open-parenthesis
   (let* ((name (input-port-name port))
	  (po (-fx (input-port-position port) 1))
	  (item (ignore)))
      (if (eq? item *end-of-list*)
	  '()
	  (let loop ((acc (if posp
			      (cons/loc item '() name po)
			      (cons item '()))))
	     (let ((item (ignore)))
		(if (eq? item *end-of-list*)
		    acc
		    (loop (if posp
			      (let ((po (input-port-last-token-position port)))
				 (cons/loc item acc name po))
			      (cons item acc)))))))))

;*---------------------------------------------------------------------*/
;*    read-quote ...                                                   */
;*---------------------------------------------------------------------*/
(define (read-quote kwote port ignore posp)
   (if posp
       (let* ((pos (input-port-position port))
	      (obj (ignore)))
	  (if (or (eof-object? obj) (eq? obj *end-of-list*))
	      (read-error/loc pos "Illegal quotation" kwote port)
	      (cons/loc kwote (cons obj '()) (input-port-name port) pos)))
       (let ((obj (ignore)))
	  (if (or (eof-object? obj) (eq? obj *end-of-list*))
	      (read-error "Illegal quotation" kwote port)
	      (cons kwote (cons obj '()))))))

;*---------------------------------------------------------------------*/
;*    read-multi-line-comment ...                                      */
;*---------------------------------------------------------------------*/
(define (read-multi-line-comment port)
   (let ((g (regular-grammar ()
	       ("#|"
		(read-multi-line-comment (the-port))
		(ignore))
	       ((+ (or (out #\# #\|) (: #\# (out #\|)) (: #\| (out #\#))))
		(ignore))
	       ("|#"
		#unspecified)
	       (else
		(let ((c (the-failure)))
		   (if (eof-object? c)
		       (read-error "EOF inside block comment -- #| missing a closing |#"
				   c
				   (the-port))))))))
      (read/rp g port)))

;*---------------------------------------------------------------------*/
;*    *sharp-grammar* ...                                              */
;*---------------------------------------------------------------------*/
(define *sharp-grammar*
   (regular-grammar ()
      
      ((: "a" (= 3 digit))
       (let ((string (the-string)))
	  (integer->char (string->integer (the-substring 1 4)))))
      
      ;; ucs-2 characters
      ((: "u" (= 4 xdigit))
       (integer->ucs2 (string->integer (the-substring 1 5) 16)))
      
      ;; foreign strings of char
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (the-escape-substring 1 (-fx (the-length) 1) #f))
      
      ;; ucs2 strings
      ((: "u\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (let ((str (the-escape-substring 2 (-fx (the-length) 1) #f)))
  	  (utf8-string->ucs2-string str)))
      ;; fixnums
      ((: "b" (? (in "-+")) (+ (in ("01"))))
       (string->integer-obj (the-substring 1 (the-length)) 2))
      ((: "o" (? (in "-+")) (+ (in ("07"))))
       (string->integer-obj (the-substring 1 (the-length)) 8))
      ((: "d" (? (in "-+")) (+ (in ("09"))))
       (string->integer-obj (the-substring 1 (the-length)) 10))
      ((: "e" (? (in "-+")) (+ digit))
       (string->elong (the-substring 1 (the-length)) 10))
      ((: "ex" (+ xdigit))
       ($strtoeul (the-substring 2 (the-length)) 0 16))
      ((: "l" (? (in "-+")) (+ digit))
       (string->llong (the-substring 1 (the-length)) 10))
      ((: "lx" (+ xdigit))
       ($strtoull (the-substring 2 (the-length)) 0 16))
      ((: "x" (? (in "-+")) (+ (in (uncase (in ("09af"))))))
       (string->integer-obj (the-substring 1 (the-length)) 16))
      ;; bignums
      ((: "z" (? (in "-+")) (+ (in ("09"))))
       (string->bignum (the-substring 1 (the-length)) 10))
      ((: "zx" (+ xdigit))
       (string->bignum (the-substring 2 (the-length)) 16))
      
      ;; unspecified and eof-object
      ((: (in "ue") (+ (in "nspecified-objt")))
       (let ((symbol (string->symbol (string-upcase! (the-string)))))
	  (cond
	     ((eq? symbol 'UNSPECIFIED)
	      unspec)
	     ((eq? symbol 'EOF-OBJECT)
	      beof)
	     (else
	      (read-error "Illegal identifier"
		 (string-append "#" (symbol->string symbol)) (the-port))))))

      ;; stdint
      ((: "s8:" (? #\-) (+ (in ("09"))))
       (fixnum->int8 (string->integer (the-substring 3 0))))
      ((: "u8:" (+ (in ("09"))))
       (fixnum->uint8 (string->integer (the-substring 3 0))))
      ;; stdint
      ((: "s16:" (? #\-) (+ (in ("09"))))
       (fixnum->int16 (string->integer (the-substring 4 0))))
      ((: "u16:" (+ (in ("09"))))
       (fixnum->uint16 (string->integer (the-substring 4 0))))
      ;; stdint
      ((: "s32:" (? #\-) (+ (in ("09"))))
       (elong->int32 (string->elong (the-substring 4 0))))
      ((: "u32:" (+ (in ("09"))))
       (cond-expand
	  (bint61
	   (elong->uint32 (string->elong (the-substring 4 0))))
	  (else
	   (llong->uint32 (string->llong (the-substring 4 0))))))
      ;; stdint
      ((: "s64:" (? #\-) (+ (in ("09"))))
       (fixnum->int64 (string->llong (the-substring 4 0))))
      ((: "u64:" (+ (in ("09"))))
       (let ((s2 (the-substring 5 0))
	     (s1 (the-substring 4 5)))
	  (+u64 (llong->uint64 (string->llong s2))
	     (let loop ((pow (string-length s2))
			(n1 (fixnum->uint64 (string->integer s1))))
		(if (=fx pow 0)
		    n1
		    (loop (-fx pow 1) (*u64 n1 (fixnum->uint64 10))))))))

      ;; constants
      ((: "<" (= 4 (or digit (uncase (in ("AF"))))) ">")
       (make-cnst (string->integer (the-substring 1 5) 16)))
      
      ;; SRFI-10 reader macros
      ((: ",")
       (let ((f (read (the-port))))
	  (if (pair? f)
	      (let ((p (get-reader-ctor (car f))))
		 (if (procedure? p)
		     (apply p (cdr f))
		     (read-error "Unknown SRFI-10 extension" (car f) (the-port))))
	      (read-error "Bad SRFI-10 form" f (the-port)))))

      (else
       (let ((c (the-failure)))
	  (if (char? c)
	      (read-error "Illegal token" (string #\# c) (the-port))
	      (read-error "Illegal char" c (the-port)))))))

;*---------------------------------------------------------------------*/
;*    *bigloo-grammar* ...                                             */
;*---------------------------------------------------------------------*/
(define *bigloo-grammar*
   (regular-grammar ((float       (or (: (* digit) "." (+ digit))
				      (: (+ digit) "." (* digit))))
		     (letter      (in ("azAZ") (#a128 #a255)))
		     (kspecial    (in "!@~$%^&*></-_+\\=?"))
		     (specialsans (or kspecial (in ":")))
		     (special     (or specialsans "."))
		     (quote       (in "\",'`"))
		     (paren       (in "()[]{}"))
		     (id          (: (* digit)
				     (or letter special)
				     (* (or letter special digit (in ",'`")))))
		     (idsans      (: (* digit)
				     (or letter specialsans)
				     (* (or letter specialsans digit (in ",'`")))))
		     (letterid    (: (or letter special)
				     (* (or letter special digit (in ",'`")))))
		     (kid         (or digit letter kspecial "."))
		     (blank       (in #\Space #\Tab #a011 #a012 #a013))
		     (field       (: idsans (+ (: "." idsans))))
		     
		     posp cycles par-open bra-open par-poses bra-poses)

      (define resolve #t)
      
      ;; newlines
      ((+ #\Newline)
       (ignore))
      
      ;; blank lines
      ((+ blank)
       (ignore))
      
      ;; comments
      ((: ";" (* all))
       (ignore))
      
      ;; multi-line comment (SRFI-30)
      ("#|"
       (read-multi-line-comment (the-port))
       (ignore))
      
      ;; #; expression comments
      ("#;"
       (begin
	  (ignore)
	  (ignore)))
      
      ;; srfi-22 support
      ((bol (: "#!" #\space (or digit letter special (in "|,'`")) (* all)))
       (ignore))
      
      ;; the interpreter header or the dsssl named constants
      ((: "#!" (+ (or digit letter special (in "|,'`"))))
       (let* ((str (the-string)))
	  (cond
	     ((string=? str "#!optional")
	      boptional)
	     ((string=? str "#!rest")
	      brest)
	     ((string=? str "#!key")
	      bkey)
	     (else
	      (set! *bigloo-interpreter* #t)
	      (ignore)))))
      
      ;; characters
      ((: "#\\" (or letter digit special (in "|#; " quote paren)))
       (string-ref (the-string) 2))
      ((: "#\\" (in ("awyzAWYZ")) (>= 2 letter))
       (let ((char-name (string->symbol
			   (string-upcase!
			      (the-substring 2 (the-length))))))
	  (cond
	     ((eq? char-name 'NEWLINE)
	      #\Newline)
	     ((eq? char-name 'TAB)
	      #\tab)
	     ((eq? char-name 'SPACE)
	      #\space)
	     ((eq? char-name 'RETURN)
	      (integer->char 13))
	     ((eq? char-name 'NULL)
	      (integer->char 0))
	     ((eq? char-name 'ALARM)
	      (integer->char 7))
	     ((eq? char-name 'BACKSPACE)
	      (integer->char 8))
	     ((eq? char-name 'DELETE)
	      (integer->char #x7f))
	     ((eq? char-name 'ESCAPE)
	      (integer->char #x1b))
	     (else
	      (read-error "Illegal character" (the-string) (the-port))))))
      ((: "#\\" (>= 3 digit))
       (integer->char (string->integer (the-substring 2 (the-length)) 8)))
      ;; R[67]RS characters notation
      ((: "#\\x" (+ xdigit))
       (let ((n (string->integer (the-substring 3 0) 16)))
	  (if (<=fx n 256)
	      (integer->char n)
	      (integer->ucs2 n))))
      
      ;; strings with newline in them in addition to compute
      ;; the string, we have to count the number of newline
      ;; in order to increment the line-num variable strings
      ((: "\"" (* (or (out #a000 #\\ #\") (: #\\ all))) "\"")
       (the-escape-substring 1 (-fx (the-length) 1) (bigloo-strict-r5rs-strings)))
      
      ;; fixnums
      ((: (? (in "+-")) (+ digit))
       (the-integer))
      
      ;; flonum
      ((: (? (in "-+"))
	  (or float
	      (: (or float (+ digit)) (in "eE") (? (in "+-")) (+ digit))))
       (the-flonum))
      
      ("+nan.0"
       $nan)
      ("+inf.0"
       $infinity)
      ("-inf.0"
       (negfl $infinity))
      
      ;; doted pairs
      ("."
       (if (<=fx par-open 0)
	   (read-error "Illegal token" #\. (the-port))
	   *dotted-mark*))
      
      ;; booleans
      ((: "#" (uncase #\t))
       #t)
      ((: "#" (uncase #\f))
       #f)
      
      ;; keywords
      ((or (: ":" (+ kid) (* (or kid #\:)))
	   (: (? (: kid (* (or kid #\:)))) (+ kid) ":"))
       ;; since the keyword expression is also matched by the id
       ;; rule, keyword rule has to be placed before the id rule.
       (the-keyword))
      
      ;; identifiers
      (field
       (if (eq? *identifier-syntax* 'bigloo)
	   (cons/loc '->
	      (map! string->symbol (string-split (the-string) "."))
	      (input-port-name (the-port))
	      (input-port-position (the-port)))
	   (the-symbol)))
      (id
       ;; this rule has to be placed after the rule matching the `.' char
       (the-symbol))
      ((: "|" (* (or (out #a000 #\\ #\|) (: #\\ all))) "|")
       (if (=fx (the-length) 2)
	   (string->symbol "")
	   (string->symbol (the-escape-substring 1 (-fx (the-length) 1) #f))))
      
      ;; quotations 
      ("'"
       (read-quote 'quote (the-port) ignore posp))
      ("`"
       (read-quote 'quasiquote (the-port) ignore posp))
      (","
       (read-quote 'unquote (the-port) ignore posp))
      (",@"
       (read-quote 'unquote-splicing (the-port) ignore posp))
      
      ;; lists
      ((in "([")
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			  par-poses))
       ;; and then, we compute the result list...
       (make-list! (collect-up-to ignore "list" (the-port) posp) (the-port)))
      ((in ")]")
       ;; we decrement the number of open parenthesis
       (set! par-open (-fx par-open 1))
       (if (<fx par-open 0)
	   (begin
	      (warning/location (input-port-name (the-port))
		 (input-port-last-token-position (the-port))
		 'read
		 "Superfluous closing parenthesis `"
		 (the-string)
		 "'")
	      (set! par-open 0)
	      (ignore))
	   (begin
	      (set! par-poses (cdr par-poses))
	      *end-of-list*)))
      
      ;; vectors
      ("#("
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			  par-poses))
       (list->vector
	  (reverse! (collect-up-to ignore "vector" (the-port) posp))))
      
      ;; typed homogeneous vectors
      ((: "#" letterid "(")
       ;; we increment the number of open parenthesis
       (set! par-open (+fx 1 par-open))
       (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			  par-poses))
       (let ((s (the-substring 1 -1)))
	  (cond
	     ((string=? s "s8")
	      (list->s8vector
		 (reverse! (collect-up-to ignore "s8vector" (the-port) posp))))
	     ((string=? s "u8")
	      (list->u8vector
		 (reverse! (collect-up-to ignore "u8vector" (the-port) posp))))
	     ((string=? s "s16")
	      (list->s16vector
		 (reverse! (collect-up-to ignore "s16vector" (the-port) posp))))
	     ((string=? s "u16")
	      (list->u16vector
		 (reverse! (collect-up-to ignore "u16vector" (the-port) posp))))
	     ((string=? s "s32")
	      (list->s32vector
		 (reverse! (collect-up-to ignore "s32vector" (the-port) posp))))
	     ((string=? s "u32")
	      (list->u32vector
		 (reverse! (collect-up-to ignore "u32vector" (the-port) posp))))
	     ((string=? s "s64")
	      (list->s64vector
		 (reverse! (collect-up-to ignore "s64vector" (the-port) posp))))
	     ((string=? s "u64")
	      (list->u64vector
		 (reverse! (collect-up-to ignore "u64vector" (the-port) posp))))
	     ((string=? s "f32")
	      (list->f32vector
		 (reverse! (collect-up-to ignore "f32vector" (the-port) posp))))
	     ((string=? s "f64")
	      (list->f64vector
		 (reverse! (collect-up-to ignore "f64vector" (the-port) posp))))
	     (else
	      (let* ((id (string->symbol
			    (case (bigloo-case-sensitivity)
			       ((upcase)
				(string-upcase! s))
			       ((downcase)
				(string-downcase! s))
			       ((sensitive)
				s)
			       (else
				(string-upcase! s)))))
		     (l (reverse! (collect-up-to ignore "vector" (the-port) posp))))
		 (list->tvector id l))))))
      
      ;; tagged vectors (Camloo backward compatibility)
      ((: #\# digit digit digit #\()
       (let ((open-key par-open)
	     (tag (string->integer (the-substring 1 4))))
	  (set! par-open (+fx 1 par-open))
	  (set! par-poses (cons (-fx (input-port-position (the-port)) 1)
			     par-poses))
	  (list->vector
	     (reverse!
		(cons tag (collect-up-to ignore "vector" (the-port) posp))))))
      
      ;; structures
      ("#{"
       ;; then, we compute the structure
       ;; we increment the number of open parenthesis
       (set! bra-open (+fx 1 bra-open))
       (set! bra-poses (cons (-fx (input-port-position (the-port)) 1)
			  bra-poses))
       (let* ((l (reverse! (collect-up-to ignore "structure" (the-port) posp)))
	      (len (length l))
	      (r (make-struct (car l) (-fx len 1) #unspecified)))
	  (let loop ((i 0)
		     (l (cdr l)))
	     (when (pair? l)
		(struct-set! r i (car l))
		(loop (+fx i 1) (cdr l))))
	  r))
      ("}"
       (set! bra-open (-fx bra-open 1))
       (if (<fx bra-open 0)
	   (begin
	      (warning/location (input-port-name (the-port))
		 (input-port-last-token-position (the-port))
		 'read
		 "Superfluous closing bracket `"
		 (the-string))
	      (set! bra-open 0)
	      (ignore))
	   (begin
	      (set! bra-poses (cdr bra-poses))
	      *end-of-list*)))
      
      ;; cyclic target mark
      ((: #\# (+ digit) "=")
       (let* ((no (string->integer (the-substring 1 (-fx (the-length) 1))))
	      (pos (input-port-position (the-port)))
	      (rsvp resolve))
	  (set! resolve #f)
	  (let ((the-object (ignore)))
	     (cond
		((eof-object? the-object)
		 (read-error/loc pos "Illegal cyclic reference" no (the-port)))
                ((>=fx no (vector-length cycles))
                 ;; extend vector
		 (let* ((old-length (vector-length cycles))
			(new-length (maxfx (+ no 1) (*fx old-length 2)))
			(new-cycles (make-vector new-length #f)))
		    (vector-copy! new-cycles 0 cycles 0)
		    (set! cycles new-cycles))))
             (when (vector-ref cycles no)
		(read-error "Illegal duplicate declaration"
		   (list no (vector-ref cycles no)) (the-port)))
             (vector-set! cycles no the-object)
	     (set! resolve rsvp)
	     (if rsvp
		 (unreference! the-object (the-port) cycles)
		 the-object))))
      
      ;; cyclic target reference
      ((: #\# (+ digit) "#")
       (let* ((no (string->integer (the-substring 1 (-fx (the-length) 1)))))
          (cond
	     ((not resolve)
	      (lambda () no))
	     (else
	      (let ((val (vector-ref cycles no)))
		 (if (not val)
		     (read-error "no target for graph reference" no (the-port))
		     val))))))

      ;; special tokens
      ("#"
       (read/rp *sharp-grammar* (the-port)))
      
      ;; error or eof
      (else
       (let ((char (the-failure)))
	  (cond
	     ((eof-object? char)
	      (cond
		 ((>fx par-open 0)
		  (read-error/loc (car par-poses)
		     "Unexpected end-of-file"
		     "Unclosed list"
		     (the-port)))
		 ((>fx bra-open 0)
		  (read-error/loc (car par-poses)
		     "Unexpected end-of-file"
		     "Unclosed vector or structure"
		     (the-port)))
		 (else
		  (reset-eof (the-port))
		  char)))
	     ((get-reader-extension char)
	      =>
	      (lambda (e) (e (the-port))))
	     (else
	      (read-error "Illegal char"
		 (illegal-char-rep char)
		 (the-port))))))))

;*---------------------------------------------------------------------*/
;*    bigloo-regular-grammar ...                                       */
;*---------------------------------------------------------------------*/
(define (bigloo-regular-grammar)
   *bigloo-grammar*)

;*---------------------------------------------------------------------*/
;*    read ...                                                         */
;*---------------------------------------------------------------------*/
(define (read #!optional (iport::input-port (current-input-port)) location)
   ;; read except an undocumented argument used by the compiler to
   ;; get line number associated with expressions.
   (if (closed-input-port? iport)
       (error 'read "Illegal closed input port" iport)
       ;; The reader is always compiled in unsafe mode then, the
       ;; expansion of the *BIGLOO-GRAMMAR* never checks if the
       ;; input port is not already closed. In consequence, we
       ;; have to explicitly test the closeness before reading.
       (read/rp (bigloo-regular-grammar) iport location '#() 0 0 '() '())))

;*---------------------------------------------------------------------*/
;*    read/case ...                                                    */
;*---------------------------------------------------------------------*/
(define (read/case case . input-port)
   (let ((old (bigloo-case-sensitivity)))
      (bigloo-case-sensitivity-set! case)
      (unwind-protect
	 (apply read input-port)
	 (bigloo-case-sensitivity-set! old))))
   
;*---------------------------------------------------------------------*/
;*    read-case-sensitive ...                                          */
;*    -------------------------------------------------------------    */
;*    Case sensitive read.                                             */
;*---------------------------------------------------------------------*/
(define (read-case-sensitive . input-port)
   (apply read/case 'sensitive input-port))

;*---------------------------------------------------------------------*/
;*    read-case-insensitive ...                                        */
;*    -------------------------------------------------------------    */
;*    Case unsensitive read.                                           */
;*---------------------------------------------------------------------*/
(define (read-case-insensitive . input-port)
   (apply read/case 'downcase input-port))

;*---------------------------------------------------------------------*/
;*    port->list ...                                                   */
;*---------------------------------------------------------------------*/
(define (port->list reader ip)
   (let loop ((exp '()))
      (let ((e (reader ip)))
	 (if (eof-object? e)
	     (reverse! exp)
	     (loop (cons e exp))))))
   
;*---------------------------------------------------------------------*/
;*    port->sexp-list ...                                              */
;*---------------------------------------------------------------------*/
(define (port->sexp-list ip #!optional location)
   (port->list (lambda (ip) (read ip location)) ip))

;*---------------------------------------------------------------------*/
;*    *reader-ctors* ...                                               */
;*---------------------------------------------------------------------*/
(define *reader-ctors* '())

;*---------------------------------------------------------------------*/
;*    get-reader-ctor ...                                              */
;*---------------------------------------------------------------------*/
(define (get-reader-ctor f)
   (let ((p (assq f *reader-ctors*)))
      (when (pair? p)
	 (cdr p))))

;*---------------------------------------------------------------------*/
;*    define-reader-ctor ...                                           */
;*---------------------------------------------------------------------*/
(define (define-reader-ctor sym proc)
   (let ((c (assq sym *reader-ctors*)))
      (if (pair? c)
	  (set-cdr! c proc)
	  (set! *reader-ctors* (cons `(,sym . ,proc) *reader-ctors*)))))

;*---------------------------------------------------------------------*/
;*    *reader-extensions* ...                                          */
;*---------------------------------------------------------------------*/
(define *reader-extensions* '())

;*---------------------------------------------------------------------*/
;*    set-read-syntax! ...                                             */
;*---------------------------------------------------------------------*/
(define (set-read-syntax! char proc)
   (let ((c (assq char *reader-extensions*)))
      (if (pair? c)
	  (set-cdr! c proc)
	  (set! *reader-extensions*
	     (cons `(,char . ,proc) *reader-extensions*)))))
 
;*---------------------------------------------------------------------*/
;*    get-reader-extension ...                                         */
;*---------------------------------------------------------------------*/
(define (get-reader-extension sym)
   (let ((c (assq sym *reader-extensions*)))
      (when (pair? c)
	 (cdr c))))
