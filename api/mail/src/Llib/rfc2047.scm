;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/mail/src/Llib/rfc2047.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed May 30 12:51:46 2007                          */
;*    Last change :  Thu Dec  1 09:11:29 2016 (serrano)                */
;*    Copyright   :  2007-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This module implements parser following the RFC 2047             */
;*    MIME (Multipurpose Internet Mail Extensions) Part Three:         */
;*    RFC 2047, may be found at:                                       */
;*      http://tools.ietf.org/html/rfc2047                             */
;*                                                                     */
;*    In other words, this module decodes the =?...?= syntax found in  */
;*    mails header.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __mail_rfc2047
   
   (import __mail_rfc2045)
   
   (export (rfc2047-decode-port ::input-port ::output-port
				#!key (charset 'iso-latin-1))
	   (rfc2047-decode::bstring ::bstring #!key (charset 'iso-latin-1))))

;*---------------------------------------------------------------------*/
;*    parse-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (parse-error proc message obj port)
   (raise
    (instantiate::&io-parse-error
       (proc proc)
       (msg message)
       (obj (if (char? obj)
		(string-append "{" (string obj) "}" (read-line port))
		obj))
       (fname (input-port-name port))
       (location (input-port-position port)))))

;*---------------------------------------------------------------------*/
;*    token-grammar ...                                                */
;*---------------------------------------------------------------------*/
(define token-grammar
   (regular-grammar ()
      ((: (+ (out #\space #\newline #\tab #\return #\?)) #\?)
       (string->symbol (string-downcase! (the-substring 0 -1))))
      (else
       (parse-error 'rfc2047-decode "Illegal token" (the-failure) (the-port)))))

;*---------------------------------------------------------------------*/
;*    encoded-text-grammar ...                                         */
;*---------------------------------------------------------------------*/
(define encoded-text-grammar
   (regular-grammar (out)
      ("?="
       #unspecified)
      (#\?
       (write-char #\? out))
      ((+ (out #\space #\newline #\tab #\return #\? #\_))
       (display (the-string) out)
       (ignore))
      (#\_
       (write-char #\space out)
       (ignore))
      ((+ (in #\space #\newline #\tab #\return))
       (display (the-string) out)
       (ignore))
      (else
       #unspecified)))

;*---------------------------------------------------------------------*/
;*    charset ...                                                      */
;*---------------------------------------------------------------------*/
(define (charset cset cs str)
   (case cs
      ((utf-8)
       (case cset
	  ((utf-8)
	   str)
	  ((iso-latin-1)
	   (with-handler
	      (lambda (e) str)
	      (utf8->iso-latin! str)))
	  ((cp1252)
	   (with-handler
	      (lambda (e) str)
	      (utf8->cp1252! str)))
	  (else
	   str)))
      ((cp1252)
       (case cset
	  ((utf-8)
	   (cp1252->utf8! str))
	  ((iso-latin-1)
	   (with-handler
	      (lambda (e) str)
	      (utf8->iso-latin! (cp1252->utf8! str))))
	  (else
	   str)))
      (else
       (case cset
	  ((utf-8)
	   (iso-latin->utf8! str))
	  ((cp1252)
	   (utf8->cp1252! (iso-latin->utf8! str)))
	  (else
	   str)))))

;*---------------------------------------------------------------------*/
;*    rfc2047-grammar ...                                              */
;*---------------------------------------------------------------------*/
(define rfc2047-grammar
   (regular-grammar (out cset)
      ("=?"
       (let* ((cs (read/rp token-grammar (the-port)))
	      (encoding (read/rp token-grammar (the-port))))
	  (case encoding
	     ((q)
	      (let ((pout (open-output-string)))
		 (read/rp encoded-text-grammar (the-port) pout)
		 (let* ((s (close-output-port pout))
			(es (if (procedure? cset)
				(cset (quoted-printable-decode s) cs)
				(charset cset cs (quoted-printable-decode s)))))
		    (display es out)))
	      (ignore))
	     ((b)
	      (let ((pout (open-output-string)))
		 (read/rp encoded-text-grammar (the-port) pout)
		 (let* ((s (close-output-port pout))
			(es (if (procedure? cset)
				(cset (base64-decode s) cs)
				(charset cset cs (base64-decode s)))))
		    (display es out)))
	      (ignore))
	     (else
	      (read/rp encoded-text-grammar (the-port) out)))))
      ((+ (out #\= #\Space #\Return #\Newline))
       (display (the-string) out)
       (ignore))
      (#\space
       (display " " out)
       (ignore))
      ((: (+ (: (? #\Return) #\Newline)) (* #\Space))
       (ignore))
      (#\?
       (write-char #\? out)
       (ignore))
      (else
       (let ((c (the-failure)))
	  (unless (eof-object? c)
	     (write-char c out)
	     (send-chars (the-port) out))))))

;*---------------------------------------------------------------------*/
;*    rfc2047-decode-port ...                                          */
;*---------------------------------------------------------------------*/
(define (rfc2047-decode-port in::input-port out::output-port
			     #!key (charset 'iso-latin-1))
   (let ((cs (if (procedure? charset)
		 charset
		 (case charset
		    ((utf-8 UTF-8)
		     'utf-8)
		    ((iso-latin-1 ISO-LATIN-1 iso-8859-1 ISO-8859-1)
		     'iso-latin-1)
		    ((cp1252 CP1252 cp-1252 CP-1252 windows-1252)
		     'cp1252)
		    (else
		     (error 'rfc2047-decode-port "Illegal charset" charset))))))
      (read/rp rfc2047-grammar in out cs)))

;*---------------------------------------------------------------------*/
;*    rfc2047-decode ...                                               */
;*---------------------------------------------------------------------*/
(define (rfc2047-decode str #!key (charset 'iso-latin-1))
   (if (<=fx (string-length str) 6)
       str
       (let ((i (string-contains str "=?")))
	  (if (not i)
	      str
	      ;; the string is encoded, decode it
	      (let ((pout (open-output-string))
		    (pin (open-input-string str i)))
		 (rfc2047-decode-port pin pout :charset charset)
		 (close-input-port pin)
		 (let ((ds (close-output-port pout)))
		    (if (=fx i 0)
			ds
			(string-append (substring str 0 i) ds))))))))
