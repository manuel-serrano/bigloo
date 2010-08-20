(module __openpgp-port-util
   (export (inline safe-read-char::char p)
	   (inline safe-read-octet::long p)
	   (inline safe-read-octets::bstring len p)
	   (base64-decode-pipe-port p::input-port)
	   (length-limited-pipe-port::input-port p::input-port len::long)
	   (concat-pipe-port::input-port p1::input-port . Lps)))


(define-inline (safe-read-char::char p)
   (let ((t (read-char p)))
      (when (eof-object? t)
	 (error "safe-read-char" "unexpected end of file" #f))
      t))
(define-inline (safe-read-octet::long p)
   (char->integer (safe-read-char p)))
(define-inline (safe-read-octets::bstring len p)
   (let ((str (read-chars len p)))
      (when (not (=fx (string-length str) len))
	 (error "safe-read-octets" "unexpected end of file" #f))
      str))

(define (length-limited-pipe-port p len)
   (open-input-procedure
    (lambda ()
       (if (=fx len 0)
	   #f
	   (let* ((len-to-read (minfx len 256))
		  (str (read-chars len-to-read p)))
	      (set! len (-fx len len-to-read))
	      (when (not (=fx len-to-read (string-length str)))
		 (error "length-limited-pipe-port"
			"unexpected end of file"
			#f))
	      str)))))

(define (concat-pipe-port p1 . Lps)
   (define (read-str)
      (let ((str (read-chars 512 p1)))
	 (cond
	    ((and (eof-object? str)
		  (null? Lps))
	     #f)
	    ((or (eof-object? str)
		 (string-null? str)) ;; should not happen
	     (set! p1 (car Lps))
	     (set! Lps (cdr Lps))
	     (read-str))
	    (else str))))

   (open-input-procedure read-str))

;; TODO: base64-decode-pipe-port currently reads the complete base64 string
;; before decoding it.
(define (base64-decode-pipe-port p)
   (let loop ((buffer ""))
      (let ((line (read-line p)))
	 (cond
	    ((eof-object? line)
	     (open-input-string (base64-decode buffer)))
	    ((string-null? line)
	     (loop (string-append buffer "\n")))
	    ((or (char=? #\- (string-ref line 0))
		 (char=? #\= (string-ref line 0)))
	     (unread-char! #\newline p)
	     (unread-string! line p)
	     (open-input-string (base64-decode buffer)))
	    (else
	     (loop (string-append buffer line "\n")))))))
