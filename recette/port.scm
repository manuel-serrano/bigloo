;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/bigloo/recette/port.scm              */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 24 10:56:01 1992                          */
;*    Last change :  Thu Sep 20 12:47:11 2018 (serrano)                */
;*                                                                     */
;*    On teste les operations simples sur les ports                    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module input-port
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-input-port)))

;*---------------------------------------------------------------------*/
;*    test1 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test1 port)
   (labels ((get-char (n)
		      (let loop ((i 1)
				 (r (read-char port)))
			 (if (= i n)
			     r
			     (loop (+ 1 i)
				   (read-char port)))))
	    (stole-char (n)
			(let loop ((i 1)
				   (r (peek-char port)))
			   (if (= i n)
			       r
			       (loop (+ 1 i)
				     (peek-char port))))))
      (get-char 10)
      (stole-char 10)
      (get-char 30)
      (stole-char 1) 
      (get-char 33)))

;*---------------------------------------------------------------------*/
;*    test2 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test2 string len)
   (let ((port (open-input-file string len)))
      (if (not (input-port? port))
	  #f
	  (labels ((get-char (n)
			     (let loop ((i 1)
					(r (read-char port)))
				(if (= i n)
				    r
				    (loop (+ 1 i)
					  (read-char port)))))
		   (stole-char (n)
			       (let loop ((i 1)
					  (r (peek-char port)))
				  (if (= i n)
				      r
				      (loop (+ 1 i)
					    (peek-char port))))))
	     (get-char 10)
	     (stole-char 10)
	     (get-char 30)
	     (stole-char 1)
	     (let ((res (get-char 33)))
		(close-input-port port)
		res)))))

;*---------------------------------------------------------------------*/
;*    test2b ...                                                       */
;*---------------------------------------------------------------------*/
(define (test2b string len)
   (let ((port (open-input-file (string-append "file:" string) len)))
      (if (not (input-port? port))
	  #f
	  (labels ((get-char (n)
			     (let loop ((i 1)
					(r (read-char port)))
				(if (= i n)
				    r
				    (loop (+ 1 i)
					  (read-char port)))))
		   (stole-char (n)
			       (let loop ((i 1)
					  (r (peek-char port)))
				  (if (= i n)
				      r
				      (loop (+ 1 i)
					    (peek-char port))))))
	     (get-char 10)
	     (stole-char 10)
	     (get-char 30)
	     (stole-char 1)
	     (let ((res (get-char 33)))
		(close-input-port port)
		res)))))

;*---------------------------------------------------------------------*/
;*    test3 ...                                                        */
;*---------------------------------------------------------------------*/
(define (test3 string)
   (let ((port (open-input-string string)))
      (let ((exp (read port)))
	 exp)))

;*---------------------------------------------------------------------*/
;*    test3b ...                                                       */
;*---------------------------------------------------------------------*/
(define (test3b string)
   (let ((port (open-input-file (string-append "string:" string))))
      (let ((exp (read port)))
	 exp)))

;*---------------------------------------------------------------------*/
;*    filepos ...                                                      */
;*---------------------------------------------------------------------*/
(define (filepos)
   (let* ((s "A toto n'est pas content A")
	  (g (regular-grammar ()
		((+ (in ("az"))) 1)
		((+ (in #"\t\n '")) (ignore))
		("A" 2)))
	  (p (open-input-string s))
	  (i (input-port-position p)))
      (let loop ((exp (read/rp g p))
		 (res (list i)))
	 (if (not (eof-object? exp))
	     (let ((new-res (cons (input-port-position p) res)))
		(loop (read/rp g p) new-res))
	     (reverse! res)))))

;*---------------------------------------------------------------------*/
;*    test-append-port ...                                             */
;*---------------------------------------------------------------------*/
(define (test-append-port l)
   (let ((p (open-output-file "recette.TMP")))
      (write (car l) p)
      (close-output-port p)
      (let ((p (append-output-file "recette.TMP")))
	 (write (cdr l) p)
	 (close-output-port p)
	 (let* ((p (open-input-file "recette.TMP"))
		(a (read p))
		(d (read p)))
	    (close-input-port p)
	    (delete-file "recette.TMP")
	    (cons a d)))))

;*---------------------------------------------------------------------*/
;*    test-reopen ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-reopen s)
   (let ((p (open-input-string "toto n'est pas content")))
      (read p)
      (read p)
      (reopen-input-c-string p s)
      (let* ((r1 (read p))
	     (r2 (read p))
	     (r3 (read p)))
	 (close-input-port p)
	 (list r1 r2 r3))))

;*---------------------------------------------------------------------*/
;*    test-binary ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-binary file obj)
   (let ((p (open-output-binary-file file))
	 (cout #\<))
      (output-obj p obj)
      (flush-binary-port p)
      (output-char p cout)
      (close-binary-port p)
      ;; close several times the port
      (close-binary-port p)
      (close-binary-port p)
      (let ((p (open-input-binary-file file)))
	 (let* ((r (input-obj p))
		(cin (input-char p)))
	    (close-binary-port p)
	    (delete-file file)
	    (if (char=? cout cin)
		r
		cin)))))

;*---------------------------------------------------------------------*/
;*    test-binary2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-binary2)
   (let ((name "recette.TMP"))
      (let ((p (open-output-binary-file name)))
	 (unwind-protect
	    (begin
	       (output-string p "foo")
	       (output-char p #\space)
	       (output-string p "bar")
	       (close-binary-port p)
	       (let ((p (open-input-binary-file name)))
		  (let* ((s (input-string p 6))
			 (c (input-char p)))
		     (close-binary-port p)
		     (and (char=? c #\r)
			  (string=? s "foo ba")))))
	    (delete-file name)))))

;*---------------------------------------------------------------------*/
;*    test-binary3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (test-binary3)
   (let ((name "recette.TMP3"))
      (and (copy-file "big_file.scm" name)
	   (unwind-protect
	      (let ((p1 (open-input-binary-file "big_file.scm"))
		    (p2 (open-input-binary-file name)))
		 (unwind-protect
		    (let loop ((s1 (input-string p1 1024))
			       (s2 (input-string p2 1024)))
		       (and (string=? s1 s2)
			    (if (=fx (string-length s1) 1024)
				(loop (input-string p1 1024)
				      (input-string p2 1024))
				#t)))
		    (begin
		       (close-binary-port p1)
		       (close-binary-port p2))))
	      (delete-file name)))))
	 
;*---------------------------------------------------------------------*/
;*    read-line-test                                                   */
;*---------------------------------------------------------------------*/
(define (read-line-test buf)
   (define (number-of char line)
      (let loop ((r (-fx (string-length line) 1))
		 (acc 0))
	 (cond
	    ((=fx r -1)
	     acc)
	    ((char=? (string-ref line r) char)
	     (loop (-fx r 1) (+fx acc 1)))
	    (else
	     (loop (-fx r 1) acc)))))
   (let ((p (open-input-file "misc/input.txt" (if buf 80 1))))
      (let loop ((line (read-line p))
		 (acc 0))
	 (if (eof-object? line)
	     (+fx acc 1024)
	     (loop (read-line p)
		   (+ (string-length line) (number-of #\space line) 1 acc))))))

;*---------------------------------------------------------------------*/
;*    read-line-test2 ...                                              */
;*---------------------------------------------------------------------*/
(define (read-line-test2)
   (with-input-from-string "toto\r\ntutu\rtata\n"
      (lambda ()
	 (let loop ((line (read-line))
		    (n 0))
	    (if (eof-object? line)
		n
		(loop (read-line) (+fx n 1)))))))

;*---------------------------------------------------------------------*/
;*    test-send-chars ...                                              */
;*---------------------------------------------------------------------*/
(define (test-send-chars p)
   (let ((pi (cond
		((procedure? p)
		 (open-input-procedure p))
		((string? p)
		 (open-input-string p))
		(else
		 p))))
      (unwind-protect
	 (with-output-to-string
	    (lambda ()
	       (send-chars pi (current-output-port))))
	 (close-input-port pi))))

;*---------------------------------------------------------------------*/
;*    test-send-chars-as-port-copy ...                                 */
;*---------------------------------------------------------------------*/
(define (test-send-chars-as-port-copy str)
   (let* ((iport (open-input-string str))
	  (fs (string-length str))
	  (oport (open-output-string)))
      (send-chars iport oport)
      (close-output-port oport)))

;*---------------------------------------------------------------------*/
;*    test-send-chars-with-length-and-offset ...                       */
;*---------------------------------------------------------------------*/
(define (test-send-chars-with-length-and-offset str)
   (let* ((iport (open-input-string str))
	  (fs (string-length str))
	  (oport (open-output-string)))
      (send-chars iport oport fs 0)
      (close-output-port oport)))

;*---------------------------------------------------------------------*/
;*    test-input-procedure ...                                         */
;*---------------------------------------------------------------------*/
(define (test-input-procedure proc)
   (with-input-from-procedure proc read))

;*---------------------------------------------------------------------*/
;*    test-read-fill-string ...                                        */
;*---------------------------------------------------------------------*/
(define (test-read-fill-string len str)
   (let ((res (make-string len #\space)))
      (with-input-from-string str
	 (lambda ()
	    (read-fill-string! res 0 len (current-input-port))))
      res))

;*---------------------------------------------------------------------*/
;*    proc-port ...                                                    */
;*---------------------------------------------------------------------*/
(define proc-port
   (let ((x 0))
      (lambda ()
	 (if (= x 103)
	     #f
	     (begin 
		(set! x (+ x 1))
		"1234567890")))))

;*---------------------------------------------------------------------*/
;*    test-procedure-port ...                                          */
;*---------------------------------------------------------------------*/
(define (test-procedure-port)
   (let ((p (open-input-procedure proc-port 10))
	 (count 0))
      (let loop ((c (read-char p)))
	 (unless (eof-object? c)
	    (set! count (+ count 1))
	    (loop (read-char p))))
      count))

;*---------------------------------------------------------------------*/
;*    test-reopen-string ...                                           */
;*---------------------------------------------------------------------*/
(define (test-reopen-string)
   (with-input-from-string "this is a string"
      (lambda ()
	 (input-port-reopen! (current-input-port))
	 (string? (read-line (current-input-port))))))

;*---------------------------------------------------------------------*/
;*    test-input-close-port ...                                        */
;*---------------------------------------------------------------------*/
(define (test-input-close-port p)
   (define (dotest thunk)
      (with-handler (lambda (e) 1) (begin (thunk) 0)))
   (+ (dotest (lambda () (read-char p)))
      (dotest (lambda () (peek-char p)))
      (dotest (lambda () (read-byte p)))
      (dotest (lambda () (peek-byte p)))
      (dotest (lambda () (read-string p)))
      (dotest (lambda () (read-of-strings p)))
      (dotest (lambda () (read-chars 10 p)))
      (dotest (lambda () (let ((s (make-string 10)))
			    (read-chars! s 10 p))))
      (dotest (lambda () (let ((s (make-string 10)))
			    (read-fill-string! s 0 10 p))))
      (dotest (lambda () (port->string-list p)))
      (dotest (lambda () (let ((op (open-output-string)))
			    (send-chars p op 10))))))

;*---------------------------------------------------------------------*/
;*    test-input-string! ...                                           */
;*---------------------------------------------------------------------*/
(define (test-input-string! string off end)
   (let* ((end (if (<fx end 0) (string-length string) end))
	  (o (open-input-string! string off end)))
      (let ((r (read o)))
	 (close-input-port o)
	 r)))
      
;*---------------------------------------------------------------------*/
;*    test-input-string ...                                            */
;*---------------------------------------------------------------------*/
(define (test-input-string string off end)
   (let* ((end (if (<fx end 0) (string-length string) end))
	  (o (open-input-string string off end)))
      (let ((r (read o)))
	 (close-input-port o)
	 r)))

;*---------------------------------------------------------------------*/
;*    test-input-port ...                                              */
;*---------------------------------------------------------------------*/
(define (test-input-port)
   (test-module "test-input-port" "port.scm")
   (test "input.1" (open-input-file "wrong") (input-port? #\space))
   (test "input.2" (call-with-input-file "misc/input.txt" test1) #\.)
   (test "input.3" (test2 "misc/input.txt" 10) #\.)
   (test "input.4" (test2b "misc/input.txt" 10) #\.)
   (test "input-string.1" (test3 "(4 5 (3) #(1 2 3) \"toto\" titi)")
	 '(4 5 (3) #(1 2 3) "toto" titi))
   (test "input-string.2" (test3b "(4 5 (3) #(1 2 3) \"toto\" titi)")
	 '(4 5 (3) #(1 2 3) "toto" titi))
   (let ((s "\"foo\" 123 bar"))
      (test "input-string.3" (test-input-string s 0 -1) "foo")
      (test "input-string.4" (test-input-string s 5 -1) 123)
      (test "input-string.5" (test-input-string s 9 -1) 'bar)
      (test "input-string.6" (test-input-string s 0 5) "foo")
      (test "input-string.7" (test-input-string s 5 8) 12)
      (test "input-string.8" (test-input-string s 7 12) 23)
      (test "input-string.9" (test-input-string s 9 13) 'bar)
      (test "input-string.10" (test-input-string s 10 12) 'ba)
      (test "input-string!.1" (test-input-string! (string-copy s) 0 -1) "foo")
      (test "input-string!.2" (test-input-string! (string-copy s) 5 -1) 123)
      (test "input-string!.3" (test-input-string! (string-copy s) 9 -1) 'bar)
      (test "input-string!.4" (test-input-string! (string-copy s) 0 5) "foo")
      (test "input-string!.5" (test-input-string! (string-copy s) 5 8) 12)
      (test "input-string!.6" (test-input-string! (string-copy s) 7 12) 23)
      (test "input-string!.7" (test-input-string! (string-copy s) 9 13) 'bar)
      (test "input-string!.8" (test-input-string! (string-copy s) 10 12) 'ba))
   (let ((p (open-output-string)))
      (write #a012 p)
      (test "char.1" (close-output-port p) "#\\x0c"))
   (let ((p (open-output-string)))
      (test "char.2" (begin
			(with-output-to-port p
			   (lambda ()
			      (write #a012)))
			(close-output-port p))
	    "#\\x0c"))
   (let ((p (open-output-string)))
      (write #a179 p)
      (test "char.3" (close-output-port p) "#\\xb3"))
   (test "char.4"
      (let* ((s "toto\000tutu\000\000")
	     (p (open-input-string s)))
	 (let loop ((i 0))
	    (let ((c (read-char p)))
	       (if (eof-object? c)
		   (-fx (string-length s) i)
		   (loop (+ i 1))))))
      0)
   (test "char.4"
      (let* ((s "\"foo\" 123 gee")
	     (p (open-input-string s)))
	 (let loop ((i 0))
	    (let ((c (read-char p)))
	       (if (eof-object? c)
		   (-fx (string-length s) i)
		   (loop (+ i 1))))))
      0)
   (let ((p (open-output-string)))
      (display '("toto" N'EST PAS CONTENT) p)
      (let ((s (get-output-string p)))
	 (close-output-port p)
	 (test "output-string" s '"(toto N'EST PAS CONTENT)")))
   (let* ((stringa (make-string 1020 #\a))
	  (stringb (make-string 1020 #\b))
	  (stringc (make-string 1020 #\c))
	  (stringd (make-string 234 #\d))
	  (string  (string-append stringa stringb stringc stringd)))
      (test "long write"
	    (let ((port (open-output-string)))
	       (display string port) (close-output-port port))
	    string))
   (test "double close" (let ((p (open-input-file "misc/input.txt")))
			   (close-input-port p)
			   (close-input-port p)
			   #f)
	 #f)
   (test "input-port-length.1"
      (call-with-input-string (make-string 23) input-port-length)
      23)
   (test "input-port-length.2" 
      (call-with-input-file "misc/input.txt" input-port-length)
      (file-size "misc/input.txt"))
   (test "filepos" (filepos) '(0 1 6 8 12 16 24 26))
   (let ((l '((1 2 3) . (4 5 6))))
      (test "append" (test-append-port l) l))
   (test "re-open" (test-reopen "tutu non plus") '(tutu non plus))
   (let* ((s "TOTO")
	  (t 'toto)
	  (c (cons s t))
	  (l (list c c c #\a #a127))
	  (v (vector s t c l 1.0 23)))
      (test "binary.1" (test-binary "misc/binary.BIN" v) v))
   (test "binary.2" (test-binary2) #t)
   (let ((p (open-input-string "a")))
      (test "char-ready(string).1" (char-ready? p) #t)
      (read p)
      (test "char-ready(string).2" (char-ready? p) #f)
      (close-input-port p)
      (test "char-ready(string).3" (char-ready? p) #f))
   (test "binary.3" (test-binary3) #t)
   (let ((p (open-input-file "misc/trap.txt")))
      (test "char-ready(file).1" (char-ready? p) #t)
      (read p)
      (test "char-ready(file).2" (char-ready? p) #t)
      (close-input-port p)
      (test "char-ready(file).3" (char-ready? p) #f))
   (let ((p (open-input-file "misc/trap.txt")))
      (let loop ((e (read p)))
	 (if (eof-object? e)
	     (begin
		(test "char-ready(file).4" (char-ready? p) #f)
		(close-input-port p)
		(test "char-ready(file).5" (char-ready? p) #f))
	     (loop (read p)))))
   (test "read-line" (read-line-test #t) (read-line-test #f))
   (test "read-line.2" (read-line-test2) 3)
   (test "format.1" (format "Hello, ~a" "World!") "Hello, World!")
   (test "format.2" (format "Error, list is too short: ~s~%" '(one "two" 3))
	 "Error, list is too short: (one \"two\" 3)\n")
   (test "format.3" (format "~a as a string is ~s.~n" '(3 4) "(3 4)")
	 "(3 4) as a string is \"(3 4)\".\n")
   (test "format.4" (format "~vtoto" '(1 2)) "(1 2)\ntoto")
   (test "format.5" (format "|~c|" #\a) "|a|")
   (test "format.6" (format "|~x|" 10) "|a|")
   (test "format.7" (format "|~o|" 10) "|12|")
   (test "format.8" (format "|~b|" 10) "|1010|")
   (test "format.9" (format "|~~|") "|~|")
   (cond-expand
      (bigloo-c (test "set-output-string-port-position"
		      (let ((p (open-output-string)))
			 (display "0123456789" p)
			 (set-output-port-position! p 4)
			 (display "ab" p)
			 (close-output-port p))
		      "0123ab"))
      (else #unspecified))
   (test "set-input-string-port-position"
	 (let ((p (open-input-string "1 2 3 4 5 6 7 8 9 10")))
	    (unwind-protect
	       (begin
		  (read p)
		  (read p)
		  (read p)
		  (read p)
		  (read p)
		  (set-input-port-position! p 2)
		  (read p))
	       (close-input-port p)))
	 2)
   (test "set-input-file-port-position"
	 (let ((p (open-input-file "misc/input.txt")))
	    (unwind-protect
	       (begin
		  (read p)
		  (read p)
		  (read p)
		  (read p)
		  (read p)
		  (read p)
		  (set-input-port-position! p 11)
		  (read p))
	       (close-input-port p)))
	 'pas)
   (test "zero char"
	 (let ((os (open-output-string)))
	    (do ((i 0 (+ i 1)))
		((> i 1000) #f)
		(display "\000a" os))
	    (get-output-string os))
	 (let ((s (make-string 2002)))
	    (let loop ((i 0))
	       (if (=fx i 2002)
		   s
		   (begin
		      (string-set! s i #a000)
		      (string-set! s (+fx i 1) #\a)
		      (loop (+fx i 2)))))))
   (test "output-string-port?.1" (output-port? (open-output-string)) #t)
   (test "output-string-port?.2" (output-string-port? (open-output-string)) #t)
   (test "output-string-port?.3" (output-string-port? (current-output-port)) #f)
   (test "input-string-port?.1" (input-port? (open-input-string "foo")) #t)
   (test "input-string-port?.2" (input-string-port? (open-input-string "fo")) #t)
   (test "input-string-port?.3" (input-string-port? (current-input-port)) #f)
   (test "read-fill-string.1" (test-read-fill-string 4 "foobar") "foob")
   (test "read-fill-string.2" (test-read-fill-string 8 "foobar") "foobar  ")
   (test "input-procedure.1"
 	 (with-input-from-procedure list
	    (lambda ()
	       (input-procedure-port? (current-input-port))))
	 #t)
   (test "input-procedure.2"
	 (with-input-from-string "list"
	    (lambda ()
	       (input-procedure-port? (current-input-port))))
	 #f)
   (test "input-procedure.3"
	 (with-input-from-procedure
	       (let ((s #t))
		  (lambda ()
		     (if s
			 (begin
			    (set! s #f)
			    "foobar")
			 s)))
	    read)
	 'foobar)
   (test "input-procedure.4"
	 (with-input-from-procedure (lambda () "foobar ")
	    read)
	 'foobar)
   (test "input-procedure.5"
	 (test-input-procedure (lambda () "(1 2 \"3\")"))
	 '(1 2 "3"))
   (test "send-chars.1" (test-send-chars "foobar") "foobar")
   (test "send-chars.2"
	 (test-send-chars (let ((s #t))
			     (lambda ()
				(if s
				    (begin
				       (set! s #f)
				       "foobar")
				    s))))
	 "foobar")
   (let ((str "now is the time for all good men..."))
      (test "send-chars.3" (test-send-chars-as-port-copy str) str)
      (test "send-chars.4" (test-send-chars-with-length-and-offset str) str))
   (test "directory->list"
	 (let ((lst (directory->list "misc")))
	    (if (every (lambda (f)
			  (member f lst))
		   '("dump.c" "dump.jvm" "input.txt" "jm.txt" "trap.txt"))
		#t #f))
	 #t)
   (test "flush-output-string-port"
	 (let ((outp (open-output-string)))
	    (display "this is some text" outp)
	    (let* ((v1 (flush-output-port outp))
		   (v2 (flush-output-port outp)))
	       (display "zot" outp)
	       (list v1 v2 (flush-output-port outp))))
	 '("this is some text" "this is some text" "this is some textzot"))
   (test "flush-output-string-port.2"
	 (let ((outp (open-output-string)))
	    (display "this is some text" outp)
	    (let* ((v1 (flush-output-port outp))
		   (_ (reset-output-port outp))
		   (v2 (flush-output-port outp)))
	       (display "zot" outp)
	       (list v1 v2 (flush-output-port outp))))
	 '("this is some text" "" "zot"))
   (test "procedure-port" (test-procedure-port) 1030)
   (test 'output-procedure-port.1
	 (output-procedure-port? '#(1 2))
	 #f)
   (test 'output-procedure-port.2
	 (output-procedure-port? (open-output-string))
	 #f)
   (test 'output-procedure-port.3
	 (output-procedure-port? (open-output-procedure (lambda (x) x)))
	 #t)
   (test 'output-procedure-port.4
	 (let* ((res '())
		(p (open-output-procedure
		    (lambda (s) (set! res (cons (string-copy s) res))))))
	    (display 1 p)
	    (newline p)
	    (display '(1 2) p)
	    (display 32 p)
	    (close-output-port p)
	    (reverse res))
	 '("1" "\n" "(" "1" " " "2" ")" "32"))
   (test 'reopen-string-port
	 (test-reopen-string)
	 #t)
   (test "close-input-port.1" (let ((p (open-input-file "misc/input.txt")))
				 (close-input-port p)
				 (when (closed-input-port? p)
				    (test-input-close-port p)))
	 11)
   (test "close-input-port.2" (let ((p (open-input-string "foo")))
				 (close-input-port p)
				 (when (closed-input-port? p)
				    (test-input-close-port p)))
	 11)
   (test "close-input-port.3" (let ((p (open-input-procedure (lambda () #f))))
				 (close-input-port p)
				 (when (closed-input-port? p)
				    (test-input-close-port p)))
	 11)
   (test "flush-hook" (let* ((p (open-output-file "recette.TMP" 10))
			     (n 0)
			     (fhook (lambda (p s) (set! n (+ s n)))))
			 (output-port-flush-hook-set! p fhook)
			 (flush-output-port p)
			 (write "foo" p)
			 (write "bar" p)
			 (write '(1 2 3) p)
			 (close-output-port p)
			 (delete-file "recette.TMP")
			 n)
	 17))
   
