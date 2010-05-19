;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/rgc-insert.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Florian Loitsch                                   */
;*    Creation    :  Thu Sep  3 12:29:48 2009                          */
;*    Last change :  Thu Sep  3 14:10:30 2009 (serrano)                */
;*    Copyright   :  2009 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Some rgc-buffer-insert-string tests                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module rgc-insert
   (import (main "main.scm"))
   (include "test.sch")
   (export (test-rgc-insert)))

(define *number*
   (regular-grammar ()
      ((: (submatch (+ digit)) "." (submatch (+ digit)))
       (cons (string->integer (the-submatch 1))
	     (string->integer (the-submatch 2))))))

(define *number2*
   (regular-grammar ()
      ((: (submatch (* digit)) "." (submatch (* digit)))
       (cons (string->integer (the-submatch 1))
	     (string->integer (the-submatch 2))))))

(define (test-rgc-insert)
   (test-module "rgc-insert" "rgc-insert.scm")
   (test "unread-string-readchars"
	 (let* ((p (open-input-string "123456789"))
		(dummy1 (unread-string! "ab" p))
		(str1 (read-chars 3 p)) ;; ab1
		(dummy2 (unread-string! "cdefghijklmnop" p))
		(str2 (read-chars 4 p)) ;; cdef
		(str3 (read-chars 10 p)) ;; g-p
		(str4 (read-chars 5 p)) ;; 23456
		(dummy3 (unread-string! "qr" p))
		(str5 (read-string p)) ;; qr789
		(c-ready? (char-ready? p))
		(dummy4 (unread-string! "st" p))
		(str6 (read-chars 1 p)) ;; s
		(c-ready?2 (char-ready? p))
		(str7 (read-chars 1 p)) ;; t
		(c-ready?3 (char-ready? p))
		(str8 (read p)))
	    (and (equal? str1 "ab1")
		 (equal? str2 "cdef")
		 (equal? str3 "ghijklmnop")
		 (equal? str4 "23456")
		 (equal? str5 "qr789")
		 (not c-ready?)
		 (equal? str6 "s")
		 c-ready?2
		 (equal? str7 "t")
		 (not c-ready?3)
		 (eof-object? str8)))
	 #t)
   (test "unread-string-read"
	 (let* ((p (open-input-string "a 1 #\\x \"str\""))
		(dummy0 (unread-string! " " p))
		(c0 (read-char p)) ;; space
		(dummy0b (unread-string! "" p))
		(sym (read p))     ;; a
		;(dummy0c (tprint (read-char p)))
		(dummy1 (unread-string! "b" p))
		(sym2 (read p)) ;; b
		(dummy2 (begin
			   (read-char p) ;; the space
			   (unread-string! "2" p)))
		(nb (read p)) ;; 21
		(char (read p))
		(dummy3 (unread-string! "\"\"" p))
		(str1 (read p))
		(dummy4 (begin
			   (read-char p) ;; the space
			   (unread-string! "#\\" p)))
		(char2 (read p)) ;; #\"
		(dummy5 (unread-string! "\"" p))
		(str2 (read p)))
	    ;(tprint sym " " sym2 " " nb " " char " " str1 " " char2 " " str2)
	    (and (equal? c0 #\space)
		 (eq? sym 'a)
		 (eq? sym2 'b)
		 (=fx nb 21)
		 (char=? char #\x)
		 (string=? str1 "")
		 (char=? char2 #\")
		 (string=? str2 "str")))
	 #t)
   (test "unread-string-read"
	 (let* ((p (open-input-string "a 1 #\\x \"str\""))
		(dummy0 (unread-char! #\space p))
		(c0 (read-char p)) ;; space
		(sym (read p))     ;; a
		(dummy1 (unread-char! #\b p))
		(sym2 (read p)) ;; b
		(dummy2 (begin
			   (read-char p) ;; the space
			   (unread-char! #\2 p)))
		(nb (read p)) ;; 21
		(char (read p))
		(dummy3 (begin
			   (unread-char! #\" p)
			   (unread-char! #\" p)))
		(str1 (read p))
		(dummy4 (begin
			   (read-char p) ;; the space
			   (unread-char! #\\ p)
			   (unread-char! #\# p)))
		(char2 (read p)) ;; #\"
		(dummy5 (unread-char! #\" p))
		(str2 (read p)))
	    ;(tprint sym " " sym2 " " nb " " char " " str1 " " char2 " " str2)
	    (and (equal? c0 #\space)
		 (eq? sym 'a)
		 (eq? sym2 'b)
		 (=fx nb 21)
		 (char=? char #\x)
		 (string=? str1 "")
		 (char=? char2 #\")
		 (string=? str2 "str")))
	 #t)
   (test "unread-char-readchars"
	 (let* ((p (open-input-string "123456789"))
		(dummy1 (unread-char! #\a p))
		(str1 (read-chars 2 p)) ;; a1
		(dummy2 (unread-char! #\c p))
		(str2 (read-chars 1 p))) ;; c
	    (and (equal? str1 "a1")
		 (equal? str2 "c")))
	 #t)
   (test "unread-substring-readchars"
	 (let* ((p (open-input-string "123456789"))
		(dummy1 (unread-substring! "0ab1" 1 3 p))
		(str1 (read-chars 3 p)) ;; ab1
		(dummy2 (unread-substring! "1234cdefghijklmnop11" 4 18 p))
		(str2 (read-chars 4 p)) ;; cdef
		(str3 (read-chars 10 p)) ;; g-p
		(str4 (read-chars 5 p)) ;; 23456
		(dummy3 (unread-substring! "qr1" 0 2 p))
		(str5 (read-string p)) ;; qr789
		(c-ready? (char-ready? p))
		(dummy4 (unread-substring! "0st" 1 3 p))
		(str6 (read-chars 1 p)) ;; s
		(c-ready?2 (char-ready? p))
		(str7 (read-chars 1 p)) ;; t
		(c-ready?3 (char-ready? p))
		(str8 (read p)))
	    (and (equal? str1 "ab1")
		 (equal? str2 "cdef")
		 (equal? str3 "ghijklmnop")
		 (equal? str4 "23456")
		 (equal? str5 "qr789")
		 (not c-ready?)
		 (equal? str6 "s")
		 c-ready?2
		 (equal? str7 "t")
		 (not c-ready?3)
		 (eof-object? str8)))
	 #t)
   (test "unread-substring-read"
	 (let* ((p (open-input-string "a 1 #\\x \"str\""))
		(dummy0 (unread-substring! "1 2" 1 2 p))
		(c0 (read-char p)) ;; space
		(dummy0b (unread-substring! "1234" 2 2 p))
		(sym (read p))     ;; a
		;(dummy0c (tprint (read-char p)))
		(dummy1 (unread-substring! "1b2222" 1 2 p))
		(sym2 (read p)) ;; b
		(dummy2 (begin
			   (read-char p) ;; the space
			   (unread-substring! "02" 1 2 p)))
		(nb (read p)) ;; 21
		(char (read p))
		(dummy3 (unread-substring! "\"\"124332" 0 2 p))
		(str1 (read p))
		(dummy4 (begin
			   (read-char p) ;; the space
			   (unread-substring! "1234567890#\\" 10 12 p)))
		(char2 (read p)) ;; #\"
		(dummy5 (unread-substring! "\"" 0 1 p))
		(str2 (read p)))
	    ;(tprint sym " " sym2 " " nb " " char " " str1 " " char2 " " str2)
	    (and (equal? c0 #\space)
		 (eq? sym 'a)
		 (eq? sym2 'b)
		 (=fx nb 21)
		 (char=? char #\x)
		 (string=? str1 "")
		 (char=? char2 #\")
		 (string=? str2 "str")))
	 #t)
   (test "unread-substring-error1"
	 (with-handler
	    (lambda (e) #t)
	    (let ((p (open-input-string "A")))
	       (unread-substring! "" 0 2 p)
	       #f))
	 #t)
   (test "unread-substring-error2"
	 (with-handler
	    (lambda (e) #t)
	    (let ((p (open-input-string "A")))
	       (unread-substring! "aoeu" -1 0 p)
	       #f))
	 #t)
   (test "unread-substring-error3"
	 (with-handler
	    (lambda (e) #t)
	    (let ((p (open-input-string "A")))
	       (unread-substring! "oaeu" 2 0 p)
	       #f))
	 #t)
   (test "unread-substring-error4"
	 (with-handler
	    (lambda (e) #t)
	    (let ((p (open-input-string "A")))
	       (close-input-port p)
	       (unread-substring! "oaeu" 0 2 p)
	       #f))
	 #t))
