;*=====================================================================*/
;*    serrano/prgm/project/bigloo/recette/rgc.scm                      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  8 11:03:03 1994                          */
;*    Last change :  Wed Apr 18 18:55:04 2012 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Quelques tests d'rgc                                             */
;*=====================================================================*/


;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module rgc
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-rgc)))

;; cette grammaire plante tous les bigloo (a la compilation)
;; jusqu'a la version 1.6c
(regular-grammar
      ((sign (in #\+ #\-))
       (optsign (>= 1 sign))
       (octdigit (in ("07"))))
   ((: #\0 optsign octdigit)
    0))

(regular-grammar ()
   ((: #\a (? #\b) #\c)
    0))

(regular-grammar ()
   ((: (? #\a) #\b #\c)
    0))

(regular-grammar ()
   ((: #\a #\b (? #\c))
    0))

;; une grammaire qui ne se compilait pas lors du permier boot de bigloo1.8
(define *std-grammar*
   (regular-grammar ((chiffre (in ("09")))
		     (lettre  (in ("azAZ") #a128 #a255))
		     (special (in "!@~$%^&*></.-_+\|=?:"))
		     (id      (: (or lettre chiffre special)
				 (* (or lettre chiffre special #\, #\' #\`)))))
      ((: #\# #\a chiffre chiffre chiffre)
       ;; character ascii forms
       0)
      ((: ";" (* all))
       ;; commets
       (ignore))
      ((: #\# (or id (: #\. (+ #\.))) #\()
       ;; typed vectors beginning
       1)
      (else
       2)))

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

(define *symbol*
   (regular-grammar ()
      ((+ (in ("az")))
       (the-subsymbol 1 (-fx (the-length) 1)))
      ((: (in ("09")) (+ (in ("az"))))
       (the-symbol))))

(define (recette-suffix string)
   (string-case string
      ((: (* all) "." (submatch (+ (out "."))))
       (the-submatch 1))
      (else
       "")))

(define (test-rgc= str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
             ((= 2 (: #\; (* all) #\newline))
	      (the-string))
	     (else
	      ""))))
      (read/rp gram port)))

(define (test-rgc-substring str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
             ((: #\" (+ alpha) #\")
	      (string=? (the-substring 1 (-fx (the-length) 1))
			(the-substring 1 -1)))
	     (else
	      #f))))
      (read/rp gram port)))

(define (test-rgc>= str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
             ((>= 2 (: #\; (* all) #\newline))
	      (the-string))
	     (else
	      ""))))
      (read/rp gram port)))

(define (rgc-and str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
		  ((+ (and (#\a #\b) "09abcd")) (the-string))
		  (else
		   ""))))
      (read/rp gram port)))

(define (rgc-and-2 str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
		  ((+ (and "am" "nz")) (the-string))
		  (else
		   ""))))
      (read/rp gram port)))

(define (rgc-but str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
		  ((+ (but ("09ad") ("ce"))) (the-string))
		  (else
		   ""))))
      (read/rp gram port)))

(define (rgc-** str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
		  ((** 3 6 #\a) (the-string)))))
      (read/rp gram port)))

(define (rgc-etc str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
		  ((... 3 "begin") (the-string)))))
      (read/rp gram port)))

(define (rgc-submatch str)
   (let ((port (open-input-string str))
	 (gram (regular-grammar ()
		  ((: (submatch (* #\space))
		      (submatch (+ #\+))
		      (submatch (* #\space)))
		   (string-append (the-submatch 1) (the-submatch 3))))))
      (read/rp gram port)))

(define (test-bof)
   (with-input-from-string "abcd"
      (lambda ()
	 (read/rp
	  (regular-grammar ()
	     ((bof all)
	      (let ((c (the-character)))
		 (cons `(bof ,c) (ignore))))
	     ((eof all)
	      (let ((c (the-character)))
		 (cons `(eof ,c) (ignore))))
	     ((bol all)
	      (let ((c (the-character)))
		 (cons `(bol ,c) (ignore))))
	     (else
	      (let ((char (the-failure)))
		 (cond ((eof-object? char) '())
		       (else
			(cons `(char ,char) (ignore)))))))
	  (current-input-port)))))

(define (test-read-chars bufsize strsize)
   (let ((p (if (number? bufsize)
		(open-input-file "misc/input.txt" bufsize)
		(open-input-file "misc/input.txt"))))
      (unwind-protect
	 (let loop ((str (read-chars strsize p))
		    (acc '()))
	    (if (or (eof-object? str) (string=? str ""))
		(apply string-append (reverse! acc))
		(loop (read-chars strsize p)
		      (cons str acc))))
	 (close-input-port p))))

(define (test-read-chars2 n bf of)
   (let ((p (open-input-file "misc/input.txt" bf)))
      (when (> of 0) (read-chars of p))
      (unwind-protect
	 (begin
	    (read-chars n p)
	    (input-port-position p))
	 (close-input-port p))))
      
(define (test-read-chars3 n bf of)
   (let ((p (open-input-file "misc/input.txt" bf)))
      (when (> of 0) (read-chars of p))
      (unwind-protect
	 (let* ((s1 (read-chars (* 2 n) p))
		(s2 (read-chars (* 2 n) p)))
	    (string-append s1 s2))
	 (close-input-port p))))
      
(define (test-read-chars4 n bf of)
   (let ((p (open-input-file "misc/input.txt" bf)))
      (when (> of 0) (read-chars of p))
      (unwind-protect
	 (read-chars (* 2 n) p)
	 (close-input-port p))))

(define (test-read-chars5 s)
   (with-input-from-string s
      (lambda ()
	 (let loop ((buf (read-chars 2))
		    (old #f))
	    (if (eof-object? buf)
		old
		(loop (read-chars 2) buf))))))

;*---------------------------------------------------------------------*/
;*    test-unread ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-unread)
   (string-case ",1"
      ((: #\, #\1)
       (unread-char! (string-ref (the-string) 1) (the-port))
       (read (the-port)))))

;*---------------------------------------------------------------------*/
;*    test-rgc ...                                                     */
;*---------------------------------------------------------------------*/
(define (test-rgc)
   (test-module "rgc" "rgc.scm")
   (test "submatch+"
      (read/rp *number* (open-input-string "3.1415")) '(3 . 1415))
   (test "submatch*"
      (read/rp *number2* (open-input-string "3.1415")) '(3 . 1415))
   (test "symbol.1"
      (read/rp *symbol* (open-input-string "abcdefgh")) 'bcdefg)
   (test "symbol.2"
      (read/rp *symbol* (open-input-string "0abcdefgh")) '0abcdefgh)
   (test "string-case" (recette-suffix "toto.org.scm") "scm")
   (test "rgc ="
      (test-rgc= #";1line\n;2line\n;3line\n;4line\n")
      #";1line\n;2line\n")
   (test "rgc substring"
      (test-rgc-substring "\"foo\"")
      #t)
   (let ((str #";1line\n;2line\n;3line\n;4line\n"))
      (test "rgc >="
	 (test-rgc>= str)
	 str))
   (test "rgc and" (rgc-and "aaaabbbbccc") "aaaabbbb")
   (test "rgc and" (rgc-and-2 "aaaabbbbccc") "")
   (test "rgc but" (rgc-but "aaaabbbbccc") "aaaabbbb")
   (test "rgc **" (rgc-** "aaaaaaaaaaabbbbccc") "aaaaaa")
   (test "rgc ..." (rgc-etc "begin") "beg")
   (test "rgc submatch" (rgc-submatch "   +++   ") "      ")
   (test "fixnum" (read/rp (regular-grammar () ((: digit) (the-fixnum)))
		     (open-input-string "1234"))
      1)
   (test "fixnum" (read/rp (regular-grammar () ((+ digit) (the-fixnum)))
		     (open-input-string "1234"))
      1234)
   (test "bof" (test-bof) '((bof #\a) (char #\b) (char #\c) (eof #\d)))
   (let ((res (test-read-chars #f 8192)))
      (test "read-chars.1" (test-read-chars 10 1) res)
      (test "read-chars.2" (test-read-chars 10 2) res)
      (test "read-chars.3" (test-read-chars 10 8) res)
      (test "read-chars.4" (test-read-chars 10 9) res)
      (test "read-chars.5" (test-read-chars 10 10) res)
      (test "read-chars.6" (test-read-chars 10 11) res)
      (test "read-chars.7" (test-read-chars 10 111) res)
      (test "read-chars.8" (test-read-chars2 3 3 0) 3)
      (test "read-chars.8b" (test-read-chars2 3 3 3) 6)
      (test "read-chars.8c" (test-read-chars2 3 10 3) 6)
      (test "read-chars.9" (test-read-chars3 3 3 0)
	 (test-read-chars4 6 3 0))
      (test "read-chars.9b" (test-read-chars3 3 3 10)
	 (test-read-chars4 6 3 10))
      (test "read-chars.9c" (test-read-chars3 10 3 10)
	 (test-read-chars4 20 3 10))
      (test "read-chars.9d" (test-read-chars3 10 3 0)
	 (test-read-chars4 20 3 0))
      (test "read-chars.10" (test-read-chars5 "123") "3")
      (test "read-chars.11" (test-read-chars5 "12") "12")
      (test "read-chars.12" (test-read-chars5 "1") "1"))
   (test "unread-chars" (test-unread) 1))
