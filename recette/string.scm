;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/recette/string.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  3 10:18:56 1992                          */
;*    Last change :  Sat Sep 23 09:28:16 2017 (serrano)                */
;*                                                                     */
;*    On teste differentes operations sur les chaines de caracteres    */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module string
   (import  (main "main.scm"))
   (include "test.sch")
   (export  (test-string)))

(define (scheme-id->c-id string)
   (let* ((nstring (if (and (not (char-alphabetic? (string-ref string 0)))
			    (not (char=? (string-ref string 0) #\_)))
		       (string-append "_" string)
		       string))
	  (len     (string-length nstring))
	  (rg      #f)
	  (res     (make-string len)))
      (let loop ((i 0))
	 (if (=fx i len)
	     (remove__ (if rg
			   (string-append
			    (string-downcase res)
			    (string-append
			     "_"
			     (integer->string 7)))
			   (string-downcase res)))
	     (let ((c (string-ref nstring i)))
		(cond
		   ((or (and (char>=? c #\A)
			     (char<=? c #\Z))
			(and (char>=? c #\a)
			     (char<=? c #\z))
			(and (char>=? c #\0)
			     (char<=? c #\9))
			(char=? c #\_))
		    (string-set! res i (string-ref nstring i))
		    (loop (+fx i 1)))
		   (else
		    (set! rg #t)
		    (string-set! res i #\_) 
		    (loop (+fx i 1)))))))))

(define (remove__ string)
   (cond
      ((not (>=fx (string-length string) 6))
       string)
      ((not (char=? (string-ref string 0) #\_))
       string)
      ((not (char=? (string-ref string 1) #\_))
       string)
      ((and (not (char=? (string-ref string 2) #\i))
	    (not (char=? (string-ref string 2) #\f)))
       string)
      ((and (not (char=? (string-ref string 3) #\n))
	    (not (char=? (string-ref string 3) #\i)))
       string)
      ((and (not (char=? (string-ref string 4) #\i))
	    (not (char=? (string-ref string 4) #\n)))
       string)
      ((and (not (char=? (string-ref string 5) #\t))
	    (not (char=? (string-ref string 5) #\i)))
       string)
      ((not (char=? (string-ref string 6) #\_))
       string)
      (else
       (string-append "_n_o_f_u_c_k_i_n_g___init_or_fini" string))))

;*---------------------------------------------------------------------*/
;*    8bits->7bits ...                                                 */
;*---------------------------------------------------------------------*/
(define (8bits->7bits string::bstring)
   (let ((len (string-length string)))
      (let loop ((i 0))
         (if (=fx i len)
             string
             (begin 
                (case (string-ref string i)
                   ((#\é #\è)
                    (string-set! string i #\e))
                   ((#\É #\È)
                    (string-set! string i #\E))
                   ((#\à)
                    (string-set! string i #\a))
                   ((#\À)
                    (string-set! string i #\A))
                   ((#\û #\ü)
                    (string-set! string i #\u))
                   ((#\Û #\Ü)
                    (string-set! string i #\U))
                   ((#\î #\ï)
                    (string-set! string i #\i))
                   ((#\Î #\Ï)
                    (string-set! string i #\I))
                   ((#\ö #\ô)
                    (string-set! string i #\o))
                   ((#\Ö #\Ô)
                    (string-set! string i #\O))
                   ((#\ç)
                    (string-set! string i #\c))
                   ((#\C)
                    (string-set! string i #\C)))
                (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    test-string-copy ...                                             */
;*---------------------------------------------------------------------*/
(define (test-string-copy str)
   (string-copy str))

;*---------------------------------------------------------------------*/
;*    string/bstring ...                                               */
;*---------------------------------------------------------------------*/
(define (string/bstring)
   (let ((str "asdf\n"))
      (string-set! str 2 #\c)
      (string/bstring-foo str)))

(define (string/bstring-foo str)
   (begin 1 2 3 4 5 6 7 8)
   (begin 1 2 3 4 5 6 7 8)
   (begin 1 2 3 4 5 6 7 8)
   str)

;*---------------------------------------------------------------------*/
;*    string-natural<? ...                                             */
;*---------------------------------------------------------------------*/
(define (string-natural<? s1 s2)
   (=fx (string-natural-compare3 s1 s2) -1))

;*---------------------------------------------------------------------*/
;*    test-sha1 ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-sha1 n str sum)
   (test (string-append "sha1-string." (integer->string n)) (sha1sum str) sum)
   (with-input-from-string str
      (lambda ()
	 (test (string-append "sha1-port." (integer->string n))
	    (sha1sum (current-input-port)) sum)))
   (test (string-append "sha1-mmap." (integer->string n))
      (sha1sum (string->mmap str)) sum))
   
;*---------------------------------------------------------------------*/
;*    test-sha256 ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-sha256 n str sum)
   (test (string-append "sha256-string." (integer->string n)) (sha256sum str) sum)
   (with-input-from-string str
      (lambda ()
	 (test (string-append "sha256-port." (integer->string n))
	    (sha256sum (current-input-port)) sum)))
   (test (string-append "sha256-mmap." (integer->string n))
      (sha256sum (string->mmap str)) sum))

;*---------------------------------------------------------------------*/
;*    test-sha256-2 ...                                                */
;*---------------------------------------------------------------------*/
(define (test-sha256-2)
   (let ((res #t))
      (for-each (lambda (s1 res)
		   (set! res (and res (string=? (sha256sum-string s1) res))))
	 '("VzqZI-2-d2WCGxDu*W.RFiJ9*bz=&F,V45a?WFKCkk5uk,OeKBVSJ8Eg9H3M;R.wc.+ZsDefqMzAA-E."
	   "++tNXb3ZBCx0yC0K+kIxeKWVCI6UUXeMPacq1ISfNSIP$,cP&K:3%pCdXlA$9hJe7ouMA;R:k3ihukX="
	   "C=,*E;ykDMzd3Ah:Ita.,uz-I8nbWTjbQ$VXM=7SmjWSm3ly&m,NGnhRZuTy-2zkCX7+xRKmD9HsPSQr"
	   "9F.NHL.KnKX-7+8z01K+EvHMBu8g;*WYAJ+K%2X,pb20?rcw#p-cnzPOTAUI$=.aF2KgH,*z;AcIu45p"
	   "g8RrL.%8eFtS7&Noar+9BMxoD-&Naonga4aLvZ&c7sUus,I#fe5Gt5yWWxnAMAjp8kDgMCiwV2QQt1Lz"
	   "O*VK=SE8.85fLVTdMG?VDtjRTsvmHq$Ylvlh:QT1BeJ:Z5To-q.$JWOfR.r1d-D-nOZd7vH;zTF1ebT#")
	 '("a97416f9cd9f7e346fe83c6cfd6e88220cdefe27dbf7fa2871915a186c90ceef"
	   "0c84722afd1093107afcad9ff599e7cb185ffc3acfe41ad5c0b63d7501563c3a"
	   "6df88b73faa26d5477efb32e4159c80db2fa700941bfe15a5cb395df07878015"
	   "e787d2bb5e7c672defd325bb2721a51bbd35d85af285af6bf8bde248929da5e6"
	   "9484011558493272f29bd108bd64479ee75d6e5d3238675bf9e58b9322d64a9a"
	   "48c29cef0652248f937af257d78d351bb049c035f94f0d6724cf62c625636522"))))

;*---------------------------------------------------------------------*/
;*    test-string ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-string)
   (test-module "string" "string.scm")
   (test "literal" "\342\202\254"
      (apply string (map integer->char '(#o342 #o202 #o254))))
   (test "string=?" (string=? "toto n'est pas content"
		       "toto n'est pas content")
      #t)
   (test "string=?" (string=? "ToTo" "tOtO") #f)
   (test "string=?" (let ((s (make-string 3 (integer->char 0))))
		       (string=? s #"\000\000\000"))
      #t)
   (test "string=?" (let ((s (make-string 3 (integer->char 0))))
		       (string=? s #"\000\001\000"))
      #f)
   (test "string-length" (string-length "12345") 5)
   (test "string" (equal? "toto n'est pas content" "toto est content")
      #f)
   (test "make-string" (string-ref (make-string 1 #\a) 0) #\a)
   (test "string-append" (string-append "Toto " "est content")
      "Toto est content")
   (test "string-append" (string-append "toto" " n'est" " pas" " content")
      "toto n'est pas content")
   (test "string-upcase" (string-upcase "toto TOTO ToTo") "TOTO TOTO TOTO")
   (test "string-ci=?"     (string-ci=? "Toto" "tOtO") #t)
   (test "string-set"    (let ((s (string-copy "0123456789")))
			    (string-set! s 0 (string-ref s 1))
			    s)
      "1123456789")
   (test "string-copy" (test-string-copy (make-string 3 #\6)) "666")
   (test "list->string" (list->string '(#\t #\o #\t #\o)) "toto")
   (test "string->list" (string->list "toto") '(#\t #\o #\t #\o))
   (test "string->list" (string->list "-$>$") '(#\- #\$ #\> #\$))
   (test "string->string" (list->string (string->list "tot oto")) "tot oto")
   (test "string->integer" (string->integer "01234") 1234)
   (test "integer->string" (integer->string 0) "0")
   (test "integer->string" (integer->string 1) "1")
   (test "integer->string" (integer->string -1) "-1")
   (test "integer->string" (integer->string 1234) "1234")
   (test "integer->string" (integer->string -1234) "-1234")
   (test "unsigned->string.1" (unsigned->string 1234 16) "4d2")
   (test "unsigned->string.2" (pair?
				 (member (unsigned->string -1234 16)
				    '("fffffb2e"  "fffffffffffffb2e")))
      #t)
   (test "unsigned->string.3" (unsigned->string #e1234 16) "4d2")
   (test "unsigned->string.4" (pair?
				 (member (unsigned->string #e-1234 16)
				    '("fffffb2e" "fffffffffffffb2e")))
      #t)
   (test "unsigned->string.5" (unsigned->string #l1234 16) "4d2")
   (test "unsigned->string.6" (unsigned->string #l-1234 16) "fffffffffffffb2e")
   (test "string->real" (string->real "1234.25") 1234.25)
   (test "real->string" (real->string 1234.25) "1234.25")
   (test "substring" (substring "0123456789" 1 5) "1234")
   (let ((dst (make-string 10 #\0))
	 (src (make-string 5 #\1)))
      (test "blit-string" (begin (blit-string! src 1 dst 1 3)
				 dst)
	 "0111000000"))
   (test "string<?" (string<? "012345" "123456") #t)
   (test "string<=?" (string<=? "012345" "012345") #t)
   (test "string>?" (string>? "012345" "123456") #f)
   (test "string>=?" (string<=? "012345" "012345") #t)
   (test "string-ci<?" (string-ci<? "abcdef" "ABCDEF") #f)
   (test "string-ci<?" (string-ci<? "abcdef" "ABCDEG") #t)
   (test "string-ci<=?" (string-ci<=? "A" "A") #t)
   (test "string-ci<=?" (string-ci<=? "abcdef" "ABCDEG") #t)
   (test "string-ci<=?" (string-ci<=? "abcdef" "ABCDEG") #t)
   (test "string-ci>?" (string-ci>? "abcdef" "ABCDEG") #f)
   (test "string-ci>?" (string-ci>? "abcdef" "ABCDEG") #f)
   (test "string-ci>=?" (string-ci>=? "abcdef" "ABCDEG") #f)
   (test "string-ci>=?" (string-ci>=? "abcdef" "ABCDEF") #t)
   (test "string-ci=?" (string-ci=? "toto" "titi") #f)
   (test "string-ci=?" (string-ci=? "toto" "Toto") #t)
   (test "string-ci=?" (string-ci=? "toto" "tot") #f)
   (test "substring=?" (substring=? "abcdef" "abcd" 3) #t)
   (test "substring=?" (substring=? "abcdef" "Abcd" 3) #f)
   (test "substring-ci=?" (substring-ci=? "abcdef" "abc" 4) #f)
   (test "substring-ci=?" (substring-ci=? "abcdef" "abcd" 3) #t)
   (test "substring-ci=?" (substring-ci=? "abcdef" "Abcd" 3) #t)
   (test "substring-ci=?" (substring-ci=? "abcdef" "Abc" 4) #f)
   (test "substring-at?.1" (substring-at? "abcdefghij" "abc" 0) #t)
   (test "substring-at?.2" (substring-at? "abcdefghij" "def" 0) #f)
   (test "substring-at?.3" (substring-at? "abcdefghij" "def" 3) #t)
   (test "substring-at?.4" (substring-at? "abcdefghij" "def" 2) #f)
   (test "substring-at?.5" (substring-at? "abcdefghij" "defj" 3) #f)
   (test "substring-at?.6" (substring-at? "abcdefghij" "defj" 3 3) #t)
   (test "substring-at?.7" (substring-at? "abcdefghij" "defj" 33 3) #f)
   (test "substring-at?.8" (substring-at? "abcdefghij" "defj" 3 -3) #f)
   (test "substring-at?.9" (substring-at? "abcdefghij" "defj" 2) #f)
   (test "substring-ci-at?.1" (substring-ci-at? "abcdefghij" "defj" 3 3) #t)
   (test "substring-ci-at?.2" (substring-ci-at? "abcdefghij" "dEFJ" 3 3) #t)
   (test "substring-ci-at?.3" (substring-ci-at? "abcdefghij" "dEFJ" 3) #f)
   (test "substring-ci-at?.4" (substring-ci-at? "abcdefghij" "dEJ" 3 3) #f)
   (test "substring-ci-at?.5" (substring-ci-at? "abcdefghij" "defj" 33 3) #f)
   (test "substring-ci-at?.6" (substring-ci-at? "abcdefghij" "defj" 3 -3) #f)
   (test "8bits" (8bits->7bits "ÉéÈèÏï") "EeEeIi")
   (test "8bits-string<?" (string<? "abecd" "abécd") #t)
   (test "8bits-string<=?" (string<=? "abecd" "abécd") #t)
   (test "8bits-string>?" (string>? "abécd" "abecd") #t)
   (test "8bits-string>=?" (string>=? "abécd" "abecd") #t)
   (test "8bits-string-ci<?" (string-ci<? "abecd" "abécd") #t)
   (test "8bits-string-ci<=?" (string-ci<=? "abecd" "abécd") #t)
   (test "8bits-string-ci>?" (string-ci>? "abécd" "abecd") #t)
   (test "8bits-string-ci>=?" (string-ci>=? "abécd" "abecd") #t)
   (test "foreign" (let ((x "\n\t\\\"")) (string->list x))
      '(#\newline #\tab #\\ #\"))
   (test "foreign" (let ((x #"\n\\\"")) (string->list x))
      '(#\newline #\\ #\"))
   (test "symbol" (symbol->string (string->symbol "tOtO")) "tOtO")
   (test "symbol" (eq? (string->symbol "ToTo") 'toto) #f)
   (test "symbol" (eq? (string->symbol "TOTO") 'TOTO) #t)
   (test "string-copy" (string-copy "toto n'est pas content")
      "toto n'est pas content")
   (test "escape.1" (string-length #"\000") 1)
   (test "escape.2" (string-length #"\x00") 1)
   (test "escape.3" (char->integer (string-ref #"\000" 0)) 0)
   (test "escape.4" (char->integer (string-ref #"\003" 0)) 3)
   (test "escape.5" (char->integer (string-ref #"\x03" 0)) 3)
   (test "escape.6" (char->integer (string-ref #"\x7f" 0)) #x7f)
   (test "escape.7" (char->integer (string-ref #"\x7F" 0)) #x7f)
   (test "escape.8" (char->integer (string-ref #"\X7e" 0)) #x7e)
   (test "escape.9" (char->integer (string-ref #"\X7E" 0)) #x7e)
   (test "escape.10" (char->integer (string-ref #"\XaE" 0)) #xae)
   (test "escape.11" (char->integer (string-ref #"\XAE" 0)) #xae)
   (test "escape(8bits)" (list "böig") '("böig"))
   (test "id"
      (scheme-id->c-id "INITIALIZE-IMPORTED-MODULES!_FOO")
      "initialize_imported_modules__foo_7")
   (test "trigraph" "??-" (string-append "?" "?" "-"))
   (test "suffix.1" (suffix "toto.scm") "scm")
   (test "suffix.2" (suffix "toto") "")
   (test "suffix.3" (suffix (make-file-name "." "toto")) "")
   (test "suffix.4" (suffix (make-file-name ".." "toto")) "")
   (test "suffix.5" (suffix (string-append (string (file-separator)) "etc" (string (file-separator)) "rc.d" (string (file-separator)) "rc.3" (string (file-separator)) "K70syslogd")) "")
   (test "path" (unix-path->list ".") '("."))
   (test "path" (unix-path->list (string-append "/" (string (path-separator))
				    "." (string (path-separator))
				    "/usr/local"))
      '("/" "." "/usr/local"))
   (test "string-compare3.1" (< (string-compare3 "abc" "def") 0) #t)
   (test "string-compare3.2" (> (string-compare3 "def" "abc") 0) #t)
   (test "string-compare3.3" (= (string-compare3 "def" "abc") 0) #f)
   (test "string-compare3.4" (= (string-compare3 "def" "def") 0) #t)
   (test "string-compare3.5" (< (string-compare3 "abc" "abci") 0) #t)
   (test "string-compare3.6" (< (string-compare3 "abc" "abd") 0) #t)
   (test "string-compare3.7" (> (string-compare3 "abci" "abc") 0) #t)
   (test "string-compare3.8" (> (string-compare3 "abd" "abc") 0) #t)
   (test "string-compare3.8" (> (string-compare3 "abd" "abc") 0) #t)
   (test "string-compare3-ci.1" (< (string-compare3-ci "abc" "DeF") 0) #t)
   (test "string-compare3-ci.2" (> (string-compare3-ci "def" "ABC") 0) #t)
   (test "string-compare3-ci.3" (= (string-compare3-ci "def" "ABC") 0) #f)
   (test "string-compare3-ci.4" (= (string-compare3-ci "def" "dEF") 0) #t)
   (test "string-compare3-ci.5" (< (string-compare3-ci "ABC" "AbCi") 0) #t)
   (test "string-compare3-ci.6" (< (string-compare3-ci "abc" "ABD") 0) #t)
   (test "string-compare3-ci.7" (> (string-compare3-ci "ABCI" "ABC") 0) #t)
   (test "string-compare3-ci.8" (> (string-compare3-ci "aBd" "abc") 0) #t)
   (test "string/bstring" (string/bstring) "ascf\n")
   (test "output-string-port"
      (let ((p (open-output-string)))
	 (display (make-string 129 #\a) p)
	 (flush-output-port p)
	 (display (make-string 129 #\b) p)
	 (flush-output-port p)
	 (display (make-string 129 #\c) p)
	 (close-output-port p))
      (string-append
	 (make-string 129 #\a)
	 (make-string 129 #\b)
	 (make-string 129 #\c)))
   (let ((s "abcdef")) 
      (test "base64.1" (base64-decode (base64-encode s)) s)
      (test "base64.1b" (base64-decode (base64-encode s 0)) s)
      (test "base64.2" (string=? (base64-encode s) s) #f))
   (let ((s "abcdefg"))
      (test "base64.3" (base64-decode (base64-encode s)) s)
      (test "base64.3b" (base64-decode (base64-encode s 0)) s))
   (let ((s "abcdefgh"))
      (test "base64.4" (base64-decode (base64-encode s)) s)
      (test "base64.4b" (base64-decode (base64-encode s 0)) s))
   (let ((s "Man is distinguished, not only by his reason, but by this singular passion from other animals, which is a lust of the mind, that by a perseverance of delight in the continued and indefatigable generation of knowledge, exceeds the short vehemence of any carnal pleasure."))
      (test "base64.5" (base64-encode s) "TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlz\nIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2Yg\ndGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGlu\ndWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRo\nZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4=")
      (test "base64.6" (base64-decode (base64-encode s)) s)
      (test "base64.6b" (base64-decode (base64-encode s 0)) s)
      (let ((ip (open-input-string s))
	    (op (open-output-string)))
	 (test "base64.7"
	    (begin
	       (base64-encode-port ip op)
	       (close-output-port op))
	    (base64-encode s)))
      (let ((ip (open-input-string s))
	    (op (open-output-string)))
	 (test "base64.8"
	    (begin
	       (base64-encode-port ip op)
	       (let* ((ip (open-input-string (close-output-port op)))
		      (op (open-output-string)))
		  (base64-decode-port ip op)
		  (close-output-port op)))
	    s))
      (let ((ip (open-input-string s))
	    (op (open-output-string)))
	 (test "base64.9"
	    (begin
	       (base64-encode-port ip op)
	       (let* ((ip (open-input-string (close-output-port op)))
		      (s (read-string ip)))
		  (base64-decode s)))
	    s))
      (let* ((base64encode (lambda (obj)
			      (base64-encode (obj->string obj))))
	     (base64unencode (lambda (obj)
				(string->obj (base64-decode obj))))
	     (a (list->vector (iota 20 1 1)))
	     (b (base64encode a))
	     (c (base64unencode b)))
	 (test "base64.10" a c)))
   (test "hex-string.1" (string-hex-extern "AAA") "414141")
   (test "hex-string.2" (string-hex-intern "414141") "AAA")
   (test "hex-string.3" (string-hex-intern! (string-copy "414141")) "AAA")
   (test "hex-string.4" (string-hex-intern (string-hex-extern "foo bar0x12"))
      "foo bar0x12")
   (test "md5.1" (md5sum "") "d41d8cd98f00b204e9800998ecf8427e")
   (test "md5.2" (md5sum "a") "0cc175b9c0f1b6a831c399e269772661")
   (test "md5.3" (md5sum "abc") "900150983cd24fb0d6963f7d28e17f72")
   (test "md5.4" (md5sum "message digest") "f96b697d7cb7938d525a2f31aaf161d0")
   (test "md5.5" (md5sum "abcdefghijklmnopqrstuvwxyz") "c3fcd3d76192e4007dfb496cca67e13b")
   (test "md5.6" (md5sum "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789") "d174ab98d277d9f5a5611c2c9f419d9f")
   (test "md5.7" (md5sum "12345678901234567890123456789012345678901234567890123456789012345678901234567890") "57edf4a22be3c955ac49da2e2107b67a")
   (test "hmac-md5.1" (hmac-md5sum-string (make-string 16 #a011)
			 "Hi There")
      "9294727a3638bb1c13f48ef8158bfc9d")
   (test "hmac-md5.2" (hmac-md5sum-string "Jefe"
			 "what do ya want for nothing?")
      "750c783e6ab0b503eaa86e310a5db738")
   (test "hmac-md5.3" (hmac-md5sum-string (make-string 16 #a170)
			 (make-string 50 #a221))
      "56be34521d144c88dbb8c733f0e8b3f6")
   (test "hmac-md5.4" (hmac-md5sum-string (string-hex-intern
					     "0102030405060708090a0b0c0d0e0f10111213141516171819")
			 (make-string 50 #a205))
      "697eaf0aca3a3aea3a75164746ffaa79")
   (test-sha1 1
      "abc"
      "a9993e364706816aba3e25717850c26c9cd0d89d")
   (test-sha1 2
      "The quick brown fox jumps over the lazy dog"
      "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")
   (test-sha1 3
      "The quick brown fox jumps over the lazy cog"
      "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3")
   (test-sha1 4
      (make-string 63 #\a)
      "03f09f5b158a7a8cdad920bddc29b81c18a551f5")
   (test-sha1 5
      (make-string 64 #\a)
      "0098ba824b5c16427bd7a1122a5a442a25ec644d")
   (test-sha1 6
      (make-string 65 #\a)
      "11655326c708d70319be2610e8a57d9a5b959d3b")
   (test-sha1 7
      (make-string 127 #\a)
      "89d95fa32ed44a7c610b7ee38517ddf57e0bb975")
   (test-sha1 8
      (make-string 128 #\a)
      "ad5b3fdbcb526778c2839d2f151ea753995e26a0")
   (test-sha1 9
      (make-string 129 #\a)
      "d96debf1bdcbc896e6c134ea76e8141f40d78536")
   (test-sha1 10
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      "84983e441c3bd26ebaae4aa1f95129e5e54670f1")
   (test-sha1 11
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnop"
      "47b172810795699fe739197d1a1f5960700242f1")
   (test-sha1 12
      "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopqr"
      "e4690e96180cb89fdd79a3ba0f2a741224a50e62")
   (test-sha1 13
      ""
      "da39a3ee5e6b4b0d3255bfef95601890afd80709")
   (test-sha1 14
      "The quick brown fox jumps over the lazy dog"
      "2fd4e1c67a2d28fced849ee1bb76e7391b93eb12")
   (test-sha1 15
      "The quick brown fox jumps over the lazy cog"
      "de9f2c7fd25e1b3afad3e85a0bd17d9b100db4b3")
   (test-sha256 1
      "abc"
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
   (test-sha256 2
      "The quick brown fox jumps over the lazy dog"
      "d7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592")
   (test-sha256 3
      "The quick brown fox jumps over the lazy cog"
      "e4c4d8f3bf76b692de791a173e05321150f7a345b46484fe427f6acc7ecc81be")
   (test-sha256 4
      (make-string 63 #\a)
      "7d3e74a05d7db15bce4ad9ec0658ea98e3f06eeecf16b4c6fff2da457ddc2f34")
   (test-sha256 5
      (make-string 64 #\a)
      "ffe054fe7ae0cb6dc65c3af9b61d5209f439851db43d0ba5997337df154668eb")
   (test-sha256 6
      (make-string 65 #\a)
      "635361c48bb9eab14198e76ea8ab7f1a41685d6ad62aa9146d301d4f17eb0ae0")
   (test-sha256 7
      (make-string 127 #\a)
      "c57e9278af78fa3cab38667bef4ce29d783787a2f731d4e12200270f0c32320a")
   (test-sha256 8
      (make-string 128 #\a)
      "6836cf13bac400e9105071cd6af47084dfacad4e5e302c94bfed24e013afb73e")
   (test-sha256 9
      (make-string 129 #\a)
      "c12cb024a2e5551cca0e08fce8f1c5e314555cc3fef6329ee994a3db752166ae")
   (test-sha256 10
      (make-string 56 #\a)
      "b35439a4ac6f0948b6d6f9e3c6af0f5f590ce20f1bde7090ef7970686ec6738a")
   (test-sha256 11
      (make-string 57 #\a)
      "f13b2d724659eb3bf47f2dd6af1accc87b81f09f59f2b75e5c0bed6589dfe8c6")
   (test-sha256 12
      (make-string 58 #\a)
      "d5c039b748aa64665782974ec3dc3025c042edf54dcdc2b5de31385b094cb678")
   (test-sha256 13
      (make-string 59 #\a)
      "111bb261277afd65f0744b247cd3e47d386d71563d0ed995517807d5ebd4fba3")
   (test-sha256 14
      (make-string 60 #\a)
      "11ee391211c6256460b6ed375957fadd8061cafbb31daf967db875aebd5aaad4")
   (test-sha256 15
      (make-string 1 #\a)
      "ca978112ca1bbdcafac231b39a23dc4da786eff8147c4e72b9807785afee48bb")
   (test-sha256 16
      (make-string 16 #\a)
      "0c0beacef8877bbf2416eb00f2b5dc96354e26dd1df5517320459b1236860f8c")
   (test-sha256 17
      (make-string 17 #\a)
      "b860666ee2966dd8f903be44ee605c6e1366f926d9f17a8f49937d11624eb99d")
   (test-sha256-2)
   (test "string-prefix-length.1"
      (string-prefix-length "abcde" "abcdef") 5)
   (test "string-prefix-length.2"
      (string-prefix-length "0abcde" "abcdef" 1) 5)
   (test "string-prefix-length.3"
      (string-prefix-length "0abcde" "abcdef" 1 3) 2)
   (test "string-prefix-length.4"
      (string-prefix-length "0abcde" "aabcdef" 1 3 1) 2)
   (test "string-prefix-length.5"
      (string-prefix-length "0abcde" "aabcdef" 1 4 1 3) 2)
   (test "string-suffix-length.1"
      (string-suffix-length "abcde" "fabcde") 5)
   (test "string-suffix-length.2"
      (string-suffix-length "0abcde" "fabcde" 1) 5)
   (test "string-suffix-length.3"
      (string-suffix-length "0cdef" "fabcde" 1 4) 3)
   (test "string-suffix-length.4"
      (string-suffix-length "0cdef" "aabcde" 1 4 1) 3)
   (test "string-suffix-length.5"
      (string-suffix-length "0abcde" "aaabcde" 1 3 2 4) 2)
   (test "string-suffix-length.6"
      (string-suffix-length "ab" "-") 0)
   (test "string-prefix-length-ci.1"
      (string-prefix-length-ci "AbCde" "abcdef") 5)
   (test "string-prefix-length-ci.2"
      (string-prefix-length-ci "0ABCDE" "abcdef" 1) 5)
   (test "string-prefix?.1"
      (string-prefix? "abc" "abcde") #t)
   (test "string-prefix?.2"
      (string-prefix? "abcf" "abcde") #f)
   (test "string-prefix?.3"
      (string-prefix? "0abc" "abcde" 1) #t)
   (test "string-suffix?.3b"
      (string-suffix? "0CDE" "abcde" 1) #f)
   (test "string-prefix?.4"
      (string-prefix? "0Abc" "abcde" 1) #f)
   (test "string-prefix?.5"
      (string-prefix? "0abcf" "abcde" 1 4) #t)
   (test "string-prefix?.6"
      (string-prefix? "0abcf" "abcde" 1 5) #f)
   (test "string-prefix?.7"
      (string-prefix? "0abcf" "0abcde" 1 4 1) #t)
   (test "string-prefix?.8"
      (string-prefix? "0abcf" "0abcde" 1 4 2) #f)
   (test "string-prefix?.9"
      (string-prefix? "0abcf" "00abcde" 1 4 2 5) #t)
   (test "string-prefix?.10"
      (string-prefix? "0abcf" "0abcde" 1 4 2 2) #f)
   (test "string-prefix-ci?.1"
      (string-prefix-ci? "abc" "ABCDE") #t)
   (test "string-prefix-ci?.2"
      (string-prefix-ci? "abcf" "ABCDE") #f)
   (test "string-prefix-ci?.3"
      (string-prefix-ci? "0abc" "ABCDE" 1) #t)
   (test "string-suffix?.1"
      (string-suffix? "cde" "abcde") #t)
   (test "string-suffix?.2"
      (string-suffix? "cdef" "abcde") #f)
   (test "string-suffix?.3"
      (string-suffix? "0cde" "abcde" 1) #t)
   (test "string-suffix?.4"
      (string-suffix? "0cdf" "abcde" 1) #f)
   (test "string-suffix?.5"
      (string-suffix? "0cdef" "abcde" 1 4) #t)
   (test "string-suffix?.6"
      (string-suffix? "0abcf" "abcde" 1 5) #f)
   (test "string-suffix?.7"
      (string-suffix? "0cdef" "0abcde" 1 4 1) #t)
   (test "string-suffix?.8"
      (string-suffix? "0abcf" "0abcde" 1 4 2) #f)
   (test "string-suffix?.9"
      (string-suffix? "0cdef" "0abcde" 1 4 2 6) #t)
   (test "string-suffix?.10"
      (string-suffix? "0abcf" "0abcde" 1 4 2 2) #f)
   (test "string-suffix?.11"
      (string-suffix? "-" "ab") #f)
   (test "string-suffix-ci?.1"
      (string-suffix-ci? "CDE" "abcde") #t)
   (test "string-suffix-ci?.2"
      (string-suffix-ci? "cdef" "ABCDE") #f)
   (test "string-suffix-ci?.3"
      (string-suffix-ci? "0CDE" "abcde" 1) #t)
   (test "string-index.1" (string-index "foobar-gee" #\-) 6)
   (test "string-index.2" (string-index "foobar-gee" "-") 6)
   (test "string-index.3" (string-index "foobar-gee" "-g") 6)
   (test "string-index.4" (string-index "foobar-gee" "-r") 5)
   (test "string-index.4" (string-index "foobar-gee" "r-") 5)
   (test "string-index.5" (string-index "foobar-ge-e" "-") 6)
   (test "string-index.6" (string-index "foobar-ge-e" "-" 7) 9)
   (test "string-index.7" (string-index "foobar-ge-e" "_") #f)
   (test "string-index.8" (string-index "foobar-ge-e" "_" 7) #f)
   (test "string-index-right.1" (string-index-right "foobar-gee" "-") 6)
   (test "string-index-right.2" (string-index-right "foobar-ge-e" "-") 9)
   (test "string-index-right.3" (string-index-right "foobar-ge-e" "-" 7) 6)
   (test "string-index-right.4" (string-index-right "foobar-ge-e" "_") #f)
   (test "string-index-right.5" (string-index-right "foobar-ge-e" "_" 7) #f)
   (test "string-index-right.6" (string-index-right "true" #\t 1) 0)
   (test "string-index-right.7" (string-index-right "true" #\t 0) #f)
   (test "string-skip.1" (string-skip "foobar-gee" #\f) 1)
   (test "string-skip.2" (string-skip "foobar-gee" "f") 1)
   (test "string-skip.3" (string-skip "foobar-gee" "fo") 3)
   (test "string-skip.4" (string-skip "foobar-gee" "fofofofofofofofofofo") 3)
   (test "string-skip.5" (string-skip "foobar-gee" "of") 3)
   (test "string-skip.6" (string-skip "foobar-gee" "ofofofofofofofofof") 3)
   (test "string-skip.7" (string-skip "foobar-ge-e" "_") 0)
   (test "string-skip.8" (string-skip "foobar-ge-e" "_" 7) 7)
   (test "string-skip.9" (string-skip "foobar-gee" "fobar-ge") #f)
   (test "string-skip.10" (string-skip "1023:00" "0123456789") 4)
   (test "string-skip.10" (string-skip "1023:00" "+-") 0)
   (test "string-skip.10" (string-skip "1023:00" "0123456789abklmnopqrstu") 4)
   (test "string-skip-right.1" (string-skip-right "foobar-gee" #\e) 7)
   (test "string-skip-right.2" (string-skip-right "foobar-gee" "e") 7)
   (test "string-skip-right.3" (string-skip-right "foobar-gee" "ge") 6)
   (test "string-skip-right.4" (string-skip-right "foobar-gee" "gegegegegegegegegege") 6)
   (test "string-skip-right.5" (string-skip-right "foobar-gee" "eg") 6)
   (test "string-skip-right.6" (string-skip-right "foobar-gee" "egegegegegegegegeg") 6)
   (test "string-skip-right.7" (string-skip-right "foobar-ge-e" "_") 10)
   (test "string-skip-right.8" (string-skip-right "foobar-ge-e" "_" 7) 6)
   (test "string-skip-right.9" (string-skip-right "foobar-gee" "fobar-ge") #f)
   (test "string-natural-compare3.1" (string-natural-compare3 "foo" "foo") 0)
   (test "string-natural-compare3.2"
      (string-natural-compare3 "foo0" "foo1") -1)
   (test "string-natural-compare3.3" (string-natural-compare3 "foo1" "foo0") 1)
   (test "string-natural<?.1" (string-natural<? "rfc1.txt" "rfc822.txt") #t)
   (test "string-natural<?.1b" (string-natural<? "rfc822.txt" "rfc1.txt") #f)
   (test "string-natural<?.2" (string-natural<? "rfc1.txt" "rfc2086.txt") #t)
   (test "string-natural<?.2b" (string-natural<? "rfc2086.txt" "rfc1.txt") #f)
   (test "string-natural<?.3" (string-natural<? "rfc822.txt" "rfc2086.txt") #t)
   (test "string-natural<?.3b" (string-natural<? "rfc2086.txt" "rfc822.txt") #f)
   (test "string-natural-compare3.4" (string-natural-compare3 "a0" "a1") -1)
   (test "string-natural-compare3.5" (string-natural-compare3 "a1" "a1a") -1)
   (test "string-natural-compare3.6" (string-natural-compare3 "a1a" "a1b") -1)
   (test "string-natural-compare3.7" (string-natural-compare3 "a1b" "a2") -1)
   (test "string-natural-compare3.8" (string-natural-compare3 "a2" "a10") -1)
   (test "string-natural-compare3.9" (string-natural-compare3 "a10" "a20") -1)
   (test "string-natural-compare3.10" (string-natural-compare3 "a2" "a20") -1)
   (test "string-natural<?.4" (string-natural<? "x2-g8" "x2-y7") #t)
   (test "string-natural<?.4b" (string-natural<? "x2-y7" "x2-y08") #f)
   (test "string-natural<?.4c" (string-natural<? "x2-y08" "x8-y8") #t)
   (test "string-natural<?.5" (string-natural<? "1.001" "1.002") #t)
   (test "string-natural<?.6" (string-natural<? "1.002" "1.010") #t)
   (test "string-natural<?.7" (string-natural<? "1.010"  "1.02") #f)
   (test "string-natural<?.8" (string-natural<? "1.02" "1.1") #t)
   (test "string-natural<?.9" (string-natural<? "1.1" "1.02") #f)
   (test "string-natural<?.10" (string-natural<? "1.02" "1.3") #t)
   (test "string-natural-compare3-ci.1"
      (string-natural-compare3-ci "foo" "foo") 0)
   (test "string-natural-compare3-ci.2"
      (string-natural-compare3-ci "foo" "Foo") 0)
   (test "string-natural-compare3-ci.3"
      (string-natural-compare3-ci "foo1" "foo0") 1)
   (test "string-natural-compare3-ci.4"
      (string-natural-compare3-ci "foo1" "FOO0") 1)
   (test "string-natural-compare3-ci.5"
      (string-natural-compare3-ci "FOO1" "foo0") 1) substring
   (test "number->string.1" (number->string 10) "10")
   (test "number->string.2" (number->string 10 16) "a")
   (test "number->string.3" (number->string 10 2) "1010")
   (test "integer->string/padding.1" (integer->string/padding 9 3) "009")
   (test "integer->string/padding.2" (integer->string/padding 19 3) "019")
   (test "integer->string/padding.3" (integer->string/padding 192 3) "192")
   (test "integer->string/padding.4" (integer->string/padding -9 3) "-09")
   (test "integer->string/padding.5" (integer->string/padding -19 3) "-19")
   (test "string-contains.1" (string-contains "foo bar" "bar") 4)
   (test "string-contains.2" (string-contains "foo bar" "bar" 1) 4)
   (test "string-contains.3" (string-contains "foo bar" "bar" 10) #f)
   (test "string-contains.4" (string-contains "foo bar" "bar" -1) 4)
   (test "string-contains-ci.1" (string-contains-ci "FOO BAR" "bar" 1) 4)
   (test "string-contains-ci.2" (string-contains-ci "FOO BAR" "bar" 1) 4)
   (test "string-contains-ci.3" (string-contains-ci "foo bar" "bar" 10) #f)
   (test "string-contains-ci.4" (string-contains-ci "foo bar" "bar" -1) 4)
   (test "string-as-read" (string-as-read (string #\\ #\n #\o #\\ #\t))
      (string #\newline #\o #\tab))
   (test "string-for-read" (string-for-read (string #\newline #\o #\tab))
      (string #\\ #\n #\o #\\ #\t))
   (test "string-delete.1" (string-delete "abcedfghij" "fgh")
      "abcedij")
   (test "string-delete.2" (string-delete "abcedfghij" "fgh" 6)
      "ij")
   (test "string-delete.3" (string-delete "abcedfghij" #\b 1 5)
      "ced")
   (test "string-delete.4" (string-delete "abcedfghij" (lambda (c) #t) 7)
      "")
   (test "strint-char-index.1" (string-char-index "abcdef" #\f) 5)
   (test "strint-char-index.2" (string-char-index "abcdef" #\f 5 1) 5)
   (test "strint-char-index.3" (string-char-index "abcdef" #\f 4 2) 5)
   (test "strint-char-index.4" (string-char-index "abcdef" #\f 4 1) #f)
   (test "strint-char-index.5" (string-char-index "abcdef" #\f 4 2) 5)
   (test "strint-char-index.6" (string-char-index "abcdef" #\e) 4)
   (test "strint-char-index.7" (string-char-index "abcdef" #\e 4 1) 4)
   (test "strint-char-index.8" (string-char-index "abcdef" #\e 4 2) 4)
   (test "strint-char-index.9" (string-char-index "abcdef" #\e 3 1) #f)
   (test "strint-char-index.10" (string-char-index "abcdef" #\e 3 3) 4))
