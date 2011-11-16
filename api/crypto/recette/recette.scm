;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/crypto/recette/recette.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Tue Nov 15 13:59:05 2011 (serrano)                */
;*    Copyright   :  2002-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of the Crypto-library.   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   (library crypto)
   (main    main))

;*---------------------------------------------------------------------*/
;*    err ...                                                          */
;*---------------------------------------------------------------------*/
(define (err . msg)
   (with-output-to-port (current-error-port)
      (lambda ()
	 (for-each write msg)
	 (newline))))

;*---------------------------------------------------------------------*/
;*    do-something-else ...                                            */
;*---------------------------------------------------------------------*/
(define (do-something-else)
   #unspecified)

;*---------------------------------------------------------------------*/
;*    *tests* ...                                                      */
;*---------------------------------------------------------------------*/
(define *tests* '())

;*---------------------------------------------------------------------*/
;*    *failure* and *success* ...                                      */
;*---------------------------------------------------------------------*/
(define *failure* '())
(define *success* 0)

;*---------------------------------------------------------------------*/
;*    test ...                                                         */
;*---------------------------------------------------------------------*/
(define (test name prgm::procedure res)
   (display* name "...")
   (flush-output-port (current-output-port))
   (let ((provided (with-handler
		      (lambda (e)
			 (error-notify e)
			 (vector res))
		      (prgm))))
      (if (or (eq? res #unspecified)
	      (and (procedure? res) (res provided))
	      (equal? res provided))
	  (begin
	     (set! *success* (+fx 1 *success*))
	     (print "ok."))
	  (begin
	     (set! *failure* (cons name *failure*))
	     (print "error.")
	     (print "   ==> provided: [" (with-output-to-string
					    (lambda () (write provided)))
		    "]\n       expected: ["
		    (let ((r (if (procedure? res) (res 'result) res)))
		       (with-output-to-string
			  (lambda () (write r))))
		    "]")))))

;*---------------------------------------------------------------------*/
;*    test-add! ...                                                    */
;*---------------------------------------------------------------------*/
(define (test-add! id fun res)
   (set! *tests* (cons (list id fun res) *tests*)))

;*---------------------------------------------------------------------*/
;*    define-test ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (define-test id prgm . rest)
   (let ((t (match-case rest
	       ((:result ?result)
		`(list ',id (lambda () ,prgm) ,result))
	       (()
		`(list ',id (lambda () ,prgm) #unspecified))
	       (else
		(error "define-test" "Illegal rest argument" rest)))))
      `(set! *tests* (cons ,t *tests*))))


(define (id x) x)
;*---------------------------------------------------------------------*/
;*    AES ...                                                          */
;*---------------------------------------------------------------------*/
(define (aes-encrypt in key) (encrypt 'aes in key :mode 'ecb :string->key id))
(define (aes-decrypt in key) (decrypt 'aes in key :mode 'ecb :string->key id))
(define *aes-input*  (string-hex-intern "3243F6A8885A308D313198A2E0370734"))
(define *aes-key*    (string-hex-intern "2B7E151628AED2A6ABF7158809CF4F3C"))
(define *aes-output* (string-hex-intern "3925841D02DC09FBDC118597196A0B32")) 

(test-add! 'aes-encrypt
	   (lambda () (string-hex-extern (aes-encrypt *aes-input* *aes-key*)))
	   (string-hex-extern *aes-output*))

(test-add! 'aes-decrypt
	   (lambda () (string-hex-extern (aes-decrypt *aes-output* *aes-key*)))
	   (string-hex-extern *aes-input*))


;*---------------------------------------------------------------------*/
;*    CAST-128 ...                                                     */
;*---------------------------------------------------------------------*/
(define (cast-128-encrypt-string str key #!key (IV #f) (mode 'ecb))
   (encrypt 'cast-128 str key :mode mode :IV IV :string->key id))
(define (cast-128-decrypt-string str key #!key (IV #f) (mode 'ecb))
   (decrypt 'cast-128 str key :mode mode :IV IV :string->key id))

(define (cast-128-very-simple-test)
   (let* ((str1 "abcdefgh")
	  (str2 "12345678")
	  (str2b (string-copy str2))
	  (str3 "aoeuhtsn',.pgcl"))
      (string=? str1 (cast-128-decrypt-string
			(cast-128-encrypt-string str1 str3 )
			str3
			))))

(define (cast-128-very-simple-test2)
   (let* ((str1 "abcdefgh")
	  (str2 "12345678")
	  (str2b (string-copy str2))
	  (str3 "aoeuhtsn',.pgcl"))
      (string=? str1 (cast-128-decrypt-string
		      (cast-128-encrypt-string  str1 str3
						:mode 'cfb
						:IV str2)
		      str3
		      :mode 'cfb
		      :IV str2))))

(define (cast-128-simple-test)
   (define (bits-128-test)
      (let* ((key-ns '(#x01 #x23 #x45 #x67 #x12 #x34 #x56 #x78
		       #x23 #x45 #x67 #x89 #x34 #x56 #x78 #x9A))
	     (plain-ns '(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF))
	     (cipher-ns '(#x23 #x8B #x4F #xE5 #x84 #x7E #x44 #xB2))
	     (key (apply string (map integer->char-ur key-ns)))
	     (plain (apply string (map integer->char-ur plain-ns)))
	     (cipher (apply string (map integer->char-ur cipher-ns))))
	 (string=? (cast-128-encrypt-string plain key )
		   cipher)))

   (define (bits-80-test)
      (let* ((key-ns '(#x01 #x23 #x45 #x67 #x12 #x34 #x56 #x78 #x23 #x45))
	     (plain-ns '(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF))
	     (cipher-ns '(#xEB #x6A #x71 #x1A #x2C #x02 #x27 #x1B))
	     (key (apply string (map integer->char-ur key-ns)))
	     (plain (apply string (map integer->char-ur plain-ns)))
	     (cipher (apply string (map integer->char-ur cipher-ns))))
	 (string=? (cast-128-encrypt-string plain key )
		   cipher)))

   (define (bits-40-test)
      (let* ((key-ns '(#x01 #x23 #x45 #x67 #x12))
	     (plain-ns '(#x01 #x23 #x45 #x67 #x89 #xAB #xCD #xEF))
	     (cipher-ns '(#x7A #xC8 #x16 #xD1 #x6E #x9B #x30 #x2E))
	     (key (apply string (map integer->char-ur key-ns)))
	     (plain (apply string (map integer->char-ur plain-ns)))
	     (cipher (apply string (map integer->char-ur cipher-ns))))
	 (string=? (cast-128-encrypt-string plain key )
		   cipher)))
   (and (bits-128-test)
	(bits-80-test)
	(bits-40-test)))
	 
;; maintenance test following rfc2144, B.2.
(define (cast-128-maintenance-test)
   (let* ((initial '(#x01 #x23 #x45 #x67 #x12 #x34 #x56 #x78
		     #x23 #x45 #x67 #x89 #x34 #x56 #x78 #x9A))
	  (verify-a-ns '(#xEE #xA9 #xD0 #xA2 #x49 #xFD #x3B #xA6
			 #xB3 #x43 #x6F #xB8 #x9D #x6D #xCA #x92))
	  (verify-b-ns '(#xB2 #xC9 #x5E #xB0 #x0C #x31 #xAD #x71
			 #x80 #xAC #x05 #xB8 #xE8 #x3D #x69 #x6E))
	  (verify-a (apply string (map integer->char-ur verify-a-ns)))
	  (verify-b (apply string (map integer->char-ur verify-b-ns)))
	  (a (apply string (map integer->char-ur initial)))
	  (b (string-copy a)))
      (let loop ((i 0))
	 (if (=fx i 1000000)
	     (and (string=? a verify-a)
		  (string=? b verify-b))
	     (begin
		(when (zerofx? (remainderfx i 10000))
		   (display ".")
		   (flush-output-port (current-output-port)))
		(set! a (cast-128-encrypt-string a b ))
		(set! b (cast-128-encrypt-string b a ))
		(loop (+fx i 1)))))))
      
(test-add! 'cast-128-very-simple cast-128-very-simple-test #t)
(test-add! 'cast-128-very-simple2 cast-128-very-simple-test2 #t)
(test-add! 'cast-128-simple cast-128-simple-test #t)
(test-add! 'cast-128-maintenance cast-128-maintenance-test #t)

;*---------------------------------------------------------------------*/
;*    DES ...                                                          */
;*---------------------------------------------------------------------*/
(define (des-encrypt str key)
   (encrypt 'des str key :mode 'ecb :string->key id))
(define (des-decrypt str key)
   (decrypt 'des str key :mode 'ecb :string->key id))

(define *des-tests*
   ;; input             key                                output
   '(("0102030405060708" "3b3898371520f75e" "0c65f4417149d98b")
     ("0000000100020003" "0102030405060708" "9C4ECDF5DE21C22C")
     ("E648DC3184BE9685" "7DAD129695BD3E2B" "DD2B4EAEB6975932")
     ("0000000000000000" "0000000000000000" "8CA64DE9C1B123A7")
     ("DC93A3B73D9E0E54" "BE2F512A294350C7" "975AF924F05821AE")
     ("0000000100020003" "01030405060708" "4FC52B2C37C0BF88")
     ("E648DC3184BE9685" "7DA29695BD3E2B" "7852FFF9A544E96F")
     ("0000000000000000" "00000000000000" "8CA64DE9C1B123A7")
     ("DC93A3B73D9E0E54" "BE2F5A294350C7" "C4B0DC94A78337A1")
     ))
     
(for-each (lambda (t i)
	     (let* ((input  (string-hex-intern (car t)))
		    (key    (string-hex-intern (cadr t)))
		    (output (string-hex-intern (caddr t))))
		(test-add! (string->symbol (format "des-encrypt-~a" i))
			   (lambda ()
			      (string-hex-extern (des-encrypt input key)))
			   (string-hex-extern output))
		(test-add! (string->symbol (format "des-decrypt-~a" i))
			   (lambda ()
			      (string-hex-extern (des-decrypt output key)))
			   (string-hex-extern input))))
	  *des-tests*
	  (iota (length *des-tests*)))


(test-add! 'des-maintenance-test
	   (lambda ()
	      ;; DES maintenance test. copied from des.c of gnupg.
	      (let ((input (string-hex-intern "FFFFFFFFFFFFFFFF"))
		    (key (string-hex-intern "5555555555555555"))
		    (result (string-hex-intern "246E9DB9C550381A")))
		 (let loop ((i 0)
			    (input input)
			    (key key))
		    (if (=fx i 64)
			(string=? result key)
			(let* ((temp1 (des-encrypt input key))
			       (temp2 (des-encrypt temp1 key))
			       (temp3 (des-decrypt temp1 temp2)))
			   (loop (+fx i 1)
				 temp1
				 temp3))))))
	   #t)

(test-add! 'des-np
	   (lambda ()
	      (let* ((input (string-hex-intern "E648DC3184BE9685"))
		     (key (string-hex-intern "7DAD129695BD3E2B"))
		     (encrypted (encrypt 'des-np input key :mode 'ecb
					 :string->key id))
		     (decrypted (decrypt 'des-np encrypted key :mode 'ecb
					 :string->key id)))
		 (string=? input decrypted)))
	   #t)

;*---------------------------------------------------------------------*/
;*    DES3 ...                                                         */
;*---------------------------------------------------------------------*/
(define (des3-encrypt str key)
   (encrypt 'des3 str key :mode 'ecb :string->key id))
(define (des3-decrypt str key)
   (decrypt 'des3 str key :mode 'ecb :string->key id))

;; copied from des.c of gnupg.
(define *des3-tests*
   '(
     ("95F8A5E5DD31D900"  ;; input
      "010101010101010101010101010101010101010101010101"  ;; key
      "8000000000000000")  ;; output
     ("9D64555A9A10B852"  ;; input
      "010101010101010101010101010101010101010101010101"  ;; key
      "0000001000000000") ;; output
     ("51454B582DDF440A"
      "3849674C2602319E3849674C2602319E3849674C2602319E"
      "7178876E01F19B2A")
     ("42FD443059577FA2"
      "04B915BA43FEB5B604B915BA43FEB5B604B915BA43FEB5B6"
      "AF37FB421F8C4095")
     ("736F6D6564617461"
      "0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF"
      "3D124FE2198BA318")
     ("736F6D6564617461"
      "0123456789ABCDEF55555555555555550123456789ABCDEF"
      "FBABA1FF9D05E9B1")
     ("736F6D6564617461"
      "0123456789ABCDEF5555555555555555FEDCBA9876543210"
      "18d748e563620572")
     ("7371756967676C65"
      "0352020767208217860287665908219864056ABDFEA93457"
      "c07d2a0fa566fa30")
     ("0000000000000000"
      "010101010101010180010101010101010101010101010102"
      "e6e6dd5b7e722974")
     ("0000000000000000"
      "10461034899880209107D0158919010119079210981A0101"
      "e1ef62c332fe825b")
     ))

(for-each (lambda (t i)
	     (let* ((input  (string-hex-intern (car t)))
		    (key    (string-hex-intern (cadr t)))
		    (output (string-hex-intern (caddr t))))
		(test-add! (string->symbol (format "des3-encrypt-~a" i))
			   (lambda ()
			      (string-hex-extern (des3-encrypt input key)))
			   (string-hex-extern output))
		(test-add! (string->symbol (format "des3-decrypt-~a" i))
			   (lambda ()
			      (string-hex-extern (des3-decrypt output key)))
			   (string-hex-extern input))))
	  *des3-tests*
	  (iota (length *des3-tests*)))


(test-add! 'des3-maintenance-test
	   (lambda ()
	      ;; DES3 maintenance test. copied from des.c of gnupg.
	      (let ((input (string-hex-intern "FEDCBA9876543210"))
		    (key1 (string-hex-intern "123456789ABCDEF0"))
		    (key2 (string-hex-intern "11223344FFAACCDD"))
		    (result (string-hex-intern "7B383B23A27D26D3")))
		 (let loop ((i 0)
			    (input input)
			    (key1 key1)
			    (key2 key2))
		    (if (=fx i 16)
			(string=? input result)
			(let* ((temp1 (des3-encrypt input
						    (string-append key1 key2)))
			       (temp2 (des3-decrypt input
						    (string-append key1 key2)))
			       (temp3 (des3-encrypt input
						    (string-append temp1
								   input
								   temp2))))
			   (loop (+fx i 1)
				 temp3
				 temp1
				 temp2))))))
	   #t)

(test-add! 'des3-np
	   (lambda ()
	      (let* ((input (string-hex-intern "FEDCBA9876543210"))
		     (key (string-hex-intern
			   "123456789ABCDEF011223344FFAACCDD7B383B23A27D26D3"))
		     (encrypted (encrypt 'des3-np input key :mode 'ecb
					 :string->key id))
		     (decrypted (decrypt 'des3-np encrypted key :mode 'ecb
					 :string->key id)))
		 (string=? input decrypted)))
	   #t)

;*---------------------------------------------------------------------*/
;*    IDEA ...                                                         */
;*---------------------------------------------------------------------*/
(define (idea-encrypt input key)
   (encrypt 'idea input key :mode 'ecb :string->key id))
(define (idea-decrypt input key)
   (decrypt 'idea input key :mode 'ecb :string->key id))

(define *idea-tests*
   ;; input             key                                output
   '(
     ("0000000100020003" "00010002000300040005000600070008" "11FBED2B01986DE5")
     ("E648DC3184BE9685" "7DAD12E916699255B31EC6FDDDBD3E2B" "A6C44967EBD4F76B")
     ("0000000000000000" "00000000000000000000000000000000" "0001000100000000")
     ("DC93A3B73D9E0E54" "BE24B6F95D1528A30279B483D522B0C7" "F1978292C10E4D01")
     ("442C8B1EEBAB9629" "5756C156AB997BF55D5CC7F3D0C9313C" "7FB0B79F3371F875")
     ))
     
(for-each (lambda (test i)
	     (let ((input  (string-hex-intern (car test)))
		   (key    (string-hex-intern (cadr test)))
		   (output (string-hex-intern (caddr test))))
		(test-add! (string->symbol (format "idea-encrypt-~a" i))
			   (lambda ()
			      (string-hex-extern (idea-encrypt input key)))
			   (string-hex-extern output))
		(test-add! (string->symbol (format "idea-decrypt-~a" i))
			   (lambda ()
			      (string-hex-extern (idea-decrypt output key)))
			   (string-hex-extern input))))
	  *idea-tests*
	  (iota (length *idea-tests*)))

;*---------------------------------------------------------------------*/
;*    Block Cipher Modes ...                                           */
;*---------------------------------------------------------------------*/

;*------- Creation ----------------*/
(define (key->param key) (char->integer (string-ref key 0)))
(define (rot-encrypt from from-pos to to-pos rot)
   (let loop ((i 0))
      (unless (=fx i 8)
	 (let* ((c (string-ref from (+fx from-pos i)))
		(ci (char->integer c))
		(ci-rot (remainderfx (+fx ci rot) 256))
		(c-rot (integer->char ci-rot)))
	    (string-set! to (+fx to-pos i) c-rot))
	 (loop (+fx i 1)))))

(define (rot-decrypt from from-pos to to-pos rot)
   (rot-encrypt from from-pos to to-pos (-fx 256 rot)))

(register-cipher! 'rot
		  (instantiate::Block-Cipher
		     (name "Rotation Cipher (Rot)")
		     (block-size 8)
		     (preferred-key-length 1)
		     (encrypt! rot-encrypt)
		     (decrypt! rot-decrypt)
		     (key->encrypt-param key->param)
		     (key->decrypt-param key->param)))

(test-add! 'rotation-cipher
	   (lambda () (encrypt 'rot "abcd" "password") #t)
	   #t)


;*------- All different modes ----------------*/

(define *rot-passwd* (list (string #a008)))
(define (block-mode-passwords cipher)
   (case cipher
      ((rot) *rot-passwd*)
      ((aes) '("1234567890123456"))
      ((idea) '("1234567890123456"))
      ((des) '("12345678" "1234567"))
      ((cast-128) '("1234567890123456"))
      ((des3) '("123456789ABCDEF0" "12345678901234" "123456789ABCDEF012345678"
		"123456789012341234567"))))

(define *all-modes* '(cfb ofb ecb pcbc cbc ctr))
(define *all-pads* '(none bit ansi-x.923 iso-10126 pkcs7 zero))

(define *IVs* '(#f "16chars__A123456"))


(define (test-cipher-string cipher str . Largs)
   (every (lambda (pass)
	     (let* ((encrypted (apply encrypt cipher str pass
				      :string->key id Largs))
		    (decrypted (apply decrypt cipher encrypted pass
				      :string->key id Largs)))
		(when (not (string=? str decrypted))
		   (error 'cipher
			  "not string=?"
			  #f))))
	  (block-mode-passwords cipher)))

(define *mode-test-cases*
   `(
     ("0123456789ABCDEF0123456789ABCDEF" ,*all-modes* ,*all-pads* ,*IVs*)
     ("1234" ,*all-modes* ,(cdr *all-pads*) ,*IVs*)
     ("123456789abcdef0" ,*all-modes* ,*all-pads* ,*IVs*)
     ("" ,*all-modes* ,*all-pads* ,*IVs*)
     ("12345678abc" ,*all-modes* ,(cdr *all-pads*) ,*IVs*)
     ("12345678" ,*all-modes* ,*all-pads* ,*IVs*)))

(for-each
 (lambda (config)
    (let ((str (car config))
	  (modes (cadr config))
	  (pads (caddr config))
	  (ivs (car (cdddr config))))
       (for-each
	(lambda (mode)
	   (test-add!
	    (string->symbol (format "mode-~a" mode))
	    (lambda ()
	       (for-each
		(lambda (IV)
		   (for-each (lambda (pad)
				(test-cipher-string 'rot str :mode mode :pad pad :IV IV)
				(unless  (and (eq? pad 'none)
					      (=fx (string-length str) 8))
				   (test-cipher-string 'aes str :mode mode :pad pad :IV IV))
				(test-cipher-string 'idea str :mode mode :pad pad :IV IV)
				(test-cipher-string 'cast-128 str :mode mode :pad pad :IV IV)
				(test-cipher-string 'des str :mode mode :pad pad :IV IV)
				(test-cipher-string 'des3 str :mode mode :pad pad :IV IV)
				)
			     pads))
		ivs)
	       #t)
	    #t))
	modes)))
    *mode-test-cases*)

(define *test-file* "recette.scm")
(define *encrypted-file* "recette.scm.encrypted")
(define *decrypted-file* "recette.scm.decrypted")

(define (read-file f)
   (with-input-from-file f read-string))
(define (file-cmp f1 f2)
   (string=? (read-file f1) (read-file f2)))

(define (test-cipher-file cipher . Largs)
   (unwind-protect
      (bind-exit (return)
	 (let* ((content (read-file *test-file*))
		(pass (car (block-mode-passwords cipher))))
	    
	    (let* ((encrypted (apply encrypt-file cipher *test-file* pass Largs))
		   (decrypted (apply decrypt-string cipher encrypted pass Largs)))
	       (when (not (string=? decrypted content))
		  (return #f)))
	    
	    (let ((p1 (open-input-file *test-file*))
		  (p2 (open-output-file *encrypted-file*)))
	       (apply encrypt-sendchars cipher p1 p2 pass Largs)
	       (close-input-port p1)
	       (close-output-port p2)
	       (let ((p3 (open-input-file *encrypted-file*))
		     (p4 (open-output-file *decrypted-file*)))
		  (apply decrypt-sendchars cipher p3 p4 pass Largs)
		  (close-input-port p3)
		  (close-output-port p4)))
	    (when (not (file-cmp *test-file* *decrypted-file*))
	       (return #f))
	    
	    (when (not (string=? content
				 (apply decrypt-file cipher *encrypted-file* pass Largs)))
	       (return #f))
	    
	    (when (not (string=? content
				 (apply decrypt-string cipher
					(apply encrypt-file cipher *test-file* pass
					       Largs)
					pass Largs)))
	       (return #f))
	    
	    (let* ((mm (open-mmap *test-file*))
		   (encrypted (apply encrypt cipher mm pass Largs)))
	       (with-output-to-file *encrypted-file*
		  (lambda () (display encrypted)))
	       (close-mmap mm)
	       (let* ((mm (open-mmap *encrypted-file*))
		      (decrypted (apply decrypt cipher mm pass Largs)))
		  (close-mmap mm)
		  (when (not (string=? decrypted content))
		     (return #f)))))
	 #t)
      (begin
	 (when (file-exists? *encrypted-file*)
	    (delete-file *encrypted-file*))
	 (when (file-exists? *decrypted-file*)
	    (delete-file *decrypted-file*)))))

(for-each (lambda (mode)
	     (test-add! (string->symbol (format "file-~a" mode))
			(lambda ()
			   (and
			    (test-cipher-file 'aes :mode mode :pad 'pkcs7)
			    (test-cipher-file 'idea :mode mode :pad 'ansi-x.923)
			    (test-cipher-file 'cast-128 :mode mode :pad 'iso-10126)
			    (test-cipher-file 'des :mode mode :pad 'zero)
			    (test-cipher-file 'des3 :mode mode :pad 'pkcs7)))
			#t))
	  *all-modes*)

;*---------------------------------------------------------------------*/
;*    RSA ...                                                          */
;*---------------------------------------------------------------------*/
(define *rsa-key* #unspecified)
(define *public-rsa-key* #unspecified)
(define *private-rsa-key* #unspecified)

(test-add! 'read-rsa-key
	   (lambda ()
	      (let ((key (read-pem-key "recette_rsa_key.pem")))
		 (set! *rsa-key* key)
		 (set! *public-rsa-key* (extract-public-rsa-key *rsa-key*))
		 (set! *private-rsa-key* (extract-private-rsa-key *rsa-key*))
		 (and (= (with-access::Rsa-Key *rsa-key* (modulus) modulus)
			 #z130391666105704179754863069395887361542045423544290727226128667988622000016368330208678611918923843169906488199421368032302034266156783618796034466013789836218914760897657426448451709571252797526227914317072378755232261618321868220913427190742682338088330024212821052494738141067080657007545442268560276602611)
		      (= (with-access::Rsa-Key *public-rsa-key* (exponent)
			    exponent)
			 #z65537)
		      (= (with-access::Rsa-Key *private-rsa-key* (exponent)
			    exponent)
			 #z64381096533476990901133925302981813817218485137703948034092155079479062186698548868923374629361378170117705290829242231979820968591289822857635828398312778570113035640055692697221545796839354062636140081163260411702606518765390520878457328974382550272037948926286049340374167340374224187363049208753299897297)
		      #t)))
	   #t)

(test-add! 'rsa-signature
	   (lambda ()
	      (let* ((msg-hash (sha1sum "my message"))
		     (msg-hash-bignum (octet-string->bignum msg-hash))
		     (signature (rsa-sign *rsa-key* msg-hash-bignum)))
		 (rsa-verify *public-rsa-key* msg-hash-bignum signature)))
	   #t)

(test-add! 'rsa-encryption
	   (lambda ()
	      (let* ((rnd-pwd "my random password")
		     (rnd-pwd-bignum (octet-string->bignum rnd-pwd))
		     (encrypted (rsa-encrypt *public-rsa-key* rnd-pwd-bignum)))
		 (= (rsa-decrypt *rsa-key* encrypted)
		    rnd-pwd-bignum)))
	   #t)

;*---------------------------------------------------------------------*/
;*    DSA ...                                                          */
;*---------------------------------------------------------------------*/
(define *dsa-key* #unspecified)
(define *public-dsa-key* #unspecified)

(test-add! 'read-dsa-key
	   (lambda ()
	      (let ((key (read-pem-key "recette_dsa_key.pem")))
		 (set! *dsa-key* key)
		 (set! *public-dsa-key* (extract-public-dsa-key *dsa-key*))
		 (with-access::Complete-Dsa-Key *dsa-key* (x p q g y)
		    (and (= x
			    #z958542209398919543566219078024864035198855398235)
			 (= p
			    #z145171029792298048254115299435299589304197088998058268095101066899812876842722580924451568812892815389770356969716991169144655287731637026167946477125747215184894075175484508903160905049382512070315075898290369217837219641401051673124297810504586682630146163006054838845822568553605532856892132169499310621071)
			 (= q
			    #z1063255273624236151888990697036903655571497260569)
			 (= g
			    #z21614491116346987550803524787596840752171111753226081183908491637298293752364885816870293139983995599792680790904493733051840969985424668364381432943189075370665042803021752129551025626111876533219531480055065360284035979355705595846733398853584885266111174871403637422095523250445600609210957474986954428795)
			 (= y
			    #z39365993753093373213937314856154524353847690169648455888772651565132818598811983299959429501478411066852802008468734900658534294266484049696744859341833047706109589961465937376179642627138182978071861497582805900983384805995737729333707727555958277176686191359199940346327556783007294789292705382601308773119)))))
	   #t)

(test-add! 'dsa-signature
	   (lambda ()
	      (let* ((msg-hash (sha1sum "my message"))
		     (msg-hash-bignum (octet-string->bignum msg-hash)))
		 (receive (r s)
		    (dsa-sign *dsa-key* msg-hash-bignum)
		    (dsa-verify *public-dsa-key* msg-hash-bignum r s))))
	   #t)

;*---------------------------------------------------------------------*/
;*    DSA ...                                                          */
;*---------------------------------------------------------------------*/
(test-add! 'elgamal-encryption
	   (lambda ()
	      (let* ((elgamal-key (instantiate::Complete-ElGamal-Key
				     (p #z20197405202305148072881733328112652492708210743963346265099864004561299024226564758768671014289182101903615724895085532022947201140011241488136452263206424645774280788542161681494998538326044286059545532896058545829993696394230348402929242987879939090128819763473727641660196631791027963925919107360149647051883823559860559426942489877244386613737522697017928837827321761308081586699360833476805368017358330905296694598390761271330756418359005705370417592602548323304012418475112334076031436530305846237431149841034985214740129206453551364374497005033764547961205376002702270848212512284731532650663425767152663072139)
				     (g #z6)
				     (y #z1686543919882436901136167512810009992239498168579524320622172809736212463909628039859927016597383070551450920991168474363397953023272234106375747733509629440301647492397026598402041050166869689472585939934631803710086236719917692255315739012417007655646216657308791724378418914452930902689314264905858565296944357798202611288622628489028503794890301746822747169061881080631484022735023040071579058841260602668728947365029076886751148871279078983404929554128325984438912525829149166983913728938928079526671763551537464072634995794734090659951578216761670441390978070659891022692976165713006072943797244007389652739738)
				     (x #z2134518737763899887266634226677740732927868102526427119184291457116975686455967173260409051058440567651)))
		     (public (extract-public-elgamal-key elgamal-key))
		     (symmetric-pwd "my password")
		     (symmetric-pwd-bignum (octet-string->bignum
					    symmetric-pwd)))
		 (receive (c1 c2)
		    (elgamal-encrypt public symmetric-pwd-bignum)
		    (string=? symmetric-pwd
			      (bignum->octet-string
			       (elgamal-decrypt elgamal-key c1 c2))))))
	   #t)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((tests '()))
      (args-parse (cdr argv)
	 ((("-h" "--help") (help "This help message"))
	  (args-parse-usage #f)
	  (exit 0))
	 (else
	  (set! tests (cons (string->symbol else) tests))))
      ;; run all the tests
      (for-each (lambda (pvn)
		   (apply test pvn))
		(if (null? tests)
		    (reverse *tests*)
		    (reverse (filter (lambda (t) (memq (car t) tests))
				     *tests*))))
      ;; if we reach that point, we are done
      (print "\n"
	     (if (null? tests) "All" (reverse tests))
	     " tests executed...\n"
	     (if (null? *failure*)
		 "all succeeded"
		 (format " ~a succeeded\n ~a failed ~a"
			 *success*
			 (length *failure*)
			 (reverse *failure*))))))
