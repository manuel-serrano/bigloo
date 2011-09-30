;*=====================================================================*/
;*    A test module that deploys the examples of openpgp.              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module recette
   ;(library crypto)
   (library openpgp)
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

(define (db-resolver db)
   (lambda (key-id)
      (print "Resolving key for: " (string-hex-extern key-id))
      (let ((possible-keys (pgp-resolve-key db key-id)))
	 (print (format "found ~a keys." (length possible-keys)))
	 (for-each (lambda (subkey)
		      (print (pgp-subkey->string subkey)))
		   possible-keys)
	 possible-keys)))

(define (password-provider pwd)
   (lambda (subkey)
      (print "Returning password for: " (pgp-subkey->string subkey))
      pwd))

(test-add! 'key-rsa-read
	   (lambda ()
	      (let ((keys (pgp-read-file "testy_rsa_pwd_xyz.key")))
		 (and (not (null? keys))
		      (pgp-key? (car keys))
		      (begin
			 (print (pgp-key->string (car keys)))
			 #t))))
	   #t)

(test-add! 'key-rsa/rsa-read
	   (lambda ()
	      (let ((keys (pgp-read-file "testy_rsa_rsa_pwd_xyz.key")))
		 (and (not (null? keys))
		      (pgp-key? (car keys))
		      (begin
			 (print (pgp-key->string (car keys)))
			 #t))))
	   #t)

(test-add! 'key_dsa_elgamal-read
	   (lambda ()
	      (let ((keys (pgp-read-file "testy_dsa_elgamal_pwd_xyz.key")))
		 (and (not (null? keys))
		      (pgp-key? (car keys))
		      (begin
			 (print (pgp-key->string (car keys)))
			 #t))))
	   #t)

(test-add! 'signature
	   (lambda ()
	      (let* ((pgp-key (car (pgp-read-file "testy_rsa_pwd_xyz.key")))
		     (sig (pgp-sign
			   "abcd" pgp-key
			   (password-provider "xyz")))
		     (str-sig (pgp-write-string sig))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db pgp-key)
		 (not (null? (pgp-verify (pgp-read-string str-sig)
					 (db-resolver db)
					 "abcd")))))
	   #t)

(test-add! 'signature-attached
	   (lambda ()
	      (let* ((pgp-key (car (pgp-read-file "testy_rsa_pwd_xyz.key")))
		     (sig (pgp-sign
			   "abcd" pgp-key
			   (password-provider "xyz")
			   :detached-signature? #f))
		     (str-sig (pgp-write-string sig))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db pgp-key)
		 (not (null? (pgp-verify (pgp-read-string str-sig)
					 (db-resolver db))))))
	   #t)

(test-add! 'decrypt-symmetric-3DES
	   (lambda ()
	      (let ((comp (pgp-read-file "symmetric3DES_pwd_foobar.msg")))
		 (string=? "abcd\n"
			   (pgp-decrypt comp
					:passkey-provider
					(lambda () "foobar")))))
	   #t)

(test-add! 'decrypt-symmetric-CAST5
	   (lambda ()
	      (let ((comp (pgp-read-file "symmetricCAST5_pwd_foobar.msg")))
		 (string=? "abcd\n"
			   (pgp-decrypt comp
					:passkey-provider
					(lambda () "foobar")))))
	   #t)

(test-add! 'decrypt-symmetric-CAST5_no-algo-info
	   (lambda ()
	      (let ((comp (pgp-read-file "symmetricCAST5_noAlgoInfo_pwd_foobar.msg")))
		 (string=? "abcd"
			   (pgp-decrypt comp
					:passkey-provider
					(lambda () "foobar")))))
	   #t)

(test-add! 'encrypt
	   (lambda ()
	      (let* ((encrypted (pgp-password-encrypt "abcd" "foobar" :mdc #f))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored)))
		 (print encrypted-str)
		 (string=? (pgp-decrypt (pgp-read-string encrypted-str)
					:passkey-provider (lambda () "foobar"))
			   "abcd")))
	   #t)


(test-add! 'encrypt-mdc
	   (lambda ()
	      (let* ((encrypted (pgp-password-encrypt "abcd" "foobar" :mdc #t))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored)))
		 (print encrypted-str)
		 (string=? (pgp-decrypt (pgp-read-string encrypted-str)
					:passkey-provider (lambda () "foobar"))
			   "abcd")))
	   #t)

(test-add! 'decrypt/public/elgamal
	   (lambda ()
	      (let ((msg (pgp-read-file "publicTesty_elgamal.encrypted"))
		    (key (car (pgp-read-file "testy_dsa_elgamal_pwd_xyz.key")))
		    (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db key)
		 (pgp-decrypt msg
			      :passkey-provider (lambda () "xyz")
			      :key-manager (db-resolver db)
			      :password-provider
			      (lambda (k)
				 (let ((str (pgp-subkey->string k)))
				    (tprint "returning password for " str))
				 "xyz"))))
	   "abcd")

(test-add! 'decrypt/public/rsa
	   (lambda ()
	      (let ((msg (pgp-read-file "publicTesty_rsa_rsa.encrypted"))
		    (key (car (pgp-read-file "testy_rsa_rsa_pwd_xyz.key")))
		    (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db key)
		 (pgp-decrypt msg
			      :passkey-provider (lambda () "xyz")
			      :key-manager (db-resolver db)
			      :password-provider
			      (lambda (k)
				 (let ((str (pgp-subkey->string k)))
				    (tprint "returning password for " str))
				 "xyz"))))
	   "abcd")


(test-add! 'encrypt/pwd
	   (lambda ()
	      (let* ((encrypted (pgp-encrypt "abcd" '() '("foobar")))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored)))
		 (print encrypted-str)
		 #t))
	   #t)

(test-add! 'encrypt/pwds
	   ;; gpg does not handle multiple passphrases per message.
	   (lambda ()
	      (let* ((encrypted (pgp-encrypt "abcd" '()
					     '("foobar" "toto" "titi"
					       "123456789012345")
					     :symmetric-algo 'aes-256))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored)))
		 (print encrypted-str)
		 #t))
	   #t)

(test-add! 'decrypt/pwds
	   ;; This seems to be broken in gpg. Only test internally.
	   (lambda ()
	      (let* ((encrypted (pgp-encrypt "abcd" '()
					     '("foobar" "toto" "titi")))
		     (encrypted-str (pgp-write-string encrypted)))
		 (and (string=?
		       "abcd"
		       (pgp-decrypt (pgp-read-string encrypted-str)
				    :passkey-provider (lambda () "foobar")))
		      (string=?
		       "abcd"
		       (pgp-decrypt (pgp-read-string encrypted-str)
				    :passkey-provider (lambda () "toto")))
		      (string=?
		       "abcd"
		       (pgp-decrypt (pgp-read-string encrypted-str)
				    :passkey-provider (lambda () "titi"))))))
	   #t)

(test-add! 'encrypt/keys
	   (lambda ()
	      (let* ((key (car (pgp-read-file "testy_rsa_rsa_pwd_xyz.key")))
		     (encrypted (pgp-encrypt "abcd" `(,key) '()
					     :symmetric-algo 'aes-256))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored)))
		 (print "Used key: " (pgp-key->string key))
		 (print encrypted-str)
		 #t))
	   #t)

(test-add! 'decrypt/keys
	   (lambda ()
	      (let* ((key (car (pgp-read-file "testy_rsa_rsa_pwd_xyz.key")))
		     (encrypted (pgp-encrypt "abcd" `(,key) '()
					     :symmetric-algo 'aes-256))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db key)
		 (print "Used key: " (pgp-key->string key))
		 (print encrypted-str)
		 (string=?
		  "abcd"
		  (pgp-decrypt (pgp-read-string encrypted-str)
			       :key-manager (db-resolver db)
			       :password-provider
			       (lambda (k)
				  (let ((str (pgp-subkey->string k)))
				     (tprint "returning password for: "
					     str)
				     "xyz"))))))
	   #t)

(test-add! 'decrypt/keys2
	   (lambda ()
	      (let* ((key1 (car (pgp-read-file "testy_rsa_rsa_pwd_xyz.key")))
		     (key2 (car (pgp-read-file "testy_rsa_pwd_xyz.key")))    
		     (key3 (car (pgp-read-file "testy_dsa_elgamal_pwd_xyz.key")))
		     (encrypted (pgp-encrypt "abcd"
					     `(,key1 ,key2 ,key3)
					     '()
					     :symmetric-algo 'aes-256))
		     (encrypted-str (pgp-write-string encrypted
						      :format 'armored))
		     (db1 (pgp-make-key-db))
		     (db2 (pgp-make-key-db))
		     (db3 (pgp-make-key-db)))
		 (pgp-add-key-to-db db1 key1)
		 (pgp-add-key-to-db db2 key2)
		 (pgp-add-key-to-db db3 key3)
		 (print encrypted-str)
		 (and
		  (string=?
		   "abcd"
		   (pgp-decrypt (pgp-read-string encrypted-str)
				:key-manager (db-resolver db1)
				:password-provider
				(lambda (k)
				   (let ((str (pgp-subkey->string k)))
				      (tprint "returning password for: "
					      str)
				      "xyz"))))
		  (string=?
		   "abcd"
		   (pgp-decrypt (pgp-read-string encrypted-str)
				:key-manager (db-resolver db2)
				:password-provider
				(lambda (k)
				   (let ((str (pgp-subkey->string k)))
				      (tprint "returning password for: "
					      str)
				      "xyz"))))
		  (string=?
		   "abcd"
		   (pgp-decrypt (pgp-read-string encrypted-str)
				:key-manager (db-resolver db3)
				:password-provider
				(lambda (k)
				   (let ((str (pgp-subkey->string k)))
				      (tprint "returning password for: "
					      str)
				      "xyz")))))))
	   #t)

(test-add! 'decrypt/pwd/public1
	   (lambda ()
	      (let ((msg (pgp-read-file "pwd_foobar_public_testy.encrypted"))
		    (key (car (pgp-read-file "testy_rsa_rsa_pwd_xyz.key"))))
		 (pgp-decrypt msg
			      :passkey-provider (lambda () "foobar"))))
	   "abcd\n")

(test-add! 'decrypt/pwd/public2
	   (lambda ()
	      (let ((msg (pgp-read-file "pwd_foobar_public_testy.encrypted"))
		    (key (car (pgp-read-file "testy_dsa_elgamal_pwd_xyz.key")))
		    (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db key)
		 (pgp-decrypt msg
			      :key-manager (db-resolver db)
			      :password-provider
			      (lambda (k)
				 (let ((str (pgp-subkey->string k)))
				    (tprint "returning password for: "
					    str)
				    "xyz")))))
	   "abcd\n")

;; For the following see RFC 3156.
;; Especially how to get to the canonical form of the message.

(test-add! 'rsa_abcd
	   (lambda ()
	      (let* ((pgp-key (car (pgp-read-file "testy_rsa_pwd_xyz.key")))
		     (sig (pgp-read-file "rsa_testy_abcd_sig"))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db pgp-key)
		 (not (null? (pgp-verify sig
					 (db-resolver db)
					 "Content-Type: Text/Plain;\r\n  charset=\"us-ascii\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\nabcd\r\n")))))
	   #t)

(test-add! 'abcd
	   (lambda ()
	      (let* ((pgp-key (car (pgp-read-file "testy_dsa_elgamal_pwd_xyz.key")))
		     (sig (pgp-read-file "testy_abcd_sig"))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db pgp-key)
		 (not (null? (pgp-verify sig
					 (db-resolver db)
					 "Content-Type: Text/Plain;\r\n  charset=\"us-ascii\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\nabcd\r\n")))))
	   #t)

(test-add! 'rsa_abcd_efgh
	   (lambda ()
	      (let* ((pgp-key (car (pgp-read-file "testy_rsa_pwd_xyz.key")))
		     (sig (pgp-read-file "rsa_testy_abcd_efgh_sig"))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db pgp-key)
		 (not (null? (pgp-verify sig
					 (db-resolver db)
					 "Content-Type: Text/Plain;\r\n  charset=\"us-ascii\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\nabcd\r\nefgh\r\n")))))
	   #t)

(test-add! 'abcd_efgh
	   (lambda ()
	      (let* ((pgp-key (car (pgp-read-file "testy_dsa_elgamal_pwd_xyz.key")))
		     (sig (pgp-read-file "testy_abcd_efgh_sig"))
		     (db (pgp-make-key-db)))
		 (pgp-add-key-to-db db pgp-key)
		 (not (null? (pgp-verify sig
					 (db-resolver db)
					 "Content-Type: Text/Plain;\r\n  charset=\"us-ascii\"\r\nContent-Transfer-Encoding: 7bit\r\n\r\nabcd\r\nefgh\r\n")))))
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
