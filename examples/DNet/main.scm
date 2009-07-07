(module foo
   (java (class lib
	    (constructor new (::procedure))
	    (method static init::bool () "init")
	    (method static string->cstring::%string (::string) "bytesToString")
	    (method static show-cstring::string (::%string) "showString")
	    (method invoke::obj (::lib ::string) "invoke")
	    "lib")
	 (class %string
	    "System.String"))
   (main main))

(lib-init)

(define (main argv)
   (print "Bigloo: running Bigloo main...")
   (let ((o (lib-new bgl-callback)))
      (print "Bigloo: o=" o)
      (let ((r (lib-invoke o (car argv))))
	 (print "Bigloo: res=" r))
      (let ((cs (lib-string->cstring "this is a string")))
	 (let ((s (lib-show-cstring cs)))
	    (print "Bigloo: s=" s)))))

(define (bgl-callback s)
   (print "Bigloo: s=" s)
   (string-length s))
      
