(module roman
   (import roman
	   arabic)
   (main   main))

(define (main argv)
   (let ((prompt ":-) "))
      (display prompt)
      (let loop ((exp (read/rp grammar-roman (current-input-port))))
	 (if (eof-object? exp)
	     'done
	     (begin
		(display* (eval exp) #\Newline prompt)
		(loop (read/rp grammar-roman (current-input-port))))))))

(define grammar-roman
   (let ((par-open 0))
      (regular-grammar ((arabic (in "09"))
			(roman  (uncase "ivxlcdm")))
	 ((+ (in #\space #\newline #\tab))
	  (ignore))
	 ((+ arabic)
	  (string->integer (the-string)))
	 ((+ roman)
	  (roman->arabic (the-string)))
	 (#\(
          (let ((open-key par-open))
	     (set! par-open (+ 1 par-open))
	     (rgc-context 'pair)
	     (let loop-pair ((walk (ignore))) 
		(cond
		   ((= open-key par-open)
		    '())
		   (else
		    (cons walk (loop-pair (ignore))))))))
	 (#\)
	  (set! par-open (- par-open 1))
	  (if (< par-open 0)
	      (begin
		 (set! par-open 0)
		 (ignore))
	      #f))
	 ((in "+-*\\")
	  (string->symbol (the-string)))
	 (else
	  (let ((char (the-failure)))
	     (if (eof-object? char)
		 char
		 (error "grammar-roman" "Illegal char" char)))))))
