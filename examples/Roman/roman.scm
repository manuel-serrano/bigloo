;<font size="-3"><pre>
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Roman/roman.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Tue Dec 22 08:44:51 1992                          */
;*    Last change :  Wed Oct 10 15:06:42 2001 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Reading and computing on Roman numbers                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module roman (main main))

;*---------------------------------------------------------------------*/
;*    roman->arabic ...                                                */
;*---------------------------------------------------------------------*/
(define (roman->arabic r)
   (define (chiffre.roman->arabic chiffre)
      (case chiffre
	 ((#\m #\M) 1000)
	 ((#\d #\D) 500)
	 ((#\c #\C) 100)
	 ((#\l #\L) 50)
	 ((#\x #\X) 10)
	 ((#\v #\V) 5)
	 ((#\i #\I) 1)
	 (else
	  (error "chiffre" "Illegal char" chiffre))))
   (let ((len (string-length r)))
      (let loop ((indice 0))
      (cond
	 ((= indice len)
	  0)
	 ((= indice (- len 1))
	  (chiffre.roman->arabic (string-ref r indice)))
	 (else
	  (let ((x (chiffre.roman->arabic (string-ref r indice)))
		(y (chiffre.roman->arabic (string-ref r (+ 1 indice)))))
	     (if (< x y)
		 (+ (- x) (loop (+ 1 indice)))
		 (+ x (loop (+ 1 indice))))))))))

;*---------------------------------------------------------------------*/
;*    grammar-roman ...                                                */
;*---------------------------------------------------------------------*/
(define grammar-roman
   (let ((par-open 0))
      (regular-grammar ((arabic (in ("09")))
			(roman  (in "ivxlcdm")))
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

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((prompt ":-) "))
      (display prompt)
      (let loop ((exp (read/rp grammar-roman (current-input-port))))
	 (if (eof-object? exp)
	     'done
	     (begin
		(display* (eval exp) #\Newline prompt)
		(loop (read/rp grammar-roman (current-input-port))))))))
;</pre></font>
