;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/examples/Lower/lower.scm             */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Oct 19 14:49:54 1992                          */
;*    Last change :  Wed Jan  8 05:55:35 2003 (serrano)                */
;*                                                                     */
;*    Un petit programme qui prends un fichier `Lisp' et qui           */
;*    convertit tous les symboles en minuscule.                        */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module LOWER
   (main MAIN))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (MAIN ARGV)
   (if (null? (cdr ARGV))
       (begin
	  (print "usage: lower [file1] ... [filen]")
	  (exit -1))
       (for-each LOWERFILE (cdr ARGV))))

;*---------------------------------------------------------------------*/
;*    lowerfile ...                                                    */
;*---------------------------------------------------------------------*/
(define (LOWERFILE FILENAME)
   (let ((input-port  (open-input-file FILENAME))
	 (output-port (open-output-file (string-append FILENAME "l"))))
      (cond
	 ((not (input-port? input-port))
	  (error "lower" "can't open input file" FILENAME))
	 ((not (output-port? output-port))
	  (error "lower"
		 "can't open output file"
		 (string-append FILENAME "l")))
	 (else
	  (lower input-port output-port)))))

;*---------------------------------------------------------------------*/
;*    lower ...                                                        */
;*---------------------------------------------------------------------*/
(define lower
   (let* ((par-open 0)
	  (grammar (regular-grammar ((chiffre (in ("09")))
				     (float   (or (: (* chiffre) #\. (+ chiffre))
						  (: (+ chiffre) #\. (* chiffre))))
	 			     (lettre  (in ("azAZ")))
				     (special (in #\! #\@ #\# #\$ #\%
						  #\^ #\& #\* #\> #\<
						  #\/ #\. #\- #\_ #\+   
						  #\\ #\| #\{ #\= #\?
						  #\} #\: #\~))
				     (quote   (in #\" #\, #\' #\`))
				     (paren   (in #\( #\) #\[ #\]))
				     (blank   (in #\space #\tab #\newline))  
				     (id      (+  (or lettre chiffre special))))
		      (blank
		       (the-string))
		      ((: #\# #\\ (or lettre
				      chiffre
				      special
				      quote
				      paren
				      ";"))
		       (the-string))
		      ((: ";" (* all)) 
		       (the-string))
		      ((uncase "#\newline")
		       (the-string))
		      ((uncase "#\tab")
		       (the-string))
		      ((uncase "#\space")
		       (the-string))
		      ((: #\" (* (or (out #\") (: #\\ #\"))) #\")
		       (the-string))
		      ((eof (: #\" (* (or (out #\") (: #\\ #\")))))
		       (error "read" "Unexpected end-of-file" 'string))
		      ((or (+ chiffre)
			   (: #\- (+ chiffre))
			   (: #\+ (+ chiffre)))
		       (the-string))
		      ((: "#o" (or (+ (in ("07")))
				   (: (in #\+ #\-)
				      (+ (in ("07"))))))
		       (the-string))
		      ((: "#d" (or (+ chiffre)
				   (: (in #\+ #\-) (+ chiffre))))
		       (the-string))
		      ((: "#x" (or (+ (or chiffre
					  (uncase (in "af"))))
				   (: (in #\+ #\-)
				      (+ (or chiffre
					     (uncase (in "af")))))))
		       (the-string))
		      ((or float
			   (: (in #\+ #\-) float)
			   (: (or float (+ chiffre))
			      (in #\e #\E) (+ chiffre)) 
			   (: (in #\+ #\-) (or float (+ chiffre))
					   (in #\e #\E) (+ chiffre))
			   (: (or float (+ chiffre)) (in #\e #\E)
						   (in #\+ #\-) (+ chiffre))
			   (: (in #\+ #\-) (or float (+ chiffre))
					   (in #\e #\E)
					   (in #\+ #\-)
					   (+ chiffre)))
		       (the-string))
		      ((context (pair) #\.)
		       (rgc-context)
		       'dot)
		      ((uncase "#t")
		       (the-string))
		      ((uncase "#f")
		       (the-string))
		      ((or id (: #\. (+ #\.)))
		       (string-downcase (the-string)))
		      (#\'
		       (cons 'quote (cons (ignore) '())))
		      (#\`
		       (cons 'quasiquote (cons (ignore) '())))
		      (#\,
		       (cons 'unquote (cons (ignore) '())))
		      ((: #\, #\@)
		       (cons 'unquote-splicing (cons (ignore) '())))
		      ((in #\( #\[)
		       (let ((open-key par-open))
			  (set! par-open (+ 1 par-open))
			  (rgc-context 'pair)
			  (let loop-pair ((walk (ignore)))
			     (cond
				((= open-key par-open)
				 '())
				((eq? walk 'dot)
				 (let ((cdr (ignore)))
				    (ignore)
				    (if (= open-key par-open)
					cdr
					(begin
					   (error "read" "illegal pair" cdr)
					   (let loop ()
					      (if (= par-open open-key)
						  (ignore)
						  (begin
						     (ignore)
						     (loop))))))))
				(else
				 (cons walk (loop-pair (ignore))))))))
		      ((in #\) #\])
		       (set! par-open (- par-open 1))
		       (if (< par-open 0)
			   (begin
			      (set! par-open 0)
			      (ignore))
			   #f))
		      ((: #\# #\()
		       (let ((car      (ignore)) 
			     (open-key par-open))
			  (set! par-open (+ 1 par-open))
			  (list->vector
			   (let loop-vector ((walk car))
			      (cond
				 ((= open-key par-open)
				  '())
				 (else
				  (cons walk
					(loop-vector (ignore)))))))))
		      (else
		       (let ((char (the-failure)))
			  (if (and (eof-object? char) (> par-open 0))
			      (error "read" "Unexpected end-of-file" char)
			      char))))))
      (lambda (input-port output-port)
	 (let loop ((sexp (read/rp grammar input-port)))
	    (if (eof-object? sexp)
		'done
		(begin
		   (display-sexp sexp output-port)
		   (loop (read/rp grammar input-port)))))))) 

;*---------------------------------------------------------------------*/
;*    display-sexp ...                                                 */
;*---------------------------------------------------------------------*/
(define (display-sexp sexp port)
   (cond
      ((pair? sexp)
       (cond
	  ((eq? (car sexp) 'quote)
	   (display "'" port)
	   (display (cadr sexp) port))
	  ((eq? (car sexp) 'unquote)
	   (display "," port)
	   (display (cadr sexp) port))
	  ((eq? (car sexp) 'quasiquote)
	   (display "`" port)
	   (display (cadr sexp) port))
	  ((eq? (car sexp) 'unquote-splicing)
	   (display ",@" port)
	   (display (cadr sexp) port))
	  (else
	   (let ((l sexp))
	      (display "(" port)
	      (for-each (lambda (exp) (display-sexp exp port))
			sexp)
	      (display ")" port)))))
      ((vector? sexp)
       (display "#(" port)
       (let loop ((i 0))
	  (if (= i (vector-length sexp))
	      (display "#)" port)
	      (begin
		 (display (vector-ref sexp i) port)
		 (loop (+ i 1))))))
      (else
       (display sexp port))))

	  
       
