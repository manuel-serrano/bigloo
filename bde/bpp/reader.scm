;*---------------------------------------------------------------------*/
;*    serrano/prgm/project/bigloo/bde/bpp/reader.scm                   */
;*                                                                     */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 22 15:23:18 1992                          */
;*    Last change :  Sun Sep  7 10:02:03 2014 (serrano)                */
;*                                                                     */
;*    Le reader de `Bigloo'                                            */
;*---------------------------------------------------------------------*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module bpp-reader
   (import  (bpp "bpp/bpp.scm"))
   (export  pp-read))

;*---------------------------------------------------------------------*/
;*    pp-read ...                                                      */
;*---------------------------------------------------------------------*/
(define pp-read
   (let* ((par-open      0)
	  (grammar
	   (regular-grammar ((float    (or (: (* digit) "." (+ digit))
					   (: (+ digit) "." (* digit))))
			     (letter   (in ("azAZ") (#a128 #a255)))
			     (special  (in "!@#~$%^&*></-_+\\=?.:"))
			     (quote    (in "\",'`"))
			     (paren    (in "()[]{}"))
			     (blank    (in #\Space #\Tab #a012 #a013 #\Newline))
			     (id       (: (or letter digit special)
					  (* (or letter digit special quote)))))
	      ((+ blank)                    ;; on oublie les separateurs
	       (ignore))
	      
	      ((: #\# #\\ (or letter        ;; Les caracteres normaux
			      digit
			      special
			      quote
			      paren
			      (in "|;")))
	       (string-ref (the-string) 2))
	      
	      ((: (* #\space) ";" (* all))  ;; les commentaires
	       (if *ignore-comment*
		   (ignore)
		   (let* ((string (the-string))
			  (indice (let loop ((i 0))
				     (if (char=? (string-ref string i) #\;)
					 i
					 (loop (+fx i 1))))))
		      (list 'COMMENT
			    indice
			    (substring string
				       indice
				       (string-length string))))))
	      ((: #\# #\\ (uncase "newline")) ;; retour charriot
	       #\newline)
	      ((: #\# #\\ (uncase "tab"))     ;; tabulation
	       #\tab)
	      ((or (: #\# #\\ (uncase "space"));; espace
		   (: #\# #\\ #\space))
	       #\space)
	      ((: #\# #\\ (uncase "return"))  ;; carriage return
	       (integer->char 13))
	      ((: #\" (* (or (out #\\ #\")     ;; Les chaines de caracteres
			     (: #\\ all))) #\")
	       (the-escape-substring 1 (-fx (the-length) 1) #t))
	      ((: #\# #\"                  ;; Les chaines de caracteres foreign
		      (* (or (out #\\ #\")  
			     (: #\\ all))) #\")
	       (the-escape-substring 2 (-fx (the-length) 1) #f))
	      ((: (or #\" (: #\# #\"))     ;; Les bouts de chaines non termines
		  (* (or (out #\\ #\")  
			 (: #\\ all))))
	       (error "read" "Unexpected end-of-file" (the-string)))
	      ((or (+ digit)             ;; Les entiers
		   (: #\- (+ digit))
		   (: #\+ (+ digit)))
	       (string->integer (the-string) 10))
	      ((: "#o" (or (+ (in (#\0 #\7))) ;; Les entiers en base 8
			   (: (in #\+ #\-)
			      (in (#\0 #\7)))))
	       (string->integer (substring (the-string) 2 (the-length)) 8))
	      ((: "#d" (or (+ digit)       ;; Les entiers en base 10
			   (: (in #\+ #\-)
			      digit)))
	       (string->integer (substring (the-string) 2 (the-length)) 10))
	      ((: "#x" (or (+ (or digit     ;; Les entiers en base 10
				 (in (#\a #\f))
				 (in (#\A #\F))))
			   (: (in #\+ #\-)
			      (+ (or digit
				     (in (#\a #\f))
				     (in (#\A #\F)))))))
	       (string->integer (substring (the-string) 2 (the-length)) 16))
	      ((or float                   ;; Les reels
		  (: (in #\+ #\-) float)
		  (: (or float (+ digit))
		     (in #\e #\E) (+ digit)) 
		  (: (in #\+ #\-) (or float (+ digit))
				  (in #\e #\E) (+ digit))
		  (: (or float (+ digit)) (in #\e #\E) (in #\+ #\-)
					    (+ digit))
		  (: (in #\+ #\-) (or float (+ digit))
				  (in #\e #\E) (in #\+ #\-) (+ digit)))
	       (string->real (the-string)))
	      ((context pair (: #\. (* blank) #\)))
		       (error "read" "Illegal pair" (the-string)))
	      ((context pair #\.)        ;; Le point des pairs pointees
		       '__dot__)
	      (#\.
	       (error "read" "Illegal token" #\.))
	      ((uncase "#t")             ;; true
	       #t)
	      ((uncase "#f")             ;; false
	       #f)
	      ("#unspecified" 
	       (unspecified))
	      ((or id (: #\. (+ #\.)))      ;; Les identificateurs
	       (if (eq? *case* 'respect)
		   (string->symbol (the-string)) 
		   (the-symbol)))
	      ((: "|" (+ (or (out #a000 #\\ #\|) (: #\\ all))) "|")
	       (let ((str (the-string)))
		  (string->symbol (string-as-read str))))
	      (#\'                     ;; Les simples quotations
	       (cons 'quote (cons (ignore) '())))
	      (#\`                     ;; Les quasiquotes
	       (cons 'quasiquote (cons (ignore) '())))
	      (#\,                     ;; Les unquotations
	       (cons 'unquote (cons (ignore) '())))
	      ((: #\, #\@)                 ;; Les unquote-splicing
	       (cons 'unquote-splicing (cons (ignore) '())))
	      ((in #\( #\[)              ;; Les parentheses ouvrantes
	       (let ((open-key par-open))
		 (set! par-open (+fx 1 par-open))
		 (rgc-context 'pair)
		 (let loop-pair ((walk (ignore))) 
		   (cond
		    ((eq? walk '__dot__) ;; une pair pointee
		     (rgc-context)
		     (let ((cdr (ignore)))
		       (ignore)
		       (if (=fx open-key par-open)
			   (begin
			     (rgc-context 'pair)
			     cdr)
			   (error "read" "Illegal pair" cdr))))
		    ((=fx open-key par-open)
		     (if (=fx open-key 0)
			 (rgc-context))
		     '())
		    (else
		     (cons walk (loop-pair (ignore))))))))
	      ((in #\) #\])               ;; Les parentheses fermantes
	       (set! par-open (-fx par-open 1))
	       (if (<fx par-open 0)
		   (begin
		     (set! par-open 0)
		     (ignore))
		   #f))
	      ((: #\# #\()                 ;; Les debuts de vecteur
	       (let ((open-key par-open))
		 (set! par-open (+fx 1 par-open))
		 (list->vector (let loop-vector ((walk (ignore)))
				 (cond
				  ((=fx open-key par-open)
				   '())
				  (else
				   (cons walk (loop-vector (ignore)))))))))
	      (else
	       (let ((char (the-failure)))
		 (if (eof-object? char)
		     (if (>fx par-open 0)
			 (error "read" "Unexpected end-of-file" char)
			 (begin
			    (reset-eof (the-port))
			    char))
		     (error "read"
			    "Illegal char"
			    (illegal-char-rep char))))))))
     (lambda input-port
       (if (null? input-port)
	   (read/rp grammar (current-input-port))
	   (read/rp grammar (car input-port))))))

		      
		 

