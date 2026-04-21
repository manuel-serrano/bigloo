;*=====================================================================*/
;*    .../prgm/project/bigloo/5.0a/api/web/src/Llib/markdown.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Apr  9 17:38:56 2026                          */
;*    Last change :  Tue Apr 21 07:30:09 2026 (serrano)                */
;*    Copyright   :  2026 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Markdown parser                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_markdown

   (static
      (class MDState
	 tag::symbol
	 (elements::pair-nil (default '()))
	 (attributes::pair-nil (default '()))
	 (value::obj (default #f))
	 (wrapper::obj (default #f))
	 (parent (default #f)))
	    
      (class MDBlock::MDState
	 (bob::bool (default #t)))
	    
      (class MDSpan::MDState))

   (import __web_html)

   (export (markdown-parse ::input-port #!key charset fontifier eval)
	   (markdown-element::struct ::symbol ::pair-nil ::pair-nil)
	   (markdown-element-tag ::struct)
	   (markdown-element-attribute ::struct ::symbol)
	   (markdown-element-attribute-set! ::struct ::symbol ::obj)
	   (markdown-text-element-value::bstring ::struct)
	   (markdown-element-children::pair-nil ::struct)
	   (markdown-element-children-set!::pair-nil ::struct ::pair-nil)
	   (markdown-element-append-child!::pair-nil ::struct ::obj)
	   (markdown-element-next-sibling ::struct)
	   (markdown-element-previous-sibling ::struct)
	   (markdown-get-elements-by-tag::pair-nil ::struct ::symbol)
	   (markdown->html html #!optional (op::output-port (current-output-port)))))

;*---------------------------------------------------------------------*/
;*    the-choord ...                                                   */
;*    -------------------------------------------------------------    */
;*    Builds a Bigloo location object                                  */
;*---------------------------------------------------------------------*/
(define (the-coord input-port offset)
   `(at ,(input-port-name input-port)
       ,(-fx (input-port-position input-port) offset)))

;*---------------------------------------------------------------------*/
;*    make-token ...                                                   */
;*---------------------------------------------------------------------*/
(define (make-token type value loc)
   (econs type value loc))

;*---------------------------------------------------------------------*/
;*    token ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (token type value offset)
   `(make-token ,type ,value (the-coord (the-port) ,offset)))

;*---------------------------------------------------------------------*/
;*    token-tag ...                                                    */
;*---------------------------------------------------------------------*/
(define (token-tag token)
   (car token))

;*---------------------------------------------------------------------*/
;*    token-value ...                                                  */
;*---------------------------------------------------------------------*/
(define (token-value token)
   (cdr token))

;*---------------------------------------------------------------------*/
;*    token-loc ...                                                    */
;*---------------------------------------------------------------------*/
(define (token-loc token)
   (cer token))

;*---------------------------------------------------------------------*/
;*    ctor ...                                                         */
;*---------------------------------------------------------------------*/
(define-struct ctor tag attrs children parent)

;*---------------------------------------------------------------------*/
;*    markdown-element ...                                             */
;*---------------------------------------------------------------------*/
(define (markdown-element tag attrs cs::pair-nil)
   (let ((el (ctor tag attrs cs #f)))
      (markdown-element-children-set! el
	 (map! (lambda (o) (if (ctor? o) o (ctor 'text '() o #f))) cs))
      el))

;*---------------------------------------------------------------------*/
;*    markdown-element-tag ...                                         */
;*---------------------------------------------------------------------*/
(define (markdown-element-tag e)
   (ctor-tag e))

;*---------------------------------------------------------------------*/
;*    markdown-element-attribute ...                                   */
;*---------------------------------------------------------------------*/
(define (markdown-element-attribute e attr)
   (let ((c (assq attr (ctor-attrs e))))
      (when (pair? c)
	 (cdr c))))

;*---------------------------------------------------------------------*/
;*    markdown-element-attribute-set! ...                              */
;*---------------------------------------------------------------------*/
(define (markdown-element-attribute-set! e attr val)
   (let ((c (assq attr (ctor-attrs e))))
      (if (pair? c)
	  (set-cdr! c val)
	  (ctor-attrs-set! e (cons (cons attr val) (ctor-attrs e))))))

;*---------------------------------------------------------------------*/
;*    markdown-text-element-value ...                                  */
;*---------------------------------------------------------------------*/
(define (markdown-text-element-value e)
   (if (eq? (ctor-tag e) 'text)
       (ctor-children e)
       (error "markdown-text-element-value"
	  "argument is not a text element"
	  (ctor-tag e))))

;*---------------------------------------------------------------------*/
;*    markdown-element-children ...                                    */
;*---------------------------------------------------------------------*/
(define (markdown-element-children e)
   (ctor-children e))

;*---------------------------------------------------------------------*/
;*    markdown-element-children-set! ...                               */
;*---------------------------------------------------------------------*/
(define (markdown-element-children-set! e os)
   (for-each (lambda (c)
		(ctor-parent-set! c #f))
      (ctor-children e))
   (let ((cs (map (lambda (o)
		     (if (ctor? o)
			 o
			 (ctor 'text '() o #f)))
		os)))
      (for-each (lambda (c)
		   (ctor-parent-set! c e))
	 cs)
      (ctor-children-set! e cs)
      cs))

;*---------------------------------------------------------------------*/
;*    markdown-element-next-sibling ...                                */
;*---------------------------------------------------------------------*/
(define (markdown-element-next-sibling e)
   (let ((parent (ctor-parent e)))
      (when (ctor? parent)
	 (let ((l (memq e (ctor-children parent))))
	    (when (pair? (cdr l))
	       (cadr l))))))

;*---------------------------------------------------------------------*/
;*    markdown-element-previous-sibling ...                            */
;*---------------------------------------------------------------------*/
(define (markdown-element-previous-sibling e)
   (let ((parent (ctor-parent e)))
      (when (ctor? parent)
	 (let ((cs (ctor-children e)))
	    (when (and (pair? cs) (pair? (cdr cs)))
	       (let loop ((p (car cs))
			  (n (cdr cs)))
		  (cond
		     ((null? n) #f)
		     ((eq? (car n) e) (car p))
		     (else (loop n (cdr n))))))))))

;*---------------------------------------------------------------------*/
;*    markdown-element-append-child! ...                               */
;*---------------------------------------------------------------------*/
(define (markdown-element-append-child! e o)
   (let* ((c (if (ctor? o)
		 o
		 (ctor 'text '() o #f)))
	  (nc (append (ctor-children e) (list c))))
      (when (ctor? c)
	 (ctor-parent-set! c e))
      (ctor-children-set! e nc)
      nc))

;*---------------------------------------------------------------------*/
;*    markdown-get-elenents-by-tag ...                                 */
;*---------------------------------------------------------------------*/
(define (markdown-get-elements-by-tag::pair-nil el::struct tag::symbol)
   
   (define (get-tag::pair-nil el::struct tag::symbol)
      (if (eq? (ctor-tag el) 'text)
	  (if (eq? tag 'text) (list el) '())
	  (let ((els (append-map (lambda (c) (get-tag c tag)) (ctor-children el))))
	     (if (eq? (ctor-tag el) tag)
		 (cons el els)
		 els))))
   
   (get-tag el tag))

;*---------------------------------------------------------------------*/
;*    flatten ...                                                      */
;*---------------------------------------------------------------------*/
(define (flatten l)
   (cond
      ((null? l) l)
      ((pair? l) (append (flatten (car l)) (flatten (cdr l))))
      (else (list l))))

;*---------------------------------------------------------------------*/
;*    section-tag ...                                                  */
;*---------------------------------------------------------------------*/
(define (section-tag depth)
   (case depth
      ((1) 'h1)
      ((2) 'h2)
      ((3) 'h3)
      ((4) 'h4)
      (else 'h5)))

;*---------------------------------------------------------------------*/
;*    inline-els ...                                                   */
;*---------------------------------------------------------------------*/
(define (inline-els tag els)
   (if (and (pair? els) (null? (cdr els)) (eq? (ctor-tag (car els)) tag))
       (ctor-children (car els))
       els))

;*---------------------------------------------------------------------*/
;*    MDState-tag ...                                                  */
;*---------------------------------------------------------------------*/
(define (MDState-tag s)
   (with-access::MDState s (tag)
      tag))

;*---------------------------------------------------------------------*/
;*    markdown-name ...                                                */
;*---------------------------------------------------------------------*/
(define (markdown-name expr)
   (let* ((s (with-output-to-string
		(lambda ()
		   (let loop ((e expr))
		      (cond
			 ((pair? e)
			  (for-each loop e))
			 ((null? e)
			  #unspecified)
			 (else
			  (display e)))))))
	  (s2 (pregexp-replace* "  |\n" s " ")))
      (pregexp-replace* "^ +| +$" s2 "")))

;*---------------------------------------------------------------------*/
;*    markdown-read-error ...                                          */
;*---------------------------------------------------------------------*/
(define (markdown-read-error msg obj port)
   (raise
      (instantiate::&io-read-error
	 (fname (input-port-name port))
	 (location (input-port-position port))
	 (proc "markdown-parser")
	 (msg msg)
	 (obj obj))))

;*---------------------------------------------------------------------*/
;*    markdown-parse-error ...                                         */
;*---------------------------------------------------------------------*/
(define (markdown-parse-error msg obj ctx)
   (match-case ctx
      ((at ?fname ?loc)
       (raise
	  (instantiate::&io-parse-error
	     (fname fname)
	     (location loc)
	     (proc "markdown-parser")
	     (msg msg)
	     (obj obj))))
      ((? input-port?)
       (raise
	  (instantiate::&io-parse-error
	     (fname (input-port-name ctx))
	     (location (input-port-position ctx))
	     (proc "markdown-parser")
	     (msg msg)
	     (obj obj))))
      (else
       (raise
	  (instantiate::&io-parse-error
	     (proc "markdown-parser")
	     (msg msg)
	     (obj obj))))))

;*---------------------------------------------------------------------*/
;*    normalize-string ...                                             */
;*---------------------------------------------------------------------*/
(define (normalize-string str)
   (let ((b (string-skip str " \t"))
	 (e (string-skip-right str " \t")))
      (cond
	 ((not b)
	  "")
	 ((=fx b 0)
	  (if (=fx e (-fx (string-length str) 1))
	      str
	      (string-shrink! str (+fx 1 e))))
	 ((=fx e (-fx (string-length str) 1))
	  (substring str b))
	 (else
	  (substring str b (+fx 1 e))))))

;*---------------------------------------------------------------------*/
;*    remove-surrounding-spaces ...                                    */
;*---------------------------------------------------------------------*/
(define (remove-surrounding-spaces l)
   (let loop ((l l)
	      (mode 'all))
      (cond
	 ((string? l)
	  (case mode
	     ((all)
	      (normalize-string l))
	     ((head)
	      (let ((b (string-skip l " \t")))
		 (if b
		     (substring l b)
		     l)))
	     ((tail)
	      (let ((b (string-skip-right l " \t")))
		 (if b
		     (string-shrink! l (+fx b 1))
		     "")))
	     (else
	      l)))
	 ((pair? l)
	  (if (null? (cdr l))
	      (list (loop (car l) mode))
	      (cons (loop (car l) 'head)
		    (let liip ((l (cdr l)))
		       (if (null? (cdr l))
			   (list (loop (car l) 'tail))
			   (cons (car l) (liip (cdr l))))))))
	 (else
	  l))))

;*---------------------------------------------------------------------*/
;*    *comment-grammar* ...                                            */
;*---------------------------------------------------------------------*/
(define *comment-grammar*
   (regular-grammar ()
      ((: (* (out "-\n\r")))
       (ignore))
      ((: (* (out "-\n\r")) #\Newline)
       #f)
      ("-"
       (ignore))
      ((: "-*-" (* all) #\Newline)
       (let ((s (the-string)))
	  (let ((i (string-contains s "-*-" 3)))
	     (when (fixnum? i)
		(let ((j (string-contains-ci s "coding:" 3)))
		   (when (and (fixnum? j) (<fx j i))
		      (let ((n (string-skip s " \t"
				  (+fx j (string-length "coding:")))))
			 (when (fixnum? n)
			    (cond
			       ((string-index s " " n)
				=>
				(lambda (m)
				   (string->symbol (substring s n m))))
			       ((string-contains s "-*-" n)
				=>
				(lambda (m)
				   (string->symbol (substring s n m)))))))))))))))

;*---------------------------------------------------------------------*/
;*    *tab-code-block-grammar* ...                                     */
;*---------------------------------------------------------------------*/
(define *tab-code-block-grammar*
   (regular-grammar ((line (or (* (out "\r\n,$"))
			       (: (* (out "\r\n,$")) ",")
			       (: (* (out "\r\n,$")) "$")
			       (+ (: (* (out "\r\n,$")) (? (: "," (out "(")))))
			       (+ (: (* (out "\r\n,$")) (? (: "$" (out "{")))))))
		     (crlf (or "\n" "\r\n"))
		     indent lines conv)
      
      (define (the-conv-substring start end)
       (if (> end start)
	   (conv (the-substring start end))
	   ""))
      
      ;; newline
      ((: (+ crlf) (: (or "\t" "    ") line crlf))
       (let* ((s (the-string))
	      (i (string-skip s "\r\n")))
	  (set! lines
	     (cons* (substring s (+fx i indent) (the-length))
		(substring s 0 i)
		lines)))
       (rgc-context)
       (ignore))
      ;; plain line of code
      ((bol (: (or "\t" "    ") line crlf))
       (set! lines (cons (the-conv-substring indent (the-length)) lines))
       (rgc-context)
       (ignore))
      ((context expr (: line crlf))
       (set! lines (cons (the-conv-substring 0 (the-length)) lines))
       (rgc-context)
       (ignore))
      ;; embedded bigloo
      ((bol (: (or "\t" "    ") line ",(" ))
       (let* ((s (the-string))
	      (i (string-skip s "\r\n")))
	  (set! lines
	     (cons* (substring s (+fx i indent) (-fx (the-length) 2))
		(substring s 0 i)
		lines)))
       (rgc-buffer-unget-char (the-port) (char->integer #\())
       (ctor 'expr '() (read (the-port)) #f)
       (rgc-context 'expr)
       (ignore))
      (else
       (unless (eof-object? (the-failure))
	  (rgc-buffer-unget-char (the-port) (char->integer (the-failure))))
       (reverse! lines))))

;*---------------------------------------------------------------------*/
;*    fontify-code ...                                                 */
;*---------------------------------------------------------------------*/
(define (fontify-code lines lang::bstring fontifier id)
   (list lines lang fontifier id))

;*---------------------------------------------------------------------*/
;*    *quote-code-block-grammar* ...                                   */
;*---------------------------------------------------------------------*/
(define *quote-code-block-grammar*
   (regular-grammar ((line (+ (out "`\n,$")))
		     (crlf (or "\n" "\r\n"))
		     lines conv eof)
      
      (define (include-string str::bstring lines)
         ;;; include a new sequence of characters
	 (call-with-input-string str
	    (lambda (ip)
	       (let loop ((lines '()))
		  (let ((line (read-line-newline ip)))
		     (if (eof-object? line)
			 lines
			 (loop (cons line lines))))))))
      
      ;; newline
      ((+ crlf)
       (set! lines (cons (the-string) lines))
       (ignore))
      ;; punctuation
      ((** 1 2 (in "*_`"))
       (set! lines (cons (the-string) lines))
       (ignore))
      ((or #\$ #\, (** 1 2 #\`))
       (set! lines (cons (the-string) lines))
       (ignore))
      ;; plain line of code
      ((: line)
       (set! lines (cons (conv (the-substring 0 (the-length))) lines))
       (ignore))
      ((bol (: "```" (* (in " \t")) (? #\Return) #\Newline))
       ;; skip the last crlf
       (if (pair? lines)
	   (reverse! (cdr lines))
	   '()))
      ((bol ",(")
       (rgc-buffer-unget-char (the-port) (char->integer #\())
       (set! lines (cons (ctor 'expr '() (read (the-port)) #f) lines))
       (ignore))
      (else
       (if (eof-object? (the-failure))
	   (if eof
	       lines
	       (error "unexpected eof" (the-failure) (the-port)))
	   (error "Illegal character" (string (the-failure)) (the-port))))))

;*---------------------------------------------------------------------*/
;*    state-add! ...                                                   */
;*---------------------------------------------------------------------*/
(define (state-add! state::MDState e)
   (unless (eq? e #unspecified)
      (with-access::MDState state (elements)
	 (set! elements (cons e elements)))))

;*---------------------------------------------------------------------*/
;*    *markdown-grammar* ...                                           */
;*---------------------------------------------------------------------*/
(define *markdown-grammar*
   (regular-grammar ((punct (in "+=/-$~#%!'\""))
		     (punct+ (or punct #\newline #\return))
		     (blank (in "<>^|:~;,{} ."))
		     (letter (out "&<>+^|*=/_-$#%:~;,\"`'[](){}! \\\n\t0123456789"))
		     (letter+ (or letter digit))
		     (ident (: #\: (+ letter) (in " \t\n\r")))
		     (crlf (or "\n" "\r\n"))
		     charset fontifier eval)
      
      (define (parse-string string charset fontifier)
       (call-with-input-string string
	  (lambda (ip)
	     (markdown-parse-elements ip charset fontifier eval))))
      
      ;; utf-8 bom
      ((bof (: #a239 #a187 #a191))
       (token 'CHARSET 'UTF-8 3))
      
      ;; utf-16 big endian
      ((bof (: #a254 #a255))
       ;; MS 23nov2011: CARE I don't know if ucs-2 is big or little endian
       (token 'CHARSET 'UCS-2 2))
      
      ;; utf-16 little endian
      ((bof (: #a255 #a254))
       ;; MS 23nov2011: CARE I don't know if ucs-2 is big or little endian
       (token 'CHARSET 'UCS-2 2))
      
      ;; comments
      ((bol (or ";*" ";;"))
       (let ((cset (read/rp *comment-grammar* (the-port))))
	  (token 'CHARSET (or cset 'ascii) (the-length))))
      ;; HTML comments
      ((: "<!--" (+ (or (out #\-) (: #\- (out #\-)) (: "--" (out #\>)))) "-->")
       (let ((s (the-substring 4 -3)))
	  (cond
	     ((pregexp-match "[ ]*nodisplay[ ]*" s)
	      (let loop ()
		 (let ((line (read-line (the-port))))
		    (if (pregexp-match "<!--[ ]*/nodisplay[ ]*-->" line)
			(ignore)
			(loop)))))
	     ((pregexp-match "[ ]*github[ ]*" s)
	      (let loop ()
		 (let ((line (read-line (the-port))))
		    (if (pregexp-match "<!--[ ]*/github[ ]*-->" line)
			(ignore)
			(loop)))))
	     ((pregexp-match "[ ]*\\[:([^\\]@]+)\\][ ]*" s)
	      =>
	      (lambda (m)
		 (token 'IDCLA (cons (cadr m) #f) (the-length))))
	     ((pregexp-match "[ ]*\\[:@([^\\]@]+)\\][ ]*" s)
	      =>
	      (lambda (m)
		 (token 'IDCLA (cons #f (cadr m)) (the-length))))
	     ((pregexp-match "[ ]*\\[:([^\\]@]+)@([^\\]\n]+)\\][ ]*" s)
	      =>
	      (lambda (m)
		 (token 'IDCLA (cons (cadr m) (caddr m)) (the-length))))
	     (else
	      (ignore)))))
      
      ;; continuation lines
      ((: #\\ (? #\Return) #\Newline)
       (ignore))
      
      ;; ident/class
      ((: "[:" (+ (out "]@")) "]")
       (token 'IDCLA (cons (the-substring 2 -1) #f) (the-length)))
      ;; class
      ((: "[:@" (+ (out "]@")) "]")
       (token 'IDCLA (cons #f (the-substring 3 -1)) (the-length)))
      ((: "[:" (+ (out "]@\n")) "@" (+ (out "]\n@")) "]")
       (let* ((str (the-substring 2 -1))
	      (i (string-index str #\@))
	      (ident (substring str 0 i))
	      (clazz (substring str (+fx i 1) (string-length str))))
	  (token 'IDCLA (cons ident clazz) (the-length))))
      
      ;; url
      ((: "<" (+ (out "> \t\n")) "://" (+ (out "> \t\n")) ">")
       (token 'URL (the-substring 1 -1) (the-length)))
      
      ;; blank lines, paragraph
      ((: (? #\Return) #\Newline)
       (token 'newline (the-string) (the-length)))
      ;; blank lines, paragraph
      ((>= 2 (: (* (or (in " \t\n") "\r\n")) (? "\r") "\n"))
       (token 'STOP (the-string) (the-length)))
      
      ;; horizontal rules
      ((bol (: (>= 3 (: (in "*-") (* #\space))) crlf))
       (token 'HR (the-string) (the-length)))
      
      ;; sections
      ((bol (: (+ #\#) (* (in " \t")) (: (out #\#) (* all)) #\Newline))
       (let* ((str (the-string))
	      (len (the-length))
	      (depth (string-skip str "#"))
	      (end (string-skip-right str "\r\n#"))
	      (j (string-skip str " \t" depth))
	      (title (substring str j (if end (+fx end 1) len))))
	  (token (section-tag (-fx j 1)) (list title) (the-length))))
      
      ;; second form of sections
      ((bol (: (+ (out #\# #\Newline)) #\Newline (+ #\=) #\Newline))
       (let* ((str (the-string))
	      (i (string-index str #\Newline))
	      (els (parse-string (substring str 0 i)
		      charset fontifier)))
	  (token 'h1 (inline-els 'p els) (the-length))))
      ((bol (: (+ (out #\# #\Newline)) #\Newline (+ #\-) #\Newline))
       (let* ((str (the-string))
	      (i (string-index str #\Newline))
	      (els (parse-string (substring str 0 i)
		      charset fontifier)))
	  (token 'h2 (inline-els 'p els) (the-length))))
      
      ;; code block mode
      ((bol (: (or "    " "\t") (* (in " \t")) (out "*- \t\n")))
       (let ((str (the-string)))
	  (token 'pre str (the-length))))
      
      ;; program
      ((bol (: "```" (* (out "\n")) #\Newline))
       (let* ((prog (the-substring 3 -1))
	      (i (string-skip-right prog " \t")))
	  (token 'prog (if i (substring prog 0 (+fx i 1)) prog)
	     (-fx (the-length) 3))))
      
      ;; itemize
      ((bol (: (* " ") (in "*+-") (** 1 3 " ")))
       (let ((str (the-string)))
	  (token 'ul (cons (string-skip str " ") str) (the-length))))
      ;; enumerate
      ((bol (: (* " ") (+ digit) #\. (** 1 3 " ")))
       (let ((str (the-string)))
	  (token 'ol (cons (string-skip str " ") str) (the-length))))
      
      ;; blockquote
      ((bol (+ (: (* (in " \t")) ">" (in " \t"))))
       (token 'BLOCKQUOTE (the-string) (the-length)))
      
      ;; code
      ("`"
       (token 'code "`" 1))
      ("``"
       (token 'code2 "``" 2))
      
      ;; emphasize/strong
      ((: "_" (out " _*\r\n\t:.;,?!"))
       (let ((str (the-string)))
	  (rgc-buffer-unget-char (the-port)
	     (char->integer (string-ref str (-fx (the-length) 1))))
	  (token 'O_ "_" 1)))
      ((: "__" (out " _*\r\n\t:.;,?!"))
       (let ((str (the-string)))
	  (rgc-buffer-unget-char (the-port)
	     (char->integer (string-ref str (-fx (the-length) 1))))
	  (token 'O__ "__" 2)))
      ((: "*" (out " *_\r\n\t:.;,?!"))
       (let ((str (the-string)))
	  (rgc-buffer-unget-char (the-port)
	     (char->integer (string-ref str (-fx (the-length) 1))))
	  (token 'O* "*" 1)))
      ((: "**" (out " *_\r\n\t:.;,?!"))
       (let ((str (the-string)))
	  (rgc-buffer-unget-char (the-port)
	     (char->integer (string-ref str (-fx (the-length) 1))))
	  (token 'O** "**" 2)))
      
      ("_" 
       (token 'O_ (the-string) 1))
      ("__" 
       (token 'O__ (the-string) 2))
      ("___" 
       (token 'text "_" 1))
      ("*" 
       (token 'O* (the-string) 1))
      ("**" 
       (token 'O** (the-string) 2))
      ("***" 
       (token 'text "*" 1))
      
      ;; links
      (#\[
       (token 'OBRA (the-string) 1))
      (#\]
       (token 'CBRA (the-string) 1))
      (#\(
       (token 'OPAR (the-string) 1))
      (#\)
       (token 'CPAR (the-string) 1))
      
      ;; alert (github extension)
      ((: "[!" (* (in ("AZ"))) "]")
       (token 'ALERT (the-substring 2 -1) 2))
      ;; image
      ("!["
       (token 'IMAGE (the-string) 2))
      
      ;; embedded hop
      (",("
       (let ((pos (input-port-position (the-port))))
	  (rgc-buffer-unget-char (the-port) (char->integer #\())
	  (with-handler
	     (lambda (e)
		(if (isa? e &error)
		    (with-access::&error e (obj msg)
		       (exception-notify e)
		       (raise
			  (instantiate::&io-read-error
			     (fname (input-port-name (the-port)))
			     (location pos)
			     (proc "markdown-parser")
			     (msg msg)
			     (obj obj))))
		    (raise e)))
	     (let* ((e (read (the-port)))
		    (s (eval e)))
		(if (string? s)
		    (begin
		       (rgc-buffer-insert-substring! (the-port) s 0 (string-length s))
		       (ignore))
		    (error "markdown"
		       (format "Expression \"~s\" does not evaluate to a string" e)
		       s))))))
      ;; embedded html
      ((: "<" (+ letter+) (or " " ">" "/>"))
       (let ((s (the-string)))
	  (rgc-buffer-insert-substring! (the-port) s 0 (string-length s))
	  (token 'html (html-parse (the-port) :procedure markdown-element :eoi (lambda (o) #t)) 0)))
      
      ;; escaped characters
      ((: "\\" (in ".`*_{}[]()#+-!>,~$&"))
       (token 'text (the-substring 1 2) (the-length)))
      
      ;; single escape characters
      ((or punct blank #\space #\\ #\&)
       (token 'text (the-string) (the-length)))
      
      ;; HTML character
      ((: #\&
          (or (: "#x" (+ (in ("09afAF"))))
	      (: "#" (+ (in ("09"))))
	      (+ (in ("azAZ"))))
	  #\;)
       (token 'CHAR (the-string) (the-length)))
      
      ;; simple text
      ((: (? (** 1 3 (in " \t"))) letter (+ (or letter+ (: punct+ letter+) (: #\space letter+))))
       (token 'text (the-string) (the-length)))
      ((+ (or digit letter))
       (token 'text (the-string) (the-length)))
      
      (else
       (let ((c (the-failure)))
	  (cond
	     ((eof-object? c)
	      (token 'EOF c 0))
	     (else
	      (error "Unexpected character" (the-failure) (the-port))))))))

;*---------------------------------------------------------------------*/
;*    markdown-parse-elements ...                                      */
;*---------------------------------------------------------------------*/
(define (markdown-parse-elements ip::input-port charset fontifier eval)

   (define unresolved-refs
      '())

   (define definitions
      (make-hashtable))

   (define (resolve-references! dom)
      (for-each (lambda (unresolved)
		   (let* ((id (car unresolved))
			  (node (cadr unresolved))
			  (def (hashtable-get definitions id)))
		      (if def
			  (begin
			     (tprint "DEF " def))
			  (parse-token-error "Unbound reference"
			     (caddr unresolved)))))
	 unresolved-refs))

   (define (parse-token-error msg token::pair)
      (let ((msg (if (eq? (token-tag token) 'BAD)
		     (cadr token)
		     (format msg (token-tag token))))
	    (obj (if (eq? (token-tag token) 'BAD)
		     (cddr token)
		     (token-value token))))
	 (match-case (token-loc token)
	    ((at ?fname ?loc)
	     (raise
		(instantiate::&io-parse-error
		   (proc "markdown")
		   (msg msg)
		   (obj obj)
		   (fname fname)
		   (location loc))))
	    (else
	     (raise
		(instantiate::&io-parse-error
		   (proc "markdown")
		   (msg msg)
		   (obj obj)))))))
   
   (define (parse-error msg obj)
      (let ((fname (input-port-name ip))
	    (loc (input-port-position ip)))
	 (raise
	    (instantiate::&io-parse-error
	       (proc "markdown")
	       (msg msg)
	       (obj (read-line ip))
	       (fname fname)
	       (location loc)))))
   
   (define *peeked-tokens* '())
   (define *previous-token-type* #unspecified)
   
   (define (peek-token)
      (if (null? *peeked-tokens*)
	  (begin
	     (set! *peeked-tokens*
		(list (read/rp *markdown-grammar* ip charset fontifier eval)))
	     (car *peeked-tokens*))
	  (car *peeked-tokens*)))
   
   (define (token-push-back! token)
      (set! *peeked-tokens* (cons token *peeked-tokens*)))
   
   (define (peek-token-type)
      (car (peek-token)))
   
   (define (peek-token-value)
      (cdr (peek-token)))
   
   (define (consume-token! type)
      (let ((token (consume-any!)))
	 (if (eq? (token-tag token) type)
	     token
	     (parse-token-error 
		(format "expected \"~a\" got \"~a\"" type (token-tag token))
		token))))
   
   (define (consume! type)
      (cdr (consume-token! type)))
   
   (define (consume-any!)
      (let ((res (peek-token)))
	 (set! *previous-token-type* (car res))
	 (set! *peeked-tokens* (cdr *peeked-tokens*))
	 res))
   
   (define conv
      (lambda (t) t))

   (define (parse-span-elements state::MDState ctor::procedure end bra-as-text o-as-text)
      (cond
	 ((eq? (peek-token-type) end)
	  (consume-any!)
	  (values 'span (ctor (reverse! (-> state elements)))))
	 ((and bra-as-text (memq (peek-token-type) '(OBRA CBRA)))
	  (state-add! state (token-value (consume-any!)))
	  (parse-span-elements state ctor end bra-as-text o-as-text))
	 ((and o-as-text (memq (peek-token-type) '(O_ O__ O* O**)))
	  (state-add! state (token-value (consume-any!)))
	  (parse-span-elements state ctor end bra-as-text o-as-text))
	 ((span state)
	  (parse-span-elements state ctor end bra-as-text o-as-text))
	 (else
	  (case (peek-token-type)
	     ((newline SOP)
	      (values 'unwind state))
	     (else
	      (values 'unwind state))))))

   (define (parse-span state::MDState ctor::procedure end cleanup
	      #!key wrapper bra-as-text o-as-text)
      (let ((sstate::MDState (instantiate::MDState
				(tag 'span)
				(wrapper wrapper))))
	 (multiple-value-bind (retcode val)
	    (parse-span-elements sstate ctor end bra-as-text o-as-text)
	    (case retcode
	       ((span)
		(state-add! state val))
	       ((unwind)
		(set! (-> state attributes)
		   (append (-> sstate attributes) (-> state attributes)))
		(state-add! state cleanup)
		(for-each (lambda (e)
			     (state-add! state e))
		   (-> sstate elements)))
	       (else
		(tprint "UNKNOWN RETCODE " retcode))))))

   (define (end-subblock state::MDState)
      (let ((el (markdown-element (-> state tag) (-> state attributes) (reverse! (-> state elements)))))
	 (state-add! (-> state parent) el)
	 (-> state parent)))
      
   (define (end-blocks state::MDState)
      (let ((el (markdown-element (-> state tag) (-> state attributes) (reverse! (-> state elements)))))
	 (if (not (-> state parent))
	     el
	     (begin
		(state-add! (-> state parent) el)
		(end-blocks (-> state parent))))))
      
   (define (hr state::MDState)
      (let ((hr (case (peek-token-type)
		   ((IDCLA)
		    (let ((v (token-value (consume-any!))))
		       (ctor 'HR `((id . ,(car v)) (class . ,(cdr v))) '() #f)))
		   (else
		    (ctor 'HR '() '() #f)))))
	 (if (pair? (-> state elements))
	     (values 'blocks (list hr (end-blocks state)))
	     (values 'block hr))))

   

   (define (section state::MDState)
      (let* ((tok (consume-any!))
	     (title (token-value tok)) 
	     (tag (token-tag tok))
	     (id #f))
	 (if (symbol? tag)
	     (let* ((s (case (peek-token-type)
			  ((IDCLA)
			   (let ((v (token-value (consume-any!))))
			      (set! id (or (car v) (symbol->string (gensym))))
			      (markdown-element tag `((id . ,id) (class . ,(cdr v))) title)))
			  (else
			   (set! id (symbol->string (gensym)))
			   (markdown-element tag `((id . ,id)) title)))))
		(if (pair? (-> state elements))
		    (values 'blocks (list s (end-blocks state)))
		    (values 'blocks (list s))))
	     (begin
		(state-add! state title)
		(block state #t)))))

   (define (li state::MDState token)
      (let ((li (instantiate::MDState
		   (tag 'li)
		   (value (car (token-value token)))
		   (parent state))))
	 (block li #t)))

   (define (ol-value token)
      (let* ((str (cdr (token-value token)))
	     (m (pregexp-match "[ ]*([[:digit:]]+)" str)))
	 (when (pair? m)
	    (cadr m))))
   
   (define (start-itemize-no-p state::MDState token)
      (let ((ul (instantiate::MDState
		   (tag (car token))
		   (value (car (token-value token)))
		   (parent state))))
	 (li ul token)))

   (define (start-itemize-in-p state::MDState token)
      (if (-> state parent)
	  (start-itemize-no-p (end-subblock state) token)
	  (let* ((ul (instantiate::MDState
			(tag (car token))
			(value (car (token-value token)))
			(parent #f))))
	     (multiple-value-bind (recode val)
		(li ul token)
		(values 'blocks (list (end-blocks state) val))))))
   
   (define (start-itemize state::MDState token)
      (if (eq? (-> state tag) 'p)
	  (start-itemize-in-p state token)
	  (start-itemize-no-p state token)))

   (define (itemize state::MDState token bob::bool)
      (let loop ((state::MDState state))
	 (cond
	    ((eq? (-> state tag) 'li)
	     (let ((parent::MDState (-> state parent)))
		(cond
		   ((>fx (car (token-value token)) (-> state value))
		    (start-itemize state token))
		   ((<fx (car (token-value token)) (-> state value))
		    (loop (end-subblock (end-subblock state))))
		   ((not (eq? (car token) (-> parent tag)))
		    (loop (end-subblock (end-subblock state))))
		   ((=fx (car (token-value token)) (-> state value))
		    (li (end-subblock state) token))
		   (else
		    (loop (end-subblock (end-subblock state)))))))
	    ((or (and bob (eq? (-> state tag) 'p))
		 (memq (-> state tag) '(HR h1 h2 h3 h4 h5)))
	     (start-itemize state token))
	    (else
	     (state-add! state (cdr (token-value token)))
	     (block state #f)))))

   (define (parse-prog-lang lang-id)
      (if (not (string-index lang-id "["))
	  (values lang-id #f #f)
	  (cond
	     ((pregexp-match "([^[]+)[ \t]*[[]:([^\\]]*)\\]" lang-id)
	      =>
	      (lambda (m)
		 (let* ((lang (cadr m))
			(id-class (caddr m))
			(i (string-index id-class #\@)))
		    (if (not i)
			(values lang id-class #f)
			(values lang
			   (substring id-class 0 i)
			   (substring id-class (+fx i 1)))))))
	     (else
	      (values lang-id #f #f)))))
	 
   (define (prog state::MDState lang-id)
      (multiple-value-bind (lang id class)
	 (parse-prog-lang lang-id)
	 (let* ((lines (read/rp *quote-code-block-grammar* ip '() conv #f))
		(body (cond
			 ((string-null? lang)
			  lines)
			 ((null? lines)
			  lines)
			 ((not (pair? lines))
			  '())
			 (else
			  (fontify-code lines lang fontifier
			     (cond
				((assq 'id (-> state attributes)) => cdr)
				(else '())))))))
	    (values 'block
	       (markdown-element 'pre
		  `((id . ,id)
		    (class . ,(cond
				 ((and (string? class) (string? lang))
				  (string-append "fontifier-prog " class " " lang))
				 ((string? class)
				  (string-append "fontifier-prog " class))
				 ((string? lang)
				  (string-append "fontifier-prog " lang))
				 (else
				  "fontifier-prog"))))
		  
		  (list
		     (markdown-element 'code
			(if (and (string? lang) (>fx (string-length lang) 0))
			    `((class . ,(string-append "language-" lang)))
			    '())
			body)))))))

   (define (pre state::MDState token indent)
      (let ((start (token-value token)))
	 (rgc-buffer-insert-substring! ip start 0 (string-length start)))
      (let ((lines (read/rp *tab-code-block-grammar* ip
		      indent '() conv)))
	 (values 'block
	    (markdown-element 'pre '() (list (markdown-element 'code '() lines))))))

   (define (href)
      (let (url (title #f))
	 (case (peek-token-type)
	    ((text)
	     (let loop ((str (list (conv (token-value (consume-any!))))))
		(if (eq? (peek-token-type) 'text)
		    (loop (cons (conv (token-value (consume-any!))) str))
		    (let* ((str (apply string-append (reverse! str)))
			   (i (string-index str #\space)))
		       (if i
			   (begin
			      (set! url (substring str 0 i))
			      (set! title (substring str
					     (+fx i 2)
					     (-fx (string-length str) 1))))
			   (set! url str))))))
	    ((expr)
	     (set! url (token-value (consume-any!)))
	     (cond
		((memq (peek-token-type) '(expr text))
		 (set! title (token-value (consume-any!))))
		((eq? (peek-token-type) 'CPAR)
		 #f)
		(else
		 (parse-token-error "Illegal href token \"~a\"" (consume-any!))))))
	 (values url title)))

   (define (lref)
      (let loop ((res ""))
	 (case (peek-token-type)
	    ((text)
	     (loop (string-append res (token-value (consume-any!)))))
	    ((expr)
	     (loop (string-append res (token-value (consume-any!)))))
	    ((CBRA)
	     res)
	    (else
	     (loop (string-append res (token-value (consume-any!))))))))

   (define (skip-spaces)
      (let loop ((res #f))
	 (if (and (eq? (peek-token-type) 'text)
		  (string=? (token-value (peek-token)) " "))
	     (begin
		(consume-any!)
		(loop #t))
	     res)))

   (define (read-string bound)
      (let loop ((acc '()))
	 (let ((tok (consume-token! 'text)))
	    (if (string=? (token-value tok) bound)
		(apply string-append (reverse! acc))
		(loop (cons (token-value tok) acc))))))
   
   (define (definition-url prefix)
      (let loop ((acc (list prefix)))
	 (case (peek-token-type)
	    ((text O_)
	     (let ((str (token-value (consume-any!))))
		(cond
		   ((string=? str "")
		    (apply string-append (reverse! acc)))
		   ((pregexp-match "[ \r\n\t]+" str)
		    (apply string-append (reverse! acc)))
		   (else
		    (loop (cons str acc))))))
	    ((newline STOP)
	     (apply string-append (reverse! acc)))
	    (else
	     (let ((tok (consume-any!)))
		(parse-token-error
		   (format "Illegal url \"~a\" ~~a" (token-value tok))
		   tok))))))

   (define (definition-title)
      (when (skip-spaces)
	 (when (eq? (peek-token-type) 'text)
	    (when (string-index (token-value (peek-token)) "'\"(")
	       (read-string (token-value (consume-any!)))))))
      
   (define (definition link)
      (let loop ()
	 (case (peek-token-type)
	    ((text)
	     (let ((str (definition-url (token-value (consume-any!)))))
		(cond
		   ((pregexp-match "[ \t]{1,3}" str)
		    =>
		    (lambda (m)
		       (if (=fx (string-length (car m)) (string-length str))
			   (loop)
			   (let* ((url (definition-url
					  (substring str (string-length (car m)))))
				  (title (definition-title)))
			      (hashtable-put! definitions link
				 (cons url title)))))))))
	    ((URL expr)
	     (let* ((url (token-value (consume-any!)))
		    (title (definition-title)))
		(hashtable-put! definitions link
		   (cons url title))))
	    ((newline)
	     (consume-any!)
	     (loop))
	    (else
	     (parse-token-error "Illegal definition \"~a\"" (consume-any!))))))

   (define (reference state::MDState)
      (let* ((lstate::MDState (instantiate::MDState
				 (tag 'a)
				 (parent state)))
	     (level (let loop ((level 0))
		       ;; count the number of "[" character that are part
		       ;; of the title string
		       (if (eq? (peek-token-type) 'OBRA)
			   (begin
			      (consume-any!)
			      (state-add! lstate "[")
			      (loop (+fx level 1)))
			   level))))
	 (if (span lstate)
	     (begin
		;; link title
		(let loop ((level level))
		   (if (eq? (peek-token-type) 'CBRA)
		       (let ((tok (consume-token! 'CBRA)))
			  (if (=fx level 0)
			      tok
			      (begin
				 (state-add! lstate "]")
				 (loop (-fx level 1)))))
		       (begin
			  (state-add! lstate
			     (conv (token-value (consume-any!))))
			  (loop level))))
		;; link url
		(let loop ()
		   (case (peek-token-type)
		      ((OPAR)
		       (consume-any!)
		       (multiple-value-bind (url title)
			  (href)
			  (consume-token! 'CPAR)
			  (state-add! state
			     (markdown-element 'URL `((url . url) (title . title))
				(reverse! (-> lstate elements))))))
		      ((text)
		       (let ((tok (consume-any!)))
			  (cond
			     ((string=? (token-value tok) " ")
			      (loop))
			     ((string=? (token-value tok) ":")
			      (definition (car (-> lstate elements))))
			     (else
			      (parse-token-error "Illegal text reference token \"~a\""
				 tok)))))
		      ((OBRA)
		       (let* ((token (consume-any!))
			      (ref (lref))
			      (el (markdown-element 'LINK '() (reverse! (-> lstate elements)))))
			  (set! unresolved-refs
			     (cons (list ref el token) unresolved-refs))
			  (consume-token! 'CBRA)
			  (state-add! state el)))
		      (else
		       (parse-token-error "Illegal reference token \"~a\""
			  (consume-any!))))))
	     (parse-token-error "Illegal reference token \"~a\""
		(consume-any!)))))

   (define (src-align src pwd srcpath)
      (let* ((apath (file-name-canonicalize (make-file-name pwd srcpath)))
	     (asrc (file-name-canonicalize (make-file-name (dirname apath) src))))
	 (relative-file-name asrc pwd)))
	    
   (define (span state::MDState)
      (case (peek-token-type)
	 ((text OPAR CPAR newline)
	  (state-add! state (conv (token-value (consume-any!))))
	  #t)
	 ((CHAR)
	  (state-add! state (token-value (consume-any!)))
	  #t)
	 ((URL)
	  (let ((url (conv (token-value (consume-any!)))))
	     (state-add! state (markdown-element 'A `((url . url)) url)))
	  #t)
	 ((O_)
	  (let ((val (token-value (consume-any!))))
	     (parse-span state (lambda (l) (markdown-element 'em '() l)) 'O_ val))
	  #t)
	 ((O*)
	  (let ((val (token-value (consume-any!))))
	     (parse-span state (lambda (l) (markdown-element 'em '() l)) 'O* val))
	  #t)
	 ((O__)
	  (let ((val (token-value (consume-any!))))
	     (parse-span state (lambda (l) (markdown-element 'STRONG '() l)) 'O__ val))
	  #t)
	 ((O**)
	  (let ((val (token-value (consume-any!))))
	     (parse-span state (lambda (l) (markdown-element 'STRONG '() l)) 'O** val))
	  #t)
	 ((code)
	  (let ((val (token-value (consume-any!))))
	     (if (eq? (-> state wrapper) 'code2)
		 (state-add! state "`")
		 (parse-span state (lambda (l) (markdown-element 'code '() l)) 'code val
		    :bra-as-text #t
		    :o-as-text #t)))
	  #t)
	 ((code2)
	  (let ((val (token-value (consume-any!))))
	     (parse-span state (lambda (l) (markdown-element 'code '() l)) 'code2 val
		:wrapper 'code2
		:bra-as-text #t
		:o-as-text #t))
	  #t)
	 ((IDCLA)
	  (let ((v (token-value (consume-any!))))
	     (set! (-> state attributes)
		`((id . ,(car v)) (class . ,(cdr v)) (-> state attributes))))
	  #t)
	 ((OBRA)
	  (consume-any!)
	  (reference state)
;* 	     (lambda (url title body)                                  */
;* 		(ctor 'A (list :href url :title title) body)))         */
	  #t)
	 ((IMAGE)
	  (let ((tok (consume-any!)))
	     (reference state))
;* 		(lambda (src title body)                               */
;* 		   (ctor 'IMG                                          */
;* 		      (list                                            */
;* 			 :src src                                      */
;* 			 :pwd (pwd)                                    */
;* 			 :title title :alt body)                       */
;* 		      '()))))                                          */
	  #t)
	 ((expr)
	  (state-add! state (token-value (consume-any!)))
	  #t)
	 ((ERROR)
	  (state-add! state
	     (error "markdown" "parse error" (token-value (consume-any!)))))
	 (else
	  #f)))

   (define (blockquote-level tok)
      (let ((str (token-value tok)))
	 (let loop ((i 0)
		    (l 0))
	    (let ((ni (string-index str #\> i)))
	       (if ni
		   (loop (+fx ni 1) (+fx l 1))
		   l)))))


   (define (start-blockquote state::MDState tok level klass)
      (let ((nstate::MDState (instantiate::MDState
				(tag 'blockquote)
				(value level)
				(parent state))))
	 (block nstate #f)))

   (define (blockquote-string str)
      (let ((i (string-index-right str ">")))
	 (substring str (+fx i 1))))
      
   (define (blockquote state::MDState token bob::bool)
      (let ((level (blockquote-level token))
	    (klass (when (eq? (peek-token-type) 'ALERT)
		      (token-value (consume-any!)))))
	 (let loop ((state::MDState state))
	    (cond
	       ((eq? (-> state tag) '>)
		(let ((parent::MDState (-> state parent)))
		   (cond
		      ((>fx level (-> state value))
		       (start-blockquote state token level klass))
		      ((=fx level (-> state value))
		       (state-add! state
			  (blockquote-string (token-value token)))
		       (block state #f))
		      ((<fx level (-> state value))
		       (loop (end-subblock state)))
		      (else
		       (loop (end-subblock state))))))
	       ((or (memq (-> state tag) '(hr section))
		    (and bob (eq? (-> state tag) 'p)))
		(start-blockquote state token level klass))
	       (else
		(state-add! state (blockquote-string (token-value token)))
		(block state #f))))))

   (define (block state::MDState bob)
      (cond
	 ((eq? (peek-token-type) 'expr)
	  (let ((el (token-value (consume-any!))))
	     (cond
		((pair? el)
		 (state-add! state (flatten el))
		 (block state #f))
		(else
		 (values 'blocks (list (end-blocks state) el))))))
	 ((eq? (peek-token-type) 'STOP)
	  (consume-any!)
	  (cond
	     ((and (eq? (-> state tag) 'li)
		   (eq? (peek-token-type) 'pre)
		   (string-prefix? "        " (token-value (peek-token))))
	      (multiple-value-bind (retcode val)
		 (pre state (consume-any!) 8)
		 (state-add! state val)
		 (block state #f)))
	     ((and (eq? (-> state tag) 'li)
		   (let ((parent::MDState (-> state parent)))
		      (eq? (peek-token-type) (-> parent tag))))
	      (let ((parent::MDState (-> state parent)))
		 (set! (-> parent wrapper) 'p)
		 (block state #t)))
	     (bob
	      ;; ignore newline at beginning of block
	      (block state #t))
	     (else
	      (values 'block (end-blocks state)))))
	 ((and (eq? (peek-token-type) 'newline) bob)
	  (consume-any!)
	  (block state #t))
	 ((span state)
	  (block state #f))
	 (else
	  (case (peek-token-type)
	     ((HR)
	      (consume-any!)
	      (hr state))
	     ((EOF)
	      (if (pair? (-> state elements))
		  (values 'eof (end-blocks state))
		  (values 'eof #f)))
	     ((h1 h2 h3 h4 h5)
	      (section state))
	     ((ul ol)
	      (itemize state (consume-any!) bob))
	     ((BLOCKQUOTE)
	      (blockquote state (consume-any!) bob))
	     ((prog)
	      (if bob
		  (prog state (token-value (consume-any!)))
		  (values 'block (end-blocks state))))
	     ((pre)
	      (let ((tok (consume-any!)))
		 (cond
		    ((and bob (string-prefix? " " (token-value tok)))
		     (pre state tok  4))
		    (bob
		     (pre state tok 1))
		    ((and (eq? (-> state tag) 'li)
			  (string-prefix? "        " (token-value tok)))
		     (pre state tok 8))
		    ((and (eq? (-> state tag) 'li)
			  (string-prefix? "\t\t" (token-value tok)))
		     (pre state tok 2))
		    (else
		     (state-add! state (token-value tok))
		     (block state #f)))))
	     ((html)
	      (let ((tok (consume-any!)))
		 (state-add! state (car (token-value tok)))
		 (block state #f)))
	     (else
	      (parse-token-error "Illegal block token \"~a\"" (consume-any!)))))))

   (define (blocks)
      (let loop ((els '()))
	 (let ((state (instantiate::MDState
			 (tag 'p))))
	    (multiple-value-bind (retcode val)
	       (block state #t)
	       (case retcode
		  ((eof)
		   (reverse! (if val (cons val els) els)))
		  ((block)
		   (loop (cons val els)))
		  ((blocks)
		   (loop (append (reverse! val) els))))))))

   (blocks))

;*---------------------------------------------------------------------*/
;*    markdown-parse ...                                               */
;*---------------------------------------------------------------------*/
(define (markdown-parse ip::input-port #!key charset fontifier eval)
   (markdown-element 'html '() (markdown-parse-elements ip charset fontifier eval)))

;*---------------------------------------------------------------------*/
;*    *margins* ...                                                    */
;*---------------------------------------------------------------------*/
(define *margins*
   '#("" " " "  " "   " "    " "     " "      " "       " "        "))

;*---------------------------------------------------------------------*/
;*    margin ...                                                       */
;*---------------------------------------------------------------------*/
(define (margin m)
   (when (>=fx m (vector-length *margins*))
      (set! *margins* (make-vector (+fx m 1)))
      (let loop ((i 0))
	 (when (<=fx i m)
	    (vector-set! *margins* i (make-string i #\space))
	    (loop (+fx i 1)))))
   (vector-ref *margins* m))

;*---------------------------------------------------------------------*/
;*    markdown->html ...                                               */
;*---------------------------------------------------------------------*/
(define (markdown->html html #!optional (op::output-port (current-output-port)))

   (define (disp m text)
      (display (margin m) op)
      (display text op))

   (define (disp-attrs attrs)
      (for-each (lambda (a)
		   (display " " op)
		   (display (car a) op)
		   (display "=\"" op)
		   (display (cdr a) op)
		   (display "\"" op))
	 attrs))

   (define (inline-element? el)
      (memq (ctor-tag el) '(text em strong)))
   
   (define (inline->html el m)
      (display "<")
      (display (ctor-tag el) op)
      (disp-attrs (ctor-attrs el))
      (display ">" op)
      (for-each (lambda (el) (el->html el m)) (ctor-children el))
      (display "</")
      (display (ctor-tag el) op)
      (display ">" op))
      
   (define (block->html el m)
      (disp m "<")
      (display (ctor-tag el) op)
      (disp-attrs (ctor-attrs el))
      (display ">" op)
      (when (pair? (ctor-children el))
	 (display "\n" op)
	 (for-each (lambda (el) (el->html el (+fx m 1))) (ctor-children el)))
      (disp m "</")
      (display (ctor-tag el) op)
      (display ">\n" op))

   (define (inline-block->html el m)
      (disp m "<")
      (display (ctor-tag el) op)
      (disp-attrs (ctor-attrs el))
      (display ">" op)
      (for-each (lambda (el) (el->html el (+fx m 1))) (ctor-children el))
      (display "</" op)
      (display (ctor-tag el) op)
      (display ">\n" op))

   (define (el->html el m)
      (case (ctor-tag el)
	 ((text)
	  (let ((v (ctor-children el)))
	     (display (if (string? v) (html-string-encode v) v) op)))
	 ((p)
	  (if (null? (ctor-children el))
	      (disp m "<p/>\n")
	      (block->html el m)))
	 ((h1 h2 h3 h4 h5)
	  (inline-block->html el m))
	 ((li div)
	  (if (every inline-element? (ctor-children el))
	      (inline-block->html el m)
	      (block->html el m)))
	 ((ul html)
	  (block->html el m))
	 (else
	  (inline->html el m))))

   (el->html html 0))

