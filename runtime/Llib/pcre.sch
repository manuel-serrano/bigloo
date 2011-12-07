;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/pcre.sch                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  6 15:43:19 2011                          */
;*    Last change :  Wed Dec  7 18:20:27 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Posix regular expressions                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(directives
   (extern
      ($regcomp::regexp (::bstring) "bgl_regcomp")
      ($regmatch::obj (::regexp ::string ::bool ::int ::int) "bgl_regmatch")
      ($regfree::obj (::regexp) "bgl_regfree")))

;*---------------------------------------------------------------------*/
;*    blit! ...                                                        */
;*---------------------------------------------------------------------*/
(define-macro (blit! s dest j)
   (let ((l (string-length s)))
      `(begin
	  (blit-string! ,s 0 ,dest ,j ,l)
	  ,l)))

;*---------------------------------------------------------------------*/
;*    pregexp ...                                                      */
;*---------------------------------------------------------------------*/
(define (pregexp re)
   ($regcomp re))

;*---------------------------------------------------------------------*/
;*    match ...                                                        */
;*---------------------------------------------------------------------*/
(define (match pat str stringp opt-args)
   (let ((beg 0)
	 (end (string-length str)))
      (when (pair? opt-args)
	 (set! beg (car opt-args))
	 (when (pair? (cdr opt-args))
	    (set! end (cadr opt-args))))
      (if (regexp? pat)
	  ($regmatch pat str stringp beg end)
	  (let* ((rx (pregexp pat))
		 (val ($regmatch rx str stringp beg end)))
	     ($regfree rx)
	     val))))

;*---------------------------------------------------------------------*/
;*    pregexp-match-positions ...                                      */
;*---------------------------------------------------------------------*/
(define (pregexp-match-positions pat str . opt-args)
   (match pat str #f opt-args))

;*---------------------------------------------------------------------*/
;*    pregexp-match ...                                                */
;*---------------------------------------------------------------------*/
(define (pregexp-match pat str . opt-args)
   (match pat str #t opt-args))

;*---------------------------------------------------------------------*/
;*    pregexp-read-escaped-number ...                                  */
;*---------------------------------------------------------------------*/
(define (pregexp-read-escaped-number s i n)
   ; s[i] = \
   (and (< (+ i 1) n) ;must have at least something following \
	(let ((c (string-ref s (+ i 1))))
	   (and (char-numeric? c)
		(let loop ((i (+ i 2)) (r (list c)))
		   (if (>= i n)
		       (list (string->number (list->string (reverse! r))) i)
		       (let ((c (string-ref s i)))
			  (if (char-numeric? c)
			      (loop (+ i 1) (cons c r))
			      (list (string->number (list->string (reverse! r)))
				 i)))))))))

;*---------------------------------------------------------------------*/
;*    pregexp-list-ref ...                                             */
;*---------------------------------------------------------------------*/
(define (pregexp-list-ref s i)
   ;like list-ref but returns #f if index is
   ;out of bounds
   (let loop ((s s) (k 0))
      (cond ((null? s) #f)
	    ((= k i) (car s))
	    (else (loop (cdr s) (+ k 1))))))

;*---------------------------------------------------------------------*/
;*    pregexp-replace-aux ...                                          */
;*---------------------------------------------------------------------*/
(define (pregexp-replace-aux str ins n backrefs)
   (let loop ((i 0) (r ""))
      (if (>= i n) r
          (let ((c (string-ref ins i)))
	     (if (char=? c #\\)
		 (let* ((br-i (pregexp-read-escaped-number ins i n))
			(br (if br-i (car br-i)
				(if (char=? (string-ref ins (+ i 1)) #\&) 0
				    #f)))
			(i (if br-i (cadr br-i)
			       (if br (+ i 2)
				   (+ i 1)))))
		    (if (not br)
			(let ((c2 (string-ref ins i)))
			   (loop (+ i 1)
			      (if (char=? c2 #\$) r
				  (string-append r (string c2)))))
			(loop i
			   (let ((backref (pregexp-list-ref backrefs br)))
			      (if backref
				  (string-append r
				     (substring str (car backref) (cdr backref)))
				  r)))))
		 (loop (+ i 1) (string-append r (string c))))))))

;*---------------------------------------------------------------------*/
;*    pregexp-split ...                                                */
;*---------------------------------------------------------------------*/
(define (pregexp-split pat str)
   ;split str into substrings, using pat as delimiter
   (let ((n (string-length str)))
      (let loop ((i 0) (r '()) (picked-up-one-undelimited-char? #f))
	 (cond ((>= i n) (reverse! r))
	       ((pregexp-match-positions pat str i n)
		=>
		(lambda (y)
		   (let ((jk (car y)))
		      (let ((j (car jk)) (k (cdr jk)))
			 (cond ((= j k)
				(loop (+ k 1) 
				   (cons (substring str i (+ j 1)) r) #t))
			       ((and (= j i) picked-up-one-undelimited-char?)
				(loop k r #f))
			       (else
				(loop k (cons (substring str i j) r) #f)))))))
	       (else (loop n (cons (substring str i n) r) #f))))))

;*---------------------------------------------------------------------*/
;*    pregexp-replace ...                                              */
;*---------------------------------------------------------------------*/
(define (pregexp-replace pat str ins)
   (let* ((n (string-length str))
	  (pp (pregexp-match-positions pat str 0 n)))
      (if (not pp) str
	  (let ((ins-len (string-length ins))
		(m-i (caar pp))
		(m-n (cdar pp)))
	     (string-append
		(substring str 0 m-i)
		(pregexp-replace-aux str ins ins-len pp)
		(substring str m-n n))))))

;*---------------------------------------------------------------------*/
;*    pregexp-replace* ...                                             */
;*---------------------------------------------------------------------*/
(define (pregexp-replace* pat str ins)
   ;return str with every occurrence of pat 
   ;replaced by ins
   (let ((pat (if (string? pat) (pregexp pat) pat))
	 (n (string-length str))
	 (ins-len (string-length ins)))
      (let loop ((i 0) (r ""))
	 ;i = index in str to start replacing from
	 ;r = already calculated prefix of answer 
	 (if (>= i n) r 
	     (let ((pp (pregexp-match-positions pat str i n)))
		(if (not pp) 
		    (if (= i 0)
			;this implies pat didn't match str at
			;all, so let's return original str
			str
			;else: all matches already found and
			;replaced in r, so let's just
			;append the rest of str
			(string-append
			   r (substring str i n)))
		    (loop (cdar pp)
		       (string-append
			  r
			  (substring str i (caar pp))
			  (pregexp-replace-aux str ins ins-len pp)))))))))

;*---------------------------------------------------------------------*/
;*    pregexp-quote ...                                                */
;*---------------------------------------------------------------------*/
(define (pregexp-quote s)
   (let loop ((i (- (string-length s) 1)) (r '()))
      (if (< i 0)
	  (list->string r)
          (loop (- i 1)
	     (let ((c (string-ref s i)))
		(if (memv c '(#\\ #\. #\? #\* #\+ #\| #\^ #\$
			      #\[ #\] #\{ #\} #\( #\)))
		    (cons #\\ (cons c r))
		    (cons c r)))))))

