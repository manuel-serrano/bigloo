;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Unsafe/pcre.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec  6 15:43:19 2011                          */
;*    Last change :  Tue Oct  8 18:25:57 2019 (serrano)                */
;*    Copyright   :  2011-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Posix regular expressions (PCRE)                                 */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    directives                                                       */
;*---------------------------------------------------------------------*/
(module __regexp
   
   (import  __error)
   
   (use     __type
            __bigloo
            __tvector
            __ucs2
            __dsssl
            __bexit
            __bignum
            __object
            __thread
	    __bit
            
            __r4_output_6_10_3
            
            __r4_numbers_6_5_fixnum
            __r4_numbers_6_5_flonum
            __r4_numbers_6_5
            __r4_equivalence_6_2
            __r4_vectors_6_8
            __r4_booleans_6_1
            __r4_characters_6_6
            __r4_symbols_6_4
            __r4_pairs_and_lists_6_3
            __r4_strings_6_7
            __r4_ports_6_10_1
            __r4_control_features_6_9

            __evenv)

   (extern ($regcomp::regexp (::bstring ::obj ::bool) "bgl_regcomp")
           (macro $regexp?::bool (::obj) "BGL_REGEXPP")
           (macro $regexp-pattern::bstring (::regexp) "BGL_REGEXP_PAT")
           (macro $regexp-capture-count::long (::regexp) "BGL_REGEXP_CAPTURE_COUNT")
	   (macro $regmatch::obj (::regexp ::string ::bool ::int ::int) "BGL_REGEXP_MATCH")
	   (macro $regmatch-n::long (::regexp ::string ::vector ::int ::int) "BGL_REGEXP_MATCH_N")
           (macro $regfree::obj (::regexp) "BGL_REGEXP_FREE"))
   
   (java   (class foreign
              (method static $regexp?::bool (::obj)
                 "BGL_REGEXPP")
              (method static $regexp-pattern::bstring (::obj)
                 "BGL_REGEXP_PAT")
              (method static $regexp-capture-count::int (::obj)
                 "BGL_REGEXP_CAPTURE_COUNT")
	      (method static $regcomp::regexp (::bstring ::obj ::bool)
		 "bgl_regcomp")
	      (method static $regmatch::obj (::regexp ::string ::bool ::int ::int)
		 "bgl_regmatch")
	      (method static $regmatch-n::long (::regexp ::string ::vector ::int ::int)
		 "bgl_regmatch_n")
	      (method static $regfree::obj (::regexp)
		 "bgl_regfree")))
 
   (export (inline regexp?::bool ::obj)
           (inline regexp-pattern::bstring ::regexp)
	   (inline regexp-capture-count::long ::regexp)
           (pregexp ::bstring . opt-args)
           (pregexp-match-positions pat str::bstring
	      #!optional (beg 0) (end (string-length str)))
	   (inline pregexp-match-n-positions!::long
	      ::regexp ::bstring ::vector ::long ::long)
           (pregexp-match pat str::bstring 
	      #!optional (beg 0) (end (string-length str)))
           (pregexp-replace::bstring pat ::bstring ins::bstring)
           (pregexp-split::pair-nil pat ::bstring)
           (pregexp-replace*::bstring pat ::bstring ins::bstring)
           (pregexp-quote::bstring ::bstring))

   (option (set! *arithmetic-genericity* #f)
           (set! *arithmetic-overflow* #f)))

;*---------------------------------------------------------------------*/
;*    regexp? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (regexp? obj)
   ($regexp? obj))

;*---------------------------------------------------------------------*/
;*    regexp-pattern ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (regexp-pattern re)
   ($regexp-pattern re))

;*---------------------------------------------------------------------*/
;*    regexp-capture-count ...                                         */
;*---------------------------------------------------------------------*/
(define-inline (regexp-capture-count re)
   ($regexp-capture-count re))

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
(define (pregexp re . opt-args)
   ($regcomp re opt-args #t))

;*---------------------------------------------------------------------*/
;*    match ...                                                        */
;*---------------------------------------------------------------------*/
(define (match pat str stringp beg end)
   (if (regexp? pat)
       ($regmatch pat str stringp beg end)
       (let* ((rx ($regcomp pat '() #f))
	      (val ($regmatch rx str stringp beg end)))
	  ($regfree rx)
	  val)))

;*---------------------------------------------------------------------*/
;*    pregexp-match-positions ...                                      */
;*---------------------------------------------------------------------*/
(define (pregexp-match-positions pat str #!optional (beg 0) (end (string-length str)))
   (match pat str #f beg end))

;*---------------------------------------------------------------------*/
;*    pregexp-match-n-positions! ...                                   */
;*---------------------------------------------------------------------*/
(define-inline (pregexp-match-n-positions! pat::regexp str vres beg end)
   ($regmatch-n pat str vres beg end))

;*---------------------------------------------------------------------*/
;*    pregexp-match ...                                                */
;*---------------------------------------------------------------------*/
(define (pregexp-match pat str #!optional (beg 0) (end (string-length str)))
   (match pat str #t beg end))

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

