;*=====================================================================*/
;*    /tmp/BUFOV/bigloo-4.3h/runtime/Unsafe/pregexp.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Dorai Sitaram                                     */
;*    Creation    :  Mon Jan 19 17:35:12 1998                          */
;*    Last change :  Fri Mar 27 18:08:35 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Posix regular expressions                                        */
;*    Portable regular expressions for Scheme                          */
;*    Dorai Sitaram                                                    */
;*    http://www.ccs.neu.edu/~dorai                                    */
;*    Oct 2, 1999                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
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

   (extern ($make-regexp::regexp (::bstring) "bgl_make_regexp")
           (macro $regexp?::bool (::obj) "BGL_REGEXPP")
           (macro $regexp-pattern::bstring (::regexp) "BGL_REGEXP_PAT")
           (macro $regexp-capture-count::long (::regexp) "BGL_REGEXP_CAPTURE_COUNT")
	   (macro $regexp-preg::obj (::regexp) "BGL_REGEXP_PREG")
	   (macro $regexp-preg-set!::obj (::regexp ::obj) "BGL_REGEXP_PREG_SET"))
   
   (java   (class foreign
	      (method static $make-regexp::regexp (::bstring)
		 "bgl_make_regexp")
	      (method static $regexp-preg::obj (::regexp)
		 "BGL_REGEXP_PREG")
	      (method static $regexp-preg-set!::obj (::regexp ::obj)
		 "BGL_REGEXP_PREG_SET")
	      (method static $regexp?::bool (::obj)
		 "BGL_REGEXPP")
	      (method static $regexp-pattern::bstring (::obj)
		 "BGL_REGEXP_PAT")
              (method static $regexp-capture-count::int (::obj)
                 "BGL_REGEXP_CAPTURE_COUNT")))
 
   (export (inline regexp?::bool ::obj)
	   (inline regexp-pattern::bstring ::regexp)
	   (inline regexp-capture-count::long ::regexp)
	   (pregexp ::bstring . opt-args)
	   (pregexp-match-positions pat str::bstring
	      #!optional (beg 0) (end (string-length str)))
	   (pregexp-match-n-positions!::long
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

(define (pregexp-error msg . opt)
   (error "pregexp" msg (if (pair? opt) (car opt) #unspecified)))

;pregexp.scm
;Portable regular expressions for Scheme
;Dorai Sitaram
;http://www.ccs.neu.edu/~dorai
;dorai AT ccs DOT neu DOT edu
;Oct 2, 1999

(define *pregexp-version* 20050502) ;last change

(define *pregexp-comment-char* #\#)

(define *pregexp-nul-char-int*
  ;can't assume #\nul maps to 0 because of Scsh
  (- (char->integer #\a) 97))

(define *pregexp-return-char*
  ;can't use #\return because it isn't R5RS
  (integer->char
    (+ 13 *pregexp-nul-char-int*)))

(define *pregexp-tab-char*
  ;can't use #\tab because it isn't R5RS
  (integer->char
    (+ 9 *pregexp-nul-char-int*)))

(define *pregexp-space-sensitive?* #t)

(define pregexp-reverse!
  ;the useful reverse! isn't R5RS
  (lambda (s)
    (let loop ((s s) (r '()))
      (if (null? s) r
          (let ((d (cdr s)))
            (set-cdr! s r)
            (loop d s))))))

;The comments ;( and ;) are there only to
;match escaped parens so my text editor will
;paren-balance correctly

(define pregexp-read-pattern
  (lambda (s i n)
    (if (>= i n)
        (list
          (list ':or (list ':seq)) i)
        (let loop ((branches '()) (i i))
          (if (or (>= i n)
                  (char=? (string-ref s i) #\)))
              (list (cons ':or (pregexp-reverse! branches)) i)
              (let ((vv (pregexp-read-branch
                          s
                          (if (char=? (string-ref s i) #\|) (+ i 1) i) n)))
                (loop (cons (car vv) branches) (cadr vv))))))))

(define pregexp-read-branch
  (lambda (s i n)
    (let loop ((pieces '()) (i i))
      (cond ((>= i n)
             (list (cons ':seq (pregexp-reverse! pieces)) i))
            ((let ((c (string-ref s i)))
               (or (char=? c #\|)
                   (char=? c #\))))
             (list (cons ':seq (pregexp-reverse! pieces)) i))
            (else (let ((vv (pregexp-read-piece s i n)))
                    (loop (cons (car vv) pieces) (cadr vv))))))))

(define pregexp-read-piece
  (lambda (s i n)
    (let ((c (string-ref s i)))
      (case c
        ((#\^) (list ':bos (+ i 1)))
        ((#\$) (list ':eos (+ i 1)))
        ((#\.) (pregexp-wrap-quantifier-if-any
                 (list ':any (+ i 1)) s n))
        ((#\[) (let ((i+1 (+ i 1)))
                 (pregexp-wrap-quantifier-if-any
                   (case (and (< i+1 n) (string-ref s i+1))
                     ((#\^)
                      (let ((vv (pregexp-read-char-list s (+ i 2) n)))
                        (list (list ':neg-char (car vv)) (cadr vv))))
                     (else (pregexp-read-char-list s i+1 n)))
                   s n)))
        ((#\()
         (pregexp-wrap-quantifier-if-any
           (pregexp-read-subpattern s (+ i 1) n) s n))
        ((#\\)
         (pregexp-wrap-quantifier-if-any
           (cond ((pregexp-read-escaped-number s i n) =>
                  (lambda (num-i)
                    (list (list ':backref (car num-i)) (cadr num-i))))
                 ((pregexp-read-escaped-char s i n) =>
                  (lambda (char-i)
                    (list (car char-i) (cadr char-i))))
                 (else (pregexp-error 'pregexp-read-piece 'backslash)))
           s n))
        (else
          (if (or *pregexp-space-sensitive?*
                  (and (not (char-whitespace? c))
                       (not (char=? c *pregexp-comment-char*))))
              (pregexp-wrap-quantifier-if-any
                (list c (+ i 1)) s n)
              (let loop ((i i) (in-comment? #f))
                (if (>= i n) (list ':empty i)
                    (let ((c (string-ref s i)))
                      (cond (in-comment?
                              (loop (+ i 1)
                                (not (char=? c #\newline))))
                            ((char-whitespace? c)
                             (loop (+ i 1) #f))
                            ((char=? c *pregexp-comment-char*)
                             (loop (+ i 1) #t))
                            (else (list ':empty i))))))))))))

(define pregexp-read-escaped-number
  (lambda (s i n)
    ; s[i] = \
    (and (< (+ i 1) n) ;must have at least something following \
         (let ((c (string-ref s (+ i 1))))
           (and (char-numeric? c)
                (let loop ((i (+ i 2)) (r (list c)))
                  (if (>= i n)
                      (list (string->number
                              (list->string (pregexp-reverse! r))) i)
                      (let ((c (string-ref s i)))
                        (if (char-numeric? c)
                            (loop (+ i 1) (cons c r))
                            (list (string->number
                                    (list->string (pregexp-reverse! r)))
                              i))))))))))

(define pregexp-read-escaped-char
  (lambda (s i n)
    ; s[i] = \
    (and (< (+ i 1) n)
         (let ((c (string-ref s (+ i 1))))
           (case c
             ((#\b) (list ':wbdry (+ i 2)))
             ((#\B) (list ':not-wbdry (+ i 2)))
             ((#\d) (list ':digit (+ i 2)))
             ((#\D) (list '(:neg-char :digit) (+ i 2)))
             ((#\n) (list #\newline (+ i 2)))
             ((#\r) (list *pregexp-return-char* (+ i 2)))
             ((#\s) (list ':space (+ i 2)))
             ((#\S) (list '(:neg-char :space) (+ i 2)))
             ((#\t) (list *pregexp-tab-char* (+ i 2)))
             ((#\w) (list ':word (+ i 2)))
             ((#\W) (list '(:neg-char :word) (+ i 2)))
             (else (list c (+ i 2))))))))

(define pregexp-read-posix-char-class
  (lambda (s i n)
    ; lbrack, colon already read
    (let ((neg? #f))
      (let loop ((i i)
		 (r '()))
	 ;; MS 22Feb06: care Bigloo2.8 requires to start with an 
	 ;; empty list since ':' has been stripped of from keyword names
        (if (>= i n)
            (pregexp-error 'pregexp-read-posix-char-class)
            (let ((c (string-ref s i)))
              (cond ((char=? c #\^)
                     (set! neg? #t)
                     (loop (+ i 1) r))
                    ((char-alphabetic? c)
                     (loop (+ i 1) (cons c r)))
                    ((char=? c #\:)
                     (if (or (>= (+ i 1) n)
                             (not (char=? (string-ref s (+ i 1)) #\])))
                         (pregexp-error 'pregexp-read-posix-char-class #\:)
                         (let ((posix-class
                                 (string->keyword
                                   (list->string (pregexp-reverse! r)))))
                           (list (if neg? (list ':neg-char posix-class)
                                     posix-class)
                             (+ i 2)))))
                    (else
                      (pregexp-error 'pregexp-read-posix-char-class)))))))))

(define pregexp-read-cluster-type
  (lambda (s i n)
    ; s[i-1] = left-paren
    (let ((c (string-ref s i)))
      (case c
        ((#\?)
         (let ((i (+ i 1)))
           (case (string-ref s i)
             ((#\:) (list '() (+ i 1)))
             ((#\=) (list '(:lookahead) (+ i 1)))
             ((#\!) (list '(:neg-lookahead) (+ i 1)))
             ((#\>) (list '(:no-backtrack) (+ i 1)))
             ((#\<)
              (list (case (string-ref s (+ i 1))
                      ((#\=) '(:lookbehind))
                      ((#\!) '(:neg-lookbehind))
                      (else (pregexp-error 'pregexp-read-cluster-type #\<)))
                (+ i 2)))
             (else (let loop ((i i) (r '()) (inv? #f))
                     (let ((c (string-ref s i)))
                       (case c
                         ((#\-) (loop (+ i 1) r #t))
                         ((#\i) (loop (+ i 1)
                                  (cons (if inv? ':case-sensitive
                                            ':case-insensitive) r) #f))
                         ((#\x)
                          (set! *pregexp-space-sensitive?* inv?)
                          (loop (+ i 1) r #f))
                         ((#\:) (list r (+ i 1)))
                         (else (pregexp-error 'pregexp-read-cluster-type c)))))))))
        (else (list '(:sub) i))))))
 
(define pregexp-read-subpattern
  (lambda (s i n)
    (let* ((remember-space-sensitive? *pregexp-space-sensitive?*)
           (ctyp-i (pregexp-read-cluster-type s i n))
           (ctyp (car ctyp-i))
           (i (cadr ctyp-i))
           (vv (pregexp-read-pattern s i n)))
      (set! *pregexp-space-sensitive?* remember-space-sensitive?)
      (let ((vv-re (car vv))
            (vv-i (cadr vv)))
        (if (and (< vv-i n)
                 (char=? (string-ref s vv-i) 
                   #\)))
            (list
              (let loop ((ctyp ctyp) (re vv-re))
                (if (null? ctyp) re
                    (loop (cdr ctyp)
                      (list (car ctyp) re))))
              (+ vv-i 1))
            (pregexp-error 'pregexp-read-subpattern))))))

(define pregexp-wrap-quantifier-if-any
  (lambda (vv s n)
    (let ((re (car vv)))
      (let loop ((i (cadr vv)))
        (if (>= i n) vv
            (let ((c (string-ref s i)))
              (if (and (char-whitespace? c) (not *pregexp-space-sensitive?*))
                  (loop (+ i 1))
                  (case c
                    ((#\* #\+ #\? #\{)
                     (let* ((new-re (list ':between 'minimal?
                                      'at-least 'at-most re))
                            (new-vv (list new-re 'next-i)))
                       (case c
                         ((#\*) (set-car! (cddr new-re) 0)
                          (set-car! (cdddr new-re) #f))
                         ((#\+) (set-car! (cddr new-re) 1)
                          (set-car! (cdddr new-re) #f))
                         ((#\?) (set-car! (cddr new-re) 0)
                          (set-car! (cdddr new-re) 1))
                         ((#\{) (let ((pq (pregexp-read-nums s (+ i 1) n)))
                                  (if (not pq)
                                      (pregexp-error 
                                        'pregexp-wrap-quantifier-if-any
                                        "left bracket must be followed by number"))
                                  (set-car! (cddr new-re) (car pq))
                                  (set-car! (cdddr new-re) (cadr pq))
                                  (set! i (caddr pq)))))
                       (let loop ((i (+ i 1)))
                         (if (>= i n)
                             (begin (set-car! (cdr new-re) #f)
                               (set-car! (cdr new-vv) i))
                             (let ((c (string-ref s i)))
                               (cond ((and (char-whitespace? c)
                                           (not *pregexp-space-sensitive?*))
                                      (loop (+ i 1)))
                                     ((char=? c #\?)
                                      (set-car! (cdr new-re) #t)
                                      (set-car! (cdr new-vv) (+ i 1)))
                                     (else (set-car! (cdr new-re) #f)
                                       (set-car! (cdr new-vv) i))))))
                       new-vv))
                    (else vv)))))))))

;

(define pregexp-read-nums
  (lambda (s i n)
    ; s[i-1] = {
    ; returns (p q k) where s[k] = }
    (let loop ((p '()) (q '()) (k i) (reading 1))
      (if (>= k n) (pregexp-error 'pregexp-read-nums))
      (let ((c (string-ref s k)))
        (cond ((char-numeric? c)
               (if (= reading 1)
                   (loop (cons c p) q (+ k 1) 1)
                   (loop p (cons c q) (+ k 1) 2)))
              ((and (char-whitespace? c) (not *pregexp-space-sensitive?*))
               (loop p q (+ k 1) reading))
              ((and (char=? c #\,) (= reading 1))
               (loop p q (+ k 1) 2))
              ((char=? c #\})
               (let ((p (string->number (list->string (pregexp-reverse! p))))
                     (q (string->number (list->string (pregexp-reverse! q)))))
                 (cond ((and (not p) (= reading 1)) (list 0 #f k))
                       ((= reading 1) (list p p k))
                       (else (list p q k)))))
              (else #f))))))

(define pregexp-invert-char-list
  (lambda (vv)
    (set-car! (car vv) ':none-of-chars)
    vv))

;

(define pregexp-read-char-list
  (lambda (s i n)
    (let loop ((r '()) (i i))
      (if (>= i n)
          (pregexp-error 'pregexp-read-char-list
			 "character class ended too soon")
          (let ((c (string-ref s i)))
            (case c
              ((#\]) (if (null? r)
                         (loop (cons c r) (+ i 1))
                         (list (cons ':one-of-chars (pregexp-reverse! r)) 
                               (+ i 1))))
              ((#\\)
               (let ((char-i (pregexp-read-escaped-char s i n)))
		  (if char-i (loop (cons (car char-i) r) (cadr char-i))
		      (pregexp-error 'pregexp-read-char-list #\\))))
              ((#\-) (if (or (null? r)
                             (let ((i+1 (+ i 1)))
                               (and (< i+1 n)
                                    (char=? (string-ref s i+1) #\]))))
                         (loop (cons c r) (+ i 1))
                         (let ((c-prev (car r)))
                           (if (char? c-prev)
                               (loop (cons (list ':char-range c-prev
                                                 (string-ref s (+ i 1))) (cdr r))
                                     (+ i 2))
                               (loop (cons c r) (+ i 1))))))
              ((#\[) (if (char=? (string-ref s (+ i 1)) #\:)
                         (let ((posix-char-class-i
                                 (pregexp-read-posix-char-class s (+ i 2) n)))
                           (loop (cons (car posix-char-class-i) r)
                                 (cadr posix-char-class-i)))
                         (loop (cons c r) (+ i 1))))
              (else (loop (cons c r) (+ i 1)))))))))

;

(define pregexp-string-match
  (lambda (s1 s i n sk fk)
    (let ((n1 (string-length s1)))
      (if (> n1 n) (fk)
          (let loop ((j 0) (k i))
            (cond ((>= j n1) (sk k))
                  ((>= k n) (fk))
                  ((char=? (string-ref s1 j) (string-ref s k))
                   (loop (+ j 1) (+ k 1)))
                  (else (fk))))))))

(define pregexp-char-word?
  (lambda (c)
    ;too restrictive for Scheme but this
    ;is what \w is in most regexp notations
    (or (char-alphabetic? c)
        (char-numeric? c)
        (char=? c #\_))))

(define pregexp-at-word-boundary?
  (lambda (s i n)
    (or (= i 0) (>= i n)
        (let ((c/i (string-ref s i))
              (c/i-1 (string-ref s (- i 1))))
          (let ((c/i/w? (pregexp-check-if-in-char-class?
                          c/i ':word))
                (c/i-1/w? (pregexp-check-if-in-char-class?
                            c/i-1 ':word)))
            (or (and c/i/w? (not c/i-1/w?))
                (and (not c/i/w?) c/i-1/w?)))))))

(define pregexp-check-if-in-char-class?
  (lambda (c char-class)
    (case char-class
      ((:any) (not (char=? c #\newline)))
      ;
      ((:alnum) (or (char-alphabetic? c) (char-numeric? c)))
      ((:alpha) (char-alphabetic? c))
      ((:ascii) (< (char->integer c) 128))
      ((:blank) (or (char=? c #\space) (char=? c *pregexp-tab-char*)))
      ((:cntrl) (< (char->integer c) 32))
      ((:digit) (char-numeric? c))
      ((:graph) (and (>= (char->integer c) 32)
                     (not (char-whitespace? c))))
      ((:lower) (char-lower-case? c))
      ((:print) (>= (char->integer c) 32))
      ((:punct) (and (>= (char->integer c) 32)
                     (not (char-whitespace? c))
                     (not (char-alphabetic? c))
                     (not (char-numeric? c))))
      ((:space) (char-whitespace? c))
      ((:upper) (char-upper-case? c))
      ((:word) (or (char-alphabetic? c)
                   (char-numeric? c)
                   (char=? c #\_)))
      ((:xdigit) (or (char-numeric? c)
                     (char-ci=? c #\a) (char-ci=? c #\b)
                     (char-ci=? c #\c) (char-ci=? c #\d)
                     (char-ci=? c #\e) (char-ci=? c #\f)))
      (else (pregexp-error 'pregexp-check-if-in-char-class? char-class)))))

(define pregexp-list-ref
  (lambda (s i)
    ;like list-ref but returns #f if index is
    ;out of bounds
    (let loop ((s s) (k 0))
      (cond ((null? s) #f)
            ((= k i) (car s))
            (else (loop (cdr s) (+ k 1)))))))

;re is a compiled regexp.  It's a list that can't be
;nil.  pregexp-match-positions-aux returns a 2-elt list whose
;car is the string-index following the matched
;portion and whose cadr contains the submatches.
;The proc returns false if there's no match.

;Am spelling loop- as loup- because these shouldn't
;be translated into CL loops by scm2cl (although
;they are tail-recursive in Scheme)

(define pregexp-make-backref-list
  (lambda (re)
    (let sub ((re re))
      (if (pair? re)
          (let ((car-re (car re))
                (sub-cdr-re (sub (cdr re))))
            (if (eqv? car-re ':sub) 
                (cons (cons re #f) sub-cdr-re)
                (append (sub car-re) sub-cdr-re)))
          '()))))

(define pregexp-match-positions-aux
  (lambda (re s sn start n i)
    (let ((identity (lambda (x) x))
          (backrefs (pregexp-make-backref-list re))
          (case-sensitive? #t))
      (let sub ((re re) (i i) (sk identity) (fk (lambda () #f)))
        ;(printf "sub ~s ~s\n" i re)
        (cond ((eqv? re ':bos)
               ;(if (= i 0) (sk i) (fk))
               (if (= i start) (sk i) (fk))
               )
              ((eqv? re ':eos)
               ;(if (>= i sn) (sk i) (fk))
               (if (>= i n) (sk i) (fk))
               )
              ((eqv? re ':empty)
               (sk i))
              ((eqv? re ':wbdry)
               (if (pregexp-at-word-boundary? s i n)
                   (sk i)
                   (fk)))
              ((eqv? re ':not-wbdry)
               (if (pregexp-at-word-boundary? s i n)
                   (fk)
                   (sk i)))
              ((and (char? re) (< i n))
               ;(printf "bingo\n")
               (if ((if case-sensitive? char=? char-ci=?)
                    (string-ref s i) re)
                   (sk (+ i 1)) (fk)))
              ((and (not (pair? re)) (< i n))
               (if (pregexp-check-if-in-char-class?
                     (string-ref s i) re)
                   (sk (+ i 1)) (fk)))
              ((and (pair? re) (eqv? (car re) ':char-range) (< i n))
               (let ((c (string-ref s i)))
                 (if (let ((c< (if case-sensitive? char<=? char-ci<=?)))
                       (and (c< (cadr re) c)
                            (c< c (caddr re))))
                     (sk (+ i 1)) (fk))))
              ((pair? re)
               (case (car re)
                 ((:char-range)
                  (if (>= i n) (fk) 
                      (pregexp-error 'pregexp-match-positions-aux)))
                 ((:one-of-chars)
                  (if (>= i n) (fk)
                      (let loup-one-of-chars ((chars (cdr re)))
                        (if (null? chars) (fk)
                            (sub (car chars) i sk
                                 (lambda ()
                                   (loup-one-of-chars (cdr chars))))))))
                 ((:neg-char)
                  (if (>= i n) (fk)
                      (sub (cadr re) i 
                           (lambda (i1) (fk))
                           (lambda () (sk (+ i 1))))))
                 ((:seq)
                  (let loup-seq ((res (cdr re)) (i i))
                    (if (null? res) (sk i )
                        (sub (car res) i 
                             (lambda (i1 )
                               (loup-seq (cdr res) i1 ))
                             fk))))
                 ((:or)
                  (let loup-or ((res (cdr re)))
                    (if (null? res) (fk)
                        (sub (car res) i 
                             (lambda (i1 )
                               (or (sk i1 )
                                   (loup-or (cdr res))))
                             (lambda () (loup-or (cdr res)))))))
                 ((:backref)
                  (let* ((c (pregexp-list-ref backrefs (cadr re)))
                         (backref
                           (cond (c => cdr)
                                 (else 
                                   (pregexp-error 'pregexp-match-positions-aux
                                                  'non-existent-backref re)
                                   #f))))
                    (if backref
                        (pregexp-string-match
                          (substring s (car backref) (cdr backref))
                          s i n (lambda (i) (sk i)) fk)
                        (sk i))))
                 ((:sub)
                  (sub (cadr re) i 
                       (lambda (i1)
                         (set-cdr! (assv re backrefs) (cons i i1))
                         (sk i1)) fk))
                 ((:lookahead)
                  (let ((found-it?
                          (sub (cadr re) i
                               identity (lambda () #f))))
                    (if found-it? (sk i) (fk))))
                 ((:neg-lookahead)
                  (let ((found-it?
                          (sub (cadr re) i
                               identity (lambda () #f))))
                    (if found-it? (fk) (sk i))))
                 ((:lookbehind)
                  (let ((n-actual n) (sn-actual sn)) 
                    (set! n i) (set! sn i)
                    (let ((found-it?
                            (sub (list ':seq '(:between #f 0 #f :any)
                                       (cadr re) ':eos) 0 
                                 identity (lambda () #f))))
                      (set! n n-actual) (set! sn sn-actual)
                      (if found-it? (sk i) (fk)))))
                 ((:neg-lookbehind)
                  (let ((n-actual n) (sn-actual sn)) 
                    (set! n i) (set! sn i)
                    (let ((found-it?
                            (sub (list ':seq '(:between #f 0 #f :any)
                                       (cadr re) ':eos) 0
                                 identity (lambda () #f))))
                      (set! n n-actual) (set! sn sn-actual)
                      (if found-it? (fk) (sk i)))))
                 ((:no-backtrack)
                  (let ((found-it? (sub (cadr re) i
                                        identity (lambda () #f))))
                    (if found-it?
                        (sk found-it?) 
                        (fk))))
                 ((:case-sensitive :case-insensitive)
                  (let ((old case-sensitive?))
                    (set! case-sensitive?
                      (eqv? (car re) ':case-sensitive))
                    (sub (cadr re) i 
                         (lambda (i1)
                           (set! case-sensitive? old)
                           (sk i1))
                         (lambda ()
                           (set! case-sensitive? old)
                           (fk)))))
                 ((:between)
                  (let* ((maximal? (not (cadr re)))
                         (p (caddr re)) 
                         (q (cadddr re))
                         (could-loop-infinitely? (and maximal? (not q)))
                         (re (car (cddddr re))))
                    (let loup-p ((k 0) (i i) )
                      (if (< k p)
                          (sub re i 
                               (lambda (i1 )
                                 (if (and could-loop-infinitely?
                                          (= i1 i))
                                     (pregexp-error 
                                       'pregexp-match-positions-aux
                                       'greedy-quantifier-operand-could-be-empty))
                                 (loup-p (+ k 1) i1 ))
                               fk)
                          (let ((q (and q (- q p))))
                            (let loup-q ((k 0) (i i))
                              (let ((fk (lambda ()
                                          (sk i ))))
                                (if (and q (>= k q)) (fk)
                                    (if maximal?
                                        (sub re i
                                             (lambda (i1)
                                               (if (and could-loop-infinitely?
                                                        (= i1 i))
                                                   (pregexp-error
                                                     'pregexp-match-positions-aux
                                                     'greedy-quantifier-operand-could-be-empty))
                                               (or (loup-q (+ k 1) i1)
                                                   (fk)))
                                             fk)
                                        (or (fk)
                                            (sub re i 
                                                 (lambda (i1)
                                                   (loup-q (+ k 1) i1))
                                                 fk)))))))))))
                 (else (pregexp-error 'pregexp-match-positions-aux))))
              ((>= i n) (fk))
              (else (pregexp-error 'pregexp-match-positions-aux))))
      ;(printf "done\n")
      (let ((backrefs (map cdr backrefs)))
        (and (car backrefs) backrefs)))))

(define pregexp-replace-aux
  (lambda (str ins n backrefs)
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
                (loop (+ i 1) (string-append r (string c)))))))))

(define pregexp
   (lambda (s . opt-args)
      (let ((re ($make-regexp s)))
	 ($regexp-preg-set!  re (tree-copy (%pregexp s)))
	 re)))

(define %pregexp
  (lambda (s)
    (when (string-prefix? "(*UTF8)" s) (set! s (substring s 7)))
    (set! *pregexp-space-sensitive?* #t) ;in case it got corrupted
    (list ':sub (car (pregexp-read-pattern s 0 (string-length s))))))

(define pregexp-match-positions
   ;; bigloo prototype change
  (lambda (pat str #!optional (beg 0) (end (string-length str)))
    (cond ((string? pat) (set! pat (%pregexp pat)))
	  ((regexp? pat) (set! pat ($regexp-preg pat)))
          ((pair? pat) #t)
          (else (pregexp-error 'pregexp-match-positions 
                               'pattern-must-be-compiled-or-string-regexp
                               pat)))
    (let* ((str-len (string-length str))
	   (start beg))
;*            (start (if (null? opt-args) 0                            */
;*                       (let ((start (car opt-args)))                 */
;*                         (set! opt-args (cdr opt-args))              */
;*                         start)))                                    */
;*            (end (if (null? opt-args) str-len                        */
;*                     (car opt-args)))                                */
      (let loop ((i start))
        (and (<= i end)
             (or (pregexp-match-positions-aux 
                   pat str str-len start end i)
                 (loop (+ i 1))))))))

(define pregexp-match-n-positions!
   ;; bigloo addition
   (lambda (pat str res beg end)
      (let ((pos (pregexp-match-positions pat str beg end))
	    (len (bit-and (vector-length res) (bit-not 1))))
	 (if (not pos)
	     -1
	     (let loop ((i 0)
			(pos pos))
		(cond
		   ((or (=fx i len) (null? pos))
		    i)
		   ((pair? (car pos))
		    (vector-set! res i (caar pos))
		    (vector-set! res (+fx i 1) (cdar pos))
		    (loop (+fx i 2) (cdr pos)))
		   (else
		    (vector-set! res i -1)
		    (vector-set! res (+fx i 1) -1)
		    (loop (+fx i 2) (cdr pos)))))))))
		
(define pregexp-match
  (lambda (pat str #!optional (beg 0) (end (string-length str)))
    (let ((ix-prs (pregexp-match-positions pat str beg end)))
      (and ix-prs
           (map
             (lambda (ix-pr)
               (and ix-pr
                    (substring str (car ix-pr) (cdr ix-pr))))
             ix-prs)))))

(define pregexp-split
  (lambda (pat str)
    ;split str into substrings, using pat as delimiter
    (let ((n (string-length str)))
      (let loop ((i 0) (r '()) (picked-up-one-undelimited-char? #f))
        (cond ((>= i n) (pregexp-reverse! r))
              ((pregexp-match-positions pat str i n)
               =>
               (lambda (y)
                 (let ((jk (car y)))
                   (let ((j (car jk)) (k (cdr jk)))
                     ;(printf "j = ~a; k = ~a; i = ~a~n" j k i)
                     (cond ((= j k)
                            ;(printf "producing ~s~n" (substring str i (+ j 1)))
                            (loop (+ k 1) 
                                  (cons (substring str i (+ j 1)) r) #t))
                           ((and (= j i) picked-up-one-undelimited-char?)
                            (loop k r #f))
                           (else
                             ;(printf "producing ~s~n" (substring str i j))
                             (loop k (cons (substring str i j) r) #f)))))))
              (else (loop n (cons (substring str i n) r) #f)))))))

(define pregexp-replace
  (lambda (pat str ins)
    (let* ((n (string-length str))
           (pp (pregexp-match-positions pat str 0 n)))
      (if (not pp) str
          (let ((ins-len (string-length ins))
                (m-i (caar pp))
                (m-n (cdar pp)))
            (string-append
              (substring str 0 m-i)
              (pregexp-replace-aux str ins ins-len pp)
              (substring str m-n n)))))))

(define pregexp-replace*
  (lambda (pat str ins)
    ;return str with every occurrence of pat 
    ;replaced by ins
    (let ((pat (if (string? pat) (%pregexp pat) pat))
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
                         (pregexp-replace-aux str ins ins-len pp))))))))))

(define pregexp-quote
  (lambda (s)
    (let loop ((i (- (string-length s) 1)) (r '()))
      (if (< i 0) (list->string r)
          (loop (- i 1)
                (let ((c (string-ref s i)))
                  (if (memv c '(#\\ #\. #\? #\* #\+ #\| #\^ #\$
                                    #\[ #\] #\{ #\} #\( #\)))
                      (cons #\\ (cons c r))
                      (cons c r))))))))
    
