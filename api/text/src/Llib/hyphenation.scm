;*=====================================================================*/
;*    .../prgm/project/bigloo/api/text/src/Llib/hyphenation.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Phil Bewig and Laurent Bloch                      */
;*    Creation    :  Tue Aug 31 10:15:44 2010                          */
;*    Last change :  Thu Nov 18 10:57:48 2010 (serrano)                */
;*    Copyright   :  2010 Phil Bewig, Laurent Bloch, Manuel Serrano    */
;*    -------------------------------------------------------------    */
;*    This Bigloo module is intended to provide word hyphenation.      */
;*                                                                     */
;*    The program is an implementation of the algorithm created        */
;*    by Frank Liang for Donald Knuth's TeX typographic system and     */
;*    exposed in Liang's PhD dissertation available on the TeX Users   */
;*    Group site here: http://www.tug.org/docs/liang/                  */
;*    Most of this implementation is borrowed from Phil Bewig's work   */
;*    available here: http://sites.google.com/site/schemephil/, along  */
;*    with his paper describing the program.                           */
;*                                                                     */
;*    word hy-phen-a-tion by com-pu-ter                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __text_hyphenation
   (export (load-hyphens path-or-symbol)
	   (make-hyphens #!key language (exceptions '()) (patterns '()))
	   (hyphenate::pair-nil word::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    load-hyphens ...                                                 */
;*---------------------------------------------------------------------*/
(define (load-hyphens path-or-symbol)
   (cond
      ((string? path-or-symbol)
       (let ((p (open-input-file path-or-symbol)))
	  (if (input-port? p)
	      (unwind-protect
		 (apply make-hyphens (read p))
		 (close-input-port p))
	      (raise
	       (instantiate::&io-file-not-found-error
		  (proc "load-hyphens")
		  (msg "file does not exist")
		  (obj path-or-symbol))))))
      ((symbol? path-or-symbol)
       (load-hyphens (make-file-path (bigloo-config 'library-directory)
				     "text"
				     "data"
				     (format "~a-hyphens.sch" path-or-symbol))))
      (else
       (bigloo-type-error "load-hyphens" "string or symbol" path-or-symbol))))

;*---------------------------------------------------------------------*/
;*    make-hyphens ...                                                 */
;*---------------------------------------------------------------------*/
(define (make-hyphens #!key language (exceptions '()) (patterns '()))
   (if (symbol? language)
       (make-t-hyph patterns exceptions)
       (bigloo-type-error "make-hyphens" "symbol" language)))

;*---------------------------------------------------------------------*/
;*    make-t-hyph ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-t-hyph patterns exceptions)
   (let loop ((pats patterns)
	      (exns exceptions)
	      (t t-null))
      (cond
	 ((null? exns)
	  t)
	 ((null? pats)
	  (multiple-value-bind (cs ns)
	     (split-exn (car exns))
	     (loop pats (cdr exns) (t-bind cs ns t))))
	 (else
	  (multiple-value-bind (cs ns)
	     (split-pat (car pats))
	     (loop (cdr pats) exns (t-bind cs ns t)))))))

;*---------------------------------------------------------------------*/
;*    fold-left ...                                                    */
;*---------------------------------------------------------------------*/
(define (fold-left op base xs)
   (if (null? xs)
       base
       (fold-left op (op base (car xs)) (cdr xs))))

;*---------------------------------------------------------------------*/
;*    t-null ...                                                       */
;*---------------------------------------------------------------------*/
(define t-null (cons '() '()))

;*---------------------------------------------------------------------*/
;*    a-look ...                                                       */
;*---------------------------------------------------------------------*/
(define (a-look c a)
   (cond ((null? a) #f)
	 ((char-ci<? c (caar a)) #f)
	 ((char-ci<? (caar a) c) (a-look c (cdr a)))
	 (else (car a))))

;*---------------------------------------------------------------------*/
;*    a-bind ...                                                       */
;*---------------------------------------------------------------------*/
(define (a-bind c x a)
   (cond ((null? a) (list (cons c x)))
	 ((char-ci<? c (caar a)) (cons (cons c x) a))
	 ((char-ci<? (caar a) c) (cons (car a) (a-bind c x (cdr a))))
	 (else (cons (cons c x) (cdr a)))))

;*---------------------------------------------------------------------*/
;*    t-look ...                                                       */
;*---------------------------------------------------------------------*/
(define (t-look ks t)
   (if (null? ks)
       (if (pair? (car t)) (caar t) #f)
       (let ((x (a-look (car ks) (cdr t))))
	  (if x (t-look (cdr ks) (cdr x)) #f))))

;*---------------------------------------------------------------------*/
;*    t-bind ...                                                       */
;*---------------------------------------------------------------------*/
(define (t-bind ks x t)
   (if (null? ks) (cons (list x) (cdr t))
       (let* ((a1 (a-look (car ks) (cdr t)))
	      (t2 (t-bind (cdr ks) x (if (pair? a1) (cdr a1) t-null))))
	  (cons (car t) (a-bind (car ks) t2 (cdr t))))))

;*---------------------------------------------------------------------*/
;*    char-num ...                                                     */
;*---------------------------------------------------------------------*/
(define (char-num::int c::char)
   (-fx (char->integer c) (char->integer #\0)))
   
;*---------------------------------------------------------------------*/
;*    split-pat ...                                                    */
;*    -------------------------------------------------------------    */
;*    LB: patterns are strings                                         */
;*---------------------------------------------------------------------*/
(define (split-pat pat)
   (let loop ((ps (string->list pat))
	      (cs '())
	      (ns '()))
      (cond ((null? ps)
	     (if (=fx (length cs) (length ns))
		 (values (reverse cs) (reverse (cons 0 ns)))
		 (values (reverse cs) (reverse ns))))
	    ((and (null? (cdr ps)) (char-numeric? (car ps)))
	     (values (reverse cs)
		     (reverse (cons (char-num (car ps)) ns))))
	    ((not (char-numeric? (car ps)))
	     (loop (cdr ps) (cons (car ps) cs) (cons 0 ns)))
	    (else
	     (loop (cddr ps)
		   (cons (cadr ps) cs)
		   (cons (char-num (car ps)) ns))))))

;*---------------------------------------------------------------------*/
;*    split-exn ...                                                    */
;*    -------------------------------------------------------------    */
;*    LB: patterns are strings                                         */
;*---------------------------------------------------------------------*/
(define (split-exn exn)
   (let loop ((ps (string->list exn))
	      (cs '(#\.))
	      (ns '(6)))
      (cond ((null? ps)
	     (values (cons #\. (reverse cs)) (cons 6 (reverse (cons 6 ns)))))
	    ((char=? (car ps) #\-)
	     (loop (cddr ps) (cons (cadr ps) cs) (cons 7 ns)))
	    (else
	     (loop (cdr ps) (cons (car ps) cs) (cons 6 ns))))))

;*---------------------------------------------------------------------*/
;*    t-looks ...                                                      */
;*---------------------------------------------------------------------*/
(define (t-looks ks t)
   (if (null? ks)
       (if (pair? (car t)) (list (caar t)) '())
       (let ((x (a-look (car ks) (cdr t))))
	  (if x
	      (if (pair? (car t))
		  (cons (caar t) (t-looks (cdr ks) (cdr x)))
		  (t-looks (cdr ks) (cdr x)))
	      (if (pair? (car t)) (list (caar t)) '())))))

;*---------------------------------------------------------------------*/
;*    max-rule ...                                                     */
;*---------------------------------------------------------------------*/
(define (max-rule xs ys)
   (let loop ((xs xs) (ys ys) (zs '()))
      (if (or (null? xs) (null? ys))
	  (append (reverse zs) xs)
	  (loop (cdr xs) (cdr ys) (cons (max (car xs) (car ys)) zs)))))

;*---------------------------------------------------------------------*/
;*    fixup ...                                                        */
;*---------------------------------------------------------------------*/
(define (fixup xs)
   (cons 0 (cons 0 (cdddr (reverse (cons 0 (cons 0 (cddr xs))))))))

;*---------------------------------------------------------------------*/
;*    hyphenate ...                                                    */
;*---------------------------------------------------------------------*/
(define (hyphenate word t-hyph)
   (if (<fx (string-length word) 5)
       (list word)
       (let loop ((ws (append (list #\.) (string->list word) (list #\.)))
		  (front (make-list (+ (string-length word) 3) 0)) (back '()))
	  (if (null? ws)
	      (let loop ((cs (string->list word))
			 (hs (fixup back))
			 (p '())
			 (ps '()))
		 (cond ((null? (cdr hs))
			(reverse (cons (list->string (reverse p)) ps)))
		       ((odd? (car hs))
			(loop (cdr cs) (cdr hs) (list (car cs))
			      (cons (list->string (reverse p)) ps)))
		       (else
			(loop (cdr cs) (cdr hs) (cons (car cs) p) ps))))
	      (let ((new-front (fold-left max-rule front (t-looks ws t-hyph))))
		 (loop (cdr ws) (cdr new-front) (cons (car new-front) back)))))))
