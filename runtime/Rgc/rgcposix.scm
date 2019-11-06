;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgcposix.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Olin Shivers                                      */
;*    Creation    :  Fri Sep 18 14:40:45 1998                          */
;*    Last change :  Sun Aug 25 09:12:13 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Parse Spencer-style regexps into a Bigloo RGC expressions.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_posix
   
   (import  __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    __rgc
	    __bit
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_characters_6_6
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_strings_6_7
	    __r4_pairs_and_lists_6_3
	    __r4_input_6_10_2
	    __r4_control_features_6_9
	    __r5_control_features_6_4
	    __r4_ports_6_10_1
	    __r4_output_6_10_3
	    __r4_vectors_6_8)

   (export (posix->rgc ::bstring)))

;*---------------------------------------------------------------------*/
;*    posix->rgc ...                                                   */
;*---------------------------------------------------------------------*/
(define (posix->rgc s)
   (set! *err-string* s)
   (multiple-value-bind (rgc i)
      (parse-posix-exp s 0)
      (if (=fx i (string-length s))
	  rgc
	  (posix-error "RGC:Illegal Posix regexp -- terminated early"))))

;*---------------------------------------------------------------------*/
;*    *err-string* ...                                                 */
;*    -------------------------------------------------------------    */
;*    The string we are to convert. This variable is only used when    */
;*    printing errors.                                                 */
;*---------------------------------------------------------------------*/
(define *err-string* #unspecified)

;*---------------------------------------------------------------------*/
;*    posix-error ...                                                  */
;*---------------------------------------------------------------------*/
(define (posix-error msg)
   (error #f msg *err-string*))

;*---------------------------------------------------------------------*/
;*    make-rgc-repeat ...                                              */
;*---------------------------------------------------------------------*/
(define (make-rgc-repeat min max rgc)
   (cond
      ((and (eq? min 0) (not max))
       `(* ,rgc))
      ((and (eq? min 1) (not max))
       `(+ ,rgc))
      ((and (eq? min 0) (eq? max 1))
       `(? ,rgc))
      ((not max)
       `(>= ,min ,rgc))
      (else
       `(** ,min ,max ,rgc))))

;*---------------------------------------------------------------------*/
;*    make-rgc-or ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-rgc-or rgcs)
   (cond
      ((null? rgcs)
       (posix-error "RGC:Illegal posix string"))
      ((null? (cdr rgcs))
       (car rgcs))
      (else
       `(or ,@rgcs))))

;*---------------------------------------------------------------------*/
;*    make-rgc-sequence ...                                            */
;*---------------------------------------------------------------------*/
(define (make-rgc-sequence rgcs)
   (cond
      ((null? rgcs)
       (posix-error "RGC:Illegal posix string"))
      ((null? (cdr rgcs))
       (car rgcs))
      (else
       `(: ,@rgcs))))
   
;*---------------------------------------------------------------------*/
;*    parse-posix-exp ...                                              */
;*    -------------------------------------------------------------    */
;*    A complete expression is a sequence of |-separated branches.     */
;*---------------------------------------------------------------------*/
(define (parse-posix-exp s i)
   (let ((len (string-length s)))
      (if (<fx i len)
	  (let loop ((i        i)
		     (branches '()))
	     (multiple-value-bind (branch i)
		(parse-posix-branch s i)
		(let ((branches (cons branch branches)))
		   (if (and (<fx i len) (char=? #\| (string-ref s i)))
		       (loop (+fx i 1) branches)
		       (values (make-rgc-or (reverse branches)) i)))))
	  (values '() i))))

;*---------------------------------------------------------------------*/
;*    parse-posix-branch ...                                           */
;*    -------------------------------------------------------------    */
;*    A branch is a sequence of pieces --                              */
;*    stuff that goes in-between |'s.                                  */
;*---------------------------------------------------------------------*/
(define (parse-posix-branch s i)
   (let ((len (string-length s)))
      (let loop ((i      i)
		 (pieces '()))
	 (if (<fx i len)
	     (multiple-value-bind (piece i)
		(parse-posix-piece s i)
		(let ((pieces (cons piece pieces)))
		   (if (<fx i len)
		       (case (string-ref s i)
			  ((#\) #\|)
			   (values (make-rgc-sequence (reverse pieces)) i))
			  (else
			   (loop i pieces)))
		       (values (make-rgc-sequence (reverse pieces)) i))))
	     (values (make-rgc-sequence (reverse pieces)) i)))))

;*---------------------------------------------------------------------*/
;*    parse-posix-piece ...                                            */
;*    -------------------------------------------------------------    */
;*    A piece is an atom possibly followed by a * ? + or {...}         */
;*    multiplier. I.e. an element of a branch sequence.                */
;*---------------------------------------------------------------------*/
(define (parse-posix-piece s i)
   (let ((len (string-length s)))
      (multiple-value-bind (atom i)
	 (parse-posix-atom s i)
	 (if (<fx i len)
	     (case (string-ref s i)
		((#\* #\+ #\?)
		 (multiple-value-bind (from to)
		    (case (string-ref s i)
		       ((#\*) (values 0 #f))
		       ((#\+) (values 1 #f))
		       ((#\?) (values 0 1)))
		    (values (make-rgc-repeat from to atom) (+fx i 1))))
		((#\{)
		 (multiple-value-bind (from to i)
		    (parse-posix-braces s (+ i 1))
		    (values (make-rgc-repeat from to atom) i)))
		(else (values atom i)))
	     (values atom i)))))

;*---------------------------------------------------------------------*/
;*    parse-posix-atom ...                                             */
;*    -------------------------------------------------------------    */
;*    An atom is something that would bind to a following * operator   */
;*    -- a letter, [...] charset, ^, $, or (...).                      */
;*---------------------------------------------------------------------*/
(define (parse-posix-atom s i)
  (let ((len (string-length s)))
    (if (<fx i (string-length s))
	(let ((c (string-ref s i)))
	  (case c
	    ((#\^) (posix-error "RGC: `^' regexps not supported."))
	    ((#\$) (posix-error "RGC: `$' regexps not supported."))
	    ((#\.) (values 'all (+fx i 1)))
	    ((#\[) (parse-posix-bracket s (+fx i 1)))
	    ((#\() (multiple-value-bind (re i)
		      (parse-posix-exp s (+fx i 1))
		      (if (and (<fx i len) (char=? #\) (string-ref s i)))
			  (values `(submatch ,re) (+fx i 1))
			 (posix-error "RGC:subexpression has no terminating close parenthesis"))))
	    ((#\\) (let ((i (+fx i 1)))
		     (if (<fx i len)
			 (values (string (string-ref s i))
				 (+fx i 1))
			 (posix-error "RGC:expression may not terminate with a backslash"))))
	    ((#\) #\| #\* #\+ #\? #\{) (values '() i))
	    (else (values (string c) (+fx i 1)))))
	(values '() i))))

;*---------------------------------------------------------------------*/
;*    parse-posix-bracket ...                                          */
;*    -------------------------------------------------------------    */
;*    Parse a [...] or [^...] bracket expression into a regexp.        */
;*    I is the index of the char following the left bracket.           */
;*---------------------------------------------------------------------*/
(define (parse-posix-bracket s i)
   (let ((len (string-length s)))
      (if (<fx i len)
	  (multiple-value-bind (negate? i0)
	     (let ((c (string-ref s i)))
		(if (char=? c #\^)
		    (values #t (+fx i 1))
		    (values #f i)))
	     (let loop ((i    i0)
			(cset '()))
		(if (<fx i len)
		    (let ((c (string-ref s i)))
		       (case c
			  ((#\[)
			   ;; We don't handle [..] [==] [::] frobs.
			   (let ((i1 (+fx i 1)))
			      (cond
				 ((and (<fx i1 len)
				       (memq (string-ref s i1) '(#\. #\= #\:)))
				  (posix-error
				   "double-bracket regexps not supported."))
				 (else
				  (loop i1 (cons #\[ cset))))))
			  ((#\])
			   (cond
			      ((=fx i i0)
			       (loop (+fx i 1)
				     (cons #\] cset)))
			      (else
			       (if negate?
				   (values `(out ,@cset) (+fx i 1))
				   (values `(in ,@cset) (+fx i 1))))))
			  ((#\-)
			   (cond
			      ((or (=fx i i0)
				   (and (<fx (+fx i 1) len)
					(char=? #\] (string-ref s (+fx i 1)))))
			       (loop (+fx i 1)
				     (cons #\- cset)))
			      (else
			       (posix-error "Illegal - in [...] regexp"))))
			  (else
			   ;; Regular letter -- either alone,
			   ;; or startpoint of a range.
			   (let ((i (+fx i 1)))
			      (if (and (<fx (+fx i 1) len)
				       (char=? #\- (string-ref s i)))
				  ;; Range
				  (let* ((i  (+fx i 1))
					 (to (char->integer (string-ref s i))))
				     (let laap ((j    (char->integer c))
						(cset cset))
					(if (>fx j to)
					    (loop (+fx i 1)
						  cset)
					    (laap (+fx j 1)
						  (cons (integer->char j)
							cset)))))
				  ;; Just a letter
				  (loop i (cons c cset)))))))
		    (posix-error "Missing close right bracket in regexp"))))
	  (posix-error "Missing close right bracket in regexp"))))

;*---------------------------------------------------------------------*/
;*    parse-posix-braces ...                                           */
;*    -------------------------------------------------------------    */
;*    Parse out a [from,to] repetition pair from a {m,n} {m} or {m,}   */
;*    expression. I is the index of the char following the left brace. */
;*---------------------------------------------------------------------*/
(define (parse-posix-braces s i)
   (define (index string char offset)
      (let ((len (string-length string)))
	 (let loop ((offset offset))
	    (cond
	       ((>=fx offset len)
		#f)
	       ((char=? (string-ref string offset) char)
		offset)
	       (else
		(loop (+fx offset 1)))))))
   (let ((comma (index s #\, i))
	 (rb    (index s #\} i)))
      (if rb
	  (if (and comma (<fx comma rb))
	      (values (string->number (substring s i comma))
		      (and (not (=fx (+fx comma 1) rb))
			   (string->number (substring s (+fx comma 1) rb)))
		      (+fx rb 1))
	      (let ((m (string->number (substring s i rb))))
		 (values m m (+fx rb 1))))
	  (posix-error "Missing close brace in regexp"))))
	
