;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/string-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:49:25 2024                          */
;*    Last change :  Tue Dec 24 09:01:41 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generic portable string implementation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import __bit
	   __unicode
	   __ucs2)
   (export (bigloo_strcmp::bool ::bstring ::bstring)
	   (bigloo_strncmp::bool ::bstring ::bstring ::long)
	   (bigloo_strcmp_at::bool ::bstring ::bstring ::long)
	   (bigloo_strncmp_at::bool ::bstring ::bstring ::long ::long)
	   (bigloo_strncmp_ci::bool ::bstring ::bstring ::long)
	   (bigloo_strcmp_ci_at::bool ::bstring ::bstring ::long)
	   (bigloo_strncmp_ci_at::bool ::bstring ::bstring ::long ::long)
	   (bigloo_strcicmp::bool ::bstring ::bstring)
	   (bigloo_string_lt::bool ::bstring ::bstring)
	   (bigloo_string_le::bool ::bstring ::bstring)
	   (bigloo_string_gt::bool ::bstring ::bstring)
	   (bigloo_string_ge::bool ::bstring ::bstring)
	   (bigloo_string_cilt::bool ::bstring ::bstring)
	   (bigloo_string_cile::bool ::bstring ::bstring)
	   (bigloo_string_cigt::bool ::bstring ::bstring)
	   (bigloo_string_cige::bool ::bstring ::bstring)
	   (bgl_escape_C_string::bstring ::bstring ::long ::long)
	   (bgl_escape_scheme_string::bstring ::bstring ::long ::long)
	   (c_constant_string_to_string::bstring ::string)
	   (BOUND_CHECK::bool ::long ::long)
	   (string_for_read::bstring ::bstring)
	   (symbol_for_read::bstring ::bstring)
	   (blit_string::obj ::bstring ::long ::bstring ::long ::long))
   (extern (export bigloo_strcmp "bigloo_strcmp")
	   (export bigloo_strncmp "bigloo_strncmp")
	   (export bigloo_strcmp_at "bigloo_strcmp_at")
	   (export bigloo_strncmp_at "bigloo_strncmp_at")
	   (export bigloo_strncmp_ci "bigloo_strncmp_ci")
	   (export bigloo_strncmp_ci_at "bigloo_strncmp_ci_at")
	   (export bigloo_strcicmp "bigloo_strcicmp")
	   (export bigloo_string_lt "bigloo_string_lt")
	   (export bigloo_string_le "bigloo_string_le")
	   (export bigloo_string_gt "bigloo_string_gt")
	   (export bigloo_string_ge "bigloo_string_ge")
	   (export bigloo_string_cilt "bigloo_string_cilt")
	   (export bigloo_string_cile "bigloo_string_cile")
	   (export bigloo_string_cigt "bigloo_string_cigt")
	   (export bigloo_string_cige "bigloo_string_cige")
	   (export bgl_escape_C_string "bgl_escape_C_string")
	   (export bgl_escape_scheme_string "bgl_escape_scheme_string")
	   (export c_constant_string_to_string "c_constant_string_to_string")
	   (export BOUND_CHECK "BOUND_CHECK")
	   (export string_for_read "string_for_read")
	   (export symbol_for_read "symbol_for_read")
	   (export blit_string "blit_string")))

;*---------------------------------------------------------------------*/
;*    bigloo_strcmp ...                                                */
;*---------------------------------------------------------------------*/
(define (bigloo_strcmp string1 string2)
   (let ((l1 (string-length string1))
	 (l2 (string-length string2)))
      (and (=fx l1 l2) (bigloo_strncmp string1 string2 l1))))

;*---------------------------------------------------------------------*/
;*    bigloo_strncmp ...                                               */
;*---------------------------------------------------------------------*/
(define (bigloo_strncmp string1 string2 len)
   (when (and (>=fx (string-length string1) len)
	      (>=fx (string-length string2) len))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     #t)
	    ((char=? (string-ref string1 i) (string-ref string2 i))
	     (loop (+fx i 1)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    bigloo_strcmp_at ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo_strcmp_at o1 o2 d)
   (let ((l1 (string-length o1))
	 (l2 (string-length o2)))
      (when (and (>=fx d 0) (<=fx (+fx l2 d) l1))
	 (let loop ((i 0))
	    (cond
	       ((=fx i l2)
		#t)
	       ((char=? (string-ref-ur o1 (+fx i d)) (string-ref-ur o2 i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    bigloo_strncmp_at ...                                            */
;*---------------------------------------------------------------------*/
(define (bigloo_strncmp_at o1 o2 d l3)
   (let* ((l1 (string-length o1))
	  (l2 (string-length o2))
	  (l (if (<fx l2 l3) l2 l3)))
      (when (and (>=fx d 0) (>=fx l3 0) (<=fx (+fx l d) l1))
	 (let loop ((i 0))
	    (cond
	       ((=fx i l)
		#t)
	       ((char=? (string-ref-ur o1 (+fx i d)) (string-ref-ur o2 i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    bigloo_strcmp_ci_at ...                                          */
;*---------------------------------------------------------------------*/
(define (bigloo_strcmp_ci_at o1 o2 d)
   (let ((l1 (string-length o1))
	 (l2 (string-length o2)))
      (when (and (>=fx d 0) (<=fx (+fx l2 d) l1))
	 (let loop ((i 0))
	    (cond
	       ((=fx i l2)
		#t)
	       ((char-ci=? (string-ref-ur o1 (+fx i d)) (string-ref-ur o2 i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    bigloo_strncmp_ci_at ...                                         */
;*---------------------------------------------------------------------*/
(define (bigloo_strncmp_ci_at o1 o2 d l3)
   (let* ((l1 (string-length o1))
	  (l2 (string-length o2))
	  (l (if (<fx l2 l3) l2 l3)))
      (when (and (>=fx d 0) (>=fx l3 0) (<=fx (+fx l d) l1))
	 (let loop ((i 0))
	    (cond
	       ((=fx i l)
		#t)
	       ((char-ci=? (string-ref-ur o1 (+fx i d)) (string-ref-ur o2 i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    bigloo_strncmp_ci ...                                            */
;*---------------------------------------------------------------------*/
(define (bigloo_strncmp_ci o1 o2 l3)
   (bigloo_strncmp_ci_at o1 o2 0 l3))

;*---------------------------------------------------------------------*/
;*    bigloo_strcicmp ...                                              */
;*---------------------------------------------------------------------*/
(define (bigloo_strcicmp bst1 bst2)
   (let ((l1 (string-length bst1)))
      (when (=fx l1 (string-length bst2))
	 (let loop ((i 0))
	    (cond
	       ((=fx i l1)
		#t)
	       ((char-ci=? (string-ref bst1 i) (string-ref bst2 i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_lt ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo_string_lt bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char<? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (<fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_le ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo_string_le bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char<=? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (<=fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_gt ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo_string_gt bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char>? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (>fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_ge ...                                             */
;*---------------------------------------------------------------------*/
(define (bigloo_string_ge bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char>=? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (>=fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_cilt ...                                           */
;*---------------------------------------------------------------------*/
(define (bigloo_string_cilt bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char-ci<? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (<fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_cile ...                                           */
;*---------------------------------------------------------------------*/
(define (bigloo_string_cile bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char-ci<=? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (<=fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_cigt ...                                           */
;*---------------------------------------------------------------------*/
(define (bigloo_string_cigt bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char-ci>? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (>fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bigloo_string_cige ...                                           */
;*---------------------------------------------------------------------*/
(define (bigloo_string_cige bst1 bst2)
   (let* ((l1 (string-length bst1))
	  (l2 (string-length bst2))
	  (min (minfx l1 l2)))
      (let for ((i 0))
	 (cond
	    ((and (char=? (string-ref-ur bst1 i) (string-ref-ur bst2 i))
		  (<fx i min))
	     (for (+fx i 1)))
	    ((<fx i min)
	     (char-ci>=? (string-ref-ur bst1 i) (string-ref-ur bst2 i)))
	    (else
	     (>=fx l1 l2))))))

;*---------------------------------------------------------------------*/
;*    bgl_escape_C_string ...                                          */
;*---------------------------------------------------------------------*/
(define (bgl_escape_C_string str start end)
   
   (define (isxdigit? c)
      (or (char-numeric? c)
	  (and (char>=? c #\a) (char<=? c #\f))
	  (and (char>=? c #\A) (char<=? c #\F))))
   
   (define (XDIGIT_TO_BYTE c)
      (cond
	 ((char-numeric? c)
	  (-fx (char->integer c) (char->integer #\0)))
	 ((and (char>=? c #\a) (char<=? c #\f))
	  (-fx (char->integer c) (char->integer #\a)))
	 ((and (char>=? c #\A) (char<=? c #\F))
	  (-fx (char->integer c) (char->integer #\A)))
	 (else
	  -1)))
   
   (define (DIGIT_TO_BYTE c)
      (cond
	 ((char-numeric? c)
	  (-fx (char->integer c) (char->integer #\0)))
	 (else
	  -1)))
   
   (let* ((len (-fx end start))
	  (buf (make-string len)))
      (let loop ((i 0)
		 (j 0))
	 (if (<fx i len)
	     (let ((c (string-ref-ur str (+fx start i))))
		(cond
		   ((not (char=? c #\\))
		    (string-set-ur! buf j c)
		    (loop (+fx i 1) (+fx j 1)))
		   ((<fx i (-fx len 1))
		    (let ((nc (string-ref-ur str (+fx 1 (+fx start i)))))
		       (case nc
			  ((#\000)
			   (string-set-ur! buf j #\\)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\n)
			   (string-set-ur! buf j #\Newline)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\t)
			   (string-set-ur! buf j #\Tab)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\b)
			   (string-set-ur! buf j #a008)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\r)
			   (string-set-ur! buf j #\Return)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\f)
			   (string-set-ur! buf j #a012)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\v)
			   (string-set-ur! buf j #a011)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\\)
			   (string-set-ur! buf j #\\)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\')
			   (string-set-ur! buf j #\')
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\")
			   (string-set-ur! buf j #\")
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\a)
			   (string-set-ur! buf j #a007)
			   (loop (+fx i 2) (+fx j 1)))
			  ((#\x #\X)
			   (if (<fx i (-fx len 3))
			       (let* ((s1 (string-ref-ur str (+fx 2 (+fx start i))))
				      (s2 (string-ref-ur str (+fx 3 (+fx start i))))
				      (n1 (XDIGIT_TO_BYTE s1))
				      (n2 (XDIGIT_TO_BYTE s2)))
				  (if (or (<fx n1 0) (<fx n2 0))
				      (begin
					 (string-set! buf j nc)
					 (loop (+fx i 2) (+fx j 1)))
				      (begin
					 (string-set! buf j
					    (integer->char
					       (+fx (*fx n1 16) n2)))
					 (loop (+fx i 4) (+fx j 1)))))))
			  ((#\u #\U)
			   (cond
			      ((and (<fx i (-fx len 4))
				    (char=? (string-ref-ur str (+fx 2 (+fx start i)))
				       #\{))
			       (let liip ((ii (+fx 3 (+fx start i)))
					  (n 0))
				  (if (=fx ii len)
				      (begin
					 (string-set! buf j c)
					 (loop (+fx i 1) (+fx j 1)))
				      (let ((c (string-ref-ur str ii)))
					 (cond
					    ((char=? c #\})
					     (let* ((u (integer->ucs2 n))
						    (s (make-ucs2-string 1 u))
						    (t (ucs2-string->utf8-string s))
						    (l (string-length t))
						    (d (-fx ii (+fx 3 (+fx start i)))))
						(blit-string! t 0 buf j l)
						(set! len (-fx len d))
						(loop (+fx i (+fx d 1)) (+fx j l))))
					    ((isxdigit? c)
					     (liip (+fx i 1)
						(+fx (*fx n 16) (XDIGIT_TO_BYTE c))))
					    (else
					     (string-set! buf j c)
					     (loop (+fx i 1) (+fx j 1))))))))
			      ((<fx i (-fx len 5))
			       (let* ((s1 (string-ref-ur str (+fx 2 (+fx start i))))
				      (s2 (string-ref-ur str (+fx 3 (+fx start i))))
				      (s3 (string-ref-ur str (+fx 4 (+fx start i))))
				      (s4 (string-ref-ur str (+fx 5 (+fx start i)))))
				  (if (and (isxdigit? s1) (isxdigit? s2)
					   (isxdigit? s3) (isxdigit? s4))
				      (let* ((n1 (XDIGIT_TO_BYTE s1))
					     (n2 (XDIGIT_TO_BYTE s2))
					     (n3 (XDIGIT_TO_BYTE s3))
					     (n4 (XDIGIT_TO_BYTE s4))
					     (n (+fx (bit-lsh n1 12)
						   (+fx (bit-lsh n2 8)
						      (+fx (bit-lsh n3 4)
							 n4))))
					     (u (integer->ucs2 n))
					     (s (make-ucs2-string 1 u))
					     (t (ucs2-string->utf8-string s))
					     (l (string-length t)))
					 (blit-string! t 0 buf j l)
					 (set! len (-fx len (-fx 5 l)))
					 (loop (+fx i 5) (+fx j l)))
				      (begin
					 (string-set-ur! buf j nc)
					 (loop (+fx i 2) (+fx j 1))))))
			      (else
			       (string-set-ur! buf j nc)
			       (loop (+fx i 2) (+fx j 1)))))
			  (else
			   (if (and (char-numeric? nc) (<fx i (-fx len 3)))
			       (let* ((s2 (string-ref-ur str (+fx 2 (+fx start i))))
				      (s3 (string-ref-ur str (+fx 3 (+fx start i))))
				      (n1 (DIGIT_TO_BYTE nc))
				      (n2 (DIGIT_TO_BYTE s2))
				      (n3 (DIGIT_TO_BYTE s3)))
				  (if (or (<fx n1 0) (<fx n2 0) (<fx n3 0))
				      (begin
					 (string-set! buf j nc)
					 (loop (+fx i 2) (+fx j 1)))
				      (begin
					 (string-set-ur! buf j
					    (integer->char
					       (+fx (*fx n1 64)
						  (+fx (*fx n2 8) n3))))
					 (loop (+fx i 4) (+fx j 1)))))
			       (begin
				  (string-set-ur! buf j nc)
				  (loop (+fx i 2) (+fx j 1))))))))
		   (else
		    (string-set-ur! buf j c)
		    (loop (+fx i 1) (+fx j 1)))))
	     (string-shrink! buf j)))))

;*---------------------------------------------------------------------*/
;*    bgl_escape_scheme_string ...                                     */
;*---------------------------------------------------------------------*/
(define (bgl_escape_scheme_string str start end)
   (let* ((len (-fx end start))
	  (buf (make-string len)))
      (let loop ((i 0)
		 (j 0))
	 (if (<fx i len)
	     (let ((c (string-ref-ur str (+fx start i))))
		(cond
		   ((not (char=? c #\\))
		    (string-set-ur! buf j c)
		    (loop (+fx i 1) (+fx j 1)))
		   ((<fx i (-fx len 1))
		    (let ((nc (string-ref-ur str (+fx i (+fx start 1)))))
		       (if (char=? nc #\n)
			   (string-set-ur! buf i #\Newline)
			   (string-set-ur! buf i nc)))
		    (loop (+fx i 2) (+fx j 1)))))
	     (string-shrink! buf j)))
      buf))

;*---------------------------------------------------------------------*/
;*    c_constant_string_to_string ...                                  */
;*---------------------------------------------------------------------*/
(define (c_constant_string_to_string c_string)
   c_string)

;*---------------------------------------------------------------------*/
;*    BOUND_CHECK ...                                                  */
;*---------------------------------------------------------------------*/
(define (BOUND_CHECK o v)
   (and (>=fx o 0) (<fx o v)))

;*---------------------------------------------------------------------*/
;*    create_string_for_read ...                                       */
;*---------------------------------------------------------------------*/
(define (create_string_for_read src symbolp)
   
   (define-macro (++ v)
      `(let ((_ ,v))
	  (set! ,v (+fx ,v 1))
	  _))
   
   (define (hexa n)
      (if (<fx n 10)
	  (integer->char (+fx n (char->integer #\0)))
	  (integer->char (+fx (-fx n 10) (char->integer #\a)))))
   
   (let* ((len (string-length src))
	  (dst (make-string (+fx (*fx len 4) 1)))
	  (esc #f)
	  (r 0)
	  (w 0))
      (let loop ()
	 (if (<fx r len)
	     (let ((c (string-ref-ur src (++ r))))
		(case c
		   ((#\Newline)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\n)
		    (set! esc #t))
		   ((#\tab)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\t)
		    (set! esc #t))
		   ((#a011)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\b)
		    (set! esc #t))
		   ((#\Return)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\r)
		    (set! esc #t))
		   ((#a012)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\f)
		    (set! esc #t))
		   ((#a011)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\v)
		    (set! esc #t))
		   ((#\\)
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\\)
		    (set! esc #t))
		   ((#\")
		    (string-set-ur! dst (++ w) #\\)
		    (string-set-ur! dst (++ w) #\")
		    (set! esc #t))
		   ((#\|)
		    (if symbolp
			(begin
			   (string-set-ur! dst (++ w) #\\)
			   (string-set-ur! dst (++ w) #\|)
			   (set! esc #t))
			(string-set-ur! dst (++ w) #\|)))
		   (else
		    (if (char>=? c #\space)
			(string-set-ur! dst (++ w) c)
			(let ((n (char->integer c)))
			   (string-set-ur! dst (++ w) #\\)
			   (string-set-ur! dst (++ w) #\x)
			   (string-set-ur! dst (++ w) (hexa (bit-lsh n 4)))
			   (string-set-ur! dst (++ w) (hexa (bit-and n 7)))))))
		(loop))
	     (string-shrink! dst w)))))

;*---------------------------------------------------------------------*/
;*    string_for_read ...                                              */
;*---------------------------------------------------------------------*/
(define (string_for_read bstring)
   (create_string_for_read bstring #f))

;*---------------------------------------------------------------------*/
;*    symbol_for_read ...                                              */
;*---------------------------------------------------------------------*/
(define (symbol_for_read bstring)
   (create_string_for_read bstring #t))

;*---------------------------------------------------------------------*/
;*    blit_string ...                                                  */
;*---------------------------------------------------------------------*/
(define (blit_string s1 offset1 s2 offset2 len)
   (let loop ((i 0))
      (when (<fx i len)
	 (string-set-ur! s2 (+fx i offset2)
	    (string-ref-ur s1 (+fx i offset1)))
	 (loop (+fx i 1))))
   #unspecified)

