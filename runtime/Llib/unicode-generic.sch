;*=====================================================================*/
;*    .../project/bigloo/wasm/runtime/Llib/unicode-generic.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Dec  7 08:19:02 2024                          */
;*    Last change :  Wed Feb  5 12:11:08 2025 (serrano)                */
;*    Copyright   :  2024-25 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generic portable unicode implementation.                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import __bit
	   __ucs2)
   (export (utf8_string_to_ucs2_string::ucs2string ::bstring)
	   (ucs2_string_to_utf8_string::bstring ::ucs2string)
	   (ucs2_strcmp::bool ::ucs2string ::ucs2string)
	   (ucs2_strcicmp::bool ::ucs2string ::ucs2string)
	   (ucs2_string_lt::bool ::ucs2string ::ucs2string)
	   (ucs2_string_le::bool ::ucs2string ::ucs2string)
	   (ucs2_string_gt::bool ::ucs2string ::ucs2string)
	   (ucs2_string_ge::bool ::ucs2string ::ucs2string)
	   (ucs2_string_cilt::bool ::ucs2string ::ucs2string)
	   (ucs2_string_cile::bool ::ucs2string ::ucs2string)
	   (ucs2_string_cigt::bool ::ucs2string ::ucs2string)
	   (ucs2_string_cige::bool ::ucs2string ::ucs2string)
	   (c_subucs2_string::ucs2string ::ucs2string ::long ::long)
	   (c_ucs2_string_copy::ucs2string ::ucs2string)
	   (ucs2_string_append::ucs2string ::ucs2string ::ucs2string))
   (extern (export utf8_string_to_ucs2_string "utf8_string_to_ucs2_string")
	   (export ucs2_string_to_utf8_string "ucs2_string_to_utf8_string")
	   (export ucs2_strcmp "ucs2_strcmp")
	   (export ucs2_strcicmp "ucs2_strcicmp")
	   (export ucs2_string_lt "ucs2_string_lt")
	   (export ucs2_string_le "ucs2_string_le")
	   (export ucs2_string_gt "ucs2_string_gt")
	   (export ucs2_string_ge "ucs2_string_ge")
	   (export ucs2_string_cilt "ucs2_string_cilt")
	   (export ucs2_string_cile "ucs2_string_cile")
	   (export ucs2_string_cigt "ucs2_string_cigt")
	   (export ucs2_string_cige "ucs2_string_cige")
	   (export c_subucs2_string "c_subucs2_string")
	   (export c_ucs2_string_copy "c_ucs2_string_copy")
	   (export ucs2_string_append "ucs2_string_append")))

;*---------------------------------------------------------------------*/
;*    utf8_string_to_ucs2_string ...                                   */
;*---------------------------------------------------------------------*/
(define (utf8_string_to_ucs2_string::ucs2string src::bstring)
   (let* ((len (string-length src))
	  (dst (make-ucs2-string (utf8-string-length src) #u0000)))
      (let loop ((r 0)
		 (w 0))
	 (if (=fx r len)
	     dst
	     (let ((s (utf8-char-size (string-ref-ur src r))))
		(multiple-value-bind (hi lo)
		   (utf8->utf16 src r)
		   (ucs2-string-set-ur! dst w (integer->ucs2-ur hi))
		   (if (>fx lo 0)
		       (begin
			  (ucs2-string-set-ur! dst (+fx w 1)
			     (integer->ucs2-ur lo))
			  (loop (+fx r s) (+fx w 2)))
		       (loop (+fx r s) (+fx w 1)))))))))

;*---------------------------------------------------------------------*/
;*    ucs2-utf8-size ...                                               */
;*---------------------------------------------------------------------*/
(define (ucs2-utf8-size::long n::long)
   (cond
      ((<=fx n #x7f) 1)
      ((<=fx n #x7ff) 2)
      ((<=fx n #xd7ff) 3)
      ((<=fx n #xdbfff) 4)
      ((<=fx n #xfffd) 3)
      ((<=fx n #xffff) 3)
      (else -1)))

;*---------------------------------------------------------------------*/
;*    uc2-string-utf8-len ...                                          */
;*---------------------------------------------------------------------*/
(define (ucs2-string-utf8-len::long src::ucs2string)
   (let ((len (ucs2-string-length src)))
      (let loop ((r 0)
		 (l 0))
	 (if (=fx r len)
	     l
	     (loop (+fx r 1)
		(+fx l (ucs2-utf8-size (ucs2->integer (ucs2-string-ref-ur src r)))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_to_utf8_string ...                                   */
;*---------------------------------------------------------------------*/
(define (ucs2_string_to_utf8_string::bstring src::ucs2string)
   
   (define (byte-set! str i n)
      (string-set! str i (integer->char n)))
   
   (define (ucs2-ref-ur src i)
      (ucs2->integer (ucs2-string-ref-ur src i)))
   
   (let* ((len (ucs2-string-length src))
	  (utf8-len (ucs2-string-utf8-len src))
	  (result (make-string utf8-len #a000)))
      (if (=fx utf8-len len)
	  (let loop ((i 0))
	     (if (=fx i len)
		 result
		 (begin
		    (string-set! result i (integer->char (ucs2-ref-ur src i)))
		    (loop (+fx i 1)))))
	  (let loop ((read 0)
		     (write 0))
	     (if (=fx read len)
		 (if (=fx write (string-length result))
		     result
		     (substring result 0 write))
		 (let* ((ucs2 (ucs2-ref-ur src read))
			(ulen (ucs2-utf8-size ucs2)))
		    (case ulen
		       ((1)
			(byte-set! result write ucs2)
			(loop (+fx read 1) (+fx write 1)))
		       ((4)
			(let ((nu (ucs2-ref-ur src (+fx read 1))))
			   (cond
			      ((and (<fx read (-fx len 1))
				    (>=fx nu #xdc00) (<=fx nu #xdfff))
			       (let* ((zzzzzz (bit-and nu #x3f))
				      (yyyy (bit-rsh (bit-and nu #x3ff) 6))
				      (xx (bit-and ucs2 #x3))
				      (wwww (bit-and (bit-rsh ucs2 2) #xf))
				      (vvvv (bit-and (bit-rsh ucs2 6) #xf))
				      (uuuuu (+fx vvvv 1)))
				  (byte-set! result (+fx write 3)
				     (+fx wwww #x80))
				  (byte-set! result (+fx write 2)
				     (+fx (bit-or (bit-lsh xx 4) yyyy)
					#x80))
				  (byte-set! result (+fx write 1)
				     (+fx (bit-or (bit-lsh (bit-or uuuuu #x3) 4)
					     wwww)
					#x80))
				  (byte-set! result write
				     (bit-or #xf0 (bit-lsh uuuuu 2)))
				  (set! utf8-len (-fx utf8-len 4))
				  (loop (+fx read 1) (+fx write ulen))))
			      ((and (>=fx ucs2 #xd800) (<=fx ucs2 #xdbff))
			       (let* ((xx (bit-and ucs2 #x3))
				      (wwww (bit-and (bit-rsh ucs2 2) #xf))
				      (vvvv (bit-and (bit-rsh ucs2 2) #xf))
				      (uuuuu (+fx vvvv 1)))
				  (byte-set! result (+fx write 3)
				     (bit-or #x80 (bit-rsh uuuuu 2)))
				  (byte-set! result (+fx write 2)
				     (+fx (bit-lsh xx 4) #x80))
				  (byte-set! result (+fx write 1)
				     (+fx (bit-or (bit-lsh (bit-and uuuuu #x3) 4)
					     wwww)
					#x80))
				  (byte-set! result write #xf8)
				  (loop (+fx read 1) (+fx write ulen))))
			      (else
			       (let* ((nu ucs2)
				      (zzzzzz (bit-and nu #x3f))
				      (yyyy (bit-rsh (bit-and nu #x3ff) 6)))
				  (byte-set! result (+fx write 3)
				     (+fx zzzzzz #x80))
				  (byte-set! result (+fx write 2)
				     (+fx yyyy #x80))
				  (byte-set! result (+fx write 1) #x80)
				  (byte-set! result write #xfc)
				  (loop (+fx read 1) (+fx write ulen)))))))
		       (else
			(when (=fx ulen 3)
			   (byte-set! result (+fx write 2)
			      (+fx #x80 (bit-and ucs2 #x3f)))
			   (set! ucs2 (bit-rsh ucs2 6)))
			(byte-set! result (+fx write 1)
			   (+fx #x80 (bit-and ucs2 #x3f)))
			(byte-set! result write
			   (+fx (-fx #xff (bit-rsh #xff ulen))
			      (bit-rsh ucs2 6)))
			(loop (+fx read 1) (+fx write ulen))))))))))
      
;*---------------------------------------------------------------------*/
;*    ucs2_strcmp ...                                                  */
;*---------------------------------------------------------------------*/
(define (ucs2_strcmp x y)
   (let ((lx (ucs2-string-length x)))
      (when (=fx lx (ucs2-string-length x))
	 (let loop ((i 0))
	    (cond
	       ((=fx i lx)
		#t)
	       ((ucs2=? (ucs2-string-ref-ur x i) (ucs2-string-ref-ur y i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    ucs2_strcicmp ...                                                */
;*---------------------------------------------------------------------*/
(define (ucs2_strcicmp x y)
   (let ((lx (ucs2-string-length x)))
      (when (=fx lx (ucs2-string-length x))
	 (let loop ((i 0))
	    (cond
	       ((=fx i lx)
		#t)
	       ((ucs2-ci=? (ucs2-string-ref-ur x i) (ucs2-string-ref-ur y i))
		(loop (+fx i 1)))
	       (else
		#f))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_lt ...                                               */
;*---------------------------------------------------------------------*/
(define (ucs2_string_lt x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     (<fx i ly))
	    ((=fx i ly)
	     #f)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2<? cx cy) #t)
		   ((ucs2=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_le ...                                               */
;*---------------------------------------------------------------------*/
(define (ucs2_string_le x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     (<=fx i ly))
	    ((=fx i ly)
	     #f)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2<? cx cy) #t)
		   ((ucs2=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_gt ...                                               */
;*---------------------------------------------------------------------*/
(define (ucs2_string_gt x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     #f)
	    ((=fx i ly)
	     #t)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2<? cx cy) #t)
		   ((ucs2=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_ge ...                                               */
;*---------------------------------------------------------------------*/
(define (ucs2_string_ge x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     (=fx i ly))
	    ((=fx i ly)
	     #t)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2>? cx cy) #t)
		   ((ucs2=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_cilt ...                                             */
;*---------------------------------------------------------------------*/
(define (ucs2_string_cilt x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     (<fx i ly))
	    ((=fx i ly)
	     #f)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2-ci<? cx cy) #t)
		   ((ucs2-ci=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_cile ...                                             */
;*---------------------------------------------------------------------*/
(define (ucs2_string_cile x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     (<=fx i ly))
	    ((=fx i ly)
	     #f)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2-ci<? cx cy) #t)
		   ((ucs2-ci=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_cigt ...                                             */
;*---------------------------------------------------------------------*/
(define (ucs2_string_cigt x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     #f)
	    ((=fx i ly)
	     #t)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2-ci<? cx cy) #t)
		   ((ucs2-ci=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    ucs2_string_cige ...                                             */
;*---------------------------------------------------------------------*/
(define (ucs2_string_cige x y)
   (let ((lx (ucs2-string-length x))
	 (ly (ucs2-string-length x)))
      (let loop ((i 0))
	 (cond
	    ((=fx i lx)
	     (=fx i ly))
	    ((=fx i ly)
	     #t)
	    (else
	     (let ((cx (ucs2-string-ref-ur x i))
		   (cy (ucs2-string-ref-ur y i)))
		(cond
		   ((ucs2-ci>? cx cy) #t)
		   ((ucs2-ci=? cx cy) (loop (+fx i 1)))
		   (else #f))))))))

;*---------------------------------------------------------------------*/
;*    c_subucs2_string ...                                             */
;*---------------------------------------------------------------------*/
(define (c_subucs2_string str start end)
   (let* ((len (-fx end start))
	  (res (make-ucs2-string len)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (ucs2-string-set-ur! res i (ucs2-string-ref-ur str (+fx start i)))
	    (loop (+fx i 1))))
      res))

;*---------------------------------------------------------------------*/
;*    c_ucs2_string_copy ...                                           */
;*---------------------------------------------------------------------*/
(define (c_ucs2_string_copy str)
   (c_subucs2_string str 0 (ucs2-string-length str)))

;*---------------------------------------------------------------------*/
;*    ucs2_string_append ...                                           */
;*---------------------------------------------------------------------*/
(define (ucs2_string_append x y)
   
   (define (blit! t s w)
      (let ((len (ucs2-string-length s)))
	 (let loop ((i 0))
	    (when (<fx i len)
	       (ucs2-string-set-ur! t (+fx w i) (ucs2-string-ref-ur s i))
	       (loop (+fx i 1))))))
   
   (let* ((lx (ucs2-string-length x))
	  (res (make-ucs2-string (+fx lx (ucs2-string-length y)))))
      (blit! res x 0)
      (blit! res y lx)
      res))
