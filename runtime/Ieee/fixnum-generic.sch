;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/fixnum-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 22 12:33:04 2024                          */
;*    Last change :  Wed Dec 25 21:06:08 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Portable fixnum implementation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (integer_to_string::bstring ::long ::long)
	   (unsigned_to_string::bstring ::long ::long)
	   (ullong_to_string::bstring ::long ::long)
	   ($integer->string/padding::bstring ::long ::long ::long)
	   (llong_to_string::bstring ::llong ::long)
	   ($$strtol::long string::bstring start::long radix::long)
	   ($$strtoel::elong string::bstring start::long radix::long)
	   ($$strtoll::llong string::bstring start::long radix::long))
   (extern (export integer_to_string "integer_to_string")
	   (export unsigned_to_string "unsigned_to_string")
	   (export ullong_to_string "ullong_to_string")
	   (export $integer->string/padding "integer->string/padding")
	   (export llong_to_string "llong_to_string")))

;*---------------------------------------------------------------------*/
;*    integer_to_string ...                                            */
;*---------------------------------------------------------------------*/
(define (integer_to_string x radix)
   (if (=fx x 0)
       (make-string 1 #\0)
       (let* ((bits (integer-bits-count x radix))
	      (aux (make-string bits)))
	  (string-set! aux (-fx bits 1) #a000)
	  (let loop ((bits bits)
		     (ax (absfx x)))
	     (unless (=fx ax 0)
		(string-set! aux (-fx bits 1)
		   (num->char (remainderfx ax radix)))
		(loop (-fx bits 1) (/fx ax radix))))
	  (when (<fx x 0)
	     (string-set! aux 0 #\-))
	  aux)))

;*---------------------------------------------------------------------*/
;*    unsigned_to_string ...                                           */
;*---------------------------------------------------------------------*/
(define (unsigned_to_string x radix)
   
   (define (integer-bits-count x radix)
      (let loop ((bits 0)
		 (ax x))
	 (if (=u64 ax #u64:0)
	     bits
	     (loop (+fx bits 1) (/u64 ax radix)))))
   
   (let ((ux (fixnum->uint64 x))
	 (ur (fixnum->uint64 radix)))
      (if (=u64 x #u64:0)
	  (make-string 1 #\0)
	  (let* ((bits (integer-bits-count ux ur))
		 (aux (make-string bits)))
	     (string-set! aux (-fx bits 1) #a000)
	     (let loop ((bits bits)
			(ax ux))
		(unless (=u64 ax #u64:0)
		   (string-set! aux (-fx bits 1)
		      (num->char (uint64->fixnum (remainderu64 ax radix))))
		   (loop (-fx bits 1) (/u64 ax radix))))
	     aux))))

;*---------------------------------------------------------------------*/
;*    ullong_to_string ...                                             */
;*---------------------------------------------------------------------*/
(define (ullong_to_string x radix)
   (unsigned_to_string x radix))

;*---------------------------------------------------------------------*/
;*    $integer->string/padding ...                                     */
;*---------------------------------------------------------------------*/
(define ($integer->string/padding x padding radix)
   (let* ((s (integer_to_string x radix))
	  (l (string-length s)))
      (if (<fx l padding)
	  (if (>=fx x 0)
	      (string-append (make-string (-fx padding l) #\0) s)
	      (let ((s (integer_to_string (negfx x) radix)))
		 (string-append "-" (make-string (-fx padding l) #\0) s)))
	  s)))

;*---------------------------------------------------------------------*/
;*    llong_to_string ...                                              */
;*---------------------------------------------------------------------*/
(define (llong_to_string x radix)
   (integer_to_string x radix))

;*---------------------------------------------------------------------*/
;*    integer-bits-count ...                                           */
;*---------------------------------------------------------------------*/
(define (integer-bits-count x radix)
   (let loop ((bits (if (<=fx x 0) 1 0))
	      (ax x))
      (if (=fx ax 0)
	  bits
	  (loop (+fx bits 1) (/fx ax radix)))))

;*---------------------------------------------------------------------*/
;*    BGL_STRTOL ...                                                   */
;*---------------------------------------------------------------------*/
(define ($$strtol string start radix)

   (define l (string-length string))
   
   (define (loop acc i)
      (if (<fx i l)
	  (let* ((c (string-ref-ur string i))
		 (n (char->num c)))
	     (loop (+fx (*fx acc radix) n) (+fx i 1)))
	  acc))
   
   (cond
      ((<=fx (-fx l start) 0) 0)
      ((char=? (string-ref string start) #\-) (negfx (loop 0 (+fx start 1))))
      (else (loop 0 start))))

;*---------------------------------------------------------------------*/
;*    $$strtoel ...                                                    */
;*---------------------------------------------------------------------*/
(define ($$strtoel string start radix)
   ($$strtol string start radix))

;*---------------------------------------------------------------------*/
;*    $$strtoll ...                                                    */
;*---------------------------------------------------------------------*/
(define ($$strtoll string start radix)
   ($$strtol string start radix))

;*---------------------------------------------------------------------*/
;*    num->char ...                                                    */
;*---------------------------------------------------------------------*/
(define (num->char num)
   (if (<fx num 10)
       (integer->char (+fx num (char->integer #\0)))
       (integer->char (+fx num (-fx (char->integer #\a) 10)))))
   
;*---------------------------------------------------------------------*/
;*    char->num ...                                                    */
;*---------------------------------------------------------------------*/
(define (char->num char)
   (cond
      ((char<=? char #\9)
       (-fx (char->integer char) (char->integer #\0)))
      ((char<=? char #\F)
       (+fx 10 (-fx (char->integer char) (char->integer #\A))))
      (else
       (+fx 10 (-fx (char->integer char) (char->integer #\a))))))
