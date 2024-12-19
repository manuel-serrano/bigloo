;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/fixnum-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 22 12:33:04 2024                          */
;*    Last change :  Thu Dec 19 07:57:05 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Portable fixnum implementation                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export (integer_to_string::bstring ::long ::long)
	   ($integer->string/padding::bstring ::long ::long ::long)
	   ($$strtol::long string::bstring start::long radix::long)
	   ($$strtoll::llong string::bstring start::long radix::long))
   (extern (export integer_to_string "integer_to_string")
	   (export $integer->string/padding "integer->string/padding")))

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
   (let loop ((acc 0)
	      (i (-fx (string-length string) 1))
	      (k 1))
      (if (>=fx i 0)
	  (let* ((c (string-ref-ur string i))
		 (n (char->num c)))
	     (loop (+fx acc (*fx k n)) (-fx i 1) (*fx k radix)))
	  acc)))

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
