;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Rgc/rgcset.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep  9 12:37:15 1998                          */
;*    Last change :  Sat Apr 21 16:08:03 2018 (serrano)                */
;*    -------------------------------------------------------------    */
;*    A simple implementation of bit vectors for handling set of       */
;*    integers.                                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __rgc_set
 
   (import  __error)

   (use     __type
	    __bigloo
	    __tvector
	    __structure
	    __bit
	    __param
	    __object
	    __thread
	    __bexit
	    __bignum
	    
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
	    __r4_vectors_6_8
	    __rgc)

   (extern  (macro $configure-int-bit-size::long "BGL_INT_BIT_SIZE"))

   (java    (class $configure
	       (field static int-bit-size::long "BGL_INT_BIT_SIZE")
	       "bigloo.configure"))

   (export  (make-rgcset          ::long)
	    (rgcset-length::long  <rgcset>)
	    (list->rgcset         <list> ::long)
	    (for-each-rgcset      ::procedure <rgcset>)
	    (rgcset->list         <rgcset>)
	    (rgcset-not!          <rgcset>)
	    (rgcset-not           <rgcset>)
	    (rgcset-and!          <rgcset> <rgcset>)
	    (rgcset-or!           <rgcset> <rgcset>)
	    (rgcset-or            <rgcset> <rgcset>)
	    (rgcset-but!          <rgcset> <rgcset>)
	    (rgcset-add!          <rgcset> ::long)
	    (rgcset-equal?::bool  <rgcset> <rgcset>)
	    (rgcset->hash::long   <rgcset>)
	    (rgcset-member?::bool <rgcset> ::long)
	    (rgcset-remove!       <rgcset> ::long)))

;*---------------------------------------------------------------------*/
;*    bit-per-word ...                                                 */
;*---------------------------------------------------------------------*/
(define bit-per-word (-fx $configure-int-bit-size 1))

;*---------------------------------------------------------------------*/
;*    rgcset structure ...                                             */
;*---------------------------------------------------------------------*/
(define-struct __rgcset max words)

;*---------------------------------------------------------------------*/
;*    make-rgcset ...                                                  */
;*---------------------------------------------------------------------*/
(define (make-rgcset size)
   (__rgcset size (make-vector (+ 1 (/fx size bit-per-word)) 0)))

;*---------------------------------------------------------------------*/
;*    rgcset-words-set! ...                                            */
;*---------------------------------------------------------------------*/
(define (rgcset-words-set! rgcset offset value)
   (vector-set! (__rgcset-words rgcset) offset value))

;*---------------------------------------------------------------------*/
;*    rgcset-words ...                                                 */
;*---------------------------------------------------------------------*/
(define (rgcset-words rgcset offset)
   (vector-ref (__rgcset-words rgcset) offset))

;*---------------------------------------------------------------------*/
;*    rgcset-words-len ...                                             */
;*---------------------------------------------------------------------*/
(define (rgcset-words-len rgcset)
   (vector-length (__rgcset-words rgcset)))

;*---------------------------------------------------------------------*/
;*    rgcset-add! ...                                                  */
;*---------------------------------------------------------------------*/
(define (rgcset-add! rgcset num::long)
   (let* ((word-num (/fx num bit-per-word))
	  (word-bit (remainderfx num bit-per-word))
	  (old      (rgcset-words rgcset word-num)))
      (rgcset-words-set! rgcset
			 word-num
			 (bit-or old (bit-lsh 1 word-bit)))))

;*---------------------------------------------------------------------*/
;*    rgcset-member? ...                                               */
;*---------------------------------------------------------------------*/
(define (rgcset-member? set num::long)
   (let* ((word-num (/fx num bit-per-word))
	  (word-bit (remainderfx num bit-per-word))
	  (old      (rgcset-words set word-num))
	  (mask     (bit-lsh 1 word-bit)))
      (=fx mask (bit-and old mask))))
   
;*---------------------------------------------------------------------*/
;*    list->rgcset ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->rgcset lst max)
   (let ((set (make-rgcset max)))
      (for-each (lambda (item) (rgcset-add! set item)) lst)
      set))
	   
;*---------------------------------------------------------------------*/
;*    rgcset->list ...                                                 */
;*---------------------------------------------------------------------*/
(define (rgcset->list set)
   (let ((max     (__rgcset-max set))
	 (maxmask (bit-lsh 1 bit-per-word)))
      (let loop ((i        0)
		 (word-num 0)
		 (word     (rgcset-words set 0))
		 (mask     1)
		 (res      '()))
	 (cond
	    ((=fx i max)
	     res)
	    ((=fx mask maxmask)
	     (loop i
		   (+fx word-num 1)
		   (rgcset-words set (+fx word-num 1))
		   1
		   res))
	    ((=fx (bit-and word mask) mask)
	     (loop (+fx i 1) word-num word (bit-lsh mask 1) (cons i res)))
	    (else
	     (loop (+fx i 1) word-num word (bit-lsh mask 1) res))))))
	     
;*---------------------------------------------------------------------*/
;*    for-each-rgcset ...                                              */
;*---------------------------------------------------------------------*/
(define (for-each-rgcset proc set)
   (let ((max     (__rgcset-max set))
	 (maxmask (bit-lsh 1 bit-per-word)))
      (let loop ((i        0)
		 (word-num 0)
		 (word     (rgcset-words set 0))
		 (mask     1))
	 (cond
	    ((=fx i max)
	     #unspecified)
	    ((=fx mask maxmask)
	     (loop i
		   (+fx word-num 1)
		   (rgcset-words set (+fx word-num 1))
		   1))
	    ((=fx (bit-and word mask) mask)
	     (proc i)
	     (loop (+fx i 1) word-num word (bit-lsh mask 1)))
	    (else
	     (loop (+fx i 1) word-num word (bit-lsh mask 1)))))))

;*---------------------------------------------------------------------*/
;*    rgcset-length ...                                                */
;*    -------------------------------------------------------------    */
;*    The number of elements in one <rgcset>                           */
;*---------------------------------------------------------------------*/
(define (rgcset-length set)
   (let ((num 0))
      (for-each-rgcset (lambda (x) (set! num (+fx 1 num))) set)
      num))

;*---------------------------------------------------------------------*/
;*    rgcset-not! ...                                                  */
;*---------------------------------------------------------------------*/
(define (rgcset-not! set)
;*    [assert (set) (__rgcset? set)]                                   */
   (let ((len (rgcset-words-len set)))
      (let loop ((i 0))
	 (if (< i len)
	     (begin
		(rgcset-words-set! set i (bit-not (rgcset-words set i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    rgcset-not ...                                                   */
;*---------------------------------------------------------------------*/
(define (rgcset-not set)
;*    [assert (set) (__rgcset? set)]                                   */
   (let ((len (rgcset-words-len set))
	 (new (make-rgcset (__rgcset-max set))))
      (let loop ((i 0))
	 (if (< i len)
	     (begin
		(rgcset-words-set! new i (bit-not (rgcset-words set i)))
		(loop (+fx i 1)))
	     new))))

;*---------------------------------------------------------------------*/
;*    rgcset-and! ...                                                  */
;*---------------------------------------------------------------------*/
(define (rgcset-and! set1 set2)
;*    [assert (set1 set2) (and (__rgcset? set1) (__rgcset? set2))]     */
   (let ((len1 (rgcset-words-len set1))
	 (len2 (rgcset-words-len set2)))
      (let loop ((i 0))
	 (if (and (< i len1) (< i len2))
	     (begin
		(rgcset-words-set! set1
				   i
				   (bit-and (rgcset-words set1 i)
					    (rgcset-words set2 i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    rgcset-or! ...                                                   */
;*---------------------------------------------------------------------*/
(define (rgcset-or! set1 set2)
;*    [assert (set1 set2) (and (__rgcset? set1) (__rgcset? set2))]     */
   (let ((len1 (rgcset-words-len set1))
	 (len2 (rgcset-words-len set2)))
;*       [assert (len1 len2) (=fx len1 len2)]                          */
      (let loop ((i 0))
	 (if (< i len1)
	     (begin
		(rgcset-words-set! set1
				   i
				   (bit-or (rgcset-words set1 i)
					   (rgcset-words set2 i)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    rgcset-or ...                                                    */
;*---------------------------------------------------------------------*/
(define (rgcset-or set1 set2)
;*    [assert (set1 set2) (and (__rgcset? set1) (__rgcset? set2))]     */
   (let* ((len1 (rgcset-words-len set1))
	  (len2 (rgcset-words-len set2))
	  (new  (make-rgcset (__rgcset-max set1))))
;*       [assert (len1 len2) (=fx len1 len2)]                          */
      (let loop ((i 0))
	 (if (< i len1)
	     (begin
		(rgcset-words-set! new
				   i
				   (bit-or (rgcset-words set1 i)
					   (rgcset-words set2 i)))
		(loop (+fx i 1)))
	     new))))

;*---------------------------------------------------------------------*/
;*    rgcset-but! ...                                                  */
;*---------------------------------------------------------------------*/
(define (rgcset-but! set1 set2)
   (let ((len1 (rgcset-words-len set1))
	 (len2 (rgcset-words-len set2)))
;*       [assert (len1 len2) (=fx len1 len2)]                          */
      (let loop ((i 0))
	 (if (< i len1)
	     (begin
		(rgcset-words-set! set1
				   i
				   (- (rgcset-words set1 i)
				      (rgcset-words set2 i)))
		(loop (+fx i 1)))))))
   
;*---------------------------------------------------------------------*/
;*    rgcset-equal? ...                                                */
;*---------------------------------------------------------------------*/
(define (rgcset-equal? set1 set2)
   (let ((len1 (rgcset-words-len set1))
	 (len2 (rgcset-words-len set2)))
      (and (=fx len1 len2)
	   (let ((w1 (__rgcset-words set1))
		 (w2 (__rgcset-words set2)))
	      (let loop ((i 0))
		 (cond
		    ((=fx i len1)
		     #t)
		    ((=fx (vector-ref w1 i) (vector-ref w2 i))
		     (loop (+fx i 1)))
		    (else
		     #f)))))))

;*---------------------------------------------------------------------*/
;*    rgcset->hash ...                                                 */
;*---------------------------------------------------------------------*/
(define (rgcset->hash set)
   (let ((len (rgcset-words-len set)))
      (let loop ((i   1)
		 (res (rgcset-words set 0)))
	 (if (=fx i len)
	     (if (<fx res 0)
		 (negfx res)
		 res)
	     (let ((v (+fx res (+fx (bit-lsh res 3) (rgcset-words set i)))))
		(loop (+fx i 1)
		      (if (=fx (rgcset-words set i) 0)
			  v
			  (+fx i v))))))))

;*---------------------------------------------------------------------*/
;*    rgcset-remove! ...                                               */
;*---------------------------------------------------------------------*/
(define (rgcset-remove! set num::long)
   (let* ((word-num (/fx num bit-per-word))
	  (word-bit (remainderfx num bit-per-word))
	  (old      (rgcset-words set word-num)))
      (rgcset-words-set! set word-num (bit-xor old (bit-lsh 1 word-bit)))))
