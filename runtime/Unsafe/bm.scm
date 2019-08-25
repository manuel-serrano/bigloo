;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Unsafe/bm.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 15 11:05:03 2016                          */
;*    Last change :  Sun Aug 25 09:18:37 2019 (serrano)                */
;*    Copyright   :  2016-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    "Boyer Moore" search algorithm.                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bm

   (import __error
	   __param)
   
   (use    __mmap
	   __type
	   __bigloo
	   __tvector
	   __bexit
	   __object
	   __thread
	   __bignum
	   __srfi4
	   __bit
	   
	   __r4_numbers_6_5
	   __r4_numbers_6_5_fixnum
	   __r4_numbers_6_5_flonum
	   __r4_booleans_6_1
	   __r4_symbols_6_4
	   __r4_vectors_6_8
	   __r4_control_features_6_9
	   __r4_pairs_and_lists_6_3
	   __r4_characters_6_6
	   __r4_equivalence_6_2 
	   __r4_strings_6_7
	   __r4_ports_6_10_1
	   __foreign
	   __evenv)

   (export (bm-table::epair ::bstring)
	   (bm-mmap::elong ::epair ::mmap ::elong)
	   (bm-string::long ::epair ::bstring ::long)
	   (bmh-table::epair ::bstring)
	   (bmh-mmap::elong ::epair ::mmap ::elong)
	   (bmh-string::long ::epair ::bstring ::long)))

;*---------------------------------------------------------------------*/
;*    alphabet-length ...                                              */
;*---------------------------------------------------------------------*/
(define (alphabet-length)
   256)

;*---------------------------------------------------------------------*/
;*    for  ....                                                        */
;*---------------------------------------------------------------------*/
(define-macro (for var min max . body)
   (let ((loop (gensym 'for))
	 (stop (gensym 'max)))
      `(let ((,stop ,max))
	  (let ,loop ((,var ,min))
		(when (<fx ,var ,stop)
		   ,@body
		   (,loop (+fx ,var 1)))))))

;*---------------------------------------------------------------------*/
;*    rof  ....                                                        */
;*---------------------------------------------------------------------*/
(define-macro (rof var max min . body)
   (let ((loop (gensym 'for))
	 (stop (gensym 'max)))
      `(let ((,stop ,min))
	  (let ,loop ((,var ,max))
		(when (>=fx ,var ,stop)
		   ,@body
		   (,loop (-fx ,var 1)))))))

;*---------------------------------------------------------------------*/
;*    istring-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (istring-ref ref obj i)
   `(char->integer (,ref ,obj ,i)))

;*---------------------------------------------------------------------*/
;*    make-delta1 ...                                                  */
;*    -------------------------------------------------------------    */
;*    delta1 table: delta1[c] contains the distance between the last   */
;*    character of pat and the rightmost occurrence of c in pat.       */
;*    If c does not occur in pat, then delta1[c] = patlen.             */
;*    If c is at string[i] and c != pat[patlen-1], we can              */
;*    safely shift i over by delta1[c], which is the minimum distance  */
;*    needed to shift pat forward to get string[i] lined up            */
;*    with some character in pat.                                      */
;*    this algorithm runs in alphabet_len+patlen time.                 */
;*---------------------------------------------------------------------*/
(define (make-delta1 delta1::u32vector pat::bstring)
   (let ((not-found (fixnum->uint32 (string-length pat)))
	 (patlen-1 (-fx (string-length pat) 1)))
      (for i 0 (alphabet-length)
	 (u32vector-set! delta1 i not-found))
      (for i 0 patlen-1
	 (u32vector-set! delta1 (char->integer (string-ref pat i))
	    (fixnum->uint32 (-fx patlen-1 i))))))

;*---------------------------------------------------------------------*/
;*    is-prefix? ...                                                   */
;*    -------------------------------------------------------------    */
;*    true if the suffix of word starting from word[pos] is a prefix   */
;*    of word                                                          */
;*---------------------------------------------------------------------*/
(define (is-prefix?::bool word::bstring pos::long)
   (let ((suffixlen (-fx (string-length word) pos)))
      (let loop ((i 0))
	 (if (<fx i suffixlen)
	     (when (char=? (string-ref word i) (string-ref word (+fx pos i)))
		(loop (+fx i 1)))
	     #t))))

;*---------------------------------------------------------------------*/
;*    suffix-length ...                                                */
;*    -------------------------------------------------------------    */
;*    length of the longest suffix of word ending on word[pos].        */
;*    suffix_length("dddbcabc", 8, 4) = 2                              */
;*---------------------------------------------------------------------*/
(define (suffix-length::long word::bstring pos::long)
   (let ((wordlen-1 (-fx (string-length word) 1)))
      (let loop ((i 0))
	 (if (char=? (string-ref word (-fx pos i))
		  (string-ref word (-fx wordlen-1 i)))
	     (if (<fx i pos)
		 (loop (+fx i 1))
		 i)
	     i))))

;*---------------------------------------------------------------------*/
;*    make-delta2 ...                                                  */
;*    -------------------------------------------------------------    */
;*    delta2 table: given a mismatch at pat[pos], we want to align     */
;*    with the next possible full match could be based on what we      */
;*    know about pat[pos+1] to pat[patlen-1].                          */
;*                                                                     */
;*    In case 1:                                                       */
;*    pat[pos+1] to pat[patlen-1] does not occur elsewhere in pat,     */
;*    the next plausible match starts at or after the mismatch.        */
;*    If, within the substring pat[pos+1 .. patlen-1], lies a prefix   */
;*    of pat, the next plausible match is here (if there are multiple  */
;*    prefixes in the substring, pick the longest). Otherwise, the     */
;*    next plausible match starts past the character aligned with      */
;*    pat[patlen-1].                                                   */
;*                                                                     */
;*    In case 2:                                                       */
;*    pat[pos+1] to pat[patlen-1] does occur elsewhere in pat. The     */
;*    mismatch tells us that we are not looking at the end of a match. */
;*    We may, however, be looking at the middle of a match.            */
;*                                                                     */
;*    The first loop, which takes care of case 1, is analogous to      */
;*    the KMP table, adapted for a 'backwards' scan order with the     */
;*    additional restriction that the substrings it considers as       */
;*    potential prefixes are all suffixes. In the worst case scenario  */
;*    pat consists of the same letter repeated, so every suffix is     */
;*    a prefix. This loop alone is not sufficient, however:            */
;*    Suppose that pat is "ABYXCDBYX", and text is ".....ABYXCDEYX".   */
;*    We will match X, Y, and find B != E. There is no prefix of pat   */
;*    in the suffix "YX", so the first loop tells us to skip forward   */
;*    by 9 characters.                                                 */
;*    Although superficially similar to the KMP table, the KMP table   */
;*    relies on information about the beginning of the partial match   */
;*    that the BM algorithm does not have.                             */
;*                                                                     */
;*    The second loop addresses case 2. Since suffix_length may not be */
;*    unique, we want to take the minimum value, which will tell us    */
;*    how far away the closest potential match is.                     */
;*---------------------------------------------------------------------*/
(define (make-delta2 delta2::u32vector pat::bstring)
   (let* ((patlen-1 (-fx (string-length pat) 1))
	  (last-prefix-index patlen-1))
      ;; first loop
      (rof p patlen-1 0
	 (when (is-prefix? pat (+fx p 1))
	    (set! last-prefix-index (+fx p 1)))
	 (u32vector-set! delta2 p
	    (+fx last-prefix-index (-fx patlen-1 p))))
      ;; second loop
      (for p 0 patlen-1
	 (let ((slen (suffix-length pat p)))
	    (unless (char=? (string-ref pat (-fx p slen))
		       (string-ref pat (-fx patlen-1 slen)))
	       (u32vector-set! delta2 (-fx patlen-1 slen)
		  (fixnum->uint32
		     (+fx (-fx patlen-1 p) slen))))))))

;*---------------------------------------------------------------------*/
;*    bm-table ...                                                     */
;*---------------------------------------------------------------------*/
(define (bm-table pat::bstring)
   (let ((delta1 (make-u32vector (alphabet-length)))
	 (delta2 (make-u32vector (string-length pat))))
      (make-delta1 delta1 pat)
      (make-delta2 delta2 pat)
      (econs delta1 delta2 pat)))
   
;*---------------------------------------------------------------------*/
;*    bm-search ...                                                    */
;*---------------------------------------------------------------------*/
(define-macro (bm-search ref len conv)
   `(cond
      ((not (u32vector? (car tp)))
       (bigloo-type-error "bm" 'u32vector (car tp)))
      ((not (u32vector? (cdr tp)))
       (bigloo-type-error "bm" 'u32vector (cdr tp)))
      ((not (string? (cer tp)))
       (error "bm" "Illegal bm-table" tp))
      (else
       (let* ((pat (cer tp))
	      (patlen (string-length pat)))
	  (if (=fx patlen 0)
	      -1
	      (let ((delta1 (car tp))
		    (delta2 (cdr tp))
		    (strlen (,len obj)))
		 (let loop ((i::long (+fx m (-fx patlen 1))))
		    (if (<fx i strlen)
			(let liip ((j (-fx patlen 1)))
			   (cond
			      ((<fx j 0)
			       (,conv (+fx i 1)))
			      ((char=? (,ref obj (,conv i))
				  (string-ref pat j))
			       (set! i (-fx i 1))
			       (liip (-fx j 1)))
			      (else
			       (let ((inc (maxfx
					     (uint32->fixnum
						(u32vector-ref delta1
						   (char->integer
						      (,ref obj i))))
					     (uint32->fixnum
						(u32vector-ref delta2 j)))))
				  (loop (+fx i inc))))))
			(,conv -1)))))))))

;*---------------------------------------------------------------------*/
;*    bm-mmap ...                                                      */
;*---------------------------------------------------------------------*/
(define (bm-mmap tp obj m)
   (bm-search mmap-ref mmap-length fixnum->elong))

;*---------------------------------------------------------------------*/
;*    bm-string ...                                                    */
;*---------------------------------------------------------------------*/
(define (bm-string tp obj m)
   (bm-search string-ref string-length (lambda (x) x)))

;*---------------------------------------------------------------------*/
;*    bmh-table ...                                                    */
;*---------------------------------------------------------------------*/
(define (bmh-table pat::bstring)
   (let ((delta1 (make-u32vector (alphabet-length))))
      (make-delta1 delta1 pat)
      (cons delta1 pat)))
   
;*---------------------------------------------------------------------*/
;*    bmh-search ...                                                   */
;*---------------------------------------------------------------------*/
(define-macro (bmh-search ref len conv)
   `(cond
       ((not (u32vector? (car tp)))
	(bigloo-type-error "bmh" 'u32vector (car tp)))
       ((not (string? (cdr tp)))
	(error "bmh" "Illegal bmh-table" tp))
       (else
	(let* ((pat (cdr tp))
	       (patlen (string-length pat)))
	   (if (=fx patlen 0)
	       -1
	       (let ((delta1 (car tp))
		     (strlen (,len obj)))
		  (let while1 ((skip 0))
		     (if (>=fx (-fx strlen skip) patlen)
			 (let while2 ((i (-fx patlen 1)) )
			    (cond
			       ((not (char=? (,ref obj (+fx skip i))
					(string-ref pat i)))
				(while1
				   (+fx skip
				      (uint32->fixnum
					 (u32vector-ref delta1
					    (char->integer
					       (,ref obj
						  (+fx skip (-fx patlen 1)))))))))
			       ((=fx i 0)
				(,conv skip))
			       (else
				(while2 (-fx i 1)))))
			 (,conv -1)))))))))
;*                                                                     */
;* 		                                                       */
;* 		 (let loop ((i::long (+fx m (-fx patlen 1))))          */
;* 		    (if (<fx i strlen)                                 */
;* 			(let liip ((j (-fx patlen 1)))                 */
;* 			   (cond                                       */
;* 			      ((<fx j 0)                               */
;* 			       (,conv (+fx i 1)))                      */
;* 			      ((char=? (,ref obj (,conv i))            */
;* 				  (string-ref pat j))                  */
;* 			       (set! i (-fx i 1))                      */
;* 			       (liip (-fx j 1)))                       */
;* 			      (else                                    */
;* 			       (let ((inc (maxfx                       */
;* 					     (uint32->fixnum           */
;* 						(u32vector-ref delta1  */
;* 						   (char->integer      */
;* 						      (,ref obj i))))  */
;* 					     (uint32->fixnum           */
;* 						(u32vector-ref delta2 j))))) */
;* 				  (loop (+fx i inc))))))               */
;* 			(,conv -1)))))))))                             */

;*---------------------------------------------------------------------*/
;*    bmh-mmap ...                                                     */
;*---------------------------------------------------------------------*/
(define (bmh-mmap tp obj m)
   (bmh-search mmap-ref mmap-length fixnum->elong))

;*---------------------------------------------------------------------*/
;*    bmh-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (bmh-string tp obj m)
   (bmh-search string-ref string-length (lambda (x) x)))
