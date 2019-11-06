;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Unsafe/kmp.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Apr 10 13:43:22 2006                          */
;*    Last change :  Sun Aug 25 09:18:21 2019 (serrano)                */
;*    Copyright   :  2006-19 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    "Knuth, Morris, and Pratt" search algorithm implementation.      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __kmp

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

   (export (kmp-table::pair ::bstring)
	   (kmp-mmap::elong ::pair ::mmap ::elong)
	   (kmp-string::long ::pair ::bstring ::long)))

;*---------------------------------------------------------------------*/
;*    kmp-table ...                                                    */
;*---------------------------------------------------------------------*/
(define (kmp-table p)
   (let* ((lp (string-length p))
	  (t (make-vector (+fx lp 2) 0))
	  (i 0)
	  (j -1)
	  (c #a000))
      (vector-set! t 0 j)
      (let while ()
	 (when (<fx i lp)
	    (cond
	       ((char=? (string-ref p i) c)
		(vector-set! t (+fx i 1) (+fx j 1))
		(set! j (+fx j 1))
		(set! i (+fx i 1)))
	       ((>fx j 0)
		(set! j (vector-ref t j)))
	       (else
		(vector-set! t (+fx i 1) 0)
		(set! i (+fx i 1))
		(set! j 0)))
	    (set! c (string-ref p j))
	    (while)))
      (cons t p)))

;*---------------------------------------------------------------------*/
;*    kmp-mmap ...                                                     */
;*---------------------------------------------------------------------*/
(define (kmp-mmap tp mm m)
   (cond
      ((not (vector? (car tp)))
       (bigloo-type-error "kmp-mmap" 'vector (car tp)))
      ((not (string? (cdr tp)))
       (bigloo-type-error "kmp-mmap" 'string (cdr tp)))
      ((not (=fx (vector-length (car tp)) (+fx 2 (string-length (cdr tp)))))
       (error "kmp-mmap" "Illegal kmp-table" tp))
      (else
       (let* ((t (car tp))
	      (p (cdr tp))
	      (ls (mmap-length mm))
	      (lp (fixnum->elong (string-length p))))
	  (let while ((i #e0))
	     (cond
		((=elong i lp)
		 m)
		((>=elong (+elong i m) ls)
		 #e-1)
		(else
		 (let ((fi (elong->fixnum i)))
		    (if (char=? (mmap-ref mm (+elong i m)) (string-ref p fi))
			(while (+elong i #e1))
			(let ((ti (fixnum->elong (vector-ref t fi))))
			   (set! m (+elong m (-elong i ti)))
			   (if (>elong i #e0)
			       (while ti)
			       (while i))))))))))))

;*---------------------------------------------------------------------*/
;*    kmp-string ...                                                   */
;*---------------------------------------------------------------------*/
(define (kmp-string tp str m)
   (cond
      ((not (vector? (car tp)))
       (bigloo-type-error "kmp-string" 'vector (car tp)))
      ((not (string? (cdr tp)))
       (bigloo-type-error "kmp-string" 'string (cdr tp)))
      ((not (=fx (vector-length (car tp)) (+fx 2 (string-length (cdr tp)))))
       (error "kmp-string" "Illegal kmp-table" tp))
      (else
       (let* ((t (car tp))
	      (p (cdr tp))
	      (ls (string-length str))
	      (lp (string-length p)))
	  (let while ((i 0))
	     (cond
		((=fx i lp)
		 m)
		((>=fx (+fx i m) ls)
		 -1)
		(else
		 (if (char=? (string-ref str (+fx i m)) (string-ref-ur p i))
		     (while (+fx i 1))
		     (let ((ti (vector-ref t i)))
			(set! m (+fx m (-fx i ti)))
			(if (>fx i 0)
			    (while ti)
			    (while i)))))))))))

