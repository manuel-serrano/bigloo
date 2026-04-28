;*=====================================================================*/
;*    serrano/prgm/project/bigloo/5.0a/runtime/Llib/hash.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  1 08:51:06 1994                          */
;*    Last change :  Mon Apr 27 13:32:05 2026 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The hash tables.                                                 */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Hash Tables@                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __hash

   (cond-expand
      ((and (not bigloo-c) (not bigloo-jvm))
       (include "Llib/hash-generic.sch")))
   
   (import  __error
	    __r4_symbols_6_4
	    __param
	    __weakhash)

   (use     __type
	    __bigloo
	    __structure
	    __bit
	    __tvector
            __weakptr
	    __object
	    __bexit
	    __bignum
	    __thread
	    __date
	    __custom
	    __ucs2
	    __unicode
	    __srfi4
	    __intext
	    
	    __r4_numbers_6_5
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_control_features_6_9
	    __r4_characters_6_6
	    __r4_booleans_6_1
	    __r4_vectors_6_8
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r5_control_features_6_4
	    
	    __foreign
	    __evenv

	    __r4_output_6_10_3
	    __r4_ports_6_10_1)

   (include "Llib/hash.sch")
   
   (extern  ($string-hash::long (::string ::int ::int) "bgl_string_hash")
	    ($string-hash-persistent::long (::string ::int ::int) "bgl_string_hash_persistent")
	    (symbol-hash-number::long (::symbol) "bgl_symbol_hash_number")
	    (symbol-hash-number-persistent::long (::symbol) "bgl_symbol_hash_number_persistent")
	    (keyword-hash-number::long (::keyword) "bgl_keyword_hash_number")
	    (keyword-hash-number-persistent::long (::keyword) "bgl_keyword_hash_number_persistent")
	    (obj-hash-number::long (::obj) "bgl_obj_hash_number")
	    (c-pointer-hashnumber::long (::obj ::long) "bgl_pointer_hashnumber")
	    (foreign-hash-number::long (::foreign) "bgl_foreign_hash_number")
	    (macro elong-hash-number::long (::elong) "(long)")
	    (macro llong-hash-number::long (::llong) "(long)")
	    (macro $strlen::long (::string) "strlen"))

   (wasm    (elong-hash-number "~0")
            (llong-hash-number "~0")
            ($strlen "(array.len ~0)"))
   
   (java    (class foreign
	       (method static $string-hash::long (::string ::int ::int)
		       "bgl_string_hash")
	       (method static $string-hash-persistent::int (::string ::int ::int)
		       "bgl_string_hash")
	       (method static symbol-hash-number::long (::symbol)
		       "bgl_symbol_hash_number")
	       (method static symbol-hash-number-persistent::long (::symbol)
		       "bgl_symbol_hash_number")
	       (method static keyword-hash-number::long (::keyword)
		       "bgl_keyword_hash_number")
	       (method static keyword-hash-number-persistent::long (::keyword)
		       "bgl_keyword_hash_number")
	       (method static obj-hash-number::long (::obj)
		       "bgl_obj_hash_number")
	       (method static foreign-hash-number::long (::obj)
		       "bgl_foreign_hash_number")
	       (method static c-pointer-hashnumber::long (::obj ::long)
		       "bgl_pointer_hash_number")
	       (method static elong-hash-number::long (::elong)
		       "bgl_elong_hash_number")
	       (method static llong-hash-number::long (::llong)
		       "bgl_llong_hash_number")))

   (export  (make-hashtable::struct #!key
	       (keys 'obj)
	       (size 128)
	       (max-bucket-length 10)
	       (eqtest #f)
	       (hash #f)
	       (weak 'none)
	       (max-length 16384)
	       (bucket-expansion 1.9)
	       (persistent #f))
	    (create-hashtable::struct #!key
	       (size 128)
	       (max-bucket-length 10)
	       (eqtest #f)
	       (hash #f)
	       (weak 'none)
	       (max-length 16384)
	       (bucket-expansion 1.9)
	       (persistent #f))
	    (create-hashtable-string::struct)
	    (get-hashnumber::long ::obj)
	    (get-hashnumber-persistent::long ::obj)
	    (inline get-pointer-hashnumber::long ::obj ::long)
	    (string-hash::long ::bstring #!optional (start 0) len)
	    (inline string-hash-number::long ::bstring)
	    (hashtable?::bool ::obj)
	    (hashtable-weak-data?::bool ::struct)
	    (hashtable-weak-keys?::bool ::struct)
            (hashtable-string?::bool ::struct)
	    (hashtable-size::long ::struct)
	    (hashtable-contains?::bool ::struct ::obj)
	    (hashtable-get::obj ::struct ::obj)
	    (string-hashtable-get::obj ::struct ::bstring)
	    ($string-hashtable-get::obj ::struct ::string)
	    (hashtable-put! ::struct ::obj ::obj)
	    (string-hashtable-put!::obj ::struct ::bstring ::obj)
	    (string-hashtable-put!::obj ::struct ::bstring ::obj)
	    (hashtable-update! ::struct ::obj ::procedure ::obj)
	    (hashtable-add! ::struct ::obj ::procedure ::obj ::obj)
	    (hashtable-remove!::bool ::struct ::obj)
	    (hashtable->vector::vector ::struct)
	    (hashtable->list::pair-nil ::struct)
	    (hashtable-key-list::pair-nil ::struct)
	    (hashtable-map::pair-nil ::struct ::procedure)
	    (hashtable-for-each ::struct ::procedure)
	    (hashtable-filter ::struct ::procedure)
	    (hashtable-filter-map ::struct ::procedure)
	    (hashtable-filter! ::struct ::procedure)
	    (hashtable-clear! ::struct)
            (hashtable-collisions::pair-nil ::struct)
	    (open-string-hashtable-contains?::bool ::struct ::bstring)
	    (open-string-hashtable-update!::obj ::struct ::bstring ::procedure ::obj)
	    (open-string-hashtable-add! ::struct ::bstring ::procedure obj init)
	    (open-string-hashtable-remove! ::struct ::bstring)
            (open-string-hashtable->vector::vector table::struct)
            (open-string-hashtable->list::pair-nil table::struct)
	    (open-string-hashtable-map::pair-nil ::struct ::procedure)
	    (open-string-hashtable-filter ::struct ::procedure)
	    (open-string-hashtable-filter-map ::struct ::procedure)
	    (open-string-hashtable-for-each ::struct ::procedure)
	    (open-string-hashtable-filter! ::struct ::procedure)
	    (string-hashtable-contains?::bool ::struct ::bstring)
	    (string-hashtable-update!::obj ::struct ::bstring ::procedure ::obj)
	    (string-hashtable-add! ::struct ::bstring ::procedure obj init)
	    (string-hashtable-remove! ::struct ::bstring)
            (string-hashtable->vector::vector table::struct)
            (string-hashtable->list::pair-nil table::struct)
	    (string-hashtable-map::pair-nil ::struct ::procedure)
	    (string-hashtable-filter ::struct ::procedure)
	    (string-hashtable-filter-map ::struct ::procedure)
	    (string-hashtable-for-each ::struct ::procedure)
	    (string-hashtable-filter! ::struct ::procedure)
	    )

   (pragma  (hashtable-contains? side-effect-free)
	    (hashtable-get side-effect-free)))
   
;*---------------------------------------------------------------------*/
;*    Default hashtable configuration                                  */
;*---------------------------------------------------------------------*/
(define default-hashtable-bucket-length 128)
(define default-max-bucket-length 10)

(define (STRING-HASHTABLE-THRESHOLD)
   (*fx 8 (*fx 1024 1024)))
   
;*---------------------------------------------------------------------*/
;*    make-hashtable ...                                               */
;*---------------------------------------------------------------------*/
(define (make-hashtable::struct #!key
	   (keys 'obj)
	   (size 128)
	   (max-bucket-length 10)
	   (eqtest #f)
	   (hash #f)
	   (weak 'none)
	   (max-length 16384)
	   (bucket-expansion 1.9)
	   (persistent #f))
   (let ((wk::long (case weak
		      ;; integers are also used in the case construct
		      ;; to let backend that use Scheme hashtables for
		      ;; implementing symbols to bootstrap more easily
		      ((keys 1) (weak-keys))
		      ((data 2) (weak-data))
		      ((both 3) (weak-both))
		      ((none 0) (weak-none))
		      (else (error "make-hashtable"
			       "Illegal weak argument"
			       weak)))))
      (if (eq? keys 'string)
	  (cond
	     ((not (=fx wk (weak-none)))
	      (error "make-hashtable"
		 "string-hashtable cannot be weak"
		 weak))
	     (eqtest
	      (error "make-hashtable"
		 "string-hashtable cannot specigy a comparison function"
		 eqtest))
	     (hash
	      (error "make-hashtable"
		 "string-hashtable cannot use a custom hash function"
		 hash))
	     (else
	      (%hashtable 0 size (make-vector (*fx 3 size) #f) #unspecified #unspecified (weak-string) 0 0)))
	  (begin
	     (%hashtable 0 max-bucket-length (make-vector size '())
		eqtest hash
		wk max-length bucket-expansion)))))

;*---------------------------------------------------------------------*/
;*    create-hashtable-string ...                                      */
;*---------------------------------------------------------------------*/
(define (create-hashtable-string)
   ;; cannot call the generic make-hashtable function because used
   ;; for boot the wasm symbol tables
   (let ((size 128)
	 (wk (weak-string)))
      (%hashtable 0 size (make-vector (*fx 3 size) #f) #unspecified #unspecified wk 0 0)))

;*---------------------------------------------------------------------*/
;*    create-hashtable ...                                             */
;*---------------------------------------------------------------------*/
(define (create-hashtable::struct #!key
	   (size 128)
	   (max-bucket-length 10)
	   (eqtest #f)
	   (hash #f)
	   (weak 'none)
	   (max-length 16384)
	   (bucket-expansion 1.9)
	   (persistent #f))
   (let ((wk::long (case weak
		      ;; integers are also used in the case construct
		      ;; to let backend that use Scheme hashtables for
		      ;; implementing symbols to bootstrap more easily
		      ((keys 1) (weak-keys))
		      ((data 2) (weak-data))
		      ((both 3) (weak-both))
		      ((none 0) (weak-none))
		      ((string 4) (weak-string))
		      ((open-string 8) (weak-string))
		      ((#t) (weak-data))
		      ((#f) (weak-none))
		      (else (error "create-hashtable"
			       "Illegal weak argument"
			       weak)))))
      (when persistent
	 (if hash
	     (error "create-hashtable"
		"Persistent hashtable cannot use custom hash function"
		hash)
	     (set! hash 'persistent)))
      (if (=fx wk (weak-string))
	  (cond
	     (eqtest
	      (error "create-hashtable"
		 "Cannot provide eqtest for string hashtable" eqtest))
	     (hash
	      (error "create-hashtable"
		 "Cannot provide hash for string hashtable" hash))
	     (else
	      (%hashtable 0 size (make-vector (*fx 3 size) #f) #unspecified #unspecified wk 0 0)))
	  (%hashtable 0 max-bucket-length (make-vector size '())
	     eqtest hash
	     wk max-length bucket-expansion))))

;*---------------------------------------------------------------------*/
;*    make-string-hashtable ...                                        */
;*    -------------------------------------------------------------    */
;*    Mainly used for backend that use a Scheme implementation         */
;*    of symbols' hashtable.                                           */
;*---------------------------------------------------------------------*/
(define (make-string-hashtable::struct)
   (make-hashtable :size 128 :keys 'string))

;*---------------------------------------------------------------------*/
;*    hashtable? ...                                                   */
;*---------------------------------------------------------------------*/
(define (hashtable?::bool obj::obj)
   (%hashtable? obj))

;*---------------------------------------------------------------------*/
;*    hashtable-weak? ...                                              */
;*---------------------------------------------------------------------*/
(define (hashtable-weak?::bool table::struct)
   (not (=fx 0 (bit-and (%hashtable-weak table) (weak-both)))))

;*---------------------------------------------------------------------*/
;*    hashtable-string? ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-string?::bool table::struct)
   (or (=fx (weak-string) (%hashtable-weak table))
       (=fx (weak-open-string) (%hashtable-weak table))))

;*---------------------------------------------------------------------*/
;*    hashtable-weak-keys? ...                                         */
;*---------------------------------------------------------------------*/
(define (hashtable-weak-keys?::bool table::struct)
   (not (=fx 0 (bit-and (weak-keys) (%hashtable-weak table)))))

;*---------------------------------------------------------------------*/
;*    hashtable-weak-data? ...                                         */
;*---------------------------------------------------------------------*/
(define (hashtable-weak-data?::bool table::struct)
   (not (=fx 0 (bit-and (weak-data) (%hashtable-weak table)))))

;*---------------------------------------------------------------------*/
;*    hashtable-size ...                                               */
;*---------------------------------------------------------------------*/
(define (hashtable-size::long table::struct)
   (%hashtable-size table))

;*---------------------------------------------------------------------*/
;*    hashtable->vector ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable->vector::vector table::struct)
   (cond
      ((hashtable-string? table)
       (string-hashtable->vector table))
      ((hashtable-weak? table)
       (weak-hashtable->vector table))
      (else
       (plain-hashtable->vector table))))

;*---------------------------------------------------------------------*/
;*    string-hashtable->vector ...                                     */
;*---------------------------------------------------------------------*/
(define (string-hashtable->vector::vector table::struct)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table))
	  (vec (make-vector (%hashtable-size table))))
      (let loop ((i 0)
		 (w 0))
	 (if (=fx i size3)
	     vec
	     (if (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
		 (begin
		    (vector-set! vec w (vector-ref buckets (+fx i 1)))
		    (loop (+fx i 3) (+fx w 1)))
		 (loop (+fx i 3) w))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable->vector ...                                      */
;*---------------------------------------------------------------------*/
(define (plain-hashtable->vector table::struct)
   (let* ((vec (make-vector (hashtable-size table)))
	  (buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
		 (w 0))
	 (if (=fx i buckets-len)
	     vec
	     (let liip ((bucket (vector-ref-ur buckets i))
			(w w))
		(if (null? bucket)
		    (loop (+fx i 1) w)
		    (begin
		       (vector-set-ur! vec w (cdar bucket))
		       (liip (cdr bucket) (+fx w 1)))))))))

;*---------------------------------------------------------------------*/
;*    hashtable->list ...                                              */
;*---------------------------------------------------------------------*/
(define (hashtable->list::pair-nil table::struct)
   (cond
      ((hashtable-string? table)
       (string-hashtable->list table))
      ((hashtable-weak? table)
       (weak-hashtable->list table))
      (else
       (plain-hashtable->list table))))

;*---------------------------------------------------------------------*/
;*    string-hashtable->list ...                                       */
;*---------------------------------------------------------------------*/
(define (string-hashtable->list::pair-nil table::struct)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i size3)
	     res
	     (if (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
		 (loop (+fx i 3) (cons (vector-ref buckets (+fx i 1)) res))
		 (loop (+fx i 3) res))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable->list ...                                        */
;*---------------------------------------------------------------------*/
(define (plain-hashtable->list table::struct)
   (let* ((vec (make-vector (hashtable-size table)))
	  (buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i buckets-len)
	     res
	     (let liip ((bucket (vector-ref-ur buckets i))
			(res res))
		(if (null? bucket)
		    (loop (+fx i 1) res)
		    (liip (cdr bucket) (cons (cdar bucket) res))))))))

;*---------------------------------------------------------------------*/
;*    hashtable-key-list ...                                           */
;*---------------------------------------------------------------------*/
(define (hashtable-key-list::pair-nil table::struct)
   (cond
      ((hashtable-string? table)
       (string-hashtable-key-list table))
      ((hashtable-weak? table)
       (weak-hashtable-key-list table))
      (else
       (plain-hashtable-key-list table))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-key-list ...                                    */
;*---------------------------------------------------------------------*/
(define (string-hashtable-key-list::pair-nil table::struct)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i size3)
	     res
	     (if (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
		 (loop (+fx i 3) (cons (vector-ref buckets i) res))
		 (loop (+fx i 3) res))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-key-list ...                                     */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-key-list table::struct)
   (let* ((vec (make-vector (hashtable-size table)))
	  (buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i buckets-len)
	     res
	     (let liip ((bucket (vector-ref-ur buckets i))
			(res res))
		(if (null? bucket)
		    (loop (+fx i 1) res)
		    (liip (cdr bucket) (cons (caar bucket) res))))))))

;*---------------------------------------------------------------------*/
;*    hashtable-map ...                                                */
;*---------------------------------------------------------------------*/
(define (hashtable-map::pair-nil table::struct fun::procedure)
   (cond
      ((hashtable-string? table)
       (string-hashtable-map table fun))
      ((hashtable-weak? table)
       (weak-hashtable-map table fun))
      (else
       (plain-hashtable-map table fun))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-map ...                                         */
;*---------------------------------------------------------------------*/
(define (string-hashtable-map::pair-nil table::struct fun)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i size3)
	     res
	     (if (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
		 (loop (+fx i 3)
		    (cons
		       (fun (vector-ref buckets i) (vector-ref buckets (+fx i 1)))
		       res))
		 (loop (+fx i 3) res))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-map ...                                          */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-map table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
		 (res '()))
	 (if (<fx i buckets-len)
	     (let liip ((lst (vector-ref-ur buckets i))
			(res res))
		(if (null? lst)
		    (loop (+fx i 1) res)
		    (let ((cell (car lst)))
		       (liip (cdr lst)
			     (cons (fun (car cell) (cdr cell)) res)))))
	     res))))

;*---------------------------------------------------------------------*/
;*    hashtable-for-each ...                                           */
;*---------------------------------------------------------------------*/
(define (hashtable-for-each table::struct fun::procedure)
   (cond
      ((hashtable-string? table)
       (string-hashtable-for-each table fun))
      ((hashtable-weak? table)
       (weak-hashtable-for-each table fun))
      (else
       (plain-hashtable-for-each table fun))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-for-each ...                                    */
;*---------------------------------------------------------------------*/
(define (string-hashtable-for-each table::struct fun)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0))
	 (unless (=fx i size3)
	    (when (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
	       (fun (vector-ref buckets i) (vector-ref buckets (+fx i 1))))
	    (loop (+fx i 3))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-for-each ...                                     */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-for-each table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0))
	 (if (<fx i buckets-len)
	     (begin
		(for-each (lambda (cell)
			     (fun (car cell) (cdr cell)))
		   (vector-ref-ur buckets i))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    hashtable-filter ...                                             */
;*---------------------------------------------------------------------*/
(define (hashtable-filter table::struct fun::procedure)
   (cond
      ((hashtable-string? table)
       (string-hashtable-filter table fun))
      ((hashtable-weak? table)
       (weak-hashtable-filter table fun))
      (else
       (plain-hashtable-filter table fun))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-filter ...                                      */
;*---------------------------------------------------------------------*/
(define (string-hashtable-filter table::struct fun)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i size3)
	     res
	     (if (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
		 (let* ((c (vector-ref buckets (+fx i 1)))
                        (o (vector-ref buckets i))
			(v (fun o c)))
		    (loop (+fx i 3)
		       (if v (cons c res) res)))
		 (loop (+fx i 3) res))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-filter ...                                       */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-filter table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
		 (res '()))
	 (if (<fx i buckets-len)
	     (let liip ((lst (vector-ref-ur buckets i))
			(res res))
		(if (null? lst)
		    (loop (+fx i 1) res)
		    (let* ((c (car lst))
			   (v (fun (car c) (cdr c))))
		       (liip (cdr lst)
			  (if v (cons (cdr c) res) res)))))
	     res))))

;*---------------------------------------------------------------------*/
;*    hashtable-filter-map ...                                         */
;*---------------------------------------------------------------------*/
(define (hashtable-filter-map table::struct fun::procedure)
   (cond
      ((hashtable-string? table)
       (string-hashtable-filter-map table fun))
      ((hashtable-weak? table)
       (weak-hashtable-filter-map table fun))
      (else
       (plain-hashtable-filter-map table fun))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-filter-map ...                                  */
;*---------------------------------------------------------------------*/
(define (string-hashtable-filter-map table::struct fun)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i size3)
	     res
	     (if (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
		 (let* ((c (vector-ref buckets (+fx i 1)))
			(v (fun (vector-ref buckets i) c)))
		    (loop (+fx i 3)
		       (if v (cons v res) res)))
		 (loop (+fx i 3) res))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-filter-map ...                                   */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-filter-map table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
		 (res '()))
	 (if (<fx i buckets-len)
	     (let liip ((lst (vector-ref-ur buckets i))
			(res res))
		(if (null? lst)
		    (loop (+fx i 1) res)
		    (let* ((c (car lst))
			   (v (fun (car c) (cdr c))))
		       (liip (cdr lst)
			  (if v (cons v res) res)))))
	     res))))

;*---------------------------------------------------------------------*/
;*    hashtable-filter! ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-filter! table::struct fun::procedure)
   (cond
      ((hashtable-string? table)
       (string-hashtable-filter! table fun))
      ((hashtable-weak? table)
       (weak-hashtable-filter! table fun))
      (else
       (plain-hashtable-filter! table fun))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-filter! ...                                     */
;*---------------------------------------------------------------------*/
(define (string-hashtable-filter! table::struct fun)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0))
	 (unless (=fx i size3)
	    (when (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
	       (unless (fun (vector-ref buckets i) (vector-ref buckets (+fx i 1)))
		  (vector-set! buckets (+fx i 1) #f)
		  (vector-set! buckets (+fx i 2) #f)
		  (string-hashtable-ntombstone-inc! table)))
	    (loop (+fx i 3))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-filter! ...                                      */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-filter! table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0) (delta 0))
	 (if (<fx i buckets-len)
	     (let* ((l (vector-ref-ur buckets i))
                    (old-len (length l))
                    (newl (filter! (lambda (cell)
				      (fun (car cell) (cdr cell)))
			     l))
                    (new-len (length newl)))
		(vector-set-ur! buckets i newl)
		(loop (+fx i 1) (+fx delta (-fx new-len old-len))))
             (%hashtable-size-set! table
		(+fx delta (%hashtable-size table)))))))

;*---------------------------------------------------------------------*/
;*    hashtable-clear! ...                                             */
;*---------------------------------------------------------------------*/
(define (hashtable-clear! table::struct)
   (cond
      ((hashtable-string? table)
       (string-hashtable-clear! table))
      ((hashtable-weak? table)
       (weak-hashtable-clear! table))
      (else
       (plain-hashtable-clear! table))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-clear! ...                                      */
;*---------------------------------------------------------------------*/
(define (string-hashtable-clear! table)
   (vector-fill! (%hashtable-buckets table) #f)
   (%hashtable-ntombstone-set! table 0)
   (%hashtable-size-set! table 0))
   
;*---------------------------------------------------------------------*/
;*    plain-hashtable-clear! ...                                       */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-clear! table::struct)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0))
	 (if (<fx i buckets-len)
	     (begin
		(vector-set-ur! buckets i '())
		(loop (+fx i 1)))
             (%hashtable-size-set! table 0)))))

;*---------------------------------------------------------------------*/
;*    hashtable-contains? ...                                          */
;*---------------------------------------------------------------------*/
(define (hashtable-contains?::bool table::struct key::obj)
   (cond
      ((hashtable-string? table)
       (string-hashtable-contains? table key))
      ((hashtable-weak? table)
       (weak-hashtable-contains? table key))
      (else
       (plain-hashtable-contains? table key))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-contains? ...                                   */
;*---------------------------------------------------------------------*/
(define (string-hashtable-contains?::bool t::struct key::bstring)
   (let* ((size (%hashtable-max-bucket-len t))
	  (buckets (%hashtable-buckets t))
	  (hash ($string-hash key 0 (string-length key))))
      ;; empty bucket
      (let loop ((off (remainderfx hash size))
		 (i 1))
	 (let ((off3 (*fx off 3)))
	    (when (vector-ref buckets off3)
	       (if (string=? (vector-ref buckets off3) key)
		   (when (vector-ref buckets (+fx off3 1))
		      #t)
		   (let ((noff (+fx off (*fx i i))))
		      (if (>=fx noff size)
			  (loop (remainderfx noff size) (+fx i 1))
			  (loop noff (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-contains? ...                                    */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-contains? table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num)))
      (let loop ((bucket bucket))
	 (cond
	    ((null? bucket)
	     #f)
	    ((hashtable-equal? table (caar bucket) key)
	     #t)
	    (else
	     (loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    hashtable-get ...                                                */
;*---------------------------------------------------------------------*/
(define (hashtable-get table::struct key::obj)
   (cond
      ((hashtable-string? table) (string-hashtable-get table key))
      ((hashtable-weak? table) (weak-hashtable-get table key))
      (else (plain-hashtable-get table key))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-get ...                                          */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-get table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num)))
      (let loop ((bucket bucket))
	 (cond
	    ((null? bucket)
	     #f)
	    ((hashtable-equal? table (caar bucket) key)
	     (cdar bucket))
	    (else
	     (loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-get ...                                         */
;*---------------------------------------------------------------------*/
(define (string-hashtable-get t key)
   (let* ((size (%hashtable-max-bucket-len t))
	  (buckets (%hashtable-buckets t))
	  (hash ($string-hash key 0 (string-length key))))
      (let loop ((off (remainderfx hash size))
		 (i 1))
	 (let ((off3 (*fx off 3)))
	    (when (vector-ref buckets off3)
	       (if (string=? (vector-ref buckets off3) key)
		   (when (vector-ref buckets (+fx off3 2))
		      (vector-ref buckets (+fx off3 1)))
		   (let ((noff (+fx off (*fx i i))))
		      (if (>=fx noff size)
			  (loop (remainderfx noff size) (+fx i 1))
			  (loop noff (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    $string-hashtable-get ...                                        */
;*    -------------------------------------------------------------    */
;*    Same as STRING-HASHTABLE-GET but KEY is a C string.              */
;*---------------------------------------------------------------------*/
(define ($string-hashtable-get t key)
   (cond-expand
      (bigloo-c
       (let* ((size (%hashtable-max-bucket-len t))
	      (buckets (%hashtable-buckets t))
	      (len ($strlen key))
	      (hash ($string-hash key 0 len)))
	  (let loop ((off (remainderfx hash size))
		     (i 1))
	     (let ((off3 (*fx off 3)))
		(when (vector-ref buckets off3)
		   (if ($memcmp (vector-ref buckets off3) key len)
		       (when (vector-ref buckets (+fx off3 2))
			  (vector-ref buckets (+fx off3 1)))
		       (let ((noff (+fx off (*fx i i))))
			  (if (>=fx noff size)
			      (loop (remainderfx noff size) (+fx i 1))
			      (loop noff (+fx i 1))))))))))
      (else
       (string-hashtable-get t key))))

;*---------------------------------------------------------------------*/
;*    hashtable-put! ...                                               */
;*---------------------------------------------------------------------*/
(define (hashtable-put! table::struct key::obj obj::obj)
   (cond
      ((hashtable-string? table)
       (string-hashtable-put! table key obj))
      ((hashtable-weak? table)
       (weak-hashtable-put! table key obj))
      (else
       (plain-hashtable-put! table key obj))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-put/hash! ...                                   */
;*---------------------------------------------------------------------*/
(define (string-hashtable-put/hash! t key val hash)
   (let ((size (%hashtable-max-bucket-len t))
	 (buckets (%hashtable-buckets t)))
      (let loop ((off (remainderfx hash size))
		 (i 1))
	 (let ((off3 (*fx off 3)))
	    (cond
	       ((not (vector-ref buckets off3))
		;; empty bucket
		(vector-set! buckets off3 key)
		(vector-set! buckets (+fx off3 1) val)
		(vector-set! buckets (+fx off3 2) hash)
		(string-hashtable-size-inc! t))
	       ((string=? (vector-ref buckets off3) key)
		;; replace
		(vector-set! buckets (+fx off3 1) val)
		(vector-set! buckets (+fx off3 2) hash))
	       ((and (>=fx i 5)
		     (<fx (%hashtable-max-bucket-len t)
			(STRING-HASHTABLE-THRESHOLD)))
		;; too long sequence
		(string-hashtable-rehash! t)
		(string-hashtable-put/hash! t key val hash))
	       (else
		;; skip
		(let ((noff (+fx off (*fx i i))))
		   (if (>=fx noff size)
		       (loop (remainderfx noff size) (+fx i 1))
		       (loop noff (+fx i 1))))))))))
  
;*---------------------------------------------------------------------*/
;*    string-hashtable-put! ...                                        */
;*---------------------------------------------------------------------*/
(define (string-hashtable-put! table key obj)
   (string-hashtable-put/hash! table key obj
      ($string-hash key 0 (string-length key))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-put! ...                                         */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-put! table::struct key::obj obj::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table)))
      (if (null? bucket)
	  (begin
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set-ur! buckets bucket-num (list (cons key obj)))
	     obj)
	  (let loop ((buck bucket)
		     (count 0))
	     (cond
		((null? buck)
		 (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
		 (vector-set-ur! buckets bucket-num (cons (cons key obj) bucket))
		 (when (>fx count max-bucket-len)
		    (plain-hashtable-expand! table))
		 obj)
		((hashtable-equal? table (caar buck) key)
		 (let ((old-obj (cdar buck)))
		    (set-cdr! (car buck) obj)
		    old-obj))
		(else
		 (loop (cdr buck) (+fx count 1))))))))

;*---------------------------------------------------------------------*/
;*    hashtable-update! ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-update! table::struct key::obj proc::procedure obj)
   (cond
      ((hashtable-string? table)
       (string-hashtable-update! table key proc obj))
      ((hashtable-weak? table)
       (weak-hashtable-update! table key proc obj))
      (else
       (plain-hashtable-update! table key proc obj))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-update! ...                                     */
;*---------------------------------------------------------------------*/
(define (string-hashtable-update! table::struct key::bstring proc::procedure obj)
   (let* ((size (%hashtable-max-bucket-len table))
	  (buckets (%hashtable-buckets table))
	  (hash ($string-hash key 0 (string-length key))))
      ;; empty bucket
      (let loop ((off (remainderfx hash size))
		 (i 1))
	 (let ((off3 (*fx off 3)))
	    (if (vector-ref buckets off3)
		(if (string=? (vector-ref buckets off3) key)
		    (if (vector-ref buckets (+fx off3 2))
			(let ((oval (vector-ref buckets (+fx off3 1))))
			   (vector-set! buckets (+fx off3 1) (proc oval)))
			(vector-set! buckets (+fx off3 1) obj))
		    (let ((noff (+fx off (*fx i i))))
		       (if (>=fx noff size)
			   (loop (remainderfx noff size) (+fx i 1))
			   (loop noff (+fx i 1)))))
		(string-hashtable-put/hash! table key obj hash))))))
   
;*---------------------------------------------------------------------*/
;*    plain-hashtable-update! ...                                      */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-update! table::struct key::obj proc::procedure obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table)))
      (if (null? bucket)
	  (begin
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set-ur! buckets bucket-num (list (cons key obj)))
	     obj)
	  (let loop ((buck bucket)
		     (count 0))
	     (cond
		((null? buck)
		 (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
		 (vector-set-ur! buckets bucket-num (cons (cons key obj) bucket))
		 (when (>fx count max-bucket-len)
		    (plain-hashtable-expand! table))
		 obj)
		((hashtable-equal? table (caar buck) key)
		 (let ((res (proc (cdar buck))))
		    (set-cdr! (car buck) res)
		    res))
		(else
		 (loop (cdr buck) (+fx count 1))))))))
   
;*---------------------------------------------------------------------*/
;*    hashtable-add! ...                                               */
;*---------------------------------------------------------------------*/
(define (hashtable-add! table::struct key::obj update::procedure obj init)
   (cond
      ((hashtable-string? table)
       (string-hashtable-add! table key update obj init))
      ((hashtable-weak? table)
       (weak-hashtable-add! table key update obj init))
      (else
       (plain-hashtable-add! table key update obj init))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-add! ...                                        */
;*---------------------------------------------------------------------*/
(define (string-hashtable-add! table::struct key::bstring proc::procedure obj init)
   (let* ((size (%hashtable-max-bucket-len table))
	  (buckets (%hashtable-buckets table))
	  (hash ($string-hash key 0 (string-length key))))
      ;; empty bucket
      (let loop ((off (remainderfx hash size))
		 (i 1))
	 (let ((off3 (*fx off 3)))
	    (if (vector-ref buckets off3)
		(if (string=? (vector-ref buckets off3) key)
		    (if (vector-ref buckets (+fx off3 2))
			(let ((oval (vector-ref buckets (+fx off3 1))))
			   (vector-set! buckets (+fx off3 1)
			      (proc obj oval)))
			(vector-set! buckets (+fx off3 1)
			   (proc obj init)))
		    (let ((noff (+fx off (*fx i i))))
		       (if (>=fx noff size)
			   (loop (remainderfx noff size) (+fx i 1))
			   (loop noff (+fx i 1)))))
		(string-hashtable-put/hash! table key
		   (proc obj init) hash))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-add! ...                                         */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-add! table::struct key::obj proc::procedure obj init)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table)))
      (if (null? bucket)
	  (let ((v (proc obj init)))
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set-ur! buckets bucket-num (list (cons key v)))
	     v)
	  (let loop ((buck bucket)
		     (count 0))
	     (cond
		((null? buck)
		 (let ((v (proc obj init)))
		    (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
		    (vector-set-ur! buckets bucket-num (cons (cons key v) bucket))
		    (when (>fx count max-bucket-len)
		       (plain-hashtable-expand! table))
		    v))
		((hashtable-equal? table (caar buck) key)
		 (let ((res (proc obj (cdar buck))))
		    (set-cdr! (car buck) res)
		    res))
		(else
		 (loop (cdr buck) (+fx count 1))))))))
   
;*---------------------------------------------------------------------*/
;*    hashtable-remove! ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-remove! table::struct key::obj)
   (cond
      ((hashtable-string? table)
       (string-hashtable-remove! table key))
      ((hashtable-weak? table)
       (weak-hashtable-remove! table key))
      (else
       (plain-hashtable-remove! table key))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-remove! ...                                     */
;*---------------------------------------------------------------------*/
(define (string-hashtable-remove! table::struct key::bstring)
   (let* ((size (%hashtable-max-bucket-len table))
	  (buckets (%hashtable-buckets table))
	  (hash ($string-hash key 0 (string-length key))))
      ;; empty bucket
      (let loop ((off (remainderfx hash size))
		 (i 1))
	 (let ((off3 (*fx off 3)))
	    (when (vector-ref buckets off3)
	       (if (string=? (vector-ref buckets off3) key)
		   (begin
		      (vector-set! buckets (+fx off3 1) #f)
		      (vector-set! buckets (+fx off3 2) #f)
		      (string-hashtable-ntombstone-inc! table))
		   (let ((noff (+fx off (*fx i i))))
		      (if (>=fx noff size)
			  (loop (remainderfx noff size) (+fx i 1))
			  (loop noff (+fx i 1))))))))))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-remove! ...                                      */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-remove! table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num)))
      (cond
	 ((null? bucket)
	  #f)
	 ((hashtable-equal? table (caar bucket) key)
	  (vector-set-ur! buckets bucket-num (cdr bucket))
	  (%hashtable-size-set! table (-fx (%hashtable-size table) 1))
	  #t)
	 (else
	  (let loop ((bucket (cdr bucket))
		     (prev bucket))
	     (if (pair? bucket)
		 (if (hashtable-equal? table (caar bucket) key)
		     (begin
			(set-cdr! prev (cdr bucket))
			(%hashtable-size-set! table
					      (-fx (%hashtable-size table) 1))
			#t)
		     (loop (cdr bucket) bucket))
		 #f))))))
   
;*---------------------------------------------------------------------*/
;*    hashtable-expand! ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-expand! table)
   (if (hashtable-weak? table)
       (weak-hashtable-expand! table)
       (plain-hashtable-expand! table)))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-expand! ...                                      */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-expand! table)
   (let* ((old-bucks (%hashtable-buckets table))
	  (len (vector-length old-bucks))
	  (new-len (+ 1 (*fx 2 len)))
	  (max-len (%hashtable-max-length table)))
      ;; enlarge the max-bucket-len
      (let ((nmax (* (%hashtable-max-bucket-len table)
		     (%hashtable-bucket-expansion table))))
	 (%hashtable-max-bucket-len-set! table
	    (if (flonum? nmax) (flonum->fixnum nmax) nmax)))
      ;; re-construct the buckets
      (if (or (<fx max-len 0) (<=fx new-len max-len))
	  (let ((new-bucks (make-vector new-len '())))
	     (%hashtable-buckets-set! table new-bucks)
	     (let loop ((i 0))
		(when (<fx i len)
		   (for-each (lambda (cell)
				(let* ((key (car cell))
				       (n (table-get-hashnumber table key))
				       (h (remainderfx n new-len)))
				   (vector-set-ur! new-bucks
				      h (cons cell (vector-ref new-bucks h)))))
		      (vector-ref-ur old-bucks i))
		   (loop (+fx i 1)))))
	  (error "hashtable-put!"
	     (format "Hashtable too large (new-len=~a/~a, size=~a)"
		new-len max-len
		(hashtable-size table))
	     table))))

;*---------------------------------------------------------------------*/
;*    hashtable-collisions ...                                         */
;*---------------------------------------------------------------------*/
(define (hashtable-collisions table::struct)
   (if (hashtable-weak? table)
       (weak-hashtable-collisions table)
       (plain-hashtable-collisions table)))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-collisions ...                                   */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-collisions table::struct)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0)
                 (res '()))
	 (if (=fx i buckets-len)
	     res
	     (let liip ((bucket (vector-ref-ur buckets i))
			(res res)
			(coll 0))
		(if (null? bucket)
		    (loop (+fx i 1) res)
		    (liip (cdr bucket)
		       (if (> coll 0) (cons coll res) res)
		       (+fx coll 1))))))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-collisions ...                                    */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-collisions table::struct)
   '())

;*---------------------------------------------------------------------*/
;*    get-hashnumber ...                                               */
;*---------------------------------------------------------------------*/
(define (get-hashnumber::long key)
   (cond
      ((string? key)
       (absfx (string-hash-number key)))
      ((symbol? key)
       (absfx (symbol-hash-number key)))
      ((keyword? key)
       (absfx (keyword-hash-number key)))
      ((fixnum? key)
       (absfx key))
      ((elong? key)
       (absfx (elong-hash-number key)))
      ((llong? key)
       (absfx (llong-hash-number key)))
      ((object? key)
       (absfx (object-hashnumber key)))
      ((foreign? key)
       (absfx (foreign-hash-number key)))
      ((real? key)
       (get-hashnumber (flonum->fixnum key)))
      (else
       (absfx (obj-hash-number key)))))

;*---------------------------------------------------------------------*/
;*    get-hashnumber-persistent ...                                    */
;*    -------------------------------------------------------------    */
;*    A non transient hashnumber (portable and session persistent).    */
;*---------------------------------------------------------------------*/
(define (get-hashnumber-persistent::long key)
   
   (define (hash n)
      (bit-and #x7ffffff n))
   
   (define (homogeneous-vector-hashnumber::long key)
      (let ((len ($hvector-length key)))
	 (multiple-value-bind (tag _ get _ _)
	    (homogeneous-vector-info key)
	    (let loop ((i (-fx len 1))
		       (acc (hash (bit-xor 98723
				     (hash (bit-xor len (obj-hash tag)))))))
	       (if (=fx i -1)
		   acc
		   (let ((o (get key i)))
		      (loop (-fx i 1)
			 (hash (bit-xor acc (obj-hash o))))))))))
   
   (define (ucs2-string-hashnumber key)
      (let ((len (ucs2-string-length key)))
	 (let loop ((i (-fx len 1))
		    (acc (hash (bit-xor 235643 len))))
	    (if (=fx i -1)
		acc
		(loop (-fx i 1)
		   (hash
		      (bit-xor
			 (obj-hash (ucs2-string-ref key i)) acc)))))))

   (define (obj-hash key)
      (cond
	 ((cnst? key)
	  (cond
	     ((eq? key #t) 12)
	     ((eq? key #f) 445)
	     ((eq? key #unspecified) 3199)
	     ((eq? key '()) 453343)
	     (else 21354)))
	 ((string? key)
	  (hash ($string-hash-persistent key 0 (string-length key))))
	 ((symbol? key)
	  (hash (symbol-hash-number-persistent key)))
	 ((keyword? key)
	  (hash (keyword-hash-number-persistent key)))
	 ((char? key)
	  (char->integer key))
	 ((fixnum? key)
	  (hash key))
	 ((elong? key)
	  (hash (elong-hash-number key)))
	 ((llong? key)
	  (hash (llong-hash-number key)))
	 ((ucs2? key)
	  (hash (bit-xor 39434 (ucs2->integer key))))
	 ((date? key)
	  (hash (bit-xor 908 (obj-hash (date->seconds key)))))
	 ((real? key)
	  (obj-hash
	     (int64->fixnum
		(bit-ands64 (flonum->int64 (*fl key 1000.))
		   (bit-lshs64 #s64:1 29)))))
	 ((ucs2-string? key)
	  (hash (ucs2-string-hashnumber key)))
	 ((homogeneous-vector? key)
	  (homogeneous-vector-hashnumber key))
	 (else
	  (obj-hash (obj->string key)))))

   (obj-hash key))

;*---------------------------------------------------------------------*/
;*    get-pointer-hashnumber ...                                       */
;*---------------------------------------------------------------------*/
(define-inline (get-pointer-hashnumber ptr::obj power::long)
   (c-pointer-hashnumber ptr power))

;*---------------------------------------------------------------------*/
;*    string-hash ...                                                  */
;*---------------------------------------------------------------------*/
(define (string-hash string #!optional (start 0) len)
   ($string-hash string start (or len (string-length string))))

;*---------------------------------------------------------------------*/
;*    string-hash-number ...                                           */
;*---------------------------------------------------------------------*/
(define-inline (string-hash-number string)
   ($string-hash string 0 (string-length string)))

;*---------------------------------------------------------------------*/
;*    %hashtable-ntombstone ...                                        */
;*---------------------------------------------------------------------*/
(define (%hashtable-ntombstone t) (%hashtable-max-length t))
(define (%hashtable-ntombstone-set! t v) (%hashtable-max-length-set! t v))

;*---------------------------------------------------------------------*/
;*    string-hashtable-rehash! ...                                     */
;*---------------------------------------------------------------------*/
(define (string-hashtable-rehash! t)
   (let* ((osize (%hashtable-max-bucket-len t))
	  (osize3 (*fx 3 osize))
	  (obuckets (%hashtable-buckets t))
	  (nsize (+fx 1 (*fx osize 2)))
	  (nbuckets (make-vector (*fx nsize 3) #f)))
      (%hashtable-max-bucket-len-set! t nsize)
      (%hashtable-buckets-set! t nbuckets)
      (%hashtable-ntombstone-set! t 0)
      (%hashtable-size-set! t 0)
      (let loop ((i 0))
	 (unless (=fx i osize3)
	    (let ((c (vector-ref obuckets i)))
	       (when c
		  (let ((h (vector-ref obuckets (+fx i 2))))
		     (when h
			(string-hashtable-put/hash! t c
			   (vector-ref obuckets (+fx i 1))
			   h))))
	       (loop (+fx i 3)))))))

;*---------------------------------------------------------------------*/
;*    string-hashtable-ntombstone-inc! ...                             */
;*---------------------------------------------------------------------*/
(define (string-hashtable-ntombstone-inc! t)
   (%hashtable-ntombstone-set! t (+fx (%hashtable-ntombstone t) 1)))

;*---------------------------------------------------------------------*/
;*    string-hashtable-size-inc! ...                                   */
;*---------------------------------------------------------------------*/
(define (string-hashtable-size-inc! t)
   (let ((n (%hashtable-size t)))
      (if (>fx (*fx n 3) (+fx 1 (*fx 2 (%hashtable-max-bucket-len t))))
	  (string-hashtable-rehash! t)
	  (%hashtable-size-set! t (+fx n 1)))))

;*---------------------------------------------------------------------*/
;*    bootstrap temprorary functions                                   */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-contains?::bool a b)
   (string-hashtable-contains? a b))
(define (open-string-hashtable-update!::obj a b c d)
   (string-hashtable-update! a b c d))
(define (open-string-hashtable-add! a::struct b::bstring c::procedure obj init)
   (string-hashtable-add! a b c obj init))
(define (open-string-hashtable-remove! a::struct b::bstring)
   (string-hashtable-remove! a b))
(define (open-string-hashtable->vector::vector table::struct)
   (string-hashtable->vector table))
(define (open-string-hashtable->list::pair-nil table::struct)
   (string-hashtable->list table))
(define (open-string-hashtable-map::pair-nil a::struct b::procedure)
   (string-hashtable-map a b))
(define (open-string-hashtable-filter a b)
   (string-hashtable-filter a b))
(define (open-string-hashtable-filter-map a b)
   (string-hashtable-filter-map a b))
(define (open-string-hashtable-for-each a b)
   (string-hashtable-for-each a b))
(define (open-string-hashtable-filter! a b)
   (string-hashtable-filter! a b))
