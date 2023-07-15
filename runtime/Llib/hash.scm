;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/hash-bad.scm                  */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  1 08:51:06 1994                          */
;*    Last change :  Fri Jun  2 08:14:11 2023 (serrano)                */
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

   (export  (make-hashtable . args)
	    (create-hashtable #!key
	       (size 128)
	       (max-bucket-length 10)
	       (eqtest #f)
	       (hash #f)
	       (weak 'none)
	       (max-length 16384)
	       (bucket-expansion 1.2)
	       (persistent #f))
	    (get-hashnumber::long ::obj)
	    (get-hashnumber-persistent::long ::obj)
	    (inline get-pointer-hashnumber::long ::obj ::long)
	    (string-hash::long ::bstring #!optional (start 0) len)
	    (inline string-hash-number::long ::bstring)
	    (hashtable?::bool ::obj)
	    (hashtable-weak-data?::bool ::struct)
	    (hashtable-weak-keys?::bool ::struct)
	    (hashtable-size::long ::struct)
	    (hashtable-contains?::bool ::struct ::obj)
	    (hashtable-get::obj ::struct ::obj)
	    (string-hashtable-get::obj ::struct ::bstring)
	    (open-string-hashtable-get::obj ::struct ::bstring)
	    ($open-string-hashtable-get::obj ::struct ::string)
	    (hashtable-put! ::struct ::obj ::obj)
	    (string-hashtable-put!::obj ::struct ::bstring ::obj)
	    (open-string-hashtable-put!::obj ::struct ::bstring ::obj)
	    (hashtable-update! ::struct ::obj ::procedure ::obj)
	    (hashtable-add! ::struct ::obj ::procedure ::obj ::obj)
	    (hashtable-remove!::bool ::struct ::obj)
	    (hashtable->vector::vector ::struct)
	    (hashtable->list::pair-nil ::struct)
	    (hashtable-key-list::pair-nil ::struct)
	    (hashtable-map ::struct ::procedure)
	    (hashtable-for-each ::struct ::procedure)
	    (hashtable-filter! ::struct ::procedure)
	    (hashtable-clear! ::struct)
            (hashtable-collisions::pair-nil ::struct)
	    (open-string-hashtable-contains?::obj ::struct ::bstring)
	    (open-string-hashtable-update!::obj ::struct ::bstring ::procedure ::obj)
	    (open-string-hashtable-add! ::struct ::bstring ::procedure obj init)
	    (open-string-hashtable-remove! ::struct ::bstring)
	    (open-string-hashtable-map ::struct ::procedure)
	    (open-string-hashtable-for-each ::struct ::procedure)
	    (open-string-hashtable-filter! ::struct ::procedure)
	    )

   (pragma  (hashtable-contains? side-effect-free)
	    (hashtable-get side-effect-free)))
   
;*---------------------------------------------------------------------*/
;*    Default hashtable configuration                                  */
;*---------------------------------------------------------------------*/
(define default-hashtable-bucket-length 128)
(define default-max-bucket-length 10)

(define (OPEN-STRING-HASHTABLE-THRESHOLD)
   (*fx 8 (*fx 1024 1024)))
   
;*---------------------------------------------------------------------*/
;*    make-hashtable ...                                               */
;*---------------------------------------------------------------------*/
(define (make-hashtable . args)
   (let* ((size (if (pair? args)
		    (let ((size (car args)))
		       (set! args (cdr args))
		       (cond
			  ((and (fixnum? size) (>=fx size 1))
			   size)
			  ((eq? size #unspecified)
			   default-hashtable-bucket-length)
			  (else
			   (error "make-hashtable"
			      "Illegal default bucket length"
			      size))))
		    default-hashtable-bucket-length))
	  (mblen (if (pair? args)
		     (let ((mblen (car args)))
			(set! args (cdr args))
			(cond
			   ((and (fixnum? mblen) (>=fx mblen 1))
			    mblen)
			   ((eq? mblen #unspecified)
			    default-max-bucket-length)
			   (else
			    (error "make-hashtable"
			       "Illegal max bucket length"
			       mblen))))
		     default-max-bucket-length))
	  (eqtest (if (pair? args)
		      (let ((eqtest (car args)))
			 (set! args (cdr args))
			 (cond
			    ((and (procedure? eqtest) (correct-arity? eqtest 2))
			     eqtest)
			    ((eq? eqtest #unspecified)
			     #f)
			    (else
			     (error "make-hashtable"
				"Illegal equality test"
				eqtest))))
		      #f))
	  (hashn (if (pair? args)
		     (let ((hn (car args)))
			(set! args (cdr args))
			(cond
			   ((and (procedure? hn) (correct-arity? hn 1))
			    hn)
			   ((eq? hn #unspecified)
			    #f)
			   (else
			    (error "make-hashtable"
			       "Illegal hashnumber function"
			       hn))))
		     #f))
	  (wkk (if (pair? args)
		   (let ((wkk (car args)))
		      (set! args (cdr args))
		      (if (and (not (eq? wkk #unspecified)) wkk)
			  (weak-keys)
			  (weak-none)))
		   (weak-none)))
	  (wkd (if (pair? args)
		   (let ((wkd (car args)))
		      (if (and (not (eq? wkd #unspecified)) wkd)
			  (weak-data)
			  (weak-none)))
		   (weak-none)))
	  (wk (bit-or wkk wkd)))
      (if (eq? wk (weak-open-string))
	  (%hashtable 0 size (make-vector (*fx 3 size) #f) eq? list wk 0 0)
	  (%hashtable 0 mblen (make-vector size '()) eqtest hashn wk -1 1.2))))

;*---------------------------------------------------------------------*/
;*    create-hashtable ...                                             */
;*---------------------------------------------------------------------*/
(define (create-hashtable #!key
	   (size 128)
	   (max-bucket-length 10)
	   (eqtest #f)
	   (hash #f)
	   (weak 'none)
	   (max-length 16384)
	   (bucket-expansion 1.2)
	   (persistent #f))
   (let ((weak (case weak
		  ((keys) (weak-keys))
		  ((data) (weak-data))
		  ((both) (weak-both))
		  ((none) (weak-none))
		  ((open-string) (weak-open-string))
		  ((string) (weak-string))
		  (else (if weak (weak-data) (weak-none))))))
      (when persistent
	 (if hash
	    (error "create-hashtable"
	       "Persistent hashtable cannot use custom hash function"
	       hash)
	    (set! hash 'persistent)))
      (if (or (eq? weak (weak-open-string)) (eq? weak (weak-string)))
	  (cond
	     (eqtest
	      (error "create-hashtable"
		 "Cannot provide eqtest for string hashtable" eqtest))
	     (hash
	      (error "create-hashtable"
		 "Cannot provide hash for string hashtable" hash))
	     ((eq? weak (weak-open-string))
	      (%hashtable 0 size (make-vector (*fx 3 size) #f) eq? list weak 0 0))
	     (else
	      (%hashtable 0 max-bucket-length (make-vector size '())
		 string=?
		 (lambda (s) ($string-hash s 0 (string-length s)))
		 weak max-length bucket-expansion)))
	  (%hashtable 0 max-bucket-length (make-vector size '())
	     eqtest hash
	     weak max-length bucket-expansion))))

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
;*    hashtable-open-string? ...                                       */
;*---------------------------------------------------------------------*/
(define (hashtable-open-string?::bool table::struct)
   (not (=fx 0 (bit-and (%hashtable-weak table) (weak-open-string)))))

;*---------------------------------------------------------------------*/
;*    hashtable-string? ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-string?::bool table::struct)
   (=fx (%hashtable-weak table) (weak-string)))

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
(define (hashtable->vector table::struct)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable->vector table))
      ((hashtable-weak? table)
       (weak-hashtable->vector table))
      (else
       (plain-hashtable->vector table))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable->vector ...                                */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable->vector table::struct)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table))
	  (vec (make-vector size)))
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
(define (hashtable->list table::struct)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable->list table))
      ((hashtable-weak? table)
       (weak-hashtable->list table))
      (else
       (plain-hashtable->list table))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable->list ...                                  */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable->list table::struct)
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
(define (hashtable-key-list table::struct)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable-key-list table))
      ((hashtable-weak? table)
       (weak-hashtable-key-list table))
      (else
       (plain-hashtable-key-list table))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-key-list ...                               */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-key-list table::struct)
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
(define (hashtable-map table::struct fun::procedure)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable->list table))
      ((hashtable-weak? table)
       (weak-hashtable-map table fun))
      (else
       (plain-hashtable-map table fun))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-map ...                                    */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-map table::struct fun)
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
      ((hashtable-open-string? table)
       (open-string-hashtable-for-each table fun))
      ((hashtable-weak? table)
       (weak-hashtable-for-each table fun))
      (else
       (plain-hashtable-for-each table fun))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-for-each ...                               */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-for-each table::struct fun)
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
;*    hashtable-filter! ...                                            */
;*---------------------------------------------------------------------*/
(define (hashtable-filter! table::struct fun::procedure)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable-filter! table fun))
      ((hashtable-weak? table)
       (weak-hashtable-filter! table fun))
      (else
       (plain-hashtable-filter! table fun))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-filter! ...                                */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-filter! table::struct fun)
   (let* ((size (%hashtable-max-bucket-len table))
	  (size3 (*fx 3 size))
	  (buckets (%hashtable-buckets table)))
      (let loop ((i 0))
	 (unless (=fx i size3)
	    (when (and (vector-ref buckets i) (vector-ref buckets (+fx i 2)))
	       (unless (fun (vector-ref buckets i) (vector-ref buckets (+fx i 1)))
		  (vector-set! buckets (+fx i 1) #f)
		  (vector-set! buckets (+fx i 2) #f)
		  (open-string-hashtable-ntombstone-inc! table)))
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
      ((hashtable-open-string? table)
       (open-string-hashtable-clear! table))
      ((hashtable-weak? table)
       (weak-hashtable-clear! table))
      (else
       (plain-hashtable-clear! table))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-clear! ...                                 */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-clear! table)
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
(define (hashtable-contains? table::struct key::obj)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable-contains? table key))
      ((hashtable-weak? table)
       (weak-hashtable-contains? table key))
      (else
       (plain-hashtable-contains? table key))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-contains? ...                              */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-contains? t key)
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
		      (vector-ref buckets (+fx off3 1)))
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
      ((hashtable-open-string? table) (open-string-hashtable-get table key))
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
(define (string-hashtable-get table::struct key::bstring)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx ($string-hash key 0 (string-length key)) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num)))
      (let loop ((bucket bucket))
	 (cond
	    ((null? bucket)
	     #f)
	    ((string=? (caar bucket) key)
	     (cdar bucket))
	    (else
	     (loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-get ...                                    */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-get t key)
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
;*    $open-string-hashtable-get ...                                   */
;*    -------------------------------------------------------------    */
;*    Same as OPEN-STRING-HASHTABLE-GET but KEY is a C string.         */
;*---------------------------------------------------------------------*/
(define ($open-string-hashtable-get t key)
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
       (open-string-hashtable-get t key))))

;*---------------------------------------------------------------------*/
;*    hashtable-put! ...                                               */
;*---------------------------------------------------------------------*/
(define (hashtable-put! table::struct key::obj obj::obj)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable-put! table key obj))
      ((hashtable-weak? table)
       (weak-hashtable-put! table key obj))
      (else
       (plain-hashtable-put! table key obj))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-put! ...                                   */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-put! table key obj)
   (open-string-hashtable-put/hash! table key obj
      ($string-hash key 0 (string-length key))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-put/hash! ...                              */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-put/hash! t key val hash)
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
		(open-string-hashtable-size-inc! t))
	       ((string=? (vector-ref buckets off3) key)
		;; replace
		(vector-set! buckets (+fx off3 1) val)
		(vector-set! buckets (+fx off3 2) hash))
	       ((and (>=fx i 5)
		     (<fx (%hashtable-max-bucket-len t)
			(OPEN-STRING-HASHTABLE-THRESHOLD)))
		;; too long sequence
		(open-string-hashtable-rehash! t)
		(open-string-hashtable-put/hash! t key val hash))
	       (else
		;; skip
		(let ((noff (+fx off (*fx i i))))
		   (if (>=fx noff size)
		       (loop (remainderfx noff size) (+fx i 1))
		       (loop noff (+fx i 1))))))))))
  
;*---------------------------------------------------------------------*/
;*    string-hashtable-put! ...                                        */
;*---------------------------------------------------------------------*/
(define (string-hashtable-put! table::struct key obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx ($string-hash key 0 (string-length key)) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table)))
      (cond-expand
	 (bigloo-unsafe-type
	  #f)
	 (else
	  (when (and (hashtable-string? table) (not (string? key)))
	     (bigloo-type-error "hashtable-put!" "bstring" key))))
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
		((string=? (caar buck) key)
		 (let ((old-obj (cdar buck)))
		    (set-cdr! (car buck) obj)
		    old-obj))
		(else
		 (loop (cdr buck) (+fx count 1))))))))

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
      ((hashtable-open-string? table)
       (open-string-hashtable-update! table key proc obj))
      ((hashtable-weak? table)
       (weak-hashtable-update! table key proc obj))
      (else
       (plain-hashtable-update! table key proc obj))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-update! ...                                */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-update! table::struct key::bstring proc::procedure obj)
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
		(open-string-hashtable-put/hash! table key obj hash))))))
   
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
(define (hashtable-add! table::struct key::obj p2::procedure obj init)
   (cond
      ((hashtable-open-string? table)
       (open-string-hashtable-add! table key p2 obj init))
      ((hashtable-weak? table)
       (weak-hashtable-add! table key p2 obj init))
      (else
       (plain-hashtable-add! table key p2 obj init))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-add! ...                                   */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-add! table::struct key::bstring proc::procedure obj init)
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
			      (proc oval init)))
			(vector-set! buckets (+fx off3 1)
			   (proc obj init)))
		    (let ((noff (+fx off (*fx i i))))
		       (if (>=fx noff size)
			   (loop (remainderfx noff size) (+fx i 1))
			   (loop noff (+fx i 1)))))
		(open-string-hashtable-put/hash! table key
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
      ((hashtable-open-string? table)
       (open-string-hashtable-remove! table key))
      ((hashtable-weak? table)
       (weak-hashtable-remove! table key))
      (else
       (plain-hashtable-remove! table key))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-remove! ...                                */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-remove! t key)
   (let* ((size (%hashtable-max-bucket-len t))
	  (buckets (%hashtable-buckets t))
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
		      (open-string-hashtable-ntombstone-inc! t))
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
	  (new-len (*fx 2 len))
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
;*    open-string-hashtable-rehash! ...                                */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-rehash! t)
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
			(open-string-hashtable-put/hash! t c
			   (vector-ref obuckets (+fx i 1))
			   h))))
	       (loop (+fx i 3)))))))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-ntombstone-inc! ...                        */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-ntombstone-inc! t)
   (%hashtable-ntombstone-set! t (+fx (%hashtable-ntombstone t) 1)))

;*---------------------------------------------------------------------*/
;*    open-string-hashtable-size-inc! ...                              */
;*---------------------------------------------------------------------*/
(define (open-string-hashtable-size-inc! t)
   (let ((n (%hashtable-size t)))
      (if (>fx (*fx n 3) (*fx 2 (%hashtable-max-bucket-len t)))
	  (open-string-hashtable-rehash! t)
	  (%hashtable-size-set! t (+fx n 1)))))
