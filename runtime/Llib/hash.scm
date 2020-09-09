;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/hash.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  1 08:51:06 1994                          */
;*    Last change :  Thu Feb 28 07:36:01 2019 (serrano)                */
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
   
   (extern  (string-hash-number::long (::string) "bgl_string_hash_number")
	    ($string-hash::long (::string ::int ::int) "bgl_string_hash")
	    (symbol-hash-number::long (::symbol) "bgl_symbol_hash_number")
	    (keyword-hash-number::long (::keyword) "bgl_keyword_hash_number")
	    (obj-hash-number::long (::obj) "bgl_obj_hash_number")
	    (c-pointer-hashnumber::long (::obj ::long) "bgl_pointer_hashnumber")
	    (foreign-hash-number::long (::foreign) "bgl_foreign_hash_number")
	    (macro elong-hash-number::long (::elong) "(long)")
	    (macro llong-hash-number::long (::llong) "(long)"))
   
   (java    (class foreign
	       (method static string-hash-number::long (::string)
		       "bgl_string_hash_number")
	       (method static $string-hash::long (::string ::int ::int)
		       "bgl_string_hash")
	       (method static symbol-hash-number::long (::symbol)
		       "bgl_symbol_hash_number")
	       (method static keyword-hash-number::long (::keyword)
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
	       (bucket-expansion 1.2))
	    (get-hashnumber::long ::obj)
	    (get-hashnumber-persistent::long ::obj)
	    (inline get-pointer-hashnumber::long ::obj ::long)
	    (string-hash::long ::bstring #!optional (start 0) len)
	    (hashtable?::bool ::obj)
	    (hashtable-weak-data?::bool ::struct)
	    (hashtable-weak-keys?::bool ::struct)
	    (hashtable-size::long ::struct)
	    (hashtable-contains?::bool ::struct ::obj)
	    (hashtable-get::obj ::struct ::obj)
	    (hashtable-put! ::struct ::obj ::obj)
	    (hashtable-update! ::struct ::obj ::procedure ::obj)
	    (hashtable-add! ::struct ::obj ::procedure ::obj ::obj)
	    (hashtable-remove!::bool ::struct ::obj)
	    (hashtable->vector::vector ::struct)
	    (hashtable->list::pair-nil ::struct)
	    (hashtable-key-list::pair-nil ::struct)
	    (hashtable-map ::struct ::procedure)
	    (hashtable-for-each ::struct ::procedure)
	    (hashtable-filter! ::struct ::procedure)
	    (hashtable-clear! ::struct))

   (pragma  (hashtable-contains? side-effect-free)
	    (hashtable-get side-effect-free)))
   
;*---------------------------------------------------------------------*/
;*    Default hashtable configuration                                  */
;*---------------------------------------------------------------------*/
(define default-hashtable-bucket-length 128)
(define default-max-bucket-length 10)

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
      (%hashtable 0 mblen (make-vector size '()) eqtest hashn wk -1 1)))

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
	   (bucket-expansion 1.2))
   (let ((weak (case weak
		  ((keys) (weak-keys))
		  ((data) (weak-data))
		  ((both) (weak-both))
		  ((none) (weak-none))
		  ((string) (weak-string))
		  (else (if weak (weak-data) (weak-none))))))
      (if (eq? weak (weak-string))
	  (cond
	     (eqtest
	      (error "create-hashtable"
		 "Cannot provide eqtest for string hashtable" eqtest))
	     (hash
	      (error "create-hashtable"
		 "Cannot provide hash for string hashtable" hash))
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
   (not (=fx 0 (bit-and (%hashtable-weak table) 3))))

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
   (if (hashtable-weak? table)
       (weak-hashtable->vector table)
       (plain-hashtable->vector table)))

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
   (if (hashtable-weak? table)
       (weak-hashtable->list table)
       (plain-hashtable->list table)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-key-list table)
       (plain-hashtable-key-list table)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-map table fun)
       (plain-hashtable-map table fun)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-for-each table fun)
       (plain-hashtable-for-each table fun)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-filter! table fun)
       (plain-hashtable-filter! table fun)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-clear! table)
       (plain-hashtable-clear! table)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-contains? table key)
       (plain-hashtable-contains? table key)))

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
      ((hashtable-weak? table) (weak-hashtable-get table key))
      ((hashtable-string? table) (string-hashtable-get table key))
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
(define (string-hashtable-get table::struct key::obj)
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
;*    hashtable-put! ...                                               */
;*---------------------------------------------------------------------*/
(define (hashtable-put! table::struct key::obj obj::obj)
   (if (hashtable-weak? table)
       (weak-hashtable-put! table key obj)
       (plain-hashtable-put! table key obj)))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-put! ...                                         */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-put! table::struct key::obj obj::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
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
   (if (hashtable-weak? table)
       (weak-hashtable-update! table key proc obj)
       (plain-hashtable-update! table key proc obj)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-add! table key p2 obj init)
       (plain-hashtable-add! table key p2 obj init)))

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
   (if (hashtable-weak? table)
       (weak-hashtable-remove! table key)
       (plain-hashtable-remove! table key)))

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
		     (loop (cdr bucket)
			   bucket))
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
	  (hash (string-hash-number key)))
	 ((symbol? key)
	  (hash (symbol-hash-number key)))
	 ((keyword? key)
	  (hash (keyword-hash-number key)))
	 ((char? key)
	  (hash (char->integer key)))
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
	  (obj-hash (flonum->fixnum (*fl key 1000.))))
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
