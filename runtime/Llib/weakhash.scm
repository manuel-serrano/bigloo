;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/weakhash.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  1 08:51:06 1994                          */
;*    Last change :  Mon Feb 25 14:08:36 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The weak hash tables.                                            */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Hash Tables@                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __weakhash

   (import  __error
	    __r4_symbols_6_4
	    __param
	    __hash
	    __bexit
	    __object
	    __thread)

   (use     __type
	    __bigloo
	    __structure
	    __bit
	    __tvector
            __weakptr
	    __bignum
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
	    __foreign
	    __evenv

	    __r4_output_6_10_3
	    __r4_ports_6_10_1)

   (include "Llib/hash.sch")

   (export (weak-hashtable->vector::vector ::struct)
	   (weak-hashtable->list::pair-nil ::struct)
	   (weak-hashtable-key-list::pair-nil ::struct)
	   (weak-hashtable-map::pair-nil ::struct ::procedure)
	   (weak-hashtable-for-each ::struct ::procedure)
	   (weak-hashtable-filter! ::struct ::procedure)
	   (weak-hashtable-clear! ::struct)
	   (weak-hashtable-contains?::bool ::struct ::obj)
	   (weak-hashtable-get ::struct ::obj)
	   (weak-hashtable-put! ::struct ::obj ::obj)
	   (weak-hashtable-update! ::struct ::obj ::procedure obj)
	   (weak-hashtable-add! ::struct ::obj ::procedure obj obj)
	   (weak-hashtable-remove! ::struct ::obj)
	   (weak-hashtable-expand! ::struct)))
   
;*---------------------------------------------------------------------*/
;*    Some constants                                                   */
;*---------------------------------------------------------------------*/
(define keepgoing (cons #unspecified #unspecified))
(define remove (cons #unspecified #unspecified))
(define removestop (cons #unspecified #unspecified))

;*---------------------------------------------------------------------*/
;*    the fork macro ...                                               */
;*---------------------------------------------------------------------*/
(define-macro (fork . code)
   ;; collects the cases
   (define (collect-cases expr cases)
      (cond ((not (pair? expr)) cases)
	    ((eq? (car expr) 'unfork-cond)
	     ;; walk all the tests
	     (let loop ((tests (cdr expr))
			(cases cases))
		(cond ((null? tests) cases)
		      ;; the car is a pair (test expr...)
		      ((member (caar tests) cases) (loop (cdr tests) cases))
		      (else (loop (cdr tests) (cons (caar tests) cases))))))
	    (else
	     (collect-cases (cdr expr) (collect-cases (car expr) cases)))))
   ; and expands only the given case
   (define (expand-case expr case)
      (cond ((not (pair? expr)) expr)
	    ((eq? (car expr) 'unfork-cond)
	     ;; write only the case
	     (let ((c (assoc case (cdr expr))))
		(if c
		    (if (pair? (cdr c))
			(cons 'begin (cdr c))
			(cdr c))
		    #unspecified)))
	    (else
	     (cons (expand-case (car expr) case)
		   (expand-case (cdr expr) case)))))
   (let ((cases (reverse (collect-cases code '()))))
      ;; panic if there are no cases
      (if (= 0 (length cases))
	  (error 'fork "Missing unfork-cond in fork" '()))
      ;; keep them ordered except the else
      (if (member 'else cases)
	  (set! cases (append (delete 'else cases) (list 'else))))
      ;; now make a cond
      (cons 'cond
	    (map (lambda (case)
		    (cons case (expand-case code case)))
		 cases))))

;*---------------------------------------------------------------------*/
;*    remove-bucket ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (remove-bucket bucket last-bucket table buckets i)
   (%hashtable-size-set! table (-fx (%hashtable-size table) 1))
   (if last-bucket
       (set-cdr! last-bucket (cdr bucket))
       (vector-set! buckets i (cdr bucket)))
   ;; very important for traverse-buckets to return keepgoing
   keepgoing)

;*---------------------------------------------------------------------*/
;*    traverse-buckets ... ...                                         */
;*---------------------------------------------------------------------*/
(define (traverse-buckets table buckets i fun)
   (fork
    (let liip ((bucket (vector-ref buckets i))
	       (last-bucket #f))
       (if (null? bucket)
	   keepgoing
	   (let ((ret (unfork-cond
		       ((=fx (weak-keys) (%hashtable-weak table))
			l; only weak keys
			(let ((key (weakptr-data (caar bucket))))
			   (if (eq? key #unspecified)
			       remove
			       (fun key (cdar bucket) bucket))))
		       ((=fx (weak-data) (%hashtable-weak table))
			;l only weak data
			(let ((data (weakptr-data (cdar bucket))))
			   (if (eq? data #unspecified)
			       remove
			       (fun (caar bucket) data bucket))))
		       ((=fx (weak-both) (%hashtable-weak table))
			;l weak keys and data
			(let ((key (weakptr-data (caar bucket)))
			      (data (weakptr-data (cdar bucket))))
			   (if (or (eq? key #unspecified)
				   (eq? data #unspecified))
			       remove
			       (fun key data bucket))))
		       (else
			;l all strong
			(fun (caar bucket) (cdar bucket) bucket)))))
	      (cond ((eq? ret keepgoing)
		     (liip (cdr bucket) bucket))
		    ((eq? ret remove)
		     (remove-bucket bucket last-bucket table buckets i)
		     (liip (cdr bucket) last-bucket))
		    ((eq? ret removestop)
		     (remove-bucket bucket last-bucket table buckets i))
		    (else ret)))))))

;*---------------------------------------------------------------------*/
;*    traverse-hash ...                                                */
;*---------------------------------------------------------------------*/
(define (traverse-hash table::struct fun)
   (fork
    (let* ((buckets (%hashtable-buckets table))
	   (buckets-len (vector-length buckets)))
       (let loop ((i 0))
	  (unless (=fx i buckets-len)
	     (let liip ((bucket (vector-ref buckets i))
			(last-bucket #f))
		(unless (null? bucket)
		   (let ((ret (unfork-cond
			       ((=fx (weak-keys) (%hashtable-weak table))
				;; only weak keys
				(let ((key (weakptr-data (caar bucket))))
				   (if (eq? key #unspecified)
				       remove
				       (fun key (cdar bucket)))))
			       ((=fx (weak-data) (%hashtable-weak table))
				;; only weak data
				(let ((data (weakptr-data (cdar bucket))))
				   (if (eq? data #unspecified)
				       remove
				       (fun (caar bucket) data))))
			       ((=fx (weak-both) (%hashtable-weak table))
				;; weak keys and data
				(let ((key (weakptr-data (caar bucket)))
				      (data (weakptr-data (cdar bucket))))
				   (if (or (eq? key #unspecified)
					   (eq? data #unspecified))
				       remove
				       (fun key data))))
			       (else
				;; all strong
				(fun (caar bucket) (cdar bucket))))))
		      ;; here we only generate remove ourselves
		      (if (eq? ret remove)
			  (begin
			     (remove-bucket bucket last-bucket table buckets i)
			     (liip (cdr bucket) last-bucket))
			  (liip (cdr bucket) bucket)))))
	     (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable->vector ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable->vector table::struct)
   (let ((vec (make-vector (hashtable-size table)))
	 (w 0))
      (traverse-hash table
		     (lambda (key val)
			(vector-set! vec w val)
			(set! w (+fx w 1))))
      (if (<fx w (hashtable-size table))
	  (copy-vector vec w)
	  vec)))

;*---------------------------------------------------------------------*/
;*    weak-hashtable->list ...                                         */
;*---------------------------------------------------------------------*/
(define (weak-hashtable->list table::struct)
   (let ((res '()))
      (traverse-hash table
		     (lambda (key val)
			(set! res (cons val res))))
      res))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-key-list ...                                      */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-key-list table::struct)
   (let ((res '()))
      (traverse-hash table
		     (lambda (key val)
			(set! res (cons key res))))
      res))

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
	     (let liip ((bucket (vector-ref buckets i))
			(res res))
		(if (null? bucket)
		    (loop (+fx i 1) res)
		    (liip (cdr bucket) (cons (caar bucket) res))))))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-map ...                                           */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-map table::struct fun::procedure)
   (let ((res '()))
      (traverse-hash table
		     (lambda (key val)
			(set! res (cons (fun key val) res))))
      res))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-for-each ...                                      */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-for-each table::struct fun::procedure)
   (traverse-hash table fun))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-filter! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-filter! table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0))
	 (if (<fx i buckets-len)
	     (begin
		(traverse-buckets table buckets i
				  (lambda (key val bucket)
				     (if (fun key val)
					 keepgoing
					 remove)))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-clear! ...                                        */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-clear! table::struct)
   (weak-hashtable-filter! table (lambda (k v) #f)))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-filter! ...                                      */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-filter! table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0) (delta 0))
	 (if (<fx i buckets-len)
	     (let* ((l (vector-ref buckets i))
                    (old-len (length l))
                    (newl (filter! (lambda (cell)
                                    (fun (car cell) (cdr cell)))
                                   l))
                    (new-len (length newl)))
		(vector-set! buckets i newl)
		(loop (+fx i 1) (+fx delta (-fx new-len old-len))))
             (%hashtable-size-set! table
                                   (+fx delta (%hashtable-size table)))))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-contains? ...                                     */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-contains? table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (ret (traverse-buckets table buckets bucket-num
				 (lambda (bkey val bucket)
				    (if (hashtable-equal? table key bkey)
					#t
					keepgoing)))))
      (not (eq? ret keepgoing))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-get ...                                           */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-get table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (ret (traverse-buckets table buckets bucket-num
				 (lambda (bkey val bucket)
				    (if (hashtable-equal? table key bkey)
					val
					keepgoing)))))
      (if (eq? ret keepgoing)
	  #f
	  ret)))

;*---------------------------------------------------------------------*/
;*    plain-hashtable-get ...                                          */
;*---------------------------------------------------------------------*/
(define (plain-hashtable-get table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num)))
      (let loop ((bucket bucket))
	 (cond
	    ((null? bucket)
	     #f)
	    ((hashtable-equal? table (caar bucket) key)
	     (cdar bucket))
	    (else
	     (loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-put! ...                                          */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-put! table::struct key::obj obj::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table))
          (count 0)
          ;; try to find it first
          (found (traverse-buckets
                  table buckets bucket-num
                  (lambda (bkey val bucket)
		     (set! count (+fx count 1))
		     (if (hashtable-equal? table bkey key)
			 (begin
			    (set-cdr! (car bucket)
				      (if (hashtable-weak-data? table)
					  (make-weakptr obj)
					  obj))
			    ;; return the old value
			    val)
			 keepgoing)))))
      ;; if found we're good
      (if (not (eq? found keepgoing))
	  found
	  (begin
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set! buckets bucket-num
			  (cons (cons (if (hashtable-weak-keys? table)
					  (make-weakptr key)
					  key)
				      (if (hashtable-weak-data? table)
					  (make-weakptr obj)
					  obj))
				;; we need to retake the bucket in case
				;; it changed while walking it
				(vector-ref (%hashtable-buckets table)
					    bucket-num)))
	     (if (>fx count max-bucket-len)
		 (weak-hashtable-expand! table))
	     obj))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-update! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-update! table::struct key::obj proc::procedure obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table))
          (count 0)
          ;; try to find it first
          (found (traverse-buckets
                  table buckets bucket-num
                  (lambda (bkey val bucket)
		     (set! count (+fx count 1))
		     (if (hashtable-equal? table bkey key)
			 (let ((newval (proc val)))
			    (set-cdr! (car bucket)
				      (if (hashtable-weak-data? table)
					  (make-weakptr newval)
					  newval))
			    ;; return the old value
			    newval)
			 keepgoing)))))
      ;; if found we're good
      (if (not (eq? found keepgoing))
	  found
	  (begin
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set! buckets bucket-num
			  (cons (cons (if (hashtable-weak-keys? table)
					  (make-weakptr key)
					  key)
				      (if (hashtable-weak-data? table)
					  (make-weakptr obj)
					  obj))
				;; we need to retake the bucket in case
				;; it changed while walking it
				(vector-ref (%hashtable-buckets table)
					    bucket-num)))
	     (if (>fx count max-bucket-len)
		 (weak-hashtable-expand! table))
	     obj))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-add! table::struct key::obj proc::procedure obj init)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table))
          (count 0)
          ;; try to find it first
          (found (traverse-buckets
                  table buckets bucket-num
                  (lambda (bkey val bucket)
		     (set! count (+fx count 1))
		     (if (hashtable-equal? table bkey key)
			 (let ((newval (proc val)))
			    (set-cdr! (car bucket)
				      (if (hashtable-weak-data? table)
					  (make-weakptr newval)
					  newval))
			    ;; return the old value
			    newval)
			 keepgoing)))))
      ;; if found we're good
      (if (not (eq? found keepgoing))
	  found
	  (let ((v (if (hashtable-weak-data? table)
		       (proc (make-weakptr obj) (make-weakptr init))
		       (proc obj init))))
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set! buckets bucket-num
			  (cons (cons (if (hashtable-weak-keys? table)
					  (make-weakptr key)
					  key)
				      v)
				;; we need to retake the bucket in case
				;; it changed while walking it
				(vector-ref (%hashtable-buckets table)
					    bucket-num)))
	     (if (>fx count max-bucket-len)
		 (weak-hashtable-expand! table))
	     v))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-remove! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-remove! table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num))
          (found  (traverse-buckets
                   table buckets bucket-num
                   (lambda (bkey val bucket)
		      (if (hashtable-equal? table key bkey)
			  removestop
			  keepgoing)))))
      (not (eq? found keepgoing))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-expand! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-expand! table)
   (fork
    (let* ((old-bucks (%hashtable-buckets table))
	   (old-bucks-len (vector-length old-bucks))
	   (new-bucks-len (*fx 2 old-bucks-len))
	   (new-bucks (make-vector new-bucks-len '()))
	   (count (%hashtable-size table)))
       ;; enlarge the max-bucket-len
      (let ((nmax (* (%hashtable-max-bucket-len table)
		     (%hashtable-bucket-expansion table))))
	 (%hashtable-max-bucket-len-set! table
	    (if (flonum? nmax) (flonum->fixnum nmax) nmax)))
       (%hashtable-buckets-set! table new-bucks)
       (let loop ((i 0))
	  (when (<fx i old-bucks-len)
	     (for-each (lambda (cell)
			  (unfork-cond
			   ((=fx (weak-keys) (%hashtable-weak table))
			    ;; only weak keys
			    (let ((key (weakptr-data (car cell))))
			       (if (eq? key #unspecified)
				   (set! count (-fx count 1))
				   (let ((h (remainderfx
					     (table-get-hashnumber table key)
					     new-bucks-len)))
				      (vector-set! new-bucks
						   h
						   (cons cell
							 (vector-ref new-bucks
								     h)))))))
			   ((=fx (weak-data) (%hashtable-weak table))
			    ; only weak data
			    (let ((data (weakptr-data (cdr cell))))
			       (if (eq? data #unspecified)
				   (set! count (-fx count 1))
				   (let ((h (remainderfx
					     (table-get-hashnumber table
								   (car cell))
					     new-bucks-len)))
				      (vector-set! new-bucks
						   h
						   (cons cell
							 (vector-ref new-bucks
								     h)))))))
			   ((=fx (weak-both) (%hashtable-weak table))
			    ; weak keys and data
			    (let ((key (weakptr-data (car cell)))
				  (data (weakptr-data (cdr cell))))
			       (if (or (eq? key #unspecified)
				       (eq? data #unspecified))
				   (set! count (-fx count 1))
				   (let ((h (remainderfx
					     (table-get-hashnumber table key)
					     new-bucks-len)))
				      (vector-set! new-bucks
						   h
						   (cons cell
							 (vector-ref new-bucks
								     h)))))))
			   (else
			    ; all strong
			    (let ((h (remainderfx
				      (table-get-hashnumber table
							    (car cell))
				      new-bucks-len)))
			       (vector-set!
				new-bucks
				h
				(cons cell
				      (vector-ref new-bucks h)))))))
		       (vector-ref old-bucks i))
	     (loop (+fx i 1))))
       (%hashtable-size-set! table count))))

