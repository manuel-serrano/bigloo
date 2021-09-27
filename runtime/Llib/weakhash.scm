;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/weakhash.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Sep  1 08:51:06 1994                          */
;*    Last change :  Mon Sep 27 08:03:36 2021 (serrano)                */
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

;* (define-macro (make-weakptr a . d) `(vector ,a ,(if (pair? d) (car d) ''()))) */
;* (define-macro (weakptr-data p) `(vector-ref ,p 0))                  */
;* (define-macro (weakptr-data-set! p v) `(vector-set! ,p 0 ,v))       */
;* (define-macro (weakptr-ref p) `(vector-ref ,p 1))                   */
;* (define-macro (weakptr-ref-set! p v) `(vector-set! ,p 1 ,v))        */

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
      (cond
	 ((not (pair? expr)) cases)
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
      (when (=fx 0 (length cases))
	 (error "weakhash-fork" "Missing unfork-cond in fork" '()))
      ;; keep them ordered except the else
      (when (member 'else cases)
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
			;; only weak keys
			(let ((key (weakptr-data (caar bucket))))
			   (if (eq? key #unspecified)
			       remove
			       (fun key (cdar bucket) bucket))))
		       ((=fx (weak-data) (%hashtable-weak table))
			;; only weak data
			(let ((data (weakptr-data (cdar bucket))))
			   (if (eq? data #unspecified)
			       remove
			       (fun (caar bucket) data bucket))))
		       ((=fx (weak-both) (%hashtable-weak table))
			;; weak keys and data
			(let ((key (weakptr-data (caar bucket)))
			      (data (weakptr-data (cdar bucket))))
			   (if (or (eq? key #unspecified)
				   (eq? data #unspecified))
			       remove
			       (fun key data bucket))))
		       (else
			;; all strong
			(fun (caar bucket) (cdar bucket) bucket)))))
	      (cond
		 ((eq? ret keepgoing)
		  (liip (cdr bucket) bucket))
		 ((eq? ret remove)
		  (remove-bucket bucket last-bucket table buckets i)
		  (liip (cdr bucket) last-bucket))
		 ((eq? ret removestop)
		  (remove-bucket bucket last-bucket table buckets i))
		 (else ret)))))))

;*---------------------------------------------------------------------*/
;*    keys-traverse-hash ...                                           */
;*---------------------------------------------------------------------*/
(define (keys-traverse-hash table::struct fun)
   ;; cleanup all dead references
   (weak-keys-hashtable-filter! table (lambda (k v) #t))
   ;; apply fun to all live entries
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0))
	 (unless (=fx i buckets-len)
	    (let ((bucket (vector-ref buckets i)))
	       (for-each (lambda (p)
			    (unless (eq? (weakptr-data p) #unspecified)
			       (fun (weakptr-data p) (weakptr-ref p))))
		  bucket))))))

;*---------------------------------------------------------------------*/
;*    old-traverse-hash ...                                            */
;*---------------------------------------------------------------------*/
(define (old-traverse-hash table::struct fun)
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
;*    traverse-hash ...                                                */
;*---------------------------------------------------------------------*/
(define (traverse-hash table::struct fun)
   (if (hashtable-weak-keys? table)
       (keys-traverse-hash table fun)
       (old-traverse-hash table fun)))

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
;*    weak-keys-hashtable-filter! ...                                  */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-filter! table::struct fun::procedure)
   (let* ((buckets (%hashtable-buckets table))
	  (buckets-len (vector-length buckets)))
      (let loop ((i 0))
	 (when (<fx i buckets-len)
	    (let ((bucket (vector-ref buckets i))
		  (count 0))
	       (vector-set! buckets i
		  (filter! (lambda (v)
			      (if (or (eq? (weakptr-data v) #unspecified)
				      (not (fun (weakptr-data v) (weakptr-ref v))))
				  (begin
				     (set! count (+fx count 1))
				     #f)
				  #t))
		     bucket))
	       (%hashtable-size-set! table (-fx (%hashtable-size table) count))
	       (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-filter! ...                                   */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-filter! table::struct fun::procedure)
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
;*    weak-hashtable-filter! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-filter! table::struct fun::procedure)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-filter! table fun)
       (weak-old-hashtable-filter! table fun)))
       
;*---------------------------------------------------------------------*/
;*    weak-hashtable-clear! ...                                        */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-clear! table::struct)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-filter! table (lambda (k v) #f))
       (weak-old-hashtable-filter! table (lambda (k v) #f))))

;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-contains? ...                                */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-contains? table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num)))
      (let loop ((bucket bucket))
	 (cond
	    ((null? bucket)
	     #f)
	    ((hashtable-equal? table (weakptr-data (car bucket)) key)
	     #t)
	    (else
	     (loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-contains? ...                                 */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-contains? table::struct key::obj)
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
;*    weak-hashtable-contains? ...                                     */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-contains? table::struct key::obj)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-contains? table key)
       (weak-old-hashtable-contains? table key)))

;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-get ...                                      */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-get table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref buckets bucket-num)))
      (let loop ((bucket bucket))
	 (cond
	    ((null? bucket)
	     #f)
	    ((hashtable-equal? table (weakptr-data (car bucket)) key)
	     (weakptr-ref (car bucket)))
	    (else
	     (loop (cdr bucket)))))))

;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-get ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-get table::struct key::obj)
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
;*    weak-hashtable-get ...                                           */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-get table::struct key::obj)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-get table key)
       (weak-old-hashtable-get table key)))
   
;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-put! ...                                     */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-put! table::struct key::obj obj::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num))
	  (max-bucket-len (%hashtable-max-bucket-len table)))
      (if (null? bucket)
	  (begin
	     (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
	     (vector-set-ur! buckets bucket-num (list (make-weakptr key obj)))
	     obj)
	  (let loop ((buck bucket)
		     (count 0))
	     (cond
		((null? buck)
		 (%hashtable-size-set! table (+fx (%hashtable-size table) 1))
		 (vector-set-ur! buckets bucket-num
		    (cons (make-weakptr key obj) bucket))
		 (when (>fx count max-bucket-len)
		    (weak-keys-hashtable-expand! table))
		 obj)
		((hashtable-equal? table (weakptr-data (car buck)) key)
		 (let ((old-obj (weakptr-ref (car buck))))
		    (weakptr-ref-set! (car buck) obj)
		    old-obj))
		(else
		 (loop (cdr buck) (+fx count 1))))))))

;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-put! ...                                      */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-put! table::struct key::obj obj::obj)
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
	     (when (>fx count max-bucket-len)
		(weak-hashtable-expand! table))
	     obj))))

;*---------------------------------------------------------------------*/
;*    weak-hashtable-put! ...                                          */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-put! table::struct key::obj obj::obj)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-put! table key obj)
       (weak-old-hashtable-put! table key obj)))

;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-update! ...                                  */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-update! table::struct key::obj proc::procedure obj)
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
		 (vector-set-ur! buckets bucket-num
		    (cons (make-weakptr key obj) bucket))
		 (when (>fx count max-bucket-len)
		    (weak-keys-hashtable-expand! table))
		 obj)
		((hashtable-equal? table (weakptr-data (car buck)) key)
		 (let ((res (proc (weakptr-ref (car buck)))))
		    (weakptr-ref-set! (car buck) res)
		    res))
		(else
		 (loop (cdr buck) (+fx count 1))))))))
   
;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-update! ...                                   */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-update! table::struct key::obj proc::procedure obj)
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
;*    weak-hashtable-update! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-update! table::struct key::obj proc::procedure obj)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-update! table key proc obj)
       (weak-old-hashtable-update! table key proc obj)))

;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-add! ...                                     */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-add! table::struct key::obj proc::procedure obj init)
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
		    (vector-set-ur! buckets bucket-num
		       (cons (make-weakptr key v) bucket))
		    (when (>fx count max-bucket-len)
		       (weak-keys-hashtable-expand! table))
		    v))
		((hashtable-equal? table (weakptr-data (car buck)) key)
		 (let ((res (proc obj (weakptr-ref (car buck)))))
		    (weakptr-ref-set! (car buck) res)
		    res))
		(else
		 (loop (cdr buck) (+fx count 1))))))))
   
;*---------------------------------------------------------------------*/
;*    weak-hashtable-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-add! table::struct key::obj proc::procedure obj init)
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
;*    weak-hashtable-add! ...                                          */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-add! table::struct key::obj proc::procedure obj init)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-add! table key proc obj init)
       (weak-old-hashtable-add! table key proc obj init)))

;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-remove! ...                                  */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-remove! table::struct key::obj)
   (let* ((buckets (%hashtable-buckets table))
	  (bucket-len (vector-length buckets))
	  (bucket-num (remainderfx (table-get-hashnumber table key) bucket-len))
	  (bucket (vector-ref-ur buckets bucket-num)))
      (cond
	 ((null? bucket)
	  #f)
	 ((hashtable-equal? table (weakptr-data (car bucket)) key)
	  (vector-set-ur! buckets bucket-num (cdr bucket))
	  (%hashtable-size-set! table (-fx (%hashtable-size table) 1))
	  #t)
	 (else
	  (let loop ((bucket (cdr bucket))
		     (prev bucket))
	     (if (pair? bucket)
		 (if (hashtable-equal? table (weakptr-data (car bucket)) key)
		     (begin
			(set-cdr! prev (cdr bucket))
			(%hashtable-size-set! table
			   (-fx (%hashtable-size table) 1))
			#t)
		     (loop (cdr bucket) bucket))
		 #f))))))

;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-remove! ...                                   */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-remove! table::struct key::obj)
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
;*    weak-hashtable-remove! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-remove! table::struct key::obj)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-remove! table key)
       (weak-old-hashtable-remove! table key)))

;*---------------------------------------------------------------------*/
;*    weak-keys-hashtable-expand! ...                                  */
;*---------------------------------------------------------------------*/
(define (weak-keys-hashtable-expand! table)
   (let* ((old-bucks (%hashtable-buckets table))
	  (len (vector-length old-bucks))
	  (new-len (*fx 2 len))
	  (max-len (%hashtable-max-length table))
	  (count 0))
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
		(cond
		   ((<fx i len)
		    (for-each (lambda (cell)
				 (let ((key (weakptr-data cell)))
				    (if (eq? key #unspecified)
					(set! count (+fx count 1))
					(let* ((n (table-get-hashnumber table key))
					       (h (remainderfx n new-len)))
					   (vector-set-ur! new-bucks
					      h (cons cell (vector-ref new-bucks h)))))))
		       (vector-ref-ur old-bucks i))
		    (loop (+fx i 1)))
		   ((>fx count 0)
		    (%hashtable-size-set! table
		       (-fx (%hashtable-size table) count))))))
	  (error "hashtable-put!"
	     (format "Hashtable too large (new-len=~a/~a, size=~a)"
		new-len max-len
		(hashtable-size table))
	     table))))
   
;*---------------------------------------------------------------------*/
;*    weak-old-hashtable-expand! ...                                   */
;*---------------------------------------------------------------------*/
(define (weak-old-hashtable-expand! table)
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

;*---------------------------------------------------------------------*/
;*    weak-hashtable-expand! ...                                       */
;*---------------------------------------------------------------------*/
(define (weak-hashtable-expand! table)
   (if (hashtable-weak-keys? table)
       (weak-keys-hashtable-expand! table)
       (weak-old-hashtable-expand! table)))

