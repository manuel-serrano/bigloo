;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Ieee/vector.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul  6 14:18:49 1992                          */
;*    Last change :  Wed Feb 12 08:59:30 2020 (serrano)                */
;*    -------------------------------------------------------------    */
;*    6.8. Vectors (page 26, r4)                                       */
;*    -------------------------------------------------------------    */
;*    Source documentation:                                            */
;*       @path ../../manuals/body.texi@                                */
;*       @node Vectors@                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module __r4_vectors_6_8
   
   (import  __error
	    __param)
   
   (use     __type
	    __bigloo
	    __tvector
	    __bignum
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_equivalence_6_2
	    __r4_strings_6_7
	    __r4_booleans_6_1
	    __r4_symbols_6_4
	    __r4_pairs_and_lists_6_3
	    __r4_control_features_6_9
	    
	    __evenv)
   
   (extern  (macro $vector?::bool (::obj) "VECTORP")
	    (macro $free-vector-uncollectable::void (::vector) "FREE_VECTOR_UNCOLLECTABLE")
	    ($make-vector::vector (::long ::obj) "make_vector")
	    ($make-vector-uncollectable::vector (::long ::obj) "make_vector_uncollectable")
	    ($create-vector::vector (::long) "create_vector")
	    ($create-vector-uncollectable::vector (::long) "create_vector_uncollectable")
	    ($vector-fill!::obj (::vector ::long ::long ::obj) "bgl_fill_vector")
	    (macro $vector-length::long (::vector) "VECTOR_LENGTH")
	    (macro $vector-ref::obj (::vector ::long) "VECTOR_REF")
	    (macro $vector-ref-ur::obj (::vector ::long) "VECTOR_REF")
	    (macro $vector-set!::obj (::vector ::long ::obj) "VECTOR_SET")
	    (macro $vector-set-ur!::obj (::vector ::long ::obj) "VECTOR_SET")
	    (macro $vector-bound-check?::bool (::long ::long) "BOUND_CHECK")
	    (macro $vector-tag-set!::obj (::vector ::int) "VECTOR_TAG_SET")
	    (macro $vector-tag::int (::vector) "VECTOR_TAG")
	    ($sort-vector::vector (::vector ::procedure) "sort_vector")
	    (macro $vector-shrink!::vector (::vector ::long) "BGL_VECTOR_SHRINK"))
   
   (java    (class foreign
	       (method static $vector?::bool (::obj)
		  "VECTORP")
	       (method static $free-vector-uncollectable::void (::vector)
		  "FREE_VECTOR_UNCOLLECTABLE")
	       (method static $make-vector::vector (::int ::obj)
		  "make_vector")
	       (method static $make-vector-uncollectable::vector (::int ::obj)
		  "make_vector")
	       (method static $create-vector::vector (::int)
		  "create_vector")
	       (method static $create-vector-uncollectable::vector (::int)
		  "create_vector")
	       (method static $vector-fill!::obj (::vector ::int ::int ::obj)
		  "fill_vector")
	       (method static $vector-length::int (::vector)
		  "VECTOR_LENGTH")
	       (method static $vector-ref::obj (::vector ::int)
		  "VECTOR_REF")
	       (method static $vector-ref-ur::obj (::vector ::int)
		  "VECTOR_REF")
	       (method static $vector-set!::obj (::vector ::int ::obj)
		  "VECTOR_SET")
	       (method static $vector-set-ur!::obj (::vector ::int ::obj)
		  "VECTOR_SET")
	       (method static $vector-bound-check?::bool (::int ::int)
		  "BOUND_CHECK")
	       (method static $vector-tag-set!::obj (::vector ::int)
		  "VECTOR_TAG_SET")
	       (method static $sort-vector::vector (::vector ::procedure)
		  "sort_vector")
	       (method static $vector-tag::int (::vector)
		  "VECTOR_TAG")
	       (method static $vector-shrink!::vector (::vector ::long)
		  "BGL_VECTOR_SHRINK")))
   
   (export  (inline vector?::bool obj)
	    (inline make-vector::vector ::long #!optional (fill #unspecified))
	    (inline vector::vector . args)
	    (inline vector-length::long ::vector)
	    (inline vector-ref ::vector ::long)
	    (inline vector-set! ::vector ::long ::obj) 
	    (inline vector-ref-ur ::vector ::long) 
	    (inline vector-set-ur! ::vector ::long ::obj)
	    (vector->list::pair-nil ::vector)
	    (list->vector::vector ::pair-nil)
	    (vector-fill! vec::vector fill
	       #!optional (start::long 0) (end::long (vector-length vec)))
	    (inline vector-tag::int ::vector)
	    (inline vector-tag-set! ::vector ::int)
	    (copy-vector::vector ::vector ::long)
	    (vector-copy3::vector ::vector start stop)
	    (vector-copy::vector ::vector . args)
	    (vector-copy! ::vector ::long source
	       #!optional (sstart 0) (send (vector-length source)))
	    (vector-append::vector ::vector . args)
	    (sort ::obj ::obj)
	    (vector-map::vector ::procedure ::vector . rests)
	    (vector-map!::vector ::procedure ::vector . rests)
	    (vector-for-each ::procedure ::vector . rests)
	    (inline vector-shrink!::vector ::vector ::long))
   
   (pragma  ($make-vector no-cfa-top nesting)
	    ($create-vector no-cfa-top nesting)
	    ($vector? (predicate-of vector) no-cfa-top nesting)
	    (vector? (predicate-of vector) nesting)
	    ($vector-length side-effect-free no-cfa-top nesting args-safe)
	    (vector-length side-effect-free no-cfa-top nesting)
	    ($vector-ref side-effect-free no-cfa-top nesting args-safe)
	    ($vector-ref-ur side-effect-free no-cfa-top nesting args-safe)
	    (vector-ref side-effect-free nesting)
	    (vector-ref-ur side-effect-free nesting)
	    (vector-tag side-effect-free no-cfa-top)))

;*---------------------------------------------------------------------*/
;*    vector? ...                                                      */
;*---------------------------------------------------------------------*/
(define-inline (vector? obj)
   ($vector? obj))

;*---------------------------------------------------------------------*/
;*    make-vector ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (make-vector int #!optional (fill #unspecified))
   ($make-vector int fill))

;*---------------------------------------------------------------------*/
;*    vector . args ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (vector . args)
   (list->vector args))

;*---------------------------------------------------------------------*/
;*    vector-length ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (vector-length vector)
   ($vector-length vector))

;*---------------------------------------------------------------------*/
;*    vector-ref ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (vector-ref vector k)
   ($vector-ref vector k))

;*---------------------------------------------------------------------*/
;*    vector-set! ...                                                  */
;*---------------------------------------------------------------------*/
(define-inline (vector-set! vector k obj)
   ($vector-set! vector k obj))

;*---------------------------------------------------------------------*/
;*    vector-ref-ur ...                                                */
;*---------------------------------------------------------------------*/
(define-inline (vector-ref-ur vector k)
   ($vector-ref-ur vector k))

;*---------------------------------------------------------------------*/
;*    vector-set-ur! ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (vector-set-ur! vector k obj)
   ($vector-set-ur! vector k obj))

;*---------------------------------------------------------------------*/
;*    vector->list ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector->list vector)
   (let ((vlen (vector-length vector)))
      (if (=fx vlen 0)
	  '()
	  (let loop ((i (-fx vlen 1))
		     (acc '()))
	     (if (=fx i 0)
		 (cons (vector-ref-ur vector i) acc)
		 (loop (-fx i 1) (cons (vector-ref-ur vector i) acc)))))))

;*---------------------------------------------------------------------*/
;*    list->vector ...                                                 */
;*---------------------------------------------------------------------*/
(define (list->vector list)
   (let* ((len (length list))
	  (vec ($create-vector len)))
      (let loop ((i 0)
		 (l list))
	 (if (=fx i len)
	     vec
	     (begin
		(vector-set-ur! vec i (car l))
		(loop (+fx i 1) (cdr l)))))))

;*---------------------------------------------------------------------*/
;*    vector-fill! ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-fill! vec fill
	   #!optional (start::long 0) (end::long (vector-length vec)))
   (cond
      ((<fx start 0)
       (error "vector-fill!" "wrong negative start" start))
      ((>fx end (vector-length vec))
       (error "vector-fill!" "end index too large" end))
      ((>=fx start end)
       (if (and (=fx start end) (=fx start 0))
	   #unspecified
	   (error "vector-fill!" "start index larger than end"
	      (cons start end))))
      (else
       ($vector-fill! vec start end fill))))

;*---------------------------------------------------------------------*/
;*    vector-tag ...                                                   */
;*---------------------------------------------------------------------*/
(define-inline (vector-tag vector)
   ($vector-tag vector))

;*---------------------------------------------------------------------*/
;*    vector-tag-set! ...                                              */
;*---------------------------------------------------------------------*/
(define-inline (vector-tag-set! vector tag)
   ($vector-tag-set! vector tag))

;*---------------------------------------------------------------------*/
;*    copy-vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (copy-vector::vector old-vec::vector new-len::long)
   (let* ((old-len (vector-length old-vec))
	  (new-vec (make-vector new-len))
	  (min     (if (<fx new-len old-len)
		       new-len
		       old-len)))
      (let loop ((i 0))
	 (if (=fx i min)
	     new-vec
	     (begin
		(vector-set-ur! new-vec i (vector-ref-ur old-vec i))
		(loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    vector-copy3 ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-copy3::vector old-vec::vector start stop)
   (let* ((old-len (vector-length old-vec))
	  (new-len (-fx stop start))
	  (new-vec (make-vector new-len))
	  (min (if (<fx new-len old-len)
		   new-len
		   old-len)))
      (if (or (<fx new-len 0) (>fx start old-len) (>fx stop old-len))
	  (error "vector-copy" "Illegal indexes" (cons start stop))
	  (let loop ((r start)
		     (w 0))
	     (if (=fx r stop)
		 new-vec
		 (begin
		    (vector-set-ur! new-vec w (vector-ref-ur old-vec r))
		    (loop (+fx r 1) (+fx w 1))))))))

;*---------------------------------------------------------------------*/
;*    vector-copy ...                                                  */
;*---------------------------------------------------------------------*/
(define (vector-copy::vector old-vec::vector . args)
   (let* ((old-len (vector-length old-vec))
	  (start (if (pair? args)
		     (if (fixnum? (car args))
			 (car args)
			 (error "vector-copy" "Illegal argument" (car args)))
		     0))
	  (stop (if (and (pair? args) (pair? (cdr args)))
		    (if (or (pair? (cddr args))
			    (not (fixnum? (cadr args))))
			(error "vector-copy" "Illegal argument" (cdr args))
			(cadr args))
		    old-len)))
      (vector-copy3 old-vec start stop)))

;*---------------------------------------------------------------------*/
;*    vector-copy! ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-copy! target tstart source
                      #!optional (sstart 0) (send (vector-length source)))
   (let* ((end (minfx send (vector-length source)))
          (count (-fx end sstart))
          (tend (minfx (+fx tstart count) (vector-length target))))
      (if (and (eq? target source)
               (<fx sstart tstart)
               (<fx tstart (+fx sstart (-fx send sstart))))
          (let loop ((i  (-fx end 1))
                     (j (-fx tend 1)))
             (when (and (>=fx i sstart) (>=fx j tstart))
                (vector-set-ur! target j (vector-ref-ur source i))
                (loop (-fx i 1) (-fx j 1))))
          (let loop ((i sstart)
                     (j tstart))
             (when (and (<fx i end) (<fx j tend))
                (vector-set-ur! target j (vector-ref-ur source i))
                (loop (+fx i 1) (+fx j 1)))))))

;*---------------------------------------------------------------------*/
;*    vector-append ...                                                */
;*---------------------------------------------------------------------*/
(define (vector-append::vector vec::vector . args)
   (let loop ((len (vector-length vec))
	      (vects args))
      (if (null? vects)
	  (let ((res (make-vector len)))
	     (vector-copy! res 0 vec)
	     (let loop ((i (vector-length vec))
			(vects args))
		(if (null? vects)
		    res
		    (let ((vec (car vects)))
		       (vector-copy! res i vec)
		       (loop (+fx i (vector-length vec)) (cdr vects))))))
	  (loop (+fx (vector-length (car vects)) len) (cdr vects)))))

;*---------------------------------------------------------------------*/
;*    @deffn sort@ ...                                                 */
;*---------------------------------------------------------------------*/
(define (sort o1 o2)
   (if (procedure? o1)
       (inner-sort o2 o1)
       (inner-sort o1 o2)))

;*---------------------------------------------------------------------*/
;*    inner-sort ...                                                   */
;*---------------------------------------------------------------------*/
(define (inner-sort obj proc::procedure)
   (cond
      ((null? obj)
       obj)
      ((and (pair? obj) (null? (cdr obj)))
       obj)
      (else
       (let ((vec (cond
		     ((vector? obj)
		      (let* ((len (vector-length obj))
			     (new ($create-vector len)))
			 (let loop ((i 0))
			    (if (<fx i len)
				(begin
				   (vector-set-ur! new i (vector-ref-ur obj i))
				   (loop (+fx i 1)))))
			 new))
		     ((pair? obj)
		      (list->vector obj))
		     (else
		      (error "sort"
			     "Object must be a list or a vector"
			     obj)))))
	  (let ((res ($sort-vector vec proc)))
	     (if (pair? obj)
		 (vector->list res)
		 res))))))

;*---------------------------------------------------------------------*/
;*    vector-map2! ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-map2! proc vdest vsrc)
   (let ((len (vector-length vdest)))
      (let loop ((i 0))
	 (if (<fx i len)
	     (begin
		(vector-set-ur! vdest i (proc (vector-ref-ur vsrc i)))
		(loop (+fx i 1)))
	     vdest))))

;*---------------------------------------------------------------------*/
;*    vector-mapN! ...                                                 */
;*---------------------------------------------------------------------*/
(define (vector-mapN! proc vdest vsrc vrest)
   (let ((len (vector-length vdest)))
      (let loop ((i 0))
	 (if (<fx i len)
	     (let* ((args (map (lambda (v) (vector-ref v i)) vrest))
		    (nval (apply proc (vector-ref vsrc i) args)))
		(vector-set-ur! vdest i nval)
		(loop (+fx i 1)))
	     vdest))))

;*---------------------------------------------------------------------*/
;*    vector-map ...                                                   */
;*---------------------------------------------------------------------*/
(define (vector-map proc v . rest)
   (let* ((len (vector-length v))
	  (nv ($create-vector len)))
      (cond
	 ((null? rest)
	  (vector-map2! proc nv v))
	 ((every (lambda (v) (and (vector? v) (=fx (vector-length v) len))))
	  (vector-mapN! proc nv v rest))
	 (else
	  (error "vector-map" "Illegal arguments" rest)))))

;*---------------------------------------------------------------------*/
;*    vector-map! ...                                                  */
;*---------------------------------------------------------------------*/
(define (vector-map! proc v . rest)
   (let ((len (vector-length v)))
      (cond
	 ((null? rest)
	  (vector-map2! proc v v))
	 ((every (lambda (v) (and (vector? v) (=fx (vector-length v) len))))
	  (vector-mapN! proc v v rest))
	 (else
	  (error "vector-map!" "Illegal arguments" rest)))))

;*---------------------------------------------------------------------*/
;*    vector-for-each2 ...                                             */
;*---------------------------------------------------------------------*/
(define (vector-for-each2 proc vsrc)
   (let ((len (vector-length vsrc)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (proc (vector-ref-ur vsrc i))
	    (loop (+fx i 1))))))

;*---------------------------------------------------------------------*/
;*    vector-for-eachN ...                                             */
;*---------------------------------------------------------------------*/
(define (vector-for-eachN proc vsrc vrest)
   (let ((len (vector-length vsrc)))
      (let loop ((i 0))
	 (when (<fx i len)
	    (let ((args (map (lambda (v) (vector-ref v i)) vrest)))
	       (apply proc (vector-ref vsrc i) args)
	       (loop (+fx i 1)))))))

;*---------------------------------------------------------------------*/
;*    vector-for-each ...                                              */
;*---------------------------------------------------------------------*/
(define (vector-for-each proc v . rest)
   (let ((len (vector-length v)))
      (cond
	 ((null? rest)
	  (vector-for-each2 proc v))
	 ((every (lambda (v) (and (vector? v) (=fx (vector-length v) len))))
	  (vector-for-eachN proc v rest))
	 (else
	  (error "vector-for-each" "Illegal arguments" rest)))))

;*---------------------------------------------------------------------*/
;*    vector-shrink! ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (vector-shrink! vec nlen)
   ($vector-shrink! vec nlen))
