;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/runtime/Llib/tvector.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Mar 27 09:38:41 1995                          */
;*    Last change :  Sun Aug 25 09:08:27 2019 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The typed vectors Scheme management.                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __tvector

   (import  __error
	    __hash
	    __reader
	    __param
	    __bexit
	    __bignum
	    __object
	    __thread
	    __r4_symbols_6_4)
   
   (use     __type
	    __bigloo
	    __structure
	    __r4_numbers_6_5_fixnum
	    __r4_numbers_6_5_flonum
	    __r4_numbers_6_5_flonum_dtoa
	    __r4_equivalence_6_2
	    __r4_booleans_6_1
	    __r4_characters_6_6
	    __r4_vectors_6_8
	    __r4_pairs_and_lists_6_3
	    __r4_strings_6_7
	    __r4_output_6_10_3
	    __r4_ports_6_10_1
	    __r4_control_features_6_9
	    __evenv
	    __bit)

   (extern (macro $tvector?::bool (::obj)
		  "TVECTORP")
	   (macro $tvector-length::int (::tvector)
		  "TVECTOR_LENGTH")
	   
	   (macro $tvector-descr::obj (::tvector)
		  "TVECTOR_DESCR")
	   (macro $tvector-descr-set!::obj (::tvector ::obj)
		  "TVECTOR_DESCR_SET")
	   
	   (export get-tvector-descriptor "get_tvector_descriptor"))

   (java   (class foreign
	      (method static $tvector?::bool (::obj)
		      "TVECTORP")
	      (method static $tvector-length::int (::obj)
		      "TVECTOR_LENGTH")
	      
	      (method static $tvector-descr::obj (::obj)
		      "TVECTOR_DESCR")
	      (method static $tvector-descr-set!::obj (::obj ::obj)
		      "TVECTOR_DESCR_SET")))

   (export  (inline tvector?::bool ::obj)
	    (inline tvector-length::int ::tvector)
	    (get-tvector-descriptor::obj ::symbol)
	    (declare-tvector! ::string  ::procedure ::obj ::obj)
	    (tvector-ref ::tvector)
	    (tvector-id ::tvector)
	    (list->tvector ::symbol ::pair-nil)
	    (vector->tvector ::symbol ::vector)
	    (tvector->vector ::tvector))

   (pragma  ($tvector? (predicate-of tvector) no-cfa-top nesting)
	    (tvector? side-effect-free no-cfa-top nesting)
	    ($tvector-length side-effect-free no-cfa-top nesting args-safe)
	    (list->tvector no-cfa-top)
	    ($tvector-descr side-effect-free no-cfa-top nesting)
	    ($tvector-descr-set! nesting)
	    (get-tvector-descriptor side-effect-free no-cfa-top)
	    (tvector-ref side-effect-free no-cfa-top)
	    (tvector-id side-effect-free no-cfa-top)
	    (vector->tvector no-cfa-top)
	    (tvector->vector no-cfa-top)))

;*---------------------------------------------------------------------*/
;*    tvector? ...                                                     */
;*---------------------------------------------------------------------*/
(define-inline (tvector? obj)
   ($tvector? obj))

;*---------------------------------------------------------------------*/
;*    tvector-length ...                                               */
;*---------------------------------------------------------------------*/
(define-inline (tvector-length obj)
   ($tvector-length obj))

;*---------------------------------------------------------------------*/
;*    tvector-id ...                                                   */
;*---------------------------------------------------------------------*/
(define (tvector-id tvect)
   (tvect-descr-id ($tvector-descr tvect)))

;*---------------------------------------------------------------------*/
;*    tvec-descr ...                                                   */
;*    -------------------------------------------------------------    */
;*    The structure to implements typed-vector descriptor. A unique    */
;*    structure is allocated for all the instances of a typed-vector.  */
;*---------------------------------------------------------------------*/
(define-struct tvect-descr
   id          ;; symbol          :: the identifier
   allocate    ;; procedure       :: a allocator
   ref         ;; procedure       :: how to access the elements
   set)        ;; procedure       :: how to set an element

;*---------------------------------------------------------------------*/
;*    *tvector-table* ...                                              */
;*---------------------------------------------------------------------*/
(define *tvector-table* '())
   
;*---------------------------------------------------------------------*/
;*    get-tvector-descriptor ...                                       */
;*---------------------------------------------------------------------*/
(define (get-tvector-descriptor id::symbol)
   (and (pair? *tvector-table*)
	(let ((cell (assq id *tvector-table*)))
	   (if (pair? cell)
	       (cdr cell)
	       #f))))

;*---------------------------------------------------------------------*/
;*    declare-tvector! ...                                             */
;*    -------------------------------------------------------------    */
;*    The `id' argument has to be a c-string otherwise                 */
;*    declare-tvector! cannot be performed before the constants        */
;*    intialization.                                                   */
;*---------------------------------------------------------------------*/
(define (declare-tvector! id allocate ref set)
   (let* ((id  (string->symbol (case (bigloo-case-sensitivity)
				  ((upcase)
				   (string-upcase ($string->bstring id)))
				  ((downcase)
				   (string-downcase ($string->bstring id)))
				  (else
				   ($string->bstring id)))))
	  (old (get-tvector-descriptor id)))
      (if (not (tvect-descr? old))
	  (let ((new (tvect-descr id allocate ref set)))
	     (set! *tvector-table* (cons (cons id new) *tvector-table*))
	     new)
	  old)))
	    
;*---------------------------------------------------------------------*/
;*    tvector-ref ...                                                  */
;*---------------------------------------------------------------------*/
(define (tvector-ref tvector)
   (tvect-descr-ref ($tvector-descr tvector)))
 
;*---------------------------------------------------------------------*/
;*    list->tvector ...                                                */
;*---------------------------------------------------------------------*/
(define (list->tvector id::symbol l::pair-nil)
   (let ((descr (get-tvector-descriptor id)))
      (if (not descr)
	  (error "list->tvector" "Undeclared tvector" id)
	  (let ((allocate (tvect-descr-allocate descr))
		(set      (tvect-descr-set descr)))
	     (if (not (procedure? set))
		 (error "list->tvector"
			"Unable to convert to such tvector"
			id)
		 (let* ((len  (length l))
			(tvec (allocate len)))
		    (let loop ((l l)
			       (i 0))
		       (if (null? l)
			   tvec
			   (begin
			      (set tvec i (car l))
			      (loop (cdr l) (+fx i 1)))))))))))

;*---------------------------------------------------------------------*/
;*    vector->tvector ...                                              */
;*---------------------------------------------------------------------*/
(define (vector->tvector id::symbol v::vector)
   (let ((descr (get-tvector-descriptor id)))
      (if (not descr)
	  (error "vector->tvector" "Undeclared tvector" id)
	  (let ((allocate (tvect-descr-allocate descr))
		(set      (tvect-descr-set descr)))
	     (if (not (procedure? set))
		 (error "vector->tvector"
			"Unable to convert to such tvector"
			id)
		 (let* ((len  (vector-length v))
			(tvec (allocate len)))
		    (let loop ((i (-fx len 1)))
		       (if (=fx i -1)
			   tvec
			   (begin
			      (set tvec i (vector-ref v i))
			      (loop (-fx i 1)))))))))))

;*---------------------------------------------------------------------*/
;*    tvector->vector ...                                              */
;*---------------------------------------------------------------------*/
(define (tvector->vector tv::tvector)
   (let ((descr ($tvector-descr tv)))
      (let ((allocate (tvect-descr-allocate descr))
	    (ref      (tvect-descr-ref descr)))
	 (if (not (procedure? ref))
	     (error "tvector->vector"
		    "Unable to convert to such tvector"
		    (tvector-id tv))
	     (let* ((len  (tvector-length tv))
		    (vec  ($create-vector len)))
		(let loop ((i (-fx len 1)))
		   (if (=fx i -1)
		       vec
		       (begin
			  (vector-set! vec i (ref tv i))
			  (loop (-fx i 1))))))))))
