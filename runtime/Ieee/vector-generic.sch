;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/runtime/Ieee/vector-generic.sch     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 24 10:17:35 2024                          */
;*    Last change :  Tue Dec 24 10:38:03 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generic portable vector implementation                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import __bit)
   (export (sort_vector::vector ::vector ::procedure))
   (extern (export sort_vector "sort_vector")))

;*---------------------------------------------------------------------*/
;*    sort_vector ...                                                  */
;*---------------------------------------------------------------------*/
(define (sort_vector vec cmp)

   (define (qsort lo hi)
      (if (<fx lo hi)
	  (let ((i lo)
		(j hi)
		(pivot (vector-ref vec hi)))
	     (let loop1 ()
		(if (<fx i j)
		    (begin
		       (let loop2 ()
			  (if (and (<fx i hi)
				   (cmp (vector-ref vec i) pivot))
			      (begin
				 (set! i (+fx i 1))
				 (loop2))))
		       (let loop3 ()
			  (if (and (>fx j lo)
				   (not (cmp (vector-ref vec j) pivot)))
			      (begin
				 (set! j (-fx j 1))
				 (loop3))))
		       (if (<fx i j)
			   (let ((temp (vector-ref vec i)))
			      (vector-set! vec i (vector-ref vec j))
			      (vector-set! vec j temp)))
		       (loop1))))
	     (let ((temp (vector-ref vec i)))
		(vector-set! vec i (vector-ref vec hi))
		(vector-set! vec hi temp))
	     (qsort lo (-fx i 1))
	     (qsort (+fx i 1) hi))))
   
   (qsort 0 (-fx (vector-length vec) 1))
   vec)
