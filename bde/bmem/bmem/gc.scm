;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/bde/bmem/bmem/gc.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 20 09:53:55 2003                          */
;*    Last change :  Mon Jan 20 08:52:24 2020 (serrano)                */
;*    Copyright   :  2003-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Visualize GC information                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem_gc
   (import  html
	    bmem
	    bmem_tools
	    bmem_function
	    bmem_type)
   (include "html.sch")
   (export  (make-gc-tables ::pair-nil ::pair-nil ::pair-nil)
	    (make-gc-summary ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    make-gc-tables ...                                               */
;*---------------------------------------------------------------------*/
(define (make-gc-tables gcmon fun* types)
   (let* ((gc* (cdr gcmon))
	  (maxhsize::llong (apply maxllong (map caddr gc*)))
	  (nbtypes (+fx 1 (apply max (map car (cdr types)))))
	  (tvec (make-vector nbtypes)))
      (for-each (lambda (t)
		   (vector-set! tvec (car t) (cadr t)))
	 (cdr types))
      (html-table
       :width "100%"
       `(,(html-tr
	   `(,(html-td
	       :valign "top" :width "50%"
	       (make-gc-function-table maxhsize gc* fun*))
	     ,(html-td
	       :valign "top" :width "50%"
	       (make-gc-type-table maxhsize gc* fun* nbtypes tvec))))))))

;*---------------------------------------------------------------------*/
;*    make-gc-function-table ...                                       */
;*---------------------------------------------------------------------*/
(define (make-gc-function-table maxhsize::llong gc*::pair-nil fun*::pair-nil)
   
   (define (gc->cell gc)
      (let* ((n (car gc))
	     (hsize (caddr gc))
	     (asize (cadr gc))
	     (per 0)
	     (sum #l0)
	     (cell* (map (lambda (f)
			    (with-access::funinfo f (num ident)
			       (let* ((fgc (funinfo-find-gc f n))
				      (size (if (pair? fgc) (cadr fgc) #l0))
				      (nf num)
				      (id (format "function~a" nf)))
				  (set! sum (+llong sum size))
				  (set! per (+fx per (% size maxhsize)))
				  (list (% size maxhsize)
				     id
				     (format "~a: ~a (~a%)"
					(function-ident-pp ident)
					(word->size size)
					(% size asize))))))
		       fun*)))
	 (when (>=llong (absllong (-llong asize sum)) #l1024)
	    (warning "make-gc-function-table"
	       "incorrect allocation size --"
	       " GC=" n " sum=" sum " alloc=" asize
	       " delta=" (-llong asize sum)))
	 (append cell*
	    (list (list (-fx (% hsize maxhsize) per)
		     "gc0"
		     (format "heap size: ~a"
			(word->size (caddr gc))))))))
   
   (let* ((gc* (filter (lambda (gc)
			  (>llong (cadr gc) 0))
		  gc*))
	  (allsize (apply + (map cadr gc*)))
	  (cell* (map gc->cell gc*))
	  (row* (map (lambda (gc cells)
			(let* ((size (cadr gc))
			       (msize (caddr gc))
			       (size% (% size maxhsize))
			       (num (integer->string (+fx 1 (car gc))))
			       (id (string-append "gc" num))
			       (tdl (html-color-item id num))
			       (tds (html-td :class "size"
				       :align "left"
				       (format "~a% (~ak/~ak)"
					  size%
					  (word->kb size)
					  (word->kb msize)))))
			   (list (html-row-gauge cells tdl tds))))
		   gc* cell*))
	  (srow (html-tr (list (html-td "")
			    (html-td :colspan 100
			       :align "right"
			       :class "olegend"
			       "overall allocated memory:")
			    (html-td :align "left"
			       :class "osize"
			       (word->size allsize))))))
      (html-profile (append (apply append row*) (list srow))
	 "gc-function" "Gc (functions)"
	 '("gc" "8%")
	 '("memory" "20%")
	 "72%")))

;*---------------------------------------------------------------------*/
;*    make-gc-type-table ...                                           */
;*---------------------------------------------------------------------*/
(define (make-gc-type-table maxhsize::llong gc*::pair-nil fun*::pair-nil
			    nbtypes tvecnames)
   (define (gc->cell gc)
      (let ((n (car gc))
	    (tvec (make-type-vector nbtypes))
	    (hsize (caddr gc))
	    (asize (cadr gc))
	    (per 0)
	    (sum #l0))
	 (define (mark-function! f)
	    (let* ((dtype (with-access::funinfo f (dtype) dtype))
		   (dtypegc (assq n dtype)))
	       (if (and (pair? dtypegc) (pair? (cdr dtypegc)))
		   (for-each (lambda (dt)
				(let ((n (car dt))
				      (s (caddr dt)))
				   (set! sum (+llong s sum))
				   (vector-set! tvec
				      n
				      (+llong s (vector-ref tvec n)))))
		      (cdr dtypegc)))))
	 (for-each mark-function! fun*)
	 (let ((cell* (mapv (lambda (t i)
			       (let ((id (format "type~a" i))
				     (p (% (vector-ref tvec i) maxhsize)))
				  (set! per (+fx per p))
				  (list p
					id
					(format "~a (~a%)"
						(vector-ref tvecnames i)
						(% (vector-ref tvec i) sum)))))
			    tvec)))
	    (append cell*
		    (list (list (-fx (% hsize maxhsize) per)
				"gc0"
				(format "heap size: ~a"
					(word->size (caddr gc)))))))))
   (let* ((gc* (filter (lambda (gc)
			  (>llong (cadr gc) #l0))
		       gc*))
	  (allsize (apply + (map cadr gc*)))
	  (cell* (map gc->cell gc*))
	  (row* (map (lambda (gc cells)
			(let* ((size (cadr gc))
			       (msize (caddr gc))
			       (size% (% size maxhsize))
			       (num (integer->string (+fx 1 (car gc))))
			       (id (string-append "gc" num))
			       (tdl (html-color-item id num))
			       (tds (html-td :class "size"
					     :align "left"
					     (format "~a% (~ak/~ak)"
						     size%
						     (word->kb size)
						     (word->kb msize)))))
			   (list (html-row-gauge cells tdl tds))))
		     gc* cell*)))
      (html-profile (apply append row*)
	 "gc-function" "Gc (types)"
	 '("gc" "8%")
	 '("memory" "20%")
	 "72%")))

;*---------------------------------------------------------------------*/
;*    make-gc-summary ...                                              */
;*---------------------------------------------------------------------*/
(define (make-gc-summary gcmon)
   (let ((hz (map caddr (cdr gcmon))))
      (html-verticalbox
	 (list
	    (html-table :width "100%" :class "gc-summary"
	       `(,(html-tr
		     `(,(html-th :align "left" :width "15%" "gc number:")
		       ,(html-td :align "left" 
			   (integer->string (length (cdr gcmon))))))
		 ,(html-tr
		     `(,(html-th :align "left" :width "15%" "alloc. size:")
		       ,(html-td :align "left" 
			   (format "~aMB"
			      (round
				 (/ (apply + (map cadr (cdr gcmon)))
				    1024 1024))))))
		 ,(html-tr
		     `(,(html-th :align "left" :width "15%" "heap size:")
		       ,(html-td :align "left" 
			   (format "min: ~aMB, max: ~aMb"
			      (round (/ (apply min hz) 1024 1024))
			      (round (/ (apply max hz) 1024 1024))))))))))))
