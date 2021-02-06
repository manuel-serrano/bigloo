;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/function.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 20 09:06:40 2003                          */
;*    Last change :  Tue Oct 24 16:02:49 2017 (serrano)                */
;*    Copyright   :  2003-20 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Display function allocations                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem_function
   (include "html.sch")
   (import  html
	    bmem_tools
	    bmem_type
	    bmem)
   (export  (class funinfo
	       (ident::symbol read-only)
	       (use::bool (default #f))
	       (num::int read-only)
	       (dsize::llong read-only)
	       (isize::llong read-only)
	       (gc*::pair-nil read-only)
	       (dtype::pair-nil read-only)
	       (itype::pair-nil read-only))
	    
	    (get-functions::pair-nil)
	    (add-function! ::pair)
	    (funinfo-find-gc ::funinfo ::int)
	    (funinfo-type-size::llong ::funinfo ::int)
	    (funinfo-gc-type-size::llong ::funinfo ::int ::int)
	    (make-function-tables fun* gcmon types)
	    (bmem-function ::bstring ::obj ::obj
			   ::pair-nil ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    *functions* ...                                                  */
;*---------------------------------------------------------------------*/
(define *functions* (make-hashtable))
(define *function-list* '())
(define *function-counter* 0)

;*---------------------------------------------------------------------*/
;*    get-functions ...                                                */
;*---------------------------------------------------------------------*/
(define (get-functions)
   *function-list*)

;*---------------------------------------------------------------------*/
;*    add-function! ...                                                */
;*---------------------------------------------------------------------*/
(define (add-function! fmon)
   (let* ((id (car fmon))
	  (gc* (sort (cdr fmon) (lambda (g1 g2) (< (car g1) (car g2)))))
	  (new (instantiate::funinfo
		  (ident id)
		  (num *function-counter*)
		  (dsize (apply + (map cadr gc*)))
		  (isize (apply + (map caddr gc*)))
		  (gc* gc*)
		  (dtype (map (lambda (gc)
				 (let ((c (assq 'dtype (cdddr gc))))
				    (if (pair? c)
					(cons (car gc) (cdr c))
					(list (car gc)))))
			      gc*))
		  (itype (map (lambda (gc)
				 (let ((c (assq 'itype (cdddr gc))))
				    (if (pair? c)
					(cons (car gc) (cdr c))
					(list (car gc)))))
			      gc*)))))
      (set! *function-list* (cons new *function-list*))
      (set! *function-counter* (+fx 1 *function-counter*))
      (hashtable-put! *functions* id new)
      new))

;*---------------------------------------------------------------------*/
;*    funinfo-find-gc ...                                              */
;*---------------------------------------------------------------------*/
(define (funinfo-find-gc fi num)
   (with-access::funinfo fi (gc*)
      (assq num gc*)))

;*---------------------------------------------------------------------*/
;*    funinfo-type-size ...                                            */
;*---------------------------------------------------------------------*/
(define (funinfo-type-size fi num)
   (with-access::funinfo fi (dtype)
      (let ((s #l0))
	 (for-each (lambda (gc)
		      (let ((c (assq num (cdr gc))))
			 (if (pair? c)
			     (set! s (+llong s (caddr c))))))
		   dtype)
	 s)))

;*---------------------------------------------------------------------*/
;*    funinfo-gc-type-size ...                                         */
;*---------------------------------------------------------------------*/
(define (funinfo-gc-type-size fi num gc)
   (with-access::funinfo fi (dtype)
      (let ((gc (assq gc dtype)))
	 (if (pair? gc)
	     (let ((c (assq num (cdr gc))))
		(if (pair? c)
		    (caddr c)
		    #l0))
	     #l0))))

;*---------------------------------------------------------------------*/
;*    make-function-tables ...                                         */
;*---------------------------------------------------------------------*/
(define (make-function-tables gcmon fun* types)
   (let* ((allsize (apply + (map cadr (cdr gcmon))))
	  (allsum (let ((sum 0))
		     (for-each (lambda (f)
				  (define (sum-type ti)
				     (set! sum (+llong sum (cadr ti))))
				  (for-each (lambda (gc)
					       (for-each sum-type (cdr gc)))
				     (with-access::funinfo f (dtype)
					dtype)))
			       fun*)
		     sum))
	  (nbtypes (+fx 1 (apply max (map car (cdr types)))))
	  (tvec (make-vector nbtypes)))
      (for-each (lambda (t)
		   (vector-set! tvec (car t) (cadr t)))
		(cdr types))
      (html-verticalbox
       `(,(make-function-type-mem-table fun* allsize nbtypes tvec)
	 ,(make-function-type-occ-table fun* allsum nbtypes tvec)
	 ,(make-function-gc-table fun* allsize nbtypes)))))

;*---------------------------------------------------------------------*/
;*    make-function-gc-table ...                                       */
;*---------------------------------------------------------------------*/
(define (make-function-gc-table fun* allsize nbtypes)
   (html-table :width "100%"
      `(,(html-tr
	    `(,(html-td :valign "top" :width "50%"
		  (make-function-gc-chart allsize fun*
		     (lambda (f) (with-access::funinfo f (dsize) dsize))
		     cadr
		     "function-gc-direct"
		     "Direct allocations (gc)"))
	      ,(html-td :valign "top" :width "50%"
		  (make-function-gc-chart allsize fun*
		     (lambda (f)
			(with-access::funinfo f (isize dsize)
			   (+llong isize dsize)))
		     (lambda (g)
			(+llong (cadr g) (caddr g)))
		     "function-gc-cumulative"
		     "Cumulative allocations (gc)")))))))

;*---------------------------------------------------------------------*/
;*    make-function-gc-chart ...                                       */
;*---------------------------------------------------------------------*/
(define (make-function-gc-chart allsize::llong fun*::pair-nil
	   funsize::procedure fungcsize::procedure
	   class::bstring caption::bstring)
   (let* ((fun* (sort (filter (lambda (f)
				 (let* ((size (funsize f))
					(size% (% size allsize)))
				    (> size% 0)))
			 fun*)
		   (lambda (f1 f2)
		      (>llong (funsize f1) (funsize f2)))))
	  (cell* (map (lambda (f)
			 ;; mark the function as used (for the legend)
			 (with-access::funinfo f (use gc* ident)
			    (set! use #t)
			    (let ((fsize (funsize f)))
			       (map (lambda (gc)
				       (let* ((n (+fx 1 (car gc)))
					      (gcsize (fungcsize gc))
					      (rsize (%00 gcsize fsize))
					      (per (% gcsize allsize))
					      (id (format "gc~a" n)))
					  (list per
					     id 
					     (format "gc ~a: ~a (~a% of ~a)"
						n
						(word->size gcsize)
						rsize
						(word->size fsize)))))
				  gc*))))
		    fun*))
	  (row* (map (lambda (f cells)
			(with-access::funinfo f (num ident)
			   (let* ((size (funsize f))
				  (size% (% size allsize))
				  (id (string-append "function"
					 (integer->string num)))
				  (tdl (html-color-item
					  id (function-ref ident)))
				  (tds (html-td :class "size"
					  :align "right"
					  (format "~a% (~a)"
					     size%
					     (word->size size))))
				  (cell% (apply + (map car cells)))
				  (rest (list (- size% cell%)
					   "gc-1"
					   (format "rest: ~a% of ~a"
					      (- size% cell%)
					      (word->size size)))))
			      (list (html-row-gauge cells tdl tds rest)
				 (html-tr (list (html-td) (html-td) (html-td) ))))))
		   fun* cell*)))
      (html-profile (apply append row*)
	 class caption
	 '("functions" "20%")
	 '("memory" "15%")
	 "65%")))

;*---------------------------------------------------------------------*/
;*    make-function-type-mem-table ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-mem-table fun* allsize nbtypes tvec)
   (html-table :width "100%"
      `(,(html-tr
	    `(,(html-td :valign "top" :width "50%"
		  (make-function-type-mem-chart allsize fun*
		     (lambda (f) (with-access::funinfo f (dsize) dsize))
		     (lambda (f) (with-access::funinfo f (dtype) dtype))
		     "function-type-direct"
		     "Direct allocations (size)"
		     nbtypes
		     tvec))
	      ,(html-td :valign "top" :width "50%"
		  (make-function-type-mem-chart allsize fun*
		     (lambda (f)
			(with-access::funinfo f (isize dsize)
			   (+llong isize dsize)))
		     (lambda (f)
			(with-access::funinfo f (itype dtype)
			   (map (lambda (i d)
				   (append i (cdr d)))
			      itype dtype)))
		     "function-type-indirect"
		     "Indirect allocations (size)"
		     nbtypes
		     tvec)))))))
   
;*---------------------------------------------------------------------*/
;*    make-function-type-occ-table ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-occ-table fun* allsize nbtypes tvec)
   (html-table :width "100%"
      `(,(html-tr
	    `(,(html-td :valign "top"  :width "50%"
		  (make-function-type-occ-chart allsize fun*
		     (lambda (f) (with-access::funinfo f (dsize) dsize))
		     (lambda (f) (with-access::funinfo f (dtype) dtype))
		     "function-type-direct"
		     "Direct allocations (occurrence)"
		     nbtypes
		     tvec))
	      ,(html-td :valign "top" :width "50%"
		  (make-function-type-occ-chart
		     allsize fun*
		     (lambda (f)
			(with-access::funinfo f (isize dsize)
			   (+fx isize dsize)))
		     (lambda (f)
			(with-access::funinfo f (itype dtype)
			   (map (lambda (i d)
				   (append i (cdr d)))
			      itype dtype)))
		     "function-type-indirect"
		     "Indirect allocations (occurrence)"
		     nbtypes
		     tvec)))))))
   
;*---------------------------------------------------------------------*/
;*    make-function-type-mem-chart ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-mem-chart allsize::llong fun*::pair-nil
				      funsize::procedure funtypes::procedure
				      class::bstring caption::bstring
				      nb-types::int tvecnames::vector)
   
   (define (fun->cell f)
      ;; mark the function used (for the legend)
      (with-access::funinfo f (use)
	 (set! use #t))
      (let* ((size (funsize f))
	     (size% (exact->inexact (% size allsize)))
	     (tvec (make-type-vector nb-types))
	     (at::llong #l0)
	     (sum::llong #l0))
	 (define (mark-type! ti)
	    (let ((t (car ti))
		  (n (caddr ti)))
	       (set! sum (+llong sum n))
	       (vector-set! tvec t (+llong (vector-ref tvec t) n))
	       (set! at (+llong at n))))
	 ;; mark all allocated types
	 (for-each (lambda (gc) (for-each mark-type! (cdr gc))) (funtypes f))
	 ;; construct the cells
	 (mapv (if (=llong at #l0)
		   (lambda (t i)
		      (list 0 "type0" "0 (0%)"))
		   (lambda (t i)
		      (let ((tp (if (>llong sum #l0)
				    (%00 t sum)
				    0))
			    (per (% t allsize)))
			 (list per
			    (string-append "type"
			       (integer->string i))
			    (format "~a: ~a (~a% of ~a)" 
			       (vector-ref tvecnames i)
			       (word->size t)
			       tp
			       (word->size sum))))))
	    tvec)))
   
   (let* ((fun* (filter (lambda (f)
			   (let* ((size (funsize f))
				  (size% (% size allsize)))
			      (>fx size% 0)))
		   fun*))
	  (fun* (sort fun*
		   (lambda (f1 f2)
		      (>llong (funsize f1) (funsize f2)))))
	  (cell* (filter-map fun->cell fun*))
	  (r (map (lambda (f cells)
		     (with-access::funinfo f (num ident)
			(let* ((size (funsize f))
			       (size% (% size allsize))
			       (id (string-append "function"
				      (integer->string num)))
			       (tdl (html-color-item
				       id
				       (function-ref ident)))
			       (tds (html-td :class "size"
				       :align "right"
				       (format "~a% (~a)"
					  size%
					  (word->size size))))
			       (cell% (apply + (map car cells)))
			       (rest (list (- size% cell%)
					"type-1"
					(format "rest: ~a% of ~a"
					   (- size% cell%)
					   (word->size size)))))
			   (list (html-row-gauge cells tdl tds rest)))))
		fun* cell*)))
      (html-profile (apply append r)
	 class caption
	 '("functions" "20%")
	 '("memory" "15%")
	 "65%")))

;*---------------------------------------------------------------------*/
;*    make-function-type-occ-chart ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-occ-chart allsize::llong fun*::pair-nil
	   funsize::procedure funtypes::procedure
	   class::bstring caption::bstring
	   nb-types::int tvecnames::vector)
   (define (fun->cell f)
      ;; mark the function used (for the legend)
      (with-access::funinfo f (use)
	 (set! use #t))
      (let* ((tvec (make-type-vector nb-types))
	     (at::llong #l0)
	     (sum::llong #l0))
	 (define (mark-type! ti)
	    (let ((t (car ti))
		  (n (cadr ti)))
	       (set! sum (+llong sum n))
	       (vector-set! tvec t (+llong (vector-ref tvec t) n))
	       (set! at (+llong at n))))
	 ;; mark all allocated types
	 (for-each (lambda (gc) (for-each mark-type! (cdr gc))) (funtypes f))
	 (let ((size% (exact->inexact (% sum allsize))))
	    ;; construct the cells
	    (list f
	       sum
	       (mapv (if (=llong at #l0)
			 (lambda (t i)
			    (list 0 "type0" "0 (0%)"))
			 (lambda (t i)
			    (let ((per (% t allsize))
				  (tp (if (>llong sum #l0)
					  (%00 t sum)
					  0)))
			       (list per 
				  (string-append "type"
				     (integer->string i))
				  (format "~a: ~a (~a%)" 
				     (vector-ref tvecnames i)
				     t
				     tp)))))
		  tvec)))))
   (let* ((cell* (map fun->cell fun*))
	  (cell* (filter (lambda (c)
			    (>fx (% (cadr c) allsize) 0))
		    cell*))
	  (cell* (sort cell*
		    (lambda (c1 c2)
		       (>llong (cadr c1) (cadr c2)))))
	  (r (map (lambda (cells)
		     (let ((f (car cells)))
			(with-access::funinfo f (num ident)
			   (let* ((size (cadr cells))
				  (cells (caddr cells))
				  (size% (% size allsize))
				  (id (string-append "function"
					 (integer->string
					    num)))
				  (tdl (html-color-item
					  id
					  (function-ref ident)))
				  (tds (html-td :class "size"
					  :align "right"
					  (format "~a% (~a)"
					     size%
					     size)))
				  (cell% (apply + (map car cells)))
				  (rest (list (- size% cell%)
					   "type-1"
					   (format "rest: ~a (~a%)"
					      size
					      (- size% cell%)))))
			      (list (html-row-gauge cells tdl tds rest))))))
		cell*)))
      (html-profile (apply append r)
	 class caption
	 '("functions" "20%")
	 '("memory" "15%")
	 "65%")))

;*---------------------------------------------------------------------*/
;*    bmem-function ...                                                */
;*---------------------------------------------------------------------*/
(define (bmem-function function css info gcmon funmon types)
   (let* ((fun (hashtable-get *functions* (string->symbol function)))
	  (fun* (list fun))
	  (allsize (apply + (map cadr (cdr gcmon))))
	  (nbtypes (+fx 1 (apply max (map car (cdr types)))))
	  (functions (get-functions))
	  (dcss (dynamic-css gcmon functions types))
	  (tvec (make-vector nbtypes)))
      (for-each (lambda (t)
		   (vector-set! tvec (car t) (cadr t)))
		(cdr types))
      (if (not (isa? fun funinfo))
	  #f
	  (let ((gc (html-verticalbox
		     (list
		      (make-function-gc-table fun* allsize nbtypes))))
		(ty (html-verticalbox
		     (list
		      (make-function-type-mem-table fun* allsize nbtypes
						    tvec)
		      (make-function-type-occ-table fun* allsize nbtypes
						    tvec)))))
	     (html-document :title (string-append "bmem (" function ")")
			    :style dcss
			    :css css
			    (list (html-h1 '("GCs")) gc (html-br)
				  (html-h1 '("Types")) ty))))))
