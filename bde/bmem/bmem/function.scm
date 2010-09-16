;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/function.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 20 09:06:40 2003                          */
;*    Last change :  Wed Aug 11 14:27:58 2010 (serrano)                */
;*    Copyright   :  2003-10 Manuel Serrano                            */
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
	       (dsize::int read-only)
	       (isize::int read-only)
	       (gc*::pair-nil read-only)
	       (dtype::pair-nil read-only)
	       (itype::pair-nil read-only))
	    
	    (get-functions::pair-nil)
	    (add-function! ::pair)
	    (funinfo-find-gc ::funinfo ::int)
	    (funinfo-type-size ::funinfo ::int)
	    (funinfo-gc-type-size ::funinfo ::int ::int)
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
   (assq num (funinfo-gc* fi)))

;*---------------------------------------------------------------------*/
;*    funinfo-type-size ...                                            */
;*---------------------------------------------------------------------*/
(define (funinfo-type-size fi num)
   (with-access::funinfo fi (dtype)
      (let ((s 0))
	 (for-each (lambda (gc)
		      (let ((c (assq num (cdr gc))))
			 (if (pair? c)
			     (set! s (+fx s (caddr c))))))
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
		    0))
	     0))))

;*---------------------------------------------------------------------*/
;*    make-function-tables ...                                         */
;*---------------------------------------------------------------------*/
(define (make-function-tables gcmon fun* types)
   (let* ((allsize (apply + (map cadr (cdr gcmon))))
	  (allsum (let ((sum 0))
		     (for-each (lambda (f)
				  (define (sum-type ti)
				     (set! sum (+fx sum (cadr ti))))
				  (for-each (lambda (gc)
					       (for-each sum-type (cdr gc)))
					    (funinfo-dtype f)))
			       fun*)
		     sum))
	  (nbtypes (+fx 1 (apply max (map car (cdr types)))))
	  (tvec (make-vector nbtypes)))
      (for-each (lambda (t)
		   (vector-set! tvec (car t) (cadr t)))
		(cdr types))
      (html-verticalbox
       `(,(make-function-gc-table fun* allsize nbtypes)
	 ,(make-function-type-mem-table fun* allsize nbtypes tvec)
	 ,(make-function-type-occ-table fun* allsum nbtypes tvec)))))

;*---------------------------------------------------------------------*/
;*    make-function-gc-table ...                                       */
;*---------------------------------------------------------------------*/
(define (make-function-gc-table fun* allsize nbtypes)
   (html-table
    :width "100%"
    `(,(html-tr
	`(,(html-td
	    :valign "top"
	    (make-function-gc-chart allsize fun*
				    funinfo-dsize cadr
				    "function-gc-direct"
				    "Direct allocations (gc)"))
	  ,(html-td
	    :valign "top"
	    (make-function-gc-chart allsize fun*
				    (lambda (f)
				       (+fx (funinfo-isize f)
					    (funinfo-dsize f)))
				    (lambda (g)
				       (+fx (cadr g) (caddr g)))
				    "function-gc-cumulative"
				    "Cumulative allocations (gc)")))))))

;*---------------------------------------------------------------------*/
;*    make-function-gc-chart ...                                       */
;*---------------------------------------------------------------------*/
(define (make-function-gc-chart allsize::int fun*::pair-nil
				funsize::procedure fungcsize::procedure
				class::bstring caption::bstring)
   (let* ((fun* (filter (lambda (f)
			   (let* ((size (funsize f))
				  (size% (% size allsize)))
			      (> size% 0)))
			fun*))
	  (fun* (sort fun*
		      (lambda (f1 f2)
			 (>fx (funsize f1) (funsize f2)))))
	  (cell* (map (lambda (f)
			 ;; mark the function as used (for the legend)
			 (funinfo-use-set! f #t)
			 (let ((fsize (funsize f)))
			    (map (lambda (gc)
				    (let* ((n (integer->string
					       (+fx 1 (car gc))))
					   (gcsize (fungcsize gc))
					   (rsize (% gcsize fsize))
					   (per (% gcsize allsize))
					   (id (string-append "gc" n)))
				       (list per
					     id 
					     (format "gc ~a: ~ak (~a%)"
						     n
						     (word->kb gcsize)
						     rsize))))
				 (funinfo-gc* f))))
		      fun*))
	  (row* (map (lambda (f cells)
			(let* ((size (funsize f))
			       (size% (% size allsize))
			       (id (string-append "function"
						  (integer->string
						   (funinfo-num f))))
			       (tdl (html-color-item
				     id
				     (function-ref (funinfo-ident f))))
			       (tds (html-td :class "size"
					     :align "left"
					     (format "~a% (~ak)"
						     size%
						     (word->kb size)))))
			   (list (html-row-gauge cells tdl tds)
				 (html-tr (list (html-td :colspan 102 ""))))))
		     fun* cell*)))
      (html-profile (apply append row*)
		    class caption
		    '("functions" "20%")
		    '("memory" "15%"))))

;*---------------------------------------------------------------------*/
;*    make-function-type-mem-table ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-mem-table fun* allsize nbtypes tvec)
   (html-table
    :width "100%"
    `(,(html-tr
	`(,(html-td
	    :valign "top"
	    (make-function-type-mem-chart allsize fun*
					  funinfo-dsize
					  funinfo-dtype
					  "function-type-direct"
					  "Direct allocations (size)"
					  nbtypes
					  tvec))
	  ,(html-td
	    :valign "top"
	    (make-function-type-mem-chart allsize fun*
					  (lambda (f)
					     (+fx (funinfo-isize f)
						  (funinfo-dsize f)))
					  (lambda (f)
					     (map (lambda (i d)
						     (append i (cdr d)))
						  (funinfo-itype f)
						  (funinfo-dtype f)))
					  "function-type-indirect"
					  "Indirect allocations (size)"
					  nbtypes
					  tvec)))))))
   
;*---------------------------------------------------------------------*/
;*    make-function-type-occ-table ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-occ-table fun* allsize nbtypes tvec)
   (html-table
    :width "100%"
    `(,(html-tr
	`(,(html-td
	    :valign "top"
	    (make-function-type-occ-chart allsize fun*
					  funinfo-dsize
					  funinfo-dtype
					  "function-type-direct"
					  "Direct allocations (occurrence)"
					  nbtypes
					  tvec))
	  ,(html-td
	    :valign "top"
	    (make-function-type-occ-chart allsize fun*
					  (lambda (f)
					     (+fx (funinfo-isize f)
						  (funinfo-dsize f)))
					  (lambda (f)
					     (map (lambda (i d)
						     (append i (cdr d)))
						  (funinfo-itype f)
						  (funinfo-dtype f)))
					  "function-type-indirect"
					  "Indirect allocations (occurrence)"
					  nbtypes
					  tvec)))))))
   
;*---------------------------------------------------------------------*/
;*    make-function-type-mem-chart ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-mem-chart allsize::int fun*::pair-nil
				      funsize::procedure funtypes::procedure
				      class::bstring caption::bstring
				      nb-types::int tvecnames::vector)
   (define (fun->cell f)
      ;; mark the function used (for the legend)
      (funinfo-use-set! f #t)
      (let* ((size (funsize f))
	     (size% (exact->inexact (% size allsize)))
	     (tvec (make-type-vector nb-types))
	     (at 0)
	     (sum 0))
	 (define (mark-type! ti)
	    (let ((t (car ti))
		  (n (caddr ti)))
	       (set! sum (+fx sum n))
	       (vector-set! tvec t (+fx (vector-ref tvec t) n))
	       (set! at (+fx at n))))
	 ;; mark all allocated types
	 (for-each (lambda (gc) (for-each mark-type! (cdr gc))) (funtypes f))
	 (set! at (exact->inexact at))
	 ;; construct the cells
	 (mapv (if (= at 0)
		   (lambda (t i)
		      (list 0 "type0" "0 (0%)"))
		   (lambda (t i)
		      (let ((tr (/fl (exact->inexact t) at))
			    (tp (if (>fx sum 0)
				    (% t sum)
				    0)))
			 (list (inexact->exact (* tr size%))
			       (string-append "type"
					      (integer->string i))
			       (format "~a: ~ak (~a%)" 
				       (vector-ref tvecnames i)
				       (word->kb t)
				       tp)))))
	       tvec)))
   (let* ((fun* (filter (lambda (f)
			   (let* ((size (funsize f))
				  (size% (% size allsize)))
			      (>fx size% 0)))
			fun*))
	  (fun* (sort fun*
		      (lambda (f1 f2)
			 (>fx (funsize f1) (funsize f2)))))
	  (cell* (map fun->cell fun*))
	  (r (map (lambda (f cells)
		     (let* ((size (funsize f))
			    (size% (% size allsize))
			    (id (string-append "function"
						  (integer->string
						   (funinfo-num f))))
			    (tdl (html-color-item
				  id
				  (function-ref (funinfo-ident f))))
			    (tds (html-td :class "size"
					  :align "left"
					  (format "~a% (~ak)"
						  size%
						  (word->kb size)))))
			(list (html-row-gauge cells tdl tds)
			      (html-tr (list (html-td :colspan 102 ""))))))
		  fun* cell*)))
      (html-profile (apply append r)
		    class caption
		    '("functions" "20%")
		    '("memory" "15%"))))

;*---------------------------------------------------------------------*/
;*    make-function-type-occ-chart ...                                 */
;*---------------------------------------------------------------------*/
(define (make-function-type-occ-chart allsize::int fun*::pair-nil
				      funsize::procedure funtypes::procedure
				      class::bstring caption::bstring
				      nb-types::int tvecnames::vector)
   (define (fun->cell f)
      ;; mark the function used (for the legend)
      (funinfo-use-set! f #t)
      (let* ((tvec (make-type-vector nb-types))
	     (at 0)
	     (sum 0))
	 (define (mark-type! ti)
	    (let ((t (car ti))
		  (n (cadr ti)))
	       (set! sum (+fx sum n))
	       (vector-set! tvec t (+fx (vector-ref tvec t) n))
	       (set! at (+fx at n))))
	 ;; mark all allocated types
	 (for-each (lambda (gc) (for-each mark-type! (cdr gc))) (funtypes f))
	 (set! at (exact->inexact at))
	 (let ((size% (exact->inexact (% sum allsize))))
	    ;; construct the cells
	    (list f
		  sum
		  (mapv (if (= at 0)
			    (lambda (t i)
			       (list 0 "type0" "0 (0%)"))
			    (lambda (t i)
			       (let ((tr (/fl (exact->inexact t) at))
				     (tp (if (>fx sum 0)
					     (% t sum)
					     0)))
				  (list (inexact->exact (* tr size%))
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
			  (>fx (cadr c1) (cadr c2)))))
	  (r (map (lambda (cells)
		     (let* ((f (car cells))
			    (size (cadr cells))
			    (cells (caddr cells))
			    (size% (% size allsize))
			    (id (string-append "function"
						  (integer->string
						   (funinfo-num f))))
			    (tdl (html-color-item
				  id
				  (function-ref (funinfo-ident f))))
			    (tds (html-td :class "size"
					  :align "left"
					  (format "~a% (~a)"
						  size%
						  size))))
			(list (html-row-gauge cells tdl tds)
			      (html-tr (list (html-td :colspan 102 ""))))))
		  cell*)))
      (html-profile (apply append r)
		    class caption
		    '("functions" "20%")
		    '("memory" "15%"))))

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
      (if (not (funinfo? fun))
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
