;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/type.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 20 10:48:45 2003                          */
;*    Last change :  Tue Nov 15 21:34:34 2011 (serrano)                */
;*    Copyright   :  2003-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Visualizing allocation classified by types                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem_type
   (include "html.sch")
   (import  bmem_function
	    bmem_tools
	    html
	    bmem)
   (export  make-type-vector
	    (make-type-tables ::pair-nil ::pair-nil ::pair-nil)
	    (bmem-type ::bstring ::obj ::obj
		       ::pair-nil ::pair-nil ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    make-type-vector ...                                             */
;*---------------------------------------------------------------------*/
(define make-type-vector
   (let ((v (vector)))
      (lambda (nb-types)
	 (if (>fx nb-types (vector-length v))
	     (begin
		(set! v (make-vector nb-types 0))
		v)
	     (begin
		(vector-fill! v 0)
		v)))))

;*---------------------------------------------------------------------*/
;*    make-type-tables ...                                             */
;*---------------------------------------------------------------------*/
(define (make-type-tables gcmon fun* types)
   (let* ((types (cdr types))
	  (gc* (cdr gcmon))
	  (allsize (apply + (map cadr gc*)))
	  (nbtypes (+fx 1 (apply max (map car types))))
	  (tvec (make-type-vector nbtypes)))
      ;; mark all types according to information found in the funinfo structs
      (for-each (lambda (fi)
		   (with-access::funinfo fi (dtype)
		      (for-each (lambda (gc)
				   (for-each (lambda (ti)
						(let* ((t (car ti))
						       (n (caddr ti))
						       (o (vector-ref tvec t)))
						   (vector-set! tvec t
								(+fx o n))))
					     (cdr gc)))
				dtype)))
		fun*)
      ;; sort the types according to the allocation size
      (let ((stypes (sort (filter (lambda (t) (>fx (cdr t) 0))
				  (mapv (lambda (t i)
					   (cons i t))
					tvec))
			  (lambda (t1 t2) (>fx (cdr t1) (cdr t2)))))
	    (tvec (make-type-vector nbtypes)))
	 (for-each (lambda (t)
		      (vector-set! tvec (car t) (cadr t)))
		   types)
	 (let ((type* (map (lambda (t)
			      (list (car t) (cdr t) (vector-ref tvec (car t))))
			   stypes)))
	    (html-table
	     :width "100%"
	     `(,(html-tr
		 `(,(html-td
		     :valign "top"
		     (make-type-function-chart allsize type* gc* fun*))
		   ,(html-td
		     :valign "top"
		     (make-type-gc-chart allsize type* gc* fun*))))))))))

;*---------------------------------------------------------------------*/
;*    make-type-function-chart ...                                     */
;*---------------------------------------------------------------------*/
(define (make-type-function-chart allsize type* gc* fun*)
   (define (type->cell type)
      (let* ((tnum (car type))
	     (tvalfl (exact->inexact (cadr type)))
	     (allsizefl (exact->inexact allsize))
	     (operfl (/fl tvalfl allsizefl)))
	 (map (lambda (f)
		 (with-access::funinfo f (num ident)
		    (let* ((size (funinfo-type-size f tnum))
			   (sizefl (exact->inexact size))
			   (rperfl (/fl sizefl tvalfl))
			   (col (inexact->exact (* 100. operfl rperfl))))
		       (list col
			  (string-append "function"
			     (integer->string num))
			  (format "~a: ~a (~a%)"
			     (function-ident-pp ident)
			     (word->size size)
			     (inexact->exact (*fl 100. rperfl)))))))
	      fun*)))
   (let* ((type* (filter (lambda (ty)
			    (let* ((size (cadr ty))
				   (size% (% size allsize)))
			       (> size% 0)))
			 type*))
	  (cell* (map type->cell type*))
	  (row* (map (lambda (ty cells)
			(let* ((size (cadr ty))
			       (size% (% size allsize))
			       (tnum (integer->string (car ty)))
			       (id (string-append "type" tnum))
			       (tdl (html-color-item
				     id
				     (type-ref (caddr ty))))
			       (tds (html-td :class "size"
					     :align "left"
					     (format "~a% (~a)"
						     size%
						     (word->size size)))))
			   (list (html-row-gauge cells tdl tds)
				 (html-tr (list (html-td :colspan 102 "&nbsp;"))))))
		     type* cell*)))
      (html-profile (apply append row*)
		    "type-function" "Type (functions)"
		    '("type" "15%")
		    '("memory" "20%"))))

;*---------------------------------------------------------------------*/
;*    make-type-gc-chart ...                                           */
;*---------------------------------------------------------------------*/
(define (make-type-gc-chart allsize type* gc* fun*)
   (define (type->cell type)
      (let* ((tnum (car type))
	     (allsizefl (exact->inexact allsize))
	     (tsize 0))
	 (for-each (lambda (f)
		      (set! tsize (+fx tsize (funinfo-type-size f tnum))))
		   fun*)
	 (map (lambda (gc)
		 (let* ((n (car gc))
			(sn (integer->string (+fx 1 n)))
			(size 0))
		    (for-each (lambda (f)
				 (let ((s (funinfo-gc-type-size f tnum n)))
				    (set! size (+fx size s))))
			      fun*)
		    (cond
		       ((or (=fx tsize 0) (=fx size 0))
			(list 0
			      (string-append "gc" sn)
			      (format "gc ~a (0%)" sn)))
		       (else
			(let* ((rper (% size tsize))
			       (tsizefl (exact->inexact tsize))
			       (sizefl (exact->inexact size))
			       (rperfl (/fl sizefl tsizefl))
			       (operfl (/fl tsizefl allsizefl))
			       (col (inexact->exact (* 100. operfl rperfl))))
			   (list col
				 (string-append "gc" sn)
				 (format "gc ~a (~a%)"
					 sn
					 (inexact->exact
					  (*fl 100. rperfl)))))))))
	      gc*)))
   (let* ((type* (filter (lambda (ty)
			    (let* ((size (cadr ty))
				   (size% (% size allsize)))
			       (> size% 0)))
			 type*))
	  (cell* (map type->cell type*))
	  (row* (map (lambda (ty cells)
			(let* ((size (cadr ty))
			       (size% (% size allsize))
			       (tnum (integer->string (car ty)))
			       (id (string-append "type" tnum))
			       (tdl (html-color-item
				     id
				     (type-ref (caddr ty))))
			       (tds (html-td :class "size"
					     :align "left"
					     (format "~a% (~a)"
						     size%
						     (word->size size)))))
			   (list (html-row-gauge cells tdl tds)
				 (html-tr (list (html-td :colspan 102 "&nbsp;"))))))
		     type* cell*)))
      (html-profile (apply append row*)
		    "type-function" "Type (gcs)"
		    '("type" "10%")
		    '("memory" "20%"))))

;*---------------------------------------------------------------------*/
;*    find-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (find-type type type*)
   (cond
      ((null? type*)
       #f)
      ((string=? type (cadr (car type*)))
       (car type*))
      (else
       (find-type type (cdr type*)))))

;*---------------------------------------------------------------------*/
;*    bmem-type ...                                                    */
;*---------------------------------------------------------------------*/
(define (bmem-type type css info gcmon funmon types)
   (tprint "bmem-type : " (get-functions))
   (let* ((types (cdr types))
	  (gc* (cdr gcmon))
	  (fun* (get-functions))
	  (asize (apply + (map cadr (cdr gcmon))))
	  (dcss (dynamic-css gcmon fun* types))
	  (nbtypes (+fx 1 (apply max (map car types))))
	  (tvec (make-type-vector nbtypes))
	  (the-type (find-type type types)))
      ;; mark all types according to information
      ;; found in the funinfo structs
      (for-each (lambda (fi)
		   (with-access::funinfo fi (dtype)
		      (for-each (lambda (gc)
				   (for-each (lambda (ti)
						(let* ((t (car ti))
						       (n (caddr ti))
						       (o (vector-ref tvec t)))
						   (vector-set! tvec t
								(+fx o n))))
					     (cdr gc)))
				dtype)))
		fun*)
      (if the-type
	  (let ((type* (list (list (car the-type)
				   (vector-ref tvec (car the-type))
				   (cadr the-type)))))
	     (if (not (pair? type*))
		 #f
		 (let* ((gchart (make-type-gc-chart asize type* gc* fun*))
			(fchart (make-type-function-chart asize type* gc* fun*))
			(gc (html-table
			     :width "100%"
			     `(,(html-tr `(,(html-td gchart))))))
			(fu (html-table
			     :width "100%"
			     `(,(html-tr `(,(html-td fchart)))))))
		    (html-document :title (string-append "bmem (" type ")")
				   :style dcss
				   :css css
				   (list (html-h1 '("GCs")) gc (html-br)
					 (html-h1 '("Functions")) fu)))))
	  #f)))
