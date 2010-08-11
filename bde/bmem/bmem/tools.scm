;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bde/bmem/bmem/tools.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 20 09:11:09 2003                          */
;*    Last change :  Wed Aug 11 14:26:36 2010 (serrano)                */
;*    Copyright   :  2003-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Various facilities                                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmem_tools
   (import  html
	    bmem_param)
   (include "html.sch")
   (export  (word->mb ::int)
	    (word->kb::int ::int)
	    (mapv::pair-nil ::procedure ::vector)
	    (%::int ::int ::int)
	    (css-color::bstring ::int)
	    (html-row-gauge ::pair-nil ::obj ::obj)
	    (html-profile ::pair-nil ::bstring ::bstring ::pair ::pair . ::obj)
	    (html-legend ::int ::obj ::pair-nil ::obj ::obj)
	    (html-color-item ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    word->mb ...                                                     */
;*---------------------------------------------------------------------*/
(define (word->mb val)
   (/fl (exact->inexact (/fx (*fx 10 (word->kb val)) 1024)) 10.))

;*---------------------------------------------------------------------*/
;*    word->kb ...                                                     */
;*---------------------------------------------------------------------*/
(define (word->kb v)
   (/fx v (/fx 1024 *sizeof-word*)))

;*---------------------------------------------------------------------*/
;*    mapv ...                                                         */
;*---------------------------------------------------------------------*/
(define (mapv fun vec)
   (let ((l (vector-length vec)))
      (let loop ((i 0)
		 (res '()))
	 (if (=fx i l)
	     (reverse! res)
	     (loop (+fx i 1) (cons (fun (vector-ref vec i) i) res))))))

;*---------------------------------------------------------------------*/
;*    % ...                                                            */
;*---------------------------------------------------------------------*/
(define (% v1 v2)
   (let ((n (round (/ (* v1 100) v2))))
      (cond
	 ((inexact? n) (inexact->exact n))
	 ((bignum? n) (bignum->fixnum n))
	 (else n))))

;*---------------------------------------------------------------------*/
;*    Colors                                                           */
;*---------------------------------------------------------------------*/
(define *css-colors* '#("#ff0000"
			"#6a5acd"
			"#ffc125"
			"#008b00"
			"#ff00ff"
			"#007fff"
			"#00dd00"
			"#be55d7"
			"#ffba08"
			"#7924cf"
			"#51ff9e"
			"#ff3030"
			"#c71486"
			"#ff34b6"
			"#efd3ef"
			"#ff7d00"
			"#b6ef38"
			"#6959cf"
			"#d72096"
			"#ff8271"
			"#00ddd0"
			"#70dd00"))

;*---------------------------------------------------------------------*/
;*    css-color ...                                                    */
;*---------------------------------------------------------------------*/
(define (css-color i)
   (let ((r (remainderfx i (vector-length *css-colors*))))
      (vector-ref *css-colors* r)))
   
;*---------------------------------------------------------------------*/
;*    html-row-gauge ...                                               */
;*    -------------------------------------------------------------    */
;*    This function constructs a list of html TDs that span over       */
;*    100 cells.                                                       */
;*---------------------------------------------------------------------*/
(define (html-row-gauge cell*::pair-nil tdl tdr)
   (let ((cell* (filter (lambda (c) (> (car c) 0)) cell*))
	 (total (apply + (map car cell*))))
      (if (=fx total 0)
	  (html-tr (list tdl (html-td :class "empty" :colspan 100 " ") tdr))
	  (let loop ((cell* cell*)
		     (td* '())
		     (sum 0))
	     (if (null? cell*)
		 (html-tr
		  (cons tdl
			(if (=fx total 100)
			    (reverse! (cons tdr td*))
			    (reverse! (cons* tdr
					     (html-td :class "empty"
						      :colspan (-fx 100 total)
						      " ")
					     td*)))))
		 (let* ((cell (car cell*))
			(val (car cell))
			(id (cadr cell))
			(help (and (pair? (cddr cell)) (caddr cell)))
			(aval (if (pair? (cdr cell*))
				  val
				  (if (=fx (+fx sum val) total)
				      val
				      (-fx total sum)))))
		    (let ((td (html-td :id id :title help :colspan aval " ")))
		       (loop (cdr cell*) (cons td td*) (+fx sum aval)))))))))

;*---------------------------------------------------------------------*/
;*    html-profile ...                                                 */
;*---------------------------------------------------------------------*/
(define (html-profile row* class caption llegend rlegend . cwidth)
   (let* ((cwidth (if (pair? cwidth) (car cwidth) 6))
	  (colgroup (list (html-colgroup :width (cadr llegend))
			  (html-colgroup :span 100 :width cwidth)
			  (html-colgroup :width (cadr rlegend))))
	  (head (html-tr (list (html-th :align "center"
					:valign "bottom"
					(car llegend))
			       (html-th :colspan 100)
			       (html-th :align "center"
					:valign "bottom"
					(car rlegend)))))
	  (row* (if (pair? row*)
		    row*
		    (list (html-tr (list (html-td :colspan 102 "")))))))
      (html-div :class "profile"
		(html-table :class class
			    :caption caption
			    :width "100%"
			    :cellpadding "0"
			    :cellspacing "10"
			    :colgroup? colgroup
			    :thead? head
			    row*))))

;*---------------------------------------------------------------------*/
;*    html-legend ...                                                  */
;*---------------------------------------------------------------------*/
(define (html-legend nbcol width lst caption class)
   (let* ((len (length lst))
	  (m (/fx len nbcol))
	  (line* (let loop ((i 0)
			    (lst lst)
			    (r1 '())
			    (r2 '()))
		    (cond
		       ((=fx i m)
			(loop 0 lst '() (cons (reverse! r1) r2)))
		       ((null? lst)
			(if (null? r1)
			    (reverse! r2)
			    (loop (+fx i 1) lst (cons #f r1) r2)))
		       (else
			(loop (+fx i 1) (cdr lst) (cons (car lst) r1) r2)))))
	  (row* (apply map (lambda l
			      (html-tr
			       (apply append
				      (map (lambda (v)
					      (if v
						  (list (html-td :class "sample"
								 :id (car v)
								 " ")
							(html-td (cadr v)))
						  (list (html-td " ")
							(html-td " "))))
					   l))))
		       line*))
	  (cper (string-append
		 (integer->string (-fx (/fx 100 nbcol) 3))
		 "%"))
	  (colgroup (apply
		     append
		     (vector->list
		      (make-vector nbcol
				   (list (html-colgroup :width "10px")
					 (html-colgroup :width cper)))))))
      (html-div :class "profile"
		(html-div :class "legend"
			  (html-table :class class
				      :caption caption
				      :colgroup? colgroup
				      :cellpadding "2px"
				      :cellspacing "10px"
				      :width width
				      row*)))))
			 
;*---------------------------------------------------------------------*/
;*    html-color-item ...                                              */
;*---------------------------------------------------------------------*/
(define (html-color-item id legend)
   (html-td :class "color-item"
	    (html-table :class "color-item"
			:colgroup? (list (html-colgroup :width "10px")
					 (html-colgroup))
			(list (html-tr
			       (list (html-td :id id " ")
				     (html-td :class "legend"
					      :align "left"
					      legend)))))))
