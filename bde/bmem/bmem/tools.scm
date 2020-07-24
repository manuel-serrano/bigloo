;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/bde/bmem/bmem/tools.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Apr 20 09:11:09 2003                          */
;*    Last change :  Sat Jan 25 08:12:26 2020 (serrano)                */
;*    Copyright   :  2003-20 Manuel Serrano                            */
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
   (export  (word->mb ::llong)
	    (word->kb::llong ::llong)
	    (word->size::bstring ::llong)
	    (mapv::pair-nil ::procedure ::vector)
	    (%::int ::llong ::llong)
	    (%00::bstring ::llong ::llong)
	    (css-color::bstring ::int ::int ::int ::int)
	    (html-row-gauge ::pair-nil ::obj ::obj #!optional rest)
	    (html-profile ::pair-nil ::bstring ::bstring ::pair ::pair . ::obj)
	    (html-legend ::int ::obj ::pair-nil ::obj ::obj)
	    (html-color-item ::bstring ::obj)))

;*---------------------------------------------------------------------*/
;*    word->mb ...                                                     */
;*---------------------------------------------------------------------*/
(define (word->mb val)
   (/fl (exact->inexact (/llong (*llong #l100 (word->kb val)) 1024)) 100.))

;*---------------------------------------------------------------------*/
;*    word->kb ...                                                     */
;*---------------------------------------------------------------------*/
(define (word->kb v)
   (/llong v (/llong #l1024 *sizeof-word*)))

;*---------------------------------------------------------------------*/
;*    word->size ...                                                   */
;*---------------------------------------------------------------------*/
(define (word->size v)
   (if (>llong v (/llong (*llong #l1024 #l1024) *sizeof-word*))
       (format "~aMB" (word->mb v))
       (format "~aKB" (word->kb v))))
       
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
   (llong->fixnum (/llong (*llong v1 #l100) v2)))

;*---------------------------------------------------------------------*/
;*    %00 ...                                                          */
;*---------------------------------------------------------------------*/
(define (%00 v1 v2)
   (let* ((% (/llong (*llong v1 #l10000) v2))
	  (r (remainderllong % #l100)))
      (if (=llong r 0)
	  (format "~a" (/llong % #l100))
	  (format "~a.~2,0d" (/llong % #l100) r))))

;*---------------------------------------------------------------------*/
;*    hsv->rgb ...                                                     */
;*---------------------------------------------------------------------*/
(define (hsv->rgb h s v)
   (let ((r 0)
         (g 0)
         (b 0))
      (if (> s 0)
          (let* ((h/60 (/fl (fixnum->flonum h) 60.))
                 (fh/60 (floor h/60))
                 (hi (modulo (flonum->fixnum fh/60) 6))
                 (f (-fl h/60 fh/60))
                 (s/100 (/fl (fixnum->flonum s) 100.))
                 (v/100 (/fl (fixnum->flonum v) 100.))
                 (p (flonum->fixnum
                     (*fl 255. (*fl v/100 (-fl 1. s/100)))))
                 (q (flonum->fixnum
                     (*fl 255. (*fl v/100 (-fl 1. (*fl f s/100))))))
                 (t (flonum->fixnum
                     (*fl 255. (* v/100 (-fl 1. (*fl (-fl 1. f) s/100))))))
                 (v*255 (flonum->fixnum
                         (roundfl (*fl v/100 255.))))
                 (r 0)
                 (g 0)
                 (b 0))
             (case hi
                ((0) (set! r v*255) (set! g t) (set! b p))
                ((1) (set! r q) (set! g v*255) (set! b p))
                ((2) (set! r p) (set! g v*255) (set! b t))
                ((3) (set! r p) (set! g q) (set! b v*255))
                ((4) (set! r t) (set! g p) (set! b v*255))
                ((5) (set! r v*255) (set! g p) (set! b q)))
             (values r g b))
          (let ((v (flonum->fixnum
                    (roundfl (*fl (/fl (fixnum->flonum v) 100.) 255.)))))
             (values v v v)))))

;*---------------------------------------------------------------------*/
;*    h ...                                                            */
;*---------------------------------------------------------------------*/
(define (h max::double min::double r::double g::double b::double)
   (cond
      ((=fl max min)
       0)
      ((=fl max r)
       (modulofx
        (flonum->fixnum
         (roundfl (+fl (*fl 60. (/fl (-fl g b) (-fl max min))) 360.))) 360))
      ((=fl max g)
       (flonum->fixnum
        (roundfl (+fl (* 60. (/fl (-fl b r) (-fl max min))) 120.))))
      (else
       (flonum->fixnum
        (roundfl (+fl (*fl 60. (/fl (-fl r g) (-fl max min))) 240.))))))

;*---------------------------------------------------------------------*/
;*    rgb->hsv ...                                                     */
;*---------------------------------------------------------------------*/
(define (rgb->hsv r g b)
   (define (s max::double min::double r::double g::double b::double)
      (if (=fl max 0.)
          0
          (flonum->fixnum (roundfl (* 100 (/ (- max min) max))))))
   (let* ((r (/fl (fixnum->flonum r) 255.))
          (g (/fl (fixnum->flonum g) 255.))
          (b (/fl (fixnum->flonum b) 255.))
          (max (max r g b))
          (min (min r g b)))
      (values (h max min r g b)
              (s max min r g b)
              (flonum->fixnum (roundfl (* 100 max))))))

;*---------------------------------------------------------------------*/
;*    css-color ...                                                    */
;*---------------------------------------------------------------------*/
(define (css-color i r g b)
   (multiple-value-bind (h s v)
      (rgb->hsv r g b)
      (multiple-value-bind (r g b)
	 (hsv->rgb (+ h (*fx i 49)) s v)
	 (format "rgb(~a,~a,~a)" r g b))))
   
;*---------------------------------------------------------------------*/
;*    html-row-gauge ...                                               */
;*---------------------------------------------------------------------*/
(define (html-row-gauge cell*::pair-nil tdl tdr #!optional rest)
   (let* ((cell* (filter (lambda (c) (> (car c) 0)) cell*))
	  (cell* (sort (lambda (x y) (> (car x) (car y))) cell*))
	  (cell* (if rest (append cell* (list rest)) cell*))
	  (total (apply + (map car cell*))))
      (if (=fx total 0)
	  (html-tr (list tdl (html-td) tdr))
	  (let loop ((cell* cell*)
		     (els '())
		     (sum 0))
	     (if (null? cell*)
		 (html-tr
		    (list tdl
		       (html-td
			  (html-div :class "gauge"
			     (if (=fx total 100)
				 (reverse! els)
				 (reverse!
				    (cons (html-span :class "gauge-filler"
					     :style (format (format "width: ~a%"
							       (-fx 100 total)))
					     "&nbsp;")
				       els)))))
		       tdr))
		 (let* ((cell (car cell*))
			(val (car cell))
			(id (cadr cell))
			(help (and (pair? (cddr cell)) (caddr cell)))
			(aval (if (pair? (cdr cell*))
				  val
				  (if (=fx (+fx sum val) total)
				      val
				      (-fx total sum)))))
		    (let ((el (html-span :class (format "~a gauge-cell" id)
				 :title help
				 :style (format "width: ~a%" aval)
				 "&nbsp;")))
		       (loop (cdr cell*) (cons el els) (+fx sum aval)))))))))

;*---------------------------------------------------------------------*/
;*    html-profile ...                                                 */
;*---------------------------------------------------------------------*/
(define (html-profile row* class caption llegend rlegend . cwidth)
   (let* ((cwidth (if (pair? cwidth) (car cwidth) 6))
	  (colgroup (list (html-colgroup :width (cadr llegend))
			  (html-colgroup :width cwidth)
			  (html-colgroup :width (cadr rlegend))))
	  (head (html-tr (list (html-th :align "center"
					:valign "bottom"
					(car llegend))
			       (html-th "")
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
						(list (html-td :class
							 (format "sample ~a"
							    (car v))
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
		  (list (html-td :class id "&nbsp;")
		     (html-td :class "legend"
			:align "left"
			legend)))))))
