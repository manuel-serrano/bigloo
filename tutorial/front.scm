;*=====================================================================*/
;*    serrano/prgm/project/bigloo/tutorial/front.scm                   */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 30 14:10:21 1997                          */
;*    Last change :  Wed Jan 27 09:30:05 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The front cover                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module front
   (include "tex.sch")
   (import  tex
	    tutorial)
   (export  (front-cover title author editor notes musicians songs)))

;*---------------------------------------------------------------------*/
;*    front-cover ...                                                  */
;*---------------------------------------------------------------------*/
(define (front-cover title author editor notes musicians songs)
   (print "%% front cover")
   (tex-environment
    "center"
    ""
    (tex-pspicture
     0 0
     *height* (* 2 *front-width*)
     ;; the dashed enclosing frame
     (tex-psframe "[linestyle=dashed,dash=3pt 2pt]"
		  0
		  0
		  *height*
		  (* 2 *front-width*))
     (tex-psline "[linestyle=dashed,dash=3pt 2pt]"
		  0
		  *front-width*
		  *height*
		  *front-width*)
     ;; the outter frame
     (tex-psframe ""
		  *front-margin*
		  (+ *front-margin* *height*)
		  (- *height* *front-margin*)
		  (+ *front-width* (- *front-width* *front-margin*)))
     ;; the title frame
     (tex-psframe "[linewidth=.05cm,fillstyle=solid,fillcolor=CDgray]"
		  (+ *front-margin* *double-frame-size*)
		  (+ *front-margin* *double-frame-size* *front-width*)
		  (+ *front-margin* *front-title-height*)
		  (+ (- *front-width* *front-margin* *double-frame-size*)
		     *front-width*))
     ;; the song frame
     (tex-psframe "[linewidth=.05cm,fillstyle=solid,fillcolor=CDgray]"
		  (+ *front-margin* *front-title-height* *double-frame-size*
		     *title-song-space*)
		  (+ *front-margin* *double-frame-size* *front-width*)
		  (- *height* *front-margin* *double-frame-size*)
		  (+ (- *front-width* *front-margin* *double-frame-size*)
		     *front-width*))
     ;; the title and the author
     (tex-rput "[t]{90}"
	       (+ (* 2 *front-margin*) *double-frame-size*)
	       (+ *front-margin* *double-frame-size*
		  (/ *front-width* 2) *front-width*)
	       (print "\\parbox{"
		      (- *front-width*
			 (* 2 *double-frame-size*)
			 (* 2 *front-margin*))
		      "cm}{")
	       (print "\\center")
	       (print "{\\huge\\textbf{\\textit{" author "}}}\\\\")
	       (print "{\\large\\textbf{\\textsf{" title "}}}}"))
     ;; the editor
     (tex-rput "[br]{90}"
	       (- (+ *front-margin* *front-title-height*)
		  (* 2 *double-frame-size*))
	       (+ (- *front-width* *front-margin* *double-frame-size*)
		  *front-width*)
	       (print "{\\small{\\sc{" editor "}}}"))
     (let* ((x      (+ (* 2 *front-margin*) *front-title-height*
		       *double-frame-size* *title-song-space*))
	    (y      (+ *front-margin* *double-frame-size*
		       (/ *front-width* 2) *front-width*))
	    (bwidth (- *front-width*
		       (* 2 *double-frame-size*)
		       (* 2 *front-margin*)))
	    (twidth (- *front-width*
		       (* 2 *double-frame-size*)
		       (* 6 *front-margin*)))
	    (col1   (* 0.85 twidth))
	    (col2   (* 0.1 twidth)))
	;; the notes, musicians and song
	(tex-rput "[t]{90}"
		  x
		  y
		  (print "\\parbox{" bwidth "cm}{")
		  (print "\\begin{tabular}{p{" col1 "cm}p{" col2 "cm}}\\hline")
		  ;; the musicians
		  (let loop ((musicians musicians))
		     (if (pair? musicians)
			 (begin
			    (display "\\multicolumn{2}{c}{")
			    (display (car (car musicians)))
			    (if (not (eq? (cdr (car musicians)) #unspecified))
				(display* " ("
					  (cdr (car musicians))
					  ")"))
			    (print "}\\\\")
			    (loop (cdr musicians)))
			 (print "\\hline")))
		  (if (pair? notes)
		      ;; the note
		      (begin
			 (for-each (lambda (note)
				      (display "\\multicolumn{2}{c}")
				      (display "{\\small ")
				      (display note)
				      (print "}\\\\"))
				   notes)
			 (print "\\hline")))
		  ;; the songs
		  (cond 
		     ((<= (+ (apply + (map length songs)) (length musicians))
			  *max-songs*)
		      ;; one song per line
		      (print "%% one song per line")
		      (let laap ((songs songs))
			 (if (pair? songs)
			     (begin
				(print-songs-one-per-line (car songs))
				(if (pair? (cdr songs))
				    (begin
				       (print "\\hline")
				       (laap (cdr songs))))))))
		     ((<= (+ (apply + (map length songs)) (length musicians))
			  (*fx 2 *max-songs*))
		      ;; all song merged
		      (print "%% all song on the same line")
		      (let laap ((songs songs))
			 (if (pair? songs)
			     (begin
				(print "\\multicolumn{2}{p{" twidth "cm}}{")
				(print-songs-all-merged (car songs))
				(if (pair? (cdr songs))
				    (begin
				       (print "\\hline")
				       (laap (cdr songs))))))))
		     (else
		      (print "}\\\\")))
		  (print "\\end{tabular}}"))))))
	    
;*---------------------------------------------------------------------*/
;*    print-songs-one-per-line ...                                     */
;*---------------------------------------------------------------------*/
(define (print-songs-one-per-line songs)
   (let loop ((songs songs)
	      (num   1))
      (if (pair? songs)
	  (let ((title  (car (car songs)))
		(author (cadr (car songs)))
		(duration (caddr (car songs))))
	     (printf "%#2d. " num)
	     (display title)
	     (if (string? author) (display* " {\\it\\small (" author ")}"))
	     (display " & ")
	     (if (string? duration) (display* "{\\small " duration "}"))
	     (print "\\\\")
	     (loop (cdr songs) (+fx 1 num))))))

;*---------------------------------------------------------------------*/
;*    print-songs-all-merged ...                                       */
;*---------------------------------------------------------------------*/
(define (print-songs-all-merged songs)
   (let loop ((songs songs)
	      (num   1))
      (if (pair? songs)
	  (let ((title  (car (car songs)))
		(author (cadr (car songs)))
		(duration (caddr (car songs))))
	     (printf "%2d. " num)
	     (display title)
	     (if (string? author) (display* " {\\it\\small (" author ")}"))
	     (display " ")
	     (if (string? duration) (display* "{\\small " duration "}"))
	     (if (pair? (cdr songs)) (print " -- "))
	     (loop (cdr songs) (+fx 1 num)))
	  (print "}\\\\"))))
