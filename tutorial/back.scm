;*=====================================================================*/
;*    serrano/prgm/project/bigloo/tutorial/back.scm                    */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 30 14:13:29 1997                          */
;*    Last change :  Wed Jan 27 09:29:53 1999 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The back cover                                                   */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module back
   (include "tex.sch")
   (import  tex
	    tutorial)
   (export  (back-cover title author editor musicians songs)))

;*---------------------------------------------------------------------*/
;*    back-cover ...                                                   */
;*---------------------------------------------------------------------*/
(define (back-cover title author editor musicians songs)
   (tex-environment
    "center"
    ""
    (tex-pspicture
     0 0
     *back-height*
     (+ *back-width* (* 2 *side-width*))
     ;; the global dashed frame
     (tex-psframe "[linestyle=dashed,dash=3pt 2pt]"
		  0
		  0
		  *back-height*
		  (+ (* 2 *side-width*) *back-width*))
     (tex-psline  "[linestyle=dashed,dash=3pt 2pt]"
		  0
		  *side-width*
		  *back-height*
		  *side-width*)
     (tex-psline  "[linestyle=dashed,dash=3pt 2pt]"
		  0
		  (+ *back-width* *side-width*)
		  *back-height*
		  (+ *back-width* *side-width*))
     ;; the left side
     (print "%% left side")
     (side-cover "[t]{180}" *side-margin* title author editor)
     ;; the right side
     (print "%% right side")
     (side-cover "[b]"
		  (+ *side-margin* *side-width* *back-width*)
		  title author editor)
     ;; the back frame
     (tex-psframe "[linewidth=0cm,linestyle=dashed,dash=0pt 5pt,fillstyle=solid,fillcolor=CDgray]"
		  *back-margin*
		  (+ *side-width* *back-margin*)
		  (- *back-height* *back-margin*)
		  (+ *side-width* (- *back-width* *back-margin*)))
     ;; the back title and author
     (print "%% the title and author")
     (tex-rput "[t]{90}"
	       *back-margin*
	       (+ *side-width* *back-margin* (/ *back-width* 2))
	       (print "\\parbox{" (- *back-width* (* 4 *back-margin*)) "cm}{")
	       (print "\\center")
	       (print "{\\huge\\textbf{\\textit{" author "}}}\\\\")
	       (print "{\\large\\textbf{\\textsf{" title "}}}")
	       (print "\\ \\\\")
	       (print "\\ \\\\")
	       (print "\\ \\\\")
	       ;; all the songs
	       (print "%% the songs")
	       (let laap ((songs songs)
			  (disc  (if (pair? (cdr songs)) 1 #unspecified)))
		  (if (pair? songs)
		      (if (number? disc)
			  (begin
			     (print "{\\underline{\\em disc " disc "}}\\\\")
			     (print-songs (car songs))
			     (print "\\ \\\\")
			     (laap (cdr songs) (+ disc 1)))
			  (print-songs (car songs)))))
	       (print "}")))))

;*---------------------------------------------------------------------*/
;*    print-songs ...                                                  */
;*---------------------------------------------------------------------*/
(define (print-songs songs)
   (let loop ((songs songs)
	      (num   1))
      (if (pair? songs)
	  (let ((title  (car (car songs)))
		(author (cadr (car songs)))
		(duration (caddr (car songs))))
	     (printf "%2d. " num)
	     (display title)
	     (if (string? author)
		 (display* " {\\it\\small (" author ")}"))
	     (if (string? duration)
		 (display* " {\\small [" duration "]}"))
	     (print "\\\\")
	     (loop (cdr songs) (+fx 1 num))))))
   
;*---------------------------------------------------------------------*/
;*    side-cover ...                                                   */
;*---------------------------------------------------------------------*/
(define (side-cover side position title author editor)
   (tex-rput side
	     (/ *back-height* 2)
	     position
	     (print "\\parbox{" (- *back-height* (* 5 *side-margin*)) "cm}{%")
	     (print "\\center")
	     (print "\\textbf{\\textit{" author "}}")
	     (print "\\hfill ")
	     (print "\\textbf{\\textsf{" title "}}")
	     (print "\\hfill ")
	     (print "\\small{\\sc{" editor "}}")
	     (print "}")))
   
		  
     
    


