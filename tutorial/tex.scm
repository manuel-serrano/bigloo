;*=====================================================================*/
;*    serrano/prgm/utils/cdda/scm/tex.scm                              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 28 22:27:07 1997                          */
;*    Last change :  Sat Jan 24 09:40:44 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The tex tools                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tex
   (export (tex-prelude)
	   (tex-psprelude)
	   (tex-postlude)
	   (tex-psline   ::bstring ::obj ::obj ::obj ::obj)
	   (tex-psframe  ::bstring ::obj ::obj ::obj ::obj)
	   (tex-psframe* ::bstring ::obj ::obj ::obj ::obj)))

;*---------------------------------------------------------------------*/
;*    tex-prelude ...                                                  */
;*---------------------------------------------------------------------*/
(define (tex-prelude)
   (print *tex-prelude*))

;*---------------------------------------------------------------------*/
;*    tex-psprelude ...                                                */
;*---------------------------------------------------------------------*/
(define (tex-psprelude)
   (print *tex-psprelude*))

;*---------------------------------------------------------------------*/
;*    tex-postlude ...                                                 */
;*---------------------------------------------------------------------*/
(define (tex-postlude)
   (print *tex-postlude*))

;*---------------------------------------------------------------------*/
;*    tex-psline ...                                                   */
;*---------------------------------------------------------------------*/
(define (tex-psline options min-x min-y max-x max-y)
   (print "\\psline" options "(" min-x "," min-y ")(" max-x "," max-y ")"))

;*---------------------------------------------------------------------*/
;*    tex-psframe ...                                                  */
;*---------------------------------------------------------------------*/
(define (tex-psframe options min-x min-y max-x max-y)
   (print "\\psframe" options "(" min-x "," min-y ")(" max-x "," max-y ")"))

;*---------------------------------------------------------------------*/
;*    tex-psframe* ...                                                 */
;*---------------------------------------------------------------------*/
(define (tex-psframe* options min-x min-y max-x max-y)
   (print "\\psframe*" options "(" min-x "," min-y ")(" max-x "," max-y ")"))

;*---------------------------------------------------------------------*/
;*    tex code                                                         */
;*---------------------------------------------------------------------*/
(define *tex-prelude*    
   "\\documentclass[10pt]{article}
\\usepackage[T1]{fontenc}
\\usepackage[french]{babel}
\\usepackage{epsf,fancybox}
\\usepackage{a4wide,fullpage}
\\usepackage{pstricks,pst-node,pst-coil}

\\pagestyle{empty}
")
   

(define *tex-postlude*
   "")

(define *tex-psprelude*
   "\\psset{unit=1cm}
\\psset{linewidth=.01cm}
\\psset{linecolor=black}
\\newgray{CDgray}{0.97}
")



