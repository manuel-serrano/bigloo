;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Write/version.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Mar 12 14:03:51 1995                          */
;*    Last change :  Tue Jan 31 16:01:02 2006 (serrano)                */
;*    Copyright   :  1995-2006 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    La version de Bigloo.                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module write_version 
   (import engine_param
	   tools_speek)
   (export (version)
	   (short-version)
	   (revision)))

;*---------------------------------------------------------------------*/
;*    revision ...                                                     */
;*---------------------------------------------------------------------*/
(define (revision)
   (print *bigloo-version*))

;*---------------------------------------------------------------------*/
;*    short-version ...                                                */
;*---------------------------------------------------------------------*/
(define (short-version)
   (print *bigloo-name*))

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define (version)
   (display-to-column "" 79 #\-)
   (newline)
   (horse *bigloo-name*
	  "`a practical Scheme compiler'"
	  (if (char=? (string-ref *bigloo-date* 0) #\space)
	      (substring *bigloo-date* 1 (string-length *bigloo-date*))
	      *bigloo-date*)
	  *bigloo-author*
	  (string-append "email: " *bigloo-email*)
	  (string-append "url: " *bigloo-url*))
   (when (>=fx *verbose* 3)
      (display-to-column " " 78 #\-)
      (newline)
      (verbose 3 " Ce travail est dedié à Nelly Lebas, ma grand-mère, décédée le 10 Mars 1995," #\Newline)
      (verbose 3 " à Marcel Lebas, mon grand-père, décédé le 29 Octobre 1998" #\Newline)
      (verbose 3 " à Carlos, mon père décédé, le 12 Mars 2001 au matin," #\Newline)
      (verbose 3 " et à Pierrette, ma mère, décédée le 31 Janvier 2006 à 10h30." #\Newline))
   (display-to-column "" 79 #\-)
   (newline)
   (newline))

;*---------------------------------------------------------------------*/
;*    horse ...                                                        */
;*---------------------------------------------------------------------*/
(define (horse . l)
   (let loop ((l     l)
	      (horse '("            ,--^, "
		       "      _ ___/ /|/  "
		       "  ,;'( )__, ) '   "
		       " ;;  //   L__.    " 
		       " '   \\    /  '    "
		       "      ^   ^       ")))
      (cond
	 ((null? l)
	  (if (null? horse)
	      'done
	      (begin
		 (display-to-column "" 62 #\space)
		 (print (car horse))
		 (loop '() (cdr horse)))))
	 ((null? horse)
	  (print (car l))
	  (loop (cdr l) '()))
	 (else
	  (display-to-column (car l) 62 #\space)
	  (print (car horse))
	  (loop (cdr l) (cdr horse))))))

;*---------------------------------------------------------------------*/
;*    display-to-column ...                                            */
;*---------------------------------------------------------------------*/
(define (display-to-column string column char)
   (display string)
   (let loop ((l (+fx 1 (string-length string))))
      (if (=fx l column)
	  'done
	  (begin
	     (write-char char)
	     (loop (+fx l 1))))))   
      
