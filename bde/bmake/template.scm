;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bee/bmake/template.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jun  9 08:55:00 1998                          */
;*    Last change :  Mon Aug  3 13:21:33 1998 (serrano)                */
;*    -------------------------------------------------------------    */
;*    The default Makefile for application                             */
;*=====================================================================*/
 
;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bmake_template
   (export *application-template*
	   *library-template*))

;*---------------------------------------------------------------------*/
;*    make-template ...                                                */
;*---------------------------------------------------------------------*/
(define-macro (make-template file)
   (let ((port (open-input-file file)))
      (let loop ((line (read-line port))
		 (lines '()))
	 (if (eof-object? line)
	     (apply string-append (reverse! lines))
	     (loop (read-line port)
		   (cons line (cons #"\n" lines)))))))

(define *application-template* (make-template "bmake/makefile.appli"))
(define *library-template* (make-template "bmake/makefile.library"))
