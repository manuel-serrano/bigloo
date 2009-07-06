;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/man.scm                       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Feb 25 09:17:51 2007                          */
;*    Last change :  Tue Apr 24 08:41:32 2007 (serrano)                */
;*    Copyright   :  2007 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    man                                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bglpkg_man

   (library pkglib)

   (import  bglpkg_param)
   
   (export  (bglpkg-man ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    bglpkg-man ...                                                   */
;*---------------------------------------------------------------------*/
(define (bglpkg-man packages)
   (let loop ((packages packages))
      (when (pair? packages)
	 (cond
	    ((null? (cdr packages))
	     (package-man (car packages)))
	    ((package-version? (cadr packages))
	     (package-version-man (car packages) (cadr packages))
	     (loop (cddr packages)))
	    (else
	     (package-man (car packages))
	     (loop (cdr packages)))))))

;*---------------------------------------------------------------------*/
;*    package-man ...                                                  */
;*---------------------------------------------------------------------*/
(define (package-man name)
   (let ((dir (make-file-name (bglpkg-doc-directory) name)))
      (when (directory? dir)
	 (let ((ls (sort (directory->list dir) string<?)))
	    (when (pair? ls)
	       (package-version-man name (car ls)))))))

;*---------------------------------------------------------------------*/
;*    package-version-man ...                                          */
;*---------------------------------------------------------------------*/
(define (package-version-man name version)
   (let ((dir (make-file-path (bglpkg-doc-directory) name version "doc")))
      (when (directory? dir)
	 (let ((files (directory->list dir)))
	    (when (pair? files)
	       (man (make-file-name dir (car files))))))))

;*---------------------------------------------------------------------*/
;*    man ...                                                          */
;*---------------------------------------------------------------------*/
(define (man file)
   (cond
      ((string-suffix-ci? ".wiki" file)
       (man-wiki file))
      ((string-suffix-ci? ".html" file)
       (man-html file))
      (else
       (with-input-from-file file
	  (lambda ()
	     (display (read-string))
	     (newline))))))

;*---------------------------------------------------------------------*/
;*    *wiki-grammar* ...                                               */
;*---------------------------------------------------------------------*/
(define *wiki-grammar*
   (regular-grammar ()
      ((bol (: #\= (+ #\=)))
       (let* ((len (the-length))
	      (str (the-string))
	      (res (read-line (the-port)))
	      (color (case len
			((2) 'arg0)
			((3) 'arg1)
			((4) 'arg2)
			(else 'package))))
	  (display (pkglib-color color str))
	  (print (pkglib-color color res))
	  (ignore)))
      ((or "++" "**" "//" "~~")
       (ignore))
      ((+ (out "+*/~<,}=["))
       (display (the-string))
       (ignore))
      ((or #\= #\[)
       (display (the-character))
       (ignore))
      ((in "+*/~<,}")
       (display (the-string))
       (ignore))
      ((: #\< (: (out #\/) (+ (out #\< #\>))) #\>)
       (let* ((s (the-symbol))
	      (rest (read-line (the-port))))
	  (case s
	     ((<doc>)
	      (let ((s (if (and (>fx (string-length rest) 0)
				(char=? (string-ref rest 0) #\space))
			   (substring rest 1 (string-length rest))
			   rest)))
		 (print (pkglib-color 'message (string-upcase s)))))
	     ((<class> <record>)
	      (display s)
	      (print (pkglib-color 'record rest)))
	     ((<procedure>)
	      (display s)
	      (print (pkglib-color 'function rest)))
	     ((<variable>)
	      (display s)
	      (print (pkglib-color 'variable rest)))
	     ((<syntax>)
	      (display s)
	      (print (pkglib-color 'macr rest)))
	     (else
	      (display s)
	      (print rest)))
	  (ignore)))
      ((: ",(<" (+ (or #\- (in ("azAZ")))) ">)")
       (ignore))
      ((: ",(<" (+ (or #\- (in ("azAZ")))) ">" (? " {"))
       (ignore))
      ("})"
       (ignore))
      ((: #\< #\/ (+ (out #\>)) #\>)
       (ignore))
      ((: "[[" (+ (or (out #\]) (: #\] (out #\])))) "]]")
       (let* ((s (the-substring 2 -2))
	      (i (string-index s "|")))
	  (if i
	      (display (substring s (+fx i 1) (string-length s)))
	      (display s))
	  (ignore)))
      (else
       'done)))
       
;*---------------------------------------------------------------------*/
;*    man-wiki ...                                                     */
;*---------------------------------------------------------------------*/
(define (man-wiki file)
   (with-input-from-file file
      (lambda ()
	 (read/rp *wiki-grammar* (current-input-port)))))

;*---------------------------------------------------------------------*/
;*    *no-html* ...                                                    */
;*---------------------------------------------------------------------*/
(define *no-html*
   (regular-grammar ()
      ((uncase (: "<" (? #\/) "strong>"))
       (display "*")
       (ignore))
      ((uncase (: "<" (? #\/) "em"))
       (display "/")
       (ignore))
      ((uncase (: "<" (? #\/) "u>"))
       (display "_")
       (ignore))
      ((: "<" (+ (out #\>)) ">")
       (ignore))
      ((uncase "<li>")
       (display "  * ")
       (ignore))
      ((uncase "<h1>")
       (display "== ")
       (ignore))
      ((uncase "<h2>")
       (display "--- ")
       (ignore))
      ((uncase "<h3>")
       (display "---- ")
       (ignore))
      (else
       (let ((c (the-failure)))
	  (if (eof-object? c)
	      'done
	      (begin
		 (display c)
		 (ignore)))))))
	      
;*---------------------------------------------------------------------*/
;*    man-html ...                                                     */
;*---------------------------------------------------------------------*/
(define (man-html file)
   (with-input-from-file file
      (lambda ()
	 (read/rp *no-html* (current-input-port)))))
