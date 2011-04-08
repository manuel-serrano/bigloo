;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/misc.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Dec 29 16:54:25 1994                          */
;*    Last change :  Tue Apr  5 09:39:36 2011 (serrano)                */
;*    Copyright   :  1994-2011 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Various general tools                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tools_misc
   (export (string*->string::bstring ::pair-nil)
	   (replace! p1 p2)
	   (string-split-char::pair-nil str::bstring separator::bchar)
	   (epairify ::pair ::obj)
	   (epairify-rec ::obj ::obj)
	   (epairify-propagate ::obj ::obj)
	   (epairify-propagate-loc ::obj ::obj)
	   (epairify* def . srcs)
	   (build-path-from-shell-variable::pair-nil ::bstring)
	   (uncygdrive::bstring ::bstring)))
   
;*---------------------------------------------------------------------*/
;*    string*->string ...                                              */
;*---------------------------------------------------------------------*/
(define (string*->string l)
   (let loop ((l (reverse l))
	      (r ""))
      (if (null? l)
	  r
	  (loop (cdr l)
		(string-append (car l) " " r)))))

;*---------------------------------------------------------------------*/
;*    replace! ...                                                     */
;*---------------------------------------------------------------------*/
(define (replace! p1 p2)
   (if (and (pair? p1) (pair? p2) (not (epair? p2)))
       (begin
	  (set-car! p1 (car p2))
	  (set-cdr! p1 (cdr p2))
	  p1)
       p2))

;*---------------------------------------------------------------------*/
;*    string-split-char ...                                            */
;*---------------------------------------------------------------------*/
(define (string-split-char str separator)
   (let ((str-length (string-length str)))
      (if (zero? str-length)
	  '()
	  (let loop ((i 0))
	     (cond ((>= i str-length)
		    (list str))
		   ((char=? (string-ref str i) separator)
		    (if (zero? i)
			(string-split-char
			 (substring str 1 str-length) separator)
			(cons (substring str 0 i)
			      (string-split-char
			       (substring str (+ i 1) str-length)
			       separator))))
		   (else
		    (loop (+ i 1))))))))

;*---------------------------------------------------------------------*/
;*    epairify ...                                                     */
;*    -------------------------------------------------------------    */
;*    If the struct definition was an extended pair (that is if we     */
;*    were tracking the source location of the structure), we          */
;*    propagate inside the generated function, the define-struct       */
;*    location.                                                        */
;*---------------------------------------------------------------------*/
(define (epairify pair::pair epair)
   (if (epair? epair)
       (econs (car pair) (cdr pair) (cer epair))
       pair))

;*---------------------------------------------------------------------*/
;*    epairify-rec ...                                                 */
;*    -------------------------------------------------------------    */
;*    Epairfy all the elements of a list.                              */
;*---------------------------------------------------------------------*/
(define (epairify-rec p ep)
   (cond
      ((not (pair? p))
       p)
      ((not (epair? ep))
       p)
      ((epair? p)
       p)
      (else
       (econs (epairify-rec (car p) (car ep))
	      (epairify-rec (cdr p) (cdr ep))
	      (cer ep)))))

;*---------------------------------------------------------------------*/
;*    epairify-propagate ...                                           */
;*    -------------------------------------------------------------    */
;*    Epairfy all the elements of a list.                              */
;*---------------------------------------------------------------------*/
(define (epairify-propagate p ep)
   (if (not (epair? ep))
       p
       (epairify-propagate-loc p (cer ep))))

;*---------------------------------------------------------------------*/
;*    epairify-propagate-loc ...                                       */
;*    -------------------------------------------------------------    */
;*    Epairfy all the elements of a list.                              */
;*---------------------------------------------------------------------*/
(define (epairify-propagate-loc p loc)
   (let loop ((p p))
      (cond
	 ((not (pair? p))
	  p)
	 ((epair? p)
	  p)
	 (else
	  (econs (loop (car p)) (loop (cdr p)) loc)))))

;*---------------------------------------------------------------------*/
;*    epairify* ...                                                    */
;*---------------------------------------------------------------------*/
(define (epairify* def . srcs)
   (let loop ((srcs srcs))
      (cond
	 ((null? srcs)
	  def)
	 ((epair? (car srcs))
	  (econs (car def) (cdr def) (cer (car srcs))))
	 (else
	  (loop (cdr srcs))))))

;*---------------------------------------------------------------------*/
;*    build-path-from-shell-variable ...                               */
;*---------------------------------------------------------------------*/
(define (build-path-from-shell-variable::pair-nil var::bstring)
   (if (string=? (os-class) "mingw")
       (mingw-build-path-from-shell-variable var)
       (unix-build-path-from-shell-variable var)))
       
;*---------------------------------------------------------------------*/
;*    mingw-build-path-from-shell-variable ...                         */
;*---------------------------------------------------------------------*/
(define (mingw-build-path-from-shell-variable::pair-nil var::bstring)
   (define (normalize-mingw-path path)
      (let ((n (string-length path))
	    (s ""))
	 (if (< n 3)
	     path
	     (if (char=? (string-ref path 0) #\/)
		 (if (char=? (string-ref path 2) #\/)
		     (string-append (string (string-ref path 1))
				    ":/"
				    (substring path 3 n))
		     path)
		 path))))
   (let ((val (getenv var))
	 (res '()))
      (if (not (string? val))
	  '()
	  (let ((n (string-length val))
		(k 0))
	     (define (splitnow? i)
		(if (char=? (string-ref val i) #\:)
		    (if (< i (- n 1))
			(not (or 
			      (char=? (string-ref val (+ i 1)) #\/)
			      (char=? (string-ref val (+ i 1)) #\\)))
			#f)
		    #f))
	     (do ((i 0 (+ i 1)))
		 ((>= i n) (map normalize-mingw-path
				(reverse (cons (substring val k i) res))))
		 (if (splitnow? i)
		     (begin 
			(set! res (cons (substring val k i) res))
			(set! k (+ i 1)))))))))

;*---------------------------------------------------------------------*/
;*    unix-build-path-from-shell-variable ...                          */
;*---------------------------------------------------------------------*/
(define (unix-build-path-from-shell-variable::pair-nil var::bstring)
   (let ((val (getenv var)))
      (if (string? val)
	  (string-case val
	     ((+ (out #\:))
	      (let* ((str (the-string))
		     (res (ignore)))
		 (cons str res)))
	     (#\:
	      (ignore))
	     (else
	      '()))
	  '())))

;*---------------------------------------------------------------------*/
;*    uncygdrive ...                                                   */
;*    -------------------------------------------------------------    */
;*    This function is used on the Win32 port to get rid of the fake   */
;*    directory introduced by Cygwin.                                  */
;*---------------------------------------------------------------------*/
(define (uncygdrive::bstring str::bstring)
   (if (substring=? "/cygdrive/" str 10)
       (if (and (>fx (string-length str) 12)
		(char-alphabetic? (string-ref str 10))
		(char=? (string-ref str 11) #\/))
	   (string-append (string (string-ref str 10) #\: #\/)
			  (substring str 12 (string-length str)))
	   str)
       str))
       

