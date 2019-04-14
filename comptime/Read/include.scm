;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bigloo/comptime/Read/include.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 26 10:44:03 1994                          */
;*    Last change :  Sun Apr 14 06:52:22 2019 (serrano)                */
;*    Copyright   :  1994-2019 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    Read an include file                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_include
   (import  engine_param
	    engine_engine
	    tools_speek
	    tools_error
	    init_main)
   (export  (read-include ::bstring)
	    (read-directives ::bstring)))

;*---------------------------------------------------------------------*/
;*    read-include ...                                                 */
;*---------------------------------------------------------------------*/
(define (read-include file)
   (let ((cell (include-already-read file)))
      (if (not (pair? cell))
	  (begin
	     (let ((fname (find-file/path file *load-path*)))
		(verbose 2 "      [reading include file " (or fname file) "]"
			 #\Newline)
		(reader-reset!)
		(if (not (string? fname))
		    (user-error "read-include" "Can't find include file" file)
		    (let ((port (open-input-file fname)))
		       (if (not (input-port? port))
			   (user-error "read-include"
				       "Can't open such file"
				       file)
			   (unwind-protect
			      (let* ((first (read port #t))
				     (direc (if (and (pair? first)
						     (eq? (car first)
							  'directives))
						first
						'())))
				 (let loop ((aux (if (pair? direc)
						     (read port #t)
						     first))
					    (sexp '()))
				    (if (eof-object? aux)
					(let ((r (cons direc
						       (reverse! sexp))))
					   (mark-include-read! file r)
					   r)
					(loop (read port #t)
					      (cons aux sexp)))))
			      (close-input-port port)))))))
	  (if *include-multiple*
	      (cdr cell)
	      (cons '() '())))))

;*---------------------------------------------------------------------*/
;*    read-directives ...                                              */
;*---------------------------------------------------------------------*/
(define (read-directives file)
   (let ((fname (find-file/path file *load-path*)))
      (verbose 2 "      [reading directives include file " (or fname file) "]"
	       #\Newline)
      (reader-reset!)
      (if (not (string? fname))
	  (user-error 'read-directives "Can't find include file" file)
	  (let ((port (open-input-file fname)))
	     (if (not (input-port? port))
		 (user-error 'read-directives "Can't open such file" file)
		 (unwind-protect
		    (let ((first (read port #t)))
		       (if (and (pair? first) (eq? (car first) 'directives))
			   (cdr first)
			   '()))
		    (close-input-port port)))))))

;*---------------------------------------------------------------------*/
;*    include-already-read ...                                         */
;*---------------------------------------------------------------------*/
(define (include-already-read file)
   (assoc file *include-read*))

;*---------------------------------------------------------------------*/
;*    mark-include-read! ...                                           */
;*---------------------------------------------------------------------*/
(define (mark-include-read! file what)
   (set! *include-read* (cons (cons file what) *include-read*)))

;*---------------------------------------------------------------------*/
;*    *include-read* ...                                               */
;*---------------------------------------------------------------------*/
(define *include-read* '())
