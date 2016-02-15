;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/src.scm                */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Dec 26 10:44:03 1994                          */
;*    Last change :  Wed Feb 10 11:16:07 2016 (serrano)                */
;*    Copyright   :  1994-2016 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    We read the source file                                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module read_src
   (import  read_reader
	    engine_param
	    engine_engine
 	    init_main)
   (export  (read-src)))

;*---------------------------------------------------------------------*/
;*    read-src ...                                                     */
;*    -------------------------------------------------------------    */
;*    If the first source file start by an expression like             */
;*    #!...                                                            */
;*    we must evaluate the file rather than compile it. Hence,         */
;*    read_src returns `#f' to the compiler which means that the       */
;*    compiler will stop and jump to the interpreter.                  */
;*---------------------------------------------------------------------*/
(define (read-src)
   (assert (*src-files*) (pair? *src-files*))
   ;; we read the first source file (which must be containing the
   ;; module clause).
   (let ((mod (read-module-clause)))
      (if (not (pair? mod))
	  mod
	  (let loop ((src (read-src/port))
		     (sfiles (cdr *src-files*)))
	     (if (null? sfiles)
		 (cons mod (reverse! src))
		 (loop (append (read-src-file (car sfiles)) src)
		       (cdr sfiles)))))))

;*---------------------------------------------------------------------*/
;*    read-handler ...                                                 */
;*---------------------------------------------------------------------*/
(define (read-handler e)
   (if (isa? e &error)
       (begin
	  ;; before producing the error, we display the
	  ;; source file name
	  (hello-world)
	  ;; then we display the error.
	  (error-notify e)
	  (close-src-port)
	  (compiler-exit 3))
       (raise e)))

;*---------------------------------------------------------------------*/
;*    read-module-clause ...                                           */
;*---------------------------------------------------------------------*/
(define (read-module-clause)
   (open-src-file (car *src-files*))
   (let ((mod (with-exception-handler
		 read-handler
		 (lambda ()
		    (compiler-read-src *port* #t)))))
      (if *bigloo-interpreter*
	  ;; the `*bigloo-interpreter* variable is setted by the
	  ;; reader when it seees a #!... expression
	  #f
	  mod)))

;*---------------------------------------------------------------------*/
;*    read-src/port ...                                                */
;*---------------------------------------------------------------------*/
(define (read-src/port)
   (let ((port *port*))
      (with-exception-handler
	 read-handler
	 (lambda ()
	    (let loop ((r (compiler-read-src port #t))
		       (acc '()))
	       (if (eof-object? r)
		   (begin
		      (close-src-port)
		      acc)
		   (loop (compiler-read-src port #t)
			 (cons r acc))))))))

;*---------------------------------------------------------------------*/
;*    read-src-file ...                                                */
;*---------------------------------------------------------------------*/
(define (read-src-file sfile)
   (open-src-file sfile)
   (read-src/port))

;*---------------------------------------------------------------------*/
;*    *port* ...                                                       */
;*---------------------------------------------------------------------*/
(define *port* #f)
		  
;*---------------------------------------------------------------------*/
;*    open-src-file ...                                                */
;*---------------------------------------------------------------------*/
(define (open-src-file sfile)
   (reader-reset!)
   (set! *port*
	 (if (string? sfile)
	     (let ((found (find-file/path sfile *load-path*)))
		(if found
		    (let ((port (open-input-file found)))
		       (if (input-port? port)
			   (begin
			      (reader-reset!)
			      port)
			   (error 'src-file->memory
				  "Can't open such file"
				  found)))
		    (error 'src-file->memory
			   "Can't find such file"
			   sfile)))
	     (current-input-port))))

;*---------------------------------------------------------------------*/
;*    close-src-port ...                                               */
;*---------------------------------------------------------------------*/
(define (close-src-port)
   (if (and (input-port? *port*) (not (eq? *port* (current-input-port))))
       (close-input-port *port*)))

   
