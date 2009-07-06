;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/interp.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May 31 08:03:39 1996                          */
;*    Last change :  Thu Mar 26 07:06:59 2009 (serrano)                */
;*    Copyright   :  1996-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The inner interpreter.                                           */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_interp
   (import engine_param
	   tools_speek)
   (export (interp version verbose file startup path args)))

;*---------------------------------------------------------------------*/
;*    interp ...                                                       */
;*---------------------------------------------------------------------*/
(define (interp version verb files startup path args)
   (set! *bigloo-interpreter* #t)
   (if (and (>=fx verb 0) (not (pair? files)))
       (begin
	  (version)
	  (verbose 1 "Welcome to the interpreter"))
       (verbose 2 "Interpreting files: " files #\Newline))
   ;; on definie la variable `(command-line)'
   (set! *the-command-line* (interp-parse-args args))
   ;; on charge le fichier de startup
   (if (string? startup)
       (let ((path (let ((home (getenv "HOME")))
		      (if (string? home)
			  (cons home path)
			  path))))
	  (let ((fstartup (find-file/path startup path)))
	     (if fstartup
		 (loadq startup)
		 (warning "interp"
			  #\Newline
			  "Can't file startup file -- " startup)))))
   ;; on rentre dans l'interprete
   (if (pair? files)
       (for-each (lambda (f) (if (string? f) (loadi f) (load-stdin))) files)
       (repl))
   0)

;*---------------------------------------------------------------------*/
;*    loadi ...                                                        */
;*---------------------------------------------------------------------*/
(define (loadi f)
   (let ((afile (make-file-name (dirname f) *access-file-default*)))
      (when (file-exists? afile)
	 (verbose 2 "      [reading afile " afile "]" #\Newline)
	 (module-load-access-file afile))
      (loadq f)))

;*---------------------------------------------------------------------*/
;*    load-stdin ...                                                   */
;*---------------------------------------------------------------------*/
(define (load-stdin)
   (let loop ((e (read)))
      (if (not (eof-object? e))
	  (begin
	     (eval e)
	     (loop (read))))))

;*---------------------------------------------------------------------*/
;*    interp-parse-args ...                                            */
;*---------------------------------------------------------------------*/
(define (interp-parse-args args)
   (let loop ((args (cdr args))
	      (res  '()))
      (cond
	 ((null? args)
	  (append *src-files* (reverse res)))
	 ((string=? (car args) "-i")
	  (loop (cdr args) res))
	 ((member (car args) *src-files*)
	  (loop (cdr args) res))
	 (else
	  (loop (cdr args) (cons (car args) res))))))
			   

