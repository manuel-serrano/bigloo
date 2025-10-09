;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/recette/utils.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Oct  9 14:54:33 2025                          */
;*    Last change :  Thu Oct  9 14:57:41 2025 (serrano)                */
;*    Copyright   :  2025 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Test utils                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module utils
   (export (do-test name thunk good?)
	   (test-module name file)
	   *nb-test*
	   *nb-err*
	   *recette-port*
	   *bigloo-path*
	   *silent*
	   *verbose*))

;*---------------------------------------------------------------------*/
;*    Global variables                                                 */
;*---------------------------------------------------------------------*/
(define *nb-test* 0)
(define *nb-err* 0)
(define *test-number* 0)
(define *silent* #t)
(define *verbose* #f)
(define *recette-port* #f)
(define *bigloo-path*
   (if (member (os-class) '("win32" "mingw"))
       "..\\bin\\bigloo.exe"
       "../bin/bigloo.sh"))

;*---------------------------------------------------------------------*/
;*    static variables                                                 */
;*---------------------------------------------------------------------*/
(define *module-name* "")

;*---------------------------------------------------------------------*/
;*    do-test ...                                                      */
;*---------------------------------------------------------------------*/
(define (do-test name thunk wanted)
   (set! *test-number* (+ 1 *test-number*))
   (set! *nb-test* (+ 1 *nb-test*))
   (define (correct? result wanted)
      (or (equal? result wanted)
	  (and (flonum? result)
	       (flonum? wanted)
	       (<fl (absfl (-fl result wanted)) 0.00001))))
   (let ((err (cons 1 2)))
      (define (test-fail result)
	 (set! *nb-err* (+ 1 *nb-err*))
	 (display* *test-number* #\. *module-name* "(" name ") ")
	 (if (eq? result err)
	     (display "abort: ")
	     (begin
		(display* "fail: ")
		(display "provided [" )
		(write-circle result)
		(display "], ")))
	 (display "wanted [")
	 (write-circle wanted)
	 (print "]"))
      (bind-exit (esc)
	 (with-exception-handler
	    (lambda (e)
	       (newline (current-error-port))
	       (test-fail err)
	       (flush-output-port (current-output-port))
	       (flush-output-port (current-error-port))
	       (error-notify e)
	       (newline (current-error-port))
	       (esc #f))
	    (lambda ()
	       (let ((result (thunk)))
		  (if (correct? result wanted)
		      (begin
			 (if (not *silent*)
			     (begin
				(display* *test-number* #\. *module-name* " : "
				   name " --> ")
				(display "ok.")))
			 (if *verbose*
			     (begin
				(display " [")
				(write-circle result)
				(print "]"))
			     (if (not *silent*)
				 (newline))))
		      (test-fail result))))))))

;*---------------------------------------------------------------------*/
;*    test-module ...                                                  */
;*---------------------------------------------------------------------*/
(define (test-module module-name file-name)
   (set! *module-name* module-name)
   (set! *test-number* 0)
   (if (not *silent*)
       (newline))
   (print file-name ":"))


   
