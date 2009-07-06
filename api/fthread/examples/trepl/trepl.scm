;*=====================================================================*/
;*    .../prgm/project/bigloo/api/fthread/examples/trepl/trepl.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Feb  4 14:28:58 2002                          */
;*    Last change :  Mon Oct 24 07:42:40 2005 (serrano)                */
;*    Copyright   :  2002-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A test module that deploys the examples of SRFI18.               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module trepl
   (library fthread)
   (main main))

(define (fair-read port)
   (thread-await!
    (make-asynchronous-signal
     (lambda (s) (read port)))))

(define (main argv)
   (thread-start!
    (make-thread
     (lambda ()
	(let loop ()
	   (display "?* ")
	   (flush-output-port (current-output-port))
	   (let ((exp (fair-read (current-input-port))))
	      (if (not (eof-object? exp))
		  (begin
		     (print (eval exp))
		     (thread-yield!)
		     (loop))))))))
   (scheduler-start!))
