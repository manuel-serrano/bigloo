;<font size="-3"><pre>
;*=====================================================================*/
;*    serrano/prgm/project/bigloo/examples/Process/process.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  SERRANO Manuel                                    */
;*    Creation    :  Thu Mar  6 09:57:09 1997                          */
;*    Last change :  Thu Mar  6 10:06:04 1997 (serrano)                */
;*    -------------------------------------------------------------    */
;*    Forking and waiting Unix processes                               */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module sys-example
   (extern (include "sys/types.h")
	   (include "wait.h")
	   (include "unistd.h")
	   (type pid int "pid_t")
	   (type int* (pointer int) "int *")

	   (macro sleep::uint (uint) "sleep")
	   (macro wait::pid (int*) "wait")
	   (macro fork::pid () "fork")))

;*---------------------------------------------------------------------*/
;*    fib ...                                                          */
;*---------------------------------------------------------------------*/
(define (fib x)
   (if (< x 2)
       1
       (+fx (fib (-fx x 1)) (fib (-fx x 2)))))
 
;*---------------------------------------------------------------------*/
;*    A top-level form                                                 */
;*---------------------------------------------------------------------*/
(let ((pid (fork)))
   (if (= pid 0)
       ;; the child process
       (begin
	  (print "child process, started...")
	  (sleep 5)
	  (print "child done."))
       ;; the parent process
       (begin
	  (print "parent process, waiting...")
	  (let ((res (fib 30)))
	     (let* ((status::int* (make-int* 0))
		    (pid::pid (wait status)))
		(if (< pid 0)
		    (error "***ERROR: "
			   "Illegal process termination"
			   pid)
		    (print "parent: " res)))))))
;</pre></font>

