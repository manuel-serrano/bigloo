;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/atimer.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Thu Oct 16 09:02:16 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV timer                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module libuv_hello
   (library libuv pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let ((loop (uv-default-loop))
	 (cv (make-condition-variable))
	 (mutex (make-mutex))
	 (timer #f))
      
      (let ((async (instantiate::UvAsync
		      (loop loop)
		      (cb (lambda (a)
			     (when timer
				(uv-timer-start timer #u64:100 #u64:50)
				(set! timer #f)))))))
	 
	 (define (start-timer msg n)
	    (let ((n n))
	       (set! timer
		  (instantiate::UvTimer
		     (loop loop)
		     (cb (lambda (t s)
			    (tprint msg "." n " T=" t)
			    (set! n (-fx n 1))
			    (when (=fx n 0)
			       (uv-timer-stop t))))))
	       (uv-async-send async)))
	 
	 (thread-start!
	    (instantiate::pthread
	       (body (lambda ()
			(sleep 1000000)
			(start-timer "FOO" 10)
			(sleep 2000000)
			(start-timer "BAR" 5)
			(sleep 50000)
			(start-timer "GEE" 3)))))
	 (display "Looping forever.\n")
	 (uv-run loop))))
      

