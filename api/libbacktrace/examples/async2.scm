;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/async2.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Thu Oct 23 10:56:43 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV async example                                              */
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
   (let* ((loop (instantiate::UvLoop))
	  (async (instantiate::UvAsync
		    (loop loop)
		    (cb (lambda (a)
			   (tprint "async")
			   (let ((timer (instantiate::UvTimer
					   (loop loop)
					   (cb (lambda (t c)
						  (tprint "timer"))))))
			      (uv-timer-start timer #u64:100 #u64:500)))))))
      (uv-async-send async)
      (thread-join!
	 (thread-start-joinable!
	    (instantiate::pthread
	       (body (lambda ()
			(display "Looping forever.\n")
			(uv-run loop))))))))
