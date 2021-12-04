;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/loops.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Thu Oct 23 08:20:28 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV multiple loops                                             */
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
   (thread-start!
      (instantiate::pthread
	 (body (lambda ()
		  (loop "loop1" #u64:500)))))
   (thread-join!
      (thread-start-joinable!
	 (instantiate::pthread
	    (body (lambda ()
		     (loop "loop2" #u64:700)))))))

;*---------------------------------------------------------------------*/
;*    loop ...                                                         */
;*---------------------------------------------------------------------*/
(define (loop id delay)   
   (let ((loop (instantiate::UvLoop)))
      (letrec* ((n 5)
		(timer (instantiate::UvTimer
			  (loop loop)
			  (cb (lambda (t s)
				 (print id ": n=" n " T=" t)
				 (set! n (-fx n 1))
				 (when (=fx n 0)
				    (uv-timer-stop t)))))))
	 (uv-timer-start timer (*u64 #u64:2 delay) delay)
	 (uv-run loop))))
