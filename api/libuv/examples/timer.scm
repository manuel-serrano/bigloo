;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/timer.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Tue May  6 16:09:13 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV timer                                                      */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module libuv_hello
   (library libuv)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let ((loop (uv-default-loop)))
      (letrec* ((n 5)
		(timer (instantiate::UvTimer
			  (loop loop)
			  (cb (lambda (t s)
				 (tprint "n=" n " T=" t)
				 (set! n (-fx n 1))
				 (when (=fx n 0)
				    (uv-timer-stop t)))))))
	 (uv-timer-start timer #u64:1000 #u64:2000)
	 (uv-run loop))))
