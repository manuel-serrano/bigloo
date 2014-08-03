;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/idle.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Thu Jul 31 13:52:31 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV idle                                                      */
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
		(idle (instantiate::UvIdle
			  (loop loop)
			  (cb (lambda (t)
				 (tprint "n=" n " T=" t)
				 (set! n (-fx n 1))
				 (when (=fx n 0)
				    (uv-idle-stop t)))))))
	 (uv-idle-start idle)
	 (uv-run loop))))
