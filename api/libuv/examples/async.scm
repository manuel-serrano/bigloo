;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/async.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Wed May 14 07:27:24 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV hello world                                                */
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
   (let* ((loop (uv-default-loop))
	  (async (instantiate::UvAsync
		    (loop loop)
		    (cb (lambda (a) (tprint "async"))))))
      (display "Looping forever.\n")
      (uv-async-send async)
      (uv-run loop)))
