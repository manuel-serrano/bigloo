;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/io.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Mon Jul 21 17:37:48 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV io example                                                 */
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
	 (path (make-file-name (pwd) "io.scm")))
      (print (synchronous-read path))
      (uv-async-send (instantiate::UvAsync
			(loop loop)
			(cb (lambda (a)
			       (asynchronous-read path loop)))))
      (uv-run loop)))

;*---------------------------------------------------------------------*/
;*    synchronous-read ...                                             */
;*---------------------------------------------------------------------*/
(define (synchronous-read path)
   (print "synchronous...")
   (let ((ip (uv-open-input-file path)))
      (unwind-protect
	 (read-string ip)
	 (close-input-port ip))))

;*---------------------------------------------------------------------*/
;*    asynchronous-read ...                                            */
;*---------------------------------------------------------------------*/
(define (asynchronous-read path loop)
   (print "asynchronous...")
   (uv-open-input-file path
      :callback (lambda (ip)
		   (when (input-port? ip)
		      (let ((buf (make-string 8192)))
			 (let while ()
			    (uv-fs-read ip buf 8192
			       (lambda (res)
				  (if (>fx res 0)
				      (begin
					 (display-substring buf 0 res
					    (current-output-port))
					 (while))
				      (begin
					 (close-input-port ip)
					 (uv-stop loop)))))))))))
      

