;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/serv.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Mon Aug  4 06:50:46 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV net server example                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module libuv_server
   (library libuv pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let ((loop (uv-default-loop)))
      (let ((server (instantiate::UvTcp
		       (loop loop))))
	 (let ((b (uv-tcp-bind server "0.0.0.0" 7000)))
	    (if (=fx b 0)
		(begin
		   (print "waiting connection...")
		   (let ((r (uv-listen server 128 :callback on-connection)))
		      (unless (=fx r 0)
			 (print "listen error: " (uv-err-name r)))))
		(print "bind error: " (uv-err-name b)))))
      (uv-run loop)))

;*---------------------------------------------------------------------*/
;*    on-connection ...                                                */
;*---------------------------------------------------------------------*/
(define (on-connection server status)
   (print "connection established")
   (with-access::UvTcp server (loop)
      (unless (=fx status -1)
	 (let ((client (instantiate::UvTcp (loop loop))))
	    (if (=fx (uv-accept server client) 0)
		(uv-stream-read-start client :callback echo-read)
		(uv-close client))))))

;*---------------------------------------------------------------------*/
;*    echo-read ...                                                    */
;*---------------------------------------------------------------------*/
(define (echo-read buf offset len)
   (print buf))
      

