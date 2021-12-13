;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/io.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Wed Jul 23 16:17:08 2014 (serrano)                */
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
      (synchronous-read path)
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
   (let ((fd (uv-fs-open path 'r)))
      (unwind-protect
	 (let* ((buf (make-string (fsize fd)))
		(res (uv-fs-read fd buf (string-length buf))))
	    (display-substring buf 0 res (current-output-port)))
	 (uv-fs-close fd))))

;*---------------------------------------------------------------------*/
;*    asynchronous-read ...                                            */
;*---------------------------------------------------------------------*/
(define (asynchronous-read path loop)
   (print "asynchronous")
   (uv-fs-open path 'r
      :callback
      (lambda (fd)
	 (when (isa? fd UvFile)
	    (let ((buf (make-string 100)))
	       (let while ()
		  (uv-fs-read fd buf 100
		     :callback
		     (lambda (res)
			(if (>fx res 0)
			    (begin
			       (display-substring buf 0 res
				  (current-output-port))
			       (while))
			    (begin
			       (when (<fx res 0)
				  (print (uv-strerror res)))
			       (uv-fs-close fd)
			       (uv-stop loop)))))))))))

;*---------------------------------------------------------------------*/
;*    fsize ...                                                        */
;*---------------------------------------------------------------------*/
(define (fsize fd)
   (let* ((stat (uv-fs-fstat fd))
	  (size (assq 'size stat)))
      (int64->fixnum (cdr size))))
      

