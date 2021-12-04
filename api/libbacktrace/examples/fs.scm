;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/fs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Thu Jul 24 07:58:45 2014 (serrano)                */
;*    Copyright   :  2014 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    LIBUV fs                                                         */
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
   (let ((loop (uv-default-loop)))

      (uv-fs-rename "FOO" "BAR"
	 :callback (lambda (res)
		      (tprint "rename..."
			 (if (<fx res 0) (uv-strerror res) "ok"))))
	 
      (uv-fs-readlink "GEE"
	 :callback (lambda (res)
		      (tprint "readlink..."
			 (if (integer? res)
			     (uv-strerror res)
			     res))))
	 
      (uv-run loop)))
      

