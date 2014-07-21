;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/io.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Mon Jul 21 09:50:45 2014 (serrano)                */
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
   (let ((loop (uv-default-loop)))
      (print (synchronous-read (make-file-name (pwd) "io.scm")))))

;*---------------------------------------------------------------------*/
;*    synchronous-read ...                                             */
;*---------------------------------------------------------------------*/
(define (synchronous-read path)
   (let ((ip (uv-open-input-file path)))
      (unwind-protect
	 (read-string ip)
	 (close-input-port ip))))
      

