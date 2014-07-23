;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/libuv/examples/fs.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue May  6 12:10:13 2014                          */
;*    Last change :  Wed Jul 23 08:53:15 2014 (serrano)                */
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
	 :callback (lambda (res path)
		      (tprint "res=" res " path=" path)))
      
      (display "Looping forever.\n")
      (uv-run loop)))
      

