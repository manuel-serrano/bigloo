;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/flac.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct 29 21:10:13 2016                          */
;*    Last change :  Sat Jun 24 09:49:18 2017 (serrano)                */
;*    Copyright   :  2016-17 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Flac utils                                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-flac
   (export (flac-file-offset ::bstring)))

;*---------------------------------------------------------------------*/
;*    flac-file-offset ...                                             */
;*    -------------------------------------------------------------    */
;*    Returns the offset in the FILE where the true flac starts.       */
;*---------------------------------------------------------------------*/
(define (flac-file-offset file)
   (let ((mm (open-mmap file :write #f)))
      (if (not (mmap? mm))
	  (raise (instantiate::&error
		    (proc "flac-file-offset")
		    (msg "cannot open file for input")
		    (obj file)))
	  (let ((kt (bm-table "fLaC")))
	     (unwind-protect
		(bm-mmap kt mm 0)
		(close-mmap mm))))))
			   
