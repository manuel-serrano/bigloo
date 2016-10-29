;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/multimedia/src/Llib/flac.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Oct 29 21:10:13 2016                          */
;*    Last change :  Sat Oct 29 21:12:12 2016 (serrano)                */
;*    Copyright   :  2016 Manuel Serrano                               */
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
   (let ((mm (open-mmap file)))
      (if (not (mmap? mm))
	  (raise (instantiate::&error
		    (proc "flac-file-offset")
		    (msg "cannot open file for input")
		    (obj file)))
	  (let ((kt (bm-table "fLaC")))
	     (bm-mmap kt mm 0)))))
			   
