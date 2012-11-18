;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/text/src/Llib/gb2312.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 29 13:56:53 2009                          */
;*    Last change :  Sun Nov 18 15:09:20 2012 (serrano)                */
;*    Copyright   :  2009-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Guojia Biaozhun Chinese characters (aka cp936)                   */
;*    see http://en.wikipedia.org/wiki/GB_2312                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __text_gb2312

   (export (gb2312->ucs2::ucs2string ::bstring)))

;*---------------------------------------------------------------------*/
;*    gb2312-lock ...                                                  */
;*---------------------------------------------------------------------*/
(define gb2312-lock (make-mutex))

;*---------------------------------------------------------------------*/
;*    gb2312-table1 ...                                                */
;*---------------------------------------------------------------------*/
(define gb2312-table #f)

;*---------------------------------------------------------------------*/
;*    load-gb2312-tables! ...                                          */
;*---------------------------------------------------------------------*/
(define (load-gb2312-tables!)
   (synchronize gb2312-lock
      (unless gb2312-table
	 (let* ((name (make-file-path (bigloo-config 'library-directory)
			 "text"
			 "data"
			 "gb2312.sch"))
		(p (open-input-file name)))
	    (if (input-port? p)
		(unwind-protect
		   (set! gb2312-table (read p))
		   (close-input-port p))
		(error "gb2312" "file missing" name))))))
      
;*---------------------------------------------------------------------*/
;*    gb2312->ucs2 ...                                                 */
;*---------------------------------------------------------------------*/
(define (gb2312->ucs2 str)
   (load-gb2312-tables!)
   (let* ((len (string-length str))
	  (res (make-ucs2-string len)))
      (let loop ((i 0)
		 (j 0))
	 (if (=fx i len)
	     (subucs2-string res 0 j)
	     (let ((c (char->integer (string-ref str i))))
		(cond
		   ((<fx c #x80)
		    (ucs2-string-set! res j (integer->ucs2 c))
		    (loop (+fx i 1) (+fx j 1)))
		   ((=fx c #x80)
		    (ucs2-string-set! res j (integer->ucs2 #x20AC))
		    (loop (+fx i 1) (+fx j 1)))
		   (else
		    (let ((n (+fx (*fx 256 c)
				  (char->integer (string-ref str (+fx i 1))))))
		       (if (>=fx n #x8140)
			   (let ((u (vector-ref gb2312-table (-fx n #x8140))))
			      (ucs2-string-set! res j (integer->ucs2 u))
			      (loop (+fx i 2) (+fx j 1)))
			   (error "gb2312" "Illegal code-point" n))))))))))


