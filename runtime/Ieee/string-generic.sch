;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Ieee/string-generic.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:49:25 2024                          */
;*    Last change :  Wed Jul 10 08:29:12 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generic portable string implementation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export ($$substring=?::bool ::bstring ::bstring ::long)
	   ($$string=?::bool ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    $$substring=? ...                                                */
;*---------------------------------------------------------------------*/
(define ($$substring=? string1 string2 len)
   (when (and (>=fx (string-length string1) len)
	      (>=fx (string-length string2) len))
      (let loop ((i 0))
	 (cond
	    ((=fx i len)
	     #t)
	    ((char=? (string-ref string1 i) (string-ref string2 i))
	     (loop (+fx i 1)))
	    (else
	     #f)))))

;*---------------------------------------------------------------------*/
;*    $$string=? ...                                                   */
;*---------------------------------------------------------------------*/
(define ($$string=? string1 string2)
   (let ((l1 (string-length string1))
	 (l2 (string-length string2)))
      (and (=fx l1 l2) ($$substring=? string1 string2 l1))))
	   

