;*=====================================================================*/
;*    .../project/bigloo/bigloo/runtime/Ieee/string-generic.sch        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jul  9 13:49:25 2024                          */
;*    Last change :  Tue Jul  9 14:03:02 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Generic portable string implementation.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (export ($$substring=?::bool ::bstring ::bstring ::long)))

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

