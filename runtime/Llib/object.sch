;*=====================================================================*/
;*    serrano/prgm/project/bigloo/runtime/Llib/object.sch              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun May 10 08:56:49 2015                          */
;*    Last change :  Sun May 10 09:00:39 2015 (serrano)                */
;*    Copyright   :  2015 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Class hashing                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    get-hash-class ...                                               */
;*    -------------------------------------------------------------    */
;*    This function is shared by the library and the compiler but      */
;*    for bootstrapping convenience, the code is duplicated (via       */
;*    an include) instead of being defined in the library only.        */
;*---------------------------------------------------------------------*/
(define (get-class-hash def)
   
   (define (gethash v)
      (bit-and (get-hashnumber-persistent v) #xffff))

   (let loop ((def def)
	      (hash 1705))
      (cond
	 ((null? def)
	  hash)
	 ((not (pair? def))
	  (bit-xor (gethash def) hash))
	 (else
	  (loop (cdr def)
	     (loop (car def)
		(bit-xor 1966 hash)))))))

