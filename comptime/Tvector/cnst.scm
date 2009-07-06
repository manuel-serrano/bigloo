;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tvector/cnst.scm            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Apr 19 14:44:40 1995                          */
;*    Last change :  Mon Jan  5 16:07:00 2004 (serrano)                */
;*    Copyright   :  1995-2004 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The compilation of constant tvectors                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module tvector_cnst
   (include "Tvector/tvector.sch")
   (import  type_type
	    type_env
	    type_cache
	    tvector_tvector)
   (export  (tvector-C-static?::bool    tvector)
	    (tvector->c-vector::bstring tvector)))

;*---------------------------------------------------------------------*/
;*    tvector-C-static? ...                                            */
;*    -------------------------------------------------------------    */
;*    Is this tvector compilable as a static C vector?                 */
;*    It is possible only if the item type is an immediat C type.      */
;*---------------------------------------------------------------------*/
(define (tvector-C-static? tvect)
      (let ((itype (tvec-item-type (a-tvector-type tvect))))
	 (cond
	    ((eq? itype *long*)   #t)
	    ((eq? itype *int*)    #t)
	    ((eq? itype *char*)   #t)
	    ((eq? itype *bool*)   #t)
	    ((eq? itype *string*) #t)
	    ((eq? itype *real*)   #t)
	    (else #f))))

;*---------------------------------------------------------------------*/
;*    tvector-c-printer ...                                            */
;*---------------------------------------------------------------------*/
(define (tvector-c-printer tvect)
   (let ((itype (tvec-item-type (a-tvector-type tvect))))
      (cond
	 ((eq? itype *long*)   display)
	 ((eq? itype *int*)    display)
	 ((eq? itype *char*)   (lambda (x port)
				  (display "(unsigned char)" port)
				  (display (char->integer x) port)))
	 ((eq? itype *bool*)   (lambda (x port)
				  (if x
				      (display "1" port)
				      (display "0" port))))
	 ((eq? itype *string*) write)
	 ((eq? itype *real*)   display)
	 (else (error "tvector-c-printer"
		      "This tvector can't not be compiled as a static C vector"
		      tvect)))))
			   
;*---------------------------------------------------------------------*/
;*    tvector->c-vector ...                                            */
;*---------------------------------------------------------------------*/
(define (tvector->c-vector::bstring tvector)
   (let* ((vect      (a-tvector-vector tvector)) 
	  (c-printer (tvector-c-printer tvector))
	  (len-1     (-fx (vector-length vect) 1))
	  (port      (open-output-string)))
      (display #\{ port)
      (let loop ((i 0))
	 (if (=fx i len-1)
	     (begin
		(c-printer (vector-ref vect i) port)
		(display #\} port)
		(close-output-port port))
	     (begin
		(c-printer (vector-ref vect i) port)
		(display ", " port)
		(loop (+fx i 1)))))))
   
      
      
		
