;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/fprint.sch            */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 12 10:57:52 1999                          */
;*    Last change :  Mon May 15 08:02:33 2000 (serrano)                */
;*    Copyright   :  1999-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    To avoid consing for printing, we define a simple macro form.    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    fprin ::obj ...                                                  */
;*---------------------------------------------------------------------*/
(define-macro (fprin port . args)
   (let ((aux (gensym 'port)))
      `(let ((,aux ,port))
	  ,@(map (lambda (x)
		    `(display ,x ,aux))
		 args))))
       
