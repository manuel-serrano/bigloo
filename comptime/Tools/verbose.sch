;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Tools/verbose.sch           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 12 10:57:52 1999                          */
;*    Last change :  Mon May 15 08:04:48 2000 (serrano)                */
;*    Copyright   :  1999-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    To avoid consing for verbose, we define a simple macro form.     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    the directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   (import (*verbose* engine_param)))

;*---------------------------------------------------------------------*/
;*    verbose ...                                                      */
;*---------------------------------------------------------------------*/
(define-expander verbose
   (lambda (x e)
      (match-case x
	 ((?- ?level . ?args)
	  (let ((aux (gensym 'level)))
	     `(let ((,aux ,(e level e)))
		 (if (<= ,aux *verbose*)
		     (verbose ,aux ,@(map (lambda (x) (e x e)) args))))))
	 (else
	  (error "verbose" "Illegal form" x)))))
		     
       
