;*=====================================================================*/
;*    serrano/prgm/project/bigloo/snowman/makefile.sch                 */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Dec 31 09:43:21 2006                          */
;*    Last change :  Sun Dec 31 09:44:28 2006 (serrano)                */
;*    Copyright   :  2006 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The macro that includes inside the snowman source code           */
;*    a Makefile template.                                             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    makefile ...                                                     */
;*---------------------------------------------------------------------*/
(define-macro (makefile file)
   (let ((p (open-input-file file)))
      (unwind-protect
	 (read-string p)
	 (close-input-port p))))
