;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdl/src/prgm.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Mar 23 06:39:37 2002                          */
;*    Last change :  Thu Aug  8 11:44:00 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Read a program description                                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdl_prgm

   (import __bdl_misc
	   __bdl_types
	   __bdl_env
	   __bdl_etags)
   
   (export (read-program ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    read-program ...                                                 */
;*---------------------------------------------------------------------*/
(define (read-program afile etags)
   (if (file-exists? afile)
       (let ((mods (with-input-from-file afile read)))
	  (if (pair? mods)
	      (if (file-exists? etags)
		  (let ((prgm (new-program afile
					   etags
					   (apply append (map cdr mods)))))
		     (read-etags! prgm mods)
		     prgm)
		  (bdl-error "read-program"
			     "Can't find `etags' file for input"
			     etags))
	      (bdl-error "read-program" "Illegal afile format" afile)))
       (bdl-error "read-program" "Can't find afile file for input" afile)))
