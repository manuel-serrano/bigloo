;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Read/access.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Mar 17 11:33:41 1993                          */
;*    Last change :  Thu Mar 26 07:22:24 2009 (serrano)                */
;*    Copyright   :  1993-2009 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The module which handle access tables `module/name'              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module read_access
   (import engine_param
	   engine_engine
	   tools_error
	   init_main
	   tools_speek)
   (export (read-access-files)))

;*---------------------------------------------------------------------*/
;*    read-access-files ...                                            */
;*---------------------------------------------------------------------*/
(define (read-access-files)
   (define (inner-read-access-file name::bstring)
      (verbose 2 "      [reading afile " name "]" #\Newline)
      (module-load-access-file name))
   (if (null? *access-files*)
       (when (file-exists? *access-file-default*)
	  (inner-read-access-file *access-file-default*))
       (for-each (lambda (f)
		    (if (file-exists? f)
			(inner-read-access-file f)
			(user-error "read-access-file"
				    "Can't find access file"
				    f)))
		 *access-files*)))
