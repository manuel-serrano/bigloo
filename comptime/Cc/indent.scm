;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Cc/indent.scm               */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 15 15:49:56 1992                          */
;*    Last change :  Mon May 15 07:30:57 2000 (serrano)                */
;*    Copyright   :  1992-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The indentation of the C file                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module cc_indent
   (export  (indent name))
   (import  tools_speek
	    engine_param))

;*---------------------------------------------------------------------*/
;*    indent ...                                                       */
;*---------------------------------------------------------------------*/
(define (indent name)
   (if (and (string? name)
	    (string? *indent*)
	    (>fx (string-length *indent*) 0))
       (begin
	  (verbose 1 "   . indent (" (basename *indent*) ")" #\Newline)
	  (let ((cmd (string-append *indent* " "
					    name ".c > "
					    name ".cc ")))
	     (verbose 2 "      [" cmd #\] #\Newline)
	     (let ((res (system cmd)))
		(if (not (=fx res 0))
		    (warning *indent* "Non nul value returned -- " res)))
	     (if (file-exists? (string-append name ".cc"))
		 (let ((sname (string-append name ".cc"))
		       (dname (string-append name ".c")))
		    (verbose 2 "        mv " sname " " dname #\Newline)
		    (rename-file sname dname)))))))
