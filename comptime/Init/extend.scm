;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Init/extend.scm             */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Oct 28 08:37:41 1994                          */
;*    Last change :  Wed Dec 27 15:33:57 2006 (serrano)                */
;*    Copyright   :  1992-2006 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    La lecture des extensions (isole'es pour ne pas avoir le pbm     */
;*    des PRIMOPs trop nombreuses).                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    Le module                                                        */
;*---------------------------------------------------------------------*/
(module init_extend
   (include "Engine/pass.sch")
   (export  (load-extend ::bstring))
   (import  engine_param
	    tools_error))

;*---------------------------------------------------------------------*/
;*    load-extend ...                                                  */
;*---------------------------------------------------------------------*/
(define (load-extend extend-name)
   (unless (member extend-name *extend-table*)
      (set! *extend-table* (cons extend-name *extend-table*))
      (let ((fname (find-file/path extend-name *lib-dir*)))
	 (if fname
	     (begin
		(pass-prelude fname)
		(loadq fname))
	     (let ((fname (find-file/path (string-append extend-name ".init")
					  *lib-dir*)))
		(if fname
		    (begin
		       (pass-prelude fname)
		       (loadq fname))
		    (error "parse-args"
			   "Can't find extend file"
			   extend-name)))))))

;*---------------------------------------------------------------------*/
;*    *extend-table* ...                                               */
;*---------------------------------------------------------------------*/
(define *extend-table* '())
