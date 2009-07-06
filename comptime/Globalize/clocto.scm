;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Globalize/clocto.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan 23 14:03:24 1996                          */
;*    Last change :  Mon May 15 07:49:57 2000 (serrano)                */
;*    Copyright   :  1996-2000 Manuel Serrano, see LICENSE file        */
;*    -------------------------------------------------------------    */
;*    The transitive closure of the `cto' property.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module globalize_clocto
   (include "Tools/trace.sch")
   (import  tools_shape
	    type_type
	    ast_var
	    ast_node
	    globalize_ginfo
	    globalize_kapture)
   (export  (cto-transitive-closure! ::local)))

;*---------------------------------------------------------------------*/
;*    cto-transitive-closure ...                                       */
;*    -------------------------------------------------------------    */
;*    In this function, we use the cto* field in two different         */
;*    ways:                                                            */
;*       a- for the host function, this field will contain the list    */
;*          of the called function.                                    */
;*       b- for the callee function non globalized function, this      */
;*          field is used as a mark in order to not enter twice in     */
;*          the same function.                                         */
;*---------------------------------------------------------------------*/
(define (cto-transitive-closure! host)
   (let* ((info      (local-value host))
	  (cto       (append (sfun/Ginfo-cto info)
			     (sfun/Ginfo-cfunction info)))
	  (cto*-orig (list cto)))
      (let loop ((cto  cto)
		 (cto* cto*-orig))
	 (cond
	    ((null? cto)
	     (sfun/Ginfo-cto*-set! info
				  (if (eq? cto* cto*-orig)
				      (car cto*-orig)
				      (union cto*))))
	    ((eq? (car cto) local)
	     (loop (cdr cto) cto*))
	    ((sfun/Ginfo-G? (local-value (car cto)))
	     (loop (cdr cto) cto*))
	    ((sfun/Ginfo-cto* (local-value (car cto)))
	     ;; we have already processed this function
	     (loop (cdr cto) cto*))
	    (else
	     (let ((callee (append
			    (sfun/Ginfo-cto (local-value (car cto)))
			    (sfun/Ginfo-cfunction (local-value (car cto))))))
		;; we mark the function has processed
		(sfun/Ginfo-cto*-set! (local-value (car cto)) #t)
		;; and we loop
		(loop (append (cdr cto) callee)
		      (cons callee cto*))))))))
