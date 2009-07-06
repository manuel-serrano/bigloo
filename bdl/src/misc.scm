;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bdl/src/misc.scm                     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Mar 22 11:58:08 2002                          */
;*    Last change :  Thu Aug  8 10:09:48 2002 (serrano)                */
;*    Copyright   :  2002 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Bdl misc                                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __bdl_misc
   (export (bdl-error obj proc msg)
	   (parse-string-id ::bstring ::bstring)))

;*---------------------------------------------------------------------*/
;*    bdl-error ...                                                    */
;*---------------------------------------------------------------------*/
(define (bdl-error obj proc msg)
   (error obj proc msg))

;*---------------------------------------------------------------------*/
;*    parse-string-id ...                                              */
;*---------------------------------------------------------------------*/
(define (parse-string-id string dt)
   (let ((len (string-length string)))
      (let loop ((walker     0)
		 (id-stop    0)
		 (type-start 0))
	 (cond
	    ((=fx walker len)
	     (cond
		((and (=fx id-stop 0) (>fx type-start 0))
		 ;; this empty name variable can be useful to declare
		 ;; prototype so it is legal.
		 (values "" (substring string type-start len)))
		((=fx id-stop 0)
		 (values string dt))
		((=fx type-start len)
		 ;; empty type are erroneous
		 (bdl-error "parse-string-id"
			    "Illegal formal identifier"
			    string))
		(else
		 (values (substring string 0 id-stop)
			 (substring string type-start len)))))
	    ((and (char=? (string-ref string walker) #\:)
		  (<fx walker (-fx len 1))
		  (char=? (string-ref string (+fx walker 1)) #\:))
	     (if (>fx type-start 0)
		 (bdl-error "parse-string-id"
			    "Illegal formal identifier"
			    string)
		 (loop (+fx walker 2) walker (+fx walker 2))))
	    (else
	     (loop (+fx walker 1) id-stop type-start))))))
