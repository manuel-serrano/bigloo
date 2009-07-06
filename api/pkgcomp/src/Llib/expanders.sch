;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pkgcomp/src/Llib/expanders.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Dec 12 05:25:37 2006                          */
;*    Last change :  Sat Mar  3 10:33:06 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The Pkgcomp expanders                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    @install-expanders! ...                                          */
;*---------------------------------------------------------------------*/
(define (@install-expanders!)
   (register-eval-srfi! 'bigloo-@expanders)
   (eval '(define-expander interface @interface-expander)))
	 
