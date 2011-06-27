;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/alsa.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:11:10 2011                          */
;*    Last change :  Sun Jun 26 07:53:42 2011 (serrano)                */
;*    Copyright   :  2011 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Alsa object wrapper                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_alsa
   
   (extern (export alsa-error "bgl_alsa_error"))
   
   (export (abstract-class alsa-object
	      (alsa-init))
	   
	   (generic alsa-init ::alsa-object)
	   
	   (class &alsa-error::&error)
	   
	   (alsa-error::int ::string ::string ::obj)))

;*---------------------------------------------------------------------*/
;*    alsa-init ...                                                    */
;*---------------------------------------------------------------------*/
(define-generic (alsa-init o::alsa-object)
   o)

;*---------------------------------------------------------------------*/
;*    alsa-error ...                                                   */
;*---------------------------------------------------------------------*/
(define (alsa-error proc msg obj)
   (raise (instantiate::&alsa-error
	     (proc proc)
	     (msg msg)
	     (obj obj)))
   0)
