;*=====================================================================*/
;*    .../project/bigloo/api/pulseaudio/src/Llib/pulseaudio.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:11:10 2011                          */
;*    Last change :  Mon Jan 25 16:41:52 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Pulseaudio object wrapper                                        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pulseaudio_pulseaudio
   
   (option (set! *dlopen-init-gc* #t))
   
   (include "pulseaudio.sch")
   
   (extern (export pulseaudio-error "bgl_pulseaudio_error"))
   
   (export (abstract-class pulseaudio-object
	      (pulseaudio-init))
	   
	   (generic pulseaudio-init ::pulseaudio-object)
	   
	   (class &pulseaudio-error::&error)
	   
	   (pulseaudio-error::int ::string ::string ::obj)
	   (pulseaudio-headers-version::bstring)))

;*---------------------------------------------------------------------*/
;*    pulseaudio-init ...                                              */
;*---------------------------------------------------------------------*/
(define-generic (pulseaudio-init o::pulseaudio-object)
   o)

;*---------------------------------------------------------------------*/
;*    pulseaudio-error ...                                             */
;*---------------------------------------------------------------------*/
(define (pulseaudio-error proc msg obj)
   (raise
      (instantiate::&pulseaudio-error
	 (proc proc)
	 (msg msg)
	 (obj obj)))
   0)

;*---------------------------------------------------------------------*/
;*    pulseaudio-headers-version ...                                   */
;*---------------------------------------------------------------------*/
(define (pulseaudio-headers-version)
   ($pa-get-headers-version))

