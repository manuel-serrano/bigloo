;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/alsa.scm           */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Jun 23 18:11:10 2011                          */
;*    Last change :  Fri Dec 13 12:00:40 2013 (serrano)                */
;*    Copyright   :  2011-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Alsa object wrapper                                              */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_alsa

   (option (set! *dlopen-init-gc* #t))
   
   (include "alsa.sch")

   (extern (export alsa-error "bgl_alsa_error"))
   
   (export (abstract-class alsa-object
	      (alsa-init))

	   (generic alsa-init ::alsa-object)
	   
	   (class &alsa-error::&error)
	   
	   (alsa-error::int ::string ::string ::obj)
	   (alsa-snd-version::bstring)
	   (alsa-snd-devices-list::pair-nil ::bstring)))

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

;*---------------------------------------------------------------------*/
;*    alsa-snd-version ...                                             */
;*---------------------------------------------------------------------*/
(define (alsa-snd-version)
   ($snd-asoundlib-version))

;*---------------------------------------------------------------------*/
;*    alsa-snd-devices-list ...                                        */
;*---------------------------------------------------------------------*/
(define (alsa-snd-devices-list iface)
   ($bgl-snd-devices-list iface))
