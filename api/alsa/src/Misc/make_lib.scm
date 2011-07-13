;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Wed Jul 13 06:43:55 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_makelib

   (library multimedia)
   
   (import  __alsa_alsa
	    __alsa_pcm
	    __alsa_music
	    __alsa_card
	    __alsa_control
	    __alsa_mixer)

   (eval    (export-all)

            (class &alsa-error)
            (class alsa-object)
	    (class alsa-snd-pcm)
	    (class alsa-snd-card)
	    (class alsa-snd-ctl)
	    (class alsa-snd-ctl-card-info)
	    (class alsa-snd-mixer)

	    (class alsabuffer)
	    (class alsamusic)
	    (class alsadecoder)
	    (class alsadecoder-host)
	    (class alsadecoder-client)))

