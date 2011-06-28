;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Mon Jun 27 19:34:57 2011 (serrano)                */
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
	    __alsa_card)

   (eval    (export-all)

            (class &alsa-error)
            (class alsa-object)
	    (class alsa-snd-pcm)
	    (class alsa-card)

	    (class alsamusic)
	    (class alsadecoder)))

