;*=====================================================================*/
;*    .../prgm/project/bigloo/api/pulseaudio/src/Llib/music.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Thu Jan 28 14:48:53 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A (multimedia) music player.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pulseaudio_music
   
   (library multimedia)
   
   (import  __pulseaudio_pulseaudio
	    __pulseaudio_simple)

   (export (class pulseaudiomusic::musicbuf
	      (simple::pulseaudio-simple (default (instantiate::pulseaudio-simple)))
	      (name::bstring (default "bigloo")))

	   (pulseaudiomusic-simple-set! ::pulseaudiomusic ::pulseaudio-simple)))

;*---------------------------------------------------------------------*/
;*    music-close ::pulseaudiomusic ...                                */
;*---------------------------------------------------------------------*/
(define-method (music-close o::pulseaudiomusic)
   (with-access::pulseaudiomusic o (simple %amutex)
      (synchronize %amutex
	 (pulseaudio-simple-close simple))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::pulseaudiomusic ...                              */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::pulseaudiomusic)
   (with-access::pulseaudiomusic o (%amutex simple)
      (synchronize %amutex
	 (eq? (pulseaudio-simple-state simple) 'closed))))

;*---------------------------------------------------------------------*/
;*    musicbuf-write ::pulseaudiomusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (musicbuf-write o::pulseaudiomusic outbuf size)
   (with-access::pulseaudiomusic o (simple)
      (pulseaudio-simple-write simple outbuf size)))

;*---------------------------------------------------------------------*/
;*    musicbuf-drain ::pulseaudiomusic ...                             */
;*---------------------------------------------------------------------*/
(define-method (musicbuf-drain o::pulseaudiomusic)
   (with-access::pulseaudiomusic o (simple)
      (pulseaudio-simple-drain simple)))
   
;*---------------------------------------------------------------------*/
;*    musicbuf-reset! ::pulseaudiomusic ...                            */
;*---------------------------------------------------------------------*/
(define-method (musicbuf-reset! o::pulseaudiomusic)
   (with-access::pulseaudiomusic o (simple)
      (when (isa? simple pulseaudio-simple)
	 (pulseaudio-simple-flush simple))))
   
;*---------------------------------------------------------------------*/
;*    pulseaudiomusic-simple-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (pulseaudiomusic-simple-set! o::pulseaudiomusic s)
   (with-access::pulseaudiomusic o (simple)
      (when (isa? simple pulseaudio-simple)
	 (pulseaudio-simple-close simple))
      (set! simple s)
      s))
