;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/alsa/src/Llib/music.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jun 25 06:55:51 2011                          */
;*    Last change :  Thu Jan 28 15:09:18 2016 (serrano)                */
;*    Copyright   :  2011-16 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    A (multimedia) music player.                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __alsa_music
   
   (library multimedia)
   
   (import  __alsa_alsa
	    __alsa_pcm)

   (export  (class alsamusic::musicbuf
	       (pcm::alsa-snd-pcm read-only (default (instantiate::alsa-snd-pcm))))))

;*---------------------------------------------------------------------*/
;*    music-close ::alsamusic ...                                      */
;*---------------------------------------------------------------------*/
(define-method (music-close o::alsamusic)
   (with-access::alsamusic o (pcm decoders %amutex)
      (synchronize %amutex
	 (unless (eq? (alsa-snd-pcm-get-state pcm) 'disconnected)
	    (alsa-snd-pcm-close pcm)))))

;*---------------------------------------------------------------------*/
;*    music-closed? ::alsamusic ...                                    */
;*---------------------------------------------------------------------*/
(define-method (music-closed? o::alsamusic)
   (with-access::alsamusic o (%amutex pcm)
      (synchronize %amutex
	 (eq? (alsa-snd-pcm-get-state pcm) 'disconnected))))

;*---------------------------------------------------------------------*/
;*    musicbuf-init! ::alsamusic ...                                   */
;*---------------------------------------------------------------------*/
(define-method (musicbuf-init! o::alsamusic)
   (with-access::alsamusic o (pcm)
      (when (eq? (alsa-snd-pcm-get-state pcm) 'not-open)
	 (alsa-snd-pcm-open pcm))))

;*---------------------------------------------------------------------*/
;*    musicbuf-reset! ::alsamusic ...                                  */
;*---------------------------------------------------------------------*/
(define-method (musicbuf-reset! o::alsamusic)
   (with-access::alsamusic o (pcm)
      (let ((pcm-state (alsa-snd-pcm-get-state pcm)))
	 (unless (eq? pcm-state 'not-open)
	    (when (memq pcm-state '(running prepared))
	       (alsa-snd-pcm-drop pcm))
	    (alsa-snd-pcm-cleanup pcm)))))

;*---------------------------------------------------------------------*/
;*    musicbuf-write ::alsamusic ...                                   */
;*---------------------------------------------------------------------*/
(define-method (musicbuf-write o::alsamusic outbuf size)
   (with-access::alsamusic o (pcm)
      (alsa-snd-pcm-write pcm outbuf size)))
   
