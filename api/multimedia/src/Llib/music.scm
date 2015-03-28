;*=====================================================================*/
;*    .../prgm/project/bigloo/api/multimedia/src/Llib/music.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Jul 30 14:07:08 2005                          */
;*    Last change :  Sat Mar 28 07:17:35 2015 (serrano)                */
;*    Copyright   :  2005-15 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    Generic music player API                                         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-music

   (option (set! *dlopen-init-gc* #t))
   
   (export (class music
	      (music-init)
	      
	      (onstate::procedure (default (lambda (o::music st::symbol) #f)))
	      (onvolume::procedure (default (lambda (o::music vol::int) #f)))
	      (onerror::procedure (default (lambda (o::music e) #f)))
	      (onevent::procedure (default (lambda (o::music evt::symbol val) #f)))
	      
	      (%mutex::mutex (default (make-mutex)))
	      (%status::musicstatus (default (instantiate::musicstatus))))

	   (class musicstatus
	      ;; init, stop, ended, play, pause, skip
	      (state::symbol (default 'stop))
	      (volume::obj (default 0))
	      (repeat::bool (default #f))
	      (random::bool (default #f))
	      (playlistid::int (default 0))
	      (playlistlength::int (default 0))
	      (xfade::int (default 0))
	      (song::int (default 0))
	      (songid::int (default 0))
	      (songpos (default 0))
	      (songlength::int (default 0))
	      (bitrate::int (default 0))
	      (khz::int (default 0))
	      (err::obj (default #f))
	      (buffering::int (default 0)))
	   
	   (generic music-init ::music)
	   
	   (generic music-close ::music)
	   (generic music-closed?::bool ::music)
	   (generic music-reset! ::music)

	   (generic music-state-set! ::music ::symbol)
	   (generic music-error-set! ::music ::obj)
	   
	   (generic music-playlist-get::pair-nil ::music)
	   (generic music-playlist-add! ::music ::bstring)
	   (generic music-playlist-delete! ::music ::int)
	   (generic music-playlist-clear! ::music)
	   
	   (generic music-play ::music . song)
	   (generic music-seek ::music ::obj . song)
	   (generic music-stop ::music)
	   (generic music-pause ::music)
	   (generic music-next ::music)
	   (generic music-prev ::music)
	   
	   (generic music-crossfade ::music ::int)
	   (generic music-random-set! ::music ::bool)
	   (generic music-repeat-set! ::music ::bool)
	   (generic music-reset-error! ::music)
	   
	   (generic music-status::musicstatus ::music)
	   (generic music-song::int ::music)
	   (generic music-songpos::int ::music)
	   (generic music-meta::pair-nil ::music)
	   (generic music-can-play-type?::bool ::music ::bstring)
	   
	   (generic music-volume-get::obj ::music)
	   (generic music-volume-set! ::music ::obj)

	   (music-charset-convert ::obj ::symbol)

	   (%multimedia-init)))

;*---------------------------------------------------------------------*/
;*    Abstract implementation                                          */
;*---------------------------------------------------------------------*/
(define-generic (music-close m::music)
   ;; this function assumes that the music mutex has already been
   ;; acquired by the submethods
   (with-access::music m (%status)
      (with-access::musicstatus %status (state)
	 (set! state 'close))))

(define-generic (music-closed? m::music))
(define-generic (music-reset! m::music))

;*---------------------------------------------------------------------*/
;*    music-state-set! ::music ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (music-state-set! m::music st::symbol)
   (with-access::music m (onstate %status)
      (with-access::musicstatus %status (state)
         (set! state st)
         (onstate m %status))))

;*---------------------------------------------------------------------*/
;*    music-error-set! ::music ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (music-error-set! m::music e)
   (with-access::music m (onerror %status)
      (with-access::musicstatus %status (state err)
	 (set! err e)
         (set! state 'error)
         (onerror m e))))

;*---------------------------------------------------------------------*/
;*    playlist                                                         */
;*---------------------------------------------------------------------*/
(define-generic (music-playlist-get::pair-nil m::music))

(define-generic (music-playlist-add! m::music s::bstring)
   (unless (utf8-string? s)
      (error "music-playlist-add!" "Illegal UTF8 song name" s)))

(define-generic (music-playlist-delete! m::music n::int))
(define-generic (music-playlist-clear! m::music))

(define-generic (music-status::musicstatus m::music)
   (with-access::music m (%status)
      %status))

(define-generic (music-play m::music . song))
(define-generic (music-seek m::music p::obj . song))
(define-generic (music-stop m::music))
(define-generic (music-pause m::music))

(define-generic (music-next m::music)
   (with-access::musicstatus (music-status m) (song playlistlength)
      (if (>=fx song (-fx playlistlength 1))
	  (raise
	     (instantiate::&io-error
		(proc "music-next")
		(msg "No next soung")
		(obj song)))
	  (music-play m (+fx song 1)))))

(define-generic (music-prev m::music)
   (with-access::musicstatus (music-status m) (song songpos playlistlength)
      (if (or (<fx song 0) (=fx playlistlength 0))
	  (raise
	     (instantiate::&io-error
		(proc "music-prev")
		(msg "No previous soung")
		(obj song)))
	  (music-play m (-fx song 1)))))

(define-generic (music-crossfade m::music sec::int))
(define-generic (music-random-set! m::music flag::bool))
(define-generic (music-repeat-set! m::music flag::bool))
(define-generic (music-reset-error! m::music) #unspecified)

;*---------------------------------------------------------------------*/
;*    music-volume ::music ...                                         */
;*---------------------------------------------------------------------*/
(define-generic (music-volume-get::obj m::music))

(define-generic (music-volume-set! m::music vol::obj)
   (with-access::music m (%status onvolume)
      (with-access::musicstatus %status (volume)
	 (set! volume vol)
	 (onvolume m vol))))

;*---------------------------------------------------------------------*/
;*    music-init ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (music-init m::music)
   (with-access::music m (%status)
      (when (nil? %status)
	 (set! %status (instantiate::musicstatus)))))

;*---------------------------------------------------------------------*/
;*    music-song ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (music-song::int m::music)
   (with-access::music m (%status)
      (with-access::musicstatus %status (song)
	 song)))

;*---------------------------------------------------------------------*/
;*    msic-songpos ...                                                 */
;*---------------------------------------------------------------------*/
(define-generic (music-songpos::int m::music)
   (with-access::music m (%status)
      (with-access::musicstatus %status (songpos)
	 songpos)))

;*---------------------------------------------------------------------*/
;*    music-meta ...                                                   */
;*---------------------------------------------------------------------*/
(define-generic (music-meta::pair-nil m::music)
   '())

;*---------------------------------------------------------------------*/
;*    music-can-play-type? ::music ...                                 */
;*---------------------------------------------------------------------*/
(define-generic (music-can-play-type? m::music mimetype::bstring)
   #t)

;*---------------------------------------------------------------------*/
;*    music-charset-convert ...                                        */
;*    -------------------------------------------------------------    */
;*    Convert a string from charset1 to charset2                       */
;*---------------------------------------------------------------------*/
(define (music-charset-convert str charset)
   (if (or (eq? 'UTF-8 charset) (not (string? str)))
       str
       (case charset
	  ((ISO-8859-1 ISO-8859-2 ISO-8859-15 ISO-LATIN-1 WINDOWS-1252)
	   (utf8->iso-latin str))
	  ((CP-1252 WINDOWS-1252)
	   (utf8->cp1252 str))
	  ((UCS-2)
	   (utf8-string->ucs2-string (iso-latin->utf8 str)))
	  (else
	   str))))

;*---------------------------------------------------------------------*/
;*    %multimedia-init ...                                             */
;*---------------------------------------------------------------------*/
(define (%multimedia-init)
   #unspecified)
