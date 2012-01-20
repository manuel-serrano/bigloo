;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/src/Llib/musicloop.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  2 09:58:46 2008                          */
;*    Last change :  Fri Jan 20 09:39:14 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of the Music Event Loop                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-music-event-loop

   (import __multimedia-music
	   __multimedia-id3)
   
   (export (generic music-event-loop ::music . ::obj)
	   (generic music-event-loop-inner ::music ::long ::obj ::obj ::obj ::obj ::obj)
	   (generic music-event-loop-reset! ::music)
	   (generic music-event-loop-abort! ::music)

	   (music-event-loop-parse-opt ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    music-event-loop ::music ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop o::music . obj)
   (with-access::music o (%loop-mutex %loop-condv %status %abort-loop %reset-loop)
      (mutex-lock! %loop-mutex)
      (set! %abort-loop #f)
      (set! %reset-loop #f)
      (mutex-unlock! %loop-mutex)
      (multiple-value-bind (onstate onmeta onerror onvol onplaylist frequency)
	 (music-event-loop-parse-opt obj)
	 ;; setup state
	 (with-access::musicstatus %status (state volume)
	    (set! state 'init))
	 ;; enter the loop
	 (unwind-protect
	    ;; the event loop is protected against timeout errors
	    (music-event-loop-inner o frequency onstate onmeta onerror onvol onplaylist)
	    ;; signal that the loop is done
	    (begin
	       (mutex-lock! %loop-mutex)
	       (set! %abort-loop #t)
	       (condition-variable-broadcast! %loop-condv)
	       (mutex-unlock! %loop-mutex))))))

;*---------------------------------------------------------------------*/
;*    music-event-loop-inner ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop-inner m::music frequency onstate onmeta onerror onvol onplaylist)
   
   (define (newstate? stat2 stat1)
      (with-access::musicstatus stat2 (state songid)
	 (with-access::musicstatus stat1 ((state1 state))
	    (or (not (eq? state1 state))
		(newsong? stat2 stat1)))))

   (define (newplaylist? stat2 stat1)
      (with-access::musicstatus stat2 (playlistid)
	 (with-access::musicstatus stat1 ((playlistid1 playlistid))
	    (not (eq? playlistid1 playlistid)))))

   (define (newsong? stat2 stat1)
      (with-access::musicstatus stat2 (songid)
	 (with-access::musicstatus stat1 ((songid1 songid))
	    (not (eq? songid1 songid)))))

   (define (newvolume? stat2 stat1)
      (with-access::musicstatus stat2 (volume)
	 (with-access::musicstatus stat1 ((volume1 volume))
	    (not (eq? volume1 volume)))))
   
   (with-access::music m (%loop-mutex %abort-loop %reset-loop %status %mutex)
      (mutex-lock! %loop-mutex)
      (let loop ((stat1 (duplicate::musicstatus %status
			   (state 'unspecified)))
		 (stat2 (duplicate::musicstatus %status
			   (state 'init))))
	 (let ((stop (or (music-closed? m) %abort-loop)))
	    (mutex-unlock! %loop-mutex)
	    (unless stop
	       (music-update-status! m stat2)
	       
	       (when (newstate? stat2 stat1)
		  (with-access::musicstatus stat2 (state err songig)
		     (case state
			((error)
			 (when (and onerror err)
			    ;; onerror
			    (onerror err)))
			((play)
			 ;; onstate
			 (when onstate
			    (onstate stat2))
			 ;; onmeta
			 (when onmeta
			    (onmeta (music-get-meta stat2 m))))
			((ended)
			 ;; onstate
			 (when onstate
			    (onstate stat2))
			 (with-access::musicstatus stat2 (song playlistlength)
			    (when (<fx song (-fx playlistlength 1))
			       (music-next m))))
			((skip)
			 ;; as ended but do not raise the onstate event
			 (with-access::musicstatus stat2 (song playlistlength)
			    (when (<fx song (-fx playlistlength 1))
			       (music-next m))))
			(else
			 ;; onstate
			 (when onstate
			    (onstate stat2))))))
	    
	       (when (and onvol (newvolume? stat2 stat1))
		  ;; onvolume
		  (with-access::musicstatus stat2 (volume)
		     (onvol volume)))

	       (when (and onplaylist (newplaylist? stat2 stat1))
		  ;; onplaylist
		  (onplaylist stat2))
		  
	       ;; wait a little bit
	       (sleep frequency)
	       
	       ;; swap the two status
	       (mutex-lock! %loop-mutex)
	       (when %reset-loop
		  (set! %reset-loop #f)
		  (with-access::musicstatus stat2 (state)
		     (set! state 'reset)))

	       ;; loop back
	       (loop stat2 stat1))))))

;*---------------------------------------------------------------------*/
;*    music-get-meta ...                                               */
;*---------------------------------------------------------------------*/
(define (music-get-meta status m)
   
   (define (alist->id3 l)
      
      (define (get k l d)
	 (let ((c (assq k l)))
	    (if (pair? c) (cdr c) d)))
      
      (instantiate::id3
	 (title (get 'title l "???"))
	 (artist (get 'artist l "???"))
	 (album (get 'album l "???"))
	 (genre (get 'genre l "???"))
	 (year (get 'year l 0))
	 (comment (get 'comment l ""))
	 (version (get 'version l "v1"))))
   
   (with-access::musicstatus status (song playlistlength)
      (when (>fx playlistlength 0)
	 (let ((plist (music-playlist-get m)))
	    (when (and (>=fx song 0) (<fx song (length plist)))
	       (let ((f (list-ref plist song)))
		  (if (file-exists? f)
		      (or (file-musictag f) (alist->id3 (music-meta m)) f)
		      (or (alist->id3 (music-meta m)) f))))))))

;*---------------------------------------------------------------------*/
;*    music-event-loop-reset! ::music ...                              */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop-reset! o::music)
   (with-access::music o (%loop-mutex %reset-loop)
      (mutex-lock! %loop-mutex)
      (set! %reset-loop #t)
      (mutex-unlock! %loop-mutex)))

;*---------------------------------------------------------------------*/
;*    music-event-loop-abort! ...                                      */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop-abort! o::music)
   (with-access::music o (%loop-mutex %abort-loop %loop-condv)
      (mutex-lock! %loop-mutex)
      (unless %abort-loop
	 (condition-variable-wait! %loop-condv %loop-mutex)
	 (mutex-unlock! %loop-mutex))))

;*---------------------------------------------------------------------*/
;*    music-event-loop-parse-opt ...                                   */
;*---------------------------------------------------------------------*/
(define (music-event-loop-parse-opt obj)

   (define (get-opt key def)
      (let ((c (memq key obj)))
	 (if (and (pair? c) (pair? (cdr c)))
	     (cadr c)
	     def)))
      
   (define (get-proc-opt key arity)
      (let ((c (memq key obj)))
	 (when (and (pair? c) (pair? (cdr c)))
	    (cond
	       ((not (procedure? (cadr c)))
		(bigloo-type-error 'music-event-loop 'procedure (cadr c)))
	       ((not (correct-arity? (cadr c) arity))
		(error 'music-event-loop
		       (format "Bad ~a arity (expecting ~a)" key arity)
		       (cadr c)))
	       (else
		(cadr c))))))
   
   (values (get-proc-opt :onstate 1)
	   (get-proc-opt :onmeta 1)
	   (get-proc-opt :onerror 1)
	   (get-proc-opt :onvolume 1)
	   (get-proc-opt :onplaylist 1)
	   (get-opt :frequency 2000000)))


