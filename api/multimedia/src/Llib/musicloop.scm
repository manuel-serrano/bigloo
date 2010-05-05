;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/src/Llib/musicloop.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  2 09:58:46 2008                          */
;*    Last change :  Thu Mar 11 11:59:52 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
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
	   (generic music-event-loop-inner ::music ::long ::obj ::obj ::obj ::obj)
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
      (multiple-value-bind (onstate onmeta onerror onvol frequency)
	 (music-event-loop-parse-opt obj)
	 ;; setup state
	 (with-access::musicstatus %status (state volume)
	    (set! volume 0)
	    (set! state 'init))
	 ;; enter the loop
	 (unwind-protect
	    ;; the event loop is protected against timeout errors
	    (music-event-loop-inner o frequency onstate onmeta onerror onvol)
	    ;; signal that the loop is done
	    (begin
	       (mutex-lock! %loop-mutex)
	       (set! %abort-loop #t)
	       (condition-variable-broadcast! %loop-condv)
	       (mutex-unlock! %loop-mutex))))))

;*---------------------------------------------------------------------*/
;*    music-event-loop-inner ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop-inner m::music frequency onstate onmeta onerror onvol)
   
   (define (newstate? stat2 stat1)
      (with-access::musicstatus stat2 (state song playlistid)
	 (or (not (eq? (musicstatus-state stat1) state))
	     (not (eq? (musicstatus-playlistid stat1) playlistid))
	     (not (eq? (musicstatus-song stat1) song)))))
   
   (define (newvolume? stat2 stat1)
      (with-access::musicstatus stat2 (volume)
	 (not (eq? (musicstatus-volume stat1) volume))))
   
   (define (newplaylist? stat2 stat1)
      (with-access::musicstatus stat2 (playlistid)
	 (not (eq? (musicstatus-playlistid stat1) playlistid))))
   
   (with-access::music m (%loop-mutex %abort-loop %reset-loop %status)
      (mutex-lock! %loop-mutex)
      (let loop ((stat1 (duplicate::musicstatus %status
			   (state 'unspecified)))
		 (stat2 (instantiate::musicstatus
			   (state 'init))))
	 (let ((stop (or (music-closed? m) (music-%abort-loop m))))
	    (mutex-unlock! %loop-mutex)
	    (unless stop
	       (music-update-status! m stat2)

	       (when (newstate? stat2 stat1)
		  (case (musicstatus-state stat2)
		     ((error)
		      (when (and onerror (musicstatus-err stat2))
			 ;; onerror
			 (onerror (musicstatus-err stat2))))
		     ((play)
		      ;; onstate
		      (when onstate
			 (onstate stat2))
		      ;; onmeta
		      (when onmeta
			 (onmeta (music-get-meta stat2 m))))
		     (else
		      ;; onstate
		      (when onstate
			 (onstate stat2)))))
		      
	       (when (and onvol (newvolume? stat2 stat1))
		  ;; onvolume
		  (onvol (musicstatus-volume stat2)))
	       
	       ;; wait a little bit
	       (sleep frequency)
	       
	       ;; swap the two status
	       (mutex-lock! %loop-mutex)
	       (when %reset-loop
		  (set! %reset-loop #f)
		  (musicstatus-state-set! stat2 'reset))

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
	    (if (and (>=fx song 0) (<fx song (length plist)))
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
	   (get-opt :frequency 2000000)))


