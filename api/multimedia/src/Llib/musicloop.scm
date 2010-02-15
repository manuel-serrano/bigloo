;*=====================================================================*/
;*    .../project/bigloo/api/multimedia/src/Llib/musicloop.scm         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri May  2 09:58:46 2008                          */
;*    Last change :  Sun Feb 14 11:41:05 2010 (serrano)                */
;*    Copyright   :  2008-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The implementation of the Music Event Loop                       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __multimedia-music-event-loop

   (import __multimedia-music)
   
   (export (generic music-event-loop ::music . ::obj)
	   (generic music-event-loop-inner ::music ::obj ::obj ::obj ::obj)
	   (generic music-event-loop-reset! ::music)
	   (generic music-event-loop-abort! ::music)

	   (music-event-loop-parse-opt ::pair-nil)))

;*---------------------------------------------------------------------*/
;*    music-event-loop ::music ...                                     */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop o::music . obj)
   (with-access::music o (frequency %loop-mutex %loop-condv
				    %status %abort-loop %reset-loop)
      (mutex-lock! %loop-mutex)
      (set! %abort-loop #f)
      (set! %reset-loop #f)
      (mutex-unlock! %loop-mutex)
      (multiple-value-bind (onstate onmeta onerror onvol)
	 (music-event-loop-parse-opt obj)
	 ;; setup state
	 (with-access::musicstatus %status (state volume)
	    (set! volume 0)
	    (set! state 'init))
	 ;; enter the loop
	 (unwind-protect
	    ;; the event loop is protected against timeout errors
	    (music-event-loop-inner o onstate onmeta onerror onvol)
	    ;; signal that the loop is done
	    (begin
	       (mutex-lock! %loop-mutex)
	       (set! %abort-loop #t)
	       (condition-variable-broadcast! %loop-condv)
	       (mutex-unlock! %loop-mutex))))))

;*---------------------------------------------------------------------*/
;*    music-event-loop-inner ...                                       */
;*---------------------------------------------------------------------*/
(define-generic (music-event-loop-inner m::music onstate onmeta onerror onvol)
   
   (define (newstate? stat2 stat1)
      (with-access::musicstatus stat2 (state song playlistid)
	 (or (not (eq? (musicstatus-state stat1) state))
	     (not (eq? (musicstatus-playlistid stat1) playlistid))
	     (not (eq? (musicstatus-song stat1) song)))))
   
   (define (newvolume? stat2 stat1)
      (with-access::musicstatus stat2 (volume)
	 (not (eq? (musicstatus-volume stat1) volume))))
   
   (with-access::music m (%loop-mutex %abort-loop %reset-loop frequency %status)
      (mutex-lock! %loop-mutex)
      (let loop ((stat1 (duplicate::musicstatus %status
			   (state 'init)))
		 (stat2 (instantiate::musicstatus
			   (state 'stop))))
	 (let ((stop (or (music-closed? m) (music-%abort-loop m))))
	    (mutex-unlock! %loop-mutex)
	    (unless stop
	       (music-update-status! m stat2)
	       
	       (when (newstate? stat2 stat1)
		  ;; onstate
		  (when onstate (onstate stat2))
		  
		  ;; onmeta
		  (with-access::musicstatus stat2 (song playlistlength)
		     (when (>fx playlistlength 0)
			(let ((plist (music-playlist-get m)))
			   (if (and (>=fx song 0) (<fx song (length plist)))
			       (onmeta (list-ref plist song) plist)
			       (onmeta #f plist)))))
		  
		  (when (and onerror (musicstatus-err stat2))
		     ;; onerror
		     (onerror (musicstatus-err stat2))))
	       
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
   
   (define (get-opt key arity)
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
   
   (values (get-opt :onstate 1)
	   (get-opt :onmeta 2)
	   (get-opt :onerror 1)
	   (get-opt :onvolume 1)))


