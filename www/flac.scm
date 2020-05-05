(module musicplay
   (library multimedia pthread alsa pulseaudio flac)
   (main main))

(define (main args)
   (let* ((pcm (instantiate::alsa-snd-pcm
		  (device "default")))
	  (decoder (instantiate::flac-alsadecoder
		      (mimetypes '("audio/flac" "application/x-flac"))))
	  (player (instantiate::alsamusic
		     (onstate (lambda (o s) (print "status " s)))
		     (onerror (lambda (o e) (print "error " e)))
		     (decoders (list decoder))
		     (pcm pcm))))
      (music-volume-set! player 80)
      (music-playlist-clear! player)
      (for-each (lambda (p) (music-playlist-add! player p)) (cdr args))
      (music-play player)))
