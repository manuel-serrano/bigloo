;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/examples/bgst_play.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jul 28 10:03:41 2008                          */
;*    Last change :  Tue Nov 15 19:16:34 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    An example of Bigloo/Gstreamer program for playing any kind      */
;*    of music files.                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bgst-play
   (library pthread multimedia gstreamer)
   (main main))

;*---------------------------------------------------------------------*/
;*    verbose ...                                                      */
;*---------------------------------------------------------------------*/
(define verbose 1)

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (version)
   (bigloo-config 'release-number))

;*---------------------------------------------------------------------*/
;*    verb ...                                                         */
;*---------------------------------------------------------------------*/
(define (verb level . args)
   (when (>=fx verbose level)
      (apply print args)))

;*---------------------------------------------------------------------*/
;*    sample ...                                                       */
;*---------------------------------------------------------------------*/
(define sample -1)

;*---------------------------------------------------------------------*/
;*    repeat ...                                                       */
;*---------------------------------------------------------------------*/
(define repeat 1)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   (let ((streams '())
	 (volume 100)
	 (src 'bigloo))
      
      (args-parse (cdr args)
	 ((("-h" "--help") (help "This message"))
	  (print "bgst-play v" (version))
	  (print "usage: bgst-play [options] file1 file2 ...")
	  (newline)
	  (args-parse-usage #f)
	  (exit 0))
	 ((("-V" "--verbose") (help "Increase verbosity"))
	  (set! verbose (+ 1 verbose)))
	 ((("-q" "--quiet") (help "Silent mode"))
	  (set! verbose 0))
	 ((("-v" "--volume") ?vol (help "Set volume"))
	  (set! volume (string->integer vol)))
	 (("-s" ?source (help "Select source (bigloo, gst)"))
	  (set! src (string->symbol source)))
	 (("--sample" ?seconds (help "Play only the first seconds"))
	  (set! sample (string->integer seconds)))
	 (("--repeat" ?n (help "Repeat n-times each song"))
	  (set! repeat (string->integer n)))
	 (else
	  (set! streams (cons else streams))))

      (when (null? streams)
	 (print "Bgst-play v" (version))
	 (print "usage: bgst-play [options] file1 file2 ...")
	 (exit 0))
	  
      (verb 3 "bgst-play: " (version))
      (verb 3 "gst-version: " (gst-version) "\n")
      (play-streams volume (reverse streams) src)))

;*---------------------------------------------------------------------*/
;*    play-streams ...                                                 */
;*---------------------------------------------------------------------*/
(define (play-streams volume streams src)
   
   (let* ((pipeline (instantiate::gst-pipeline
		       (name "audio-player")))
	  (source (if (eq? src 'bigloo)
		      (gst-element-factory-make "bglportsrc" "file-source")
		      (gst-element-factory-make "filesrc" "file-source")))
	  (decoder (gst-element-factory-make "decodebin" "decoder"))
	  (conv (gst-element-factory-make "audioconvert" "converter"))
	  (resample (gst-element-factory-make "audioresample" "resample"))
	  (sink (gst-element-factory-make "autoaudiosink" "audio-sink"))
	  (bus (with-access::gst-pipeline pipeline (bus) bus)))
      
      ;; create the pipeline
      (gst-bin-add! pipeline source decoder conv resample sink)
      (gst-element-link! source decoder)
      (gst-element-link! conv resample sink)
      
      ;; add handler for dynamic padd addition
      (gst-object-connect! decoder
			   "pad-added"
			   (lambda (el pad)
			      (verb 2 "pad-added: el=" el " pad=" pad)
			      (let ((p (gst-element-pad conv "sink")))
				 (gst-pad-link! pad p))))
      
      (let loop ((streams streams))
	 (when (pair? streams)
	    (if (directory? (car streams))
		(loop (append (sort (directory->path-list (car streams))
				    (lambda (p1 p2)
				       (< (string-natural-compare3 p1 p2) 0)))
			      (cdr streams)))
		(let laap ((r repeat))
		   (if (=fx r 0)
		       (loop (cdr streams))
		       (begin
			  (verb 1 "Playing: " (car streams))
			  (if (eq? src 'bigloo)
			      (gst-object-property-set! source :uri (car streams))
			      (gst-object-property-set! source :location (car streams)))
			  (gst-element-state-set! pipeline 'playing)
			  
			  (let liip ((s sample))
			     (let ((msg (gst-bus-poll bus :timeout #l1000000000)))
				(cond
				   (msg
				    ;;;
				    (verb 4 "message: " msg
				       (if (gst-message-state-changed? msg)
					   (case (gst-message-new-state msg)
					      ((playing) " => playing")
					      ((paused) " => paused")
					      ((ready) " => ready")
					      ((null) " => null"))
					   ""))
				    (with-access::gst-message msg (type)
				       (when (=fx type $gst-message-error)
					  (error 'gstreamer
					     "Message error"
					     (gst-message-error-string msg)))
				       (when (=fx type $gst-message-duration-changed)
					  (verb 3 "duration: "
					     (gst-element-query-duration
						pipeline)))
				       (when (=fx type $gst-message-tag)
					  (for-each (lambda (t)
						       (verb 3 "  "
							  (car t) ": " (cdr t)))
					     (gst-message-tag-list msg)))
				       (if (=fx type $gst-message-eos)
					   (begin
					      (gst-element-state-set! pipeline 'null)
					      (gst-element-state-set! pipeline 'ready)
					      (laap (-fx r 1)))
					   (liip s))))
				   ((=fx s 0)
				    (gst-element-state-set! pipeline 'null)
				    (gst-element-state-set! pipeline 'ready)
				    (laap (-fx r 1)))
				   (else
				    (liip (-fx s 1))))))))))))))
