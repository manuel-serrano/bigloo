;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/examples/bgst_png.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sat Aug  9 06:47:25 2008                          */
;*    Last change :  Tue Nov 15 19:21:15 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    This example scales a source image into a png image.             */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module bgst-play
   (library pthread multimedia gstreamer)
   (main main))

;*---------------------------------------------------------------------*/
;*    version ...                                                      */
;*---------------------------------------------------------------------*/
(define-macro (version)
   (bigloo-config 'release-number))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main args)
   
   (let ((width 120)
	 (height 80)
	 (src #f)
	 (mode 'file)
	 (dst #f))
      
      (args-parse (cdr args)
	 (("--help" (help "This message"))
	  (print "bgst-png v" (version))
	  (print "usage: bgst-png [options] file")
	  (newline)
	  (args-parse-usage #f)
	  (exit 0))
	 ((("-w" "--width") ?w (help "Set output width"))
	  (set! width (string->integer w)))
	 ((("-h" "--height") ?h (help "Set output height"))
	  (set! height (string->integer h)))
	 ((("-o" "--output") ?d (help "Output file"))
	  (set! dst d))
	 (("-s" ?source (help "Select source (bigloo, gst)"))
	  (set! mode (string->symbol source)))
	 (else
	  (set! src else)))

      (when (not src)
	 (print "Bgst-png v" (version))
	 (print "usage: bgst-png [options] file")
	 (exit 0))

      (unless dst (set! dst (string-append (prefix src) ".png")))
      
      (let* ((pipeline (instantiate::gst-pipeline))
	     (source (if (eq? mode 'bigloo)
			 (gst-element-factory-make "bglportsrc" "file-source"
						   :uri src)
			 (gst-element-factory-make "filesrc" "file-source"
						   :location src)))
	     (decoder (gst-element-factory-make "decodebin"))
	     (csp (gst-element-factory-make "ffmpegcolorspace"))
	     (gdkpixbuf (gst-element-factory-make "gdkpixbufscale"))
	     (encoder (gst-element-factory-make "pngenc"))
	     (sink (if (eq? mode 'bigloo)
		       (gst-element-factory-make "bglportsink" "file-sink" :uri dst)
		       (gst-element-factory-make "filesink" "file-sink" :location dst)))
	     (bus (with-access::gst-pipeline pipeline (bus) bus)))

	 (tprint "src=" src " dst=" dst)
	 
	 ;; create the pipeline
	 (gst-bin-add! pipeline source decoder csp encoder sink gdkpixbuf)
	 (gst-element-link! source decoder)
	 (gst-element-link! csp gdkpixbuf)
	 (gst-element-link-mime! gdkpixbuf encoder
			    "video/x-raw-rgb"
			    :width width :height height)
	 (gst-element-link! encoder sink)

	 (gst-object-connect! decoder
			      "pad-added"
			      (lambda (el pad)
				 (let ((p (gst-element-pad csp "sink")))
				    (gst-pad-link! pad p))))
;* 	 (gst-object-connect! decoder                                  */
;* 			      "new-decoded-pad"                        */
;* 			      (lambda (el pad x)                       */
;* 				 (tprint "glop...")                    */
;* 				 (let ((sinkpad (gst-element-compatible-pad */
;* 						 csp                   */
;* 						 pad (gst-pad-caps pad)))) */
;* 				    (when (gst-caps-always-compatible? */
;* 					   (gst-pad-caps sinkpad)      */
;* 					   (gst-pad-caps pad))         */
;* 				       (gst-pad-link! pad sinkpad))))) */

	 (gst-element-state-set! pipeline 'playing)

	 (let loop ()
	    (let ((msg (gst-bus-poll bus)))
	       (cond
		  ((gst-message-eos? msg)
		   #unspecified)
		  ((gst-message-error? msg)
		   (error 'bgst-png
			  "Cannot produce image"
			  (gst-message-error-string msg)))
		  ((gst-message-state-changed? msg)
		   (tprint "new state: " (gst-message-new-state msg))
		   (loop))
		  (else
		   (tprint "msg: " msg)
		   (loop)))))
	 (gst-element-state-set! pipeline 'null))))
