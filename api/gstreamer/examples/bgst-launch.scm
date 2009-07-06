;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/examples/bgst-launch.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 14 10:26:04 2008                          */
;*    Last change :  Fri Jul 25 07:44:05 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    gst-launch implemented with the Bigloo GSTREAMER binding.        */
;*    -------------------------------------------------------------    */
;*    Run with:                                                        */
;*      bgst-launch filesrc location=foo.mp3 !                         */
;*           ffdemux_mp3 ! ffdec_mp3 ! audioconvert ! alsasink         */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gstreamer_example-gst-launch
   (library gstreamer multimedia pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    *parameter-mutex*                                                */
;*---------------------------------------------------------------------*/
(define *parameter-mutex* (make-mutex "param"))

;*---------------------------------------------------------------------*/
;*    define-parameter ...                                             */
;*---------------------------------------------------------------------*/
(define-macro (define-parameter id default . setter)
   (let ((vid (symbol-append '* id '*)))
      `(begin
          (define ,vid ,default)
          (define (,id)
             ,vid)
          (define (,(symbol-append id '-set!) v)
             (mutex-lock! *parameter-mutex*)
             ,(if (pair? setter)
                  `(set! ,vid (,(car setter) v))
                  `(set! ,vid v))
             (mutex-unlock! *parameter-mutex*)
             v))))

;*---------------------------------------------------------------------*/
;*    bgst-launch-version ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter bgst-launch-version "0.10")
(define-parameter bgst-launch-verbose #f)
(define-parameter bgst-launch-messages #f)

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((args '()))
      (args-parse (cdr argv)
	 (section "Help Options:")
	 ((("-?" "--help") (help "Show help options"))
	  (print "Usage:")
	  (print "  "
		 "bgst-launch-" (bgst-launch-version)
		 " [OPTION...] PIPELINE-DESCRIPTION")
	  (args-parse-usage #f)
	  (newline)
	  (print "Examples:")
	  (print "   bgst-launch bglportsrc file=foo.mp3 ! decodebin ! audioconvert ! audioresample ! autoaudiosink")
	  (print "   bgst-launch bglportsrc file=http://www.nowhere.org/foo.mp3 ! decodebin ! audioconvert ! audioresample ! alsasink")
	  (exit 0))
	 (section "Application Options")
	 ((("-v" "--verbose") (help "Output status information and property notifications"))
	  (bgst-launch-verbose-set! #t))
	 ((("-m" "--messages") (help "Output messages"))
	  (bgst-launch-messages-set! #t))
	 (("--version" (help "Print version information and exit"))
	  (print "bgst-launch version " (bgst-launch-version))
	  (print (gst-version))
	  (exit 0))
	 (else
	  (set! args (cons else args))))
      (let ((el (gst-parse-launchv (reverse! args))))
	 (unless el (error 'bgst-launch "Cannot create pipeline" el))
	 (let ((pipeline (if (gst-pipeline? el)
			     el
			     (let ((pipe (instantiate::gst-pipeline)))
				(gst-bin-add! pipe el)
				pipe))))
	    (display "Setting pipeline to PAUSED...")
	    (flush-output-port (current-output-port))
	    (print (gst-element-state-set! pipeline 'paused))
	    (display "Setting pipeline to PLAYING ...")
	    (flush-output-port (current-output-port))
	    (print (gst-element-state-set! pipeline 'playing))
	    (let ((bus (gst-pipeline-bus pipeline)))
	       (let loop ()
		  (let ((msg (gst-bus-poll bus)))
		     (when (bgst-launch-messages)
			(display
			 (format "Got Message from element \"~a\" (~a): "
				 (let ((src (gst-message-src msg)))
				    (if (gst-element? src)
					(gst-element-name src)
					(find-runtime-type src)))
				 (gst-message-type-name msg))))
		     (cond
			((gst-message-state-changed? msg)
			 (when (bgst-launch-messages)
			    (print "GstMessageState, old-state="
				   (gst-message-old-state msg)
				   ", new-state="
				   (gst-message-new-state msg)
				   ", pending-state="
				   (gst-message-pending-state msg) ";"))
			 (loop))
			((not (gst-message-eos? msg))
			 (loop))))))
	    (display "Setting pipeline to PAUSED...")
	    (flush-output-port (current-output-port))
	    (print (gst-element-state-set! pipeline 'paused))
	    (display "Setting pipeline to NULL...")
	    (flush-output-port (current-output-port))
	    (print (gst-element-state-set! pipeline 'null))))))
	    
				       
	  
