;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/examples/bgst_launch.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 14 10:26:04 2008                          */
;*    Last change :  Sun Nov 18 15:00:10 2012 (serrano)                */
;*    Copyright   :  2008-12 Manuel Serrano                            */
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
	     (synchronize *parameter-mutex*
		,(if (pair? setter)
		     `(set! ,vid (,(car setter) v))
		     `(set! ,vid v)))
             v))))

;*---------------------------------------------------------------------*/
;*    bgst-launch-version ...                                          */
;*---------------------------------------------------------------------*/
(define-parameter bgst-launch-version "1.0")
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
	 (let ((pipeline (if (isa? el gst-pipeline)
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
	    (with-access::gst-pipeline pipeline (bus)
	       (let loop ()
		  (let ((msg (gst-bus-poll bus)))
		     (when (bgst-launch-messages)
			(display
			 (format "Got Message from element \"~a\" (~a): "
			    (with-access::gst-message msg (src)
			       (if (isa? src gst-element)
				   (with-access::gst-element src (name)
				      name)
				   (find-runtime-type src)))
			    (with-access::gst-message msg (type-name)
			       type-name))))
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
	    
				       
	  
