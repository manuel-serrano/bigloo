;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/examples/typefind.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  6 18:55:05 2008                          */
;*    Last change :  Thu Jan 31 07:36:42 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    The example shows how to mplement a mime type discovery tool     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gstreamer_example-typefind
   (library gstreamer multimedia pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let* ((pipeline (gst-pipeline-new "pipeline"))
	  (bus (gst-pipeline-bus pipeline))
	  (filesrc (gst-element-factory-make "filesrc" "source"))
	  (typefind (gst-element-factory-make "typefind" "typefinder"))
	  (file (if (null? (cdr argv))
		    "/usr/local/lib/hop/1.9.0/weblets/doc/etc/sound1.mp3"
		    (cadr argv))))
      (gst-object-property-set! filesrc :location file)
      (gst-object-connect! typefind
			    "have-type"
			    (lambda (_ probability caps)
			       (print
				(format "~a, probabilty=~a"
					(gst-caps-to-string caps)
					probability))
			       (exit 0)))
      (gst-bin-add! pipeline filesrc typefind)
      (gst-element-link! filesrc typefind)
      (gst-element-state-set! pipeline 'playing)
      (let loop ()
	 (let ((msg (gst-bus-poll bus $gst-message-any #l1000000000)))
	    (when msg
	       (tprint "msg=" msg)
	       (when (eq? (gst-message-type msg) $gst-message-error)
		  (tprint "*** ERROR: " (gst-message-error-string msg))))
	    (unless (and msg (=fx (gst-message-type msg) $gst-message-eos))
	       (loop))))))
      
      
	      
