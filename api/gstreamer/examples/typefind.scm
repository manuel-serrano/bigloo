;*=====================================================================*/
;*    .../prgm/project/bigloo/api/gstreamer/examples/typefind.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Sun Jan  6 18:55:05 2008                          */
;*    Last change :  Fri Nov 18 18:24:41 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
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
	  (bus (with-access::gst-pipeline pipeline (bus) bus))
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
      (tprint "1")
      (gst-bin-add! pipeline filesrc typefind)
      (tprint "2")
      (gst-element-link! filesrc typefind)
      (gst-element-state-set! pipeline 'playing)
      (let loop ()
	 (let ((msg (gst-bus-poll bus $gst-message-any #l1000000000)))
	    (when (isa? msg gst-message)
	       (tprint "msg=" msg)
	       (with-access::gst-message msg (type)
		  (when (eq? type $gst-message-error)
		     (tprint "*** ERROR: " (gst-message-error-string msg)))))
	    (unless (and (isa? msg gst-message)
			 (with-access::gst-message msg (type)
			    (=fx type $gst-message-eos)))
	       (loop))))))
      
      
	      
