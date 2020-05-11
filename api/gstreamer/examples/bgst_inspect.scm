;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/examples/bgst_inspect.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Mon Jan 14 08:08:42 2008                          */
;*    Last change :  Tue Nov 15 19:13:52 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    gst-inspect implemented with the Bigloo GSTREAMER binding.       */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module gstreamer_example-gst-inspect
   (library gstreamer multimedia pthread)
   (main main))

;*---------------------------------------------------------------------*/
;*    bgst-inspect-version ...                                         */
;*---------------------------------------------------------------------*/
(define (bgst-inspect-version) "1.0")

;*---------------------------------------------------------------------*/
;*    main ...                                                         */
;*---------------------------------------------------------------------*/
(define (main argv)
   (let ((all #t))
      (args-parse (cdr argv)
	 (section "Help Options:")
	 ((("-?" "--help") (help "Show help options"))
	  (print "Usage:")
	  (print "  "
		 "bgst-inspect-" (bgst-inspect-version)
		 " [OPTION...] [ELEMENT-NAME | PLUGIN-NAME]")
	  (args-parse-usage #f)
	  (exit 0))
	 (section "Gstreamer Options")
	 (("--gst-version" (help "Print the GStreamer version"))
	  (print (gst-version))
	  (exit 0))
	 (section "Application Options")
	 ((("-a" "--print-all") (help "Print all elements"))
	  (inspect-plugins)
	  (set! all 'force)
	  (exit 0))
	 (("--print-plugin-auto-install-info" (help "Print a machine-parsable list of features the specified plugin provides. Useful in connection with external automatic plugin installation mechanisms"))
	  'TODO)
	 (("--version" (help "Print version information and exit"))
	  (print "bgst-inspect version " (bgst-inspect-version))
	  (print (gst-version))
	  (exit 0))
	 (else
	  (let ((factory (gst-element-factory-find else)))
	     (unless (eq? all 'force) (set! all #f))
	     (if (isa? factory gst-element-factory)
		 (inspect-factory factory)
		 (let ((plugin (gst-registry-find-plugin else)))
		    (if (isa? plugin gst-plugin)
			(inspect-plugin plugin)
			(error 'bgst-insect "No such element or plugin" else)))))))
      (when all
	 (inspect-plugins))))

;*---------------------------------------------------------------------*/
;*    inspect-static-pad-template ...                                  */
;*---------------------------------------------------------------------*/
(define (inspect-static-pad-template tpl)
   (with-access::gst-static-pad-template tpl (direction name-template presence)
      (print "  " (string-upcase (symbol->string direction))
	     " template: '" name-template "'")
      (print "    Availability: " presence)))
	  
;*---------------------------------------------------------------------*/
;*    inspect-factory ...                                              */
;*---------------------------------------------------------------------*/
(define (inspect-factory factory)
   (print "Factory Details:")
   (with-access::gst-element-factory factory (longname
					      plugin-name
					      klass description author rank
					      static-pad-templates)
      (print-row 14 "Long name:" longname)
      (print-row 14 "Class:" klass)
      (print-row 14 "Description:" description)
      (print-row 14 "Authors(s):" author)
      (print-row 14 "Rank:" rank)
      (newline)
      (let ((plugin (gst-registry-find-plugin plugin-name)))
	 (when (isa? plugin gst-plugin) (inspect-plugin-details plugin)))
      (newline)
      (let ((el (gst-element-factory-create factory "")))
	 (print "Implemented Interfaces:")
	 (for-each (lambda (i) (print "  " i))
	    (with-access::gst-element el (interface-list) interface-list))
	 (newline)
	 (print "Pad Templates:")
	 (for-each inspect-static-pad-template static-pad-templates)
	 (newline)
	 (print "Element Properties:")
	 (let loop ((l (gst-object-property-list el)))
	    (when (pair? l)
	       (print "  " (car l) " Current: \"" (cadr l) "\"")
	       (loop (cddr l)))))))

;*---------------------------------------------------------------------*/
;*    print-row ...                                                    */
;*---------------------------------------------------------------------*/
(define (print-row tab th td)
   (print "  " th (make-string (- tab (string-length th)) #\space) td))

;*---------------------------------------------------------------------*/
;*    inspect-plugin-feature ::gst-plugin-feature ...                  */
;*---------------------------------------------------------------------*/
(define-generic (inspect-plugin-feature feature::gst-plugin-feature)
   (with-access::gst-plugin-feature feature (name)
      (print "  " name ": ")))

;*---------------------------------------------------------------------*/
;*    inspect-plugin-feature ::gst-element-factory ...                 */
;*---------------------------------------------------------------------*/
(define-method (inspect-plugin-feature feature::gst-element-factory)
   (with-access::gst-element-factory feature (name longname)
      (print "  " name ": " longname)))

;*---------------------------------------------------------------------*/
;*    inspect-plugin-details ...                                       */
;*---------------------------------------------------------------------*/
(define (inspect-plugin-details plugin)
   (print "Plugin Details:")
   (with-access::gst-plugin plugin (name
				    description filename version license source
				    package origin)
      (print-row 20 "Name:" name)
      (print-row 20 "Description:" description)
      (print-row 20 "Filename:" filename)
      (print-row 20 "Version:" version)
      (print-row 20 "License:" license)
      (print-row 20 "Source module:" source)
      (print-row 20 "Package:" package)
      (print-row 20 "Origin:" origin)))

;*---------------------------------------------------------------------*/
;*    inspect-plugin ...                                               */
;*---------------------------------------------------------------------*/
(define (inspect-plugin plugin)
   (inspect-plugin-details plugin)
   (newline)
   (for-each inspect-plugin-feature
	     (gst-registry-feature-list-by-plugin plugin)))

;*---------------------------------------------------------------------*/
;*    inspect-plugins ...                                              */
;*---------------------------------------------------------------------*/
(define (inspect-plugins)
   (for-each (lambda (p)
		(with-access::gst-plugin p (name)
		   (print name ":")))
	     (gst-registry-plugin-list)))
      
