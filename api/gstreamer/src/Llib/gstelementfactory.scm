;*=====================================================================*/
;*    .../bigloo/api/gstreamer/src/Llib/gstelementfactory.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 06:53:19 2008                          */
;*    Last change :  Tue Nov 15 17:00:11 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstElementFactory                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstelementfactory
   
   (include "gst.sch")
   
   (use	    __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)

   (import  __gstreamer_gstpluginfeature
	    __gstreamer_gstelement
	    __gstreamer_gstpad)
   
   (export  (class gst-element-factory::gst-plugin-feature
	       (longname::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-element-factory o ($builtin)
			     ($gst-element-factory-get-metadata
				($gst-element-factory $builtin)
				$gst-element-metadata-long-name)))))
	       (klass::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-element-factory o ($builtin)
			     ($gst-element-factory-get-metadata
				($gst-element-factory $builtin)
				$gst-element-metadata-klass)))))
	       (description::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-element-factory o ($builtin)
			     ($gst-element-factory-get-metadata
				($gst-element-factory $builtin)
				$gst-element-metadata-description)))))
	       (author::string
		  read-only
		  (get (lambda (o)
			  (with-access::gst-element-factory o ($builtin)
			     ($gst-element-factory-get-metadata
				($gst-element-factory $builtin)
				$gst-element-metadata-author)))))
	       (uri-protocols::pair-nil
		  read-only
		  (get (lambda (o)
			  (with-access::gst-element-factory o ($builtin)
			     ($gst-element-factory-get-uri-protocols
				($gst-element-factory
				   $builtin))))))
	       (static-pad-templates::pair-nil
		  read-only
		  (get (lambda (o)
			  (with-access::gst-element-factory o ($builtin)
			     ($gst-element-factory-get-static-pad-templates
				($gst-element-factory
				   $builtin)))))))
	    
	    ($make-gst-element-factory::obj ::$gst-element-factory ::obj)
	    
	    (gst-element-factory-make::gst-element ::bstring . a)
	    (gst-element-factory-create::gst-element ::gst-element-factory . a)
	    (gst-element-factory-find::obj ::bstring)
	    (gst-element-factory-has-interface?::bool ::gst-element-factory ::bstring)
	    (gst-element-factory-can-sink-all-caps?::bool ::gst-element-factory ::gst-caps)
	    (gst-element-factory-can-src-all-caps?::bool ::gst-element-factory ::gst-caps))
   
   (extern  (export $make-gst-element-factory "bgl_gst_element_factory_new")))

;*---------------------------------------------------------------------*/
;*    $make-gst-element-factory ...                                    */
;*---------------------------------------------------------------------*/
(define ($make-gst-element-factory factory::$gst-element-factory finalizer)
   (instantiate::gst-element-factory
      ($builtin ($gst-element-factory->object factory))
      ($finalizer finalizer)))

;*---------------------------------------------------------------------*/
;*    gst-object->string ::gst-element-factory ...                     */
;*---------------------------------------------------------------------*/
(define-method (object-display o::gst-element-factory . port)
   (with-access::gst-element-factory o (longname)
      (let ((p (if (pair? port) (car port) (current-output-port))))
	 (display "<" p)
	 (display (find-runtime-type o) p)
	 (display " refcount=" p)
	 (with-access::gst-object o ($builtin)
	    (display ($gst-object-refcount $builtin) p))
	 (display " longname=" p)
	 (display longname p)
	 (display ">" p))))

;*---------------------------------------------------------------------*/
;*    gst-element-init ...                                             */
;*---------------------------------------------------------------------*/
(define (gst-element-init $el::$gst-element
			  proc::symbol
			  factoryname::bstring
			  name::bstring
			  args)
   (if ($gst-element-null? $el)
       (raise (instantiate::&gst-create-error
		 (proc proc)
		 (msg "Cannot create object")
		 (obj (cons* factoryname name args))))
       (let ((el ($gst-object-to-obj ($gst-element->object $el) #f)))
	  (let loop ((args args))
	     (cond
		((null? args)
		 el)
		((null? (cdr args))
		 (error proc "Illegal argument" (car args)))
		(else
		 (gst-object-property-set! el (car args) (cadr args))
		 (loop (cddr args))))))))
			  
;*---------------------------------------------------------------------*/
;*    gst-element-factory-make ...                                     */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-make factoryname . a)
   (let ((name::string (if (and (pair? a) (string? (car a)))
			   (car a)
			   ($gst-element-factory-name-nil)))
	 (rest (if (and (pair? a) (string? (car a))) (cdr a) a)))
      (gst-element-init ($gst-element-factory-make factoryname name)
			'gst-element-factory-make
			factoryname
			name
			rest)))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-create ...                                   */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-create factory  . a)
   (with-access::gst-element-factory factory ($builtin (fname name))
      (let* ((name::string (if (and (pair? a) (string? (car a)))
			       (car a)
			       ($gst-element-factory-name-nil)))
	     (rest (if (and (pair? a) (string? (car a))) (cdr a) a))
	     ($el::$gst-element ($gst-element-factory-create
				   ($gst-element-factory
				      $builtin)
				   name)))
	 (gst-element-init $el 'gst-element-factory-create fname name rest))))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-find ...                                     */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-find name)
   (let ((ef::$gst-element-factory ($gst-element-factory-find name)))
      (unless ($gst-element-factory-null? ef)
	 (instantiate::gst-element-factory
	    ($builtin ($gst-element-factory->object ef))
	    ($finalizer %gst-object-finalize!)))))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-has-interface? ...                           */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-has-interface? factory name)
   (with-access::gst-element-factory factory ($builtin)
      ($gst-element-factory-has-interface?
	 ($gst-element-factory $builtin)
	 name)))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-can-sink-all-caps? ...                       */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-can-sink-all-caps? factory caps)
   (with-access::gst-element-factory factory ($builtin)
      (with-access::gst-caps caps ((caps-builtin $builtin))
	 ($gst-element-factory-can-sink-all-caps?
	    ($gst-element-factory $builtin)
	    caps-builtin))))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-can-src-all-caps? ...                        */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-can-src-all-caps? factory caps)
   (with-access::gst-element-factory factory ($builtin)
      (with-access::gst-caps caps ((caps-builtin $builtin))
	 ($gst-element-factory-can-src-all-caps?
	    ($gst-element-factory $builtin)
	    caps-builtin))))
