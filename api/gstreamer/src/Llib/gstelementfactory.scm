;*=====================================================================*/
;*    .../bigloo/api/gstreamer/src/Llib/gstelementfactory.scm          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Jan  2 06:53:19 2008                          */
;*    Last change :  Tue Jul 29 11:48:16 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstElementFactory                                                */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gstelementfactory
   
   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject
	    __gstreamer_gstpluginfeature
	    __gstreamer_gstelement
	    __gstreamer_gstpad
	    __gstreamer_gstcaps
	    __gstreamer_gststructure)
   
   (export  (class gst-element-factory::gst-plugin-feature
	       (longname::string
		read-only
		(get (lambda (o)
			($gst-element-factory-get-longname
			 ($gst-element-factory
			  (gst-element-factory-$builtin o))))))
	       (klass::string
		read-only
		(get (lambda (o)
			($gst-element-factory-get-klass
			 ($gst-element-factory
			  (gst-element-factory-$builtin o))))))
	       (description::string
		read-only
		(get (lambda (o)
			($gst-element-factory-get-description
			 ($gst-element-factory
			  (gst-element-factory-$builtin o))))))
	       (author::string
		read-only
		(get (lambda (o)
			($gst-element-factory-get-author
			 ($gst-element-factory
			  (gst-element-factory-$builtin o))))))
	       (uri-protocols::pair-nil
		read-only
		(get (lambda (o)
			($gst-element-factory-get-uri-protocols
			 ($gst-element-factory
			  (gst-element-factory-$builtin o))))))
	       (static-pad-templates::pair-nil
		read-only
		(get (lambda (o)
			($gst-element-factory-get-static-pad-templates
			 ($gst-element-factory
			  (gst-element-factory-$builtin o)))))))
	    
	    ($make-gst-element-factory::obj ::$gst-element-factory ::obj)
	    
	    (gst-element-factory-make::gst-element ::bstring . a)
	    (gst-element-factory-create::gst-element ::gst-element-factory . a)
	    (gst-element-factory-find::obj ::bstring)
	    (gst-element-factory-has-interface?::bool ::gst-element-factory ::bstring)
	    (gst-element-factory-can-sink-caps?::bool ::gst-element-factory ::gst-caps)
	    (gst-element-factory-can-src-caps?::bool ::gst-element-factory ::gst-caps))
   
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
	 (display ($gst-object-refcount (gst-object-$builtin o)) p)
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
   (let* ((name::string (if (and (pair? a) (string? (car a)))
			    (car a)
			    ($gst-element-factory-name-nil)))
	  (rest (if (and (pair? a) (string? (car a))) (cdr a) a))
	  ($el::$gst-element ($gst-element-factory-create
			      ($gst-element-factory
			       (gst-element-factory-$builtin factory))
			      name)))
      (gst-element-init $el
			'gst-element-factory-create
			(gst-element-factory-name factory)
			name
			rest)))

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
   ($gst-element-factory-has-interface?
    ($gst-element-factory (gst-element-factory-$builtin factory))
    name))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-can-sink-caps? ...                           */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-can-sink-caps? factory caps)
   ($gst-element-factory-can-sink-caps?
    ($gst-element-factory (gst-element-factory-$builtin factory))
    (gst-caps-$builtin caps)))

;*---------------------------------------------------------------------*/
;*    gst-element-factory-can-src-caps? ...                            */
;*---------------------------------------------------------------------*/
(define (gst-element-factory-can-src-caps? factory caps)
   ($gst-element-factory-can-src-caps?
    ($gst-element-factory (gst-element-factory-$builtin factory))
    (gst-caps-$builtin caps)))
