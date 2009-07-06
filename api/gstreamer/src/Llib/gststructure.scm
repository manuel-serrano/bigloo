;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gststructure.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  4 06:19:50 2008                          */
;*    Last change :  Mon Feb 11 15:14:40 2008 (serrano)                */
;*    Copyright   :  2008 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    GstStructure                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gststructure
   
   (include "gst.sch")
   
   (import  __gstreamer_gsterror
	    __gstreamer_gstobject)
   
   (export  (class gst-structure
	       (%gst-structure-init)
	       ($builtin::$gst-structure (default ($gst-structure-nil)))
	       ($finalizer::obj read-only (default (lambda (o)
						      ($gst-structure-free!
						       (gst-structure-$builtin
							o)))))
	       (name::string
		(get (lambda (o)
			($gst-structure-get-name
			 (gst-structure-$builtin o))))
		(set (lambda (o v)
			($gst-structure-set-name!
			 (gst-structure-$builtin o)
			 v)
			v))))

	    (%gst-structure-init ::gst-structure)

	    (gst-structure-property-list::pair-nil ::gst-structure)
	    (gst-structure-property ::gst-structure ::keyword)
	    (gst-structure-property-set! ::gst-structure ::keyword ::bstring)))

;*---------------------------------------------------------------------*/
;*    %gst-structure-init ::gst-structure ...                          */
;*---------------------------------------------------------------------*/
(define (%gst-structure-init o::gst-structure)
   (with-access::gst-structure o ($builtin $finalizer)
      (when ($gst-element-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-structure-init)
		   (msg "Illegal gst-structure")
		   (obj o))))
      (cond
	 ((procedure? $finalizer)
	  ($gst-add-finalizer! o $finalizer))
	 ($finalizer
	  ($gst-add-finalizer! o (lambda (o)
				    ($gst-structure-free!
				     (gst-structure-$builtin o))))))
      o))

;*---------------------------------------------------------------------*/
;*    gst-structure-property-list ...                                  */
;*---------------------------------------------------------------------*/
(define (gst-structure-property-list o::gst-structure)
   ($gst-structure-property-list (gst-structure-$builtin o)))

;*---------------------------------------------------------------------*/
;*    gst-structure-property ...                                       */
;*---------------------------------------------------------------------*/
(define (gst-structure-property o::gst-structure prop)
   ($gst-structure-get-property (gst-structure-$builtin o)
			    (keyword->string! prop)))

;*---------------------------------------------------------------------*/
;*    gst-structure-property-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (gst-structure-property-set! o::gst-structure prop val)
   ($gst-structure-set-property! (gst-structure-$builtin o)
			     (keyword->string! prop) val))
