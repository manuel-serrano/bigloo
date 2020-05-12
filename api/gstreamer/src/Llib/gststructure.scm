;*=====================================================================*/
;*    .../project/bigloo/api/gstreamer/src/Llib/gststructure.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Jan  4 06:19:50 2008                          */
;*    Last change :  Tue Nov 15 17:40:29 2011 (serrano)                */
;*    Copyright   :  2008-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    GstStructure                                                     */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __gstreamer_gststructure
   
   (include "gst.sch")
   
   (import  __gstreamer_gsterror)
   
   (export  (class gst-structure
	       (%gst-structure-init)
	       ($builtin::$gst-structure
		  (default ($gst-structure-nil)))
	       ($finalizer::obj read-only
		  (default (lambda (o)
			      (with-access::gst-structure o ($builtin)
				 ($gst-structure-free! $builtin)))))
	       (name::string
		  (get (lambda (o)
			  (with-access::gst-structure o ($builtin)
			     ($gst-structure-get-name $builtin))))
		  (set (lambda (o v)
			  (with-access::gst-structure o ($builtin)
			     ($gst-structure-set-name! $builtin v))
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
      (when ($gst-structure-null? $builtin)
	 (raise (instantiate::&gst-create-error
		   (proc '%gst-structure-init)
		   (msg "Illegal gst-structure")
		   (obj o))))
      (cond
	 ((procedure? $finalizer)
	  ($gst-add-finalizer! o $finalizer))
	 ($finalizer
	  ;;;
	  ($gst-add-finalizer! o (lambda (o)
				    (with-access::gst-structure o ($builtin)
				       ($gst-structure-free! $builtin))))))
      o))

;*---------------------------------------------------------------------*/
;*    gst-structure-property-list ...                                  */
;*---------------------------------------------------------------------*/
(define (gst-structure-property-list o::gst-structure)
   (with-access::gst-structure o ($builtin)
      ($gst-structure-property-list $builtin)))

;*---------------------------------------------------------------------*/
;*    gst-structure-property ...                                       */
;*---------------------------------------------------------------------*/
(define (gst-structure-property o::gst-structure prop)
   (with-access::gst-structure o ($builtin)
      ($gst-structure-get-property $builtin (keyword->string! prop))))

;*---------------------------------------------------------------------*/
;*    gst-structure-property-set! ...                                  */
;*---------------------------------------------------------------------*/
(define (gst-structure-property-set! o::gst-structure prop val)
   (with-access::gst-structure o ($builtin)
      ($gst-structure-set-property! $builtin (keyword->string! prop) val)))
