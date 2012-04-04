;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgspatial.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Mar 27 09:11:24 2012                          */
;*    Last change :  Wed Apr  4 11:47:05 2012 (serrano)                */
;*    Copyright   :  2012 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget Spatial                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_spatial
   
   (include "pdg.sch")

   (extern (export $make-spatial "bgl_spatial_new")
	   (macro $pdg-spatial-builtin&::$pdg-spatial* (::phidget-spatial)
		  "BGL_PHIDGET_SPATIAL_BUILTIN"))

   (import __phidget_types
	   __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-spatial::phidget
	       (datarate::int
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-spatial-get-datarate
				($pdg-phidget->spatial $builtin) o))))
		  (set (lambda (o v)
			  (with-access::phidget o ($builtin)
			     (phidget-return
				($pdg-phidget-spatial-set-datarate!
				   ($pdg-phidget->spatial $builtin)
				   v)
				"datarate" o)))))
	       (datarate-max::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-spatial-get-datarate-max
				($pdg-phidget->spatial $builtin) o)))))
	       (datarate-min::int
		  read-only
		  (get (lambda (o)
			  (with-access::phidget o ($builtin)
			     ($pdg-phidget-spatial-get-datarate-min
				($pdg-phidget->spatial $builtin) o))))))

	    ($make-spatial::obj ::$pdg-spatial)))

;*---------------------------------------------------------------------*/
;*    $make-spatial ...                                                */
;*---------------------------------------------------------------------*/
(define ($make-spatial spatial::$pdg-spatial)
   (instantiate::phidget-spatial
      ($builtin ($pdg-spatial->phidget spatial))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-spatial)
   (phidget-init!)
   (with-access::phidget-spatial o ($builtin)
      (phidget-return
       ($pdg-spatial-create ($pdg-spatial-builtin& o))
       "phidget-spatial" o)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-spatial ...                */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-spatial event proc)
   (with-access::phidget-spatial o ($builtin)
      (phidget-return
       ($pdg-phidget-spatial-add-event-listener!
	($pdg-phidget->spatial $builtin) event o proc)
       "phidget-add-event-listener!" o)))


