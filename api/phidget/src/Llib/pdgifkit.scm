;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgifkit.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Sep 22 10:43:19 2010                          */
;*    Last change :  Wed Sep 22 16:52:25 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget Interface Kit                                            */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_ifkit

   (include "pdg.sch")

   (extern (export $make-ifkit "bgl_ifkit_new")
	   (macro $pdg-ifkit-builtin&::$pdg-ifkit* (::phidget-ifkit)
		  "BGL_PHIDGET_IFKIT_BUILTIN"))

   (import __phidget_types
	   __phidget
	   __phidget_event
	   __phidget_phidget)

   (export  (class phidget-ifkit::phidget)

	    ($make-ifkit::obj ::$pdg-ifkit)))

;*---------------------------------------------------------------------*/
;*    $make-ifkit ...                                                  */
;*---------------------------------------------------------------------*/
(define ($make-ifkit ifkit::$pdg-ifkit)
   (instantiate::phidget-ifkit
      ($builtin ($pdg-ifkit->phidget ifkit))))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-ifkit)
   (phidget-init!)
   (with-access::phidget-ifkit o ($builtin)
      (phidget-return
       ($pdg-ifkit-create ($pdg-ifkit-builtin& o))
       "phidget-ikfkit" o)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-ifkit ...                  */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-ifkit event proc)
   (with-access::phidget-ifkit o ($builtin)
      (phidget-return
       ($pdg-phidget-ifkit-add-event-listener!
	($pdg-phidget->ifkit $builtin) event o proc)
       "phidget-add-event-listener!" o)))

