;*=====================================================================*/
;*    .../prgm/project/bigloo/api/phidget/src/Llib/pdgmanager.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Sep 21 11:30:44 2010                          */
;*    Last change :  Wed Sep 22 16:57:34 2010 (serrano)                */
;*    Copyright   :  2010 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    Phidget manager                                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __phidget_manager

   (include "pdg.sch")

   (extern (macro $pdg-manager-builtin&::$pdg-manager* (::phidget-manager)
		  "BGL_PHIDGET_MANAGER_BUILTIN"))
   
   (import __phidget_types
	   __phidget
	   __phidget_event)

   (export (class phidget-manager::%phidget
	       ($builtin::$pdg-manager (default ($pdg-manager-nil))))

	   (phidget-manager-open ::phidget-manager)
	   (phidget-manager-close ::phidget-manager)))

;*---------------------------------------------------------------------*/
;*    %phidget-init ...                                                */
;*---------------------------------------------------------------------*/
(define-method (%phidget-init o::phidget-manager)
   (phidget-init!)
   (with-access::phidget-manager o ($builtin)
      (phidget-return
       ($pdg-manager-create ($pdg-manager-builtin& o))
       "phidget-manager" o)))

;*---------------------------------------------------------------------*/
;*    phidget-add-event-listener! ::phidget-manager ...                */
;*---------------------------------------------------------------------*/
(define-method (phidget-add-event-listener! o::phidget-manager event proc)
   (with-access::phidget-manager o ($builtin)
      (phidget-return
       ($pdg-manager-add-event-listener! $builtin event o proc)
       "phidget-add-event-listener!" o)))

;*---------------------------------------------------------------------*/
;*    phidget-manager-open ...                                         */
;*---------------------------------------------------------------------*/
(define (phidget-manager-open o::phidget-manager)
   (with-access::phidget-manager o ($builtin)
      (phidget-return
       ($pdg-manager-open $builtin)
       "phidget-manager-open" o)))

;*---------------------------------------------------------------------*/
;*    phidget-manager-close ...                                        */
;*---------------------------------------------------------------------*/
(define (phidget-manager-close o::phidget-manager)
   (with-access::phidget-manager o ($builtin)
      (phidget-return
       ($pdg-manager-close $builtin)
       "phidget-manager-close" o)))

