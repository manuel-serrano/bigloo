;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/avahi/src/Misc/make_lib.scm      */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Wed Dec 14 13:32:14 2011 (serrano)                */
;*    Copyright   :  2001-11 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __avahi_makelib

   (import __avahi_avahi)

   (eval   (export-all)

           (class &avahi-error)
           (class &avahi-collision-error)

	   (class avahi-poll)
	   (class avahi-simple-poll)
	   (class avahi-threaded-poll)
	   (class avahi-client)))
