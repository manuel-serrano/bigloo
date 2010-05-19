;*=====================================================================*/
;*    .../prgm/project/bigloo/api/calendar/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Apr 20 18:17:27 2008 (serrano)                */
;*    Copyright   :  2001-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __calendar_makelib

   (import __calendar_types
	   __calendar_ical
	   __calendar_utils)

   (eval   (export-all)
	   (class calendar)
	   (class calevent)
	   (class caltodo)
	   (class calrecurrence)))
