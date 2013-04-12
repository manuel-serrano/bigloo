;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/upnp/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Fri Apr 12 17:06:03 2013 (serrano)                */
;*    Copyright   :  2001-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __upnp_makelib
   
   (import __upnp_soap
	   __upnp_ssdp
	   __upnp_content-directory)
   
   (eval   (class ssdp-header)
           (class ssdp-search-response)
	   (class ssdp-advertisement)
	   (class ssdp-root)
	   
	   (export-all)))



