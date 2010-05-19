;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Mon Mar 29 09:02:46 2010 (serrano)                */
;*    Copyright   :  2001-10 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __web_makelib

   (import __web_xml
	   __web_cgi
	   __web_feeds
	   __web_webdav
	   __web_html
	   __web_css
	   __web_css-ast
	   __web_date)

   (eval   (class &webdav-access-control-exception)
	   (export-all)))
