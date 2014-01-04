;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/web/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sat Jan  4 14:43:57 2014 (serrano)                */
;*    Copyright   :  2001-14 Manuel Serrano                            */
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
	   __web_date
	   __web_json)

   (eval   (class &webdav-access-control-exception)
	   (class css-uri)
	   (class css-ext)
	   (class css-stylesheet)
	   (class css-charset)
	   (class css-comment)
	   (class css-import)
	   (class css-media)
	   (class css-media-query)
	   (class css-page)
	   (class css-fontface)
	   (class css-pseudopage)
	   (class css-ruleset)
	   (class css-selector)
	   (class css-selector-class)
	   (class css-selector-hash)
	   (class css-selector-attr)
	   (class css-selector-name)
	   (class css-selector-pseudo)
	   (class css-declaration)
	   (class css-function)
	   (class css-hash-color)
	   
	   (export-all)))
