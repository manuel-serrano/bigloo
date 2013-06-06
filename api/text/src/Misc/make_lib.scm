;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/text/src/Misc/make_lib.scm       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Tue Jun  4 15:12:06 2013 (serrano)                */
;*    Copyright   :  2001-13 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file and the _e library        */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __text_makelib
   
   (import __text_bibtex
	   __text_gb2312
	   __text_hyphenation
	   __text_levenshtein)
   
   (eval   (export-all)))

