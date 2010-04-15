;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Apr 20 19:54:02 2008 (serrano)                */
;*    Copyright   :  2001-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_makelib

   (import __sqlite_sqlite
	   __sqlite_sqltiny)

   (eval   (export-all)
	   (class %sqlite)
	   (class sqlite)
	   (class sqltiny)))
