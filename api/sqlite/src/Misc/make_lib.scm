;*=====================================================================*/
;*    .../project/bigloo/5.0.x/api/sqlite/src/Misc/make_lib.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Jul 12 06:32:05 2026 (serrano)                */
;*    Copyright   :  2001-26 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __sqlite_makelib

   (import __sqlite_types
	   __sqlite
	   __sqlite_sqltiny)

   (eval   (export-all)
	   (class %sqlite)
	   (class sqltiny))

   (cond-expand
      ((and bigloo-c (not sqltiny))
       (eval (class sqlite)))))
