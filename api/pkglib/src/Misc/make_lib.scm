;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/pkglib/src/Misc/make_lib.scm     */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Nov  6 15:09:37 2001                          */
;*    Last change :  Sun Apr 20 20:00:14 2008 (serrano)                */
;*    Copyright   :  2001-08 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the heap file.                          */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __pkglib_makelib
   
   (library sqlite)
   
   (import __pkglib_param
	   __pkglib_database
	   __pkglib_repo
	   __pkglib_interface
	   __pkglib_package
	   __pkglib_misc)
   
   (eval    (export-all)))
