;*=====================================================================*/
;*    serrano/prgm/project/bigloo/bglpkg/etc/make_lib.scm              */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Jan  2 13:16:31 2007                          */
;*    Last change :  Wed Nov 14 13:10:14 2007 (serrano)                */
;*    Copyright   :  2006-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The module used to build the Snow heap files.                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module __@LIBNAME@_makelib
   
   (option (set! *dlopen-init* "@DLOADSYM@_e"))
   
   (import @MODULES@)
   
   (eval   (export-all)
	   @CLASSES@))

(cond-expand
   (bigloo-eval
    (eval '(begin @MACROS@))))
