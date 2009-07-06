;*=====================================================================*/
;*    serrano/prgm/project/bigloo/comptime/Engine/configure.scm        */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Fri Dec 29 09:31:00 2000                          */
;*    Last change :  Fri Jun 24 12:11:07 2005 (serrano)                */
;*    Copyright   :  2000-05 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The machine dependent configuration.                             */
;*    -------------------------------------------------------------    */
;*    In order to avoid daunting bootstrap problem, I have decided not */
;*    to produce this file automatically. It is written and maintained */
;*    by (my) hand.                                                    */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The module                                                       */
;*---------------------------------------------------------------------*/
(module engine_configure
   
   (extern (macro bgl-foreign-dlopen-init::string "BGL_DYNAMIC_LOAD_INIT")
	   (macro bgl-foreign-BDB_LIBRARY_MAGIC_NUMBER::int "BDB_LIBRARY_MAGIC_NUMBER"))
   
   (java   (class bgl-foreign
	      (field static dlopen-init::string "BGL_DYNAMIC_LOAD_INIT")
	      (field static BDB_LIBRARY_MAGIC_NUMBER::int "BDB_LIBRARY_MAGIC_NUMBER")
	      "bigloo.foreign")))
 
