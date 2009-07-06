;*=====================================================================*/
;*    serrano/prgm/project/bigloo/api/sqlite/src/Llib/sqlite.sch       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 10 14:23:34 2005                          */
;*    Last change :  Thu Feb  8 09:19:49 2007 (serrano)                */
;*    Copyright   :  2005-07 Manuel Serrano                            */
;*    -------------------------------------------------------------    */
;*    The native interfaces of SQLITE                                  */
;*=====================================================================*/

;*---------------------------------------------------------------------*/
;*    The directives                                                   */
;*---------------------------------------------------------------------*/
(directives
   
   (extern
    (include "sqlite3.h")
    (type $sqlite void* "void *")
    
    ($sqlite-open::$sqlite (::string) "bgl_sqlite_open")
    (infix macro $_sqlite-nil::$sqlite () "0L")
    ($sqlite-close::void (::$sqlite ::obj) "bgl_sqlite_close")
    ($sqlite-exec::obj (::$sqlite ::string ::obj) "bgl_sqlite_exec")
    ($sqlite-eval::obj (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_eval")
    ($sqlite-map::pair-nil (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_map")))
