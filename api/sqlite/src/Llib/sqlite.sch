;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/sqlite/src/Llib/sqlite.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 10 14:23:34 2005                          */
;*    Last change :  Thu Oct 12 17:25:49 2023 (serrano)                */
;*    Copyright   :  2005-23 Manuel Serrano                            */
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
    ($sqlite-get::obj (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_get")
    ($sqlite-map::pair-nil (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_map")
    ($sqlite-for-each::pair-nil (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_for_each")))
