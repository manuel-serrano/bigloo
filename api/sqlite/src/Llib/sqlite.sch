;*=====================================================================*/
;*    .../project/bigloo/bigloo/api/sqlite/src/Llib/sqlite.sch         */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Thu Nov 10 14:23:34 2005                          */
;*    Last change :  Sun Oct 15 07:44:26 2023 (serrano)                */
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
    
    (macro $sqlite-config::int (::int) "sqlite3_config")
    ($sqlite-open::$sqlite (::string) "bgl_sqlite_open")
    (infix macro $_sqlite-nil::$sqlite () "0L")
    ($sqlite-close::void (::$sqlite ::obj) "bgl_sqlite_close")
    ($sqlite-exec::obj (::$sqlite ::string ::obj) "bgl_sqlite_exec")
    ($sqlite-eval::obj (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_eval")
    ($sqlite-get::obj (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_get")
    ($sqlite-map::pair-nil (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_map")
    ($sqlite-for-each::obj (::$sqlite ::procedure ::string ::obj) "bgl_sqlite_for_each")
    ($sqlite-run::obj (::$sqlite ::string ::obj) "bgl_sqlite_run")

    
    (macro $SQLITE_CONFIG_SINGLETHREAD::int "SQLITE_CONFIG_SINGLETHREAD")
    (macro $SQLITE_CONFIG_MULTITHREAD::int "SQLITE_CONFIG_MULTITHREAD")
    (macro $SQLITE_CONFIG_SERIALIZED::int "SQLITE_CONFIG_SERIALIZED")
    (macro $SQLITE_CONFIG_MALLOC::int "SQLITE_CONFIG_MALLOC")
    (macro $SQLITE_CONFIG_GETMALLOC::int "SQLITE_CONFIG_GETMALLOC")
    (macro $SQLITE_CONFIG_SCRATCH::int "SQLITE_CONFIG_SCRATCH")
    (macro $SQLITE_CONFIG_PAGECACHE::int "SQLITE_CONFIG_PAGECACHE")
    (macro $SQLITE_CONFIG_HEAP::int "SQLITE_CONFIG_HEAP")
    (macro $SQLITE_CONFIG_MEMSTATUS::int "SQLITE_CONFIG_MEMSTATUS")
    (macro $SQLITE_CONFIG_MUTEX::int "SQLITE_CONFIG_MUTEX")
    (macro $SQLITE_CONFIG_GETMUTEX::int "SQLITE_CONFIG_GETMUTEX")
    (macro $SQLITE_CONFIG_LOOKASIDE::int "SQLITE_CONFIG_LOOKASIDE")
    (macro $SQLITE_CONFIG_PCACHE::int "SQLITE_CONFIG_PCACHE")
    (macro $SQLITE_CONFIG_GETPCACHE::int "SQLITE_CONFIG_GETPCACHE")
    (macro $SQLITE_CONFIG_LOG::int "SQLITE_CONFIG_LOG")
    (macro $SQLITE_CONFIG_URI::int "SQLITE_CONFIG_URI")
    (macro $SQLITE_CONFIG_PCACHE2::int "SQLITE_CONFIG_PCACHE2")
    (macro $SQLITE_CONFIG_GETPCACHE2::int "SQLITE_CONFIG_GETPCACHE2")
    (macro $SQLITE_CONFIG_COVERING_INDEX_SCAN::int "SQLITE_CONFIG_COVERING_INDEX_SCAN")
    (macro $SQLITE_CONFIG_SQLLOG::int "SQLITE_CONFIG_SQLLOG")
    (macro $SQLITE_CONFIG_MMAP_SIZE::int "SQLITE_CONFIG_MMAP_SIZE")
    (macro $SQLITE_CONFIG_WIN32_HEAPSIZE::int "SQLITE_CONFIG_WIN32_HEAPSIZE")
    (macro $SQLITE_CONFIG_PCACHE_HDRSZ::int "SQLITE_CONFIG_PCACHE_HDRSZ")
    (macro $SQLITE_CONFIG_PMASZ::int "SQLITE_CONFIG_PMASZ")
    (macro $SQLITE_CONFIG_STMTJRNL_SPILL::int "SQLITE_CONFIG_STMTJRNL_SPILL")
    (macro $SQLITE_CONFIG_SMALL_MALLOC::int "SQLITE_CONFIG_SMALL_MALLOC")
    (macro $SQLITE_CONFIG_SORTERREF_SIZE::int "SQLITE_CONFIG_SORTERREF_SIZE")
    (macro $SQLITE_CONFIG_MEMDB_MAXSIZE::int "SQLITE_CONFIG_MEMDB_MAXSIZE")))
