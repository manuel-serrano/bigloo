;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/api/sqlite/src/Wlib/wsqlite.wat     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Tue Jul 29 12:54:52 2025                          */
;*    Last change :  Wed Sep 10 16:42:55 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    SQLITE Wasm binding                                              */
;*=====================================================================*/

(module $__sqlite
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   ;;(type $$sqlite (sub $foreign))
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__js_sqlite" "nil" (func $sqlite_nil (result externref)))
   (import "__js_sqlite" "open" (func $sqlite_open (param i32 i32) (result externref)))
   (import "__js_sqlite" "close" (func $sqlite_close (param externref)))
   (import "__js_sqlite" "exec" (func $sqlite_exec (param externref i32 i32)))
   
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   
   ;; -----------------------------------------------------------------
   ;; Unimplemented functions and variables
   ;; -----------------------------------------------------------------
   
   (func $sqlite3_config (export "sqlite3_config")
      (param $i i32)
      (result i32)
      (return (local.get $i)))
   
   (func $bgl_sqlite_eval (export "bgl_sqlite_eval")
      (param $db externref)
      (param $proc (ref $procedure))
      (param $cmd (ref $bstring))
      (param $arg (ref eq))
      (result (ref eq))
      (return (global.get $BUNSPEC)))
   
   (func $bgl_sqlite_get (export "bgl_sqlite_get")
      (param $db externref)
      (param $proc (ref $procedure))
      (param $cmd (ref $bstring))
      (param $arg (ref eq))
      (result (ref eq))
      (return (global.get $BUNSPEC)))
   
   (func $bgl_sqlite_map (export "bgl_sqlite_map")
      (param $db externref)
      (param $proc (ref $procedure))
      (param $cmd (ref $bstring))
      (param $arg (ref eq))
      (result (ref eq))
      (return (global.get $BUNSPEC)))
   
   (func $bgl_sqlite_for_each (export "bgl_sqlite_for_each")
      (param $db externref)
      (param $proc (ref $procedure))
      (param $cmd (ref $bstring))
      (param $arg (ref eq))
      (result (ref eq))
      (return (global.get $BUNSPEC)))
   
   (func $bgl_sqlite_run (export "bgl_sqlite_run")
      (param $db externref)
      (param $cmd (ref $bstring))
      (param $arg (ref eq))
      (result (ref eq))
      (return (global.get $BUNSPEC)))
   
   (global $SQLITE_CONFIG_SINGLETHREAD (export "SQLITE_CONFIG_SINGLETHREAD")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_MULTITHREAD (export "SQLITE_CONFIG_MULTITHREAD")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_SERIALIZED (export "SQLITE_CONFIG_SERIALIZED")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_MALLOC (export "SQLITE_CONFIG_MALLOC")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_GETMALLOC (export "SQLITE_CONFIG_GETMALLOC")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_SCRATCH (export "SQLITE_CONFIG_SCRATCH")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_PAGECACHE (export "SQLITE_CONFIG_PAGECACHE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_HEAP (export "SQLITE_CONFIG_HEAP")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_MEMSTATUS (export "SQLITE_CONFIG_MEMSTATUS")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_MUTEX (export "SQLITE_CONFIG_MUTEX")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_GETMUTEX (export "SQLITE_CONFIG_GETMUTEX")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_LOOKASIDE (export "SQLITE_CONFIG_LOOKASIDE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_PCACHE (export "SQLITE_CONFIG_PCACHE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_GETPCACHE (export "SQLITE_CONFIG_GETPCACHE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_LOG (export "SQLITE_CONFIG_LOG")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_URI (export "SQLITE_CONFIG_URI")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_PCACHE2 (export "SQLITE_CONFIG_PCACHE2")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_GETPCACHE2 (export "SQLITE_CONFIG_GETPCACHE2")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_COVERING_INDEX_SCAN (export "SQLITE_CONFIG_COVERING_INDEX_SCAN")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_SQLLOG (export "SQLITE_CONFIG_SQLLOG")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_MMAP_SIZE (export "SQLITE_CONFIG_MMAP_SIZE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_WIN32_HEAPSIZE (export "SQLITE_CONFIG_WIN32_HEAPSIZE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_PCACHE_HDRSZ (export "SQLITE_CONFIG_PCACHE_HDRSZ")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_PMASZ (export "SQLITE_CONFIG_PMASZ")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_STMTJRNL_SPILL (export "SQLITE_CONFIG_STMTJRNL_SPILL")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_SMALL_MALLOC (export "SQLITE_CONFIG_SMALL_MALLOC")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_SORTERREF_SIZE (export "SQLITE_CONFIG_SORTERREF_SIZE")
      i32 (i32.const 0))
   (global $SQLITE_CONFIG_MEMDB_MAXSIZE (export "SQLITE_CONFIG_MEMDB_MAXSIZE")
      i32 (i32.const 0))
   
   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------
   
   ;; bgl_sqlite_nil
   (func $bgl_sqlite_nil (export "bgl_sqlite_nil")
      (result externref)
      (return (call $sqlite_nil)))
   
   ;; bgl_sqlite_open
   (func $bgl_sqlite_open (export "bgl_sqlite_open")
      (param $path (ref $bstring))
      (result externref)
      (call $store_string (local.get $path) (i32.const 128))
      (return (call $sqlite_open (i32.const 128) (array.len (local.get $path)))))
   
   ;; bgl_sqlite_close
   (func $bgl_sqlite_close (export "bgl_sqlite_close")
      (param $db externref)
      (param $odb (ref eq))
      (call $sqlite_close (local.get $db)))
   
   ;; bgl_sqlite_exec
   (func $bgl_sqlite_exec (export "bgl_sqlite_exec")
      (param $db externref)
      (param $cmd (ref $bstring))
      (param $odb (ref eq))
      (result (ref eq))
      (call $store_string (local.get $cmd) (i32.const 128))
      (call $sqlite_exec (local.get $db) (i32.const 128) (array.len (local.get $cmd)))
      (return (global.get $BUNSPEC)))
   )

      

