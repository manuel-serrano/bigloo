;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/api/sqlite/src/Wlib/wsqlite.wat     */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Tue Jul 29 12:54:52 2025                          */
;*    Last change :  Tue Sep  9 09:19:08 2025 (serrano)                */
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

   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   
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
      (call $sqlite_close $db)))
      

