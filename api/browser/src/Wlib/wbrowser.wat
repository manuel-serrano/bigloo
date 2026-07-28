;*=====================================================================*/
;*    .../project/bigloo/5.0.x/api/browser/src/Wlib/wbrowser.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Tue Jul 29 12:54:52 2025                          */
;*    Last change :  Tue Jul 28 11:08:47 2026 (serrano)                */
;*    Copyright   :  2025-26 manuel serrano                            */
;*    -------------------------------------------------------------    */
;*    DOM Wasm binding                                                 */
;*=====================================================================*/

(module $__browser
   
   ;; -----------------------------------------------------------------
   ;; Imports 
   ;; -----------------------------------------------------------------
   
   (import "__bigloo" "bgl_store_string" (func $store_string (param (ref $bstring)) (param i32)))
   (import "__bigloo" "bgl_load_string" (func $load_string (param i32) (param i32) (result (ref $bstring))))
   (import "__bigloo" "BUNSPEC" (global $BUNSPEC (ref eq)))
  
   (import "__browser" "getElementById" (func $getElementById (param i32 i32) (result externref)))
   (import "__browser" "innerHTMLset" (func $innerHTMLset (param externref i32 i32)))
   (import "__browser" "innerHTMLget" (func $innerHTMLget (param externref i32) (result i32)))
   (import "__browser" "alert" (func $alert (param i32 i32)))

   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------
   
   (func $bgl_get_element_by_id (export "bgl_get_element_by_id")
      (param $id (ref $bstring))
      (result externref)
      (call $store_string (local.get $id) (i32.const 128))
      (return_call $getElementById (i32.const 128) (array.len (local.get $id))))

   (func $bgl_inner_html_set (export "bgl_inner_html_set")
      (param $el externref)
      (param $html (ref $bstring))
      (result (ref eq))
      (call $store_string (local.get $html) (i32.const 128))
      (call $innerHTMLset (local.get $el) (i32.const 128) (array.len (local.get $html)))
      (return (global.get $BUNSPEC)))

   (func $bgl_inner_html_get (export "bgl_inner_html_get")
      (param $el externref)
      (result (ref $bstring))
      (return_call $load_string
	 (i32.const 128)
	 (call $innerHTMLget (local.get $el) (i32.const 128))))

   (func $bgl_alert (export "bgl_alert")
      (param $msg (ref $bstring))
      (result (ref eq))
      (call $store_string (local.get $msg) (i32.const 128))
      (call $alert (i32.const 128) (array.len (local.get $msg)))
      (return (global.get $BUNSPEC))))
   
