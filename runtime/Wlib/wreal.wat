;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wread.wat          */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Tue Oct  1 09:40:49 2024                          */
;*    Last change :  Tue Oct  1 09:56:51 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM reals                                                       */
;*=====================================================================*/

(module $__bigloo_real
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   ;; -----------------------------------------------------------------
   ;; Library functions 
   ;; -----------------------------------------------------------------
   (func $bgl_strtod (export "bgl_strtod")
      (param $str (ref $bstring))
      (result f64)
      
      (call $store_substring (local.get $str)
	 (i64.const 0)
	 (i64.extend_i32_u (array.len (local.get $str)))
	 (i32.const 128))
      
      (return_call $js_strtod (i32.const 128)
	 (array.len (local.get $str))))

   )
