;*=====================================================================*/
;*    serrano/prgm/project/bigloo/wasm/runtime/Wlib/wforeign.wat       */
;*    -------------------------------------------------------------    */
;*    Author      :  Manuel Serrano                                    */
;*    Creation    :  Wed Oct  2 10:14:39 2024                          */
;*    Last change :  Wed Oct  2 10:16:49 2024 (serrano)                */
;*    Copyright   :  2024 Manuel Serrano                               */
;*    -------------------------------------------------------------    */
;*    WASM foreign objects                                             */
;*=====================================================================*/

(module $__bigloo_foreign
   
   ;; -----------------------------------------------------------------
   ;; Type declarations 
   ;; -----------------------------------------------------------------
   
   (rec
      (type $cobj
	 (struct))
      (type $foreign
	 (struct
	    (field $id (ref $symbol))
	    (field $ptr i32))))
   
   ;; -----------------------------------------------------------------
   ;; Global variables 
   ;; -----------------------------------------------------------------
   
   (global $foreign-default-value
      (export "BGL_FOREIGN_DEFAULT_VALUE") (ref $foreign)
      (struct.new $foreign
	 (global.get $symbol-default-value)
	 (i32.const 0)))
   
   )
