;*=====================================================================*/
;*    .../prgm/project/bigloo/wasm/api/sqlite/src/Wlib/sqlite.wat      */
;*    -------------------------------------------------------------    */
;*    Author      :  manuel serrano                                    */
;*    Creation    :  Tue Jul 29 12:54:52 2025                          */
;*    Last change :  Tue Jul 29 12:56:15 2025 (serrano)                */
;*    Copyright   :  2025 manuel serrano                               */
;*    -------------------------------------------------------------    */
;*    SQLITE Wasm binding                                              */
;*=====================================================================*/

(module $__sqlite

   (type $$sqlite (struct (field $db externref))))
